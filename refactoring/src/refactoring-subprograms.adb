-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2010, AdaCore              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Entities.Queries;       use Entities, Entities.Queries;
with Glib;                   use Glib;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GPS.Editors;            use GPS.Editors;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;     use GPS.Kernel.Scripts;
with GPS.Kernel;             use GPS.Kernel;
with Gtk.Box;                use Gtk.Box;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Label;              use Gtk.Label;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Widget;             use Gtk.Widget;
with Language;               use Language;
with Language.Ada;           use Language.Ada;
with Language_Handlers;      use Language_Handlers;
with Language.Tree.Database; use Language.Tree.Database;
with Refactoring_Module;     use Refactoring_Module;
with Refactoring.Performers; use Refactoring.Performers;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with Ada_Semantic_Tree.Parts;         use Ada_Semantic_Tree.Parts;

package body Refactoring.Subprograms is

   Me : constant Debug_Handle := Create ("Refactor.Subprograms");

   type Extract_Method_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Extract Method" menu

   type Extracted_Entity_Flag is
     (Flag_Modified,
      Flag_Read,
      --  Whether the entity is modified or read in the extracted code

      Flag_Ref_Outside_Parent,
      --  Whether the entity is referenced outside of the function containing
      --  the extracted code

      Flag_Read_Before, Flag_Modified_Before,
      --  Whether the entity is modified or read before the extracted code, but
      --  the same function as the extracted code

      Flag_Read_After, Flag_Modified_After
      --  Whether the entity is modified or read after the extracted code, but
      --  the same function as the extracted code
     );

   type Extracted_Entity_Flags is array (Extracted_Entity_Flag) of Boolean;

   type Extracted_Entity is record
      Entity : Entity_Information;
      Decl   : Entity_Declaration;
      Flags  : Extracted_Entity_Flags;
   end record;
   --  An entity found in the code to be extracted, and various information
   --  about its usage

   package Extracted_Entity_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Extracted_Entity);

   type Extract_Context is record
      File                 : GNATCOLL.VFS.Virtual_File;
      Line_Start, Line_End : Integer;
      --  Which code do we want to extract ?

      Source               : Source_File;
      --  The file in which the extracted code is at the start

      Parent               : Entity_Information;
      --  The subprogram that contained the extracted code before the
      --  refactoring

      Entities             : Extracted_Entity_Lists.List;
      --  The entities referenced in the extracted code
   end record;
   Invalid_Context : constant Extract_Context :=
     (GNATCOLL.VFS.No_File, -1, -1, null, null, Entities => <>);

   procedure Compute_Context_Parent
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : in out Extract_Context);
   --  Check that the selection in Context is within a single subprogram

   procedure Compute_Context_Entities
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : in out Extract_Context);
   --  Compute all entities referenced in the context.
   --  Returns Invalid_Context if something prevents the refactoring (an error
   --  message has already been displayed in that case).

   type Parameter_Description is record
      Parameter : Extracted_Entity;
      Is_Tagged : Boolean;
      PType     : Parameter_Type;
   end record;
   package Parameter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Parameter_Description);

   type Text_Range is record
      Start  : Integer;
      Lines  : Natural;
   end record;

   function ">" (Range1, Range2 : Text_Range) return Boolean;

   package Text_Ranges is new Ada.Containers.Indefinite_Ordered_Sets
     (Text_Range, ">", "=");

   use Parameter_Lists, Entity_Information_Arrays, Text_Ranges;
   use Extracted_Entity_Lists;

   procedure Generate_Extracted_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Context     : Extract_Context;
      To_Delete   : out Text_Ranges.Set;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String);
   --  Generate the code of the new method

   procedure Sort_Parameters (Params : in out Parameter_Lists.List);
   --  Sort the parameters (any dispatching entity goes first)

   function Extract_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Context     : Extract_Context;
      Method_Name : String) return Command_Return_Type;
   --  Extract a method

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands

   function Accepts_Primitive_Ops
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      Current_Offset : String_Index_Type) return Boolean;
   --  Whether the entity is an instance of a class or interface

   procedure Insert_New_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      In_File     : GNATCOLL.VFS.Virtual_File;
      Before_Line : Integer;
      Context     : Extract_Context;
      Method_Decl : String;
      Method_Body : String);
   --  Insert the new method decl and body in In_File, if possible before the
   --  line Before_Line.

   procedure Prepare_Code
     (Code          : String;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Code_Start    : out Natural;
      Code_End      : out Natural);
   --  Prepares the extracted code for pretty printing to its new location.
   --  In particular, this skips leading and trailing white spaces, and if the
   --  code starts with a comment it separates the comment from the code

   procedure Is_Parameter_Of
     (Entity       : Entity_Information;
      Is_Parameter : out Boolean;
      PType        : out Parameter_Type);
   --  Whether Entity is a parameter for a subprogram, and if it is which type

   ---------
   -- ">" --
   ---------

   function ">" (Range1, Range2 : Text_Range) return Boolean is
   begin
      return Range1.Start > Range2.Start
        or else (Range1.Start = Range2.Start
                 and then Range1.Lines > Range2.Lines);
   end ">";

   ------------------
   -- Prepare_Code --
   ------------------

   procedure Prepare_Code
     (Code          : String;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Code_Start    : out Natural;
      Code_End      : out Natural)
   is
      Lines_Skipped : Natural;
   begin
      Code_Start := Code'First;
      Skip_Blanks (Code, Code_Start);

      Comment_Start := Code_Start;
      Comment_End   := Comment_Start - 1;

      while Code_Start < Code'Last
        and then Code (Code_Start .. Code_Start + 1) = "--"
      loop
         Skip_Lines (Code, 1, Code_Start, Lines_Skipped);
         Comment_End := Code_Start;

         Skip_Blanks (Code, Code_Start);
      end loop;

      if Comment_End > Comment_Start then
         Skip_Blanks (Code, Comment_End, Step => -1);
      end if;

      Code_End := Code'Last;
      Skip_Blanks (Code, Code_End, Step => -1);
   end Prepare_Code;

   ---------------------------
   -- Accepts_Primitive_Ops --
   ---------------------------

   function Accepts_Primitive_Ops
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      Current_Offset : String_Index_Type) return Boolean
   is
      EA  : Entity_Access := Get_Entity_Access (Kernel, Get_Type_Of (Entity));
   begin
      if EA /= Null_Entity_Access then
         EA := Get_Last_Visible_Declaration
           (EA, Get_File (EA), Offset => Current_Offset);

         return Get_Construct (EA).Attributes (Ada_Tagged_Attribute)
           or else Get_Construct (EA).Attributes (Ada_Interface_Attribute);
      else
         return False;
      end if;
   end Accepts_Primitive_Ops;

   ---------------------
   -- Sort_Parameters --
   ---------------------

   procedure Sort_Parameters (Params : in out Parameter_Lists.List) is
      P : Parameter_Lists.Cursor;
   begin
      --  Put all "out" parameters last, so that we read from input parameters
      --  to output parameters

      P := Params.First;
      while Has_Element (P) loop
         case Element (P).PType is
            when Out_Parameter | In_Out_Parameter =>
               Params.Swap (P, Params.Last);
            when In_Parameter | Access_Parameter =>
               null;
         end case;
         Next (P);
      end loop;

      --  Put the dispatching parameter first, if any. By convention, we also
      --  put "Self" or "This" first if we can't find another dispatching
      --  parameter.
      --  Needs to be done after the "out" parameter, in case the dispatching
      --  parameter is also out

      P := Params.First;
      while Has_Element (P) loop
         if Element (P).Is_Tagged then
            Params.Swap (P, Params.First);
         end if;
         Next (P);
      end loop;

      --  ??? Put parameters with default values last
      --  We currently do not create default values, so this is irrelevant

      null;
   end Sort_Parameters;

   -------------------------------
   -- Generate_Extracted_Method --
   -------------------------------

   procedure Generate_Extracted_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Context     : Extract_Context;
      To_Delete   : out Text_Ranges.Set;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String)
   is
      Params              : Parameter_Lists.List;
      Param               : Parameter_Lists.Cursor;
      Local_Vars          : Extracted_Entity_Lists.List;
      Out_Params_Count    : Natural := 0;
      In_Out_Params_Count : Natural := 0;
      In_Params_Count     : Natural := 0;
      Result, Decl        : Unbounded_String;
      Typ                 : Entity_Information;
      Last_Out_Param      : Entity_Information;
      Flags               : Extracted_Entity_Flags;

      Editor              : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Kernel).Get (Context.File);
      Code : constant String :=
        Editor.Get_Chars
          (Editor.New_Location (Context.Line_Start, 1),
           Editor.New_Location (Context.Line_End, 1).End_Of_Line);

      Code_Start, Code_End : Integer;
      Comment_Start, Comment_End : Integer;
      Entity : Entity_Information;
      Struct : Structured_File_Access;
      Offset : String_Index_Type;
      P      : Extracted_Entity_Lists.Cursor;
      E      : Extracted_Entity;
   begin
      Struct := Get_Or_Create
        (Get_Construct_Database (Kernel), File => Context.File);
      Offset := To_String_Index (Struct, Context.Line_Start, 1);

      Prepare_Code (Code, Comment_Start, Comment_End, Code_Start, Code_End);

      P := Context.Entities.First;
      while Has_Element (P) loop
         E := Element (P);

         Entity := E.Entity;
         Flags  := E.Flags;

         if Flags (Flag_Read) and then not Flags (Flag_Modified) then
            if Flags (Flag_Modified_Before)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               Params.Append
                 ((Parameter => E,
                   Is_Tagged => Accepts_Primitive_Ops (Kernel, Entity, Offset),
                   PType     => In_Parameter));
               In_Params_Count := In_Params_Count + 1;

            else
               --  Variable not modified before, so its value before the
               --  extracted code is irrelevant. But the variable is also not
               --  modified within the code.

               if Flags (Flag_Ref_Outside_Parent) then
                  --  ??? A "global" variable. This should limit the scope to
                  --  which the code can be extracted. That scope must include
                  --  the declaration of the variable.
                  null;

               else
                  --  A reference to an uninitialized variable
                  Local_Vars.Append (E);
               end if;
            end if;

         elsif Flags (Flag_Modified) then
            if Flags (Flag_Modified_Before) then
               if Flags (Flag_Read_After)
                 or else Flags (Flag_Ref_Outside_Parent)
               then
                  --  Written before and at least needed after the call

                  Params.Append
                    ((Parameter => E,
                      Is_Tagged =>
                        Accepts_Primitive_Ops (Kernel, Entity, Offset),
                      PType     => In_Out_Parameter));
                  In_Out_Params_Count := In_Out_Params_Count + 1;

               else
                  --  Written before, but not needed after.
                  --  ??? It is modified in the function, so we should have
                  --  a local variable that takes its value and is modified,
                  --  we do not need to return the parameter itself

                  Params.Append
                    ((Parameter => E,
                      Is_Tagged =>
                        Accepts_Primitive_Ops (Kernel, Entity, Offset),
                      PType     => In_Out_Parameter));
                  In_Out_Params_Count := In_Out_Params_Count + 1;
               end if;

            elsif Flags (Flag_Read_After)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               --  Not set before the call, but needed after
               Params.Append
                 ((Parameter => E,
                   Is_Tagged => Accepts_Primitive_Ops (Kernel, Entity, Offset),
                   PType     => Out_Parameter));
               Out_Params_Count := Out_Params_Count + 1;
               Last_Out_Param := Entity;

            else
               --  Not set before the call, and not needed after
               Local_Vars.Append (E);
            end if;
         end if;

         Next (P);
      end loop;

      Sort_Parameters (Params);

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Decl := To_Unbounded_String ("function ");
      else
         Decl := To_Unbounded_String ("procedure ");
         Last_Out_Param := null;
      end if;

      Append (Decl, Name);

      --  Do we have at least one parameter left ?

      if In_Params_Count + In_Out_Params_Count > 0
        or else Out_Params_Count > 1
      then
         Append (Decl, ASCII.LF & "   (");

         Param := Params.First;
         while Has_Element (Param) loop
            --  Do not emit anything if there is a single out parameter, since
            --  we then use a function for the extracted method
            if Element (Param).PType /= Out_Parameter
              or else Out_Params_Count /= 1
              or else In_Out_Params_Count /= 0
            then
               Append
                 (Decl, Element (Param).Parameter.Decl.Display_As_Parameter
                  (Element (Param).PType));

               if Element (Param).Is_Tagged then
                  --  Since we are putting the code in the body, we should not
                  --  be a dispatching subprogram
                  Append (Decl, "'Class");
               end if;

               if Param /= Params.Last
                 and then Element (Next (Param)).Parameter.Entity /=
                 Last_Out_Param
               then
                  Append (Decl, ";" & ASCII.LF & "    ");
               end if;
            end if;
            Next (Param);
         end loop;
         Decl := Decl & ")";
      end if;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Typ := Get_Type_Of (Last_Out_Param);
         Decl := Decl & " return " & Get_Name (Typ).all;
      end if;

      if Add_Subprogram_Box.Get_Pref then
         Append (Result, (1 .. Name'Length + 6 => '-') & ASCII.LF);
         Append (Result, "-- " & Name & " --" & ASCII.LF);
         Append (Result, (1 .. Name'Length + 6 => '-') & ASCII.LF);
         Append (Result, ASCII.LF);
      end if;

      Append (Result, Decl);

      Append (Decl, ";" & ASCII.LF);

      if Comment_Start < Comment_End then
         Append (Decl, Code (Comment_Start .. Comment_End));
         Append (Decl, ASCII.LF);
      end if;

      Append (Result, ASCII.LF & "is" & ASCII.LF);

      P := Local_Vars.First;
      while Has_Element (P) loop
         E := Element (P);

         declare
            Decl : constant String := E.Decl.Display_As_Variable;
         begin
            --  Will remove the previous declaration unless it is a parameter

            if Is_Parameter_Of (E.Entity) = null then
               To_Delete.Include
                 ((Start  => Get_Line (Get_Declaration_Of (E.Entity)),
                   Lines  => Lines_Count (Decl)));
            end if;

            Append (Result, "   ");
            Append (Result, Decl);
            Append (Result, ASCII.LF);
         end;

         Next (P);
      end loop;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Typ := Get_Type_Of (Last_Out_Param);
         Append (Result, "   " & Get_Name (Last_Out_Param).all
                 & " : " & Get_Name (Typ).all & ";" & ASCII.LF);
      end if;

      Append (Result, "begin" & ASCII.LF & "   ");
      Append (Result, Code (Code_Start .. Code_End));
      Append (Result, ASCII.LF);

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Append (Result, "   return "
           & Get_Name (Last_Out_Param).all & ";" & ASCII.LF);
      end if;

      Append (Result, "end " & Name & ";" & ASCII.LF);

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Method_Call := To_Unbounded_String
           (Get_Name (Last_Out_Param).all & " := ");
      end if;

      Append (Method_Call, Name);
      if In_Params_Count + In_Out_Params_Count > 0
        or else Out_Params_Count > 1
      then
         Append (Method_Call, " (");

         Param := Params.First;
         while Has_Element (Param) loop
            if Element (Param).PType /= Out_Parameter
              or else Out_Params_Count /= 1
              or else In_Out_Params_Count /= 0
            then
               Append (Method_Call,
                       Get_Name (Element (Param).Parameter.Entity).all);

               if Param /= Params.Last
                 and then Element (Next (Param)).Parameter.Entity /=
                    Last_Out_Param
               then
                  Append (Method_Call, ", ");
               end if;
            end if;
            Next (Param);
         end loop;

         Append (Method_Call, ")");
      end if;

      Append (Method_Call, ";" & ASCII.LF);
      Method_Decl := Decl;
      Method_Body := Result;
   end Generate_Extracted_Method;

   -----------------------
   -- Insert_New_Method --
   -----------------------

   procedure Insert_New_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      In_File     : GNATCOLL.VFS.Virtual_File;
      Before_Line : Integer;
      Context     : Extract_Context;
      Method_Decl : String;
      Method_Body : String)
   is
      Languages  : constant Language_Handler := Get_Language_Handler (Kernel);
      Handler    : constant LI_Handler :=
        Get_LI_Handler_From_File (Languages, In_File);
      Constructs : Construct_List;
      Line       : Integer := Before_Line;
      Decl_Line  : Integer := Integer'Last;
      Inserted   : Boolean;
      pragma Unreferenced (Inserted);
   begin
      Parse_File_Constructs (Handler, Languages, In_File, Constructs);
      Constructs.Current := Constructs.First;
      while Constructs.Current /= null loop
         if Constructs.Current.Category in Subprogram_Category then
            if Constructs.Current.Sloc_Start.Line < Decl_Line then
               Decl_Line := Constructs.Current.Sloc_Start.Line;
            end if;

            if Constructs.Current.Sloc_Start.Line <= Line
              and then Constructs.Current.Sloc_End.Line > Line
            then
               Line := Constructs.Current.Sloc_Start.Line;
            end if;
         end if;

         Constructs.Current := Constructs.Current.Next;
      end loop;

      Free (Constructs);

      --  Insert the body before the decl, so that if they are inserted at the
      --  same line, they occur with the decl first.
      --  We must also insert before any "subprogram box" preceding the first
      --  subprogram we found

      Inserted := Insert_Text
        (Kernel, In_File, Line, 1, Method_Body,
         Indent                    => True,
         Surround_With_Blank_Lines => True,
         Skip_Comments_Backward    => True);

      if Create_Subprogram_Decl.Get_Pref then
         Inserted :=
           Insert_Text (Kernel, In_File, Decl_Line, 1, Method_Decl,
                        Indent                    => True,
                        Surround_With_Blank_Lines => True,
                        Skip_Comments_Backward    => True);
      end if;
   end Insert_New_Method;

   ----------------------------
   -- Compute_Context_Parent --
   ----------------------------

   procedure Compute_Context_Parent
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : in out Extract_Context)
   is
      Scopes : constant Lines_To_Scope := Compute_Scopes (Context.Source);
   begin
      Context.Parent := Get_Scope (Scopes, Context.Line_Start);

      if Active (Me) and then Context.Parent /= null then
         Trace (Me, "Context.Parent=" & Get_Name (Context.Parent).all);
      end if;

      if Get_Scope (Scopes, Context.Line_End) /= Context.Parent then
         Insert
           (Kernel,
            -"The selected code does not belong to a single subprogram",
            Mode => Error);
         Context := Invalid_Context;
      end if;
   end Compute_Context_Parent;

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   procedure Is_Parameter_Of
     (Entity       : Entity_Information;
      Is_Parameter : out Boolean;
      PType        : out Parameter_Type)
   is
      Sub   : constant Entity_Information := Is_Parameter_Of (Entity);
      Iter  : Subprogram_Iterator;
      Param : Entity_Information;
   begin
      if Sub /= null then
         Iter := Get_Subprogram_Parameters (Sub);
         loop
            Get (Iter, Param);
            exit when Param = null;

            if Param = Entity then
               Is_Parameter := True;
               PType        := Get_Type (Iter);
               return;
            end if;

            Next (Iter);
         end loop;
      end if;

      Is_Parameter := False;
   end Is_Parameter_Of;

   ------------------------------
   -- Compute_Context_Entities --
   ------------------------------

   procedure Compute_Context_Entities
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : in out Extract_Context)
   is
      Ref_Iter : Entity_Reference_Iterator;
      Iter     : Entity_Iterator;
      Caller   : Entity_Information;
      Ref : Entity_Reference;
      Decl, Location, Body_Loc : File_Location;
      Entity    : Extracted_Entity;
      Is_Global : Boolean;
      Is_Param  : Boolean;
      PType     : Parameter_Type;
      Struct    : Structured_File_Access;
      ERef      : Entity_Reference_Details;

   begin
      Context.Source := Get_Or_Create (Get_Database (Kernel), Context.File);

      --  This code might require loading a file, so we only freeze afterward
      Find_All_Entities_In_File (Iter, Context.Source);
      Freeze (Get_Database (Kernel), Mode => No_Create_Or_Update);

      Struct := Get_Or_Create
        (Get_Construct_Database (Kernel), File => Context.File);
      Update_Contents (Struct);

      Compute_Context_Parent (Kernel, Context);
      if Context = Invalid_Context then
         Thaw (Get_Database (Kernel));
         return;
      end if;

      while not At_End (Iter) loop
         Entity := (Entity => Get (Iter),
                    Decl   => No_Entity_Declaration,
                    Flags  => (others => False));

         Decl := Get_Declaration_Of (Entity.Entity);
         Find_Next_Body
           (Entity   => Entity.Entity,
            Location => Body_Loc);

         --  An entity is "global" (ie does not need an entry in the parameter
         --  list) if it is defined in another file, or in the current file at
         --  library level. We however need to accept in case the entity is
         --  declared in the '.ads' but we are working in the '.adb' (for
         --  instance the parameter to a subprogram).

         Is_Global := Get_LI (Decl.File) /= Get_LI (Context.Source)
           or else not Is_Subprogram
             (Get_Caller (Declaration_As_Reference (Entity.Entity)));

         if Active (Me) then
            Trace (Me, "Entity=" & Debug_Name (Entity.Entity)
                   & " Is_Global=" & Is_Global'Img);
         end if;

         if not Is_Global then
            Find_All_References
              (Ref_Iter, Entity.Entity, In_File => Context.Source);
            while not At_End (Ref_Iter) loop
               Ref      := Get (Ref_Iter);
               Location := Get_Location (Ref);
               Caller   := Get_Caller (Ref);

               if Context.Parent /= null
                 and then Caller /= Context.Parent
               then
                  --  A reference outside of the current subprogram
                  Entity.Flags (Flag_Ref_Outside_Parent) := True;

                  --  No interest in further references, since they can't be
                  --  in the extracted code and we already know the entity
                  --  is ref outside of that code

                  exit when Location.Line > Context.Line_End;

               else
                  --  A reference within the current subprogram

                  if Location.Line > Context.Line_End then
                     Entity.Flags (Flag_Read_After) := True;

                  elsif Location.Line < Context.Line_Start then
                     if Location.Line /= Decl.Line
                       and then Location.Line /= Body_Loc.Line
                     then
                        if Is_Write_Reference (Get_Kind (Ref)) then
                           Entity.Flags (Flag_Modified_Before) := True;
                        elsif Is_Read_Reference (Get_Kind (Ref)) then
                           Entity.Flags (Flag_Read_Before) := True;
                        end if;

                     else
                        Is_Parameter_Of (Entity.Entity, Is_Param, PType);
                        if Is_Param then
                           case PType is
                              when Out_Parameter =>
                                 --  the entity is needed outside of the
                                 --  extracted code (both to read its value and
                                 --  set it for the caller)
                                 Entity.Flags (Flag_Modified_After) := True;
                                 Entity.Flags (Flag_Read_After) := True;

                              when others =>
                                 --  An initial value might be passed through
                                 --  the parameter
                                 Entity.Flags (Flag_Modified_Before) := True;
                           end case;

                        else
                           if Entity.Decl = No_Entity_Declaration then
                              Entity.Decl := Get_Declaration
                                (Kernel, Entity.Entity);
                           end if;

                           if Entity.Decl.Initial_Value /= "" then
                              Entity.Flags (Flag_Modified_Before) := True;
                           end if;
                        end if;
                     end if;

                  else
                     --  A reference within the extracted code

                     --  Should ignore the reference if it is a named
                     --  parameter in a subprogram call, as in
                     --  "Foo (Title => ...)"

                     ERef := Find_Reference_Details
                       (Get_Tree_Language (Struct),
                        Struct,
                        To_String_Index
                          (Struct, Location.Line, Location.Column));

                     if not ERef.Is_Named_Parameter then
                        if Entity.Decl = No_Entity_Declaration then
                           Entity.Decl := Get_Declaration
                             (Kernel, Entity.Entity);
                        end if;

                        --  If we are calling a subprogram nested within the
                        --  parent, we can't extract the code.
                        --  ??? We could if we extract to another nested
                        --  subprogram.
                        --  ??? We also could if the only reference to that
                        --  nested is within the extracted code, in which
                        --  case we should extract the subprogram too

                        if Is_Subprogram (Entity.Entity) then
                           Caller := Get_Caller
                             (Declaration_As_Reference (Entity.Entity));

                           --  ??? We should test if it is nested within
                           --  Context.Parent, when that is set
                           if Caller /= null
                             and then Is_Subprogram (Caller)
                           then
                              Trace (Me, "Call to nested subprogram");
                              Insert
                                (Kernel,
                                 Text => -"A call to the nested subprogram "
                                 & Get_Name (Entity.Entity).all
                                 & (-" prevents the refactoring"),
                                 Mode => Error);
                              Context := Invalid_Context;
                              Thaw (Get_Database (Kernel));
                              return;
                           end if;
                        end if;

                        if Is_Write_Reference (Get_Kind (Ref)) then
                           Entity.Flags (Flag_Modified) := True;
                        elsif Is_Read_Reference (Get_Kind (Ref)) then
                           Entity.Flags (Flag_Read) := True;
                        end if;
                     end if;
                  end if;
               end if;

               Next (Ref_Iter);
            end loop;

            Destroy (Ref_Iter);

            if Entity.Flags (Flag_Modified)
              or else Entity.Flags (Flag_Read)
            then
               if Active (Me) then
                  Trace (Me, "Extract references """
                         & Get_Name (Entity.Entity).all
                         & """ r=" & Entity.Flags (Flag_Read)'Img
                         & " w=" & Entity.Flags (Flag_Modified)'Img
                         & " before/r="
                         & Entity.Flags (Flag_Read_Before)'Img
                         & " w=" & Entity.Flags (Flag_Modified_Before)'Img
                         & " after/r="
                         & Entity.Flags (Flag_Read_After)'Img
                         & " w=" & Entity.Flags (Flag_Modified_After)'Img
                         & " outside="
                         & Entity.Flags (Flag_Ref_Outside_Parent)'Img);
               end if;

               Context.Entities.Append (Entity);
            end if;
         end if;

         Next (Iter);
      end loop;

      Destroy (Iter);
      Thaw (Get_Database (Kernel));

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Thaw (Get_Database (Kernel));
         raise;
   end Compute_Context_Entities;

   --------------------
   -- Extract_Method --
   --------------------

   function Extract_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Context     : Extract_Context;
      Method_Name : String) return Command_Return_Type
   is
      Method_Decl, Method_Body, Method_Call : Unbounded_String;
      To_Delete  : Text_Ranges.Set;
      Line_Start : Integer;
      Iter       : Text_Ranges.Cursor;
   begin
      if Context = Invalid_Context then
         Trace (Me, "Extract_Method: Invalid context");
         return Failure;
      end if;

      Generate_Extracted_Method
        (Kernel,
         Name        => Method_Name,
         Context     => Context,
         To_Delete   => To_Delete,
         Method_Decl => Method_Decl,
         Method_Body => Method_Body,
         Method_Call => Method_Call);

      if Method_Body /= Null_Unbounded_String then
         Start_Undo_Group (Kernel, Context.File);

         Line_Start := Context.Line_Start;
         Iter := To_Delete.First;

         while Has_Element (Iter) loop
            if Element (Iter).Start < Line_Start then
               Line_Start := Line_Start - Element (Iter).Lines;
            end if;

            Delete_Text
              (Kernel     => Kernel,
               In_File    => Context.File,
               Line_Start => Element (Iter).Start,
               Line_End   => Element (Iter).Start + Element (Iter).Lines - 1);

            Next (Iter);
         end loop;

         Delete_Text
           (Kernel      => Kernel,
            In_File     => Context.File,
            Line_Start  => Line_Start,
            Line_End    => Line_Start + Context.Line_End - Context.Line_Start);
         if Insert_Text
           (Kernel     => Kernel,
            In_File    => Context.File,
            Line       => Line_Start,
            Column     => 1,
            Text       => To_String (Method_Call),
            Indent     => True)
         then
            Insert_New_Method
              (Kernel      => Kernel,
               In_File     => Context.File,
               Before_Line => Line_Start,
               Context     => Context,
               Method_Decl => To_String (Method_Decl),
               Method_Body => To_String (Method_Body));
            Finish_Undo_Group (Kernel, Context.File);
            return Success;
         else
            Trace (Me,
                   "Extract_Method: Error inserting call to new subprogram");
            Finish_Undo_Group (Kernel, Context.File);
            return Failure;
         end if;
      else
         Trace (Me,
                "Extract_Method: Couldn't compute body of new subprogram");
         return Failure;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Failure;
   end Extract_Method;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Dialog : Gtk_Dialog;
      Ent    : Gtk_Entry;
      Button : Gtk_Widget;
      Label  : Gtk_Label;

      Extract : Extract_Context;

      Result : Command_Return_Type := Failure;
      pragma Unreferenced (Command, Button);

   begin
      Extract := (File              => File_Information (Context.Context),
                  Source            => null,
                  Line_Start        => <>,
                  Line_End          => <>,
                  Parent            => <>,
                  Entities          => <>);
      Get_Area (Context.Context, Extract.Line_Start, Extract.Line_End);
      Compute_Context_Entities (Get_Kernel (Context.Context), Extract);

      if Extract = Invalid_Context then
         --  Nothing to do, and error message already printed
         return Failure;
      end if;

      Gtk_New (Dialog,
               Title  => -"Extract Method",
               Parent => Get_Current_Window (Get_Kernel (Context.Context)),
               Flags  => Modal);
      Gtk_New (Label, -"Name of the new subprogram:");
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False);

      Gtk_New (Ent);
      Set_Text (Ent, -"New_Method");
      Select_Region (Ent, 0, -1);
      Set_Activates_Default (Ent, True);
      Pack_Start (Get_Vbox (Dialog), Ent, Expand => False);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Result := Extract_Method
           (Kernel      => Get_Kernel (Context.Context),
            Method_Name => Get_Text (Ent),
            Context     => Extract);
      end if;

      Destroy (Dialog);

      return Result;
   end Execute;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Context     : Extract_Context;
   begin
      Context := (File              => Create (Nth_Arg (Data, 1)),
                  Source            => null,
                  Line_Start        => Nth_Arg (Data, 2),
                  Line_End          => Nth_Arg (Data, 3),
                  Parent            => <>,
                  Entities          => <>);
      Compute_Context_Entities (Get_Kernel (Data), Context);

      if Extract_Method
        (Kernel      => Get_Kernel (Data),
         Method_Name => Nth_Arg (Data, 4, "New_Method"),
         Context     => Context)
        /= Success
      then
         Set_Error_Msg (Data, "Couldn't extract method");
      end if;
   end Command_Handler;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      C : Interactive_Command_Access;
      Filter : Action_Filter;
   begin
      --  Disabled for now
      if Active (Me) then
         C := new Extract_Method_Command;
         Filter := new Is_Area_Context;
         Register_Contextual_Menu
           (Kernel,
            Name  => "Extract Subprogram",
            Label => "Refactoring/Extract Subprogram",
            Filter => Filter and Create (Module => "Source_Editor") and
              Create (Language => "ada"),
            Action => C);
      end if;

      if Active (Testsuite_Handle) then
         Register_Command
           (Kernel, "extract_method", 3, 4, Command_Handler'Access);
      end if;
   end Register_Refactoring;

end Refactoring.Subprograms;
