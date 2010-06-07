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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Basic_Types;            use Basic_Types;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Entities.Queries;       use Entities, Entities.Queries;
with Glib;                   use Glib;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GPS.Editors;            use GPS.Editors;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple;
use GPS.Kernel.Messages, GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;     use GPS.Kernel.Scripts;
with GPS.Kernel;             use GPS.Kernel;
with Gtk.Box;                use Gtk.Box;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Label;              use Gtk.Label;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Widget;             use Gtk.Widget;
with Language;               use Language;
with Language_Handlers;      use Language_Handlers;
with Language.Tree.Database; use Language.Tree.Database;
with Refactoring.Services;   use Refactoring.Services;
with Refactoring_Module;     use Refactoring_Module;
with Refactoring.Performers; use Refactoring.Performers;
with Refactoring.UI;         use Refactoring.UI;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

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

      Factory             : Factory_Context;

      Entities             : Extracted_Entity_Lists.List;
      --  The entities referenced in the extracted code
   end record;
   Invalid_Context : constant Extract_Context :=
     (GNATCOLL.VFS.No_File, -1, -1, null, null, Factory => <>, Entities => <>);

   procedure Free (Context : in out Extract_Context);
   --  Free the memory used by the context

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

   ----------------
   -- Parameters --
   ----------------

   type Parameter_Description is record
      Parameter : Extracted_Entity;
      Is_Tagged : Boolean;
      PType     : Parameter_Type;
   end record;
   package Parameter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Parameter_Description);

   type Parameters is tagged record
      List           : Parameter_Lists.List;
      Last_Out_Param : Entity_Information;
      Count          : Natural;
      Is_Function    : Boolean;
   end record;
   --  The parameters that should be used for the extracted method.
   --  Last_Out_Param points to the last "out" parameter, which is used as the
   --  returned type for functions.
   --  Is_Function is True if the extracted method should be implemented as a
   --  method

   function Generate
     (Self : Parameters'Class;
      Context : Extract_Context) return Unbounded_String;
   --  Generate the list of parameters (including surrounding parenthesis) for
   --  the extracted subprogram

   function Generate_Method_Call
     (Self : Parameters'Class; Name : String) return Unbounded_String;
   --  Generate the code to call the new method

   procedure Sort (Self : in out Parameters'Class);
   --  Sort the parameters (any dispatching entity goes first)

   ----------
   -- Misc --
   ----------

   procedure Compute_Params_And_Vars
     (Kernel              : access Kernel_Handle_Record'Class;
      Context             : Extract_Context;
      Params              : out Parameters'Class;
      Local_Vars          : out Extracted_Entity_Lists.List'Class);
   --  From the list of entities in the context, compute those that should
   --  become parameters and those that should be local variables

   use Parameter_Lists, Entity_Information_Arrays, Extracted_Entity_Lists;

   procedure Generate_Extracted_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Context     : Extract_Context;
      Local_Vars  : out Extracted_Entity_Lists.List;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String);
   --  Generate the code of the new method

   function Generate_Box (Name : String) return Unbounded_String;
   --  Generate the subprogram box

   procedure Generate_Local_Vars
     (Local_Vars : Extracted_Entity_Lists.List;
      Result     : out Unbounded_String);
   --  Generate the local variables declarations

   function Extract_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Context     : Extract_Context;
      Method_Name : String) return Command_Return_Type;
   --  Extract a method

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands

   procedure Insert_New_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      In_File     : GNATCOLL.VFS.Virtual_File;
      Name        : String;
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

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Extract_Context) is
      C : Extracted_Entity_Lists.Cursor := Context.Entities.First;
      E : Extracted_Entity;
   begin
      while Has_Element (C) loop
         E := Element (C);
         Free (E.Decl);
         Next (C);
      end loop;
   end Free;

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

   ----------
   -- Sort --
   ----------

   procedure Sort (Self : in out Parameters'Class) is
      P : Parameter_Lists.Cursor;
   begin
      --  Put all "out" parameters last, so that we read from input parameters
      --  to output parameters

      P := Self.List.First;
      while Has_Element (P) loop
         case Element (P).PType is
            when Out_Parameter | In_Out_Parameter =>
               Self.List.Swap (P, Self.List.Last);
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

      P := Self.List.First;
      while Has_Element (P) loop
         if Element (P).Is_Tagged then
            Self.List.Swap (P, Self.List.First);
         end if;
         Next (P);
      end loop;

      --  ??? Put parameters with default values last
      --  We currently do not create default values, so this is irrelevant

      null;
   end Sort;

   -----------------------------
   -- Compute_Params_And_Vars --
   -----------------------------

   procedure Compute_Params_And_Vars
     (Kernel              : access Kernel_Handle_Record'Class;
      Context             : Extract_Context;
      Params              : out Parameters'Class;
      Local_Vars          : out Extracted_Entity_Lists.List'Class)
   is
      type Parameter_Count is array (Parameter_Type) of Natural;
      Count  : Parameter_Count := (others => 0);
      Entity : Entity_Information;
      Flags  : Extracted_Entity_Flags;
      E      : Extracted_Entity;
      P      : Extracted_Entity_Lists.Cursor := Context.Entities.First;
      Struct : Structured_File_Access;
      Offset : String_Index_Type;
   begin
      Struct := Get_Or_Create
        (Get_Construct_Database (Kernel), File => Context.File);
      Offset := To_String_Index (Struct, Context.Line_Start, 1);

      Count := (others => 0);
      Params.Last_Out_Param := null;

      while Has_Element (P) loop
         E := Element (P);

         Entity := E.Entity;
         Flags  := E.Flags;

         if Flags (Flag_Read) and then not Flags (Flag_Modified) then
            if Flags (Flag_Modified_Before)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               Params.List.Append
                 ((Parameter => E,
                   Is_Tagged =>
                     Accepts_Primitive_Ops (Context.Factory, Entity, Offset),
                   PType     => In_Parameter));
               Count (In_Parameter) := Count (In_Parameter) + 1;

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

                  Params.List.Append
                    ((Parameter => E,
                      Is_Tagged =>
                        Accepts_Primitive_Ops
                          (Context.Factory, Entity, Offset),
                      PType     => In_Out_Parameter));
                  Count (In_Out_Parameter) := Count (In_Out_Parameter) + 1;

               else
                  --  Written before, but not needed after.
                  --  ??? It is modified in the function, so we should have
                  --  a local variable that takes its value and is modified,
                  --  we do not need to return the parameter itself

                  Params.List.Append
                    ((Parameter => E,
                      Is_Tagged =>
                        Accepts_Primitive_Ops
                          (Context.Factory, Entity, Offset),
                      PType     => In_Out_Parameter));
                  Count (In_Out_Parameter) := Count (In_Out_Parameter) + 1;
               end if;

            elsif Flags (Flag_Read_After)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               --  Not set before the call, but needed after
               Params.List.Append
                 ((Parameter => E,
                   Is_Tagged =>
                     Accepts_Primitive_Ops (Context.Factory, Entity, Offset),
                   PType     => Out_Parameter));
               Count (Out_Parameter) := Count (Out_Parameter) + 1;
               Params.Last_Out_Param := Entity;

            else
               --  Not set before the call, and not needed after
               Local_Vars.Append (E);
            end if;
         end if;

         Next (P);
      end loop;

      if Count (Out_Parameter) = 1
        and then Count (In_Out_Parameter) = 0
      then
         Params.Is_Function := True;
         Params.Count := Integer (Params.List.Length) - 1;
      else
         Params.Is_Function := False;
         Params.Count := Integer (Params.List.Length);
         Params.Last_Out_Param := null;
      end if;
   end Compute_Params_And_Vars;

   --------------
   -- Generate --
   --------------

   function Generate
     (Self : Parameters'Class;
      Context : Extract_Context) return Unbounded_String
   is
      Decl  : Unbounded_String;
      Param : Parameter_Lists.Cursor;
   begin
      if Self.Count > 0 then
         Append (Decl, "(");

         Param := Self.List.First;
         while Has_Element (Param) loop
            --  Do not emit anything if there is a single out parameter, since
            --  we then use a function for the extracted method
            if Element (Param).PType /= Out_Parameter
              or else not Self.Is_Function
            then
               Append
                 (Decl, Element (Param).Parameter.Decl.Display_As_Parameter
                  (Context.Factory, Element (Param).PType));

               if Element (Param).Is_Tagged then
                  --  Since we are putting the code in the body, we should not
                  --  be a dispatching subprogram
                  Append (Decl, "'Class");
               end if;

               if Param /= Self.List.Last
                 and then Element (Next (Param)).Parameter.Entity /=
                 Self.Last_Out_Param
               then
                  Append (Decl, ";" & ASCII.LF & "    ");
               end if;
            end if;
            Next (Param);
         end loop;
         Decl := Decl & ")";
      end if;

      return Decl;
   end Generate;

   ------------------
   -- Generate_Box --
   ------------------

   function Generate_Box (Name : String) return Unbounded_String is
      Result : Unbounded_String;
   begin
      if Add_Subprogram_Box.Get_Pref then
         Append (Result, (1 .. Name'Length + 6 => '-') & ASCII.LF);
         Append (Result, "-- " & Name & " --" & ASCII.LF);
         Append (Result, (1 .. Name'Length + 6 => '-') & ASCII.LF);
         Append (Result, ASCII.LF);
      end if;
      return Result;
   end Generate_Box;

   --------------------------
   -- Generate_Method_Call --
   --------------------------

   function Generate_Method_Call
     (Self : Parameters'Class; Name : String) return Unbounded_String
   is
      Param : Parameter_Lists.Cursor;
      Method_Call : Unbounded_String;
   begin
      if Self.Is_Function then
         Method_Call := To_Unbounded_String
           (Get_Name (Self.Last_Out_Param).all & " := ");
      end if;

      Append (Method_Call, Name);

      if Self.Count > 0 then
         Append (Method_Call, " (");

         Param := Self.List.First;
         while Has_Element (Param) loop
            if Element (Param).PType /= Out_Parameter
              or else not Self.Is_Function
            then
               Append (Method_Call,
                       Get_Name (Element (Param).Parameter.Entity).all);

               if Param /= Self.List.Last
                 and then Element (Next (Param)).Parameter.Entity /=
                 Self.Last_Out_Param
               then
                  Append (Method_Call, ", ");
               end if;
            end if;
            Next (Param);
         end loop;

         Append (Method_Call, ")");
      end if;

      Append (Method_Call, ";" & ASCII.LF);
      return Method_Call;
   end Generate_Method_Call;

   -------------------------
   -- Generate_Local_Vars --
   -------------------------

   procedure Generate_Local_Vars
     (Local_Vars : Extracted_Entity_Lists.List;
      Result     : out Unbounded_String)
   is
      E    : Extracted_Entity;
      P    : Extracted_Entity_Lists.Cursor;
   begin
      Result := Null_Unbounded_String;

      P := Local_Vars.First;
      while Has_Element (P) loop
         E := Element (P);
         Append (Result, "   ");
         Append (Result, E.Decl.Display_As_Variable);
         Append (Result, ASCII.LF);
         Next (P);
      end loop;
   end Generate_Local_Vars;

   -------------------------------
   -- Generate_Extracted_Method --
   -------------------------------

   procedure Generate_Extracted_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Context     : Extract_Context;
      Local_Vars  : out Extracted_Entity_Lists.List;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String)
   is
      Editor  : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Kernel).Get (Context.File);
      Code    : constant String :=
        Editor.Get_Chars
          (Editor.New_Location (Context.Line_Start, 1),
           Editor.New_Location (Context.Line_End, 1).End_Of_Line);

      Params               : Parameters;
      Typ                  : Entity_Information;
      Code_Start, Code_End : Integer;
      Comment_Start, Comment_End : Integer;
      PList, Local, Returns : Unbounded_String;
      Newline_Before_Is : Boolean := False;
   begin
      Prepare_Code (Code, Comment_Start, Comment_End, Code_Start, Code_End);
      Compute_Params_And_Vars (Kernel, Context, Params, Local_Vars);
      Params.Sort;
      PList := Params.Generate (Context);

      if Params.Is_Function then
         Typ := Get_Type_Of (Params.Last_Out_Param);
         Returns := To_Unbounded_String (" return " & Get_Name (Typ).all);
      end if;

      if Params.Is_Function then
         Method_Decl := To_Unbounded_String ("function ");
      else
         Method_Decl := To_Unbounded_String ("procedure ");
      end if;

      Append (Method_Decl, Name);

      if PList /= Null_Unbounded_String then
         --  4 = " " + " is"
         if Params.Count > 1
           or else Length (Method_Decl) + Length (PList) + Length (Returns) + 4
           > Highlight_Column.Get_Pref
         then
            Append (Method_Decl, ASCII.LF & "  ");
            Newline_Before_Is := True;
         end if;

         Append (Method_Decl, " ");
         Append (Method_Decl, PList);
      end if;

      Append (Method_Decl, Returns);

      Method_Body := Generate_Box (Name);
      Append (Method_Body, Method_Decl);
      Append (Method_Decl, ";" & ASCII.LF);

      if Comment_Start < Comment_End then
         Append (Method_Decl, Code (Comment_Start .. Comment_End));
         Append (Method_Decl, ASCII.LF);
      end if;

      if Newline_Before_Is then
         Append (Method_Body, ASCII.LF & "is" & ASCII.LF);
      else
         Append (Method_Body, " is" & ASCII.LF);
      end if;

      Generate_Local_Vars (Local_Vars, Local);
      Append (Method_Body, Local);

      if Params.Is_Function then
         Typ := Get_Type_Of (Params.Last_Out_Param);
         Append (Method_Body, "   " & Get_Name (Params.Last_Out_Param).all
                 & " : " & Get_Name (Typ).all & ";" & ASCII.LF);
      end if;

      Append (Method_Body, "begin" & ASCII.LF & "   ");
      Append (Method_Body, Code (Code_Start .. Code_End));
      Append (Method_Body, ASCII.LF);

      if Params.Is_Function then
         Append (Method_Body, "   return "
                 & Get_Name (Params.Last_Out_Param).all & ";" & ASCII.LF);
      end if;

      Append (Method_Body, "end " & Name & ";" & ASCII.LF);

      Method_Call := Params.Generate_Method_Call (Name);
   end Generate_Extracted_Method;

   -----------------------
   -- Insert_New_Method --
   -----------------------

   procedure Insert_New_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      In_File     : GNATCOLL.VFS.Virtual_File;
      Name        : String;
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
      Create_Simple_Message
        (Get_Messages_Container (Kernel),
         -"Refactoring - extract subprogram " & Name,
         In_File, Line, 1,
         -"Extracted subprogram body inserted",
         0,
         (Editor_Side => True, Locations => True));

      if Create_Subprogram_Decl.Get_Pref then
         Inserted :=
           Insert_Text (Kernel, In_File, Decl_Line, 1, Method_Decl,
                        Indent                    => True,
                        Surround_With_Blank_Lines => True,
                        Skip_Comments_Backward    => True);
         Create_Simple_Message
           (Get_Messages_Container (Kernel),
            -"Refactoring - extract subprogram " & Name,
            In_File, Decl_Line, 1,
            -"Extracted subprogram spec inserted",
            0,
            (Editor_Side => True, Locations => True));
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
      Editor  : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Kernel).Get (Context.File);
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

      procedure Create_Decl_If_Necessary;
      --  Get information about the entity declaration, if needed

      procedure Create_Decl_If_Necessary is
      begin
         if Entity.Decl = No_Entity_Declaration then
            Entity.Decl := Get_Declaration (Context.Factory, Entity.Entity);

            --  Marks are used to remove the declaration for the entity, which
            --  will not happen unless the entity is declared in the current
            --  file.
            if Get_Filename (Decl.File) = Context.File then
               Create_Marks (Entity.Decl, Editor);
            end if;
         end if;
      end Create_Decl_If_Necessary;

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

            For_Each_Ref :
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

                  exit For_Each_Ref when Location.Line > Context.Line_End;

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
                           Create_Decl_If_Necessary;

                           if Entity.Decl.Initial_Value /= "" then
                              Entity.Flags (Flag_Modified_Before) := True;
                           end if;
                        end if;
                     end if;

                  else
                     --  A reference within the extracted code

                     if Location.Line = Decl.Line then
                        --  If this is the declaration, it means we have a
                        --  "declare" block, and as such we can simply ignore
                        --  this entity, it will be automatically extracted.
                        --
                        --  ??? Of course, the user could also have selected
                        --  the local vars in the original subprogram, or only
                        --  part of a declare block, in which case we should
                        --  really display an error

                        exit For_Each_Ref;
                     end if;

                     --  Should ignore the reference if it is a named
                     --  parameter in a subprogram call, as in
                     --  "Foo (Title => ...)"

                     ERef := Find_Reference_Details
                       (Get_Tree_Language (Struct),
                        Struct,
                        To_String_Index
                          (Struct, Location.Line, Location.Column));

                     if not ERef.Is_Named_Parameter then
                        Create_Decl_If_Necessary;

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
            end loop For_Each_Ref;

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
      Buffer : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Kernel).Get (Context.File);
      Method_Decl, Method_Body, Method_Call : Unbounded_String;
      Local_Vars : Extracted_Entity_Lists.List;
      Iter       : Extracted_Entity_Lists.Cursor;
      Result     : Command_Return_Type;
      E          : Extracted_Entity;
   begin
      if Context = Invalid_Context then
         Trace (Me, "Extract_Method: Invalid context");
         return Failure;
      end if;

      Buffer.Start_Undo_Group;

      Generate_Extracted_Method
        (Kernel,
         Name        => Method_Name,
         Context     => Context,
         Local_Vars  => Local_Vars,
         Method_Decl => Method_Decl,
         Method_Body => Method_Body,
         Method_Call => Method_Call);

      if Method_Body /= Null_Unbounded_String then
         declare
            Line_Start : constant Editor_Mark'Class :=
              Buffer.New_Location (Context.Line_Start, 1).Create_Mark;
            Line_End : constant Editor_Mark'Class :=
              Buffer.New_Location (Context.Line_End, 1)
              .End_Of_Line.Create_Mark;
         begin
            Iter := Local_Vars.First;

            --  Will remove the previous declaration unless it is a parameter

            while Has_Element (Iter) loop
               E := Element (Iter);
               if Is_Parameter_Of (E.Entity) = null then
                  E.Decl.Remove;
               end if;
               Next (Iter);
            end loop;

            Delete_Text
              (Kernel      => Kernel,
               In_File     => Context.File,
               Line_Start  => Line_Start.Line,
               Line_End    => Line_End.Line);

            if Insert_Text
              (Kernel     => Kernel,
               In_File    => Context.File,
               Line       => Line_Start.Line,
               Column     => 1,
               Text       => To_String (Method_Call),
               Indent     => True)
            then
               Create_Simple_Message
                 (Get_Messages_Container (Kernel),
                  -"Refactoring - extract subprogram " & Method_Name,
                  Context.File, Line_Start.Line,
                  1, -"Extracted subprogram call inserted",
                  0,
                  (Editor_Side => True, Locations => True));

               Insert_New_Method
                 (Kernel      => Kernel,
                  In_File     => Context.File,
                  Name        => Method_Name,
                  Before_Line => Line_Start.Line,
                  Context     => Context,
                  Method_Decl => To_String (Method_Decl),
                  Method_Body => To_String (Method_Body));
               Result := Success;
            else
               Trace
                 (Me,
                  "Extract_Method: Error inserting call to new subprogram");
               Result := Failure;
            end if;

            Line_Start.Delete;
            Line_End.Delete;
         end;
      else
         Trace (Me,
                "Extract_Method: Couldn't compute body of new subprogram");
         Result := Failure;
      end if;

      Buffer.Finish_Undo_Group;
      return Result;

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
      Extract := (File           => File_Information (Context.Context),
                  Factory        => Get_Context (Get_Kernel (Context.Context)),
                  Source         => null,
                  Line_Start     => <>,
                  Line_End       => <>,
                  Parent         => <>,
                  Entities       => <>);
      Get_Area (Context.Context, Extract.Line_Start, Extract.Line_End);
      Compute_Context_Entities (Get_Kernel (Context.Context), Extract);

      if Extract = Invalid_Context then
         --  Nothing to do, and error message already printed
         Free (Extract);
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

      Free (Extract);
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
      Context := (File           => Create (Nth_Arg (Data, 1)),
                  Factory        => Get_Context (Get_Kernel (Data)),
                  Source         => null,
                  Line_Start     => Nth_Arg (Data, 2),
                  Line_End       => Nth_Arg (Data, 3),
                  Parent         => <>,
                  Entities       => <>);
      Compute_Context_Entities (Get_Kernel (Data), Context);

      if Extract_Method
        (Kernel      => Get_Kernel (Data),
         Method_Name => Nth_Arg (Data, 4, "New_Method"),
         Context     => Context)
        /= Success
      then
         Set_Error_Msg (Data, "Couldn't extract method");
      end if;

      Free (Context);
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
