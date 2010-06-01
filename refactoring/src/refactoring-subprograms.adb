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
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Dynamic_Arrays;
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
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Label;              use Gtk.Label;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Widget;             use Gtk.Widget;
with Histories;              use Histories;
with Language;               use Language;
with Language_Handlers;      use Language_Handlers;
with Language.Tree.Database; use Language.Tree.Database;
with Refactoring.Performers; use Refactoring.Performers;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

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
      Flags  : Extracted_Entity_Flags;
   end record;
   --  An entity found in the code to be extracted, and various information
   --  about its usage

   package Extracted_Entity_Arrays is new Dynamic_Arrays
     (Data                    => Extracted_Entity,
      Table_Multiplier        => 1,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 10);

   type Extract_Context is record
      File                 : GNATCOLL.VFS.Virtual_File;
      Line_Start, Line_End : Integer;
      --  Which code do we want to extract ?

      Use_In_Keyword    : Boolean;
      Use_Separate_Decl : Boolean;
      --  The options to configure the output

      Parent               : Entity_Information;
      --  The subprogram that contained the extracted code before the
      --  refactoring

      Entities             : Extracted_Entity_Arrays.Instance;
      --  The entities referenced in the extracted code
   end record;
   Invalid_Context : constant Extract_Context :=
     (GNATCOLL.VFS.No_File, -1, -1, False, False, null, Entities => <>);

   procedure Compute_Context_Entities
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : in out Extract_Context);
   --  Compute all entities referenced in the context.
   --  Returns Invalid_Context if something prevents the refactoring (an error
   --  message has already been displayed in that case).

   type Parameter_Description is record
      Parameter : Entity_Information;
      PType     : Parameter_Type;
   end record;

   type Text_Range is record
      Start  : Integer;
      Lines  : Natural;
   end record;

   function ">" (Range1, Range2 : Text_Range) return Boolean;

   package Text_Ranges is new Ada.Containers.Indefinite_Ordered_Sets
     (Text_Range, ">", "=");

   package Parameter_Arrays is new Dynamic_Arrays (Parameter_Description);

   use Parameter_Arrays, Entity_Information_Arrays, Text_Ranges;

   procedure Generate_Extracted_Method
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Context     : Extract_Context;
      To_Delete   : out Text_Ranges.Set;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String);
   --  Generate the code of the new method

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
      Before_Line : Integer;
      Context     : Extract_Context;
      Method_Decl : String;
      Method_Body : String);
   --  Insert the new method decl and body in In_File, if possible before the
   --  line Before_Line.

   ---------
   -- ">" --
   ---------

   function ">" (Range1, Range2 : Text_Range) return Boolean is
   begin
      return Range1.Start > Range2.Start
        or else (Range1.Start = Range2.Start
                 and then Range1.Lines > Range2.Lines);
   end ">";

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
      use Extracted_Entity_Arrays;

      Params              : Parameter_Arrays.Instance;
      Local_Vars          : Entity_Information_Arrays.Instance;
      Out_Params_Count    : Natural := 0;
      In_Out_Params_Count : Natural := 0;
      In_Params_Count     : Natural := 0;
      Result, Decl        : Unbounded_String;
      Entity              : Entity_Information;
      Typ                 : Entity_Information;
      First_Out_Param     : Entity_Information;
      Editor              : constant Editor_Buffer'Class :=
        Get_Buffer_Factory (Kernel).Get (Context.File);
      Flags               : Extracted_Entity_Flags;
      Declaration_Added   : Boolean;
   begin
      for P in Extracted_Entity_Arrays.First .. Last (Context.Entities) loop
         Entity := Context.Entities.Table (P).Entity;
         Flags  := Context.Entities.Table (P).Flags;

         if Flags (Flag_Read) and then not Flags (Flag_Modified) then
            if Flags (Flag_Modified_Before)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               Append (Params, (Parameter => Entity, PType => In_Parameter));

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
                  Append (Local_Vars, Entity);
               end if;
            end if;

         elsif Flags (Flag_Modified) then
            if Flags (Flag_Modified_Before) then
               if Flags (Flag_Read_After)
                 or else Flags (Flag_Ref_Outside_Parent)
               then
                  --  Written before and at least needed after the call
                  Append (Params, (Entity, In_Out_Parameter));

               else
                  --  Written before, but not needed after.
                  --  ??? It is modified in the function, so we should have
                  --  a local variable that takes its value and is modified,
                  --  we do not need to return the parameter itself
                  Append (Params, (Entity, In_Out_Parameter));
               end if;

            elsif Flags (Flag_Read_After)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               --  Not set before the call, but needed after
               Append (Params, (Entity, Out_Parameter));

            else
               --  Not set before the call, and not needed after
               Append (Local_Vars, Entity);
            end if;
         end if;
      end loop;

      for P in Parameter_Arrays.First .. Last (Params) loop
         if Params.Table (P).PType = Out_Parameter then
            Out_Params_Count := Out_Params_Count + 1;
            First_Out_Param := Params.Table (P).Parameter;
         elsif Params.Table (P).PType = In_Out_Parameter then
            In_Out_Params_Count := In_Out_Params_Count + 1;
         else
            In_Params_Count := In_Params_Count + 1;
         end if;
      end loop;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Decl := To_Unbounded_String (ASCII.LF & "function ");
      else
         Decl := To_Unbounded_String (ASCII.LF & "procedure ");
      end if;

      Append (Decl, Name);

      --  Do we have at least one parameter left ?

      if In_Params_Count + In_Out_Params_Count > 0
        or else Out_Params_Count > 1
      then
         Append (Decl, ASCII.LF & "   (");
         for P in Parameter_Arrays.First .. Last (Params) loop

            --  Do not emit anything if there is a single out parameter, since
            --  we then use a function for the extracted method
            if Params.Table (P).PType /= Out_Parameter
              or else Out_Params_Count /= 1
              or else In_Out_Params_Count /= 0
            then
               Append
                 (Decl, Get_Name (Params.Table (P).Parameter).all & " : ");
               Typ := Get_Type_Of (Params.Table (P).Parameter);
               if Typ = null then
                  Insert (Kernel,
                          Text => -"Couldn't find the type of "
                             & Get_Name (Params.Table (P).Parameter).all,
                          Mode => Error);
                  Method_Decl := Null_Unbounded_String;
                  Method_Body := Null_Unbounded_String;
                  return;
               end if;

               if Params.Table (P).PType = Out_Parameter then
                  Append (Decl, "out ");
               elsif Params.Table (P).PType = In_Out_Parameter then
                  Append (Decl, "in out ");
               elsif Context.Use_In_Keyword then
                  Append (Decl, "in ");
               end if;

               Append (Decl, Get_Name (Typ).all);
               if P /= Last (Params) then
                  Append (Decl, ";" & ASCII.LF & "    ");
               end if;
            end if;
         end loop;
         Decl := Decl & ")";
      end if;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Typ := Get_Type_Of (First_Out_Param);
         Decl := Decl & " return " & Get_Name (Typ).all;
      end if;

      Result := Decl;
      Append (Decl, ";" & ASCII.LF);
      Append (Result, ASCII.LF & "is" & ASCII.LF);

      for L in Entity_Information_Arrays.First .. Last (Local_Vars) loop
         Append (Result, "   " & Get_Name (Local_Vars.Table (L)).all & " ");

         Declaration_Added := False;

         --  We will remove the previous declaration unless it is a parameter

         if Is_Parameter_Of (Local_Vars.Table (L)) = null then
            declare
               Decl : constant String :=
                 Get_Declaration (Kernel, Local_Vars.Table (L));
            begin
               if Decl /= "" then
                  Trace (Me, "Removing declaration of "
                         & Get_Name (Local_Vars.Table (L)).all);
                  To_Delete.Insert
                    ((Start  =>
                        Get_Line (Get_Declaration_Of (Local_Vars.Table (L))),
                      Lines  => Lines_Count (Decl)));

                  Append (Result, Decl);
                  Declaration_Added := True;
               end if;
            end;
         end if;

         if not Declaration_Added then
            Typ := Get_Type_Of (Local_Vars.Table (L));
            if Typ = null then
               Insert (Kernel,
                       Text => -"Couldn't find the type of "
                       & Get_Name (Local_Vars.Table (L)).all,
                       Mode => Error);
               Method_Decl := Null_Unbounded_String;
               Method_Body := Null_Unbounded_String;
               return;
            end if;

            Append (Result, ": " & Get_Name (Typ).all & ";");
         end if;

         Append (Result, ASCII.LF);
      end loop;

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Typ := Get_Type_Of (First_Out_Param);
         Append (Result, "   " & Get_Name (First_Out_Param).all
                 & " : " & Get_Name (Typ).all & ";" & ASCII.LF);
      end if;

      Append (Result, "begin" & ASCII.LF & "   ");
      Append (Result, String'
        (Editor.Get_Chars
           (Editor.New_Location (Context.Line_Start, 1),
            Editor.New_Location (Context.Line_End, 1).End_Of_Line)));

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Append (Result, "   return "
           & Get_Name (First_Out_Param).all & ";" & ASCII.LF);
      end if;

      Append (Result, "end " & Name & ";" & ASCII.LF);

      if Out_Params_Count = 1
        and then In_Out_Params_Count = 0
      then
         Method_Call := To_Unbounded_String
           (Get_Name (First_Out_Param).all & " := ");
      end if;

      Append (Method_Call, Name);
      if In_Params_Count + In_Out_Params_Count > 0
        or else Out_Params_Count > 1
      then
         Append (Method_Call, " (");

         for P in Parameter_Arrays.First .. Last (Params) loop
            if Params.Table (P).PType /= Out_Parameter
              or else Out_Params_Count /= 1
              or else In_Out_Params_Count /= 0
            then
               Append (Method_Call,
                       Get_Name (Params.Table (P).Parameter).all);

               if P /= Last (Params) then
                  Append (Method_Call, ", ");
               end if;
            end if;
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
               Decl_Line := Constructs.Current.Sloc_Start.Line - 1;
            end if;

            if Constructs.Current.Sloc_Start.Line <= Line
              and then Constructs.Current.Sloc_End.Line > Line
            then
               Line := Constructs.Current.Sloc_Start.Line - 1;
            end if;
         end if;

         Constructs.Current := Constructs.Current.Next;
      end loop;

      Free (Constructs);

      --  Insert the body before the decl, so that if they are inserted at the
      --  same line, they occur with the decl first
      Inserted := Insert_Text (Kernel, In_File, Line, 1, Method_Body, True);

      if Context.Use_Separate_Decl then
         Inserted :=
           Insert_Text (Kernel, In_File, Decl_Line, 1, Method_Decl, True);
      end if;
   end Insert_New_Method;

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
      Decl, Location : File_Location;
      Source   : constant Source_File :=
        Get_Or_Create (Get_Database (Kernel), Context.File);
      Entity   : Extracted_Entity;
      Is_Global       : Boolean;

      Struct : Structured_File_Access;
      ERef   : Entity_Reference_Details;

   begin
      --  This code might require loading a file, so we only freeze afterward
      Find_All_Entities_In_File (Iter, Source);
      Freeze (Get_Database (Kernel), Mode => No_Create_Or_Update);

      Struct := Get_Or_Create
        (Get_Construct_Database (Kernel), File => Context.File);
      Update_Contents (Struct);

      declare
         Scopes : constant Lines_To_Scope := Compute_Scopes (Source);
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
            Thaw (Get_Database (Kernel));
            return;
         end if;

         while not At_End (Iter) loop
            Entity := (Entity => Get (Iter),
                       Flags  => (others => False));

            Decl := Get_Declaration_Of (Entity.Entity);

            --  An entity is "global" (ie does not need an entry in the
            --  parameter list) if it is defined in another file, or in the
            --  current file at library level. The call to Get_Caller requires
            --  parsing an ALI file so doing it systematically means parsing a
            --  lot more ALI files than really necessary.
            --  We however need to accept in case the entity is declared in the
            --  '.ads' but we are working in the '.adb' (for instance the
            --  parameter to a subprogram).

            Is_Global := Get_LI (Decl.File) /= Get_LI (Source);
            if not Is_Global then
               Caller := Get_Caller (Declaration_As_Reference (Entity.Entity));
               Is_Global := Caller = null or else not Is_Subprogram (Caller);
            end if;

            if Active (Me) then
               Trace (Me, "Entity=" & Get_Name (Entity.Entity).all
                      & " Is_Global=" & Is_Global'Img);
            end if;

            if not Is_Global then
               Find_All_References
                 (Ref_Iter, Entity.Entity, In_File => Source);
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
                        if Location.Line /= Decl.Line then
                           if Is_Write_Reference (Get_Kind (Ref)) then
                              Entity.Flags (Flag_Modified_Before) := True;
                           elsif Is_Read_Reference (Get_Kind (Ref)) then
                              Entity.Flags (Flag_Read_Before) := True;
                           end if;

                        elsif Is_Parameter_Of (Entity.Entity) /= null then
                           --  A parameter declaration is considered as a read
                           --  reference, since it will provide an initial
                           --  value
                           Entity.Flags (Flag_Modified_Before) := True;

                        --  If we have an initial value, assume the variable is
                        --  modified. This is not optimal (for instance in the
                        --  case of a Local_Variable we could simply copy the
                        --  declaration, but if the variable ends up in a
                        --  parameter we need to keep the initial value.
                        --  ??? Don't do this if it ends up as a local_var

                        elsif Get_Initial_Value (Kernel, Entity.Entity)
                          /= ""
                        then
                           Entity.Flags (Flag_Modified_Before) := True;
                        end if;

                     else
                        --  A reference within the extracted code

                        if Active (Me) then
                           Trace (Me, "Reference in extracted code "
                                  & Get_Kind (Ref)'Img);
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

                  Extracted_Entity_Arrays.Append (Context.Entities, Entity);
               end if;
            end if;

            Next (Iter);
         end loop;
      end;

      Destroy (Iter);
      Thaw (Get_Database (Kernel));

   exception
      when others =>
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
            Finish_Undo_Group (Kernel, Context.File);
            return Failure;
         end if;
      else
         return Failure;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Thaw (Get_Database (Kernel));
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
      Check, Separate_Decl  : Gtk_Check_Button;

      Extract : Extract_Context;

      Result : Command_Return_Type := Failure;
      pragma Unreferenced (Command, Button);

   begin
      Extract := (File              => File_Information (Context.Context),
                  Line_Start        => <>,
                  Line_End          => <>,
                  Parent            => <>,
                  Use_In_Keyword    => False,
                  Use_Separate_Decl => True,
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

      Gtk_New (Check, -"Use ""in"" keyword");
      Pack_Start (Get_Vbox (Dialog), Check, Expand => False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Get_Kernel (Context.Context)).all,
         Key           => "Refactoring_Use_In_Keyword",
         Default_Value => False);
      Associate (Get_History (Get_Kernel (Context.Context)).all,
                "Refactoring_Use_In_Keyword", Check);

      Gtk_New (Separate_Decl, -"Create declaration for subprogram");
      Pack_Start (Get_Vbox (Dialog), Separate_Decl, Expand => False);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Get_Kernel (Context.Context)).all,
         Key           => "Refactoring_Create_Declaration",
         Default_Value => True);
      Associate (Get_History (Get_Kernel (Context.Context)).all,
                "Refactoring_Create_Declaration", Separate_Decl);

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Extract.Use_In_Keyword    := Get_Active (Check);
         Extract.Use_Separate_Decl := Get_Active (Separate_Decl);

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
                  Line_Start        => Nth_Arg (Data, 2),
                  Line_End          => Nth_Arg (Data, 3),
                  Parent            => <>,
                  Use_In_Keyword    => True,
                  Use_Separate_Decl => True,
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
