------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Basic_Types;            use Basic_Types;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Entities.Queries;       use Entities, Entities.Queries;
with Glib;                   use Glib;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GNATCOLL.Symbols;       use GNATCOLL.Symbols;
with GPS.Editors;            use GPS.Editors;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple;
use GPS.Kernel.Messages, GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
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
with Language.Tree.Database; use Language.Tree.Database;
with Refactoring.Services;   use Refactoring.Services;
with Refactoring.Performers; use Refactoring.Performers;
with Traces;                 use Traces;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

package body Refactoring.Subprograms is

   Me : constant Debug_Handle := Create ("Refactor.Subprograms");

   type Extract_Method_Command is new Interactive_Command with null record;

   overriding function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Extract Method" menu

   type Extracted_Entity is record
      Entity : Entity_Information;
      Decl   : Entity_Declaration;
      Flags  : Entity_References_Flags;
   end record;
   --  An entity found in the code to be extracted, and various information
   --  about its usage

   package Extracted_Entity_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Extracted_Entity);

   type Extract_Context is record
      Code     : Range_Of_Code;
      --  Which code do we want to extract ?

      Source   : Source_File;
      --  The file in which the extracted code is at the start

      Parent   : Entity_Information;
      --  The subprogram that contained the extracted code before the
      --  refactoring

      Entities : Extracted_Entity_Lists.List;
      --  The entities referenced in the extracted code
   end record;
   Invalid_Context : constant Extract_Context :=
     (Empty_Range_Of_Code, null, null, Entities => <>);

   procedure Free (Context : in out Extract_Context);
   --  Free the memory used by the context

   procedure Compute_Context_Entities
     (Context : in out Extract_Context);
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
     (Context             : Extract_Context;
      Params              : out Parameters'Class;
      Local_Vars          : out Extracted_Entity_Lists.List'Class);
   --  From the list of entities in the context, compute those that should
   --  become parameters and those that should be local variables

   use Parameter_Lists, Entity_Information_Arrays, Extracted_Entity_Lists;

   procedure Generate_Extracted_Method
     (Name        : String;
      Context     : Extract_Context;
      Local_Vars  : out Extracted_Entity_Lists.List;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String);
   --  Generate the code of the new method

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

   procedure Prepare_Code
     (Code          : String;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Code_Start    : out Natural;
      Code_End      : out Natural);
   --  Prepares the extracted code for pretty printing to its new location.
   --  In particular, this skips leading and trailing white spaces, and if the
   --  code starts with a comment it separates the comment from the code

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
         Skip_Blanks_Backward (Code, Comment_End);
      end if;

      Code_End := Code'Last;
      Skip_Blanks_Backward (Code, Code_End);
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
     (Context             : Extract_Context;
      Params              : out Parameters'Class;
      Local_Vars          : out Extracted_Entity_Lists.List'Class)
   is
      type Parameter_Count is array (Parameter_Type) of Natural;
      Count  : Parameter_Count := (others => 0);
      Entity : Entity_Information;
      Flags  : Entity_References_Flags;
      E      : Extracted_Entity;
      P      : Extracted_Entity_Lists.Cursor := Context.Entities.First;
      Struct : Structured_File_Access;
      Offset : String_Index_Type;
   begin
      Struct := Get_Or_Create
        (Context.Code.Context.Construct_Db, File => Context.Code.File);
      Offset := To_String_Index (Struct, Context.Code.From_Line, 1);

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
                   Is_Tagged => Accepts_Primitive_Ops
                     (Context.Code.Context, Entity, Offset),
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
                          (Context.Code.Context, Entity, Offset),
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
                          (Context.Code.Context, Entity, Offset),
                      PType     => In_Out_Parameter));
                  Count (In_Out_Parameter) := Count (In_Out_Parameter) + 1;
               end if;

            elsif Flags (Flag_Read_After)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               --  Not set before the call, but needed after
               Params.List.Append
                 ((Parameter => E,
                   Is_Tagged => Accepts_Primitive_Ops
                     (Context.Code.Context, Entity, Offset),
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
                  (Context.Code.Context, Element (Param).PType));

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
           (Get (Get_Name (Self.Last_Out_Param)).all & " := ");
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
                       Get (Get_Name (Element (Param).Parameter.Entity)).all);

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
     (Name        : String;
      Context     : Extract_Context;
      Local_Vars  : out Extracted_Entity_Lists.List;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String)
   is
      Editor  : constant Editor_Buffer'Class :=
        Context.Code.Context.Buffer_Factory.Get (Context.Code.File);
      Code    : constant String :=
        Editor.Get_Chars
          (Editor.New_Location (Context.Code.From_Line, 1),
           Editor.New_Location (Context.Code.To_Line, 1).End_Of_Line);

      Params               : Parameters;
      Typ                  : Entity_Information;
      Code_Start, Code_End : Integer;
      Comment_Start, Comment_End : Integer;
      PList, Local, Returns : Unbounded_String;
      Newline_Before_Is : Boolean := False;
   begin
      Prepare_Code (Code, Comment_Start, Comment_End, Code_Start, Code_End);
      Compute_Params_And_Vars (Context, Params, Local_Vars);
      Params.Sort;
      PList := Params.Generate (Context);

      if Params.Is_Function then
         Typ := Get_Type_Of (Params.Last_Out_Param);
         Returns := To_Unbounded_String
           (" return " & Get (Get_Name (Typ)).all);
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

      Method_Body := Method_Decl;
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
         Append (Method_Body,
                 "   " & Get (Get_Name (Params.Last_Out_Param)).all
                 & " : " & Get (Get_Name (Typ)).all & ";" & ASCII.LF);
      end if;

      Append (Method_Body, "begin" & ASCII.LF & "   ");
      Append (Method_Body, Code (Code_Start .. Code_End));
      Append (Method_Body, ASCII.LF);

      if Params.Is_Function then
         Append (Method_Body, "   return "
                 & Get (Get_Name (Params.Last_Out_Param)).all
                 & ";" & ASCII.LF);
      end if;

      Append (Method_Body, "end " & Name & ";" & ASCII.LF);

      Method_Call := Params.Generate_Method_Call (Name);
   end Generate_Extracted_Method;

   ------------------------------
   -- Compute_Context_Entities --
   ------------------------------

   procedure Compute_Context_Entities
     (Context : in out Extract_Context)
   is
      Editor  : constant Editor_Buffer'Class :=
        Context.Code.Context.Buffer_Factory.Get (Context.Code.File);

      procedure Callback
        (Entity : Entity_Information; Flags : Entity_References_Flags);
      --  Called when an entity used in the range of text has been found

      procedure Callback
        (Entity : Entity_Information;
         Flags  : Entity_References_Flags)
      is
         Decl : Entity_Declaration;
      begin
         Decl := Get_Declaration (Context.Code.Context, Entity);

         --  Marks are used to remove the declaration for the entity, which
         --  will not happen unless the entity is declared in the current
         --  file.
         if Get_Filename (Get_Declaration_Of (Entity).File) =
           Context.Code.File
         then
            Create_Marks (Decl, Editor);
         end if;

         Context.Entities.Append
           (Extracted_Entity'
              (Entity => Entity,
               Decl   => Decl,
               Flags  => Flags));
      end Callback;

      Success : Boolean;

   begin
      Context.Code.For_All_Variable_In_Range
        (Callback'Access, Omit_Library_Level => True, Success => Success);
      if not Success then
         Context := Invalid_Context;
      end if;
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
      Local_Vars : Extracted_Entity_Lists.List;
      Iter       : Extracted_Entity_Lists.Cursor;
      Result     : Command_Return_Type;
      E          : Extracted_Entity;
      Category   : constant String :=
        -"Refactoring - extract subprogram " & Method_Name;
   begin
      if Context = Invalid_Context then
         Trace (Me, "Extract_Method: Invalid context");
         return Failure;
      end if;

      declare
         Buffer : constant Editor_Buffer'Class :=
           Context.Code.Context.Buffer_Factory.Get (Context.Code.File);
      begin
         Buffer.Start_Undo_Group;

         Generate_Extracted_Method
           (Name        => Method_Name,
            Context     => Context,
            Local_Vars  => Local_Vars,
            Method_Decl => Method_Decl,
            Method_Body => Method_Body,
            Method_Call => Method_Call);

         if Method_Body /= Null_Unbounded_String then
            declare
               Line_Start : constant Editor_Mark'Class :=
                 Buffer.New_Location (Context.Code.From_Line, 1).Create_Mark;
               Line_End : constant Editor_Mark'Class :=
                 Buffer.New_Location (Context.Code.To_Line, 1)
                 .End_Of_Line.Create_Mark;
            begin
               Iter := Local_Vars.First;

               --  Will remove the previous declaration unless it is a
               --  parameter

               while Has_Element (Iter) loop
                  E := Element (Iter);
                  if Is_Parameter_Of (E.Entity) = null then
                     E.Decl.Remove;
                  end if;
                  Next (Iter);
               end loop;

               Delete_Text
                 (Kernel      => Kernel,
                  In_File     => Context.Code.File,
                  Line_Start  => Line_Start.Line,
                  Line_End    => Line_End.Line);

               if Insert_Text
                 (Context    => Context.Code.Context,
                  In_File    => Context.Code.File,
                  Line       => Line_Start.Line,
                  Column     => 1,
                  Text       => To_String (Method_Call),
                  Indent     => True)
               then
                  Create_Simple_Message
                    (Get_Messages_Container (Kernel),
                     Category,
                     Context.Code.File, Line_Start.Line,
                     1, -"Extracted subprogram call inserted",
                     0,
                     (Editor_Side => True, Locations => True));

                  Insert_Subprogram_Body
                    (Context.Code.Context,
                     In_File     => Context.Code.File,
                     Name        => Method_Name,
                     Before_Line => Line_Start.Line,
                     Code        => To_String (Method_Body),
                     Category    => Category);
                  Insert_Subprogram_Declaration
                    (Context.Code.Context,
                     In_File  => Context.Code.File,
                     Decl     => To_String (Method_Decl),
                     Category => Category);

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
      end;

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
      From_Line, To_Line : Integer;

      Extract : Extract_Context;

      Result : Command_Return_Type := Failure;
      pragma Unreferenced (Command, Button);

   begin
      Get_Area (Context.Context, From_Line, To_Line);
      Extract := (Code => Create_Range
                  (Context => Get_Kernel (Context.Context).Refactoring_Context,
                   File    => File_Information (Context.Context),
                   From_Line => From_Line,
                   To_Line   => To_Line),
                  Source         => null,
                  Parent         => <>,
                  Entities       => <>);
      Compute_Context_Entities (Extract);

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
      Context : Extract_Context;
   begin
      Context := (Code       => Create_Range
                  (Context   => Get_Kernel (Data).Refactoring_Context,
                   File      => Create (Nth_Arg (Data, 1)),
                   From_Line => Nth_Arg (Data, 2),
                   To_Line   => Nth_Arg (Data, 3)),
                  Source         => null,
                  Parent         => <>,
                  Entities       => <>);
      Compute_Context_Entities (Context);

      if Extract_Method
        (Kernel      => Get_Kernel (Data),
         Method_Name => Nth_Arg (Data, 4, "New_Method"),
         Context     => Context) /= Success
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
