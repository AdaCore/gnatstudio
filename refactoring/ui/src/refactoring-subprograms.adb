------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Basic_Types;                     use Basic_Types;
with Commands.Interactive;            use Commands, Commands.Interactive;
with Glib;                            use Glib;
with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.Xref;                   use GNATCOLL.Xref;
with GPS.Editors;                     use GPS.Editors;
with GPS.Intl;                        use GPS.Intl;
with GPS.Kernel.Actions;              use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;             use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                  use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple;
use GPS.Kernel.Messages, GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;              use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;           use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;          use GPS.Kernel.Preferences;
with GPS.Kernel.Project;              use GPS.Kernel.Project;
with GPS.Kernel.Scripts;              use GPS.Kernel.Scripts;
with GPS.Kernel;                      use GPS.Kernel;
with Gtk.Box;                         use Gtk.Box;
with Gtk.Dialog;                      use Gtk.Dialog;
with Gtk.GEntry;                      use Gtk.GEntry;
with Gtk.Label;                       use Gtk.Label;
with Gtk.Stock;                       use Gtk.Stock;
with Gtk.Widget;                      use Gtk.Widget;
with Language;                        use Language;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Language.Tree.Database;          use Language.Tree.Database;
with Refactoring.Services;            use Refactoring.Services;
with Refactoring.Performers;          use Refactoring.Performers;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GPS.Dialogs;                     use GPS.Dialogs;
with Xref;                            use Xref;

package body Refactoring.Subprograms is

   Me : constant Trace_Handle := Create ("GPS.REFACTORING.Subprograms");

   type Extract_Method_Command is new Interactive_Command with null record;

   overriding function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Extract Method" menu

   type Extracted_Entity is record
      Entity : Root_Entity_Ref;
      Decl   : Refactoring.Services.Entity_Declaration;
      Flags  : Entity_References_Flags;
   end record;
   --  An entity found in the code to be extracted, and various information
   --  about its usage

   package Extracted_Entity_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Extracted_Entity);

   type Extract_Context is record
      Code     : Range_Of_Code;
      --  Which code do we want to extract ?

      Source   : Virtual_File;
      --  The file in which the extracted code is at the start

      Entities : Extracted_Entity_Lists.List;
      --  The entities referenced in the extracted code
   end record;
   Invalid_Context : constant Extract_Context :=
     (Empty_Range_Of_Code, No_File, others => <>);

   procedure Free (Context : in out Extract_Context);
   --  Free the memory used by the context

   procedure Compute_Context_Entities
     (Context : in out Extract_Context;
      Db      : General_Xref_Database);
   --  Compute all entities referenced in the context.
   --  Returns Invalid_Context if something prevents the refactoring (an error
   --  message has already been displayed in that case).

   type Separate_Method_Command is new Interactive_Command with null record;

   overriding function Execute
     (Command : access Separate_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Separate Method" menu

   ----------------
   -- Parameters --
   ----------------

   type Parameter_Description is record
      Parameter : Extracted_Entity;
      Is_Tagged : Boolean;
      PType     : Parameter_Kind;
   end record;
   package Parameter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Parameter_Description);

   type Parameters is tagged record
      List           : Parameter_Lists.List;
      Last_Out_Param : Root_Entity_Ref;
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
     (Self : Parameters'Class;
      Name : String;
      Db   : General_Xref_Database) return Unbounded_String;
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

   use Parameter_Lists, Extracted_Entity_Lists;

   procedure Generate_Extracted_Method
     (Name        : String;
      Context     : Extract_Context;
      Db          : General_Xref_Database;
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

   type Separate_Context_Item is new Context_Item with record
      From : Natural := 0;
      To   : Natural := 0;
   end record;
   type Separate_Context_Item_Access is access all Separate_Context_Item;

   type Is_Top_Level_Subprogram is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Top_Level_Subprogram;
      Context : Selection_Context) return Boolean;
   --  True if the Context contains package level subprogramm.

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
      Dispatchs : Parameter_Lists.List;  --  The dispatching parameters
      Normals   : Parameter_Lists.List;  --  Non dispatching, non out params
      Outs      : Parameter_Lists.List;  --  Out parameters

   begin

      --  Put the dispatching parameter first, if any. By convention, we also
      --  put "Self" or "This" first if we can't find another dispatching
      --  parameter.
      --  Needs to be done after the "out" parameter, in case the dispatching
      --  parameter is also out
      --  Put all "out" parameters last, so that we read from input parameters
      --  to output parameters

      --  ??? Put parameters with default values last
      --  We currently do not create default values, so this is irrelevant

      for E of Self.List loop
         if E.Is_Tagged then
            Dispatchs.Append (E);
         else
            case E.PType is
            when Out_Parameter | In_Out_Parameter =>
               Outs.Append (E);
            when In_Parameter | Access_Parameter =>
               Normals.Append (E);
            end case;
         end if;
      end loop;

      Self.List.Clear;

      for E of Dispatchs loop
         Self.List.Append (E);
      end loop;

      for E of Normals loop
         Self.List.Append (E);
      end loop;

      for E of Outs loop
         Self.List.Append (E);
      end loop;
   end Sort;

   -----------------------------
   -- Compute_Params_And_Vars --
   -----------------------------

   procedure Compute_Params_And_Vars
     (Context             : Extract_Context;
      Params              : out Parameters'Class;
      Local_Vars          : out Extracted_Entity_Lists.List'Class)
   is
      type Parameter_Count is array (Parameter_Kind) of Natural;
      Count  : Parameter_Count := (others => 0);
      Flags  : Entity_References_Flags;
      E      : Extracted_Entity;
      P      : Extracted_Entity_Lists.Cursor := Context.Entities.First;
      Struct : Structured_File_Access;
      Offset : String_Index_Type;
   begin
      Struct := Get_Or_Create
        (Context.Code.Context.Db.Constructs, File => Context.Code.File);
      Offset := To_String_Index (Struct, Context.Code.From_Line, 1);

      Count := (others => 0);
      Params.Last_Out_Param.Replace_Element (No_Root_Entity);

      while Has_Element (P) loop
         E := Element (P);

         Flags  := E.Flags;

         Trace (Me, "Compute Params: Entity="
                & Get_Name (E.Entity.Element)
                & " Flags: read=" & Flags (Flag_Read)'Img
                & " modified=" & Flags (Flag_Modified)'Img
                & " modified_before=" & Flags (Flag_Modified_Before)'Img
                & " ref_outside_parent="
                & Flags (Flag_Ref_Outside_Parent)'Img);

         if Flags (Flag_Read) and then not Flags (Flag_Modified) then
            if Flags (Flag_Modified_Before)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               Params.List.Append
                 ((Parameter => E,
                   Is_Tagged => Accepts_Primitive_Ops
                     (Context.Code.Context, E.Entity.Element, Offset),
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
               Params.List.Append
                 ((Parameter => E,
                   Is_Tagged =>
                     Accepts_Primitive_Ops
                       (Context.Code.Context, E.Entity.Element, Offset),
                   PType     => In_Out_Parameter));
               Count (In_Out_Parameter) := Count (In_Out_Parameter) + 1;

            elsif Flags (Flag_Read_After)
              or else Flags (Flag_Ref_Outside_Parent)
            then
               --  Not set before the call, but needed after
               Params.List.Append
                 ((Parameter => E,
                   Is_Tagged => Accepts_Primitive_Ops
                     (Context.Code.Context, E.Entity.Element, Offset),
                   PType     => Out_Parameter));
               Count (Out_Parameter) := Count (Out_Parameter) + 1;
               Params.Last_Out_Param.Replace_Element (E.Entity.Element);

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
         Params.Last_Out_Param.Replace_Element (No_Root_Entity);
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
                 and then Element (Next (Param)).Parameter.Entity.Element /=
                 Self.Last_Out_Param.Element
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
     (Self : Parameters'Class;
      Name : String;
      Db   : General_Xref_Database) return Unbounded_String
   is
      pragma Unreferenced (Db);
      Param : Parameter_Lists.Cursor;
      Method_Call : Unbounded_String;
   begin
      if Self.Is_Function then
         Method_Call := To_Unbounded_String
           (Get_Name (Self.Last_Out_Param.Element) & " := ");
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
                       Get_Name (Element (Param).Parameter.Entity.Element));

               if Param /= Self.List.Last
                 and then Element (Next (Param)).Parameter.Entity.Element /=
                 Self.Last_Out_Param.Element
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
      Db          : General_Xref_Database;
      Local_Vars  : out Extracted_Entity_Lists.List;
      Method_Decl : out Unbounded_String;
      Method_Body : out Unbounded_String;
      Method_Call : out Unbounded_String)
   is
      Editor  : constant Editor_Buffer'Class :=
        Context.Code.Context.Buffer_Factory.Get (Context.Code.File);
      Code    : constant String :=
        Editor.Get_Chars
          (Editor.New_Location_At_Line (Context.Code.From_Line),
           Editor.New_Location_At_Line (Context.Code.To_Line).End_Of_Line);

      Params               : Parameters;
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
         Returns := To_Unbounded_String
           (" return " & Get_Name
              (Get_Type_Of (Params.Last_Out_Param.Element)));
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
         Append
           (Method_Body,
            "   " & Get_Name (Params.Last_Out_Param.Element)
            & " : " & Get_Name (Get_Type_Of (Params.Last_Out_Param.Element))
            & ";" & ASCII.LF);
      end if;

      Append (Method_Body, "begin" & ASCII.LF & "   ");
      Append (Method_Body, Code (Code_Start .. Code_End));
      Append (Method_Body, ASCII.LF);

      if Params.Is_Function then
         Append (Method_Body, "   return "
                 & Get_Name (Params.Last_Out_Param.Element) & ";" & ASCII.LF);
      end if;

      Append (Method_Body, "end " & Name & ";" & ASCII.LF);

      Method_Call := Params.Generate_Method_Call (Name, Db => Db);
   end Generate_Extracted_Method;

   ------------------------------
   -- Compute_Context_Entities --
   ------------------------------

   procedure Compute_Context_Entities
     (Context : in out Extract_Context;
      Db      : General_Xref_Database)
   is
      Editor  : constant Editor_Buffer'Class :=
        Context.Code.Context.Buffer_Factory.Get (Context.Code.File);

      procedure Callback
        (Entity : Root_Entity'Class; Flags : Entity_References_Flags);
      --  Called when an entity used in the range of text has been found

      procedure Callback
        (Entity : Root_Entity'Class;
         Flags  : Entity_References_Flags)
      is
         Decl : Refactoring.Services.Entity_Declaration;
         H    : Root_Entity_Ref;
      begin
         Decl := Get_Declaration (Context.Code.Context, Entity);

         --  Marks are used to remove the declaration for the entity, which
         --  will not happen unless the entity is declared in the current
         --  file.
         if Get_Declaration (Entity).Loc.File = Context.Code.File then
            Create_Marks (Decl, Editor);
         end if;

         H.Replace_Element (Entity);

         Context.Entities.Append
           (Extracted_Entity'
              (Entity => H,
               Decl   => Decl,
               Flags  => Flags));
      end Callback;

      Success : Boolean;

   begin
      Context.Code.For_All_Variable_In_Range
        (Db, Callback'Access,
         Omit_Library_Level => True, Success => Success);
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
         G      : constant Group_Block :=
           Buffer.New_Undo_Group;
      begin
         Generate_Extracted_Method
           (Name        => Method_Name,
            Context     => Context,
            Db          => Kernel.Databases,
            Local_Vars  => Local_Vars,
            Method_Decl => Method_Decl,
            Method_Body => Method_Body,
            Method_Call => Method_Call);

         if Method_Body /= Null_Unbounded_String then
            declare
               Line_Start : Editor_Mark'Class :=
                 Buffer.New_Location_At_Line
                   (Context.Code.From_Line).Create_Mark;
               Line_End : Editor_Mark'Class :=
                 Buffer.New_Location_At_Line (Context.Code.To_Line)
                 .End_Of_Line.Create_Mark;
            begin
               Iter := Local_Vars.First;

               --  Will remove the previous declaration unless it is a
               --  parameter

               while Has_Element (Iter) loop
                  E := Element (Iter);
                  if Is_Parameter_Of (E.Entity.Element) = No_Root_Entity then
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
                     Unspecified,
                     Side_And_Locations);

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
      end;

      return Result;

   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Extract_Method;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Extract_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Dialog : GPS_Dialog;
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
                   File      => File_Information (Context.Context),
                   Project   => Project_Information (Context.Context),
                   From_Line => From_Line,
                   To_Line   => To_Line),
                  Source     => No_File,
                  Entities   => <>);
      Compute_Context_Entities
        (Extract, Db => Get_Kernel (Context.Context).Databases);

      if Extract = Invalid_Context then
         --  Nothing to do, and error message already printed
         Free (Extract);
         return Failure;
      end if;

      Gtk_New (Dialog,
               Title  => -"Extract Method",
               Kernel => Get_Kernel (Context.Context),
               Flags  => Destroy_With_Parent or Modal);
      Gtk_New (Label, -"Name of the new subprogram:");
      Pack_Start (Get_Content_Area (Dialog), Label, Expand => False);

      Gtk_New (Ent);
      Set_Text (Ent, -"New_Method");
      Select_Region (Ent, 0, -1);
      Set_Activates_Default (Ent, True);
      Pack_Start (Get_Content_Area (Dialog), Ent, Expand => False);

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
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      File    : Virtual_File;
      Project : Project_Type;

   begin
      --  Use the first possible project, since we have no other context
      File := Create (Nth_Arg (Data, 1));
      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Kernel.Registry.Tree.Info_Set (File).First_Element);
      begin
         Project := F_Info.Project;
      end;

      Context := (Code       => Create_Range
                  (Context   => Kernel.Refactoring_Context,
                   File      => File,
                   Project   => Project,
                   From_Line => Nth_Arg (Data, 2),
                   To_Line   => Nth_Arg (Data, 3)),
                  Source         => No_File,
                  Entities       => <>);
      Compute_Context_Entities (Context, Db => Get_Kernel (Data).Databases);

      if Extract_Method
        (Kernel      => Get_Kernel (Data),
         Method_Name => Nth_Arg (Data, 4, "New_Method"),
         Context     => Context) /= Success
      then
         Set_Error_Msg (Data, "Couldn't extract method");
      end if;

      Free (Context);
   end Command_Handler;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Top_Level_Subprogram;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

   begin
      if not Has_Entity_Name_Information (Context) then
         return False;
      end if;

      declare
         Entity : constant Root_Entity'Class := Get_Entity (Context);
      begin
         if Entity = No_Root_Entity
           or else not Is_Subprogram (Entity)
         then
            --  No Entity or it is not a subprogram
            return False;
         end if;

         --  Checks whether the Entity is a subprogram body,
         --  defined at a package level
         declare
            Loc : constant General_Location := Get_Body (Entity);
         begin
            if Loc.File /= File_Information (Context)
              or else Loc.Line > GPS.Kernel.Contexts.Line_Information (Context)
            then
               --  It is not a body
               return False;
            end if;

            --  Checking Entity's parent
            declare
               Current : Semantic_Tree_Iterator'Class :=
                 Get_Kernel (Context).Get_Abstract_Tree_For_File
                 ("EDIT", File_Information (Context)).Root_Iterator;
            begin
               while Has_Element (Current) loop
                  declare
                     Node : constant Semantic_Node'Class := Element (Current);
                  begin
                     if Node.Category in Cat_Procedure .. Cat_Method
                       and then Node.Name = Get_Name (Get_Entity (Context))
                       and then not Node.Is_Declaration
                     then
                        --  It is a semantic node which belong to the Entity
                        if Node.Parent = No_Semantic_Node
                          or else Node.Parent.Category /= Cat_Package
                        then
                           --  Entity's parent is not a package
                           return False;
                        end if;

                        --  Is Entity's parent a top level package?
                        if Node.Parent.Parent = No_Semantic_Node then
                           declare
                              Item : constant access Separate_Context_Item :=
                                new Separate_Context_Item;
                           begin
                              --  Fill information and add it into the Context
                              --  for a future usage in
                              --  Separate_Method_Command.Execurt

                              Item.From := Node.Sloc_Start.Line;
                              Item.To   := Node.Sloc_End.Line;
                              Item.Text := To_Unbounded_String
                                (Name (Node.Parent));

                              Set_Refactoring_Variable
                                (Context, Context_Item_Access (Item));
                           end;

                           return True;
                        else
                           --  Entity's parent is not a top level package
                           return False;
                        end if;
                     end if;
                  end;

                  Next (Current);
               end loop;
            end;
         end;
      end;

      return False;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Separate_Method_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Item : constant Separate_Context_Item_Access :=
        Separate_Context_Item_Access
          (Get_Refactoring_Variable (Context.Context));
      XEntity : constant Root_Entity'Class := Get_Entity (Context.Context);
      Entity  : constant Language.Tree.Database.Entity_Access :=
        Get_Entity_Access (Kernel.Refactoring_Context, XEntity);
      Editor : constant Editor_Buffer'Class :=
      Get_Buffer_Factory (Kernel).Get (File_Information (Context.Context));
      Loc_Start : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Item.From);
      Loc_End   : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Item.To).End_Of_Line;

      Struct : Structured_File_Access;
      Spec   : Ada.Strings.Unbounded.Unbounded_String;
      Impl   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Struct := Get_Or_Create
        (Db   => Kernel.Refactoring_Context.Db.Constructs,
         File => File_Information (Context.Context));
      Update_Contents (Struct);

      Spec := To_Unbounded_String
        ((if Returned_Type (XEntity) = No_Root_Entity
         then "   procedure "
         else "   function ") & Get_Name (XEntity) & " " &
           Get_Tree_Language (Struct).Get_Profile (Entity) & " is separate;" &
           ASCII.LF);

      Impl := To_Unbounded_String (Editor.Get_Chars (Loc_Start, Loc_End));
      Editor.Start_Undo_Group;
      Editor.Delete (Loc_Start, Loc_End);
      Editor.Insert (Loc_Start, To_String (Spec));
      Editor.Indent (Loc_Start, Loc_Start);
      Editor.Finish_Undo_Group;

      declare
         New_Editor : constant Editor_Buffer'Class :=
           Get_Buffer_Factory (Kernel).Get
           (Create_From_Dir
              (Dir (File_Information (Context.Context)),
               Get_Project (Kernel).File_From_Unit
               (Unit_Name       => Ada.Characters.Handling.To_Lower
                (To_String (Item.Text) & "." & Get_Name (XEntity)),
                Part            => GNATCOLL.Projects.Unit_Body,
                File_Must_Exist => False,
                Language        => "ada")),
            Force => True, Open_Buffer => True, Open_View => True);
      begin
         New_Editor.Insert
           (New_Editor.End_Of_Buffer,
            "separate (" & To_String (Item.Text) & ")" & ASCII.LF &
              To_String (Impl));
         New_Editor.Indent
           (New_Editor.Beginning_Of_Buffer, New_Editor.End_Of_Buffer);
      end;

      return Success;
   end Execute;

   --------------------------
   -- Register_Refactoring --
   --------------------------

   procedure Register_Refactoring
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      F : Action_Filter;
   begin
      F := new Is_Area_Context;
      Register_Action
        (Kernel, "extract subprogram",
         Command     => new Extract_Method_Command,
         Description => -"Move selected code into its own subprogram",
         Filter  => F
            and Create (Module => "Source_Editor")
            and Create (Language => "ada"),
         Category     => -"Refactoring",
         For_Learning => True);
      Register_Contextual_Menu
        (Kernel,
         Label  => "Refactoring/Extract Subprogram",
         Action => "extract subprogram");

      Register_Action
        (Kernel, "separate subprogram",
         Command     => new Separate_Method_Command,
         Description =>
           -"Move selected subprogram into its own separate package",
         Filter  => Create (Module => "Source_Editor")
         and Create (Language => "ada")
         and new Is_Top_Level_Subprogram,
         Category     => -"Refactoring",
         For_Learning => True);
      Register_Contextual_Menu
        (Kernel,
         Label  => "Refactoring/Separate Subprogram %e",
         Action => "separate subprogram");

      if Active (Testsuite_Handle) then
         Kernel.Scripts.Register_Command
           ("extract_method", 3, 4, Command_Handler'Access);
      end if;
   end Register_Refactoring;

end Refactoring.Subprograms;
