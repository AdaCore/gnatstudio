------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Commands;                       use Commands;
with Commands.Interactive;           use Commands.Interactive;
with Debugger;                       use Debugger;
with GNATCOLL.JSON;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;

with GPS.Default_Styles;             use GPS.Default_Styles;
with GPS.Editors;                    use GPS.Editors;
with GPS.Editors.Line_Information;   use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;             use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;             use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;          use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Properties;          use GPS.Kernel.Properties;
with GPS.Intl;                       use GPS.Intl;
with GPS.Properties;                 use GPS.Properties;
with GVD_Module;                     use GVD_Module;
with GVD.Preferences;                use GVD.Preferences;
with GVD.Process;                    use GVD.Process;
with Xref;                           use Xref;
with JSON_Utils;

package body GVD.Breakpoints_List is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.BREAKPOINTS");

   type Breakpoints_Module is new Module_ID_Record with record
      Breakpoints : aliased Breakpoint_List;
      --  The list of persistent breakpoints for the current project. This
      --  list can be manipulated even when no debugger is running, and is
      --  loaded/saved to disk as needed
   end record;
   type Breakpoints_Module_Access is access all Breakpoints_Module'Class;

   Messages_Category_For_Breakpoints : constant String := "breakpoints";
   Breakpoints_Message_Flags         : constant Message_Flags :=
     (Editor_Side => False,
      Locations   => False,
      Editor_Line => True);

   Module : Breakpoints_Module_Access;

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project changes. This is a good time to load the
   --  persistent breakpoints

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called before the project is changed. This is a good time to save the
   --  persistent breakpoints

   type On_Before_Exit is new Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean;
   --  Called before GPS exist. This is a good time to save the persistent
   --  breakpoints.

   type On_Debugger_Terminated is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Terminated;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when one debugger terminates. This is a good time to save
   --  persistent breakpoints.

   type On_Debugger_Started is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Started;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when one debugger starts. The persistent breakpoints are applied.

   type On_Debugger_Location_Changed is
     new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Location_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when the current location has changed in the debugger.
   --  This is a good time to show, on the side of the editor, which lines
   --  have breakpoints.

   procedure Add_Information
     (Kernel  : not null access Kernel_Handle_Record'Class;
      B       : Breakpoint_Data);
   --  Create a new message to display information on the side of editors for
   --  that breakpoint.

   procedure Reindex_Breakpoints;
   --  Simply reindex the breakpoints by their position in the view.
   --  Should only be called outside a debugger session.

   function Is_Interactive
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Process : not null access Base_Visual_Debugger'Class)
      return Boolean;
   --  return True if debuger can process commands

   function To_String (Breakpoint : Breakpoint_Data) return String;
   --  Return a suitable string representation to display for the given
   --  breakpoint.

   --------------
   -- Commands --
   --------------

   type Breakpoint_Command_Mode is (Set, Unset);
   type Set_Breakpoint_Command_At_Line is new Root_Command with record
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode;
   end record;
   overriding function Execute
     (Self : access Set_Breakpoint_Command_At_Line) return Command_Return_Type;

   function Create_Set_Breakpoint_Command
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode) return Command_Access;
   --  Create a new instance of the command that sets or removes a breakpoint
   --  at a specific location.

   type Set_Breakpoint_Command_Context is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   overriding function Execute
     (Command : access Set_Breakpoint_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Set a breakpoint at the line given in the context

   type Remove_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove a breakpoint from the line given in the context

   type Toggle_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Toggle_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Toggle a breakpoint at the line given in the context

   -------------
   -- Filters --
   -------------

   type Find_Breakpoint_Filter is new Action_Filter_Record with record
      Found : Boolean := True;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Find_Breakpoint_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Return Found if breakpoint is set for current line

   type Is_Breakpoint_Active_Filter is
     new Action_Filter_Record with record
      Invert : Boolean := False;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Breakpoint_Active_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Return True if breakpoint is active for current line. Inverts result if
   --  Invert property is set.

   ----------------
   -- Properties --
   ----------------

   type Breakpoint_Property_Record is new Property_Record with record
      Kernel      : access Kernel_Handle_Record'Class;
      Breakpoints : Breakpoint_Vectors.Vector;
   end record;
   overriding procedure Save
     (Property : access Breakpoint_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Property : in out Breakpoint_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value);

   procedure Save_Persistent_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Save persistent breakpoints to properties.

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Breakpoint_Active_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      Kernel  : constant Kernel_Handle   := Get_Kernel (Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      Loc  : Location_Marker;
   begin
      if not Has_File_Information (Context)
        or else not Has_Line_Information (Context)
      then
         return False;
      end if;

      Loc := Kernel.Get_Buffer_Factory.Create_Marker
        (File   => File_Information (Context),
         Line   => Editable_Line_Type (Contexts.Line_Information (Context)),
         Column => 1);

      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         if Similar (B.Location, Loc) then
            if Filter.Invert then
               return not B.Enabled;
            else
               return B.Enabled;
            end if;
         end if;
      end loop;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Find_Breakpoint_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      Kernel  : constant Kernel_Handle   := Get_Kernel (Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      Loc  : Location_Marker;
   begin
      if not Has_File_Information (Context)
        or else not Has_Line_Information (Context)
      then
         return not Filter.Found;
      end if;

      Loc := Kernel.Get_Buffer_Factory.Create_Marker
        (File   => File_Information (Context),
         Line   => Editable_Line_Type (Contexts.Line_Information (Context)),
         Column => 1);

      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         if Similar (B.Location, Loc) then
            return Filter.Found;
         end if;
      end loop;

      return not Filter.Found;
   end Filter_Matches_Primitive;

   -----------------------------------
   -- Create_Set_Breakpoint_Command --
   -----------------------------------

   function Create_Set_Breakpoint_Command
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode) return Command_Access is
   begin
      return new Set_Breakpoint_Command_At_Line'
        (Root_Command with
         Kernel => Kernel, Mode => Mode);
   end Create_Set_Breakpoint_Command;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Editable_Line_Type;
      Temporary     : Boolean := False)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class);
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class)
      is
         Num : Breakpoint_Identifier with Unreferenced;
      begin
         if Is_Interactive (Kernel, Self) then
            Num := Visual_Debugger (Self).Debugger.Break_Source
              (File,
               Line,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end if;
      end On_Debugger;

   begin
      if Process = null then
         Module.Breakpoints.List.Append
           (Breakpoint_Data'
              (Location => Kernel.Get_Buffer_Factory.Create_Marker
                   (File   => File,
                    Line   => Line,
                    Column => 1),
               Num         =>
                 Breakpoint_Identifier (Module.Breakpoints.List.Length) + 1,
               Disposition => (if Temporary then Delete else Keep),
               others      => <>));
         Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);
      else
         For_Each_Debugger (Kernel, On_Debugger'Access);
      end if;
   end Break_Source;

   --------------------
   -- Unbreak_Source --
   --------------------

   procedure Unbreak_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Editable_Line_Type)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Num     : Breakpoint_Identifier := GVD.Types.No_Breakpoint;

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class);
      --  Remove a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class) is
      begin
         if Is_Interactive (Kernel, Self) then
            if Num = GVD.Types.No_Breakpoint then
               Visual_Debugger (Self).Debugger.Remove_Breakpoint_At
                 (File, Line, Mode => Visible);
            else
               declare
                  List : Breakpoint_Identifier_Lists.List;
               begin
                  List.Append (Num);

                  Visual_Debugger (Self).Debugger.Remove_Breakpoints
                    (List, Mode => Visible);
               end;
            end if;
         end if;
      end On_Debugger;

   begin
      if Process = null then
         declare
            To_Delete_List : Breakpoint_Identifier_Lists.List;
         begin

            --  Find the breakpoint to delete

            for Idx in Module.Breakpoints.List.First_Index
              .. Module.Breakpoints.List.Last_Index
            loop
               if Get_File (Module.Breakpoints.List (Idx).Location) = File
                 and then
                   Get_Line (Module.Breakpoints.List (Idx).Location) = Line
               then
                  To_Delete_List.Append (Module.Breakpoints.List (Idx).Num);
                  exit;
               end if;
            end loop;

            --  Delete it if it has been found

            if not To_Delete_List.Is_Empty then
               Delete_Multiple_Breakpoints (Kernel, To_Delete_List);
            end if;
         end;
      else
         for Idx in Process.Breakpoints.List.First_Index ..
           Process.Breakpoints.List.Last_Index
         loop
            if Get_File (Process.Breakpoints.List (Idx).Location) = File
              and then Get_Line
                (Process.Breakpoints.List (Idx).Location) = Line
            then
               Num := Process.Breakpoints.List (Idx).Num;
               exit;
            end if;
         end loop;

         For_Each_Debugger (Kernel, On_Debugger'Access);
      end if;
   end Unbreak_Source;

   ---------------------------------
   -- Delete_Multiple_Breakpoints --
   ---------------------------------

   procedure Delete_Multiple_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List)
   is
      Process : constant Visual_Debugger :=
                  Visual_Debugger (Get_Current_Debugger (Kernel));
      Deleted : Boolean := False;
   begin
      if not List.Is_Empty then

         if Process = null then
            for Num of List loop
               for Idx in Module.Breakpoints.List.First_Index
                 .. Module.Breakpoints.List.Last_Index
               loop
                  if Module.Breakpoints.List (Idx).Num = Num then
                     Module.Breakpoints.List.Delete (Idx);
                     Deleted := True;
                     exit;
                  end if;
               end loop;

            end loop;

            if Deleted then
               Reindex_Breakpoints;
               Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
               Show_Breakpoints_In_All_Editors (Kernel);
            end if;

         else
            --  Check the interactivity only once:
            --  the action "delete a breakpoint" doesn't run the debugger.
            if Is_Interactive (Kernel, Process) then
               Process.Debugger.Remove_Breakpoints
                 (List, Mode => GVD.Types.Visible);
            end if;
         end if;
      end if;
   end Delete_Multiple_Breakpoints;

   ---------------------------
   -- Clear_All_Breakpoints --
   ---------------------------

   procedure Clear_All_Breakpoints
     (Kernel        : not null access Kernel_Handle_Record'Class)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
   begin
      if Process = null then
         Module.Breakpoints.List.Clear;
         Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);

      elsif Is_Interactive (Kernel, Process) then
         Process.Debugger.Remove_Breakpoints
           (Breakpoint_Identifier_Lists.Empty_List,
            Mode => GVD.Types.Visible);
      end if;
   end Clear_All_Breakpoints;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Subprogram    : String;
      Temporary     : Boolean := False)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class);

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class)
      is
         Num      : Breakpoint_Identifier with Unreferenced;
      begin
         if Is_Interactive (Kernel, Self) then
            Num := Process.Debugger.Break_Subprogram
              (Subprogram, Temporary => Temporary, Mode => GVD.Types.Visible);
         end if;
      end On_Debugger;

   begin
      if Process = null then
         Module.Breakpoints.List.Append
           (Breakpoint_Data'
              (Subprogram => To_Unbounded_String (Subprogram),
               Num         =>
                 Breakpoint_Identifier (Module.Breakpoints.List.Length) + 1,
               Disposition => (if Temporary then Delete else Keep),
               others      => <>));
         Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);
      else
         For_Each_Debugger (Kernel, On_Debugger'Access);
      end if;
   end Break_Subprogram;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Set_Breakpoint_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use GPS.Kernel.Contexts;

      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Num      : Breakpoint_Identifier with Unreferenced;

   begin
      if Command.Continue_Till then
         --  Only works if there is a current debugger
         if Process /= null
           and then Is_Interactive (Kernel, Process)
         then
            Num := Process.Debugger.Break_Source
              (File_Information (Context.Context),
               Editable_Line_Type
                 ((if Has_File_Line_Information (Context.Context)
                  then File_Line_Information (Context.Context)
                  else Contexts.Line_Information (Context.Context))),
               Temporary => True,
               Mode      => GVD.Types.Visible);
            Process.Debugger.Continue (Mode => GVD.Types.Visible);
         end if;

      elsif Command.On_Line then
         Break_Source
           (Kernel,
            File  => File_Information (Context.Context),
            Line  => Editable_Line_Type
              ((if Has_File_Line_Information (Context.Context)
               then File_Line_Information (Context.Context)
               else Contexts.Line_Information (Context.Context))));
      else
         declare
            Entity : constant Root_Entity'Class :=
                       Get_Entity (Context.Context);
         begin
            if Is_Fuzzy (Entity) or else Is_Subprogram (Entity) then
               Break_Subprogram
                 (Kernel,
                  Subprogram => Entity_Name_Information (Context.Context));
            end if;
         end;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Set_Breakpoint_Command_At_Line) return Command_Return_Type
   is
      Context : constant Selection_Context := Self.Kernel.Get_Current_Context;

   begin
      case Self.Mode is
         when Set =>
            Break_Source
              (Self.Kernel,
               File => GPS.Kernel.Contexts.File_Information (Context),
               Line => Editable_Line_Type
                 (GPS.Kernel.Contexts.Line_Information (Context)));

         when Unset =>
            Unbreak_Source
              (Self.Kernel,
               File => GPS.Kernel.Contexts.File_Information (Context),
               Line => Editable_Line_Type
                 (GPS.Kernel.Contexts.Line_Information (Context)));
      end case;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      if not Has_File_Information (Context.Context)
        or else not Has_Line_Information (Context.Context)
      then
         return Commands.Failure;
      end if;

      Unbreak_Source
        (Get_Kernel (Context.Context),
         File  => File_Information (Context.Context),
         Line  => Editable_Line_Type
           (GPS.Kernel.Contexts.Line_Information (Context.Context)));

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Toggle_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Loc     : Location_Marker;
      List    : Breakpoint_Identifier_Lists.List;
   begin
      if not Has_File_Information (Context.Context)
        or else not Has_Line_Information (Context.Context)
      then
         return Commands.Failure;
      end if;

      Loc := Kernel.Get_Buffer_Factory.Create_Marker
        (File   => File_Information (Context.Context),
         Line   => Editable_Line_Type
           (Contexts.Line_Information (Context.Context)),
         Column => 1);

      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         if Similar (B.Location, Loc) then
            List.Append (B.Num);
            Set_Breakpoints_State
              (Kernel, List, not B.Enabled);
            return Commands.Success;
         end if;
      end loop;

      return Commands.Failure;
   end Execute;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Breakpoint_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;

   begin
      GNATCOLL.Traces.Trace (Me, "Saving breakpoints for future sessions");

      for B of Property.Breakpoints loop
         declare
            Value : constant JSON_Value := Create_Object;
         begin
            Value.Set_Field ("type", Breakpoint_Type'Image (B.The_Type));
            if B.The_Type = Other then
               Value.Set_Field ("type_name", To_String (B.The_Type_Name));
            end if;
            Value.Set_Field
              ("disposition",
               Breakpoint_Disposition'Image (B.Disposition));
            Value.Set_Field ("enabled", B.Enabled);
            Value.Set_Field ("expression", To_String (B.Expression));
            if B.Location.Is_Null then
               Value.Set_Field ("file", "");
               Value.Set_Field ("line", "");
            else
               Value.Set_Field
                 ("file", JSON_Utils.Save (Get_File (B.Location)));
               Value.Set_Field
                 ("line", Editable_Line_Type'Image (Get_Line (B.Location)));
            end if;
            Value.Set_Field ("exception", To_String (B.Except));
            Value.Set_Field ("subprogram", To_String (B.Subprogram));
            if B.Address /= Invalid_Address then
               Value.Set_Field ("address", Address_To_String (B.Address));
            else
               Value.Set_Field ("address", "");
            end if;
            Value.Set_Field ("ignore", B.Ignore);
            Value.Set_Field ("condition", To_String (B.Condition));
            Value.Set_Field ("command", To_String (B.Commands));
            Value.Set_Field ("scope", Scope_Type'Image (B.Scope));
            Value.Set_Field ("action", Action_Type'Image (B.Action));
            Append (Values, Value);
         end;
      end loop;
      Value.Set_Field ("breakpoints", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Breakpoint_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
      B      : Breakpoint_Data;

   begin
      GNATCOLL.Traces.Trace
        (Me, "Restoring breakpoints from previous session");

      Values := Value.Get ("breakpoints");

      for Index in 1 .. Length (Values) loop
         declare
            Item     : constant JSON_Value := Get (Values, Index);
            Loc      : Location_Marker     := No_Marker;
            The_Type : Breakpoint_Type;
         begin
            if String'(Item.Get ("line")) /= ""
              and then JSON_Value'(Item.Get ("file")) /= JSON_Null
            then
               Loc := Property.Kernel.Get_Buffer_Factory.Create_Marker
                 (File   => JSON_Utils.Load (Item.Get ("file")),
                  Line   => Editable_Line_Type'Value (Item.Get ("line")),
                  Column => 1);
            end if;

            The_Type := Breakpoint_Type'Value (Item.Get ("type"));
            B :=
              (Num           =>
                 Breakpoint_Identifier (Property.Breakpoints.Length) + 1,
               Trigger       => Write,
               The_Type      => The_Type,
               The_Type_Name => (if The_Type = Other
                                 then To_Unbounded_String
                                   (String'(Item.Get ("type_name")))
                                else Null_Unbounded_String),
               Disposition   => Breakpoint_Disposition'Value
                 (Item.Get ("disposition")),
               Enabled       => Item.Get ("enabled"),
               Expression    => Item.Get ("expression"),
               Except        => Item.Get ("exception"),
               Subprogram    => Item.Get ("subprogram"),
               Location      => Loc,
               Address       => String_To_Address (Item.Get ("address")),
               Ignore        => Item.Get ("ignore"),
               Condition     => Item.Get ("condition"),
               Commands      => Item.Get ("command"),
               Scope         => Scope_Type'Value (Item.Get ("scope")),
               Action        => Action_Type'Value (Item.Get ("action")));
            Property.Breakpoints.Append (B);
         end;
      end loop;
   end Load;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Prop  : Breakpoint_Property_Record;
      Found : Boolean;
   begin
      Module.Breakpoints.List.Clear;
      Get_Messages_Container (Kernel).Remove_Category
        (Messages_Category_For_Breakpoints,
         Breakpoints_Message_Flags);

      if not Preserve_State_On_Exit.Get_Pref then
         Trace (Me, "Not loading persistent breakpoints");
         return;
      end if;

      Trace (Me, "Loading persistent breakpoints");
      Prop.Kernel := Kernel;
      Get_Property
        (Prop, Get_Project (Kernel), Name => "breakpoints", Found => Found);
      if Found then
         Module.Breakpoints.List := Prop.Breakpoints;
         Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, File);
   begin
      Save_Persistent_Breakpoints (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Save_Persistent_Breakpoints (Kernel);
      return True;  --  allow exit
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Terminated;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      Process : constant Visual_Debugger := Visual_Debugger (Debugger);
      pragma Unreferenced (Self);
   begin
      --  We always save the debugger-specific breakpoints to the global list,
      --  so that later debuggers are started with the same list. If we don't
      --  do that, and the Preserve_State_On_Exit pref is disabled, we would
      --  end up with complex cases where breakpoints set before the debugger
      --  is started are set when the debugger starts, but not those set while
      --  the debugger is running

      if Visual_Debugger (Debugger).Debugger.Get_Executable = No_File then
         --  If there was no executable, we did not even try to set
         --  breakpoints, so don't save them either

         --  Remove breakpoint markers from sources
         Get_Messages_Container (Kernel).Remove_Category
           (Messages_Category_For_Breakpoints,
            Breakpoints_Message_Flags);

         return;
      end if;

      --  In case the user has set breakpoints manually via the console,
      --  synchronize the global list of breakpoints.
      Module.Breakpoints.List.Clear;

      if Break_On_Exception.Get_Pref then
         for B of Process.Breakpoints.List loop
            if B.Except = "" or else B.Except /= "all" then
               Module.Breakpoints.List.Append (B);
            end if;
         end loop;
      else
         Module.Breakpoints := Process.Breakpoints;
      end if;

      --  Put back the unrecognized breakpoints in the list
      if not Process.Imaginary_Breakpoints.Is_Empty then
         for B of Process.Imaginary_Breakpoints loop
            Module.Breakpoints.List.Append (B);
         end loop;

         Process.Imaginary_Breakpoints.Clear;
      end if;

      Reindex_Breakpoints;
      --  Save the breakpoints
      Save_Persistent_Breakpoints (Kernel);
   end Execute;

   ---------------------------------
   -- Save_Persistent_Breakpoints --
   ---------------------------------

   procedure Save_Persistent_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      if not Preserve_State_On_Exit.Get_Pref then
         Trace (Me, "Not saving persistent breakpoints");
         return;
      end if;

      if Module.Breakpoints.List.Is_Empty then
         Trace (Me, "No persistent breakpoint to save");
         Remove_Property (Kernel, Get_Project (Kernel), "breakpoints");
         return;
      end if;

      Trace (Me, "Saving persistent breakpoints");

      Set_Property
        (Kernel     => Kernel,
         Project    => Get_Project (Kernel),
         Name       => "breakpoints",
         Property   =>
            new Breakpoint_Property_Record'
           (Kernel      => Kernel,
            Breakpoints => Module.Breakpoints.List),
         --  Filter breakpoints that are created automatically by GPS as a
         --  result of preferences.
         Persistent => True);
   end Save_Persistent_Breakpoints;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Started;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Process          : constant Visual_Debugger :=
                            Visual_Debugger (Debugger);
      Id               : Breakpoint_Identifier;
      Warning_Displayed : Boolean := False;
   begin
      if Process.Debugger.Get_Executable = No_File then
         --  Do not try to restore breakpoints, since the debugger has no
         --  source anyway. We do not want to lose the list of persistent
         --  breakpoints.
         return;
      end if;

      Trace (Me, "Restore persistent breakpoints");
      for B of Module.Breakpoints.List loop
         if B.Except /= "" then
            Id := Process.Debugger.Break_Exception
              (To_String (B.Except),
               Temporary => B.Disposition /= Keep, Mode => Internal,
               Unhandled => False);
         elsif B.Location /= No_Marker then
            Id := Process.Debugger.Break_Source
              (Get_File (B.Location),
               Get_Line (B.Location),
               Temporary => B.Disposition /= Keep, Mode => Internal);
         elsif B.Subprogram /= "" then
            Id := Process.Debugger.Break_Subprogram
              (To_String (B.Subprogram),
               Temporary => B.Disposition /= Keep, Mode => Internal);
         elsif B.Address /= Invalid_Address then
            Id := Process.Debugger.Break_Address
              (B.Address,
               Temporary => B.Disposition /= Keep, Mode => Internal);
         else
            Id := GVD.Types.No_Breakpoint;
         end if;

         if Id /= GVD.Types.No_Breakpoint then
            if not B.Enabled then
               declare
                  List : Breakpoint_Identifier_Lists.List;
               begin
                  List.Append (Id);

                  Process.Debugger.Enable_Breakpoints
                    (List, B.Enabled, Internal);
               end;
            end if;

            if B.Condition /= "" then
               Process.Debugger.Set_Breakpoint_Condition
                 (Id, To_String (B.Condition), Internal);
            end if;

            if B.Commands /= "" then
               Process.Debugger.Set_Breakpoint_Command
                 (Id, To_String (B.Commands), Internal);
            end if;

            if B.Ignore /= 0 then
               Process.Debugger.Set_Breakpoint_Ignore_Count
                 (Id, B.Ignore, Internal);
            end if;

            if B.Scope /= No_Scope or else B.Action /= No_Action then
               Process.Debugger.Set_Scope_Action
                 (B.Scope, B.Action, Id, Internal);
            end if;
         else
            --  Display a warning message when a breakpoint that was set
            --  before starting the debugger is not recognized by it.
            --  This can mean that the executable has not been compiled with
            --  the debug flags for instance.

            if not Warning_Displayed then
               Process.Output_Text
                 (Str          => -"Some breakpoints set graphically are not "
                  & "recognized by the debugger and, thus, will be lost "
                  & "when running it. "
                  & ASCII.LF
                  & "This can happen when the executable "
                  & "being debugged has not been compiled with the debug "
                  & "flags or when the breakpoint's source file is not found "
                  & "in the symbols table. This also can happen for "
                  & "catchpoints."
                  & ASCII.LF
                  & "You should try to set them after a start command."
                  & ASCII.LF
                  & "Breakpoints and/or catchpoints that could not be set: "
                  & ASCII.LF
                  & ASCII.LF);

               Warning_Displayed := True;
            end if;

            Process.Output_Text (To_String (B) & ASCII.LF);
            Process.Imaginary_Breakpoints.Append (B);

         end if;
      end loop;

      --  If the breakpoints' warning message has been displayed, avoid the
      --  copy of the debugger's breakpoints list to the persistent's one.

      if Warning_Displayed then
         Process.Debugger.Display_Prompt;
      end if;

      --  Reparse the list to make sure of what the debugger is actually using
      Refresh_Breakpoints_List (Kernel, Process);
   end Execute;

   ----------------------------
   -- Set_Breakpoints_State --
   ----------------------------

   procedure Set_Breakpoints_State
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List;
      State  : Boolean)
   is
      Process         : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Debugger_Active : constant Boolean := Process /= null
        and then Is_Interactive (Kernel, Process);
   begin
      if List.Is_Empty then
         return;
      end if;

      --  If a debugger is active, enable/disable the breakpoints by
      --  sending the appropriate command.
      --  Otherwise, modify the state of the breakpoints stored in the
      --  persistant list.

      if Debugger_Active then
         Process.Debugger.Enable_Breakpoints
              (List, State, Mode => GVD.Types.Visible);
      else
         for Num of List loop
            for Breakpoint of Get_Stored_List_Of_Breakpoints.List loop
               if Breakpoint.Num = Num then
                  Breakpoint.Enabled := State;
                  exit;
               end if;
            end loop;
         end loop;

         Show_Breakpoints_In_All_Editors (Kernel);
      end if;
   end Set_Breakpoints_State;

   ----------------------------
   -- Get_Breakpoint_From_Id --
   ----------------------------

   function Get_Breakpoint_From_Id
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Id      : Breakpoint_Identifier)
      return Breakpoint_Data
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
   begin
      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         if B.Num = Id then
            return B;
         end if;
      end loop;
      return Null_Breakpoint;
   end Get_Breakpoint_From_Id;

   ------------------------------
   -- Refresh_Breakpoints_List --
   ------------------------------

   procedure Refresh_Breakpoints_List
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      Process  : constant Visual_Debugger := Visual_Debugger (Debugger);
   begin
      if Process /= null
        and then Process.Debugger /= null
        and then not Process.Command_In_Process
      then
         declare
            use Breakpoint_Vectors;
            Old : constant Breakpoint_Vectors.Vector :=
              Process.Breakpoints.List;
            Pos : Breakpoint_Vectors.Cursor := Old.First;
         begin
            Process.Debugger.List_Breakpoints
              (Kernel, Process.Breakpoints.List);
            Process.Breakpoints.Has_Temporary_Breakpoint := False;

            --  Check whether we have temporary breakpoints

            for B of Process.Breakpoints.List loop
               if B.Disposition /= Keep
                 and then B.Enabled
               then
                  Process.Breakpoints.Has_Temporary_Breakpoint := True;
               end if;

               if not Has_Element (Pos) then
                  Debugger_Breakpoint_Added_Hook.Run
                    (Kernel, Process, Integer (B.Num));
               else
                  while Has_Element (Pos)
                    and then Element (Pos).Num < B.Num
                  loop
                     Debugger_Breakpoint_Deleted_Hook.Run
                       (Kernel, Process, Integer (Element (Pos).Num));
                     Next (Pos);
                  end loop;

                  if Has_Element (Pos)
                    and then Element (Pos).Num = B.Num
                  then
                     if not Is_Equal (Element (Pos), B) then
                        Debugger_Breakpoint_Changed_Hook.Run
                          (Kernel, Process, Integer (B.Num));
                     end if;
                     Next (Pos);
                  else
                     Debugger_Breakpoint_Added_Hook.Run
                       (Kernel, Process, Integer (B.Num));
                  end if;
               end if;
            end loop;

            while Has_Element (Pos) loop
               Debugger_Breakpoint_Deleted_Hook.Run
                 (Kernel, Process, Integer (Element (Pos).Num));
               Next (Pos);
            end loop;
         end;
      end if;

      Show_Breakpoints_In_All_Editors
        (Kernel,
         Show_Debugger_Breakpoints => Process /= null);

      Debugger_Breakpoints_Changed_Hook.Run (Kernel, Process);
   end Refresh_Breakpoints_List;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Location_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Debugger);
   begin
      Show_Breakpoints_In_All_Editors (Kernel);
   end Execute;

   ---------------------
   -- Add_Information --
   ---------------------

   procedure Add_Information
     (Kernel  : not null access Kernel_Handle_Record'Class;
      B       : Breakpoint_Data)
   is
      Msg  : Simple_Message_Access;
      File : Virtual_File;
      Line : Editable_Line_Type;
   begin
      if B.Location = No_Marker then
         return;
      end if;

      File := Get_File (B.Location);
      Line := Get_Line (B.Location);

      Msg := Create_Simple_Message
        (Get_Messages_Container (Kernel),
         Category                 => Messages_Category_For_Breakpoints,
         File                     => File,
         Line                     => Natural (Line),
         Column                   => 0,
         Text                     =>
           (if not B.Enabled
            then "A disabled breakpoint has been set on this line"
            elsif B.Condition /= ""
            then "A conditional breakpoint has been set on this line"
            else "An active breakpoint has been set on this line"),
         Importance               => Unspecified,
         Flags                    => Breakpoints_Message_Flags,
         Allow_Auto_Jump_To_First => False);

      Msg.Set_Action
        (new Line_Information_Record'
           (Text               => Null_Unbounded_String,
            Tooltip_Text       => Msg.Get_Text,
            Image              => Null_Unbounded_String,
            Message            => Create (Message_Access (Msg)),
            Associated_Command => Create_Set_Breakpoint_Command
              (Kernel,
               Mode => Unset)));

      if not B.Enabled then
         Msg.Set_Highlighting
           (Debugger_Disabled_Breakpoint_Style, Length => 1);
      elsif B.Condition /= "" then
         Msg.Set_Highlighting
           (Debugger_Conditional_Breakpoint_Style, Length => 1);
      else
         Msg.Set_Highlighting (Debugger_Breakpoint_Style, Length => 1);
      end if;
   end Add_Information;

   -------------------------
   -- Reindex_Breakpoints --
   -------------------------

   procedure Reindex_Breakpoints is
   begin
      for Idx in Module.Breakpoints.List.First_Index
        .. Module.Breakpoints.List.Last_Index
      loop
         Module.Breakpoints.List (Idx).Num := Breakpoint_Identifier (Idx);
      end loop;
   end Reindex_Breakpoints;

   -------------------------------------
   -- Show_Breakpoints_In_All_Editors --
   -------------------------------------

   procedure Show_Breakpoints_In_All_Editors
     (Kernel                    : not null access Kernel_Handle_Record'Class;
      Show_Debugger_Breakpoints : Boolean := True)
   is
      Process : constant Visual_Debugger :=
                  (if Show_Debugger_Breakpoints then
                      Visual_Debugger
                         (Get_Current_Debugger (Kernel))
                   else
                      null);
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (Messages_Category_For_Breakpoints,
         Breakpoints_Message_Flags);

      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         Add_Information (Kernel, B);
      end loop;
   end Show_Breakpoints_In_All_Editors;

   ------------------------------------
   -- Get_Stored_List_Of_Breakpoints --
   ------------------------------------

   function Get_Stored_List_Of_Breakpoints
     (Debugger : access Base_Visual_Debugger'Class := null)
      return access Breakpoint_List is
   begin
      if Debugger = null
        or else Visual_Debugger (Debugger).Debugger = null
      then
         return Module.Breakpoints'Access;
      else
         return Visual_Debugger (Debugger).Breakpoints'Access;
      end if;
   end Get_Stored_List_Of_Breakpoints;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (B1, B2 : Breakpoint_Data) return Boolean is
   begin
      return B1.Num = B2.Num
        and then B1.The_Type    = B2.The_Type
        and then B1.Disposition = B2.Disposition
        and then B1.Enabled     = B2.Enabled
        and then B1.Address     = B2.Address
        and then B1.Trigger     = B2.Trigger
        and then B1.Expression  = B2.Expression
        and then B1.Except      = B2.Except
        and then B1.Subprogram  = B2.Subprogram
        and then Similar (B1.Location, B2.Location)
        and then B1.Condition   = B2.Condition
        and then B1.Ignore      = B2.Ignore
        and then B1.Commands    = B2.Commands
        and then B1.Scope       = B2.Scope
        and then B1.Action      = B2.Action;
   end Is_Equal;

   --------------------
   -- Is_Interactive --
   --------------------

   function Is_Interactive
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Process : not null access Base_Visual_Debugger'Class)
      return Boolean is
   begin
      if Process.Command_In_Process then
         Insert
           (Kernel,
            -("The debugger"
              & Integer'Image (Integer (Process.Get_Num))
              & " is busy processing a command"),
            Mode => Info);
         return False;

      else
         return True;
      end if;
   end Is_Interactive;

   ---------------
   -- To_String --
   ---------------

   function To_String (Breakpoint : Breakpoint_Data) return String is
   begin
      if Breakpoint.Except /= "" then
         return "exception " & To_String (Breakpoint.Except);
      elsif Breakpoint.Location /= No_Marker then
         return Get_File (Breakpoint.Location).Display_Base_Name
           & ":"
           & GNATCOLL.Utils.Image
           (Integer (Get_Line (Breakpoint.Location)),
            Min_Width => 0);
      elsif Breakpoint.Subprogram /= "" then
         return To_String (Breakpoint.Subprogram);
      elsif Breakpoint.Address /= Invalid_Address then
         return Address_To_String (Breakpoint.Address);
      else
         return "";
      end if;
   end To_String;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      No_Debugger_Or_Stopped : Action_Filter;
      Breakable_Source         : Action_Filter;
   begin
      Module := new Breakpoints_Module;
      Register_Module (Module, Kernel, "Persistent_Breakpoints");

      Project_Changed_Hook.Add (new On_Project_Changed);
      Project_Changing_Hook.Add (new On_Project_Changing);
      Before_Exit_Action_Hook.Add (new On_Before_Exit);
      Debugger_Terminated_Hook.Add (new On_Debugger_Terminated);
      Debugger_Started_Hook.Add (new On_Debugger_Started);
      Debugger_Location_Changed_Hook.Add (new On_Debugger_Location_Changed);

      No_Debugger_Or_Stopped := Kernel.Lookup_Filter
        ("Debugger inactive or stopped");

      Register_Action
        (Kernel, "debug set subprogram breakpoint",
         Command     => new Set_Breakpoint_Command_Context,
         Description => "Set a breakpoint on subprogram",
         Filter      => No_Debugger_Or_Stopped and
             Kernel.Lookup_Filter ("Debugger entity name"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Set breakpoint on %e",
         Action => "debug set subprogram breakpoint");

      Breakable_Source := Kernel.Lookup_Filter ("Debugger breakable source");

      Register_Action
        (Kernel, "debug set line breakpoint",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => True, Continue_Till => False),
         Description => "Set a breakpoint on line",
         Filter      => No_Debugger_Or_Stopped and
           Kernel.Lookup_Filter ("Source editor") and
             Kernel.Lookup_Filter ("Debugger breakable source") and
             Breakable_Source,
         Category    => -"Debug");

      Kernel.Set_Default_Line_Number_Click ("debug set line breakpoint");

      Register_Action
        (Kernel, "continue till line",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => True, Continue_Till => True),
         Description => "Continue executing until the given line",
         Filter      => Kernel.Lookup_Filter ("Debugger stopped") and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Continue until line %l",
         Action => "continue till line");

      Register_Action
        (Kernel, "debug remove breakpoint",
         Command     => new Remove_Breakpoint_Command,
         Description => "Remove breakpoint",
         Filter      => No_Debugger_Or_Stopped and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Remove breakpoint",
         Action => "debug remove breakpoint",
         Filter => new Find_Breakpoint_Filter'
           (Action_Filter_Record with Found => True));

      Register_Action
        (Kernel, "debug disable breakpoint",
         Command     => new Toggle_Breakpoint_Command,
         Description => "Disable breakpoint",
         Filter      => No_Debugger_Or_Stopped and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Disable breakpoint",
         Action => "debug disable breakpoint",
         Filter => new Is_Breakpoint_Active_Filter'
           (Action_Filter_Record with Invert => False));

      Register_Action
        (Kernel, "debug enable breakpoint",
         Command     => new Toggle_Breakpoint_Command,
         Description => "Enable breakpoint",
         Filter      => No_Debugger_Or_Stopped and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Enable breakpoint",
         Action => "debug enable breakpoint",
         Filter => new Is_Breakpoint_Active_Filter'
           (Action_Filter_Record with Invert => True));
   end Register_Module;

end GVD.Breakpoints_List;
