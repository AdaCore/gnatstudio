------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with GNATCOLL.JSON;                  use GNATCOLL.JSON;
with GNATCOLL.Traces;                use GNATCOLL.Traces;

with VSS.Strings.Conversions;

with Commands;                       use Commands;
with Commands.Interactive;           use Commands.Interactive;

with GPS.Default_Styles;
with GPS.Debuggers;                  use GPS.Debuggers;
with GPS.Editors;                    use GPS.Editors;
with GPS.Editors.Line_Information;
with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages.References;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;
with GPS.Kernel.Properties;
with GPS.Markers;                    use GPS.Markers;
with GPS.Properties;                 use GPS.Properties;

with DAP.Clients;                    use DAP.Clients;
with DAP.Clients.Breakpoint_Managers;
with DAP.Modules.Preferences;        use DAP.Modules.Preferences;
with DAP.Views.Breakpoints;
with DAP.Utils;                      use DAP.Utils;

with Xref;                           use Xref;
with JSON_Utils;

package body DAP.Module.Breakpoints is

   Me : constant Trace_Handle := Create
     ("GPS.DAP.MODULES_PERSISTENT_BREAKPOINTS");

   Persistent_Category : constant String := "dap_breakpoints";
   Persistent_Field    : constant String := "breakpoints";

   Messages_Category_For_Breakpoints : constant String := "breakpoints";
   Breakpoints_Message_Flags         : constant Message_Flags :=
     (Editor_Side => False,
      Locations   => False,
      Editor_Line => True);

   type Breakpoint_Command_Mode is (Set, Unset);

   --  Methods --

   function Create_Set_Breakpoint_Command
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode) return Command_Access;
   --  Create a new instance of the command that sets or removes a breakpoint
   --  at a specific location.

   -- Commands --

   type Set_Breakpoint_Command_Context is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   overriding function Execute
     (Command : access Set_Breakpoint_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Set a breakpoint at the line given in the context

   type Set_Breakpoint_Command_At_Line is new Root_Command with record
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode;
   end record;
   overriding function Execute
     (Self : access Set_Breakpoint_Command_At_Line) return Command_Return_Type;

   type Remove_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove a breakpoint from the line given in the context

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

   type On_Breakpoints_Changed is new Debugger_Hooks_Function
   with null record;
   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class);
   --  Called when the breakpoints' have changed. Used to show the breakpoints
   --  in the editors.

   type On_Breakpoint_Event (Event : Breakpoint_Event)
   is new Debugger_Breakpoint_Hook_Function with null record;
   overriding procedure Execute
      (Self     : On_Breakpoint_Event;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class;
       Id       : Integer);
   --  Called when a specific breakpoint has been added/deleted/changed.
   --  Used to update the breakpoints shown in the editors.

   --  Filters --

   type Find_Breakpoint_Filter is new Action_Filter_Record with record
      Found : Boolean := True;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Find_Breakpoint_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Return Found if breakpoint is set for current line

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

   function "<" (L, R : Location_Marker) return Boolean;

   package Locations_Sets is new Ada.Containers.Ordered_Sets
     (Location_Marker, "=" => GPS.Markers."=");

   ------------------------
   -- Editor Breakpoints --
   ------------------------

   procedure Show_Breakpoint
     (Kernel  : not null access Kernel_Handle_Record'Class;
      B       : Breakpoint_Data);
   --  Show the given breakpoint in the editor, using a different style
   --  according to the breakpoint's state.

   procedure Show_Breakpoints_In_All_Editors
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Breakpoints : Breakpoint_Vectors.Vector);
   --  Update the side column for all editors, showing the given breakpoints.

   procedure Hide_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Hide all the breakpoints in all editors.

   -- Vars --

   Persistent_Breakpoints : Breakpoint_Holder;
   --  The holder for the persistent breakpoints (i.e: breakpoints that
   --  exist outside of a debugging session and need to be saved).

   Shown_Breakpoint_Locations : Locations_Sets.Set;
   --  The list of locations where a breakpoint is shown in the editors' side
   --  columns.

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Location_Marker) return Boolean is
      F1 : constant Virtual_File := GPS.Editors.Get_File (L);
      F2 : constant Virtual_File := GPS.Editors.Get_File (L);
   begin
      if F1 = F2 then
         return GPS.Editors.Get_Line (L) < GPS.Editors.Get_Line (R);
      else
         return F1 < F2;
      end if;
   end "<";

   ---------------------------
   -- Clear_All_Breakpoints --
   ---------------------------

   procedure Clear_All_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access);
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         Debugger.Get_Breakpoints_Manager.Remove_All_Breakpoints;
      end On_Debugger;

   begin
      if DAP.Module.Get_Current_Debugger = null then
         Persistent_Breakpoints.Clear;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Hide_Breakpoints (Kernel);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Get_Breakpoints_Manager.
              Remove_All_Breakpoints;
         end if;
      end if;
   end Clear_All_Breakpoints;

   ----------------------------
   -- Get_Breakpoint_From_Id --
   ----------------------------

   function Get_Breakpoint_From_Id
     (Id : Breakpoint_Identifier) return Breakpoint_Data
   is
      Debugger : constant DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
   begin
      if Debugger = null then
         return Persistent_Breakpoints.Get_Breakpoint_From_Id (Id);
      else
         return Debugger.Get_Breakpoints_Manager.Get_Breakpoint_From_Id (Id);
      end if;
   end Get_Breakpoint_From_Id;

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

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Find_Breakpoint_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      use GPS.Kernel.Contexts;

      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Loc    : Location_Marker;
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

      if DAP.Module.Get_Current_Debugger = null then
         if Persistent_Breakpoints.Contains (Loc) then
            return Filter.Found;
         else
            return not Filter.Found;
         end if;

      else
         return Filter.Found =
           DAP.Module.Get_Current_Debugger.Get_Breakpoints_Manager.
             Has_Breakpoint (Loc);
      end if;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Set_Breakpoint_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use GPS.Kernel.Contexts;

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);

   begin
      if Command.Continue_Till then
         if DAP.Module.Get_Current_Debugger /= null
           and then DAP.Module.Get_Current_Debugger.Is_Stopped
         then
            declare
               Location : constant Breakpoint_Location_Type :=
                 (Marker =>
                    Kernel.Get_Buffer_Factory.Create_Marker
                      (File   => File_Information (Context.Context),
                       Line   => Editable_Line_Type
                         ((if Has_File_Line_Information (Context.Context)
                          then File_Line_Information (Context.Context)
                          else Contexts.Line_Information
                            (Context.Context))),
                       Column => 1),
                  Address => Invalid_Address);
            begin
               DAP.Module.Get_Current_Debugger.
                 Get_Breakpoints_Manager.Continue_Until_Location (Location);
            end;
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
      use GPS.Kernel.Contexts;
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

   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class)
   is
      Client : constant DAP_Client_Access :=
        (if Debugger = null then null
         else DAP_Visual_Debugger_Access (Debugger).Client);
   begin
      --  If there is a running debugger, show its breakpoints. Otherwise
      --  show the persistent ones.
      Show_Breakpoints_In_All_Editors
        (Kernel      => Kernel,
         Breakpoints =>
           (if Client /= null then
               Client.Get_Breakpoints_Manager.Get_Breakpoints
            else
               Persistent_Breakpoints.Get_Breakpoints));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self     : On_Breakpoint_Event;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class;
       Id       : Integer)
   is
      Client     : constant DAP_Client_Access :=
        DAP_Visual_Debugger_Access (Debugger).Client;
   begin
      Show_Breakpoints_In_All_Editors
        (Kernel      => Kernel,
         Breakpoints =>
           (if Client /= null then
                 Client.Get_Breakpoints_Manager.Get_Breakpoints
            else
               Persistent_Breakpoints.Get_Breakpoints));
   end Execute;

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
      Persistent_Breakpoints.Clear;
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
        (Property => Prop,
         Project  => GPS.Kernel.Project.Get_Project (Kernel),
         Name     => Persistent_Category,
         Found    => Found);

      if Found then
         Persistent_Breakpoints.Initialize (Prop.Breakpoints);
         Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
      end if;
   end Execute;

   --------------------------------
   -- Get_Persistent_Breakpoints --
   --------------------------------

   function Get_Persistent_Breakpoints return Breakpoint_Vectors.Vector is
   begin
      return Persistent_Breakpoints.Get_Breakpoints;
   end Get_Persistent_Breakpoints;

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

      if Persistent_Breakpoints.Get_Breakpoints.Is_Empty then
         Trace (Me, "No persistent breakpoint to save");
         GPS.Kernel.Properties.Remove_Property
           (Kernel, GPS.Kernel.Project.Get_Project (Kernel),
            Persistent_Category);
         return;
      end if;

      Trace (Me, "Saving persistent breakpoints");

      GPS.Kernel.Properties.Set_Property
        (Kernel     => Kernel,
         Project    => GPS.Kernel.Project.Get_Project (Kernel),
         Name       => Persistent_Category,
         Property   =>
            new Breakpoint_Property_Record'
           (Kernel      => Kernel,
            Breakpoints => Persistent_Breakpoints.Get_Breakpoints),
         --  Filter breakpoints that are created automatically by GNAT Studio
         --  as a result of preferences.
         Persistent => True);

   exception
      when E : others =>
         Trace (Me, E);
   end Save_Persistent_Breakpoints;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Breakpoint_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;

      ----------
      -- Save --
      ----------

      procedure Save (B : Breakpoint_Data);
      procedure Save (B : Breakpoint_Data)
      is
         Value : constant JSON_Value := Create_Object;
      begin
         Value.Set_Field ("kind", Breakpoint_Kind'Image (B.Kind));
         Value.Set_Field ("ignore", B.Ignore);

         if not B.Condition.Is_Empty then
            Value.Set_Field ("condition", UTF8 (B.Condition));
         end if;

         if not B.Commands.Is_Empty then
            Value.Set_Field ("command", UTF8 (B.Commands));
         end if;

         Value.Set_Field
           ("disposition",
            Breakpoint_Disposition'Image (B.Disposition));

         case B.Kind is
            when On_Line =>
               if B.Location.Marker /= No_Marker then
                  Value.Set_Field
                    ("file", JSON_Utils.Save (Get_File (Get_Location (B))));
                  Value.Set_Field
                    ("line", Editable_Line_Type'Image
                       (Get_Line (Get_Location (B))));
               end if;

            when On_Subprogram =>
               Value.Set_Field ("subprogram", To_String (B.Subprogram));

            when On_Instruction =>
               --  we do not store breakpoints for addresses
               null;

            when On_Exception =>
               Value.Set_Field ("exception", To_String (B.Exception_Name));
               Value.Set_Field ("unhandled", B.Unhandled);
         end case;

         Append (Values, Value);
      end Save;

   begin
      GNATCOLL.Traces.Trace (Me, "Saving breakpoints for future sessions");

      for Data of Property.Breakpoints loop
         if Data.Kind /= On_Instruction then
            Save (Data);
         end if;
      end loop;

      Value.Set_Field (Persistent_Field, Values);
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
   begin
      GNATCOLL.Traces.Trace
        (Me, "Restoring breakpoints from previous session");

      if not Value.Has_Field (Persistent_Field) then
         GNATCOLL.Traces.Trace (Me, "No breakpoints");
         return;
      end if;

      Values := Value.Get (Persistent_Field);

      for Index in 1 .. Length (Values) loop
         declare
            Id   : constant Breakpoint_Identifier :=
              Breakpoint_Identifier (Index);
            Item : constant JSON_Value := Get (Values, Index);

            Kind : constant Breakpoint_Kind :=
              Breakpoint_Kind'Value (Item.Get ("kind"));
            Loc  : Location_Marker     := No_Marker;

            Condition : constant VSS.Strings.Virtual_String :=
              (if Item.Has_Field ("condition")
               then VSS.Strings.Conversions.To_Virtual_String
                 (String'(Item.Get ("condition")))
               else VSS.Strings.Empty_Virtual_String);

            Commands : constant VSS.Strings.Virtual_String :=
              (if Item.Has_Field ("command")
               then VSS.Strings.Conversions.To_Virtual_String
                 (String'(Item.Get ("command")))
               else VSS.Strings.Empty_Virtual_String);
            B    : Breakpoint_Data (Kind);
         begin
            case Kind is
               when On_Line =>
                  if Item.Has_Field ("line")
                    and then Item.Has_Field ("file")
                  then
                     Loc := Property.Kernel.Get_Buffer_Factory.Create_Marker
                       (File   => JSON_Utils.Load (Item.Get ("file")),
                        Line   => Editable_Line_Type'Value (Item.Get ("line")),
                        Column => 1);

                     B :=
                       (Kind           => On_Line,
                        Num            => Id,
                        Disposition    => Breakpoint_Disposition'Value
                          (Item.Get ("disposition")),
                        Enabled        => True,
                        Location       => (Loc, Invalid_Address),
                        Ignore         => Item.Get ("ignore"),
                        Condition      => Condition,
                        Commands       => Commands,
                        Continue_Until => False,
                        Verified       => True);
                  end if;

               when On_Subprogram =>
                  B :=
                    (Kind          => On_Subprogram,
                     Num           => Id,
                     Disposition   => Breakpoint_Disposition'Value
                       (Item.Get ("disposition")),
                     Enabled       => True,
                     Subprogram    => Item.Get ("subprogram"),
                     Ignore        => Item.Get ("ignore"),
                     Condition     => Condition,
                     Commands      => Commands,
                     others        => <>);

               when On_Instruction =>
                  --  we do not store breakpoints for addresses
                  null;

               when On_Exception =>
                  B :=
                    (Kind            => On_Exception,
                     Num             => Id,
                     Disposition     => Breakpoint_Disposition'Value
                       (Item.Get ("disposition")),
                     Enabled         => True,
                     Exception_Name  => Item.Get ("exception"),
                     Unhandled       => Item.Get ("unhandled"),
                     Ignore          => Item.Get ("ignore"),
                     Condition       => Condition,
                     Commands        => Commands,
                     others          => <>);
            end case;

            Property.Breakpoints.Append (B);
         end;
      end loop;
   end Load;

   ----------------------
   -- Hide_Breakpoints --
   ----------------------

   procedure Hide_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (Messages_Category_For_Breakpoints,
         Breakpoints_Message_Flags);
      Shown_Breakpoint_Locations.Clear;
   end Hide_Breakpoints;

   -------------------------------------
   -- Show_Breakpoints_In_All_Editors --
   -------------------------------------

   procedure Show_Breakpoints_In_All_Editors
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Breakpoints : Breakpoint_Vectors.Vector) is
   begin
      Hide_Breakpoints (Kernel);

      for Data of Breakpoints loop
         Show_Breakpoint (Kernel, Data);
      end loop;
   end Show_Breakpoints_In_All_Editors;

   ---------------------
   -- Show_Breakpoint --
   ---------------------

   procedure Show_Breakpoint
     (Kernel  : not null access Kernel_Handle_Record'Class;
      B       : Breakpoint_Data)
   is
      Msg    : Simple_Message_Access;
      File   : Virtual_File;
      Line   : Editable_Line_Type;
      Action : GPS.Editors.Line_Information.Line_Information_Access;
   begin
      --  We should only show breakpoints that have a unique location, so
      --  source breakpoints.
      if B.Kind /= On_Line then
         return;
      end if;

      if not Shown_Breakpoint_Locations.Contains (B.Location.Marker) then
         Shown_Breakpoint_Locations.Insert (B.Location.Marker);

         File := Get_File (B.Location.Marker);
         Line := Get_Line (B.Location.Marker);

         Msg := Create_Simple_Message
           (Get_Messages_Container (Kernel),
            Category                 => Messages_Category_For_Breakpoints,
            File                     => File,
            Line                     => Natural (Line),
            Column                   => 0,
            Text                     =>
              (if B.Condition.Is_Empty
               then "An active breakpoint has been set on this line"
               else "A conditional breakpoint has been set on this line"),
            Importance               => Unspecified,
            Flags                    => Breakpoints_Message_Flags,
            Allow_Auto_Jump_To_First => False);

         Action := new GPS.Editors.Line_Information.Line_Information_Record'
           (Text                     => Null_Unbounded_String,
            Tooltip_Text             => Msg.Get_Text,
            Image                    => Null_Unbounded_String,
            Message                  =>
              GPS.Kernel.Messages.References.Create
                (Message_Access (Msg)),
            Category                 => <>,
            Display_Popup_When_Alone => False,
            Associated_Command       => Create_Set_Breakpoint_Command
              (Kernel,
               Mode => Unset));
         Msg.Set_Action (Action);

         if not B.Enabled then
            Msg.Set_Highlighting
              (GPS.Default_Styles.Debugger_Disabled_Breakpoint_Style,
               Length => 1);

         elsif not B.Condition.Is_Empty then
            Msg.Set_Highlighting
              (GPS.Default_Styles.Debugger_Conditional_Breakpoint_Style,
               Length => 1);
         else
            Msg.Set_Highlighting
              (GPS.Default_Styles.Debugger_Breakpoint_Style, Length => 1);
         end if;
      end if;
   end Show_Breakpoint;

   -----------
   -- Break --
   -----------

   procedure Break
     (Kernel : not null access Kernel_Handle_Record'Class;
      Data   : in out Breakpoint_Data)
   is

      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access);
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         Debugger.Get_Breakpoints_Manager.Break (Data);
      end On_Debugger;

   begin
      if DAP.Module.Get_Current_Debugger = null then
         Persistent_Breakpoints.Append (Data);
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoint (Kernel, Data);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Get_Breakpoints_Manager.Break
              (Data);
         end if;
      end if;
   end Break;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Line,
         Num         => No_Breakpoint,
         Location    => DAP.Types.Breakpoints.Breakpoint_Location_Type'
           (Marker  => Kernel.Get_Buffer_Factory.Create_Marker
              (File   => File,
               Line   => Line,
               Column => 1),
            Address => Invalid_Address),
         Disposition => (if Temporary then Delete else Keep),
         Condition   => Condition,
         Ignore      => Ignore,
         Commands    => Commands,
         others      => <>);

   begin
      Break (Kernel, B);
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Exception,
         Num         => No_Breakpoint,
         Exception_Name      => To_Unbounded_String (Name),
         Unhandled   => Unhandled,
         Disposition => (if Temporary then Delete else Keep),
         others      => <>);
   begin
      Break (Kernel, B);
   end Break_Exception;

   -----------------------------
   -- Break_On_All_Exceptions --
   -----------------------------

   procedure Break_On_All_Exceptions
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Unhandled : Boolean := False)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Exception,
         Num         => No_Breakpoint,
         Exception_Name      => To_Unbounded_String (All_Exceptions_Filter),
         Unhandled   => Unhandled,
         Disposition => Keep,
         others      => <>);
   begin
      Break (Kernel, B);
   end Break_On_All_Exceptions;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Subprogram    : String;
      Temporary     : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Subprogram,
         Num         => No_Breakpoint,
         Subprogram  => To_Unbounded_String (Subprogram),
         Disposition => (if Temporary then Delete else Keep),
         Condition   => Condition,
         Ignore      => Ignore,
         Commands    => Commands,
         others      => <>);

   begin
      Break (Kernel, B);
   end Break_Subprogram;

   -------------------
   -- Break_Address --
   -------------------

   procedure Break_Address
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Instruction,
         Num         => No_Breakpoint,
         Location    => Breakpoint_Location_Type'
           (Address => Address, others => <>),
         Disposition => (if Temporary then Delete else Keep),
         Condition   => Condition,
         Ignore      => Ignore,
         Commands    => Commands,
         others      => <>);

   begin
      Break (Kernel, B);
   end Break_Address;

   --------------------
   -- Unbreak_Source --
   --------------------

   procedure Unbreak_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Editable_Line_Type)
   is
      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access);
      --  Remove a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         Debugger.Get_Breakpoints_Manager.Remove_Breakpoint_At (File, Line);
      end On_Debugger;

   begin
      if DAP.Module.Get_Current_Debugger = null then
         Persistent_Breakpoints.Delete (File, Line);
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Get_Breakpoints_Manager.
              Remove_Breakpoint_At (File, Line);
         end if;
      end if;
   end Unbreak_Source;

   ---------------------------------
   -- Delete_Multiple_Breakpoints --
   ---------------------------------

   procedure Delete_Multiple_Breakpoints
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Indexes : Breakpoint_Index_Lists.List)
   is
      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access);
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         Debugger.Get_Breakpoints_Manager.Remove_Breakpoints (Indexes);
      end On_Debugger;

   begin
      if Indexes.Is_Empty then
         return;
      end if;

      if DAP.Module.Get_Current_Debugger = null then
         Persistent_Breakpoints.Delete (Indexes);
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Kernel, null);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Get_Breakpoints_Manager.
              Remove_Breakpoints (Indexes);
         end if;
      end if;
   end Delete_Multiple_Breakpoints;

   ----------------------------
   -- Set_Breakpoints_State --
   ----------------------------

   procedure Set_Breakpoints_State
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Indexes : Breakpoint_Index_Lists.List;
      State   : Boolean) is
   begin
      if Indexes.Is_Empty then
         return;
      end if;

      --  If a debugger is active, enable/disable the breakpoints by
      --  sending the appropriate command.
      --  Otherwise, modify the state of the breakpoints stored in the
      --  persistant list.

      if DAP.Module.Get_Current_Debugger = null then
         Persistent_Breakpoints.Set_Breakpoints_State (Indexes, State);
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Kernel, null);
      else
         DAP.Module.Get_Current_Debugger.Get_Breakpoints_Manager.
           Set_Breakpoints_State (Indexes, State);
      end if;
   end Set_Breakpoints_State;

   -------------------------
   -- Store_As_Persistent --
   -------------------------

   procedure Store_As_Persistent
     (Executable  : Virtual_File;
      Breakpoints : Breakpoint_Vectors.Vector)
   is
      Count : Natural := 0;

      procedure Count_Running_Debuggers
        (Debugger : DAP.Clients.DAP_Client_Access);

      -----------------------------
      -- Count_Running_Debuggers --
      -----------------------------

      procedure Count_Running_Debuggers
        (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         if Debugger.Get_Executable = Executable then
            Count := Count + 1;
         end if;
      end Count_Running_Debuggers;

   begin
      if DAP.Module.Get_Started_Per_Session_Debuggers < 2 then
         --  We had only one debugger, copy the debugger's breakpoints,
         --  reseting their IDs since there is no running DAP server anymore.
         Persistent_Breakpoints.Initialize (Breakpoints, Full_Copy => False);
      else

         DAP.Module.For_Each_Debugger (Count_Running_Debuggers'Access);

         if Count > 1 then
            --  Store breakpoints only from the last closed session
            --  for the executable
            return;
         end if;

         Persistent_Breakpoints.Replace
           (Breakpoints => Breakpoints,
            Full_Copy   => False);
      end if;
   end Store_As_Persistent;

   -----------------------------
   -- On_Debugging_Terminated --
   -----------------------------

   procedure On_Debugging_Terminated
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Save_Persistent_Breakpoints (Kernel);
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
   end On_Debugging_Terminated;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy is
   begin
      Persistent_Breakpoints.Clear;
   end On_Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      No_Debugger_Or_Ready : Action_Filter;
   begin
      --  Connect to project' hooks
      Project_Changing_Hook.Add (new On_Project_Changing);
      Project_Changed_Hook.Add (new On_Project_Changed);

      --  Connect to breakpoints' hooks
      Debugger_Breakpoints_Changed_Hook.Add
        (new On_Breakpoints_Changed);
      Debugger_Breakpoint_Added_Hook.Add
        (new On_Breakpoint_Event (Added));
      Debugger_Breakpoint_Changed_Hook.Add
        (new On_Breakpoint_Event (Changed));
      Debugger_Breakpoint_Deleted_Hook.Add
        (new On_Breakpoint_Event (Deleted));
      No_Debugger_Or_Ready := Kernel.Lookup_Filter ("No debugger or ready");

      --  Register all the breakpoint-related actions
      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug set line breakpoint",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => True, Continue_Till => False),
         Description => "Set a breakpoint on line",
         Filter      => Kernel.Lookup_Filter ("Source editor") and
             No_Debugger_Or_Ready and
             Kernel.Lookup_Filter ("Debugger breakable source"),
         Category    => "Debug");
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => "Debug/Set breakpoint",
         Action => "debug set line breakpoint",
         Filter => new Find_Breakpoint_Filter'
           (Action_Filter_Record with Found => False));

      Kernel.Set_Default_Line_Number_Click
        (Action     => "debug set line breakpoint",
         Click_Type => GPS.Kernel.Normal_Click);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug set subprogram breakpoint",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => False, Continue_Till => False),
         Description => "Set a breakpoint on subprogram",
         Filter      => No_Debugger_Or_Ready and
             Kernel.Lookup_Filter ("Debugger entity name"),
         Category    => "Debug");
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => "Debug/Set breakpoint on %e",
         Action => "debug set subprogram breakpoint");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "debug remove breakpoint",
         Command     => new Remove_Breakpoint_Command,
         Description => "Remove breakpoint",
         Filter      => Kernel.Lookup_Filter ("Source editor"),
         Category    => "Debug");
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => "Debug/Remove breakpoint",
         Action => "debug remove breakpoint",
         Filter => new Find_Breakpoint_Filter'
           (Action_Filter_Record with Found => True));

      GPS.Kernel.Actions.Register_Action
        (Kernel, "continue till line",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => True, Continue_Till => True),
         Description => "Continue executing until the given line",
         Filter      => Kernel.Lookup_Filter ("Debugger stopped") and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => "Debug");
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => "Debug/Continue until line %l",
         Action => "continue till line");

      DAP.Views.Breakpoints.Register_Module (Kernel);
   end Register_Module;

end DAP.Module.Breakpoints;
