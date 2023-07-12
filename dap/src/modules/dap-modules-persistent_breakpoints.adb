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
with GNATCOLL.Tribooleans;           use GNATCOLL.Tribooleans;

with VSS.Strings.Conversions;

with Commands;                       use Commands;
with Commands.Interactive;           use Commands.Interactive;

with GPS.Default_Styles;
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
with DAP.Module;
with DAP.Modules.Preferences;        use DAP.Modules.Preferences;
with DAP.Views.Breakpoints;

with GPS.Debuggers;
with Generic_Views;
with Xref;                           use Xref;
with JSON_Utils;

package body DAP.Modules.Persistent_Breakpoints is

   Me : constant Trace_Handle := Create
     ("GPS.DAP.MODULES_PERSISTENT_BREAKPOINTS");

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

   type On_Before_Exit is new Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean;
   --  Called before GNAT Studio exist. This is a good time to save the
   --  persistent breakpoints.

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

   procedure Save_Persistent_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Save persistent breakpoints to properties.

   function "<" (L, R : Location_Marker) return Boolean;

   package Locations_Sets is new Ada.Containers.Ordered_Sets
     (Location_Marker, "=" => GPS.Markers."=");

   -- Vars --

   Breakpoints : Breakpoint_Holder;
   Shown       : Locations_Sets.Set;

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
         Debugger.Remove_All_Breakpoints;
      end On_Debugger;

   begin
      if DAP.Module.Get_Current_Debugger = null then
         Breakpoints.Clear;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Hide_Breakpoints (Kernel);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Remove_All_Breakpoints;
         end if;
      end if;
   end Clear_All_Breakpoints;

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

      if DAP.Module.Get_Current_Debugger = null then
         Loc := Kernel.Get_Buffer_Factory.Create_Marker
           (File   => File_Information (Context),
            Line   => Editable_Line_Type (Contexts.Line_Information (Context)),
            Column => 1);

         if Breakpoints.Contains (Loc) then
            return Filter.Found;
         else
            return not Filter.Found;
         end if;

      else
         return Filter.Found = DAP.Module.Get_Current_Debugger.Has_Breakpoint
           (File_Information (Context),
            Editable_Line_Type (Contexts.Line_Information (Context)));
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
         null;

      elsif Command.On_Line then
         Break_Source
           (Kernel,
            Num   => No_Breakpoint,
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
                  Num        => No_Breakpoint,
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
               Num  => No_Breakpoint,
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
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Prop  : Breakpoint_Property_Record;
      Found : Boolean;
   begin
      Breakpoints.Clear;
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
        (Prop, GPS.Kernel.Project.Get_Project (Kernel),
         Name => "breakpoints", Found => Found);
      if Found then
         Breakpoints.Initialize (Prop.Breakpoints);
         Breakpoints.Initialized;
         Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);
      end if;
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

   --------------------------------
   -- Get_Persistent_Breakpoints --
   --------------------------------

   function Get_Persistent_Breakpoints return Breakpoint_Vectors.Vector is
   begin
      return Breakpoints.Get_Breakpoints;
   end Get_Persistent_Breakpoints;

   -----------------------------------
   -- Get_Persistent_For_Executable --
   -----------------------------------

   function Get_Persistent_For_Executable
     (Executable : Virtual_File) return Breakpoint_Vectors.Vector is
   begin
      return Breakpoints.Get_Breakpoints (Executable);
   end Get_Persistent_For_Executable;

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

      if Breakpoints.Get_Breakpoints.Is_Empty then
         Trace (Me, "No persistent breakpoint to save");
         GPS.Kernel.Properties.Remove_Property
           (Kernel, GPS.Kernel.Project.Get_Project (Kernel), "breakpoints");
         return;
      end if;

      Trace (Me, "Saving persistent breakpoints");

      GPS.Kernel.Properties.Set_Property
        (Kernel     => Kernel,
         Project    => GPS.Kernel.Project.Get_Project (Kernel),
         Name       => "breakpoints",
         Property   =>
            new Breakpoint_Property_Record'
           (Kernel      => Kernel,
            Breakpoints => Breakpoints.Get_Breakpoints),
         --  Filter breakpoints that are created automatically by GNAT Studio
         --  as a result of preferences.
         Persistent => True);
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
            Value.Set_Field
              ("condition",
               VSS.Strings.Conversions.To_UTF_8_String (B.Condition));
         end if;

         if B.Executable /= Null_Unbounded_String then
            Value.Set_Field ("executable", To_String (B.Executable));
         end if;

         if not B.Commands.Is_Empty then
            Value.Set_Field
              ("command",
               VSS.Strings.Conversions.To_UTF_8_String (B.Commands));
         end if;

         Value.Set_Field
           ("disposition",
            Breakpoint_Disposition'Image (B.Disposition));

         case B.Kind is
            when On_Line =>
               if not B.Locations.Is_Empty then
                  Value.Set_Field
                    ("file", JSON_Utils.Save (Get_File (Get_Location (B))));
                  Value.Set_Field
                    ("line", Editable_Line_Type'Image
                       (Get_Line (Get_Location (B))));
               end if;

            when On_Subprogram =>
               Value.Set_Field ("subprogram", To_String (B.Subprogram));

            when On_Address =>
               --  we do not store breakpoints for addresses
               null;

            when On_Exception =>
               Value.Set_Field ("exception", To_String (B.Except));
               Value.Set_Field ("unhandled", B.Unhandled);
         end case;

         Append (Values, Value);
      end Save;

   begin
      GNATCOLL.Traces.Trace (Me, "Saving breakpoints for future sessions");

      for Data of Property.Breakpoints loop
         if Data.Kind /= On_Address then
            Save (Data);
         end if;
      end loop;

      Value.Set_Field ("dap_breakpoints", Values);
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

      if not Value.Has_Field ("dap_breakpoints") then
         GNATCOLL.Traces.Trace (Me, "No breakpoints");
         return;
      end if;

      Values := Value.Get ("dap_breakpoints");

      for Index in 1 .. Length (Values) loop
         declare
            Id   : constant Breakpoint_Identifier := Breakpoints.Get_Next_Id;
            Item : constant JSON_Value := Get (Values, Index);

            Kind : constant Breakpoint_Kind :=
              Breakpoint_Kind'Value (Item.Get ("kind"));
            Loc  : Location_Marker     := No_Marker;

            Exec : constant Unbounded_String    :=
              (if Item.Has_Field ("executable")
               then To_Unbounded_String
                 (To_String (Item.Get ("executable")))
               else Null_Unbounded_String);

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
                       (Kind          => On_Line,
                        Num           => Id,
                        Disposition   => Breakpoint_Disposition'Value
                          (Item.Get ("disposition")),
                        State         => Enabled,
                        Locations     => Location_Vectors.To_Vector
                          ((Id, Loc, Invalid_Address), 1),
                        Ignore        => Item.Get ("ignore"),
                        Condition     => Condition,
                        Executable    => Exec,
                        Commands      => Commands);
                  end if;

               when On_Subprogram =>
                  B :=
                    (Kind          => On_Subprogram,
                     Num           => Id,
                     Disposition   => Breakpoint_Disposition'Value
                       (Item.Get ("disposition")),
                     State         => Enabled,
                     Subprogram    => Item.Get ("subprogram"),
                     Ignore        => Item.Get ("ignore"),
                     Condition     => Condition,
                     Executable    => Exec,
                     Commands      => Commands,
                     others        => <>);

               when On_Address =>
                  --  we do not store breakpoints for addresses
                  null;

               when On_Exception =>
                  B :=
                    (Kind          => On_Exception,
                     Num           => Id,
                     Disposition   => Breakpoint_Disposition'Value
                       (Item.Get ("disposition")),
                     State         => Enabled,
                     Except        => Item.Get ("exception"),
                     Unhandled     => Item.Get ("unhandled"),
                     Ignore        => Item.Get ("ignore"),
                     Condition     => Condition,
                     Executable    => Exec,
                     Commands      => Commands,
                     others        => <>);
            end case;

            Property.Breakpoints.Append (B);
         end;
      end loop;
   end Load;

   -----------------
   -- Get_Next_Id --
   -----------------

   function Get_Next_Id return Breakpoint_Identifier is
   begin
      return Breakpoints.Get_Next_Id;
   end Get_Next_Id;

   ----------------------
   -- Hide_Breakpoints --
   ----------------------

   procedure Hide_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (Messages_Category_For_Breakpoints,
         Breakpoints_Message_Flags);
      Shown.Clear;
   end Hide_Breakpoints;

   -------------------------------------
   -- Show_Breakpoints_In_All_Editors --
   -------------------------------------

   procedure Show_Breakpoints_In_All_Editors
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Hide_Breakpoints (Kernel);

      for Data of Breakpoints.Get_Breakpoints loop
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
      if B.Locations.Is_Empty
        or else B.Num = 0
      then
         return;
      end if;

      for Loc of B.Locations loop
         if not Shown.Contains (Loc.Marker) then
            Shown.Insert (Loc.Marker);

            File := Get_File (Loc.Marker);
            Line := Get_Line (Loc.Marker);

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
               Display_Popup_When_Alone => False,
               Associated_Command       => Create_Set_Breakpoint_Command
                 (Kernel,
                  Mode => Unset));
            Msg.Set_Action (Action);

            if B.State /= Enabled
              or else B.Num = 0
            then
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
      end loop;
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
         Debugger.Break (Data);
      end On_Debugger;

      Dummy : Triboolean;
   begin
      if DAP.Module.Get_Current_Debugger = null then
         if Data.Num = No_Breakpoint then
            Data.Num := Breakpoints.Get_Next_Id;
            Breakpoints.Added (Data);
         else
            Breakpoints.Replace (Data, Dummy);
         end if;

         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoint (Kernel, Data);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Break (Data);
         end if;
      end if;
   end Break;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Num       : Breakpoint_Identifier;
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
         Num         => Num,
         Locations   => Location_Vectors.To_Vector
           (DAP.Modules.Breakpoints.Location'
                (Num     => 0,
                 Marker  => Kernel.Get_Buffer_Factory.Create_Marker
                   (File   => File,
                    Line   => Line,
                    Column => 1),
                 Address => Invalid_Address),
            1),
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
      Num       : Breakpoint_Identifier;
      Name      : String;
      Unhandled : Boolean := False;
      Temporary : Boolean := False)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Exception,
         Num         => Num,
         Except      => To_Unbounded_String (Name),
         Unhandled   => Unhandled,
         Disposition => (if Temporary then Delete else Keep),
         others      => <>);
   begin
      Break (Kernel, B);
   end Break_Exception;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Num           : Breakpoint_Identifier;
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
         Num         => Num,
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
      Num       : Breakpoint_Identifier;
      Address   : Address_Type;
      Temporary : Boolean := False;
      Condition : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      Ignore    : Natural := 0;
      Commands  : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
      B : Breakpoint_Data := Breakpoint_Data'
        (Kind        => On_Address,
         Num         => Num,
         Address     => Address,
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
         Debugger.Remove_Breakpoint_At (File, Line);
      end On_Debugger;

   begin
      if DAP.Module.Get_Current_Debugger = null then
         Breakpoints.Deleted (File, Line);
         Breakpoints.Set_Numbers;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Remove_Breakpoint_At (File, Line);
         end if;
      end if;
   end Unbreak_Source;

   ---------------------------------
   -- Delete_Multiple_Breakpoints --
   ---------------------------------

   procedure Delete_Multiple_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List)
   is
      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access);
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         Debugger.Remove_Breakpoints (List);
      end On_Debugger;

   begin
      if List.Is_Empty then
         return;
      end if;

      if DAP.Module.Get_Current_Debugger = null then
         Breakpoints.Deleted (List);
         Breakpoints.Set_Numbers;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Kernel, null);
         Show_Breakpoints_In_All_Editors (Kernel);

      else
         if Breakpoints_For_All_Debuggers.Get_Pref then
            DAP.Module.For_Each_Debugger (On_Debugger'Access);
         else
            DAP.Module.Get_Current_Debugger.Remove_Breakpoints (List);
         end if;
      end if;
   end Delete_Multiple_Breakpoints;

   ----------------------------
   -- Set_Breakpoints_State --
   ----------------------------

   procedure Set_Breakpoints_State
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List;
      State  : Boolean) is
   begin
      if List.Is_Empty then
         return;
      end if;

      --  If a debugger is active, enable/disable the breakpoints by
      --  sending the appropriate command.
      --  Otherwise, modify the state of the breakpoints stored in the
      --  persistant list.

      if DAP.Module.Get_Current_Debugger = null then
         Breakpoints.Set_Enabled (List, State);
         Show_Breakpoints_In_All_Editors (Kernel);

      else
         DAP.Module.Get_Current_Debugger.Set_Breakpoints_State (List, State);
      end if;
   end Set_Breakpoints_State;

   -----------
   -- Store --
   -----------

   procedure Store
     (Executable : Virtual_File;
      List       : Breakpoint_Vectors.Vector)
   is
      Count : Natural := 0;
      procedure Calculate (Debugger : DAP.Clients.DAP_Client_Access);
      procedure Calculate (Debugger : DAP.Clients.DAP_Client_Access) is
      begin
         if Debugger.Get_Executable = Executable then
            Count := Count + 1;
         end if;
      end Calculate;

   begin
      if DAP.Module.Get_Started_Per_Session_Debuggers < 2 then
         --  We had only one debugger, copy breakpoints
         Breakpoints.Initialize (List);
      else
         DAP.Module.For_Each_Debugger (Calculate'Access);
         if Count > 1 then
            --  Store breakpoints only from the last closed session
            --  for the executable
            return;
         end if;

         Breakpoints.Replace (Executable, List);

         if DAP.Module.Count_Running_Debuggers < 2 then
            Breakpoints.Set_Numbers;
         end if;
      end if;
   end Store;

   -----------------------------
   -- On_Debugging_Terminated --
   -----------------------------

   procedure On_Debugging_Terminated
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run (Kernel, null);
      if DAP.Module.Get_Breakpoints_View /= null then
         DAP.Views.View_Access
           (DAP.Module.Get_Breakpoints_View).On_Status_Changed
           (GPS.Debuggers.Debug_Available);
      end if;

      Show_Breakpoints_In_All_Editors (Kernel);
      Save_Persistent_Breakpoints (Kernel);
   end On_Debugging_Terminated;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      No_Debugger_Or_Ready : Action_Filter;
   begin
      Project_Changing_Hook.Add (new On_Project_Changing);
      Project_Changed_Hook.Add (new On_Project_Changed);
      Before_Exit_Action_Hook.Add (new On_Before_Exit);

      No_Debugger_Or_Ready := Kernel.Lookup_Filter ("No debugger or ready");

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

      DAP.Views.Breakpoints.Register_Module (Kernel);
   end Register_Module;

end DAP.Modules.Persistent_Breakpoints;
