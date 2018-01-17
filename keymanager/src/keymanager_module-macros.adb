------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;

with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with Glib.Main;                 use Glib, Glib.Main;
with Glib.Object;               use Glib.Object;
with Gdk.Device_Manager;        use Gdk.Device_Manager;
with Gdk.Display;               use Gdk.Display;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types.Keysyms;         use Gdk.Types, Gdk.Types.Keysyms;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Macro;              use Gtkada.Macro;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Intl;                  use GPS.Intl;

package body KeyManager_Module.Macros is
   Me                  : constant Trace_Handle := Create ("Keymanager.Macros");

   Mouse_Macro_Support : constant Trace_Handle :=
                           Create
                             ("Keymanager.Mouse_Macro", GNATCOLL.Traces.Off);
   --  ??? For now disable by default since this is a work in progress

   File_Cst                  : aliased constant String := "file";
   Speed_Cst                 : aliased constant String := "speed";
   Load_Macro_Cmd_Parameters : constant Cst_Argument_List :=
                                 (1 => File_Cst'Access);
   Play_Macro_Cmd_Parameters : constant Cst_Argument_List :=
                                 (1 => Speed_Cst'Access);

   type Events_Mask is array (Gdk_Event_Type) of Boolean;
   --  What type of events should be recorded

   type Event_Set is record
      Events        : Macro_Item_Access;
      --  Set of events recorded

      Last_Event    : Macro_Item_Access;
      --  Last event recorded

      Prev_Time     : Guint32;
      --  Time of previous event recorded

      Current_Event : Macro_Item_Access;
      --  Current event being replayed

      Speed         : Duration := 1.0;
      --  Speed at which replay is made. 1.0 means normal speed

      Child         : GPS_MDI_Child;
      --  Current MDI window when we started playing the macro. This is used
      --  to create undo groups. This will not work when multiple editors are
      --  impacted by the macro, but will enhance behavior when inside a
      --  single editor.
   end record;
   type Event_Set_Access is access all Event_Set;
   --  ??? Some of these fields should be moved to the module directly

   All_Keyboard_Events : constant Events_Mask :=
     (Key_Press | Key_Release => True,
      others                  => False);
   All_Mouse_Events    : constant Events_Mask :=
     (Button_Press
      | Gdk_2button_Press
      | Gdk_3button_Press
      | Button_Release
      | Motion_Notify
      | Scroll         => True,
      others           => False);

   type Keymanager_Macro_Module_Record is new Module_ID_Record with record
      Recording        : Boolean := False;
      --  Whether we are currently recording

      Current_Macro : Event_Set_Access;
      --  The current macro, there is currently only one of those.
      --  In the final version of the code, we should have a table of those

      Event_Mask        : Events_Mask;
      --  Mask of events to record

      Start_Clock       : Ada.Calendar.Time;
      --  Start time of event replay

      Time_Spent        : Guint32;
      --  Virtual time spent so far in events (addition of Events.Time)
   end record;
   type Keymanager_Macro_Module_Id
     is access all Keymanager_Macro_Module_Record'Class;

   overriding procedure Destroy
     (Module : in out Keymanager_Macro_Module_Record);
   --  See inherited doc

   Keymanager_Macro_Module : Keymanager_Macro_Module_Id;

   procedure Free (Events : in out Event_Set_Access);
   --  Free events and its associated list of events

   package Event_Timeout is new Glib.Main.Generic_Sources (Event_Set_Access);

   function General_Event_Handler
     (Event  : Gdk_Event;
      Kernel : access Kernel_Handle_Record'Class) return Boolean;
   --  Event handler called before even gtk can do its dispatching. This
   --  intercepts all events going through the application

   procedure Play_Current_Event_And_Timeout
     (Kernel : Kernel_Handle;
      Events : Event_Set_Access);
   --  Play the current event, and start a timeout that will play the next

   procedure Macro_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the key manager module

   function Record_Macro
     (Kernel : Kernel_Handle;
      Mask   : Events_Mask) return Event_Set_Access;
   --  Start record of all events. All recorded events will be stored in the
   --  return value (none are stored when this function returns, since
   --  recording is asynchronous.

   procedure Play_Macro
     (Kernel : Kernel_Handle;
      Speed  : Duration := 1.0;
      Macro  : Event_Set_Access);
   --  Play current set of events

   procedure Stop_Macro
     (Kernel : Kernel_Handle;
      Events : Event_Set_Access);
   --  Stop the current macro

   type On_Stop_Macro is new Simple_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Stop_Macro;
       Kernel : not null access Kernel_Handle_Record'Class);
   --  Stop running current macro as a result of the "stop_macro_action_hook'

   type In_Macro_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Macro_Filter;
      Context : Selection_Context) return Boolean;

   type Has_Macro_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Macro_Filter;
      Context : Selection_Context) return Boolean;

   function Load_Macro
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Event_Set_Access;
   --  Load macro file

   type Macro_Load_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Macro_Load_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for loading set of events to replay

   type Macro_Save_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Macro_Save_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Save last set of events recorded

   function Play_Macro_Timer (Events : Event_Set_Access) return Boolean;
   --  Move to next event, and play it

   type Macro_Command_Action is
     (Action_Start_Keyboard,
      Action_Start_Mouse,
      Action_Stop,
      Action_Play);

   type Macro_Command is new Interactive_Command with record
      Action : Macro_Command_Action;
   end record;
   overriding function Execute
     (Command : access Macro_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Execute a macro-related action

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Keymanager_Macro_Module_Record) is
   begin
      Free (Module.Current_Macro);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (Events : in out Event_Set_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Event_Set, Event_Set_Access);
   begin
      if Events /= null then
         Free_List (Events.Events);
         Unchecked_Free (Events);
      end if;
   end Free;

   ---------------------------
   -- Macro_Command_Handler --
   ---------------------------

   procedure Macro_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = "macro_load" then
         Name_Parameters (Data, Load_Macro_Cmd_Parameters);

         declare
            File    : constant Filesystem_String := Nth_Arg (Data, 1);
         begin
            Free (Keymanager_Macro_Module.Current_Macro);
            Keymanager_Macro_Module.Current_Macro :=
              Load_Macro (Get_Kernel (Data), Create (File));

            if Keymanager_Macro_Module.Current_Macro = null then
               Set_Error_Msg
                 (Data, Command & ": " & (-"error while reading file"));
            end if;
         end;

      elsif Command = "macro_play" then
         Name_Parameters (Data, Play_Macro_Cmd_Parameters);

         declare
            Speed : constant String := Nth_Arg (Data, 1, Default => "1.0");
         begin
            Play_Macro
              (Get_Kernel (Data), Duration'Value (Speed),
               Keymanager_Macro_Module.Current_Macro);
         exception
            when Constraint_Error =>
               Set_Error_Msg (Data, Command & ": " & (-"invalid speed value"));
         end;

      elsif Command = "macro_record" then
         Free (Keymanager_Macro_Module.Current_Macro);
         Keymanager_Macro_Module.Current_Macro :=
           Record_Macro (Get_Kernel (Data), All_Keyboard_Events);
         Get_Kernel (Data).Refresh_Context;
      end if;

   exception
      when E : others => Trace (Me, E);
   end Macro_Command_Handler;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Macro_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
   begin
      return Keymanager_Macro_Module.Recording;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Macro_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
   begin
      return not Keymanager_Macro_Module.Recording
        and then Keymanager_Macro_Module.Current_Macro /= null;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Macro_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      case Command.Action is
         when Action_Start_Keyboard =>
            --  If we are not recording a macro and not playing one
            if not Keymanager_Macro_Module.Recording
              and then
                (Keymanager_Macro_Module.Current_Macro = null
                 or else Keymanager_Macro_Module.Current_Macro.Current_Event =
                 null)
            then
               Trace (Me, "Start recording keyboard macro");
               Free (Keymanager_Macro_Module.Current_Macro);
               Keymanager_Macro_Module.Current_Macro := Record_Macro
                 (Kernel, All_Keyboard_Events);
               Kernel.Refresh_Context;
            end if;

         when Action_Start_Mouse =>
            --  If we are not recording a macro and not playing one
            if not Keymanager_Macro_Module.Recording
              and then
                (Keymanager_Macro_Module.Current_Macro = null
                 or else Keymanager_Macro_Module.Current_Macro.Current_Event =
                 null)
            then
               Trace (Me, "Start recording mouse macro");
               Free (Keymanager_Macro_Module.Current_Macro);
               Keymanager_Macro_Module.Current_Macro := Record_Macro
                 (Kernel, All_Keyboard_Events or All_Mouse_Events);
               Kernel.Refresh_Context;
            end if;

         when Action_Stop =>
            if Keymanager_Macro_Module.Recording then
               Trace (Me, "Stop recording macro");
               Keymanager_Macro_Module.Recording := False;
               Remove_Event_Handler
                 (Kernel, General_Event_Handler'Access);

               --  We need to refresh the context because response from
               --  In_Macro filter was cached in the context and this cache
               --  will not be dropped until context is changed.
               Kernel.Refresh_Context;
            end if;

         when Action_Play =>
            if not Keymanager_Macro_Module.Recording then
               Trace (Me, "Play macro");
               --  ??? We should check in the macro itself whether there are
               --  mouse events
               if Active (Mouse_Macro_Support) then
                  Play_Macro
                    (Kernel,
                     Macro => Keymanager_Macro_Module.Current_Macro,
                     Speed => 1.0);
               else
                  Play_Macro
                    (Kernel,
                     Macro => Keymanager_Macro_Module.Current_Macro,
                     Speed => 10_000.0);
               end if;
            end if;

      end case;
      return Commands.Success;
   end Execute;

   ------------------
   -- Record_Macro --
   ------------------

   function Record_Macro
     (Kernel : Kernel_Handle;
      Mask   : Events_Mask) return Event_Set_Access
   is
      Events : Event_Set_Access;
   begin
      --  ??? There's no way to remove Pointer_Motion_Mask afterwards
      Add_Events (Get_Main_Window (Kernel), Pointer_Motion_Mask);

      Events := new Event_Set;
      Events.Last_Event := null;
      Events.Prev_Time  := 0;

      Keymanager_Macro_Module.Event_Mask := Mask;
      Keymanager_Macro_Module.Recording := True;
      Add_Event_Handler (Kernel, General_Event_Handler'Access);
      return Events;
   end Record_Macro;

   ------------------------------------
   -- Play_Current_Event_And_Timeout --
   ------------------------------------

   procedure Play_Current_Event_And_Timeout
     (Kernel : Kernel_Handle;
      Events : Event_Set_Access)
   is
      Current_Event : Macro_Item_Access renames Events.Current_Event;
      Timeout       : Guint;
      Wait          : Duration;
      Ignore        : Boolean;
      Ignore_Id     : G_Source_Id;
      pragma Unreferenced (Ignore, Ignore_Id);
   begin
      if Current_Event /= null then
         Ignore := Play_Event
           (Current_Event.all,
            Device         =>
              Get_Device_Manager (Gdk.Display.Get_Default).Get_Client_Pointer,
            Default_Widget => Gtk_Widget (Get_Main_Window (Kernel)));

         --  Compute proper timeout value, taking into account the time
         --  spent to handle each event manually.

         Keymanager_Macro_Module.Time_Spent :=
           Keymanager_Macro_Module.Time_Spent + Current_Event.Time;
         Wait := Keymanager_Macro_Module.Start_Clock - Clock +
           Duration (Keymanager_Macro_Module.Time_Spent) /
           Duration (Events.Speed * 1000.0);

         if Wait > 0.0 then
            Timeout := Guint (Wait * 1000.0);
         else
            Timeout := 0;
         end if;

         --  Play low priority, so that it plays after the event we just sent
         --  has been processed. In particular, this ensures that if we select
         --  the "Start Recording Macro" menu through keyboard shortcuts, we
         --  can properly detect we are still processing a macro, and do
         --  nothing in that case (FC14-014)

         Ignore_Id := Event_Timeout.Timeout_Add
           (Timeout, Play_Macro_Timer'Access, Events,
            Priority => Priority_Low);
      end if;
   end Play_Current_Event_And_Timeout;

   ----------------------
   -- Play_Macro_Timer --
   ----------------------

   function Play_Macro_Timer (Events : Event_Set_Access) return Boolean is
      Kernel        : constant Kernel_Handle :=
                        Get_Kernel (Keymanager_Macro_Module.all);
      Current_Event : Macro_Item_Access renames Events.Current_Event;

   begin
      --  Move to next event
      if Current_Event /= null then
         Current_Event := Current_Event.Next;
      end if;

      if Current_Event = null then
         Stop_Macro (Kernel, Events);
      else
         Play_Current_Event_And_Timeout (Kernel, Events);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Play_Macro_Timer;

   ----------------
   -- Stop_Macro --
   ----------------

   procedure Stop_Macro
     (Kernel : Kernel_Handle;
      Events : Event_Set_Access)
   is
      pragma Unreferenced (Kernel);
   begin
      Events.Current_Event := null;

      if Events.Child /= null then
         End_Group (Get_Command_Queue (Events.Child));
      end if;
   end Stop_Macro;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Stop_Macro;
       Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      if Keymanager_Macro_Module.Current_Macro /= null then
         Stop_Macro
           (Kernel_Handle (Kernel), Keymanager_Macro_Module.Current_Macro);
      end if;
   end Execute;

   ----------------
   -- Play_Macro --
   ----------------

   procedure Play_Macro
     (Kernel : Kernel_Handle;
      Speed  : Duration := 1.0;
      Macro  : Event_Set_Access)
   is
      Id         : G_Source_Id;
      pragma Unreferenced (Id);
      C          : MDI_Child;

   begin
      if Macro /= null then
         if Keymanager_Macro_Module.Current_Macro /= null
           and then Keymanager_Macro_Module.Current_Macro.Current_Event /= null
         then
            --  Stop the current macro instead (FC14-014)
            Trace (Me, "Play_Macro: a macro is already playing, stopping it"
                   & " and cancelling new call to play");
            Stop_Macro (Kernel, Keymanager_Macro_Module.Current_Macro);

         else
            Macro.Current_Event := Macro.Events;

            if Macro.Current_Event /= null then
               C := Get_Focus_Child (Get_MDI (Kernel));
               if C.all in GPS_MDI_Child_Record'Class then
                  Macro.Child := GPS_MDI_Child (C);
               end if;

               if Macro.Child /= null then
                  Start_Group (Get_Command_Queue (Macro.Child));
               end if;

               Keymanager_Macro_Module.Start_Clock := Clock;
               Keymanager_Macro_Module.Time_Spent  := 0;
               Macro.Speed                         := Speed;

               Play_Current_Event_And_Timeout (Kernel, Macro);
            end if;
         end if;
      end if;
   end Play_Macro;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Macro_Load_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      declare
         Name    : constant Virtual_File :=
           Select_File
             (Title             => -"Load Macro",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if Name = GNATCOLL.VFS.No_File then
            return Commands.Failure;
         end if;

         Free (Keymanager_Macro_Module.Current_Macro);
         Keymanager_Macro_Module.Current_Macro := Load_Macro (Kernel, Name);

         if Keymanager_Macro_Module.Current_Macro = null then
            Insert (Kernel, -"Error while loading macro", Mode => Error);
            return Commands.Failure;
         end if;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Macro_Save_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Events : constant Macro_Item_Access :=
                 Keymanager_Macro_Module.Current_Macro.Events;
   begin
      if Events = null then
         return Commands.Failure;
      end if;

      declare
         Success : Boolean;
         Name    : constant Virtual_File :=
                     Select_File
                       (Title             => -"Save Macro As",
                        Parent            => Get_Current_Window (Kernel),
                        Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                        Kind              => Save_File,
                        History           => Get_History (Kernel));

      begin
         if Name = GNATCOLL.VFS.No_File then
            return Commands.Failure;
         end if;

         Success := Save_List (+Full_Name (Name), Events);

         if not Success then
            Insert (Kernel, -"Error while saving macro", Mode => Error);
            return Commands.Failure;
         end if;
      end;

      return Commands.Success;
   end Execute;

   ----------------
   -- Load_Macro --
   ----------------

   function Load_Macro
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Event_Set_Access
   is
      pragma Unreferenced (Kernel);
      Buffer  : String_Access := Read_File (File);
      Macro   : Event_Set_Access;
      Success : Boolean;
   begin
      if Buffer /= null then
         Macro := new Event_Set;
         Load_List (Buffer.all, Macro.Events, Success);
         if not Success then
            Free (Macro);
         end if;
         Free (Buffer);
      end if;
      return Macro;
   end Load_Macro;

   ---------------------------
   -- General_Event_Handler --
   ---------------------------

   function General_Event_Handler
     (Event  : Gdk_Event;
      Kernel : access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Kernel);
      Macro       : constant Event_Set_Access :=
                      Keymanager_Macro_Module.Current_Macro;
      Event_Type  : constant Gdk_Event_Type := Get_Event_Type (Event);
      Key_Item    : Macro_Item_Key_Access;
      Button_Item : Macro_Item_Mouse_Access;
      Motion_Item : Macro_Item_Motion_Access;
      Scroll_Item : Macro_Item_Scroll_Access;

      procedure Save_Item (Item : Macro_Item_Access);
      --  Save item in list of current events, if non null

      ---------------
      -- Save_Item --
      ---------------

      procedure Save_Item (Item : Macro_Item_Access) is
      begin
         if Item /= null then
            if Macro.Events = null then
               Macro.Events := Item;
               Macro.Last_Event := Item;

            else
               --  Store the relative time, to ease replay

               Item.Prev := Macro.Last_Event;
               Macro.Last_Event.Next := Item;
               Macro.Last_Event := Item;
            end if;

            Macro.Prev_Time := Get_Time (Event);
         end if;
      end Save_Item;

   begin
      if Keymanager_Macro_Module.Event_Mask (Event_Type) then
         case Event_Type is
            when Key_Press | Key_Release =>
               if Macro.Current_Event /= null
                 and then Event.Key.Send_Event /= 0
                 and then Event.Key.Keyval = GDK_Escape
               then
                  Trace (Me, "Replay cancelled");
                  Macro.Current_Event := null;
                  return True;
               end if;

               Key_Item := Create_Item (Event.Key, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Key_Item));

            when Button_Press | Button_Release
               | Gdk_2button_Press
               | Gdk_3button_Press
                 =>
               Button_Item := Create_Item (Event.Button, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Button_Item));

            when Motion_Notify =>
               Motion_Item := Create_Item (Event.Motion, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Motion_Item));

            when Scroll =>
               Scroll_Item := Create_Item (Event.Scroll, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Scroll_Item));

               --  Other events should not be needed: they will be generated as
               --  part the basic events handled above (keyboard/mouse events).

            when others =>
               null;
         end case;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end General_Event_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command    : Interactive_Command_Access;
      Filter     : Action_Filter;
   begin
      Keymanager_Macro_Module := new Keymanager_Macro_Module_Record;
      Register_Module (Keymanager_Macro_Module, Kernel, "macros");

      Command := new Macro_Command;
      Macro_Command (Command.all).Action := Action_Start_Keyboard;
      Register_Action
        (Kernel,
         Name        => "Macro Start Keyboard",
         Command     => Command,
         Description =>
           -("Start recording a keyboard macro. Only keyboard events are"
             & " recorded."),
         Category    => -"Macro");

      if Active (Mouse_Macro_Support) then
         Command := new Macro_Command;
         Macro_Command (Command.all).Action := Action_Start_Mouse;
         Register_Action
           (Kernel,
            Name        => "Macro Start Mouse",
            Command     => Command,
            Description =>
              -("Start recording a general macro. Mouse and keyboards events"
                & " are recorded, and can be replayed later on"),
            Category    => -"Macro");
      end if;

      Command := new Macro_Command;
      Macro_Command (Command.all).Action := Action_Stop;
      Filter := new In_Macro_Filter;
      Register_Action
        (Kernel,
         Name        => "Macro Stop",
         Command     => Command,
         Description => -"Stop recording the current macro",
         Filter      => Filter,
         Category    => -"Macro");

      Command := new Macro_Command;
      Macro_Command (Command.all).Action := Action_Play;
      Filter := new Has_Macro_Filter;
      Register_Action
        (Kernel,
         Name        => "Macro Play",
         Command     => Command,
         Description => -"Replay the last macro that was recorded",
         Category    => -"Macro",
         Filter      => Filter);

      Register_Action
         (Kernel, "macro load", new Macro_Load_Command,
          -"Load a macro from an external file");

      Register_Action
         (Kernel, "macro save", new Macro_Save_Command,
          -"Save the current macro to an external file",
          Filter => Filter);

      --  ??? Do not export these commands for now. We should have a class to
      --  represent a macro, so that several macros can be accessible at the
      --  same time. We'll make the command visible only when this is available
      --  so that scripts do not start depending on the current API
      if Active (Mouse_Macro_Support) then
         Register_Command
           (Kernel, "macro_play",
            Maximum_Args => 1,
            Handler      => Macro_Command_Handler'Access);
         Register_Command
           (Kernel, "macro_record",
            Handler      => Macro_Command_Handler'Access);
         Register_Command
           (Kernel, "macro_load",
            Minimum_Args => 1,
            Maximum_Args => 1,
            Handler      => Macro_Command_Handler'Access);
      end if;

      Stop_Macro_Action_Hook.Add (new On_Stop_Macro);
   end Register_Module;

end KeyManager_Module.Macros;
