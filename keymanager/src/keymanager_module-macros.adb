-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2006                       --
--                             AdaCore                               --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Exceptions;          use Ada.Exceptions;
with Commands.Interactive;    use Commands, Commands.Interactive;
with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types, Gdk.Types.Keysyms;
with Glib.Convert;            use Glib, Glib.Convert;
with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib.Object;
with GNAT.Strings;            use GNAT.Strings;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Intl;                use GPS.Intl;
with Gtkada.File_Selector;    use Gtkada.File_Selector;
with Gtkada.Macro;            use Gtkada.Macro;
with Gtkada.MDI;              use Gtkada.MDI;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Traces;                  use Traces;
with VFS;                     use VFS;

package body KeyManager_Module.Macros is
   Me        : constant Debug_Handle := Create ("Keymanager.Macros");

   Mouse_Macro_Support : constant Debug_Handle :=
     Create ("Keymanager.Mouse_Macro", Off);
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
      Events           : Macro_Item_Access;
      --  Set of events recorded.

      Last_Event       : Macro_Item_Access;
      --  Last event recorded.

      Prev_Time         : Guint32;
      --  Time of previous event recorded.

      Current_Event    : Macro_Item_Access;
      --  Current event being replayed.

      Speed            : Duration := 1.0;
      --  Speed at which replay is made. 1.0 means normal speed.

      Child            : GPS_MDI_Child;
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
      --  Start time of event replay.

      Time_Spent        : Guint32;
      --  Virtual time spent so far in events (addition of Events.Time)
   end record;
   type Keymanager_Macro_Module_Id
     is access all Keymanager_Macro_Module_Record'Class;

   procedure Destroy (Module : in out Keymanager_Macro_Module_Record);
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

   procedure Macro_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the key manager module.

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
   --  Play current set of events.

   function Load_Macro
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : Virtual_File) return Event_Set_Access;
   --  Load macro file.

   procedure On_Load_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for loading set of events to replay.

   procedure On_Save_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Save last set of events recorded.

   function Play_Macro_Timer (Events : Event_Set_Access) return Boolean;
   --  Timer used by On_Play_Macro

   type Macro_Command_Action is
     (Action_Start_Keyboard,
      Action_Start_Mouse,
      Action_Stop,
      Action_Play);

   type Macro_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
      Action : Macro_Command_Action;
   end record;
   function Execute
     (Command : access Macro_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Execute a macro-related action

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Keymanager_Macro_Module_Record) is
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
            File    : constant String := Nth_Arg (Data, 1);
            Macro   : constant String := '/' & (-"Tools/Macro") & '/';

         begin
            Free (Keymanager_Macro_Module.Current_Macro);
            Keymanager_Macro_Module.Current_Macro :=
              Load_Macro (Get_Kernel (Data), Create (File));

            if Keymanager_Macro_Module.Current_Macro /= null then
               Set_Sensitive
                 (Find_Menu_Item (Get_Kernel (Data), Macro & (-"Play")), True);
            else
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
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Macro_Command_Handler;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Macro_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Context);
   begin
      case Command.Action is
         when Action_Start_Keyboard =>
            if not Keymanager_Macro_Module.Recording then
               Free (Keymanager_Macro_Module.Current_Macro);
               Keymanager_Macro_Module.Current_Macro := Record_Macro
                 (Command.Kernel, All_Keyboard_Events);
            end if;

         when Action_Start_Mouse =>
            if not Keymanager_Macro_Module.Recording then
               Free (Keymanager_Macro_Module.Current_Macro);
               Keymanager_Macro_Module.Current_Macro := Record_Macro
                 (Command.Kernel, All_Keyboard_Events or All_Mouse_Events);
            end if;

         when Action_Stop =>
            if Keymanager_Macro_Module.Recording then
               declare
                  Macro : constant String := '/' & (-"Tools/Macro") & '/';
               begin
                  Keymanager_Macro_Module.Recording := False;
                  Remove_Event_Handler
                    (Command.Kernel, General_Event_Handler'Access);
                  Set_Follow_Events (False);
                  Set_Sensitive
                    (Find_Menu_Item
                       (Command.Kernel, Macro & (-"Start Keyboard Macro")),
                     True);
                  Set_Sensitive
                    (Find_Menu_Item
                       (Command.Kernel, Macro & (-"Stop Recording")), False);
                  Set_Sensitive
                    (Find_Menu_Item
                       (Command.Kernel, Macro & (-"Play")), True);
                  Set_Sensitive
                    (Find_Menu_Item
                       (Command.Kernel, Macro & (-"Save As...")), True);
               end;
            end if;

         when Action_Play =>
            if not Keymanager_Macro_Module.Recording then
               --  ??? We should check in the macro itself whether there are
               --  mouse events
               if Active (Mouse_Macro_Support) then
                  Play_Macro
                    (Command.Kernel,
                     Macro => Keymanager_Macro_Module.Current_Macro,
                     Speed => 1.0);
               else
                  Play_Macro
                    (Command.Kernel,
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
      Macro  : constant String := '/' & (-"Tools/Macro") & '/';
      Events : Event_Set_Access;
   begin
      Set_Follow_Events (True);

      --  ??? There's no way to remove Pointer_Motion_Mask afterwards
      Add_Events (Get_Main_Window (Kernel), Pointer_Motion_Mask);

      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Start Keyboard Macro")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Stop Recording")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Play")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Save As...")), False);

      Events := new Event_Set;
      Events.Last_Event := null;
      Events.Prev_Time  := 0;

      Keymanager_Macro_Module.Event_Mask := Mask;
      Keymanager_Macro_Module.Recording := True;
      Add_Event_Handler (Kernel, General_Event_Handler'Access);
      return Events;
   end Record_Macro;

   ----------------------
   -- Play_Macro_Timer --
   ----------------------

   function Play_Macro_Timer (Events : Event_Set_Access) return Boolean is
      Kernel        : constant Kernel_Handle :=
        Get_Kernel (Keymanager_Macro_Module.all);
      Macro         : constant String := '/' & (-"Tools/Macro") & '/';
      Current_Event : Macro_Item_Access renames Events.Current_Event;
      Timeout       : Guint;
      Wait          : Duration;
      Success       : Boolean;
      Id            : G_Source_Id;
      pragma Unreferenced (Id, Success);

   begin
      if Current_Event /= null then
         Success := Play_Event
           (Current_Event.all, Gtk_Widget (Get_Main_Window (Kernel)));
         Current_Event := Current_Event.Next;
      end if;

      if Current_Event = null then
         Set_Sensitive
           (Find_Menu_Item (Kernel, Macro & (-"Play")), True);

         if Events.Child /= null then
            End_Group (Get_Command_Queue (Events.Child));
         end if;

      else
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

         Id := Event_Timeout.Timeout_Add
           (Timeout, Play_Macro_Timer'Access, Events);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return False;
   end Play_Macro_Timer;

   ----------------
   -- Play_Macro --
   ----------------

   procedure Play_Macro
     (Kernel : Kernel_Handle;
      Speed  : Duration := 1.0;
      Macro  : Event_Set_Access)
   is
      Macro_Menu    : constant String := '/' & (-"Tools/Macro") & '/';
      Id            : G_Source_Id;
      pragma Unreferenced (Id);
      C             : MDI_Child;

   begin
      if Macro /= null then
         Macro.Current_Event := Macro.Events;

         if Macro.Current_Event /= null then
            C := Get_Focus_Child (Get_MDI (Kernel));
            if C.all in GPS_MDI_Child_Record'Class then
               Macro.Child := GPS_MDI_Child (C);
            end if;

            if Macro.Child /= null then
               Start_Group (Get_Command_Queue (Macro.Child));
            end if;

            Set_Sensitive
              (Find_Menu_Item (Kernel, Macro_Menu & (-"Play")), False);
            Keymanager_Macro_Module.Start_Clock := Clock;
            Keymanager_Macro_Module.Time_Spent  := 0;
            Macro.Speed                         := Speed;
            Id :=
              Event_Timeout.Timeout_Add (0, Play_Macro_Timer'Access, Macro);
         end if;
      end if;
   end Play_Macro;

   -------------------
   -- On_Load_Macro --
   -------------------

   procedure On_Load_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Macro : constant String := '/' & (-"Tools/Macro") & '/';
      pragma Unreferenced (Widget);
   begin
      declare
         Name    : constant Virtual_File :=
           Select_File
             (Title             => -"Load Macro",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if Name = VFS.No_File then
            return;
         end if;

         Free (Keymanager_Macro_Module.Current_Macro);
         Keymanager_Macro_Module.Current_Macro := Load_Macro (Kernel, Name);

         if Keymanager_Macro_Module.Current_Macro /= null then
            Set_Sensitive (Find_Menu_Item (Kernel, Macro & (-"Play")), True);
         else
            Insert (Kernel, -"Error while loading macro", Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Load_Macro;

   -------------------
   -- On_Save_Macro --
   -------------------

   procedure On_Save_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Events : constant Macro_Item_Access :=
        Keymanager_Macro_Module.Current_Macro.Events;
   begin
      if Events = null then
         return;
      end if;

      declare
         Success : Boolean;
         Name    : constant Virtual_File :=
           Select_File
             (Title             => -"Save Macro As",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Save_File,
              History           => Get_History (Kernel));

      begin
         if Name = VFS.No_File then
            return;
         end if;

         Success :=
           Save_List (Locale_From_UTF8 (Full_Name (Name).all), Events);

         if not Success then
            Insert (Kernel, -"Error while saving macro", Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Save_Macro;

   ----------------
   -- Load_Macro --
   ----------------

   function Load_Macro
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : Virtual_File) return Event_Set_Access
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
               --  Store the relative time, to ease replay.

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
                 and then not Get_Send_Event (Event)
                 and then Get_Key_Val (Event) = GDK_Escape
               then
                  Trace (Me, "Replay cancelled");
                  Macro.Current_Event := null;
                  return True;
               end if;

               Key_Item := Create_Item (Event, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Key_Item));

            when Button_Press | Button_Release
               | Gdk_2button_Press
               | Gdk_3button_Press
                 =>
               Button_Item := Create_Item (Event, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Button_Item));

            when Motion_Notify =>
               Motion_Item := Create_Item (Event, Macro.Prev_Time);
               Save_Item (Macro_Item_Access (Motion_Item));

            when Scroll =>
               Scroll_Item := Create_Item (Event, Macro.Prev_Time);
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
         Trace
           (Exception_Handle,
            "Unexpected exception: " & Exception_Information (E));
      return False;
   end General_Event_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Macro_Menu : constant String := "/" & (-"Tools/M_acro");
      Command    : Interactive_Command_Access;
      Action     : Action_Record_Access;
   begin
      Keymanager_Macro_Module := new Keymanager_Macro_Module_Record;
      Register_Module (Keymanager_Macro_Module, Kernel, "macros");

      Command := new Macro_Command;
      Macro_Command (Command.all).Action := Action_Start_Keyboard;
      Macro_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Action := Register_Action
        (Kernel,
         Name        => "Macro Start Keyboard",
         Command     => Command,
         Description =>
           -("Start recording a keyboard macro. Only keyboard events are"
             & " recorded."),
         Category    => -"Macro");
      Register_Menu
        (Kernel, Macro_Menu, -"_Start Keyboard Macro",
         Callback => null,
         Action   => Action);

      if Active (Mouse_Macro_Support) then
         Command := new Macro_Command;
         Macro_Command (Command.all).Action := Action_Start_Mouse;
         Macro_Command (Command.all).Kernel := Kernel_Handle (Kernel);
         Action := Register_Action
           (Kernel,
            Name        => "Macro Start Mouse",
            Command     => Command,
            Description =>
              -("Start recording a general macro. Mouse and keyboards events"
                & " are recorded, and can be replayed later on"),
            Category    => -"Macro");
         Register_Menu
           (Kernel, Macro_Menu, -"_Start Mouse Macro",
            Callback => null,
            Action   => Action);
      end if;

      Command := new Macro_Command;
      Macro_Command (Command.all).Action := Action_Stop;
      Macro_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Action := Register_Action
        (Kernel,
         Name        => "Macro Stop",
         Command     => Command,
         Description => -"Stop recording the current macro",
         Category    => -"Macro");
      Register_Menu
        (Kernel, Macro_Menu, -"_Stop Recording",
         Callback  => null,
         Sensitive => False,
         Action    => Action);
      Bind_Default_Key (Kernel, "Macro Stop", "control-escape");

      Command := new Macro_Command;
      Macro_Command (Command.all).Action := Action_Play;
      Macro_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Action := Register_Action
        (Kernel,
         Name        => "Macro Play",
         Command     => Command,
         Description => -"Replay the last macro that was recorded",
         Category    => -"Macro");
      Register_Menu
        (Kernel, Macro_Menu, -"_Play",
         Callback  => null,
         Sensitive => False,
         Action    => Action);

      Register_Menu
        (Kernel, Macro_Menu, -"Load...",
         Callback  => On_Load_Macro'Access);
      Register_Menu
        (Kernel, Macro_Menu, -"_Save As...",
         Callback  => On_Save_Macro'Access,
         Sensitive => False);

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
   end Register_Module;

end KeyManager_Module.Macros;
