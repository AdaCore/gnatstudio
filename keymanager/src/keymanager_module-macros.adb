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
with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types, Gdk.Types.Keysyms;
with Glib.Convert;            use Glib, Glib.Convert;
with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib.Object;
with GNAT.Strings;            use GNAT.Strings;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Intl;                use GPS.Intl;
with Gtkada.File_Selector;    use Gtkada.File_Selector;
with Gtkada.Macro;            use Gtkada.Macro;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Traces;                  use Traces;
with VFS;                     use VFS;

package body KeyManager_Module.Macros is
   Me        : constant Debug_Handle := Create ("Keymanager.Macros");
   Use_Macro : constant Debug_Handle := Create ("Keymanager.Macro", Off);
   --  ??? For now disable by default since this is a work in progress

   File_Cst                  : aliased constant String := "file";
   Speed_Cst                 : aliased constant String := "speed";
   Load_Macro_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => File_Cst'Access);
   Play_Macro_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Speed_Cst'Access);

   type Event_Set is record
      Events           : Macro_Item_Access;
      --  Set of events recorded.

      Kernel           : Kernel_Handle;

      Last_Event       : Macro_Item_Access;
      --  Last event recorded.

      Current_Event    : Macro_Item_Access;
      --  Current event being replayed.

      Start_Clock       : Ada.Calendar.Time;
      --  Start time of event replay.

      Time_Spent        : Guint32;
      --  Virtual time spent so far in events (addition of Events.Time)

      Prev_Time         : Guint32;
      --  Time of previous event recorded.

      Speed            : Duration := 1.0;
      --  Speed at which replay is made. 1.0 means normal speed.
   end record;
   type Event_Set_Access is access all Event_Set;

   Current_Macro : Event_Set_Access;
   --  ??? The current macro, there is currently only one of those.
   --  In the final version of the code, we should have a table of those,
   --  probably stored in a local module.

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

   function Record_Macro (Kernel : Kernel_Handle) return Event_Set_Access;
   --  Start record of all events. All recorded events will be stored in the
   --  return value (none are stored when this function returns, since
   --  recording is asynchronous.

   procedure On_Start_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for starting record of all events for later re-play.

   procedure On_Stop_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Stop recording key events for later re-play.

   procedure On_Play_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for playing current set of events.

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

   ----------
   -- Free --
   ----------

   procedure Free (Events : in out Event_Set_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Event_Set, Event_Set_Access);
   begin
      Free_List (Events.Events);
      Unchecked_Free (Events);
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
            Free (Current_Macro);
            Current_Macro := Load_Macro (Get_Kernel (Data), Create (File));

            if Current_Macro /= null then
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
              (Get_Kernel (Data), Duration'Value (Speed), Current_Macro);
         exception
            when Constraint_Error =>
               Set_Error_Msg (Data, Command & ": " & (-"invalid speed value"));
         end;

      elsif Command = "macro_record" then
         Free (Current_Macro);
         Current_Macro := Record_Macro (Get_Kernel (Data));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Macro_Command_Handler;

   ------------------------
   -- On_Start_Recording --
   ------------------------

   procedure On_Start_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Free (Current_Macro);
      Current_Macro := Record_Macro (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Start_Recording;

   ------------------
   -- Record_Macro --
   ------------------

   function Record_Macro (Kernel : Kernel_Handle) return Event_Set_Access is
      Macro  : constant String := '/' & (-"Tools/Macro") & '/';
      Events : Event_Set_Access;
   begin
      Set_Follow_Events (True);

      --  ??? There's no way to remove Pointer_Motion_Mask afterwards
      Add_Events (Get_Main_Window (Kernel), Pointer_Motion_Mask);

      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Start Recording")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Stop Recording")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Play")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Save As...")), False);

      Events := new Event_Set;
      Events.Kernel     := Kernel;
      Events.Last_Event := null;
      Events.Prev_Time  := 0;

      Add_Event_Handler (Kernel, General_Event_Handler'Access);
      return Events;
   end Record_Macro;

   -----------------------
   -- On_Stop_Recording --
   -----------------------

   procedure On_Stop_Recording
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Macro : constant String := '/' & (-"Tools/Macro") & '/';
   begin
      Remove_Event_Handler (Kernel, General_Event_Handler'Access);
      Set_Follow_Events (False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Start Recording")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Stop Recording")), False);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Play")), True);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Macro & (-"Save As...")), True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Stop_Recording;

   ----------------------
   -- Play_Macro_Timer --
   ----------------------

   function Play_Macro_Timer (Events : Event_Set_Access) return Boolean is
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
           (Current_Event.all, Gtk_Widget (Get_Main_Window (Events.Kernel)));
         Current_Event := Current_Event.Next;
      end if;

      if Current_Event = null then
         Set_Sensitive
           (Find_Menu_Item (Events.Kernel, Macro & (-"Play")), True);
      else
         --  Compute proper timeout value, taking into account the time
         --  spent to handle each event manually.

         Events.Time_Spent := Events.Time_Spent + Current_Event.Time;
         Wait := Events.Start_Clock - Clock +
           Duration (Events.Time_Spent) / Duration (Events.Speed * 1000.0);

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

   begin
      if Macro /= null then
         Macro.Current_Event := Macro.Events;

         if Macro.Current_Event /= null then
            Set_Sensitive
              (Find_Menu_Item (Kernel, Macro_Menu & (-"Play")), False);
            Macro.Start_Clock := Clock;
            Macro.Time_Spent  := 0;
            Macro.Speed       := Speed;
            Id :=
              Event_Timeout.Timeout_Add (0, Play_Macro_Timer'Access, Macro);
         end if;
      end if;
   end Play_Macro;

   -------------------
   -- On_Play_Macro --
   -------------------

   procedure On_Play_Macro
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Play_Macro (Kernel, Macro => Current_Macro);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Play_Macro;

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

         Free (Current_Macro);
         Current_Macro := Load_Macro (Kernel, Name);

         if Current_Macro /= null then
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
      Events : constant Macro_Item_Access := Current_Macro.Events;
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
      Buffer  : String_Access := Read_File (File);
      Macro   : Event_Set_Access;
      Success : Boolean;
   begin
      if Buffer /= null then
         Macro := new Event_Set;
         Macro.Kernel := Kernel_Handle (Kernel);
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
      Macro       : constant Event_Set_Access := Current_Macro;
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

      return False;

   exception
      when E : others =>
         Trace
           (Exception_Handle,
            "Unexpected exception: " & Exception_Information (E));
      return False;
   end General_Event_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Macro_Menu : constant String := "/" & (-"Tools/Macro");
   begin
      if Active (Use_Macro) then
         Register_Menu
           (Kernel, Macro_Menu, -"_Start Recording",
            Callback => On_Start_Recording'Access);
         Register_Menu
           (Kernel, Macro_Menu, -"S_top Recording",
            Callback   => On_Stop_Recording'Access,
            Accel_Key  => GDK_Escape,
            Accel_Mods => Control_Mask,
            Sensitive  => False);
         Register_Menu
           (Kernel, Macro_Menu, -"_Play",
            Callback  => On_Play_Macro'Access,
            Sensitive => False);
         Register_Menu
           (Kernel, Macro_Menu, -"Load...",
            Callback  => On_Load_Macro'Access);
         Register_Menu
           (Kernel, Macro_Menu, -"_Save As...",
            Callback  => On_Save_Macro'Access,
            Sensitive => False);
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
   end Register_Commands;

end KeyManager_Module.Macros;
