-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2000-2008, AdaCore                   --
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

with Glib;               use Glib;
with Gtk;                use Gtk;
with Gtk.Menu_Item;      use Gtk.Menu_Item;
with Gtk.Widget;         use Gtk.Widget;
with Gtkada.Handlers;    use Gtkada.Handlers;

with Breakpoints_Editor; use Breakpoints_Editor;
with GVD_Module;         use GVD_Module;
with GVD.Scripts;        use GVD.Scripts;
with GVD.Process;        use GVD.Process;
with GVD.Types;          use GVD.Types;
with Debugger;           use Debugger;
with Process_Proxies;    use Process_Proxies;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Intl;           use GPS.Intl;

package body GPS.Main_Window.Debug is

   -------------------------------
   -- Prepare_Cleanup_Debuggers --
   -------------------------------

   procedure Prepare_Cleanup_Debuggers
     (Window : access GPS_Window_Record'Class)
   is
      Debugger : Visual_Debugger;
      List     : Debugger_List_Link := Get_Debugger_List (Window.Kernel);
   begin
      while List /= null loop
         Debugger := Visual_Debugger (List.Debugger);

         if Debugger.Debugger /= null
           and then Command_In_Process (Get_Process (Debugger.Debugger))
         then
            Interrupt (Debugger.Debugger);
         end if;

         List := List.Next;
      end loop;
   end Prepare_Cleanup_Debuggers;

   -----------------------
   -- Cleanup_Debuggers --
   -----------------------

   procedure Cleanup_Debuggers
     (Window : access GPS_Window_Record'Class)
   is
      Debugger : Visual_Debugger;
      List     : Debugger_List_Link := Get_Debugger_List (Window.Kernel);

   begin
      while List /= null loop
         Debugger := Visual_Debugger (List.Debugger);
         Debugger.Exiting := True;
         Set_Current_Debugger (Window.Kernel, Glib.Object.GObject (Debugger));

         if Debugger.Debugger /= null then
            begin
               Close (Debugger.Debugger);
            exception
               when others =>
                  --  ??? Would be nice to handle more specific errors, but
                  --  since we are exiting, ignore any exception instead of
                  --  generating unfriendly bug boxes
                  null;
            end;
         end if;

         List := List.Next;
      end loop;

      Set_Current_Debugger (Window.Kernel, null);
   end Cleanup_Debuggers;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Window : access GPS_Window_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name
        (Gtk_Widget (Window), "preferences_changed");
   end Preferences_Changed;

   ---------------------
   -- Switch_Debugger --
   ---------------------

   procedure Switch_Debugger
     (Window   : access GPS_Window_Record'Class;
      Debugger : Glib.Object.GObject)
   is
      Process     : constant Visual_Debugger := Visual_Debugger (Debugger);
      Widget      : Gtk_Menu_Item;
      Bp_Editor   : Breakpoint_Editor_Access;

      use type Glib.Object.GObject;

   begin
      if Get_Current_Debugger (Window.Kernel) = Debugger then
         return;
      end if;

      Set_Current_Debugger (Window.Kernel, Debugger);

      if Process.Debugger = null then
         return;
      end if;

      Bp_Editor := Breakpoint_Editor_Access
        (Get_Breakpoints_Editor (Window.Kernel));

      if Bp_Editor /= null then
         Set_Process (Bp_Editor, Process);
      end if;

      --  ??? Replace by a signal "debugger_switch" on the main window
      --  Do not call Executable_Changed_Hook if the debugger is busy, since
      --  this would generate an assert failure, trying to send commands to
      --  the gdb while the debugger is already handling a command.
      --  This test should also go when we use a debugger_switch signal.

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         Run_Debugger_Hook (Process, Debugger_Executable_Changed_Hook);
      end if;

      --  Update the sensitivity of the Data/Protection Domains menu
      --  item

      Widget := Find_Menu_Item
        (Window.Kernel, -"/Debug/Data/Protection Domains");

      if Widget /= null then
         Set_Sensitive
            (Widget, VxWorks_Version (Process.Debugger) = Vx653);
      end if;
   end Switch_Debugger;

end GPS.Main_Window.Debug;
