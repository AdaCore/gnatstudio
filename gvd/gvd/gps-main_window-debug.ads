-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides routines related to the GPS main window and
--  the debugger module.

package GPS.Main_Window.Debug is

   procedure Update_External_Dialogs
     (Window   : access GPS_Window_Record'Class;
      Debugger : Glib.Object.GObject := null);
   --  Update the contents of all the dialogs associated with the window
   --  (backtrace, threads, ...) if they are visible.
   --  Their contents is updated based on the current debugger, unless
   --  Debugger is not null.

   procedure Find_Match
     (H   : in out History_List;
      Num : in Natural;
      D   : in Direction);
   --  Moves in the history in the given direction until it finds a non-hidden
   --  command which was sent to the debugger with number Num.
   --  No_Such_Item is raised if no matching command is found.

   procedure Preferences_Changed
     (Window : access GPS_Window_Record'Class);
   --  Emit the "preferences_changed" signal, which indicates a change in
   --  the preferences. The exact change is not accessible as a parameter.

   procedure Prepare_Cleanup_Debuggers
     (Window : access GPS_Window_Record'Class);
   --  Prepare call to Cleanup_Debuggers below by stopping all the debuggers
   --  contained in the main window.

   procedure Cleanup_Debuggers (Window : access GPS_Window_Record'Class);
   --  Close all the debuggers associated with a given main debug window.

   procedure Switch_Debugger
     (Window   : access GPS_Window_Record'Class;
      Debugger : Glib.Object.GObject);
   --  Set the current debugger associated with Window to Debugger.
   --  Update any associated dialogs (e.g. Task window) accordingly.

end GPS.Main_Window.Debug;
