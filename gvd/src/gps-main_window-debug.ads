------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

--  This package provides routines related to the GPS main window and
--  the debugger module.

with Glib.Object;

package GPS.Main_Window.Debug is

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
