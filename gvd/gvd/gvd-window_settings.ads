-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Widget; use Gtk.Widget;

package GVD.Window_Settings is

   --  This package provides functions for storing and recalling the sizes
   --  of all the windows and dialogs in GVD.

   procedure Save_Window_Settings
     (File_Name         : String;
      Main_Debug_Window : Gtk_Widget);
   --  Save the window settings in the given file.

   procedure Load_Window_Settings
     (File_Name         : String;
      Main_Debug_Window : Gtk_Widget);
   --  Load the window settings from the file.

end GVD.Window_Settings;


