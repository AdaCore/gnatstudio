-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                             ACT-Europe                            --
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

with Gtk.Window; use Gtk.Window;
with General_Preferences_Pkg; use General_Preferences_Pkg;

package GVD.Preferences_Dialog is

   type GVD_Preferences_Record is new General_Preferences_Record with record
      Main_Window : Gtk.Window.Gtk_Window;
      --  The main window to which this preferences dialog belongs
   end record;
   type GVD_Preferences_Access is access all GVD_Preferences_Record'Class;

   procedure Gtk_New
     (Preferences : out GVD_Preferences_Access;
      Main_Window : access Gtk.Window.Gtk_Window_Record'Class);

   procedure Initialize
     (Preferences : access GVD_Preferences_Record'Class;
      Main_Window : access Gtk.Window.Gtk_Window_Record'Class);

end GVD.Preferences_Dialog;
