-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
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

--  This package contains a series of subprograms that can be used
--  for misc. graphical tasks.

with Gdk.Window;
with Gtk.Combo;
with Gtk.List;

package Odd.Utils is

   procedure Add_Unique_List_Entry
     (List : access Gtk.List.Gtk_List_Record'Class;
      Text  : String);
   --  Add Text to List if it is not already there. Nothing is done if Text
   --  is already visible in the list

   procedure Add_Unique_Combo_Entry
     (Combo : access Gtk.Combo.Gtk_Combo_Record'Class;
      Text  : String);
   --  Add Text to the popdown list of Combo, if it is not already there.
   --  If the Text is already in the combo box, nothing is done.

   procedure Set_Busy_Cursor
     (Window   : Gdk.Window.Gdk_Window;
      Busy     : Boolean := True);
   --  Enable or disable the "busy" cursor for a specific top-level window.

end Odd.Utils;
