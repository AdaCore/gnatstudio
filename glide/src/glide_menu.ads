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

with Gtk.Item_Factory; use Gtk.Item_Factory;
pragma Elaborate_All (Gtk.Item_Factory);
with Gtk.Window;

package Glide_Menu is
   package Factory_Data is new
     Gtk.Item_Factory.Data_Item (Gtk.Window.Gtk_Window_Record'Class);
   use Factory_Data;

   --------------------
   -- GVD_Menu_Items --
   --------------------

   type Gtk_Item_Factory_Entry_Access is access Gtk_Item_Factory_Entry_Array;

   function Glide_Menu_Items return Gtk_Item_Factory_Entry_Access;
   --  Return a pointer to the Factory_Entry_Array needed to create the
   --  Glide menu items.

end Glide_Menu;
