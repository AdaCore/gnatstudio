-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Gtk.Item_Factory; use Gtk.Item_Factory;
with Glide_Kernel;     use Glide_Kernel;

package Glide_Menu is

   type Gtk_Item_Factory_Entry_Access is access Gtk_Item_Factory_Entry_Array;

   function Glide_Menu_Items return Gtk_Item_Factory_Entry_Access;
   --  Return a pointer to the Factory_Entry_Array needed to create the
   --  Glide menu items.

   procedure Register_Common_Menus
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register some of the common menus that couldn't be created through
   --  Glide_Menu_Items.

end Glide_Menu;
