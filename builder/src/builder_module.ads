-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  Builder module.

with Glide_Kernel;
with Gtk.Menu;
with Gtk.Menu_Item;
with String_List_Utils;

package Builder_Module is

   Builder_Module_ID   : Glide_Kernel.Module_ID;

   type Builder_Module_ID_Record is new Glide_Kernel.Module_ID_Record
   with record
      Make_Menu  : Gtk.Menu.Gtk_Menu;
      Run_Menu   : Gtk.Menu.Gtk_Menu;
      Build_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      --  The build menu, updated automatically every time the list of main
      --  units changes.

      Output     : String_List_Utils.String_List.List;
      --  The last build output.
   end record;
   --  Data stored with the module id.

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record;

   Builder_Module_Name : constant String := "Builder";

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   procedure Set_Sensitive_Menus
     (Kernel    : Glide_Kernel.Kernel_Handle;
      Sensitive : Boolean);
   --  Change the sensitive aspect of the build menu items.

end Builder_Module;
