-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Glide_Kernel;          use Glide_Kernel;
with Glide_Kernel.Contexts; use Glide_Kernel.Contexts;
with Glide_Kernel.Modules;  use Glide_Kernel.Modules;
with Glide_Intl;            use Glide_Intl;
with Refactoring.Rename;    use Refactoring.Rename;
with Src_Info.Queries;      use Src_Info.Queries;
with String_Utils;          use String_Utils;
with Glib.Object;           use Glib.Object;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Traces;                use Traces;
with Ada.Exceptions;        use Ada.Exceptions;

package body Refactoring_Module is

   Me : constant Debug_Handle := Create ("Refactoring");

   procedure Refactoring_Contextual
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Handles requests for contextual menus

   ----------------------------
   -- Refactoring_Contextual --
   ----------------------------

   procedure Refactoring_Contextual
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Selection : Entity_Selection_Context_Access;
      Entity    : Entity_Information;
      Submenu   : Gtk_Menu;
      Item      : Gtk_Menu_Item;

   begin
      if Context.all not in Entity_Selection_Context'Class then
         return;
      end if;

      Selection := Entity_Selection_Context_Access (Context);

      if not Has_Entity_Name_Information (Selection) then
         return;
      end if;

      Entity := Get_Entity (Selection);

      if Entity = No_Entity_Information then
         return;
      end if;

      Gtk_New (Item, -"Refactoring");
      Append (Menu, Item);

      Gtk_New (Submenu);
      Set_Submenu (Item, Submenu);

      Gtk_New (Item, -"Rename " & Krunch (Get_Name (Entity)));
      Append (Submenu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller (On_Rename_Entity'Access),
         Selection_Context_Access (Context));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Refactoring_Contextual;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Refactoring_Module_Id : Module_ID;
   begin
      Register_Module
        (Refactoring_Module_Id,
         Kernel,
         "refactoring",
        Contextual_Menu_Handler => Refactoring_Contextual'Access);
   end Register_Module;


end Refactoring_Module;
