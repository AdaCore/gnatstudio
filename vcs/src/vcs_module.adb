-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib.Object;               use Glib.Object;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;

with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Intl;                use Glide_Intl;

with Traces;                    use Traces;

with VCS_View_API;              use VCS_View_API;
with VCS_View_Pkg;              use VCS_View_Pkg;

with Ada.Exceptions;            use Ada.Exceptions;

with Log_Utils;

package body VCS_Module is

   VCS_Module_Name : constant String := "VCS_Interface";
   Me : constant Debug_Handle := Create (VCS_Module_Name);

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Display the VCS explorer

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Explorer (Kernel, null);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Interface;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Submenu      : Gtk_Menu;
      Menu_Item    : Gtk_Menu_Item;
   begin
      if Context.all in File_Selection_Context'Class then
         Gtk_New (Menu_Item, Label => -"VCS");
         Gtk_New (Submenu);
         VCS_View_API.VCS_Contextual_Menu (Object, Context, Submenu);
         Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
         Append (Menu, Menu_Item);
      end if;
   end VCS_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      VCS_Root  : constant String := -"VCS";
      VCS       : constant String := '/' & VCS_Root;

   begin
      Register_Module
        (Module                  => VCS_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => VCS_Module_Name,
         Priority                => Default_Priority,
         MDI_Child_Tag           => VCS_View_Record'Tag,
         Contextual_Menu_Handler => VCS_Contextual_Menu'Access,
         Default_Context_Factory => VCS_View_API.Context_Factory'Access);

      Register_Menu
        (Kernel,
         "/_" & VCS_Root,
         Ref_Item => -"Navigate",
         Add_Before => False);

      Register_Menu (Kernel, VCS, -"Explorer", "", On_Open_Interface'Access);
      Register_Menu (Kernel, VCS, -"Update project", "", Update_All'Access);
      Register_Menu
        (Kernel, VCS, -"Query status for project", "",
         Query_Status_For_Project'Access);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, VCS, Menu_Item);
      Register_Menu (Kernel, VCS, -"Update", "", Update'Access);
      Register_Menu (Kernel, VCS, -"Start Editing", "", Open'Access);
      Register_Menu (Kernel, VCS, -"View Diff", "", View_Diff'Access);
      Register_Menu (Kernel, VCS, -"Edit log", "", Edit_Log'Access);
      Register_Menu (Kernel, VCS, -"Commit", "", Commit'Access);
      Register_Menu (Kernel, VCS, -"Annotate", "", View_Annotate'Access);
      Register_Menu (Kernel, VCS, -"View Changelog", "", View_Log'Access);
      Register_Menu (Kernel, VCS, -"Revert", "", Revert'Access);
      Register_Menu (Kernel, VCS, -"Add to repository", "", Add'Access);
      Register_Menu
        (Kernel, VCS, -"Remove from repository", "", Remove'Access);

      Log_Utils.Initialize (Kernel);
   end Register_Module;

end VCS_Module;
