-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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

with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Menu_Item;           use Gtk.Menu_Item;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Intl;              use Glide_Intl;

with Traces;                  use Traces;

with Gtkada.MDI;              use Gtkada.MDI;

with VCS_View_Pkg;            use VCS_View_Pkg;

package body VCS_Module is

   VCS_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("VCS_Module");

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  ???

   procedure On_Context_Changed
     (Widget  : access GObject_Record'Class;
      Params  : GValues;
      Kernel  : Kernel_Handle);
   --  ???

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Widget  : access GObject_Record'Class;
      Params  : GValues;
      Kernel  : Kernel_Handle)
   is
   begin
      null;
   end On_Context_Changed;

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      MDI      : MDI_Window := Get_MDI (Kernel);
      Explorer : VCS_View_Access;
      Child    : MDI_Child;

   begin
      Child := Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);

      if Child = null then
         Gtk_New (Explorer, Kernel);
         Set_Size_Request (Get_Child (Explorer), 400, 400);
         Child := Put (MDI, Explorer);
         Set_Title (Child, "VCS Explorer");
         Show_Files (Explorer, "");

         Kernel_Callback.Object_Connect
           (Kernel,
            "context_changed",
            On_Context_Changed'Access,
            Explorer,
            Kernel_Handle (Kernel));
      else
         Set_Focus_Child (Child);
      end if;
   end On_Open_Interface;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      VCS       : constant String := -"VCS";

   begin
      Register_Menu
        (Kernel, "/_" & VCS, Ref_Item => -"Navigate", Add_Before => False);
      Gtk_New (Menu_Item, -"VCS Explorer");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Open_Interface'Access),
         Kernel_Handle (Kernel));
   end Initialize_Module;

begin
   VCS_Module_ID := Register_Module
     (Module_Name             => VCS_Module_Name,
      Priority                => Default_Priority,
      Initializer             => Initialize_Module'Access,
      Contextual_Menu_Handler => null);
end VCS_Module;
