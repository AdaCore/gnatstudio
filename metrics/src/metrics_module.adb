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

with Gtk.Menu_Item;           use Gtk.Menu_Item;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Intl;              use Glide_Intl;

with Traces;                  use Traces;

package body Metrics_Module is

   Metrics_Module_ID   : Module_ID;
   Metrics_Module_Name : constant String := "Metrics";

   Me : Debug_Handle := Create (Metrics_Module_Name);

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      Metrics   : constant String := '/' & (-"Tools") & '/' & (-"Metrics");
      Complex   : constant String := '/' & (-"Code Complexity");
      Packages  : constant String := '/' & (-"Packages");
      Lines     : constant String := '/' & (-"Lines");

   begin
      Gtk_New (Menu_Item, -"Cyclomatic Complexity");
      Register_Menu (Kernel, Metrics & Complex, Menu_Item);
      --  Kernel_Callback.Connect
      --    (Menu_Item, "activate",
      --     Kernel_Callback.To_Marshaller (On_Open_Interface'Access),
      --     Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Maximum nesting level");
      Register_Menu (Kernel, Metrics & Complex, Menu_Item);
      Gtk_New (Menu_Item, -"Number of statements");
      Register_Menu (Kernel, Metrics & Complex, Menu_Item);

      Gtk_New (Menu_Item, -"Number of types");
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);
      Gtk_New (Menu_Item, -"Number of routines");
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);
      Gtk_New (Menu_Item, -"Number of specification lines");
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);
      Gtk_New (Menu_Item, -"Number of body lines");
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);

      Gtk_New (Menu_Item, -"Number of lines");
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of blank lines");
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of code lines");
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of comment lines");
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of mixed code and comment lines");
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
   end Initialize_Module;

begin
   Metrics_Module_ID := Register_Module
     (Module_Name             => Metrics_Module_Name,
      Priority                => Default_Priority,
      Initializer             => Initialize_Module'Access,
      Contextual_Menu_Handler => null);
end Metrics_Module;
