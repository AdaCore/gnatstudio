-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with Glide_Intl;              use Glide_Intl;

package body Metrics_Module is

   Metrics_Module_ID   : Module_ID;
   Metrics_Module_Name : constant String := "Metrics";

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      Metrics   : constant String := '/' & (-"Tools") & '/' & (-"Metrics");
      Complex   : constant String := '/' & (-"Code Complexity");
      Packages  : constant String := '/' & (-"Packages");
      Lines     : constant String := '/' & (-"Lines");
   begin
      Register_Module
        (Module                  => Metrics_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Metrics_Module_Name,
         Priority                => Default_Priority);

      Gtk_New (Menu_Item, -"Cyclomatic Complexity");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Complex, Menu_Item);

      Gtk_New (Menu_Item, -"Maximum nesting level");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Complex, Menu_Item);
      Gtk_New (Menu_Item, -"Number of statements");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Complex, Menu_Item);

      Gtk_New (Menu_Item, -"Number of types");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);
      Gtk_New (Menu_Item, -"Number of routines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);
      Gtk_New (Menu_Item, -"Number of specification lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);
      Gtk_New (Menu_Item, -"Number of body lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Packages, Menu_Item);

      Gtk_New (Menu_Item, -"Number of lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of blank lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of code lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of comment lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
      Gtk_New (Menu_Item, -"Number of mixed code and comment lines");
      Set_Sensitive (Menu_Item, False);
      Register_Menu (Kernel, Metrics & Lines, Menu_Item);
   end Register_Module;

end Metrics_Module;
