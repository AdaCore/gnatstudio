-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007, AdaCore              --
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

with Glib.Object;                       use Glib.Object;
with Gtk.Menu_Item;                     use Gtk.Menu_Item;
with Gtk.Dialog;                        use Gtk.Dialog;

with Ada.Exceptions;                    use Ada.Exceptions;

with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Intl;                          use GPS.Intl;

with Make_Harness_Window_Pkg;           use Make_Harness_Window_Pkg;
with Make_Harness_Window_Pkg.Callbacks; use Make_Harness_Window_Pkg.Callbacks;
with Make_Suite_Window_Pkg;             use Make_Suite_Window_Pkg;
with Make_Suite_Window_Pkg.Callbacks;   use Make_Suite_Window_Pkg.Callbacks;
with Make_Test_Window_Pkg;              use Make_Test_Window_Pkg;
with Make_Test_Window_Pkg.Callbacks;    use Make_Test_Window_Pkg.Callbacks;

with Traces;                            use Traces;

package body Aunit_Module is

   type Aunit_Module_Record is new Module_ID_Record with null record;
   Aunit_Module_ID   : Module_ID;
   Aunit_Module_Name : constant String := "Unit_Testing";

   procedure On_New_Test_Case
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Edit->Unit Testing->New Test Case menu

   procedure On_New_Test_Suite
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Edit->Unit Testing->New Test Suite menu

   procedure On_New_Test_Harness
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Edit->Unit Testing->New Test Harness menu

   ----------------------
   -- On_New_Test_Case --
   ----------------------

   procedure On_New_Test_Case
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Make_Test_Window : Make_Test_Window_Access;
      Response         : Gtk_Response_Type;
   begin
      Gtk_New (Make_Test_Window, Kernel);
      Show_All (Make_Test_Window);
      Response := Run (Make_Test_Window);

      if Response = Gtk_Response_OK then
         On_Ok_Clicked (Make_Test_Window);
      end if;

      Destroy (Make_Test_Window);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception in On_New_Test_Case: "
                & Exception_Information (E));
   end On_New_Test_Case;

   -----------------------
   -- On_New_Test_Suite --
   -----------------------

   procedure On_New_Test_Suite
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Make_Suite_Window : Make_Suite_Window_Access;
      Response          : Gtk_Response_Type;
   begin
      Gtk_New (Make_Suite_Window, Kernel);
      Show_All (Make_Suite_Window);
      Response := Run (Make_Suite_Window);

      if Response = Gtk_Response_OK then
         On_Ok_Clicked (Make_Suite_Window);
      end if;

      Destroy (Make_Suite_Window);

   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception in On_New_Test_Suite: "
                & Exception_Information (E));
   end On_New_Test_Suite;

   -------------------------
   -- On_New_Test_Harness --
   -------------------------

   procedure On_New_Test_Harness
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Make_Harness_Window : Make_Harness_Window_Access;
      Response            : Gtk_Response_Type;
   begin
      Gtk_New (Make_Harness_Window, Kernel);
      Show_All (Make_Harness_Window);
      Response := Run (Make_Harness_Window);

      if Response = Gtk_Response_OK then
         On_Ok_Clicked (Make_Harness_Window);
      end if;

      Destroy (Make_Harness_Window);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception in On_New_Test_Harness: "
                & Exception_Information (E));
   end On_New_Test_Harness;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item    : Gtk_Menu_Item;
      Edit         : constant String := '/' & (-"Edit") & '/';
      Unit_Testing : constant String := -"Unit Testing";
   begin
      Aunit_Module_ID := new Aunit_Module_Record;
      Register_Module
        (Module                  => Aunit_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Aunit_Module_Name,
         Priority                => Default_Priority);

      Register_Menu (Kernel, Edit & '_' & Unit_Testing,
                     Ref_Item => -"Aliases");
      Register_Menu (Kernel, Edit & Unit_Testing,
                     -"New Test _Case...", "", On_New_Test_Case'Access);

      --  ??? Add once this is implemented
      --  Register_Menu (Kernel, Edit & Unit_Testing,
      --                 -"_Add Routine...", "", null, Sensitive => False);

      Register_Menu (Kernel, Edit & Unit_Testing,
                     -"New Test _Suite...", "", On_New_Test_Suite'Access);
      Register_Menu (Kernel, Edit & Unit_Testing,
                     -"New Test _Harness...", "", On_New_Test_Harness'Access);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Edit, Menu_Item, Ref_Item => -"Aliases");
   end Register_Module;

end Aunit_Module;
