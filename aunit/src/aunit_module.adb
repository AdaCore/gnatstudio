-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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

with Glib.Object;             use Glib.Object;
with Gtk.Main;                use Gtk.Main;
with Gtk.Menu_Item;           use Gtk.Menu_Item;

with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Ada.Exceptions;          use Ada.Exceptions;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Kernel.Editor;     use Glide_Kernel.Editor;
with Glide_Intl;              use Glide_Intl;

with Make_Harness_Window_Pkg; use Make_Harness_Window_Pkg;
with Make_Suite_Window_Pkg;   use Make_Suite_Window_Pkg;
with Make_Test_Window_Pkg;    use Make_Test_Window_Pkg;

with Traces;                  use Traces;

package body Aunit_Module is

   Aunit_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Aunit_Module");

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

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
      pragma Warnings (Off, Widget);
      Make_Test_Window : Make_Test_Window_Access;
   begin
      Gtk_New (Make_Test_Window);
      Show_All (Make_Test_Window);
      Gtk.Main.Main;

      if Make_Test_Window.Name /= null then
         declare
            File : constant String := Make_Test_Window.Name.all;
         begin
            --  ??? Should use correct body and spec names
            Open_Or_Create (Kernel, File & ".ads");
            Open_Or_Create (Kernel, File & ".adb");
         end;
      end if;

      Free (Make_Test_Window.Name);
      Destroy (Make_Test_Window);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_New_Test_Case: "
                & Exception_Information (E));
   end On_New_Test_Case;

   -----------------------
   -- On_New_Test_Suite --
   -----------------------

   procedure On_New_Test_Suite
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Warnings (Off, Widget);
      Make_Suite_Window : Make_Suite_Window_Access;
   begin
      Gtk_New (Make_Suite_Window);
      Show_All (Make_Suite_Window);
      Gtk.Main.Main;

      if Make_Suite_Window.Name /= null then
         declare
            File : constant String := Make_Suite_Window.Name.all;
         begin
            Open_Or_Create (Kernel, File & ".adb");
         end;
      end if;

      Free (Make_Suite_Window.Name);
      Destroy (Make_Suite_Window);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_New_Test_Case: "
                & Exception_Information (E));
   end On_New_Test_Suite;

   -------------------------
   -- On_New_Test_Harness --
   -------------------------

   procedure On_New_Test_Harness
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Warnings (Off, Widget);
      Make_Harness_Window : Make_Harness_Window_Access;
   begin
      Gtk_New (Make_Harness_Window);
      Show_All (Make_Harness_Window);
      Gtk.Main.Main;

      if Make_Harness_Window.Procedure_Name /= null then
         declare
            File : constant String := Make_Harness_Window.Procedure_Name.all;
         begin
            Open_Or_Create (Kernel, File & ".adb");
         end;
      end if;

      Free (Make_Harness_Window.Procedure_Name);
      Destroy (Make_Harness_Window);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_New_Test_Case: "
                & Exception_Information (E));
   end On_New_Test_Harness;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item    : Gtk_Menu_Item;
      Edit         : constant String := '/' & (-"Edit") & '/';
      Unit_Testing : constant String := -"Unit Testing";

   begin
      Register_Menu (Kernel, Edit & Unit_Testing,
                     Ref_Item => -"Select All", Add_Before => False);

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, Edit, Menu_Item,
                     Ref_Item => Unit_Testing, Add_Before => True);

      Gtk_New (Menu_Item, -"New Test Case...");
      Register_Menu (Kernel, Edit & Unit_Testing, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_New_Test_Case'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Add Routine...");
      Register_Menu (Kernel, Edit & Unit_Testing, Menu_Item);

      Gtk_New (Menu_Item, -"New Test Suite...");
      Register_Menu (Kernel, Edit & Unit_Testing, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_New_Test_Suite'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"New Test Harness...");
      Register_Menu (Kernel, Edit & Unit_Testing, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_New_Test_Harness'Access),
         Kernel_Handle (Kernel));
   end Initialize_Module;

begin
   Aunit_Module_ID := Register_Module
     (Module_Name             => Aunit_Module_Name,
      Priority                => Default_Priority,
      Initializer             => Initialize_Module'Access,
      Contextual_Menu_Handler => null);
end Aunit_Module;
