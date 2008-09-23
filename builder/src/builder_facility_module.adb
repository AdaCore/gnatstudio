-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;

with Build_Configurations;        use Build_Configurations;
with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with Traces;                    use Traces;

with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Builder_Facility_Module is

   package Buttons_Map is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Gtk_Tool_Button);

   type Builder_Module_ID_Record is
     new GPS.Kernel.Modules.Module_ID_Record
   with record
      Registry : Build_Config_Registry_Access;

      Buttons  : Buttons_Map.Map;
      --  The set of toolbar buttons
   end record;

   type Builder_Module_ID_Access is access all Builder_Module_ID_Record'Class;
   --  Data stored with the module id

   Builder_Module_ID : Builder_Module_ID_Access;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Log (M : String; Mode : Message_Mode);
   --  Logger for the registry.

   function Get_Kernel return Kernel_Handle;
   --  Utility function to get the kernel

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Launch the build manager

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   procedure Clear_Toolbar_Buttons;
   --  Remove the build-related buttons from the main toolbar

   procedure Install_Toolbar_Buttons;
   --  Install the build-related buttons into the main toolbar

   ---------
   -- Log --
   ---------

   procedure Log (M : String; Mode : Message_Mode) is
      pragma Unreferenced (Mode);
      Kernel : constant Kernel_Handle := Get_Kernel (Builder_Module_ID.all);
   begin
      Insert (Kernel, M);
   end Log;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel return Kernel_Handle is
   begin
      return Get_Kernel (Builder_Module_ID.all);
   end Get_Kernel;

   ---------------------------
   -- Clear_Toolbar_Buttons --
   ---------------------------

   procedure Clear_Toolbar_Buttons is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Get_Kernel);
      use Buttons_Map;

      C : Cursor;
   begin
      --  Browse through the registry and remove already added buttons

      C := Builder_Module_ID.Buttons.First;

      while Has_Element (C) loop
         Remove (Toolbar, Element (C));
         Next (C);
      end loop;

      --  Remove all buttons from registry

      Builder_Module_ID.Buttons.Clear;

   end Clear_Toolbar_Buttons;

   -----------------------------
   -- Install_Toolbar_Buttons --
   -----------------------------

   procedure Install_Toolbar_Buttons is
      Kernel  : constant Kernel_Handle := Get_Kernel;
      Toolbar : constant Gtk_Toolbar   := Get_Toolbar (Kernel);

      procedure Install_Button_For_Target (Target : Target_Access);
      --  Install one button

      -------------------------------
      -- Install_Button_For_Target --
      -------------------------------

      procedure Install_Button_For_Target (Target : Target_Access) is
         Button : Gtk.Tool_Button.Gtk_Tool_Button;
      begin
         Gtk_New_From_Stock (Button, Get_Icon (Target));
         Set_Label (Button, Get_Name (Target));
         Builder_Module_ID.Buttons.Insert
           (To_Unbounded_String (Get_Name (Target)), Button);
         Insert (Toolbar => Toolbar, Item    => Button);
      end Install_Button_For_Target;

      C : Target_Cursor := Get_First_Target (Builder_Module_ID.Registry);
      T : Target_Access;

      Added : Boolean := False;
   begin
      loop
         T := Get_Target (C);
         exit when T = null;

         if Get_Properties (T).Icon_In_Toolbar then
            Install_Button_For_Target (T);
            Added := True;
         end if;

         Next (C);
      end loop;

      if Added then
         Show_All (Toolbar);
      end if;
   end Install_Toolbar_Buttons;

   ----------------------
   -- On_Build_Manager --
   ----------------------

   procedure On_Build_Manager
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Configuration_Dialog
        (Builder_Module_ID.Registry,
         Get_Main_Window (Kernel),
         Get_Tooltips (Kernel));

      Clear_Toolbar_Buttons;
      Install_Toolbar_Buttons;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Build_Manager;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Builder_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level) is
      pragma Unreferenced (Module, File, Level);
   begin
      if Node.Tag.all = "target" then
         Load_Target_From_XML (Builder_Module_ID.Registry, Node);
      elsif Node.Tag.all = "target-model" then
         Create_Model_From_XML (Builder_Module_ID.Registry, Node);
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Build : constant String := '/' & (-"New builder") & '/';
   begin
      Builder_Module_ID := new Builder_Module_ID_Record;

      --  Initialise the registy
      Builder_Module_ID.Registry := Create (Log'Unrestricted_Access);

      Register_Module
        (Module      => Builder_Module_ID,
         Kernel      => Kernel,
         Module_Name => "Builder Facility");

      Register_Menu (Kernel, "/_" & (-"New builder"), Ref_Item => -"Tools");
      Register_Menu (Kernel, Build, -"Build Manager", "",
                     On_Build_Manager'Access);

      --  ??? need to add the toolbar buttons after all the XML has been loaded
   end Register_Module;

end Builder_Facility_Module;
