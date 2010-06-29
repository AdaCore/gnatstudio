-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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
with Input_Sources.File;
with Glib.Object;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Menu_Item;
with GPS.Intl;
with GPS.Kernel.Console;
with GPS.Kernel.Contexts;
with GPS.Kernel.Project;
with GPS.Kernel.Modules.UI;
with GNATCOLL.Projects;
with Traces;

with GNATStack.Readers;
with GNATStack.Module.Editors;
with GNATStack.Shell_Commands;

package body GNATStack.Module is

   use GPS.Intl;
   use GPS.Kernel.Modules.UI;
   use GNATStack.Readers;
   use GNATStack.Data_Model;
   use GNATStack.Data_Model.Object_Information_Vectors;
   use GNATStack.Data_Model.Subprogram_Information_Sets;
   use GNATStack.Data_Model.Subprogram_Location_Sets;
   use Traces;

   type GNATStack_Submenu_Factory_Record
     (Module : access GNATStack_Module_Id_Record'Class) is
     new GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;

   package Object_Callbacks is
     new Gtk.Handlers.Callback (Glib.Object.GObject_Record);

   overriding procedure Append_To_Menu
     (Self    : access GNATStack_Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure On_Analyze_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze stack usage" menu item is activated

   procedure On_Load_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Load data" menu item is activated

   procedure On_Show_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class);
   --  Shows stack usage information in the editor for selected file.

   procedure On_Hide_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class);
   --  Hides stack usage information in the editor for selected file.

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Self    : access GNATStack_Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

   begin
      if not Self.Module.Loaded then
         return;
      end if;

      if GPS.Kernel.Contexts.Has_File_Information (Context) then
         declare
            Item : Gtk.Menu_Item.Gtk_Menu_Item;

         begin
            Module.File := GPS.Kernel.Contexts.File_Information (Context);
            Gtk.Menu_Item.Gtk_New (Item, -"Show stack usage");
            Menu.Append (Item);
            Object_Callbacks.Connect
              (Item,
               Gtk.Menu_Item.Signal_Activate,
               Object_Callbacks.To_Marshaller (On_Show_Stack_Usage'Access));
            Gtk.Menu_Item.Gtk_New (Item, -"Hide stack usage");
            Menu.Append (Item);
            Object_Callbacks.Connect
              (Item,
               Gtk.Menu_Item.Signal_Activate,
               Object_Callbacks.To_Marshaller (On_Hide_Stack_Usage'Access));
         end;
      end if;
   end Append_To_Menu;

   ----------------------------
   -- On_Analyze_Stack_Usage --
   ----------------------------

   procedure On_Analyze_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      GNATStack.Shell_Commands.Build_Target_Execute
        (Kernel,
         GNATStack.Shell_Commands.Build_Target (Kernel, "Run GNATStack"),
         Synchronous => False);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Analyze_Stack_Usage;

   -------------------------
   -- On_Hide_Stack_Usage --
   -------------------------

   procedure On_Hide_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Widget);

   begin
      Editors.Hide_Stack_Usage (Module, Module.File);
   end On_Hide_Stack_Usage;

   ------------------
   -- On_Load_Data --
   ------------------

   procedure On_Load_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Name   : constant GNATCOLL.VFS.Virtual_File :=
                 GNATCOLL.VFS.Create_From_Dir
                   (GNATCOLL.Projects.Object_Dir
                      (GPS.Kernel.Project.Get_Project (Kernel)),
                    "stack_usage.xml");
      File   : Input_Sources.File.File_Input;
      Reader : GNATStack.Readers.Reader;

   begin
      if Name.Is_Regular_File then
         Input_Sources.File.Open (String (Name.Full_Name.all), File);
         Reader.Parse (File);
         Input_Sources.File.Close (File);
         Module.Data := Reader.Get_Data;
         Module.Loaded := True;

         Editors.Show_Stack_Usage_In_Opened_Editors (Module);

      else
         GPS.Kernel.Console.Insert
           (Kernel,
            "stack_usage.xml not found. Run GNATStack first.",
            True,
            GPS.Kernel.Console.Error);
      end if;

   exception
      when E : others =>
         GPS.Kernel.Console.Insert
           (Kernel,
            "Unable to load stack usage information.",
            True,
            GPS.Kernel.Console.Error);
         Trace (Exception_Handle, E);
   end On_Load_Data;

   -------------------------
   -- On_Show_Stack_Usage --
   -------------------------

   procedure On_Show_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Widget);

   begin
      Editors.Show_Stack_Usage (Module, Module.File);
   end On_Show_Stack_Usage;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use GNATCOLL.VFS;

      GNATStack_Path : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Locate_On_Path ("gnatstack");
      Factory        : GPS.Kernel.Modules.UI.Submenu_Factory;

   begin
      if GNATStack_Path = No_File then
         --  There is no GNATStack executable available, module is not
         --  registered.

         return;
      end if;

      Module := new GNATStack_Module_Id_Record (Kernel);

      Factory := new GNATStack_Submenu_Factory_Record (Module);

      Module.Register_Module (Kernel, "GNATStack");
      Register_Contextual_Submenu
        (Kernel  => Kernel,
         Name    => "GNATStack",
         Label   => -"GNATStack",
         Submenu => Factory);
      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => -"/Tools/GNATStac_k",
         Text        => -"_Analyze stack usage",
         Ref_Item    => -"Macro",
         Callback    => On_Analyze_Stack_Usage'Access);
      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => -"/Tools/GNATStac_k",
         Text        => -"_Load data",
         Callback    => On_Load_Data'Access);

      Editors.Register_Module (Module);
   end Register_Module;

end GNATStack.Module;
