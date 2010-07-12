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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Input_Sources.File;
with Glib.Object;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Menu_Item;
with GPS.Editors.Line_Information;
with GPS.Intl;
with GPS.Kernel.Console;
with GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;
with GPS.Kernel.Project;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Standard_Hooks;
with GNATCOLL.Projects;
with Traces;

with GNATStack.Readers;
with GNATStack.Module.Editors;
with GNATStack.Shell_Commands;

package body GNATStack.Module is

   use GPS.Editors.Line_Information;
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

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "compilation_finished" hook, to schedule other tasks

   procedure Load_Data
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Loads analysis data.

   procedure Open_Report
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Opens stack usage analysis report.

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

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is
      Name   : constant GNATCOLL.VFS.Virtual_File :=
                 GNATCOLL.VFS.Create_From_Dir
                   (GNATCOLL.Projects.Object_Dir
                      (GPS.Kernel.Project.Get_Project (Self.Kernel)),
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

      else
         GPS.Kernel.Console.Insert
           (Self.Kernel,
            "stack_usage.xml not found. Run GNATStack first.",
            True,
            GPS.Kernel.Console.Error);
      end if;
   end Load_Data;

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

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

      Hook_Data : constant
        GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args :=
          GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args (Data.all);

   begin
      if Hook_Data.Status = 0
        and Hook_Data.Target_Name = "Run GNATStack"
      then
         Load_Data (Module);
         Open_Report (Module);
         Editors.Show_Stack_Usage_In_Opened_Editors (Module);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Compilation_Finished;

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

   begin
      Load_Data (Module);
      Open_Report (Module);
      Editors.Show_Stack_Usage_In_Opened_Editors (Module);

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

   -----------------
   -- Open_Report --
   -----------------

   procedure Open_Report
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use Indirect_Call_Information_Vectors;
      use Subprogram_Information_Ordered_Sets;
      use Subprogram_Information_Vectors;
      use Subprogram_Information_Vector_Vectors;

      Buffer : constant GPS_Editor_Buffer'Class :=
                 GPS_Editor_Buffer'Class
                   (Self.Kernel.Get_Buffer_Factory.Get_New);

      procedure Add_Line (Item : String := "");
      --  Adds special line at the beginning of the buffer.

      procedure Process_Cycle
        (Position : Subprogram_Information_Vector_Vectors.Cursor);
      --  Outputs cycle's information.

      procedure Process_Unbounded_Objects
        (Position : Subprogram_Information_Sets.Cursor);
      --  Outputs list of unbounded objects in the subprogram.

      procedure Process_External_Subprogram
        (Position : Subprogram_Information_Ordered_Sets.Cursor);
      --  Outputs external subprogram's information.

      procedure Process_Indirect_Calls
        (Position : Subprogram_Information_Sets.Cursor);
      --  Output list of indirect calls in the subprogram.

      procedure Process_Entry_Point
        (Position : Subprogram_Information_Sets.Cursor);
      --  Output entry point information.

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (Item : String := "") is
      begin
         Add_Special_Line (Buffer, 1, Item);
      end Add_Line;

      -------------------
      -- Process_Cycle --
      -------------------

      procedure Process_Cycle
        (Position : Subprogram_Information_Vector_Vectors.Cursor)
      is
         procedure Process_Subprogram
           (Position : Subprogram_Information_Vectors.Cursor);

         procedure Process_Subprogram
           (Position : Subprogram_Information_Vectors.Cursor) is
         begin
            Add_Line (To_String (Element (Position).Identifier.Prefix_Name));
         end Process_Subprogram;

      begin
         Add_Special_Line (Buffer, 1, "");

         Element (Position).Iterate (Process_Subprogram'Access);
      end Process_Cycle;

      -------------------------
      -- Process_Entry_Point --
      -------------------------

      procedure Process_Entry_Point
        (Position : Subprogram_Information_Sets.Cursor) is
      begin
         Add_Line
           ("  " & To_String (Element (Position).Identifier.Prefix_Name));
      end Process_Entry_Point;

      ----------------------------
      -- Process_Indirect_Calls --
      ----------------------------

      procedure Process_Indirect_Calls
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Subprogram : constant Subprogram_Information_Access :=
                        Element (Position);

         procedure Process_Indirect_Call
           (Position : Indirect_Call_Information_Vectors.Cursor);

         ---------------------------
         -- Process_Indirect_Call --
         ---------------------------

         procedure Process_Indirect_Call
           (Position : Indirect_Call_Information_Vectors.Cursor)
         is
            Info : constant Indirect_Call_Information  :=
                     Element (Position);

         begin
            Add_Line
              ("    at "
               & To_String (Info.File)
               & ":"
               & Trim (Integer'Image (Info.Line), Both));
         end Process_Indirect_Call;

      begin
         Add_Line;
         Add_Line
           (" "
            & Integer'Image (Integer (Subprogram.Indirects.Length))
            & " indirect call in "
            & To_String (Subprogram.Identifier.Prefix_Name));
         Subprogram.Indirects.Iterate (Process_Indirect_Call'Access);
      end Process_Indirect_Calls;

      ---------------------------------
      -- Process_External_Subprogram --
      ---------------------------------

      procedure Process_External_Subprogram
        (Position : Subprogram_Information_Ordered_Sets.Cursor)
      is
      begin
         Add_Line
           ("  " & To_String (Element (Position).Identifier.Prefix_Name));
      end Process_External_Subprogram;

      -------------------------------
      -- Process_Unbounded_Objects --
      -------------------------------

      procedure Process_Unbounded_Objects
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Subprogram : constant Subprogram_Information_Access :=
                        Element (Position);

         procedure Process_Object
           (Position : Object_Information_Vectors.Cursor);
         --  Outputs name and location of the object.

         --------------------
         -- Process_Object --
         --------------------

         procedure Process_Object
           (Position : Object_Information_Vectors.Cursor)
         is
            Object : constant Object_Information := Element (Position);

         begin
            Add_Line
              ("    "
               & To_String (Object.Name)
               & " at "
               & To_String (Object.File)
               & ":"
               & Trim (Integer'Image (Object.Line), Both)
               & ":"
               & Trim (Integer'Image (Object.Column), Both));
         end Process_Object;

      begin
         Add_Line;
         Add_Line
           ("  In " & To_String (Subprogram.Identifier.Prefix_Name));
         Subprogram.Unbounded.Iterate (Process_Object'Access);
      end Process_Unbounded_Objects;

   begin
      Buffer.Set_Read_Only (True);

      if not Self.Data.Unbounded_Set.Is_Empty
        or not Self.Data.External_Set.Is_Empty
        or not Self.Data.Indirect_Set.Is_Empty
        or not Self.Data.Cycle_Set.Is_Empty
      then
         Add_Line ("Worst case analysis is *not* accurate because of");

         if not Self.Data.Cycle_Set.Is_Empty then
            Add_Line (" - cycles");
         end if;

         if not Self.Data.Unbounded_Set.Is_Empty then
            Add_Line (" - unbounded frames");
         end if;

         if not Self.Data.External_Set.Is_Empty then
            Add_Line (" - external calls");
         end if;

         if not Self.Data.Indirect_Set.Is_Empty then
            Add_Line (" - indirect calls");
         end if;
      end if;

      if not Self.Data.Cycle_Set.Is_Empty then
         Add_Line;
         Add_Line ("List of reachable cycles:");
         Self.Data.Cycle_Set.Iterate (Process_Cycle'Access);
      end if;

      if not Self.Data.Unbounded_Set.Is_Empty then
         Add_Line;
         Add_Line
           ("List of reachable subprograms with dynamic unbounded frames");
         Self.Data.Unbounded_Set.Iterate (Process_Unbounded_Objects'Access);
      end if;

      if not Self.Data.External_Set.Is_Empty then
         Add_Line;
         Add_Line ("List of reachable external subprograms:");
         Self.Data.External_Set.Iterate (Process_External_Subprogram'Access);
      end if;

      if not Self.Data.Indirect_Set.Is_Empty then
         Add_Line;
         Add_Line
           ("List of reachable and unresolved indirect"
            & " (including dispatching) calls:");
         Self.Data.Indirect_Set.Iterate (Process_Indirect_Calls'Access);
      end if;

      if not Self.Data.Entry_Set.Is_Empty then
         Add_Line;
         Add_Line ("List of entry points:");
         Self.Data.Entry_Set.Iterate (Process_Entry_Point'Access);
      end if;
   end Open_Report;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use GNATCOLL.VFS;

      Menu           : constant String := -"/Tools/GNATStac_k";
      GNATStack_Path : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Locate_On_Path ("gnatstack");
      Factory        : GPS.Kernel.Modules.UI.Submenu_Factory;
      Mitem          : Gtk.Menu_Item.Gtk_Menu_Item;

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
         Parent_Path => Menu,
         Text        => -"_Analyze stack usage",
         Ref_Item    => -"Macro",
         Callback    => On_Analyze_Stack_Usage'Access);

      Gtk.Menu_Item.Gtk_New (Mitem);
      Register_Menu (Kernel, Menu, Mitem);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Load last stack usage",
         Callback    => On_Load_Data'Access);

      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Compilation_Finished_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Compilation_Finished'Access),
         Name => "gnatstack.compilation_finished");

      Editors.Register_Module (Module);
   end Register_Module;

end GNATStack.Module;
