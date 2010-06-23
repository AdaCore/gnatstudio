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
with GPS.Intl;
with GPS.Kernel.Console;
with GPS.Kernel.Project;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Traces;

with GNATStack.Readers;
with GNATStack.Shell_Commands;

package body GNATStack.Module is

   use GPS.Intl;
   use Traces;

   type GNATStack_Module_Id is access all GNATStack_Module_Id_Record'Class;

   procedure On_Analyze_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze stack usage" menu item is activated

   procedure On_Load_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Load data" menu item is activated

   Module : GNATStack_Module_Id;

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
         Force       => True,
         Synchronous => False);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Analyze_Stack_Usage;

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
         Module.Loaded := True;

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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use GNATCOLL.VFS;

      GNATStack_Path : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Locate_On_Path ("gnatstack");

   begin
      if GNATStack_Path = No_File then
         --  There is no GNATStack executable available, module is not
         --  registered.

         return;
      end if;

      Module := new GNATStack_Module_Id_Record (Kernel);

      Module.Register_Module (Kernel, "GNATStack");
      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => -"/Tools/GNATStack",
         Text        => -"_Analyze stack usage",
         Ref_Item    => -"Macro",
         Callback    => On_Analyze_Stack_Usage'Access);
      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => -"/Tools/GNATStack",
         Text        => -"_Load data",
         Callback    => On_Load_Data'Access);
   end Register_Module;

end GNATStack.Module;
