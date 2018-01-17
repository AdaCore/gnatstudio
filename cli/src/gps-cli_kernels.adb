------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Utils;

package body GPS.CLI_Kernels is

   ---------------------
   -- Create_Database --
   ---------------------

   overriding procedure Create_Database
     (Self   : not null access CLI_Kernel_Record;
      Result : out Xref.General_Xref_Database) is
   begin
      Result := new Xref.General_Xref_Database_Record;

      Xref.Initialize
        (Result, Self.Lang_Handler, Self.Symbols, Self.Registry);
   end Create_Database;

   ---------------------
   -- Create_Registry --
   ---------------------

   overriding procedure Create_Registry
     (Self   : not null access CLI_Kernel_Record;
      Result : out Projects.Project_Registry_Access)
   is
      pragma Unreferenced (Self);
      Tree : constant GNATCOLL.Projects.Project_Tree_Access :=
        new GNATCOLL.Projects.Project_Tree;
   begin
      Result := Projects.Create (Tree);
      Tree.Load_Empty_Project;
   end Create_Registry;

   -------------------
   -- Get_Share_Dir --
   -------------------

   overriding function Get_Share_Dir
     (Self : not null access CLI_Kernel_Record)
      return GNATCOLL.VFS.Virtual_File
   is
      pragma Unreferenced (Self);

      Dir : GNATCOLL.VFS.Virtual_File :=
        Create (+GNATCOLL.Utils.Executable_Location);
   begin
      if Dir.Base_Dir_Name = "obj" then
         Dir := Create_From_Dir (Dir.Get_Parent.Get_Parent, "share/");
      else
         Dir := Create_From_Dir (Dir, "share/gps/");
      end if;

      return Dir;
   end Get_Share_Dir;

   ------------------------
   -- Get_Buffer_Factory --
   ------------------------

   overriding function Get_Buffer_Factory
     (Kernel : not null access CLI_Kernel_Record)
      return GPS.Editors.Editor_Buffer_Factory_Access
   is
   begin
      raise Program_Error with "Get_Buffer_Factory not implemented in GPS cli";
      return null;
   end Get_Buffer_Factory;

   ---------------------
   -- Messages_Window --
   ---------------------

   overriding function Messages_Window
     (Self : not null access CLI_Kernel_Record)
      return GPS.Messages_Windows.Abstract_Messages_Window_Access is
   begin
      return Self.Messages_Window'Access;
   end Messages_Window;

   ----------------------
   -- Process_Launcher --
   ----------------------

   overriding function Process_Launcher
     (Self : not null access CLI_Kernel_Record)
      return GPS.Process_Launchers.Process_Launcher is
   begin
      return Self.Launcher'Access;
   end Process_Launcher;

   ----------------
   -- Get_Target --
   ----------------

   overriding function Get_Target
     (Kernel : not null access CLI_Kernel_Record) return String is
   begin
      return Kernel.Registry.Tree.Root_Project.Get_Target;
   end Get_Target;

   -----------------
   -- Get_Runtime --
   -----------------

   overriding function Get_Runtime
     (Kernel : not null access CLI_Kernel_Record) return String is
   begin
      return Kernel.Registry.Tree.Root_Project.Get_Runtime;
   end Get_Runtime;

end GPS.CLI_Kernels;
