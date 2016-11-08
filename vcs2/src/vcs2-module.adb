------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.VCS_Engines;         use GPS.VCS_Engines;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with VCS2.Scripts;            use VCS2.Scripts;
with VCS2.Commits;

package body VCS2.Module is

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed.
   --  This looks for what VCS engine to use for each project. It tries to
   --  reuse existing engines when possible, to benefit from their caches.

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Compute_VCS_Engines (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Info    : constant File_Info'Class := File_Info'Class
         (Get_Registry (Kernel).Tree.Info_Set (File).First_Element);
      Project : constant Project_Type := Info.Project (True);
   begin
      Get_VCS (Kernel, Project).Invalidate_File_Status_Cache (File);
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      VCS2.Scripts.Register_Scripts (Kernel);

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
      File_Saved_Hook.Add (new On_File_Saved);

      VCS2.Commits.Register_Module (Kernel);
   end Register_Module;

end VCS2.Module;
