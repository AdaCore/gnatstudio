------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

with GNATdoc.Project_Environments;

package body GNATdoc.Kernels is

   ---------------------
   -- Create_Registry --
   ---------------------

   overriding procedure Create_Registry
     (Self   : not null access GNATdoc_Kernel_Record;
      Result : out Projects.Project_Registry_Access)
   is
      pragma Unreferenced (Self);

      Tree : constant GNATCOLL.Projects.Project_Tree_Access :=
        new GNATCOLL.Projects.Project_Tree;
      Env  : constant GNATCOLL.Projects.Project_Environment_Access :=
        new GNATdoc.Project_Environments.GNATdoc_Project_Environment;

   begin
      Result := Projects.Create (Tree, Env);
      Tree.Load_Empty_Project;
   end Create_Registry;

end GNATdoc.Kernels;
