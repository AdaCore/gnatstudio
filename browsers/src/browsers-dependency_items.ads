------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2021, AdaCore                     --
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

with Ada.Containers.Vectors;
with GPS.Kernel;        use GPS.Kernel;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package Browsers.Dependency_Items is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Dependency_Description_Type is record
      File         : Virtual_File;
      Project_Path : Virtual_File;
   end record;
   package Dependency_Description_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Dependency_Description_Type,
      "="          => "=");
   --  Types used to describe dependencies.

   type Dependency_Kind_Type is (Show_Imported, Show_Importing);
   --  The kind of dependencies.
   --
   --  . Show_Imported: show the files that the queried file depends on
   --  . Show_Importing: show the files that depend on the queried file

   procedure Show_Dependencies
     (Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Project      : Project_Type;
      Dependencies : Dependency_Description_Vectors.Vector);
   --  Show the given dependencies in the browser.

   type Dependency_Browser_Provider_Interface is interface;
   type Dependency_Browser_Provider_Access is
     access all Dependency_Browser_Provider_Interface'Class;
   --  Interface for dependency browsers' providers.

   procedure Compute_Dependencies
     (Provider      : Dependency_Browser_Provider_Interface;
      Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Project       : GNATCOLL.Projects.Project_Type;
      Kind          : Dependency_Kind_Type;
      Show_Implicit : Boolean) is abstract;
   --  Compute the dependencies for the given File.
   --  If Show_Implicit is True, implicit dependencies should also be computed.
   --  Call Show_Dependencies once the dependencies have been computed in order
   --  to display them (asyncronous mechanism).

   procedure Set_Dependency_Browser_Provider
     (Provider : not null access Dependency_Browser_Provider_Interface'Class);
   --  Set the dependency browsers' provider.

end Browsers.Dependency_Items;
