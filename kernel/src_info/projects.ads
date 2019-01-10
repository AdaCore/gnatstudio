------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with Ada.Containers;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

package Projects is

   type GPS_Project_Data is new GNATCOLL.Projects.Project_Data with private;

   type Project_Registry is tagged private;
   type Project_Registry_Access is access all Project_Registry'Class;
   --  The registry is the name given to the set of currently loaded project
   --  files. Only one project hierarchy can be loaded at any given time.

   Saved_Config_File : aliased constant Filesystem_String := "gpsauto.cgpr";
   --  Name of the config file that is saved when loading a project. This file
   --  includes the user's specified config file (or the default GNAT naming
   --  scheme), and any naming scheme defined in plugins.
   --  Aliased in order to be accessible through AJIS.

   function Is_Valid_Project_Name (Name : String) return Boolean;
   --  Return True if Name is a valid project name

   function Create
     (Tree : not null access GNATCOLL.Projects.Project_Tree'Class;
      Env  : GNATCOLL.Projects.Project_Environment_Access := null)
      return Project_Registry_Access;
   --  Create a new project registry (associated with a custom tree).
   --  Env can be passed a specific value if you want to extend
   --  Project_Environment. In all cases, Initialize(Env) is called.

   function Environment
     (Self : Project_Registry)
      return GNATCOLL.Projects.Project_Environment_Access;
   --  Return the environment for the loaded projects

   function Tree
     (Self : Project_Registry)
      return GNATCOLL.Projects.Project_Tree_Access;
   --  Return the loaded project tree

   procedure Destroy (Registry : in out Project_Registry_Access);
   --  Destroy the registry

   type Project_Type_Array is array (Natural range <>) of Project_Type;
   type Project_Type_Array_Access is access all Project_Type_Array;

   procedure Free (Self : in out Project_Type_Array_Access);
   --  Free the memory used by Self

   function Project_Name_Hash
     (Project : Project_Type) return Ada.Containers.Hash_Type;
   --  Return a Hash_Type computed from the full name of the given Project.
   --  Could be used to instantiate an Ada 2005 container that uses a
   --  Project_Type as key and requires a hash function.

   function Project_Directory
     (Project : Project_Type;
      Host    : String := GNATCOLL.VFS.Local_Host)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the directory that contains the project file.
   --  No_File is returned if the project is No_Project.

   function Source_Files_Non_Recursive
     (Projects : Project_Type_Array) return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of all direct source files for all projects.
   --  Result must be freed by user.

   function Source_Dirs_With_VCS
     (Project   : Project_Type;
      Recursive : Boolean) return GNATCOLL.VFS.File_Array;
   --  Return the list of source directories under version control

   type Paths_Type_Information is (Relative, Absolute, From_Pref);

   procedure Set_Paths_Type
     (Project : Project_Type; Paths : Paths_Type_Information);
   --  Indicate how the types should be stored internally for the project

   function Get_Paths_Type
     (Project : Project_Type) return Paths_Type_Information;
   --  Indicate how the types are stored internally for the project

   procedure Cleanup_Subdirs (Tree : GNATCOLL.Projects.Project_Tree'Class);
   --  Cleanup empty subdirs created when opening a project with prj.subdirs.
   --  Also remove other temporary files

private
   type Project_Registry is tagged record
      Env  : GNATCOLL.Projects.Project_Environment_Access;
      Tree : GNATCOLL.Projects.Project_Tree_Access;
   end record;

   type GPS_Project_Data is new GNATCOLL.Projects.Project_Data with record
      Paths_Type : Paths_Type_Information := From_Pref;
      --  True if the paths in the project file should be stored as relative
      --  paths.
   end record;
   type GPS_Project_Data_Access is access all GPS_Project_Data'Class;

end Projects;
