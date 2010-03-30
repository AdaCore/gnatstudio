-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2002-2010, AdaCore                --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers;
with GNAT.Strings;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;

package Projects is

   type GPS_Project_Tree is new Project_Tree with private;
   type GPS_Project_Tree_Access is access GPS_Project_Tree'Class;

   type GPS_Project_Environment is new Project_Environment with null record;

   type Project_Registry is tagged private;
   type Project_Registry_Access is access all Project_Registry'Class;
   --  The registry is the name given to the set of currently loaded project
   --  files. Only one project hierarchy can be loaded at any given time.

   function Is_Valid_Project_Name (Name : String) return Boolean;
   --  Return True if Name is a valid project name

   function Create return Project_Registry_Access;
   --  Create a new project registry

   function Environment
     (Self : Project_Registry)
      return GNATCOLL.Projects.Project_Environment_Access;
   --  Return the environment for the loaded projects

   function Tree
     (Self : Project_Registry)
      return GNATCOLL.Projects.Project_Tree_Access;
   --  Return the loaded project tree

   procedure Destroy (Registry : in out Project_Registry);
   --  Destroy the registry

   type Project_Type_Array is array (Natural range <>) of Project_Type;

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

   type Project_Status is (From_File, Default, From_Executable, Empty);
   --  How the project was created: either read from a file, automatically
   --  created from a directory, automatically created from an executable
   --  (debugger case), or default empty project. An actual project file exists
   --  on disk only in the From_File or Default cases.

   function Status (Project : Project_Type) return Project_Status;
   --  Return true if the project is a default project, ie not associated with
   --  a physical file on the disk.

   procedure Set_Status (Project : Project_Type; Status : Project_Status);
   --  Indicate whether the project is a default project.
   --  You shouldn't use this function unless you are creating a new project.

   procedure Compute_Predefined_Paths
     (Registry     : Project_Registry_Access;
      GNAT_Version : out GNAT.Strings.String_Access;
      Gnatls_Args  : GNAT.Strings.String_List_Access;
      Errors       : GNATCOLL.Projects.Error_Report := null);
   --  Compute the predefined paths for the GNAT runtime, and return the
   --  GNAT version that is used.

private
   type GPS_Project_Tree is new Project_Tree with null record;

   overriding function Data_Factory
     (Self : GPS_Project_Tree) return Project_Data_Access;
   --  See inherited documentation

   type Project_Registry is tagged record
      Env  : GNATCOLL.Projects.Project_Environment_Access;
      Tree : Projects.GPS_Project_Tree_Access;
   end record;

   overriding procedure Recompute_View
     (Self   : in out GPS_Project_Tree;
      Errors : Error_Report := null);
   --  See inherited documentation

   overriding procedure Set_Object_Subdir
     (Self   : in out GPS_Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String);
   --  See inherited documentation

end Projects;
