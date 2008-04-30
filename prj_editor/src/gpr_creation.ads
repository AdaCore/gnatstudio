-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2006                      --
--                              AdaCore                              --
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

--  This package provides a series of subprograms for creating simple GPR
--  files given input data.

with GNAT.Strings;
with Projects.Registry;
with GNATCOLL.VFS;

package GPR_Creation is

   procedure Create_Gpr_Files
     (Registry          : Projects.Registry.Project_Registry'Class;
      Root_Project      : Projects.Project_Type;
      Source_Dirs       : GNAT.Strings.String_List;
      Object_Dirs       : GNAT.Strings.String_List;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.Strings.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "");
   --  Complete the attributes of Root_Project, and possibly add dependencies
   --  to other project files as needed.
   --  Root_Project must have been created already, and other projects will
   --  be created in the same directory.
   --
   --  Attempt to create one or more .gpr project file to match the setup.
   --  If the application was not compiled, a single object directory is kept,
   --  and a single .gpr file is created. Otherwise, we try and generate a
   --  set of project files that match the current setup as much as possible.
   --
   --  Paths in Source_Dirs and Obj_dirs must be absolute paths
   --  The first directory in Object_Dirs is the one that will be kept if a
   --  single search directory is needed.

   procedure Create_Gpr_Files
     (Registry          : Projects.Registry.Project_Registry'Class;
      Root_Project      : Projects.Project_Type;
      Source_Dirs       : GNATCOLL.VFS.File_Array;
      Object_Dirs       : GNATCOLL.VFS.File_Array;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.Strings.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "");
   --  Same as above, using File_array for directory list

end GPR_Creation;
