-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
--                            AdaCore                                --
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

with GNAT.OS_Lib;

package GPR_Creation is

   function Create_Gpr_Files
     (Root_Project_Name : String;
      Output_Dir        : String;
      Source_Dirs       : GNAT.OS_Lib.String_List;
      Object_Dirs       : GNAT.OS_Lib.String_List;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "") return String;
   --  Attempt to create one or more .gpr project file to match the setup.
   --  If the application was not compiled, a single object directory is kept,
   --  and a single .gpr file is created. Otherwise, we try and generate a
   --  set of project files that match the current setup as much as possible.
   --
   --  Root_Project_Name is the name of the root project to create.
   --  Paths in Source_Dirs and Obj_dirs must be absolute paths
   --  The first directory in Object_Dirs is the one that will be kept if a
   --  single search directory is needed.
   --
   --  The full path name for the root project is given. All projects are
   --  created in the same directory (Output_Dir).

end GPR_Creation;
