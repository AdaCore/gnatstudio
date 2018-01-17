------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

--  This package handles the on-the-fly creation of an environment to
--  compile a file from the currently loaded project tree without actually
--  modifying it, by creating an Extending project.

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPS.Core_Kernels;   use GPS.Core_Kernels;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package Extending_Environments is

   type Extending_Environment is private;

   function Get_File (Env : Extending_Environment) return Virtual_File;
   --  Return the source file in Env

   function Get_Project (Env : Extending_Environment) return Virtual_File;
   --  Return the project file in Env

   function Create_Extending_Environment
     (Kernel  : access Core_Kernel_Record'Class;
      Source  : Virtual_File;
      Project : Project_Type) return Extending_Environment;
   --  Create an extending environment needed to build Source.
   --  The current Source is copied as-is from the current buffer into the
   --  extending environment.
   --  This environment should be Destroyed when no longer needed.
   --  Project is used with aggregate projects to remove ambiguities.

   procedure Destroy (Env : Extending_Environment);
   --  Remove files created for Env

private

   type Extending_Environment is record
      Project       : Project_Type := No_Project;
      File          : Virtual_File := No_File;
      Project_File  : Virtual_File := No_File;
      Temporary_Dir : Virtual_File := No_File;
   end record;

end Extending_Environments;
