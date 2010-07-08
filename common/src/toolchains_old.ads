-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2010, AdaCore                 --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Toolchains_Old is

   function Is_Toolchains_Active return Boolean;
   --  Tell if the multiple toolchains mode is active

   function Get_Tool_Search_Path return Virtual_File;
   --  Get the tool search path

   function Get_Compiler_Search_Path return Virtual_File;
   --  Get the compiler search path

   function Locate_Exec
     (Exec_Name : Filesystem_String; Path : File_Array)
      return Virtual_File;
   --  Locate exec on specified Path.

   function Locate_Tool_Executable
     (Exec_Name : Filesystem_String) return Virtual_File;
   --  Locate a tool executable on path

   function Locate_Compiler_Executable
     (Exec_Name : Filesystem_String) return Virtual_File;
   --  Locate a compiler executable on path

   procedure Set_Toolchains_Properties
     (Active               : Boolean;
      Tool_Search_Path     : Virtual_File;
      Compiler_Search_Path : Virtual_File);
   --  Set the toolchains selections global properties

end Toolchains_Old;
