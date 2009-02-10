-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
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

with GNATCOLL.Filesystem;     use GNATCOLL.Filesystem;

package Toolchains is

   function Is_Toolchains_Active return Boolean;
   --  Tell if the multiple toolchains mode is active

   function Get_Tool_Search_Path return Filesystem_String;
   --  Get the tool search path

   function Get_Compiler_Search_Path return Filesystem_String;
   --  Get the compiler search path

   function Locate_Exec
     (Exec_Name : Filesystem_String; Path : Filesystem_String)
      return Filesystem_String_Access;
   --  Locate exec on specified Path.

   function Locate_Tool_Executable
     (Exec_Name : Filesystem_String) return Filesystem_String_Access;
   --  Locate a tool executable on path

   function Locate_Compiler_Executable
     (Exec_Name : Filesystem_String) return Filesystem_String_Access;
   --  Locate a compiler executable on path

   procedure Set_Toolchains_Properties
     (Active               : Boolean;
      Tool_Search_Path     : Filesystem_String;
      Compiler_Search_Path : Filesystem_String);
   --  Set the toolchains selections global properties

end Toolchains;
