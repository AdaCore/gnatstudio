------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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
