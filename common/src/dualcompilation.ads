-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with System.OS_Lib; use System.OS_Lib;

package Dualcompilation is

   function Is_Dualcompilation_Active return Boolean;
   --  Tell if the dual compilation mode is active

   function Get_Tool_Search_Path return String;
   --  Get the tool search path

   function Get_Compiler_Search_Path return String;
   --  Get the compiler search path

   function Locate_Exec (Exec_Name : String; Path : String)
                         return String_Access;
   --  Locate exec on specified Path.

   function Locate_Tool_Executable (Exec_Name : String) return String_Access;
   --  Locate a tool executable on path

   function Locate_Compiler_Executable
     (Exec_Name : String) return String_Access;
   --  Locate a compiler executable on path

   procedure Set_Dualcompilation_Properties
     (Active               : Boolean;
      Tool_Search_Path     : String;
      Compiler_Search_Path : String);
   --  Set the dual compilation global properties

end Dualcompilation;
