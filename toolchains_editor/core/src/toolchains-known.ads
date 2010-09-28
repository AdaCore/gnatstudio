-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

package Toolchains.Known is

   function Is_Known_Toolchain_Name (Name : String) return Boolean;
   --  Tell if the name is a known toolchain description

   function Get_Known_Toolchain_Names return String_List;
   --  Get the list of known toolchains.
   --  The result must not be freed by the caller.

   function Tool_Command (Tc : String; Name : Valid_Tools) return String;

   function Is_Compiler_Defined (Tc : String; Lang : String) return Boolean;
   function Compiler_Command (Tc : String; Lang : String) return String;

end Toolchains.Known;
