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

with GNATCOLL.VFS;

with XML_Utils;

package Toolchains.Known is

   Invalid_File : exception;

   procedure Read_From_XML_File (File : GNATCOLL.VFS.Virtual_File);
   procedure Read_From_XML (Node : XML_Utils.Node_Ptr);
   --  Parse xml elements to construct the known toolchains database

   function Is_Known_Toolchain_Name (Name : String) return Boolean;
   --  Tell if the name is a known toolchain description

   function Get_Known_Toolchain_Names return String_List_Access;
   --  Get the list of known toolchains.
   --  The result must be freed by the caller.

   function Has_Naming_Exception (Name : String) return Boolean;
   --  Tell if the Name toolchain has tools with naming exceptions (e.g.
   --  not of the form Name-tool)

   function Tool_Command (Tc : String; Name : Valid_Tools) return String;

   function Is_Compiler_Defined (Tc : String; Lang : String) return Boolean;
   function Compiler_Command (Tc : String; Lang : String) return String;
   function Langs (Tc : String) return String_List_Access;
   --  Return the list of languages defined for this toolchain

end Toolchains.Known;
