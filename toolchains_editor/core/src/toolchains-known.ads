------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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
   --  not of the form Name-tool).

   function Tool_Command (Tc : String; Name : Valid_Tools) return String;

   function Is_Compiler_Defined (Tc : String; Lang : String) return Boolean;
   function Compiler_Command (Tc : String; Lang : String) return String;
   function Langs (Tc : String) return String_List_Access;
   --  Return the list of languages defined for this toolchain

end Toolchains.Known;
