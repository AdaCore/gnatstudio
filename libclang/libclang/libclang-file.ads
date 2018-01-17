------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Libclang.Index; use Libclang.Index;
with clang_c_Index_h; use clang_c_Index_h;
with GNATCOLL.VFS;

package Libclang.File is
   subtype Libclang_File is CXFile;

   function File
     (TU : Clang_Translation_Unit; File_Name : String) return Libclang_File;

   function File
     (TU : Clang_Translation_Unit;
      File : GNATCOLL.VFS.Virtual_File) return Libclang_File
   is
     (Libclang.File.File (TU, String (File.Full_Name.all)));

   function File (File : Libclang_File) return GNATCOLL.VFS.Virtual_File;

end Libclang.File;
