------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Libclang.File is

   function File
     (TU : Clang_Translation_Unit; File_Name : String) return Libclang_File
   is
      C_File_Name : chars_ptr := New_String (File_Name);
      Result : constant CXFile := clang_getFile (TU, C_File_Name);
   begin
      Free (C_File_Name);
      return Result;
   end File;

   function File (File : Libclang_File) return GNATCOLL.VFS.Virtual_File
   is
   begin
      return GNATCOLL.VFS.Create
        (GNATCOLL.VFS.Filesystem_String
           (To_String (clang_getFileName (File))));
   end File;

end Libclang.File;
