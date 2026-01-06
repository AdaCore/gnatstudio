------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

--  This package should eventually be merged with GNAT.OS_Lib

with Basic_Types;  use Basic_Types;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package File_Utils is

   function URL_List_To_Files (URL_List : String) return File_Array_Access;
   --  Convert list of URLs in form "file:///path/file" to file array.
   --  Files in list are separated by CR/LF characters.

   function UTF8_Full_Name (File : Virtual_File) return UTF8_String;
   --  Try to convert file name to utf-8 string

private

   type Path_Iterator is record
      First, Last : Natural;
   end record;

end File_Utils;
