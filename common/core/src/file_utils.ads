------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

   function Suffix_Matches
     (File_Name : Filesystem_String;
      Suffix    : Filesystem_String) return Boolean;
   --  Return true if File_Name has the given Suffix. This is more general
   --  than extensions, since it doesn't need to start after a '.'.
   --  Note that this function also return False when Filename = Extension
   --  as this does not make sense for a source filename.

--     function To_Unix_Pathname
--       (Path : Filesystem_String) return Filesystem_String;
--     --  Convert all occurences of Directory_Separator to '/'

--     function To_Host_Pathname
--       (Path : Filesystem_String) return Filesystem_String;
--     --  By default, return Path.
--     --  If Directory_Separator is different than '/', the following
--     --  substitution is operated:
--     --  /cydrive/x/ -> x:\
--     --  where x is an single character.

   function To_File_Name (Name : Filesystem_String) return Filesystem_String;
   --  Returns a file name from an ada subprogram/package name (ie converts '.'
   --  and '-' to the appropriate characters).
   --  ??? Note: this should be modified to use the naming schemes, if needed.

   function Shorten
     (Path    : String;
      Max_Len : Natural := 40) return String;
   --  Shorten a path to at most Max_Len characters, by replacing the first
   --  directories with "[...]".
   --  For example, "directory_1/directory_2/directory_3/filename"
   --  is shortened as "[...]/directory_3/filename"

   procedure Ensure_Valid_Output;
   --  Ensure that the standard output/error file descriptors can be safely
   --  used. In particular, when using -mwindows under Windows, this procedure
   --  will allocate a console window if needed.

   function URL_List_To_Files (URL_List : String) return File_Array_Access;
   --  Convert list of URLs in form "file:///path/file" to file array.
   --  Files in list are separated by CR/LF characters.

   function UTF8_Full_Name (File : Virtual_File) return UTF8_String;
   --  Try to convert file name to utf-8 string

private

   type Path_Iterator is record
      First, Last : Natural;
   end record;

   pragma Import (C, Ensure_Valid_Output, "__gnat_ensure_valid_output");

end File_Utils;
