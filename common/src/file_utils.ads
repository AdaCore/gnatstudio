-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2009, AdaCore              --
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

--  This package should eventually be merged with GNAT.OS_Lib

with GNATCOLL.VFS; use GNATCOLL.VFS;

package File_Utils is

   function Is_Absolute_Path_Or_URL (Name : Filesystem_String) return Boolean;
   --  Return True if Name is an absolute file name on the host, or a URL

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

private

   type Path_Iterator is record
      First, Last : Natural;
   end record;

   pragma Import (C, Ensure_Valid_Output, "__gnat_ensure_valid_output");

end File_Utils;
