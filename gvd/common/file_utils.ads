-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with VFS;
with GNAT.Directory_Operations;

package File_Utils is

   function Subdirectories_Count (Directory : String) return Integer;
   --  Return the number of subdirectories for Directory (not counting . and
   --  ..). If Directory is not a directory, then -1 is returned.

   procedure Get_Logical_Drive_Strings
     (Buffer : out String;
      Len    : out Natural);
   --  Store in Buffer (Buffer'First .. Buffer'First + Len) a ASCII.NUL
   --  separated string containing the names of the --  drives, e.g
   --  "a:\" & NUL & "c:\", or a null string if not relevant on the target.

   function Read_Files_From_Dirs
     (Dirs : String) return VFS.File_Array_Access;
   --  Return all the files found in Dirs. Each directory in Dirs should be
   --  separated with Path_Separator

   function Filenames_Are_Case_Sensitive return Boolean;
   --  Return true if filenames are case sensitive on the current system.

   function File_Equal (File1, File2 : String) return Boolean;
   --  Perform a comparison of file, taking into account the case sensitivity
   --  depending on the OS.

   procedure Set_Writable
     (File     : VFS.Virtual_File;
      Writable : Boolean);
   --  If Writable is True, make File writable, otherwise make File unwritable.

   procedure Set_Readable
     (File     : String;
      Readable : Boolean);
   --  If Readable is True, make File readable, otherwise make File unreadable.

   function Relative_Path_Name
     (File_Name : String; Base_Name : String) return String;
   --  Modifies File_Name so that it is relative to Base_Name.
   --  Both names are first normalized to platform specific conventions, but
   --  the links are not resolved.

   function Name_As_Directory
     (Name  : String;
      Style : GNAT.Directory_Operations.Path_Style :=
        GNAT.Directory_Operations.System_Default) return String;
   --  Add a directory separator at the end of Name if there is none.
   --  This also normalizes the pathname (see
   --  GNAT.Directory_Operations.Format_Pathname).
   --  ??? Should go into GNAT.Directory_Operations

   function Suffix_Matches
     (File_Name : String; Suffix : String) return Boolean;
   --  Return true if File_Name has the given Suffix. This is more general
   --  than extensions, since it doesn't need to start after a '.'.
   --  Note that this function also return False when Filename = Extension
   --  as this does not make sense for a source filename.

   function To_Unix_Pathname (Path : String) return String;
   --  Convert all occurences of Directory_Separator to '/'.

   function To_Host_Pathname (Path : String) return String;
   --  By default, return Path.
   --  If Directory_Separator is different than '/', the following
   --  substitution is operated:
   --  /cydrive/x/ -> x:\
   --  where x is an single character.

   function To_File_Name (Name : in String) return String;
   --  Returns a file name from an ada subprogram/package name (ie converts '.'
   --  and '-' to the appropriate characters).
   --  ??? Note: this should be modified to use the naming schemes, if needed.

   function Shorten (Path : String; Max_Len : Natural := 40) return String;
   --  Shorten a path to at most Max_Len characters, by replacing the first
   --  directories with "[...]".
   --  For example, "directory_1/directory_2/directory_3/filename"
   --  is shortened as "[...]/directory_3/filename"

   type Path_Iterator is private;

   function Start (Path : String) return Path_Iterator;
   --  Return the first directory in Path

   function Next (Path : String; Iter : Path_Iterator) return Path_Iterator;
   --  Return the next iterator in Path

   function At_End (Path : String; Iter : Path_Iterator) return Boolean;
   --  Return True if there are no more directories to return

   function Current (Path : String; Iter : Path_Iterator) return String;
   --  Return the current directory. The name might be empty if the Path
   --  contains something like "::" on Unix systems.

private

   type Path_Iterator is record
      First, Last : Natural;
   end record;

end File_Utils;
