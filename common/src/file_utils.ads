-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
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
with Ada.Calendar;
with GNAT.Directory_Operations;
with Remote;                    use Remote;

package File_Utils is

   function Subdirectories_Count (Directory : String) return Integer;
   --  Return the number of subdirectories for Directory (not counting . and
   --  ..). If Directory is not a directory, then -1 is returned.

   procedure Get_Logical_Drive_Strings
     (Buffer : out String;
      Len    : out Natural);
   --  Store in Buffer (Buffer'First .. Buffer'First + Len) a ASCII.NUL
   --  separated string containing the names of the drives, e.g
   --  "a:\" & NUL & "c:\", or a null string if not relevant on the target.

   function Read_Files_From_Dirs
     (Dirs : String) return VFS.File_Array_Access;
   --  Return all the files found in Dirs. Each directory in Dirs should be
   --  separated with Path_Separator

   function Is_Case_Sensitive
     (Server : Server_Type := GPS_Server) return Boolean;
   --  Return true if filenames are case sensitive on the server.

   function File_Equal
     (File1, File2 : String;
      Server : Server_Type := GPS_Server) return Boolean;
   --  Perform a comparison of file, taking into account the case sensitivity
   --  depending on Server's OS.

   function Relative_Path_Name
     (File_Name, Base_Name : String;
      Server    : Server_Type := GPS_Server) return String;
   --  Modifies File_Name so that it is relative to Base_Name.
   --  Both names are first normalized to platform specific conventions, but
   --  the links are not resolved. The case sensitivity of Server is used.

   function Name_As_Directory
     (Name  : String;
      Style : GNAT.Directory_Operations.Path_Style :=
        GNAT.Directory_Operations.System_Default) return String;
   --  Add a directory separator at the end of Name if there is none.
   --  This also normalizes the pathname (see
   --  GNAT.Directory_Operations.Format_Pathname).
   --  ??? Should go into GNAT.Directory_Operations

   function Is_Absolute_Path_Or_URL (Name : String) return Boolean;
   --  Return True if Name is an absolute file name on the host, or a URL

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

   function File_Time_Stamp (File : String) return Ada.Calendar.Time;
   --  Similar to GNAT.OS_Lib.File_Time_Stamp, but with a return value that can
   --  be easily compared to other time stamps.
   --  Return VFS.No_Time if File does not exist or timestamp cannot be read.

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

   function Find_On_Path
     (Base_Name : String; Path : String) return VFS.Virtual_File;
   --  Search for a file called Base_Name in all the directories. The first
   --  match is returned, or No_File if not found.

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
