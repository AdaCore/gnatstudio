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

end File_Utils;
