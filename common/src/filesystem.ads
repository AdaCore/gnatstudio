-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
--                              AdaCore                              --
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

--  This package is used to describe a complete filesystem and how to
--  manipulate files and directories.
--  A default implementation valid for both windows and unix local filesystems
--  is provided, but this type still needs to be overridden to provide
--  system-specific implementation in some cases. It is also possible to
--  further derive this type to implement remote file systems, ie access to
--  files on remote hosts.

with Ada.Calendar;
with GNAT.Strings;

package Filesystem is

   type Filesystem_Record is abstract tagged null record;
   type Filesystem_Access is access all Filesystem_Record'Class;

   function To_Unix
     (FS         : Filesystem_Record;
      Path       : String;
      Use_Cygwin : Boolean := False) return String is abstract;
   --  Translate a Path to unix style

   function From_Unix
     (FS   : Filesystem_Record;
      Path : String) return String is abstract;
   --  Translate a Path from unix style

   function Is_Subtree
     (FS        : Filesystem_Record;
      Directory : String;
      Full_Path : String) return Boolean;
   --  Tell if Full_Path is in the subtree of Directory
   --  By default, it compares the two strings, and return true if the first
   --  part of Full_Path is equal to directory, taking into account the
   --  case sensitivity

   function Is_Absolute_Path
     (FS   : Filesystem_Record;
      Path : String) return Boolean is abstract;
   --  Tell wether the path is absolute

   function File_Extension
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the file extension
   --  By default, return the characters after the last dot

   function Concat
     (FS   : Filesystem_Record;
      Root : String;
      Sub  : String) return String;
   --  Concatenate a root direectory and a subdirectory
   --  by default, equivalent to 'Root & Sub'.

   function Base_Name
     (FS     : Filesystem_Record;
      Path   : String;
      Suffix : String := "") return String;
   --  Return the base file name

   function Base_Dir_Name
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the directory base name

   function Dir_Name
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the directory path

   function Get_Root
     (FS   : Filesystem_Record;
      Path : String) return String is abstract;
   --  Return the root directory of the path

   function Get_Parent
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return the parent directory of the path

   function Ensure_Directory
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Return a directory path from furnished path.
   --  On Windows, for a path C:\path\to, this will return C:\path\to\
   --  On VMS, for a path disk:[path]to.dir, this will return disk:[path.to]

   function Device_Name
     (FS   : Filesystem_Record;
      Path : String) return String is abstract;
   --  Return the device of the path (if applicable). Empty string otherwise.

   function Normalize
     (FS   : Filesystem_Record;
      Path : String) return String;
   --  Replace every ./ or ../ items of the path.

   function Path
     (FS     : Filesystem_Record;
      Device : String;
      Dir    : String;
      File   : String) return String is abstract;
   --  Return a path composed of Device, Dir, and File.

   function Is_Case_Sensitive
     (FS : Filesystem_Record) return Boolean is abstract;
   --  Tell if the Filesystem_Record is case sensitive

   function Has_Devices
     (FS : Filesystem_Record) return Boolean is abstract;
   --  Tell if the Filesystem handles devices (hard disk letters for windows)

   function Multi_Unit_Index_Char
     (FS : Filesystem_Record) return Character;
   --  The character used by GNAT when creating ALI files for multi-unit files
   --  on the given filesystem (this is generally '~' expect on VMS where it is
   --  set to '$').

   -------------------------
   -- Operations on files --
   -------------------------

   function Home_Dir
     (FS   : Filesystem_Record;
      Host : String) return String;
   --  Return the home directory on the specified host.
   --  If home dir cannot be determined, return root directory

   function Is_Regular_File
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Return True if Local_Full_Name exists on the remote host.

   function Read_File
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return GNAT.Strings.String_Access;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  No special encoding/decoding for charsets is done on the file.

   function Delete
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Sends host a delete command for file.

   function Is_Writable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Return True if File is writable.
   --  Some protocols are read-only (HTTP), and will always return False.

   function Is_Directory
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Return True if File is in fact a directory

   function File_Time_Stamp
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Ada.Calendar.Time;
   --  Return the timestamp for this file.
   --  If the Connection doesn't support this operation, or the file
   --  doesn't exists, it should return a date of No_Time, so as to force, when
   --  possible, a read operation from the caller.

   procedure Write
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False);
   --  Overwrite the contents of Local_Full_Name with the contents of the
   --  Temporary_File.
   --  Raise Use_Error if the file could not be written

   procedure Set_Writable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean);
   --  If Writable is True, make the file writable, otherwise make the file
   --  unwritable.

   procedure Set_Readable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean);
   --  If Readable is True, make the file readable, otherwise make the file
   --  unreadable.

   --------------------------
   -- Directory management --
   --------------------------

   procedure Get_Logical_Drives
     (FS     : Filesystem_Record;
      Host   : String;
      Buffer : in out String;
      Len    : out Integer) is abstract;
   --  Buffer return the logical drives separated by a NULL character.

   function Make_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean;
   --  Create a new directory on remote named Local_Dir_Name.
   --  Return the creation status

   function Remove_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean;
   --  Delete an empty directory on remote named Local_Dir_Name.
   --  Return the deletion status

   function Read_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False)
      return GNAT.Strings.String_List;
   --  Read the specified directory and returns a list of filenames
   --  (base names). If Dirs_Only is set, then the files returned are directory
   --  only. Same for Files_Only, concerning regular files.

end Filesystem;
