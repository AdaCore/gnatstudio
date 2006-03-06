-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2006                            --
--                             AdaCore                               --
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

--  This package describes the unix filesystem and shell commands

package Filesystem.Unix is

   type Unix_Filesystem_Record is new Filesystem_Record with null record;

   procedure Initialize_Module (FS : Unix_Filesystem_Record);
   --  Initializes the module. Needs to be called once per actual
   --  filesystem class.

   function To_Unix (FS   : Unix_Filesystem_Record;
                     Path : String) return String;
   --  Translates a Path to unix style

   function From_Unix (FS   : Unix_Filesystem_Record;
                       Path : String) return String;
   --  Translates a Path from unix style

   function Is_Absolute_Path (FS   : Unix_Filesystem_Record;
                              Path : String) return Boolean;
   --  Tell wether the path is absolute.

   function Base_Name (FS     : Unix_Filesystem_Record;
                       Path   : String;
                       Suffix : String := "") return String;
   --  Returns the base file name

   function Base_Dir_Name (FS   : Unix_Filesystem_Record;
                           Path : String) return String;
   --  Returns the directory base name

   function Dir_Name (FS   : Unix_Filesystem_Record;
                      Path : String) return String;
   --  Returns the directory name

   function Ensure_Directory (FS   : Unix_Filesystem_Record;
                              Path : String) return String;
   --  Returns a directory path from furnished path

   function Device_Name (FS   : Unix_Filesystem_Record;
                         Path : String) return String;
   --  Returns the device of the path (if applicable). Empty string else.

   function Normalize (FS   : Unix_Filesystem_Record;
                       Path : String) return String;
   --  Replaces every ./ or ../ items of the path.

   function Path (FS : Unix_Filesystem_Record;
                  Device : String;
                  Dir    : String;
                  File   : String) return String;
   --  Returns a path composed of Device, Dir, and File.

   function Is_Case_Sensitive (FS : Unix_Filesystem_Record) return Boolean;
   --  Tell if the Unix_Filesystem_Record is case sensitive

   function Has_Devices (FS : Unix_Filesystem_Record) return Boolean;
   --  Tell if the filesystem handles devices (hard disk letters for Unix)

   -------------------------
   -- Operations on files --
   -------------------------

   function Is_Regular_File
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Return True if Local_Full_Name exists on the remote host.

   function Read_File
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return GNAT.OS_Lib.String_Access;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  No special encoding/decoding for charsets is done on the file.

   function Delete
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Sends host a delete command for file.

   function Is_Writable
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Return True if File is writable.
   --  Some protocols are read-only (HTTP), and will always return False.

   function Is_Directory
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   --  Return True if File is in fact a directory

   function File_Time_Stamp
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return Ada.Calendar.Time;
   --  Return the timestamp for this file.
   --  If the Connection doesn't support this operation, or the file
   --  doesn't exists, it should return a date of No_Time, so as to force, when
   --  possible, a read operation from the caller.

   procedure Write
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String);
   --  Overwrite the contents of Local_Full_Name with Contents.
   --  Raises Use_Error if the file could not be written

   procedure Set_Writable
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean);
   --  If Writable is True, make the file writable, otherwise make the file
   --  unwritable.

   procedure Set_Readable
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean);
   --  If Readable is True, make the file readable, otherwise make the file
   --  unreadable.

   --------------------------
   -- Directory management --
   --------------------------

   procedure Get_Logical_Drives (FS     : Unix_Filesystem_Record;
                                 Host   : String;
                                 Buffer : in out String;
                                 Len    :    out Integer);
   --  Unix filesystems don't support logical drives. Return empty string

   function Make_Dir
     (FS             : Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String)
      return Boolean;
   --  creates a new directory on remote named Local_Dir_Name. Returns the
   --  creation status

   function Remove_Dir
     (FS             : Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean)
      return Boolean;
   --  deletes an empty directory on remote named Local_Dir_Name. Returns the
   --  deletion status

   function Read_Dir
     (FS             : Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return GNAT.OS_Lib.String_List;
   --  reads the specified directory and returns a list of filenames
   --  (base names)

end Filesystem.Unix;
