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

--  This package is used to describe a complete filesystem and how to use it

with Ada.Calendar;
with GNAT.OS_Lib;

package Filesystem is

   type Filesystem_Record is abstract tagged null record;
   type Filesystem_Access is access all Filesystem_Record'Class;

   procedure Initialize_Module (FS : Filesystem_Record) is abstract;
   --  Initializes the module. Needs to be called once per actual
   --  filesystem class.

   function To_Unix (FS   : Filesystem_Record;
                     Path : String) return String is abstract;
   --  Translates a Path to unix style

   function From_Unix (FS   : Filesystem_Record;
                       Path : String) return String is abstract;
   --  Translates a Path from unix style

   function Is_Subtree (FS        : Filesystem_Record;
                        Directory : String;
                        Full_Path : String) return Boolean;
   --  Tells if Full_Path is in the subtree of Directory
   --  By default, it compares the two strings, and return true if the first
   --  part of Full_Path is equal to directory, taking into account the
   --  case sensitivity

   function Is_Absolute_Path (FS   : Filesystem_Record;
                              Path : String) return Boolean is abstract;
   --  Tell wether the path is absolute.

   function File_Extension (FS   : Filesystem_Record;
                            Path : String) return String;
   --  Returns the file extension
   --  By default, return the characters after the last dot

   function Concat (FS   : Filesystem_Record;
                    Root : String;
                    Sub  : String) return String;
   --  Concatenate a root direectory and a subdirectory
   --  by default, equivalent to 'Root & Sub'

   function Base_Name (FS     : Filesystem_Record;
                       Path   : String;
                       Suffix : String := "") return String is abstract;
   --  Returns the base file name

   function Base_Dir_Name (FS   : Filesystem_Record;
                           Path : String) return String is abstract;
   --  Returns the directory base name

   function Dir_Name (FS   : Filesystem_Record;
                      Path : String) return String is abstract;
   --  Returns the directory path

   function Ensure_Directory (FS   : Filesystem_Record;
                              Path : String) return String is abstract;
   --  Returns a directory path from furnished path
   --  On windows, for a path C:\path\to, this will return C:\path\to\
   --  On VMS, for a path disk:[path]to.dir, this will return disk:[path.to]

   function Device_Name (FS   : Filesystem_Record;
                         Path : String) return String is abstract;
   --  Returns the device of the path (if applicable). Empty string else.

   function Normalize (FS   : Filesystem_Record;
                       Path : String) return String is abstract;
   --  Replaces every ./ or ../ items of the path.

   function Path (FS : Filesystem_Record;
                  Device : String;
                  Dir    : String;
                  File   : String) return String is abstract;
   --  Returns a path composed of Device, Dir, and File.

   function Is_Case_Sensitive (FS : Filesystem_Record) return Boolean
   is abstract;
   --  Tell if the Filesystem_Record is case sensitive

   function Has_Devices (FS : Filesystem_Record) return Boolean
   is abstract;
   --  Tell if the Filesystem handles devices (hard disk letters for windows)

   -------------------------
   -- Operations on files --
   -------------------------

   function Is_Regular_File
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean is abstract;
   --  Return True if Local_Full_Name exists on the remote host.

   function Read_File
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return GNAT.OS_Lib.String_Access is abstract;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  No special encoding/decoding for charsets is done on the file.

   function Delete
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean is abstract;
   --  Sends host a delete command for file.

   function Is_Writable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean is abstract;
   --  Return True if File is writable.
   --  Some protocols are read-only (HTTP), and will always return False.

   function Is_Directory
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean is abstract;
   --  Return True if File is in fact a directory

   function File_Time_Stamp
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return Ada.Calendar.Time is abstract;
   --  Return the timestamp for this file.
   --  If the Connection doesn't support this operation, or the file
   --  doesn't exists, it should return a date of No_Time, so as to force, when
   --  possible, a read operation from the caller.

   procedure Write
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String) is abstract;
   --  Overwrite the contents of Local_Full_Name with Contents.
   --  Raises Use_Error if the file could not be written

   procedure Set_Writable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean) is abstract;
   --  If Writable is True, make the file writable, otherwise make the file
   --  unwritable.

   procedure Set_Readable
     (FS              : Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean) is abstract;
   --  If Readable is True, make the file readable, otherwise make the file
   --  unreadable.

   --------------------------
   -- Directory management --
   --------------------------

   procedure Get_Logical_Drives (FS     : Filesystem_Record;
                                 Host   : String;
                                 Buffer : in out String;
                                 Len    :    out Integer) is abstract;
   --  Buffer return the logical drives separated by a NULL character.

   function Make_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String)
      return Boolean is abstract;
   --  creates a new directory on remote named Local_Dir_Name. Returns the
   --  creation status

   function Remove_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean)
      return Boolean is abstract;
   --  deletes an empty directory on remote named Local_Dir_Name. Returns the
   --  deletion status

   function Read_Dir
     (FS             : Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return GNAT.OS_Lib.String_List
   is abstract;
   --  reads the specified directory and returns a list of filenames
   --  (base names)

private

   procedure Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Status                : out Boolean;
      Execution_Directory   : String  := "");

   procedure Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Out_Value             : out GNAT.OS_Lib.String_Access;
      Status                : out Boolean;
      Execution_Directory   : String  := "");

end Filesystem;
