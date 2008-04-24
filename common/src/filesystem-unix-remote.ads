-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2008, AdaCore                  --
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

--  This package describes a unix filesystem on a remote host. All file
--  manipulation are done by spawning shell commands.

package Filesystem.Unix.Remote is

   type Remote_Unix_Filesystem_Record
     is new Unix_Filesystem_Record with null record;

   overriding function Home_Dir
     (FS   : Remote_Unix_Filesystem_Record;
      Host : String) return String;
   overriding function Is_Regular_File
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Read_File
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return GNAT.Strings.String_Access;
   overriding function Delete
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Writable
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Directory
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function File_Time_Stamp
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return Ada.Calendar.Time;
   overriding procedure Write
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False);
   overriding procedure Set_Writable
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean);
   overriding procedure Set_Readable
     (FS              : Remote_Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean);
   overriding function Make_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean;
   overriding function Remove_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean;
   overriding function Read_Dir
     (FS             : Remote_Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List;
   --  See inherited documentation in parent package

end Filesystem.Unix.Remote;
