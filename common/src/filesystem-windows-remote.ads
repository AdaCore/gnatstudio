-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2008, AdaCore             --
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

--  This package describes the Windows filesystem on a remote host

package Filesystem.Windows.Remote is

   type Remote_Windows_Filesystem_Record is new Windows_Filesystem_Record
      with null record;

   overriding function Home_Dir
     (FS   : Remote_Windows_Filesystem_Record;
      Host : String) return String;
   overriding function Is_Regular_File
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Read_File
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return GNAT.Strings.String_Access;
   overriding function Delete
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Writable
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Symbolic_Link
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function Is_Directory
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;
   overriding function File_Time_Stamp
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Ada.Calendar.Time;
   overriding procedure Write
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String;
      Append          : Boolean := False);
   overriding procedure Set_Writable
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean);
   overriding procedure Set_Readable
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean);
   overriding procedure Get_Logical_Drives
     (FS     : Remote_Windows_Filesystem_Record;
      Host   : String;
      Buffer : in out String;
      Len    :    out Integer);
   overriding function Make_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean;
   overriding function Remove_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean;
   overriding function Read_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List;
   overriding function Rename
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   overriding function Copy
     (FS              : Remote_Windows_Filesystem_Record;
      Host            : String;
      From_Local_Name : String;
      To_Local_Name   : String) return Boolean;
   overriding function Change_Dir
     (FS             : Remote_Windows_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean;
   --  See inherited documentation

   --  Copy_Dir is inherited from the local version: we have to read the list
   --  of files in the directory and then copy them one by one.

end Filesystem.Windows.Remote;
