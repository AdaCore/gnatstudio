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

   --  See filesystem.ads for specs of the following routines

   function To_Unix
     (FS         : Unix_Filesystem_Record;
      Path       : String;
      Use_Cygwin : Boolean := False) return String;

   function From_Unix
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Is_Absolute_Path
     (FS   : Unix_Filesystem_Record;
      Path : String) return Boolean;

   function Base_Name
     (FS     : Unix_Filesystem_Record;
      Path   : String;
      Suffix : String := "") return String;

   function Base_Dir_Name
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Dir_Name
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Get_Root
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Ensure_Directory
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Device_Name
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Normalize
     (FS   : Unix_Filesystem_Record;
      Path : String) return String;

   function Path
     (FS     : Unix_Filesystem_Record;
      Device : String;
      Dir    : String;
      File   : String) return String;

   function Is_Case_Sensitive (FS : Unix_Filesystem_Record) return Boolean;

   function Has_Devices (FS : Unix_Filesystem_Record) return Boolean;

   -------------------------
   -- Operations on files --
   -------------------------

   function Home_Dir
     (FS   : Unix_Filesystem_Record;
      Host : String) return String;

   function Is_Regular_File
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;

   function Read_File
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return GNAT.Strings.String_Access;

   function Delete
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;

   function Is_Writable
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;

   function Is_Directory
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String) return Boolean;

   function File_Time_Stamp
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String)
      return Ada.Calendar.Time;

   procedure Write
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Temporary_File  : String);

   procedure Set_Writable
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Writable        : Boolean);

   procedure Set_Readable
     (FS              : Unix_Filesystem_Record;
      Host            : String;
      Local_Full_Name : String;
      Readable        : Boolean);

   --------------------------
   -- Directory management --
   --------------------------

   procedure Get_Logical_Drives
     (FS     : Unix_Filesystem_Record;
      Host   : String;
      Buffer : in out String;
      Len    : out Integer);

   function Make_Dir
     (FS             : Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String) return Boolean;

   function Remove_Dir
     (FS             : Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Recursive      : Boolean) return Boolean;

   function Read_Dir
     (FS             : Unix_Filesystem_Record;
      Host           : String;
      Local_Dir_Name : String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List;

end Filesystem.Unix;
