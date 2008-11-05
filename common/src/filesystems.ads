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

--  This package contains the GPS layer for GNATCOLL.Filesystem and
--  GNATCOLL.VFS

with GNATCOLL.Filesystem;  use GNATCOLL.Filesystem;
with GNATCOLL.VFS;         use GNATCOLL.VFS;
with String_List_Utils;    use String_List_Utils;

package Filesystems is

   type Filesystem_Type is (Windows, Unix);
   --  The filesystems supported by GPS

   function Get_Filesystem (Nickname : String) return Filesystem_Access;
   --  Retrieve the filesystem of the specified server
   --  Raise Invalid_Nickname if Nickname does not correspond to a server

   function Is_Local_Filesystem
     (FS : access Filesystem_Record'Class) return Boolean;
   --  Whether the filesystem is running on the local machine or a remote host
   --  (or more precisely whether it is accessed directly through the C
   --  library (local and NFS-mounted file systems), or through external
   --  commands.

   function Get_Host
     (FS : access Filesystem_Record'Class) return String;
   --  Name of the host on which the filesystem is running.
   --  This returns "" for the local host.

   function Filesystem_Factory
     (Typ      : Filesystem_Type;
      Nickname : String) return GNATCOLL.Filesystem.Filesystem_Access;
   --  Create a new instance that applies to a specific network host.
   --  If the Nickname is the empty string is Remote.Lock_Nickname, then a
   --  local filesystem is created. Otherwise, a remote filesytem is created.

   function Create (Files : String_List.List) return File_Array;
   --  Returns a File_Array out of a string list of file names

   function Get_Host (File : Virtual_File) return String;
   --  Returns the host containing the file. If the host is the localhost,
   --  the empty string is returned.

   function Is_Local (File : Virtual_File) return Boolean;
   --  Whether the file is on the local host or on a remote host

   function Filename_To_UTF8 (Name : String) return String;
   --  Used to convert a filename into the proper internal representation
   --  expected by GPS with is UTF-8.

   function Filename_From_UTF8 (Name : String) return String;
   --  Convert a filename encoded in UTF-8 back into the local encoding

end Filesystems;
