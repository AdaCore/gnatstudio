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

--  This package contains the GPS layer for GNATCOLL.Filesystem

with GNATCOLL.Filesystem;  use GNATCOLL.Filesystem;

package Filesystems is

   type Filesystem_Type is (Windows, Unix);
   --  The filesystems supported by GPS

   function Get_Filesystem (Nickname : String) return Filesystem_Record'Class;
   --  Retrieve the filesystem of the specified server
   --  Raise Invalid_Nickname if Nickname does not correspond to a server

   function Get_Local_Filesystem return Filesystem_Record'Class;
   --  Retrieve the local filesystem type

   function Filesystem_Factory
     (Typ      : Filesystem_Type;
      Nickname : String) return GNATCOLL.Filesystem.Filesystem_Access;
   --  Create a new instance that applies to a specific network host.
   --  If the Nickname is the empty string is Remote.Lock_Nickname, then a
   --  local filesystem is created. Otherwise, a remote filesytem is created.
end Filesystems;
