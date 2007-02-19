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

--  This package contains subprograms corresponding to general-purpose
--  filesystem queries.

with Filesystem;      use Filesystem;

package Filesystem.Queries is

   function Get_Filesystem (Nickname : String) return Filesystem_Record'Class;
   --  Retrieve the filesystem of the specified server
   --  Raise Invalid_Nickname if Nickname does not correspond to a server

   function Get_Local_Filesystem return Filesystem_Record'Class;
   --  Retrieve the local filesystem type

end Filesystem.Queries;
