-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2007                        --
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

with Config; use Config;

with Shell_Descriptors; use Shell_Descriptors;

with Filesystem.Windows;
with Filesystem.Unix;

package body Filesystem.Queries is

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (Nickname : String) return Filesystem_Record'Class is
   begin
      if Nickname = "" then
         return Get_Local_Filesystem;
      else
         return Get_Shell_Descriptor (Nickname).Filesystem.all;
      end if;
   end Get_Filesystem;

   --------------------------
   -- Get_Local_Filesystem --
   --------------------------

   function Get_Local_Filesystem return Filesystem_Record'Class is
      Windows_FS : Filesystem.Windows.Windows_Filesystem_Record;
      Unix_FS    : Filesystem.Unix.Unix_Filesystem_Record;
   begin
      if Host = Config.Windows then
         return Windows_FS;
      else
         --  Unix and windows support only
         return Unix_FS;
      end if;
   end Get_Local_Filesystem;

end Filesystem.Queries;
