-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with VCS; use VCS;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package Ignore_Db is

   function Ignore_File
     (VCS  : VCS_Access;
      File : Virtual_File) return Boolean;
   --  Returns True if the given file is to be ignored. This depends on the
   --  VCS. If a specific ignore file is set it is used to check wether a file
   --  must be ignored or not. On CVS this is typically what is done with the
   --  file .cvsignore. This implementation can used any filename and supports
   --  any VCS. An ingored file will not be displyed on the VCS Explorer.

end Ignore_Db;
