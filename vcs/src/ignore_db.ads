------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

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
