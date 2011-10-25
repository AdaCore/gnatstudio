-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2005-2011, AdaCore                --
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

with Entities;
with Language_Handlers;

package Doc_Utils is

   function Get_Documentation
     (Lang_Handler : access Language_Handlers.Language_Handler_Record'Class;
      Entity       : Entities.Entity_Information;
      Declaration_File_Contents : String := "") return String;
   --  Return the documentation for Entity. This is the block of comments
   --  just before or just after the declaration of the entity.
   --  Declaration_File_Contents can be provided to save loading the file from
   --  the disk. However, if not specified, the contents of the file will be
   --  read from the disk as appropriate. When specified,
   --  Declaration_File_Contents must be UTF8-encoded
   --  The returned string is the UTF8-encoded text that is found in the file.

end Doc_Utils;
