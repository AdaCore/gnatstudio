------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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
