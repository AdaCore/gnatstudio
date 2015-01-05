------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

--  This package implements the kernel functions needed to retrieve registered
--  styles or load/save styles to disk.

with String_Hash;

with GPS.Styles;    use GPS.Styles;
with GPS.Styles.UI; use GPS.Styles.UI;

package GPS.Kernel.Styles is

   function Get_Or_Create_Style
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Create : Boolean := True) return Style_Access;
   --  Lookup the style Name.
   --  If it doesn't exist and Create = True, then create it.

   function Get_Or_Create_Style_Copy
     (Kernel     : Kernel_Handle;
      Name       : String;
      From_Style : Style_Access) return Style_Access;
   --  Lookups the style Name. Creates new style as copy of Style when it
   --  doesn't exists.

   procedure Init (Kernel : Kernel_Handle);
   --  Initialize the data structures used to store styles

private
   package Style_Htable is new String_Hash (Style_Access, Free, null);

   type Style_Htable_Record is new Root_Table with record
      Table : Style_Htable.String_Hash_Table.Instance;
   end record;
   type Style_Htable_Access is access all Style_Htable_Record'Class;

   overriding procedure Reset (X : access Style_Htable_Record);
   --  Reset the table.

end GPS.Kernel.Styles;
