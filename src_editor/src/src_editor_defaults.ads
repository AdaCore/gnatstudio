-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  <description>
--  this package contain all the default values used by the source editor.
--  </description>

package Src_Editor_Defaults is

   Default_Font_Name : constant String := "Courier";
   --  The name of the default font.

   Default_Font_Size : constant := 12;
   --  The size of the default font.

   Default_Font_Description : constant String :=
     Default_Font_Name & Natural'Image (Default_Font_Size);
   --  A font description of the default font. This description is
   --  compatible with the Pango description format.

end Src_Editor_Defaults;
