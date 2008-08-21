-----------------------------------------------------------------------
--                                GPS                                --
--                                                                   --
--                     Copyright (C) 2000-2008, AdaCore              --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Basic_Types; use Basic_Types;

package Pixmaps_Vdiff2 is

   red_button_xpm       : aliased Pixmap_Array;
   green_button_xpm     : aliased Pixmap_Array;

private
   pragma Import (C, red_button_xpm, "red_button_xpm");
   pragma Import (C, green_button_xpm, "green_button_xpm");
end Pixmaps_Vdiff2;
