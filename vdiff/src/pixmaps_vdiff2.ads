-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
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

   up_diff_xpm          : aliased Pixmap_Array;
   down_diff_xpm        : aliased Pixmap_Array;
   first_diff_xpm       : aliased Pixmap_Array;
   last_diff_xpm        : aliased Pixmap_Array;
   reload_diff_xpm      : aliased Pixmap_Array;
   unhighlight_diff_xpm : aliased Pixmap_Array;
   close_diff_xpm       : aliased Pixmap_Array;

private
   pragma Import (C, up_diff_xpm, "up_diff_xpm");
   pragma Import (C, down_diff_xpm, "down_diff_xpm");
   pragma Import (C, first_diff_xpm, "first_diff_xpm");
   pragma Import (C, last_diff_xpm, "last_diff_xpm");
   pragma Import (C, reload_diff_xpm, "reload_diff_xpm");
   pragma Import (C, unhighlight_diff_xpm, "unhighlight_diff_xpm");
   pragma Import (C, close_diff_xpm, "close_diff_xpm");

end Pixmaps_Vdiff2;


