------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Basic_Types; use Basic_Types;

package Pixmaps_IDE is

   --  Note that the following pixmaps are in alphabetical order.
   --  The actual size of the pixmaps is only known by GtkAda, which is
   --  enough for our needs and avoid having to maintain this information here.

   arrow_xpm               : aliased Pixmap_Array;
   box_xpm                 : aliased Pixmap_Array;
   --  break_xpm               : aliased Pixmap_Array;
   --  cancel_xpm              : aliased Pixmap_Array;
   --  display_small_xpm       : aliased Pixmap_Array;
   dot_xpm                 : aliased Pixmap_Array;
   grey_dot_xpm            : aliased Pixmap_Array;
   --  package_xpm             : aliased Pixmap_Array;
   --  paint_xpm               : aliased Pixmap_Array;
   --  subprogram_xpm          : aliased Pixmap_Array;
   stop_xpm                : aliased Pixmap_Array;
   trash_xpm               : aliased Pixmap_Array;
   --  var_xpm                 : aliased Pixmap_Array;

private
   pragma Import (C, arrow_xpm, "arrow_xpm");
   pragma Import (C, box_xpm, "box_xpm");
   --  pragma Import (C, break_xpm, "break_xpm");
   --  pragma Import (C, cancel_xpm, "cancel_xpm");
   --     pragma Import (C, display_small_xpm, "display_small_xpm");
   pragma Import (C, dot_xpm, "dot_xpm");
   pragma Import (C, grey_dot_xpm, "grey_dot_xpm");
   pragma Import (C, stop_xpm, "stop_xpm");
   --  pragma Import (C, package_xpm, "package_xpm");
   --  pragma Import (C, paint_xpm, "paint_xpm");
   --  pragma Import (C, subprogram_xpm, "subprogram_xpm");
   pragma Import (C, trash_xpm, "trash_xpm");
   --  pragma Import (C, var_xpm, "var_xpm");
end Pixmaps_IDE;
