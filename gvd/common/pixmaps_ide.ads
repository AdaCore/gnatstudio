-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

package Pixmaps_IDE is

   --  Note that the following pixmaps are in alphabetical order.
   --  The actual size of the pixmaps is only known by GtkAda, which is
   --  enough for our needs and avoid having to maintain this information here.

   arrow_xpm               : aliased Pixmap_Array;
   box_xpm                 : aliased Pixmap_Array;
   break_xpm               : aliased Pixmap_Array;
   cancel_xpm              : aliased Pixmap_Array;
   cont_xpm                : aliased Pixmap_Array;
   display_small_xpm       : aliased Pixmap_Array;
   dot_xpm                 : aliased Pixmap_Array;
   down_xpm                : aliased Pixmap_Array;
   finish_xpm              : aliased Pixmap_Array;
   interrupt_xpm           : aliased Pixmap_Array;
   lock_xpm                : aliased Pixmap_Array;
   mini_folder_xpm         : aliased Pixmap_Array;
   mini_ofolder_xpm        : aliased Pixmap_Array;
   next_xpm                : aliased Pixmap_Array;
   nexti_xpm               : aliased Pixmap_Array;
   package_xpm             : aliased Pixmap_Array;
   paint_xpm               : aliased Pixmap_Array;
   plot_xpm                : aliased Pixmap_Array;
   run_xpm                 : aliased Pixmap_Array;
   start_xpm               : aliased Pixmap_Array;
   step_xpm                : aliased Pixmap_Array;
   stepi_xpm               : aliased Pixmap_Array;
   stop_xpm                : aliased Pixmap_Array;
   subprogram_xpm          : aliased Pixmap_Array;
   trash_xpm               : aliased Pixmap_Array;
   up_xpm                  : aliased Pixmap_Array;
   var_xpm                 : aliased Pixmap_Array;
   watch_xpm               : aliased Pixmap_Array;

private
   pragma Import (C, arrow_xpm, "arrow_xpm");
   pragma Import (C, box_xpm, "box_xpm");
   pragma Import (C, break_xpm, "break_xpm");
   pragma Import (C, cancel_xpm, "cancel_xpm");
   pragma Import (C, cont_xpm, "cont_xpm");
   pragma Import (C, display_small_xpm, "display_small_xpm");
   pragma Import (C, dot_xpm, "dot_xpm");
   pragma Import (C, down_xpm, "down_xpm");
   pragma Import (C, finish_xpm, "finish_xpm");
   pragma Import (C, interrupt_xpm, "interrupt_xpm");
   pragma Import (C, lock_xpm, "lock_xpm");
   pragma Import (C, mini_folder_xpm, "mini_folder_xpm");
   pragma Import (C, mini_ofolder_xpm, "mini_ofolder_xpm");
   pragma Import (C, next_xpm, "next_xpm");
   pragma Import (C, nexti_xpm, "nexti_xpm");
   pragma Import (C, package_xpm, "package_xpm");
   pragma Import (C, paint_xpm, "paint_xpm");
   pragma Import (C, plot_xpm, "plot_xpm");
   pragma Import (C, run_xpm, "run_xpm");
   pragma Import (C, start_xpm, "start_xpm");
   pragma Import (C, step_xpm, "step_xpm");
   pragma Import (C, stepi_xpm, "stepi_xpm");
   pragma Import (C, stop_xpm, "stop_xpm");
   pragma Import (C, subprogram_xpm, "subprogram_xpm");
   pragma Import (C, trash_xpm, "trash_xpm");
   pragma Import (C, up_xpm, "up_xpm");
   pragma Import (C, var_xpm, "var_xpm");
   pragma Import (C, watch_xpm, "watch_xpm");
end Pixmaps_IDE;
