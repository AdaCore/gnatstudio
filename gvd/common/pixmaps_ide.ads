-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

with Gtkada.Types; use Gtkada.Types;

package Odd.Pixmaps is

   --  Note that the following pixmaps are in alphabetical order.
   --  The actual size of the pixmaps is only known by GtkAda, which is
   --  enough for our needs and avoid having to maintain this information here.

   arrow_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   box_xpm           : aliased Chars_Ptr_Array (0 .. 0);
   breakat_xpm       : aliased Chars_Ptr_Array (0 .. 0);
   break_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   cancel_xpm        : aliased Chars_Ptr_Array (0 .. 0);
   cont_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   display_xpm       : aliased Chars_Ptr_Array (0 .. 0);
   display_small_xpm : aliased Chars_Ptr_Array (0 .. 0);
   dot_xpm           : aliased Chars_Ptr_Array (0 .. 0);
   down_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   findfwd_xpm       : aliased Chars_Ptr_Array (0 .. 0);
   finish_xpm        : aliased Chars_Ptr_Array (0 .. 0);
   interrupt_xpm     : aliased Chars_Ptr_Array (0 .. 0);
   kill_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   lock_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   lookup_xpm        : aliased Chars_Ptr_Array (0 .. 0);
   mini_folder_xpm   : aliased Chars_Ptr_Array (0 .. 0);
   mini_ofolder_xpm  : aliased Chars_Ptr_Array (0 .. 0);
   next_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   nexti_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   package_xpm       : aliased Chars_Ptr_Array (0 .. 0);
   plot_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   print_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   rotate_xpm        : aliased Chars_Ptr_Array (0 .. 0);
   run_xpm           : aliased Chars_Ptr_Array (0 .. 0);
   set_xpm           : aliased Chars_Ptr_Array (0 .. 0);
   show_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   start_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   step_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   stepi_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   stop_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   subprogram_xpm    : aliased Chars_Ptr_Array (0 .. 0);
   trash_xpm         : aliased Chars_Ptr_Array (0 .. 0);
   undisplay_xpm     : aliased Chars_Ptr_Array (0 .. 0);
   up_xpm            : aliased Chars_Ptr_Array (0 .. 0);
   var_xpm           : aliased Chars_Ptr_Array (0 .. 0);
   watch_xpm         : aliased Chars_Ptr_Array (0 .. 0);

private
   pragma Import (C, arrow_xpm, "arrow_xpm");
   pragma Import (C, box_xpm, "box_xpm");
   pragma Import (C, break_xpm, "break_xpm");
   pragma Import (C, breakat_xpm, "breakat_xpm");
   pragma Import (C, cancel_xpm, "cancel_xpm");
   pragma Import (C, cont_xpm, "cont_xpm");
   pragma Import (C, display_xpm, "display_xpm");
   pragma Import (C, display_small_xpm, "display_small_xpm");
   pragma Import (C, dot_xpm, "dot_xpm");
   pragma Import (C, down_xpm, "down_xpm");
   pragma Import (C, findfwd_xpm, "findfwd_xpm");
   pragma Import (C, finish_xpm, "finish_xpm");
   pragma Import (C, interrupt_xpm, "interrupt_xpm");
   pragma Import (C, kill_xpm, "kill_xpm");
   pragma Import (C, lock_xpm, "lock_xpm");
   pragma Import (C, lookup_xpm, "lookup_xpm");
   pragma Import (C, mini_folder_xpm, "mini_folder_xpm");
   pragma Import (C, mini_ofolder_xpm, "mini_ofolder_xpm");
   pragma Import (C, next_xpm, "next_xpm");
   pragma Import (C, nexti_xpm, "nexti_xpm");
   pragma Import (C, package_xpm, "package_xpm");
   pragma Import (C, plot_xpm, "plot_xpm");
   pragma Import (C, print_xpm, "print_xpm");
   pragma Import (C, rotate_xpm, "rotate_xpm");
   pragma Import (C, run_xpm, "run_xpm");
   pragma Import (C, set_xpm, "set_xpm");
   pragma Import (C, show_xpm, "show_xpm");
   pragma Import (C, start_xpm, "start_xpm");
   pragma Import (C, step_xpm, "step_xpm");
   pragma Import (C, stepi_xpm, "stepi_xpm");
   pragma Import (C, stop_xpm, "stop_xpm");
   pragma Import (C, subprogram_xpm, "subprogram_xpm");
   pragma Import (C, trash_xpm, "trash_xpm");
   pragma Import (C, undisplay_xpm, "undisplay_xpm");
   pragma Import (C, up_xpm, "up_xpm");
   pragma Import (C, var_xpm, "var_xpm");
   pragma Import (C, watch_xpm, "watch_xpm");
end Odd.Pixmaps;
