with Gtkada.Types; use Gtkada.Types;

package Odd.Pixmaps is

   breakat   : Chars_Ptr_Array;
   cont      : Chars_Ptr_Array;
   display   : Chars_Ptr_Array;
   down      : Chars_Ptr_Array;
   findfwd   : Chars_Ptr_Array;
   finish    : Chars_Ptr_Array;
   interrupt : Chars_Ptr_Array;
   kill      : Chars_Ptr_Array;
   lookup    : Chars_Ptr_Array;
   next      : Chars_Ptr_Array;
   nexti     : Chars_Ptr_Array;
   plot      : Chars_Ptr_Array;
   print     : Chars_Ptr_Array;
   rotate    : Chars_Ptr_Array;
   run       : Chars_Ptr_Array;
   set       : Chars_Ptr_Array;
   show      : Chars_Ptr_Array;
   start     : Chars_Ptr_Array;
   step      : Chars_Ptr_Array;
   stepi     : Chars_Ptr_Array;
   undisplay : Chars_Ptr_Array;
   until     : Chars_Ptr_Array;
   up        : Chars_Ptr_Array;
   watch     : Chars_Ptr_Array;

private
   pragma Import (C, breakat, "breakat_xpm");
   pragma Import (C, cont, "cont_xpm");
   pragma Import (C, display, "display_xpm");
   pragma Import (C, down, "down_xpm");
   pragma Import (C, findfwd, "findfwd_xpm");
   pragma Import (C, finish, "finish_xpm");
   pragma Import (C, interrupt, "interrupt_xpm");
   pragma Import (C, kill, "kill_xpm");
   pragma Import (C, lookup, "lookup_xpm");
   pragma Import (C, next, "next_xpm");
   pragma Import (C, nexti, "nexti_xpm");
   pragma Import (C, plot, "plot_xpm");
   pragma Import (C, print, "print_xpm");
   pragma Import (C, rotate, "rotate_xpm");
   pragma Import (C, run, "run_xpm");
   pragma Import (C, set, "set_xpm");
   pragma Import (C, show, "show_xpm");
   pragma Import (C, start, "start_xpm");
   pragma Import (C, step, "step_xpm");
   pragma Import (C, stepi, "stepi_xpm");
   pragma Import (C, undisplay, "undisplay_xpm");
   pragma Import (C, until, "until_xpm");
   pragma Import (C, up, "up_xpm");
   pragma Import (C, watch, "watch_xpm");
end Odd.Pixmaps;
