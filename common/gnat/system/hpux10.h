#include "hpux9shr.h"

#define HPUX10

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef srand48
#undef HAVE_RANDOM
#define HPUX10
#define FORCE_ALLOCA_H

/* AlainF 20-Jul-1996 says this is right.  */
#undef KERNEL_FILE
#define KERNEL_FILE "/stand/vmunix"

#ifdef LIBS_SYSTEM
#undef LIBS_SYSTEM
#endif
#ifdef HPUX_NET
#define LIBS_SYSTEM -ln -l:libdld.sl
#else
#define LIBS_SYSTEM -l:libdld.sl
#endif

/* Rainer Malzbender <rainer@displaytech.com> says definining
   HAVE_XRMSETDATABASE allows Emacs to compile on HP-UX 10.20
   using GCC.  */

#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif

/* Make sure we get select from libc rather than from libcurses
   because libcurses on HPUX 10.10 has a broken version of select.
   We used to use -lc -lcurses, but this may be cleaner.  */
#define LIBS_TERMCAP -ltermcap

#undef C_SWITCH_X_SYSTEM
#undef LD_SWITCH_X_DEFAULT
/* However, HPUX 10 puts Xaw and Xmu in a strange place
   (if you install them at all).  So search that place.  */
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/Motif1.2 -I/usr/contrib/X11R5/include
#define LD_SWITCH_X_DEFAULT -L/usr/lib/X11R5 -L/usr/lib/Motif1.2 -L/usr/contrib/X11R5/lib
