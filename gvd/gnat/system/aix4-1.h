#define AIX4_1 

#include "aix4.h"

/* olson@mcs.anl.gov says -li18n is needed by -lXm.  */
#undef LIB_MOTIF
#define LIB_MOTIF -lXm -li18n

#ifdef __GNUC__
#undef _NO_PROTO
#endif

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes.  Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu.
   4.1 seems to need -g again. -- larry@vaquita.mitra.com.  */
/* David Edelsohn <dje@watson.ibm.com> says that this actually depends
   on the version of XLC, which can't be predicted from the system version.
   What a mess!  */
/* No more of a mess than other systems, GNU+GCC included.  See
   comments in aix3-2-5.h.  -- fx */
#if 0
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#undef C_OPTIMIZE_SWITCH
#define C_DEBUG_SWITCH -g
#endif
#endif

/* The X internationalization stuff is still broken in AIX 4.1, so
   don't #undef X11R5_INHIBIT_I18N
   It still causes shift, ctrl, and alt to resend the last character,
   if it was a control character like tab, enter, backspace, or ESC.
   Bill_Mann @ PraxisInt.com   */
/* #undef X11R5_INHIBIT_I18N */

#ifndef HAVE_LIBXMU
#define LIBXMU

/* Unfortunately without libXmu we cannot support EditRes.  */
#define NO_EDITRES
#endif
