#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */

#define OSF1

/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd

#define GETPGRP_NO_ARG

#define SYSV_SYSTEM_DIR

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  We convert it to a -rpath option
   which is what OSF1 uses.  */
#define LD_SWITCH_SYSTEM `echo LD_SWITCH_X_SITE_AUX | sed -e 's/-R/-Wl,-rpath,/'`

#define HAVE_TERMIOS

/* Specify the type that the 3rd arg of `accept' points to.  */
#define SOCKLEN_TYPE int

#ifndef __GNUC__
/* Optimize, inaccurate debugging.  */
#define C_DEBUG_SWITCH -g3
#endif
