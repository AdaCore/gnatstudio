/* Handle Solaris 2.5.  */

/* Enable large-file support if available (Solaris 2.6 and later).
   Do this before including any system include file.  */
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE 1
#endif
#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 64
#endif

#include <sys/ioctl.h>
#include <sys/stropts.h>
#include "sol2-4.h"

