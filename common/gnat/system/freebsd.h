/* s/ file for freebsd system.  */

/* Get the correct __FreeBSD_version, even if this is before that was
   defined. */
#ifndef __FreeBSD__
#define __FreeBSD_version 199401
#elif __FreeBSD__ == 1
#define __FreeBSD_version 199405
#else
#include <osreldate.h>
#endif

/* '__FreeBSD__' is defined by the preprocessor on FreeBSD-1.1 and up.
   Earlier versions do not have shared libraries, so inhibit them.
   You can inhibit them on newer systems if you wish
   by defining NO_SHARED_LIBS.  */
#ifndef __FreeBSD__
#define NO_SHARED_LIBS
#endif


#if 0 /* This much, alone, seemed sufficient as of 19.23.
	 But it seems better to be independent of netbsd.h.  */
#include "netbsd.h"

#undef LIB_GCC
#define LIB_GCC -lgcc
#undef NEED_ERRNO
#endif /* 0 */


/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h. */
#define BSD4_2

/* These aren't needed, since we have getloadavg.  */
#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* Use XPG4 Library for FreeBSD */
#define LIBS_XPG
#if (__FreeBSD_version >= 220000 && __FreeBSD_version < 400020) \
    || (__FreeBSD_version >= 500000 && __FreeBSD_version < 500005)
#ifdef USE_XPG
#undef LIBS_XPG
#define LIBS_XPG -lxpg4
#endif
#endif

#define LIBS_DEBUG
#define LIBS_SYSTEM -lutil LIBS_XPG
#if __FreeBSD_version < 400000
#define LIBS_TERMCAP -ltermcap
#endif

#define SYSV_SYSTEM_DIR

/* freebsd has POSIX-style pgrp behavior. */
#undef BSD_PGRPS
#define GETPGRP_NO_ARG

#ifdef __ELF__

#define LD_SWITCH_SYSTEM

#ifdef __alpha__
#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crtbegin.o
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o
#else
#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o /usr/lib/crtbegin.o
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o /usr/lib/crtn.o
#endif

#define UNEXEC unexelf.o
#undef LIB_GCC
#define LIB_GCC

#else /* not __ELF__ */

#ifndef NO_SHARED_LIBS
#define LD_SWITCH_SYSTEM -e start -dc
#define HAVE_TEXT_START		/* No need to define `start_of_text'. */
#if __FreeBSD_version >= 300002
#define START_FILES pre-crt0.o /usr/lib/aout/crt0.o
#else /* __FreeBSD_version < 300002 */
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#endif /* __FreeBSD_version < 300002 */
#define UNEXEC unexsunos4.o
#define RUN_TIME_REMAP
#define LIB_GCC -lgcc

#ifndef N_TRELOFF
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)
#endif
#else /* NO_SHARED_LIBS */
#ifdef __FreeBSD__  /* shared libs are available, but the user prefers
                     not to use them.  */
#define LD_SWITCH_SYSTEM -Bstatic
#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#endif /* __FreeBSD__ */
#endif /* NO_SHARED_LIBS */

#endif /* not __ELF__ */

#define HAVE_WAIT_HEADER
#define HAVE_GETLOADAVG
/*#define HAVE_GETPAGESIZE  /* configure now puts this in config.h */
#define HAVE_TERMIOS
#define NO_TERMIO
#define DECLARE_GETPWUID_WITH_UID_T

/* freebsd uses OXTABS instead of the expected TAB3. */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* this silences a few compilation warnings */
#undef BSD_SYSTEM
#if __FreeBSD__ == 1
#define BSD_SYSTEM 199103
#elif __FreeBSD__ == 2
#define BSD_SYSTEM 199306
#elif __FreeBSD__ >= 3
#define BSD_SYSTEM 199506
#endif

#define WAITTYPE int
/* get this since it won't be included if WAITTYPE is defined */
#ifdef emacs
#include <sys/wait.h>
#endif
#define WRETCODE(w) (_W_INT(w) >> 8)

/* Needed to avoid hanging when child process writes an error message
   and exits -- enami tsugutomo <enami@ba2.so-net.or.jp>.  */
#define vfork fork

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */
#define DONT_REOPEN_PTY

/* CLASH_DETECTION is defined in bsd4-3.h.
   In FreeBSD 2.1.5 (and other 2.1.x), this results useless symbolic links
   remaining in /tmp or other directories with +t bit.
   To avoid this problem, you could #undef it to use no file lock. */
/* #undef CLASH_DETECTION */

/* Circumvent a bug in FreeBSD.  In the following sequence of
   writes/reads on a PTY, read(2) returns bogus data:

   write(2)  1022 bytes
   write(2)   954 bytes, get EAGAIN
   read(2)   1024 bytes in process_read_output
   read(2)     11 bytes in process_read_output

   That is, read(2) returns more bytes than have ever been written
   successfully.  The 1033 bytes read are the 1022 bytes written
   successfully after processing (for example with CRs added if the
   terminal is set up that way which it is here).  The same bytes will
   be seen again in a later read(2), without the CRs.  */

#define BROKEN_PTY_READ_AFTER_EAGAIN 1
