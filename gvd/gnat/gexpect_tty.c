
/********************************************************************
 **  Handling of pseudo-terminals
 **  This code was copied from Emacs's sources, and adapted to the
 **  context of GNAT.Expect.Tty.
 **
 **  Note that this code is GPL.
 **  Most of this code comes from process.c (function create_process)
 **
 ********************************************************************/

/*  List of #define involved
    AIX
    BSD4_1
    BTTYDISC
    DONT_REOPEN_PTY
    DOS_NT
    EMACS_SET_TTY_PGRP
    FIONBIO
    HAVE_LTCHARS
    HAVE_SETPGID
    HAVE_SETSID
    HAVE_TCATTR
    HAVE_TERMIO
    HAVE_VFORK
    HPUX
    IBMRTAIX
    IBMR2AIX
    IRIS
    ISTRIP
    IUCLC
    LDISC1
    MSDOS
    NLDLY
    OLCUC
    POSIX_SIGNALS
    RTU
    SETUP_SLAVE_PTY
    SET_CHILD_PTY_PGRP
    SIGCHLD
    SIGNALS_VIA_CHARACTERS
    SKTPAIR
    STRIDE
    subprocess
    TIOCNOTTY
    TIOCSCTTY
    TIOCSETD
    WINDOWSNT
    __sgi
    pfa
 */

/* known macros
   USG
   UNIPLUS
   BSD_SYSTEM
   PTY_ITERATION
   PTY_OPEN
   PTY_NAME_SPRINTF
   FIRST_PTY_LETTER
   PTY_TTY_NAME_SPRINTF
   HAVE_PTYS
   EMACS_GET_TTY
   O_NDELAY
   O_NOCTTY
   O_NONBLOCK
 */


/*******************************
 **  These macros and constants are defined for maximum compatibility with
 **  Emacs, so that we can easily compare later on the changes done in Emacs
 **  and the ones in this file
 *******************************/

/* Should use ptys or pipes to communicate with the processes ? */
int Vprocess_connection_type = 1;

#define NILP(x) ((x) == 0)
#define Qnil 0
#define Qt 1
#define Qrun 2
#define report_file_error(x, y) fprintf (stderr, x);
#define XPROCESS(x) (x)
#define XSETINT(x,y) (x)=(y)
#define XSETFASTINT(x,y) (x)=(y)
#define BLOCK_INPUT {}
#define emacs_write(fd,str,len) write(fd,str,len)
#undef SET_EMACS_PRIORITY
#define STRING_BYTES(x) strlen(x)
#define fatal(a, b) fprintf(stderr, a), exit(1)


/* Include the system-specific definitions */
#include SYSTEM_INCLUDE

#define P_(X) ()
#define RETSIGTYPE void

#include "systty.h"
#include <signal.h>
#include "syssignal.h"
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_STROPTS_H
#include <sys/stropts.h>
#endif


#if defined(BSD_SYSTEM) || defined(STRIDE)
#include <sys/ioctl.h>
#if !defined (O_NDELAY) && defined (HAVE_PTYS) && !defined(USG5)
#include <fcntl.h>
#endif /* HAVE_PTYS and no O_NDELAY */
#endif /* BSD_SYSTEM || STRIDE */


#ifdef HAVE_PTYS
/* The file name of the pty opened by allocate_pty.  */

static char pty_name[24];
#endif




struct Lisp_Process {
  int infd;      /* descriptor to read from the process */
  int outfd;     /* descriptor by which we write to the process */
  int subtty;    /* descriptor for the tty that the process is using */
  int pty_flag;  /* non-nil if communicating through a pty */
  int status;    /* Symbol indicating status of the process:
		    Qrun, Qopen, Qclosed */
  int pid;       /* Number of this process */

  sigset_t procmask;
  int forkin, forkout;
};




#ifdef _WIN32

void 
register_child (int pid, int fd)
{
  /* see w32proc.c in Emacs's sources */
}

/* The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are *not* inheritable, we make them
   stdin, stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the std handles to inheritable duplicates of the ones being passed in.
     (Note that _get_osfhandle() is an io.h procedure that retrieves the
     NT file handle for a crt file descriptor.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Close the std handles passed to the child.
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

void
prepare_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  HANDLE parent;
  HANDLE newstdin, newstdout, newstderr;

  parent = GetCurrentProcess ();

  handles[0] = GetStdHandle (STD_INPUT_HANDLE);
  handles[1] = GetStdHandle (STD_OUTPUT_HANDLE);
  handles[2] = GetStdHandle (STD_ERROR_HANDLE);

  /* make inheritable copies of the new handles */
  if (!DuplicateHandle (parent, 
		       (HANDLE) _get_osfhandle (in),
		       parent,
		       &newstdin, 
		       0, 
		       TRUE, 
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating input handle for child", Qnil);
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (out),
		       parent,
		       &newstdout,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating output handle for child", Qnil);
  
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (err),
		       parent,
		       &newstderr,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating error handle for child", Qnil);

  /* and store them as our std handles */
  if (!SetStdHandle (STD_INPUT_HANDLE, newstdin))
    report_file_error ("Changing stdin handle", Qnil);
  
  if (!SetStdHandle (STD_OUTPUT_HANDLE, newstdout))
    report_file_error ("Changing stdout handle", Qnil);

  if (!SetStdHandle (STD_ERROR_HANDLE, newstderr))
    report_file_error ("Changing stderr handle", Qnil);
}

void
reset_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  /* close the duplicated handles passed to the child */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  /* now restore parent's saved std handles */
  SetStdHandle (STD_INPUT_HANDLE, handles[0]);
  SetStdHandle (STD_OUTPUT_HANDLE, handles[1]);
  SetStdHandle (STD_ERROR_HANDLE, handles[2]);
}


#else  /* _WIN32 */

/******************************************************
 **  emacs_open ()
 **
 ******************************************************/

int
emacs_open (char* path, int oflag, int mode)
{
  register int rtnval;

#ifdef BSD4_1
  if (oflag & O_CREAT) 
    return creat (path, mode);
#endif
  
  while ((rtnval = open (path, oflag, mode)) == -1
         && (errno == EINTR));
  return (rtnval);
}

/******************************************************
 **  emacs_close ()
 **
 ******************************************************/

int
emacs_close (fd)
     int fd;
{
  int did_retry = 0;
  register int rtnval;

  while ((rtnval = close (fd)) == -1
         && (errno == EINTR))
    did_retry = 1;

  /* If close is interrupted SunOS 4.1 may or may not have closed the
     file descriptor.  If it did the second close will fail with
     errno = EBADF.  That means we have succeeded.  */
  if (rtnval == -1 && did_retry && errno == EBADF)
    return 0;

  return rtnval;
}

/*******************************************************
 **  relocate_fd ()
 **
 **  Move the file descriptor FD so that its number is not less than MINFD.
 **  If the file descriptor is moved at all, the original is freed.
 *******************************************************/

static int
relocate_fd (fd, minfd)
     int fd, minfd;
{
  if (fd >= minfd)
    return fd;
  else
    {
      int new = dup (fd);
      if (new == -1)
	{
	  char *message1 = "Error while setting up child: ";
	  char *errmessage = strerror (errno);
	  char *message2 = "\n";
	  emacs_write (2, message1, strlen (message1));
	  emacs_write (2, errmessage, strlen (errmessage));
	  emacs_write (2, message2, strlen (message2));
	  _exit (1);
	}
      /* Note that we hold the original FD open while we recurse,
	 to guarantee we'll get a new FD if we need it.  */
      new = relocate_fd (new, minfd);
      emacs_close (fd);
      return new;
    }
}


/********************************************************************
 **  emacs_set_tty ()
 **
 **  Set the parameters of the tty on FD according to the contents of
 **  *SETTINGS.  If FLUSHP is non-zero, we discard input.
 **  Return 0 if all went well, and -1 if anything failed.
 ********************************************************************/

int
emacs_set_tty (fd, settings, flushp)
     int fd;
     struct emacs_tty *settings;
     int flushp;
{
  /* Set the primary parameters - baud rate, character size, etcetera.  */
#ifdef HAVE_TCATTR
  int i;
  /* We have those nifty POSIX tcmumbleattr functions.
     William J. Smith <wjs@wiis.wang.com> writes:
     "POSIX 1003.1 defines tcsetattr to return success if it was
     able to perform any of the requested actions, even if some
     of the requested actions could not be performed.
     We must read settings back to ensure tty setup properly.
     AIX requires this to keep tty from hanging occasionally."  */
  /* This make sure that we don't loop indefinitely in here.  */
  for (i = 0 ; i < 10 ; i++)
    if (tcsetattr (fd, flushp ? TCSAFLUSH : TCSADRAIN, &settings->main) < 0)
      {
	if (errno == EINTR)
	  continue;
	else
	  return -1;
      }
    else
      {
	struct termios new;

	bzero (&new, sizeof (new));
	/* Get the current settings, and see if they're what we asked for.  */
	tcgetattr (fd, &new);
	/* We cannot use memcmp on the whole structure here because under
	 * aix386 the termios structure has some reserved field that may
	 * not be filled in.
	 */
	if (   new.c_iflag == settings->main.c_iflag
	    && new.c_oflag == settings->main.c_oflag
	    && new.c_cflag == settings->main.c_cflag
	    && new.c_lflag == settings->main.c_lflag
	    && memcmp (new.c_cc, settings->main.c_cc, NCCS) == 0)
	  break;
	else
	  continue;
      }

#else
#ifdef HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, flushp ? TCSETAF : TCSETAW, &settings->main) < 0)
    return -1;

#else
#ifdef VMS
  /* Vehemently Monstrous System?  :-)  */
  if (! (SYS$QIOW (0, fd, IO$_SETMODE, &input_iosb, 0, 0,
		   &settings->main.class, 12, 0, 0, 0, 0)
	 & 1))
    return -1;

#else
#ifndef DOS_NT
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, (flushp) ? TIOCSETP : TIOCSETN, &settings->main) < 0)
    return -1;
#endif /* not DOS_NT */

#endif
#endif
#endif

  /* Suivant - Do we have to get struct ltchars data?  */
#ifdef HAVE_LTCHARS
  if (ioctl (fd, TIOCSLTC, &settings->ltchars) < 0)
    return -1;
#endif

  /* How about a struct tchars and a wordful of lmode bits?  */
#ifdef HAVE_TCHARS
  if (ioctl (fd, TIOCSETC, &settings->tchars) < 0
      || ioctl (fd, TIOCLSET, &settings->lmode) < 0)
    return -1;
#endif
  
  /* We have survived the tempest.  */
  return 0;
}

/****************************************************************
 **  emacs_get_tty ()
 **
 **  Set *TC to the parameters associated with the terminal FD.
 **  Return zero if all's well, or -1 if we ran into an error we
 **  couldn't deal with.
 ****************************************************************/

int
emacs_get_tty (fd, settings)
     int fd;
     struct emacs_tty *settings;
{
  /* Retrieve the primary parameters - baud rate, character size, etcetera.  */
#ifdef HAVE_TCATTR
  /* We have those nifty POSIX tcmumbleattr functions.  */
  bzero (&settings->main, sizeof (settings->main));
  if (tcgetattr (fd, &settings->main) < 0)
    return -1;

#else
#ifdef HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, TCGETA, &settings->main) < 0)
    return -1;

#else
#ifdef VMS
  /* Vehemently Monstrous System?  :-)  */
  if (! (SYS$QIOW (0, fd, IO$_SENSEMODE, settings, 0, 0,
		   &settings->main.class, 12, 0, 0, 0, 0)
	 & 1))
    return -1;

#else
#ifndef DOS_NT
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, TIOCGETP, &settings->main) < 0)
    return -1;
#endif /* not DOS_NT */
#endif
#endif
#endif

  /* Suivant - Do we have to get struct ltchars data?  */
#ifdef HAVE_LTCHARS
  if (ioctl (fd, TIOCGLTC, &settings->ltchars) < 0)
    return -1;
#endif

  /* How about a struct tchars and a wordful of lmode bits?  */
#ifdef HAVE_TCHARS
  if (ioctl (fd, TIOCGETC, &settings->tchars) < 0
      || ioctl (fd, TIOCLGET, &settings->lmode) < 0)
    return -1;
#endif

  /* We have survived the tempest.  */
  return 0;
}

/***********************************************************
 **  sys_sigunblock ()
 **
 ***********************************************************/

sigset_t
sys_sigunblock (sigset_t new_mask)
{
  sigset_t old_mask;
  sigprocmask (SIG_UNBLOCK, (sigset_t*)&new_mask, &old_mask);
  return (old_mask);
}

/************************************************************
 **  sys_sigblock ()
 **
 ************************************************************/

sigset_t
sys_sigblock (sigset_t new_mask)
{
  sigset_t old_mask;
  sigprocmask (SIG_BLOCK, &new_mask, &old_mask);
  return (old_mask);
}

/*************************************************************
 **  sys_signal ()
 **
 *************************************************************/

#ifdef POSIX_SIGNALS

signal_handler_t
sys_signal (int signal_number, signal_handler_t action)
{
  struct sigaction new_action, old_action;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_handler = action;
#ifdef SA_RESTART
  /* Emacs mostly works better with restartable system services. If this
   * flag exists, we probably want to turn it on here.
   */
  new_action.sa_flags = SA_RESTART;
#else
  new_action.sa_flags = 0;
#endif /* SA_RESTART */
  sigaction (signal_number, &new_action, &old_action);
  return (old_action.sa_handler);
}

#endif  /* POSIX_SIGNALS */

/***********************************************************
 **  setup_pty ()
 **
 ** Set up the proper status flags for use of a pty.
 ***********************************************************/

#ifdef HAVE_PTYS

void
setup_pty (fd)
     int fd;
{
  /* I'm told that TOICREMOTE does not mean control chars
     "can't be sent" but rather that they don't have
     input-editing or signaling effects.
     That should be good, because we have other ways
     to do those things in Emacs.
     However, telnet mode seems not to work on 4.2.
     So TIOCREMOTE is turned off now. */

  /* Under hp-ux, if TIOCREMOTE is turned on, some calls
     will hang.  In particular, the "timeout" feature (which
     causes a read to return if there is no data available)
     does this.  Also it is known that telnet mode will hang
     in such a way that Emacs must be stopped (perhaps this
     is the same problem).
     
     If TIOCREMOTE is turned off, then there is a bug in
     hp-ux which sometimes loses data.  Apparently the
     code which blocks the master process when the internal
     buffer fills up does not work.  Other than this,
     though, everything else seems to work fine.
     
     Since the latter lossage is more benign, we may as well
     lose that way.  -- cph */
#ifdef FIONBIO
#if defined(SYSV_PTYS) || defined(UNIX98_PTYS)
  {
    int on = 1;
    ioctl (fd, FIONBIO, &on);
  }
#endif
#endif
#ifdef IBMRTAIX
  /* On AIX, the parent gets SIGHUP when a pty attached child dies.  So, we */
  /* ignore SIGHUP once we've started a child on a pty.  Note that this may */
  /* cause EMACS not to die when it should, i.e., when its own controlling  */
  /* tty goes away.  I've complained to the AIX developers, and they may    */
  /* change this behavior, but I'm not going to hold my breath.             */
  signal (SIGHUP, SIG_IGN);
#endif
}
#endif /* HAVE_PTYS */


/*********************************************************
 **  close_process_descs ()
 **
 **  Close all descriptors currently in use for communication
 **  with subprocess.  This is used in a newly-forked subprocess
 **  to get rid of irrelevant descriptors.
 *********************************************************/

void
close_process_descs ()
{
  /*  This is in fact an Emacs-specific function, which we don't
      really care about in gvd's context at this point.
      In fact, we might need to have something similar, but this
      could be implemented directly in Ada
  */
}

void
close_load_descs () {
  /* This is also Emacs specific */
}

/*********************************************************
 **  allocate_pty ()
 **  Open an available pty, returning a file descriptor.
 **  Return -1 on failure.
 **  The file name of the terminal corresponding to the pty
 **  is left in the variable pty_name.
 **
 *********************************************************/

#ifdef HAVE_PTYS

int
allocate_pty ()
{
  struct stat stb;
  register int c, i;
  int fd;

  /* Some systems name their pseudoterminals so that there are gaps in
     the usual sequence - for example, on HP9000/S700 systems, there
     are no pseudoterminals with names ending in 'f'.  So we wait for
     three failures in a row before deciding that we've reached the
     end of the ptys.  */
  int failed_count = 0;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
#ifdef IRIS
	/* Unusual IRIS code */
 	*ptyv = emacs_open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
 	if (fd < 0)
 	  return -1;
	if (fstat (fd, &stb) < 0)
	  return -1;
#else /* not IRIS */
	if (stat (pty_name, &stb) < 0)
	  {
	    failed_count++;
	    if (failed_count >= 3)
	      return -1;
	  }
	else
	  failed_count = 0;
#ifdef O_NONBLOCK
	fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#else
	fd = emacs_open (pty_name, O_RDWR | O_NDELAY, 0);
#endif
#endif /* not IRIS */
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
            sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* no PTY_TTY_NAME_SPRINTF */
#ifndef UNIPLUS
	    if (access (pty_name, 6) != 0)
	      {
		emacs_close (fd);
#if !defined(IRIS) && !defined(__sgi)
		fprintf (stderr, "Could not access pty_name --%s--\n",
			 pty_name);
		continue;
#else
		return -1;
#endif /* IRIS */
	      }
#endif /* not UNIPLUS */
	    setup_pty (fd);
	    return fd;
	  }
      }
  fprintf (stderr, "return -1 from allocate_tty\n");
  return -1;
}
#endif /* HAVE_PTYS */



/**********************************************************
 **  child_setup_tty ()
 **
 **  Set up the terminal at the other end of a pseudo-terminal that
 **  we will be controlling an inferior through.
 **  It should not echo or do line-editing, since that is done
 **  in Emacs.  No padding needed for insertion into an Emacs buffer.
 **
 ***********************************************************/

void
child_setup_tty (out)
     int out;
{
#ifndef DOS_NT
  struct emacs_tty s;

  EMACS_GET_TTY (out, &s);

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  s.main.c_oflag |= OPOST;	/* Enable output postprocessing */
  s.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */
#ifdef NLDLY
  s.main.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
  				/* No output delays */
#endif
  s.main.c_lflag &= ~ECHO;	/* Disable echo */
  s.main.c_lflag |= ISIG;	/* Enable signals */
#ifdef IUCLC
  s.main.c_iflag &= ~IUCLC;	/* Disable downcasing on input.  */
#endif
#ifdef ISTRIP
  s.main.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
#ifdef OLCUC
  s.main.c_oflag &= ~OLCUC;	/* Disable upcasing on output.  */
#endif
  /*s.main.c_oflag &= ~TAB3;	/* Disable tab expansion */
  s.main.c_cflag = (s.main.c_cflag & ~CSIZE) | CS8; /* Don't strip 8th bit */
#if 0
  /* Said to be unnecessary:  */
  s.main.c_cc[VMIN] = 1;	/* minimum number of characters to accept  */
  s.main.c_cc[VTIME] = 0;	/* wait forever for at least 1 character  */
#endif

  s.main.c_lflag |= ICANON;	/* Enable erase/kill and eof processing */
  s.main.c_cc[VEOF] = 04;	/* insure that EOF is Control-D */
  s.main.c_cc[VERASE] = CDISABLE;	/* disable erase processing */
  s.main.c_cc[VKILL] = CDISABLE;	/* disable kill processing */
  

#ifdef HPUX
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* HPUX */

#ifdef AIX
/* AIX enhanced edit loses NULs, so disable it */
#ifndef IBMR2AIX
  s.main.c_line = 0;
  s.main.c_iflag &= ~ASCEDIT;
#endif
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.  */
  s.main.c_iflag &= ~IGNBRK;
  s.main.c_iflag &= ~BRKINT;
  /* QUIT and INTR work better as signals, so disable character forms */
  s.main.c_cc[VINTR] = 0377;
#ifdef SIGNALS_VIA_CHARACTERS
  /* the QUIT and INTR character are used in process_send_signal
     so set them here to something useful.  */
  if (s.main.c_cc[VQUIT] == 0377)
    s.main.c_cc[VQUIT] = '\\'&037;	/* Control-\ */
  if (s.main.c_cc[VINTR] == 0377)
    s.main.c_cc[VINTR] = 'C'&037;	/* Control-C */
#else /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  /* QUIT and INTR work better as signals, so disable character forms */
  s.main.c_cc[VQUIT] = 0377;
  s.main.c_cc[VINTR] = 0377;
  s.main.c_lflag &= ~ISIG;
#endif /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  s.main.c_cc[VEOL] = 0377;
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* AIX */

#else /* not HAVE_TERMIO */

  s.main.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE
		       | CBREAK | TANDEM);
  s.main.sg_flags |= LPASS8;
  s.main.sg_erase = 0377;
  s.main.sg_kill = 0377;
  s.lmode = LLITOUT | s.lmode;        /* Don't strip 8th bit */

#endif /* not HAVE_TERMIO */

  EMACS_SET_TTY (out, &s, 0);

#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */
#ifdef RTU
  {
    int zero = 0;
    ioctl (out, FIOASYNC, &zero);
  }
#endif /* RTU */
#endif /* not DOS_NT */
}



/************************************************************
 **  child_setup ()
 **
 **  This is the last thing run in a newly forked inferior
 **  either synchronous or asynchronous.
 **  Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
 **  Initialize inferior's priority, pgrp, connected dir and environment.
 **  then exec another program based on new_argv.
 **
 **  This function may change environ for the superior process.
 **  Therefore, the superior process must save and restore the value
 **  of environ around the vfork and the call to this function.
 **
 **  SET_PGRP is nonzero if we should put the subprocess into a separate
 **  process group.  
 **
 **  CURRENT_DIR is an elisp string giving the path of the current
 **  directory the subprocess should have.  Since we can't really signal
 **  a decent error from within the child, this should be verified as an
 **  executable directory by the parent.
 **
 **************************************************************/

int
child_setup (in, out, err, new_argv, set_pgrp, current_dir)
     int in, out, err;
     register char **new_argv;
     int set_pgrp;
     char* current_dir;
{
  char **env;
  char *pwd_var;
#ifdef WINDOWSNT
  int cpid;
  HANDLE handles[3];
#endif /* WINDOWSNT */

  int pid = getpid ();

#ifdef SET_EMACS_PRIORITY
  {
    extern int emacs_priority;

    if (emacs_priority < 0)
      nice (- emacs_priority);
  }
#endif

#ifdef subprocesses
  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();
#endif
  /* DOS_NT isn't in a vfork, so if we are in the middle of load-file,
     we will lose if we call close_load_descs here.  */
#ifndef DOS_NT
  close_load_descs ();
#endif

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA it is safe because that changes
     the superior's static variables as if the superior had done alloca
     and will be cleaned up in the usual way.  */
#if 0   /* ??? MANU: Do not process the current directory at this point */
  {
    register char *temp;
    register int i;

    i = STRING_BYTES (XSTRING (current_dir));
    pwd_var = (char *) alloca (i + 6);
    temp = pwd_var + 4;
    bcopy ("PWD=", pwd_var, 4);
    bcopy (XSTRING (current_dir)->data, temp, i);
    if (!IS_DIRECTORY_SEP (temp[i - 1])) temp[i++] = DIRECTORY_SEP;
    temp[i] = 0;

#ifndef DOS_NT
    /* We can't signal an Elisp error here; we're in a vfork.  Since
       the callers check the current directory before forking, this
       should only return an error if the directory's permissions
       are changed between the check and this chdir, but we should
       at least check.  */
    if (chdir (temp) < 0)
      _exit (errno);
#endif

#ifdef DOS_NT
    /* Get past the drive letter, so that d:/ is left alone.  */
    if (i > 2 && IS_DEVICE_SEP (temp[1]) && IS_DIRECTORY_SEP (temp[2]))
      {
	temp += 2;
	i -= 2;
      }
#endif

    /* Strip trailing slashes for PWD, but leave "/" and "//" alone.  */
    while (i > 2 && IS_DIRECTORY_SEP (temp[i - 1]))
      temp[--i] = 0;
  }
#endif /* 0,  MANU */
  
  /* Set `env' to a vector of the strings in Vprocess_environment.  */
#if 0  /*  ???  MANU: don't understand this code */
  {
    register Lisp_Object tem;
    register char **new_env;
    register int new_length;

    new_length = 0;
    for (tem = Vprocess_environment;
	 CONSP (tem) && STRINGP (XCAR (tem));
	 tem = XCDR (tem))
      new_length++;

    /* new_length + 2 to include PWD and terminating 0.  */
    env = new_env = (char **) alloca ((new_length + 2) * sizeof (char *));

    /* If we have a PWD envvar, pass one down,
       but with corrected value.  */
    if (getenv ("PWD"))
      *new_env++ = pwd_var;

    /* Copy the Vprocess_environment strings into new_env.  */
    for (tem = Vprocess_environment;
	 CONSP (tem) && STRINGP (XCAR (tem));
	 tem = XCDR (tem))
      {
	char **ep = env;
	char *string = (char *) XSTRING (XCAR (tem))->data;
	/* See if this string duplicates any string already in the env.
	   If so, don't put it in.
	   When an env var has multiple definitions,
	   we keep the definition that comes first in process-environment.  */
	for (; ep != new_env; ep++)
	  {
	    char *p = *ep, *q = string;
	    while (1)
	      {
		if (*q == 0)
		  /* The string is malformed; might as well drop it.  */
		  goto duplicate;
		if (*q != *p)
		  break;
		if (*q == '=')
		  goto duplicate;
		p++, q++;
	      }
	  }
	*new_env++ = string;
      duplicate: ;
      }
    *new_env = 0;
  }
#endif  /* 0  MANU */
  
#ifdef WINDOWSNT
  prepare_standard_handles (in, out, err, handles);
  /* ??? MANU  set_process_dir (XSTRING (current_dir)->data);*/
  
#else  /* not WINDOWSNT */
  /* Make sure that in, out, and err are not actually already in
     descriptors zero, one, or two; this could happen if Emacs is
     started with its standard in, out, or error closed, as might
     happen under X.  */
  {
    int oin = in, oout = out;

    /* We have to avoid relocating the same descriptor twice!  */

    in = relocate_fd (in, 3);

    if (out == oin)
      out = in;
    else
      out = relocate_fd (out, 3);

    if (err == oin)
      err = in;
    else if (err == oout)
      err = out;
    else
      err = relocate_fd (err, 3);
  }

#ifndef MSDOS
  emacs_close (0);
  emacs_close (1);
  emacs_close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  emacs_close (in);
  emacs_close (out);
  emacs_close (err);
#endif /* not MSDOS */
#endif /* not WINDOWSNT */

#if defined(USG) && !defined(BSD_PGRPS)
#ifndef SETPGRP_RELEASES_CTTY
  setpgrp ();			/* No arguments but equivalent in this case */
#endif
#else
  setpgid (pid, pid);   /* ??? MANU: This was setpgrp in Emacs */
#endif /* USG */
  /* setpgrp_of_tty is incorrect here; it uses input_fd.  */
  EMACS_SET_TTY_PGRP (0, &pid);

#ifdef vipc
  something missing here;
#endif /* vipc */

#ifdef MSDOS
  pid = run_msdos_command (new_argv, pwd_var + 4, in, out, err, env);
  if (pid == -1)
    /* An error occurred while trying to run the subprocess.  */
    report_file_error ("Spawning child process", Qnil);
  return pid;
#else  /* not MSDOS */
#ifdef WINDOWSNT
  /* Spawn the child.  (See ntproc.c:Spawnve).  */
  cpid = spawnve (_P_NOWAIT, new_argv[0], new_argv, env);
  reset_standard_handles (in, out, err, handles);
  if (cpid == -1)
    /* An error occurred while trying to spawn the process.  */
    report_file_error ("Spawning child process", Qnil);
  return cpid;
#else /* not WINDOWSNT */
  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  /* ??? MANU   environ = env; */
  execvp (new_argv[0], new_argv);

  emacs_write (1, "Can't exec program: ", 20);
  emacs_write (1, new_argv[0], strlen (new_argv[0]));
  emacs_write (1, "\n", 1);
  _exit (1);
#endif /* not WINDOWSNT */
#endif /* not MSDOS */
}


/********************************
 **  setupCommunication ()
 ********************************/

int
setupCommunication (struct Lisp_Process** process_out)  /* output parameter */
{
  struct Lisp_Process* process;
  int pid, inchannel, outchannel;
  int sv[2];
#ifdef POSIX_SIGNALS
  sigset_t blocked;
  struct sigaction sigint_action;
  struct sigaction sigquit_action;
#ifdef AIX
  struct sigaction sighup_action;
#endif /* AIX */
#else /* !POSIX_SIGNALS */
#if 0
#ifdef SIGCHLD
  SIGTYPE (*sigchld)();
#endif /* SIGCHLD */
#endif /* 0 */
#endif /* !POSIX_SIGNALS */
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  volatile int forkin, forkout;
  volatile int pty_flag = 0;


  process = (struct Lisp_Process*)malloc (sizeof (struct Lisp_Process));
  *process_out = process;

  inchannel = outchannel = -1;

#ifdef HAVE_PTYS
  if (!NILP (Vprocess_connection_type))
    outchannel = inchannel = allocate_pty ();

  if (inchannel >= 0)
    {
#ifndef USG 
      /* On USG systems it does not work to open the pty's tty here
	       and then close and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else  /* O_NOCTTY */
      forkout = forkin = emacs_open (pty_name, O_RDWR, 0);
#endif /* O_NOCTTY */
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
#else /* not USG */
      forkin = forkout = -1;
#endif /* not USG */
      pty_flag = 1;
    }
  else
#endif /* HAVE_PTYS */
#ifdef SKTPAIR
    {
      if (socketpair (AF_UNIX, SOCK_STREAM, 0, sv) < 0)
	report_file_error ("Opening socketpair", Qnil);
      outchannel = inchannel = sv[0];
      forkout = forkin = sv[1];
    }
#else /* not SKTPAIR */
    {
      int tem;
      tem = pipe (sv);
      if (tem < 0)
	report_file_error ("Creating pipe", Qnil);
      inchannel = sv[0];
      forkout = sv[1];
      tem = pipe (sv);
      if (tem < 0)
	{
	  emacs_close (inchannel);
	  emacs_close (forkout);
	  report_file_error ("Creating pipe", Qnil);
	}
      outchannel = sv[1];
      forkin = sv[0];
    }
#endif /* not SKTPAIR */

#if 0
  /* Replaced by close_process_descs */
  set_exclusive_use (inchannel);
  set_exclusive_use (outchannel);
#endif

/* Stride people say it's a mystery why this is needed
   as well as the O_NDELAY, but that it fails without this.  */
#if defined (STRIDE) || (defined (pfa) && defined (HAVE_PTYS))
  {
    int one = 1;
    ioctl (inchannel, FIONBIO, &one);
  }
#endif /* STRIDE */

#ifdef O_NONBLOCK
  fcntl (inchannel, F_SETFL, O_NONBLOCK);
  fcntl (outchannel, F_SETFL, O_NONBLOCK);
#else /* O_NONBLOCK */
#ifdef O_NDELAY
  fcntl (inchannel, F_SETFL, O_NDELAY);
  fcntl (outchannel, F_SETFL, O_NDELAY);
#endif /* O_NDELAY */
#endif /* O_NONBLOCK */

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  XSETINT (XPROCESS (process)->infd, inchannel);
  XSETINT (XPROCESS (process)->outfd, outchannel);
  /* Record the tty descriptor used in the subprocess.  */
  if (forkin < 0)
    XPROCESS (process)->subtty = Qnil;
  else
    XSETFASTINT (XPROCESS (process)->subtty, forkin);
  XPROCESS (process)->pty_flag = (pty_flag ? Qt : Qnil);
  XPROCESS (process)->status = Qrun;

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
#ifdef POSIX_SIGNALS
  sigemptyset (&blocked);
#ifdef SIGCHLD
  sigaddset (&blocked, SIGCHLD);
#endif /* SIGCHLD */
#ifdef HAVE_VFORK
  /* On many hosts (e.g. Solaris 2.4), if a vforked child calls `signal',
     this sets the parent's signal handlers as well as the child's.
     So delay all interrupts whose handlers the child might munge,
     and record the current handlers so they can be restored later.  */
  sigaddset (&blocked, SIGINT );  sigaction (SIGINT , 0, &sigint_action );
  sigaddset (&blocked, SIGQUIT);  sigaction (SIGQUIT, 0, &sigquit_action);
#ifdef AIX
  sigaddset (&blocked, SIGHUP );  sigaction (SIGHUP , 0, &sighup_action );
#endif /* AIX */
#endif /* HAVE_VFORK */
  sigprocmask (SIG_BLOCK, &blocked, &(process->procmask));
#else /* !POSIX_SIGNALS */
#ifdef SIGCHLD
#ifdef BSD4_1
  sighold (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD_SYSTEM) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (sigmask (SIGCHLD));
#else /* ordinary USG */
#if 0
  sigchld_deferred = 0;
  sigchld = signal (SIGCHLD, create_process_sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

  /* ??? MANU: This is Emacs specific, right ? 
     FD_SET (inchannel, &input_wait_mask);
     FD_SET (inchannel, &non_keyboard_wait_mask);
     if (inchannel > max_process_desc)
     max_process_desc = inchannel;
  */

  /* Until we store the proper pid, enable sigchld_handler
     to recognize an unknown pid as standing for this process.
     It is very important not to let this `marker' value stay
     in the table after this function has returned; if it does
     it might cause call-process to hang and subsequent asynchronous
     processes to get their return values scrambled.  */
  XSETINT (XPROCESS (process)->pid, -1);

  BLOCK_INPUT;
  
  /* child_setup must clobber environ on systems with true vfork.
     Protect it from permanent change.
     
     process->save_environ = environ; */
  process->forkin = forkin;
  process->forkout = forkout;
}

/***************************************************************
 ** setupChildCommunication ()
 **
 ***************************************************************/

int
setupChildCommunication (struct Lisp_Process* process, char** new_argv)
{
  char* current_dir = ".";
  int pid = 0;
  
  int xforkin = process->forkin;
  int xforkout = process->forkout;

#if 0 /* This was probably a mistake--it duplicates code later on,
	 but fails to handle all the cases.  */
  /* Make sure SIGCHLD is not blocked in the child.  */
  sigsetmask (SIGEMPTYMASK);
#endif

  /* Make the pty be the controlling terminal of the process.  */
#ifdef HAVE_PTYS
  /* First, disconnect its current controlling terminal.  */
#ifdef HAVE_SETSID
  /* We tried doing setsid only if pty_flag, but it caused
     process_set_signal to fail on SGI when using a pipe.  */
  setsid ();
  /* Make the pty's terminal the controlling terminal.  */
  if (process->pty_flag == Qt)
    {
#ifdef TIOCSCTTY
      /* We ignore the return value
	 because faith@cs.unc.edu says that is necessary on Linux.  */
      ioctl (xforkin, TIOCSCTTY, 0);
#endif /* TIOCSCTTY */
    }
#else /* not HAVE_SETSID */
#ifdef USG
  /* It's very important to call setpgrp here and no time
     afterwards.  Otherwise, we lose our controlling tty which
     is set when we open the pty. */
  setpgrp ();
#endif /* USG */
#endif /* not HAVE_SETSID */
#if defined (HAVE_TERMIOS) && defined (LDISC1)
  if (process->pty_flag == Qt && xforkin >= 0)
    {
      struct termios t;
      tcgetattr (xforkin, &t);
      t.c_lflag = LDISC1;
      if (tcsetattr (xforkin, TCSANOW, &t) < 0)
	emacs_write (1, "create_process/tcsetattr LDISC1 failed\n", 39);
    }
#else
#if defined (NTTYDISC) && defined (TIOCSETD)
  if (process->pty_flag == Qt && xforkin >= 0)
    {
      /* Use new line discipline.  */
      int ldisc = NTTYDISC;
      ioctl (xforkin, TIOCSETD, &ldisc);
    }
#endif
#endif
#ifdef TIOCNOTTY 
  /* In 4.3BSD, the TIOCSPGRP bug has been fixed, and now you
     can do TIOCSPGRP only to the process's controlling tty.  */
  if (process->pty_flag == Qt)
    {
      /* I wonder: would just ioctl (0, TIOCNOTTY, 0) work here? 
	 I can't test it since I don't have 4.3.  */
      int j = emacs_open ("/dev/tty", O_RDWR, 0);
      ioctl (j, TIOCNOTTY, 0);
      emacs_close (j);
#ifndef USG
      /* In order to get a controlling terminal on some versions
	 of BSD, it is necessary to put the process in pgrp 0
	 before it opens the terminal.  */
#ifdef HAVE_SETPGID
      setpgid (0, 0);
#else
      setpgrp (0, 0);
#endif
#endif
    }
#endif /* TIOCNOTTY */
  
#if !defined (RTU) && !defined (UNIPLUS) && !defined (DONT_REOPEN_PTY)
/*** There is a suggestion that this ought to be a
     conditional on TIOCSPGRP,
     or !(defined (HAVE_SETSID) && defined (TIOCSCTTY)).
     Trying the latter gave the wrong results on Debian GNU/Linux 1.1;
     that system does seem to need this code, even though
     both HAVE_SETSID and TIOCSCTTY are defined.  */
  /* Now close the pty (if we had it open) and reopen it.
     This makes the pty the controlling terminal of the subprocess.  */
  if (process->pty_flag == Qt)
    {
#ifdef SET_CHILD_PTY_PGRP
      int pgrp = getpid ();
#endif
      
      /* I wonder if emacs_close (emacs_open (pty_name, ...))
	 would work?  */
      if (xforkin >= 0)
	emacs_close (xforkin);
      xforkout = xforkin = emacs_open (pty_name, O_RDWR, 0);
      
      if (xforkin < 0)
	{
	  emacs_write (1, "Couldn't open the pty terminal ", 31);
	  emacs_write (1, pty_name, strlen (pty_name));
	  emacs_write (1, "\n", 1);
	  _exit (1);
	}
      
#ifdef SET_CHILD_PTY_PGRP
      ioctl (xforkin, TIOCSPGRP, &pgrp);
      ioctl (xforkout, TIOCSPGRP, &pgrp);
#endif
    }
#endif /* not UNIPLUS and not RTU and not DONT_REOPEN_PTY */

#ifdef SETUP_SLAVE_PTY
  if (process->pty_flag == Qt)
    {
      SETUP_SLAVE_PTY;
    }
#endif /* SETUP_SLAVE_PTY */
#ifdef AIX
  /* On AIX, we've disabled SIGHUP above once we start a child on a pty.
     Now reenable it in the child, so it will die when we want it to.  */
  if (process->pty_flag == Qt)
    signal (SIGHUP, SIG_DFL);
#endif
#endif /* HAVE_PTYS */
  
  signal (SIGINT, SIG_DFL);
  signal (SIGQUIT, SIG_DFL);
  
  /* Stop blocking signals in the child.  */
#ifdef POSIX_SIGNALS
  sigprocmask (SIG_SETMASK, &(process->procmask), 0);
#else /* !POSIX_SIGNALS */
#ifdef SIGCHLD
#ifdef BSD4_1
  sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD_SYSTEM) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
  signal (SIGCHLD, sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */
  
  if (process->pty_flag)
    child_setup_tty (xforkout);
#ifdef WINDOWSNT
  pid = child_setup (xforkin, xforkout, xforkout,
		     new_argv, 1, current_dir);
#else  /* not WINDOWSNT */	
  child_setup (xforkin, xforkout, xforkout,
	       new_argv, 1, current_dir);
#endif /* not WINDOWSNT */

  process->pid=pid;
  return pid;
}


/**************************************************************
 **  setupParentCommunication ()
 **
 **************************************************************/

int
setupParentCommunication
   (struct Lisp_Process* process,
    int*   in_fd,  /* output */
    int*   out_fd, /* output */
    int*   err_fd, /* output */
    int*   pid_out) /* in-out parameter */
{
  process->pid = *pid_out;
  
  /* This runs in the Emacs process.  */
  if (process->pid < 0)
    {
      if (process->forkin >= 0)
	emacs_close (process->forkin);
      if (process->forkin != process->forkout && process->forkout >= 0)
	emacs_close (process->forkout);
    }
  else
    {
#ifdef WINDOWSNT
      register_child (process->pid, inchannel);
#endif /* WINDOWSNT */

      /* If the subfork execv fails, and it exits,
	 this close hangs.  I don't know why.
	 So have an interrupt jar it loose.  */
      {
	/*  ??? MANU: this was commented out for now 
	  struct atimer *timer;
	EMACS_TIME offset;
	
	stop_polling ();
	EMACS_SET_SECS_USECS (offset, 1, 0);
	timer = start_atimer (ATIMER_RELATIVE, offset, create_process_1, 0);
	
	XPROCESS (process)->subtty = Qnil;
	if (forkin >= 0)
	  emacs_close (forkin);

	cancel_atimer (timer);
	start_polling ();
	*/
      }
      
      if (process->forkin != process->forkout && process->forkout >= 0)
	emacs_close (process->forkout);

      /*
#ifdef HAVE_PTYS
      if (pty_flag)
	XPROCESS (process)->tty_name = build_string (pty_name);
      else
#endif
	XPROCESS (process)->tty_name = Qnil;
      */
    }

  /* Restore the signal state whether vfork succeeded or not.
     (We will signal an error, below, if it failed.)  */
#ifdef POSIX_SIGNALS
#ifdef HAVE_VFORK
  /* Restore the parent's signal handlers.  */
  sigaction (SIGINT, &sigint_action, 0);
  sigaction (SIGQUIT, &sigquit_action, 0);
#ifdef AIX
  sigaction (SIGHUP, &sighup_action, 0);
#endif
#endif /* HAVE_VFORK */
  /* Stop blocking signals in the parent.  */
  sigprocmask (SIG_SETMASK, &(process->procmask), 0);
#else /* !POSIX_SIGNALS */
#ifdef SIGCHLD
#ifdef BSD4_1
  sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD_SYSTEM) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
  signal (SIGCHLD, sigchld);
  /* Now really handle any of these signals
     that came in during this function.  */
  if (sigchld_deferred)
    kill (getpid (), SIGCHLD);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

  /* Now generate the error if vfork failed.  */
  if (process->pid < 0)
    report_file_error ("Doing vfork", Qnil);

  *in_fd = process->outfd;
  *out_fd = process->infd;
  *err_fd = process->infd;
  free (process);
}

#endif /* _WIN32 */
