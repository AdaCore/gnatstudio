/* Handling of pseudo-terminals
   Adapted from process.c in GNU Emacs.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95, 96, 1998
      Free Software Foundation, Inc.
   Copyright (C) 2000, 2001 ACT-Europe.

This file is part of GVD.

GVD is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GVD is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GVD; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

*/

/* Include the system-specific definitions */
#include SYSTEM_INCLUDE

#define P_(X) X
#define RETSIGTYPE void

#include "systty.h"
#include <signal.h>
#include "syssignal.h"
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
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

#ifdef WIN32
#include <windows.h>
#endif

#ifdef HAVE_PTYS
/* The file name of the pty opened by allocate_pty.  */

static char pty_name[24];
#endif

#ifdef AIX
  /* Now define a symbol for the cpu type, if your compiler
     does not define it automatically.  */
  /* Emacs defines this in m/ibmrs6000.h, which is always included for
     the powerpc/AIX machines (but not for x86/AIX machines) */
#define IBMR2AIX
#endif

struct GVD_Process {
  int infd;      /* descriptor to read from the process */
  int outfd;     /* descriptor by which we write to the process */
  int subtty;    /* descriptor for the tty that the process is using */
  int pty_flag;  /* non-nil if communicating through a pty */
  int status;    /* Symbol indicating status of the process:
		    Qrun, Qopen, Qclosed */
  int pid;       /* Number of this process */

  sigset_t procmask;
  int forkin, forkout;
#ifdef WIN32
  PROCESS_INFORMATION procinfo;
#endif
};

#define NILP(x) ((x) == 0)
#define Qnil 0
#define Qt 1
#define Qrun 2
#define report_file_error(x, y) fprintf (stderr, "Error: "x"\n");
#define XPROCESS(x) (x)
#define XSETINT(x,y) (x)=(y)
#define XFASTINT(a) ((a) + 0)
#define XSETFASTINT(a, b) ((a) = (b))
#define BLOCK_INPUT {}
#define STRING_BYTES(x) strlen(x)
#define fatal(a, b) fprintf(stderr, a), exit(1)
#define INTEGERP(x) 1
#define XINT(x) x

/* Should use ptys or pipes to communicate with the processes ?
   0 = pipe
   1 = tty
 */
static int Vprocess_connection_type = 1;

#ifdef WIN32
#define pipe __gnat_pipe
#define HAVE_NTGUI
#define MAXPATHLEN 1024

/* Control whether create_child causes the process to inherit GVD'
   console window, or be given a new one of its own.  The default is
   0, to allow multiple DOS programs to run on Win95.  Having separate
   consoles also allows Gvd to cleanly terminate process groups.  */
static int Vw32_start_process_share_console = 0;

/* Control whether create_child cause the process to inherit GVD'
   error mode setting.  The default is 1, to minimize the possibility of
   subprocesses blocking when accessing unmounted drives.  */
static int Vw32_start_process_inherit_error_mode = 1;

/* Control whether create_child causes the process' window to be
   hidden.  The default is 0. */
static int Vw32_start_process_show_window = 0;

/* Control whether spawnve quotes arguments as necessary to ensure
   correct parsing by child process.  Because not all uses of spawnve
   are careful about constructing argv arrays, we make this behaviour
   conditional (on by default). */
static int Vw32_quote_process_args = 1;

static int
nt_spawnve (char *exe, char **argv, char *env, PROCESS_INFORMATION *procinfo)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
  SECURITY_DESCRIPTOR sec_desc;
  DWORD flags;
  char dir[ MAXPATHLEN ];
  int pid;
  char *cmdline, *parg, **targ;
  int do_quoting = 0;
  char escape_char;
  int arglen;

  /* we have to do some conjuring here to put argv and envp into the
     form CreateProcess wants...  argv needs to be a space separated/null
     terminated list of parameters, and envp is a null
     separated/double-null terminated list of parameters.

     Additionally, zero-length args and args containing whitespace or
     quote chars need to be wrapped in double quotes - for this to work,
     embedded quotes need to be escaped as well.  The aim is to ensure
     the child process reconstructs the argv array we start with
     exactly, so we treat quotes at the beginning and end of arguments
     as embedded quotes.

     Note that using backslash to escape embedded quotes requires
     additional special handling if an embedded quote is already
     preceeded by backslash, or if an arg requiring quoting ends with
     backslash.  In such cases, the run of escape characters needs to be
     doubled.  For consistency, we apply this special handling as long
     as the escape character is not quote.

     Since we have no idea how large argv and envp are likely to be we
     figure out list lengths on the fly and allocate them.  */

  if (!NILP (Vw32_quote_process_args))
    {
      do_quoting = 1;
      /* Override escape char by binding w32-quote-process-args to
	 desired character, or use t for auto-selection.  */
      if (INTEGERP (Vw32_quote_process_args))
	escape_char = XINT (Vw32_quote_process_args);
      else
	escape_char = '\\';
    }
  
  /* do argv...  */
  arglen = 0;
  targ = argv;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;
      int escape_char_run = 0;

      if (*p == 0)
	need_quotes = 1;
      for ( ; *p; p++)
	{
	  if (*p == '"')
	    {
	      /* allow for embedded quotes to be escaped */
	      arglen++;
	      need_quotes = 1;
	      /* handle the case where the embedded quote is already escaped */
	      if (escape_char_run > 0)
		{
		  /* To preserve the arg exactly, we need to double the
		     preceding escape characters (plus adding one to
		     escape the quote character itself).  */
		  arglen += escape_char_run;
		}
	    }
	  else if (*p == ' ' || *p == '\t')
	    {
	      need_quotes = 1;
	    }

	  if (*p == escape_char && escape_char != '"')
	    escape_char_run++;
	  else
	    escape_char_run = 0;
	}
      if (need_quotes)
	{
	  arglen += 2;
	  /* handle the case where the arg ends with an escape char - we
	     must not let the enclosing quote be escaped.  */
	  if (escape_char_run > 0)
	    arglen += escape_char_run;
	}
      arglen += strlen (*targ++) + 1;
    }
  cmdline = (char*)malloc (arglen + 1);
  targ = argv;
  parg = cmdline;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;

      if (*p == 0)
	need_quotes = 1;

      if (do_quoting)
	{
	  for ( ; *p; p++)
	    if (*p == ' ' || *p == '\t' || *p == '"')
	      need_quotes = 1;
	}
      if (need_quotes)
	{
	  int escape_char_run = 0;
	  char * first;
	  char * last;

	  p = *targ;
	  first = p;
	  last = p + strlen (p) - 1;
	  *parg++ = '"';
	  for ( ; *p; p++)
	    {
	      if (*p == '"')
		{
		  /* double preceding escape chars if any */
		  while (escape_char_run > 0)
		    {
		      *parg++ = escape_char;
		      escape_char_run--;
		    }
		  /* escape all quote chars, even at beginning or end */
		  *parg++ = escape_char;
		}
	      *parg++ = *p;

	      if (*p == escape_char && escape_char != '"')
		escape_char_run++;
	      else
		escape_char_run = 0;
	    }
	  /* double escape chars before enclosing quote */
	  while (escape_char_run > 0)
	    {
	      *parg++ = escape_char;
	      escape_char_run--;
	    }
	  *parg++ = '"';
	}
      else
	{
	  strcpy (parg, *targ);
	  parg += strlen (*targ);
	}
      *parg++ = ' ';
      targ++;
    }
  *--parg = '\0';

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);
  
#ifdef HAVE_NTGUI
  if (NILP (Vw32_start_process_show_window))
    start.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  else
    start.dwFlags = STARTF_USESTDHANDLES;
  start.wShowWindow = SW_HIDE;

  start.hStdInput = GetStdHandle (STD_INPUT_HANDLE);
  start.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  start.hStdError = GetStdHandle (STD_ERROR_HANDLE);
#endif /* HAVE_NTGUI */

  /* Explicitly specify no security */
  if (!InitializeSecurityDescriptor (&sec_desc, SECURITY_DESCRIPTOR_REVISION))
    goto EH_Fail;
  if (!SetSecurityDescriptorDacl (&sec_desc, TRUE, NULL, FALSE))
    goto EH_Fail;
  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = &sec_desc;
  sec_attrs.bInheritHandle = FALSE;
  
  flags = (!NILP (Vw32_start_process_share_console)
	   ? CREATE_NEW_PROCESS_GROUP
	   : CREATE_NEW_CONSOLE);
  if (NILP (Vw32_start_process_inherit_error_mode))
    flags |= CREATE_DEFAULT_ERROR_MODE;
  if (!CreateProcess (exe, cmdline, &sec_attrs, NULL, TRUE,
		      flags, env, ".", &start, procinfo))
    goto EH_Fail;

  pid = (int) procinfo->dwProcessId;

  /* Hack for Windows 95, which assigns large (ie negative) pids */
  if (pid < 0)
    pid = -pid;

  return pid;

 EH_Fail:
  return -1;
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

static void
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

static void
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

#endif /* WIN32 */

/******************************************************
 **  gvd_open ()
 **
 ******************************************************/

static int
gvd_open (char* path, int oflag, int mode)
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
 **  gvd_close ()
 **
 ******************************************************/

static int
gvd_close (fd)
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
	  write (2, message1, strlen (message1));
	  write (2, errmessage, strlen (errmessage));
	  write (2, message2, strlen (message2));
	  _exit (1);
	}
      /* Note that we hold the original FD open while we recurse,
	 to guarantee we'll get a new FD if we need it.  */
      new = relocate_fd (new, minfd);
      gvd_close (fd);
      return new;
    }
}


/********************************************************************
 **  gvd_set_tty ()
 **
 **  Set the parameters of the tty on FD according to the contents of
 **  *SETTINGS.  If FLUSHP is non-zero, we discard input.
 **  Return 0 if all went well, and -1 if anything failed.
 ********************************************************************/

static int
gvd_set_tty (fd, settings, flushp)
     int fd;
     struct gvd_tty *settings;
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
 **  gvd_get_tty ()
 **
 **  Set *TC to the parameters associated with the terminal FD.
 **  Return zero if all's well, or -1 if we ran into an error we
 **  couldn't deal with.
 ****************************************************************/

static int
gvd_get_tty (fd, settings)
     int fd;
     struct gvd_tty *settings;
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

#ifndef WIN32

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

#endif /* WIN32 */

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

static void
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
  /* cause GVD not to die when it should, i.e., when its own controlling  */
  /* tty goes away.  I've complained to the AIX developers, and they may    */
  /* change this behavior, but I'm not going to hold my breath.             */
  signal (SIGHUP, SIG_IGN);
#endif
}
#endif /* HAVE_PTYS */


/*********************************************************
 **  allocate_pty ()
 **  Open an available pty, returning a file descriptor.
 **  Return -1 on failure.
 **  The file name of the terminal corresponding to the pty
 **  is left in the variable pty_name.
 **
 *********************************************************/

#ifdef HAVE_PTYS

static int
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
 	*ptyv = gvd_open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
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
	fd = gvd_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#else
	fd = gvd_open (pty_name, O_RDWR | O_NDELAY, 0);
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
		gvd_close (fd);
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
 **  in GVD. No padding needed for insertion into a buffer.
 **
 ***********************************************************/

static void
child_setup_tty (out)
     int out;
{
#ifndef DOS_NT
  struct gvd_tty s;

  GVD_GET_TTY (out, &s);

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

  GVD_SET_TTY (out, &s, 0);

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
 **  CURRENT_DIR is a string giving the path of the current
 **  directory the subprocess should have.  Since we can't really signal
 **  a decent error from within the child, this should be verified as an
 **  executable directory by the parent.
 **
 **************************************************************/

#ifndef VMS /* VMS version is in vmcproc.c */
static int
child_setup (in, out, err, new_argv, set_pgrp, current_dir, process)
     int in, out, err;
     register char **new_argv;
     int set_pgrp;
     char *current_dir;
     struct GVD_Process *process;
{
  char **env;
  char *pwd_var;
#ifdef WINDOWSNT
  int cpid;
  HANDLE handles[3];
#endif /* WINDOWSNT */

  int pid = getpid ();

  /* ??? Original Emacs code had a section to deal with the current directory.
     This code has been removed completely. */

  /* ??? Original Emacs code had a section to deal with the setting of
     `env' to a vector of the strings in Vprocess_environment.
     This code has been removed completely. */
  
#ifdef WINDOWSNT
  prepare_standard_handles (in, out, err, handles);
  
#else  /* not WINDOWSNT */
  /* Make sure that in, out, and err are not actually already in
     descriptors zero, one, or two; this could happen if GVD is
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
  gvd_close (0);
  gvd_close (1);
  gvd_close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  gvd_close (in);
  gvd_close (out);
  gvd_close (err);
#endif /* not MSDOS */
#endif /* not WINDOWSNT */

#if defined(USG) && !defined(BSD_PGRPS)
#ifndef SETPGRP_RELEASES_CTTY
  setpgrp ();			/* No arguments but equivalent in this case */
#endif
#else
#ifdef HAVE_SETPGID
  setpgid (pid, pid);
#endif /* HAVE_SETPGID */
#endif /* USG */

  /* setpgrp_of_tty is incorrect here; it uses input_fd.  */
  GVD_SET_TTY_PGRP (0, &pid);

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
  /* Spawn the child. */
  cpid = nt_spawnve (new_argv[0], new_argv, NULL, &process->procinfo);
  if (cpid == -1)
    /* An error occurred while trying to spawn the process.  */
    report_file_error ("Spawning child process", Qnil);
  reset_standard_handles (in, out, err, handles);
  return cpid;
#else /* not WINDOWSNT */
  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  /* Disabled env handling in GVD ??? */
  execvp (new_argv[0], new_argv);

  write (1, "Can't exec program: ", 20);
  write (1, new_argv[0], strlen (new_argv[0]));
  write (1, "\n", 1);
  _exit (1);
#endif /* not WINDOWSNT */
#endif /* not MSDOS */
}
#endif /* VMS */

/********************************
 **  gvd_setup_communication ()
 ********************************/

int
gvd_setup_communication (struct GVD_Process** process_out) /* output param */
{
  struct GVD_Process* process;
  int pid, inchannel, outchannel;
  int sv[2];
#ifdef POSIX_SIGNALS
  sigset_t blocked;
  struct sigaction sigint_action;
  struct sigaction sigquit_action;
#ifdef AIX
  struct sigaction sighup_action;
#endif /* AIX */
#endif /* !POSIX_SIGNALS */
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  volatile int forkin, forkout;
  volatile int pty_flag = 0;


  process = (struct GVD_Process*)malloc (sizeof (struct GVD_Process));
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
      forkout = forkin = gvd_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else  /* O_NOCTTY */
      forkout = forkin = gvd_open (pty_name, O_RDWR, 0);
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
	report_file_error ("Can't create pipe", Qnil);
      inchannel = sv[0];
      forkout = sv[1];
      tem = pipe (sv);
      if (tem < 0)
	{
	  gvd_close (inchannel);
	  gvd_close (forkout);
	  report_file_error ("Can't create pipe", Qnil);
	}
      outchannel = sv[1];
      forkin = sv[0];
    }
#endif /* not SKTPAIR */

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
     As a result, child_setup will close Gvd's side of the pipes.  */
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
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

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
 ** gvd_setup_child_communication ()
 **
 ***************************************************************/

int
gvd_setup_child_communication (struct GVD_Process* process, char** new_argv)
{
  char* current_dir = ".";
  int pid = 0;
  
  int xforkin = process->forkin;
  int xforkout = process->forkout;

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
	write (1, "create_process/tcsetattr LDISC1 failed\n", 39);
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
      int j = gvd_open ("/dev/tty", O_RDWR, 0);
      ioctl (j, TIOCNOTTY, 0);
      gvd_close (j);
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
      
      /* I wonder if gvd_close (gvd_open (pty_name, ...))
	 would work?  */
      if (xforkin >= 0)
	gvd_close (xforkin);
      xforkout = xforkin = gvd_open (pty_name, O_RDWR, 0);
      
      if (xforkin < 0)
	{
	  write (1, "Couldn't open the pty terminal ", 31);
	  write (1, pty_name, strlen (pty_name));
	  write (1, "\n", 1);
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

#ifndef WIN32
  signal (SIGQUIT, SIG_DFL);
#endif /* WIN32 */
  
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
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */
  
  if (process->pty_flag)
    child_setup_tty (xforkout);
#ifdef WINDOWSNT
  pid = child_setup (xforkin, xforkout, xforkout,
		     new_argv, 1, current_dir, process);
#else  /* not WINDOWSNT */	
  child_setup (xforkin, xforkout, xforkout,
	       new_argv, 1, current_dir, process);
#endif /* not WINDOWSNT */

  process->pid=pid;
  return pid;
}


/**************************************************************
 **  gvd_setup_parent_communication ()
 **
 **************************************************************/

int
gvd_setup_parent_communication
   (struct GVD_Process* process,
    int*   in_fd,  /* output */
    int*   out_fd, /* output */
    int*   err_fd, /* output */
    int*   pid_out) /* in-out parameter */
{
  process->pid = *pid_out;
  
  /* This runs in the Gvd process.  */
  if (process->pid < 0)
    {
      if (process->forkin >= 0)
	gvd_close (process->forkin);
      if (process->forkin != process->forkout && process->forkout >= 0)
	gvd_close (process->forkout);
    }
  else
    {
      /* ??? Removed Emacs code that deals with the following situation:
	 If the subfork execv fails, and it exits,
	 this close hangs.  I don't know why.
	 So have an interrupt jar it loose.  */

      if (process->forkin != process->forkout && process->forkout >= 0)
	gvd_close (process->forkout);

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
}

/* Ctrl-C Handling */

#ifndef WIN32

#ifdef subprocesses

/*
 *    flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */

static void
flush_pending_output (channel)
     int channel;
{
#ifdef HAVE_TERMIOS
  /* If we try this, we get hit with SIGTTIN, because
     the child's tty belongs to the child's pgrp. */
#else
#ifdef TCFLSH
  ioctl (channel, TCFLSH, 1);
#else
#ifdef TIOCFLUSH
  int zero = 0;
  /* 3rd arg should be ignored
     but some 4.2 kernels actually want the address of an int
     and nonzero means something different.  */
  ioctl (channel, TIOCFLUSH, &zero);
#endif  /* TIOCFLUSH */
#endif  /* TCFLSH */
#endif  /* HAVE_TERMIOS */
}

#endif  /* subprocess */

/* send a signal number SIGNO to PROCESS.
   If CURRENT_GROUP is t, that means send to the process group
   that currently owns the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If CURRENT_GROUP is lambda, that means send to the process group
   that currently owns the terminal, but only if it is NOT the shell itself.

   If we can, we try to signal PROCESS by sending control characters
   down the pty.  This allows us to signal inferiors who have changed
   their uid, for which killpg would return an EPERM error.  */

static void
process_send_signal (p, signo, current_group)
     struct GVD_Process* p;
     int signo;
     int current_group;
{
  int gid;
  int no_pgrp = 0;

  if (NILP (p->pty_flag))
    current_group = Qnil;

  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (!NILP (current_group))
    {
#ifdef SIGNALS_VIA_CHARACTERS
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */

      /* TERMIOS is the latest and bestest, and seems most likely to
         work.  If the system has it, use it.  */
#ifdef HAVE_TERMIOS
      struct termios t;

      switch (signo)
	{
	case SIGINT:
	  tcgetattr (XINT (p->infd), &t);
	  write (p->outfd, &t.c_cc[VINTR], 1);
	  return;

	case SIGQUIT:
	  tcgetattr (XINT (p->infd), &t);
	  write (p->outfd, &t.c_cc[VQUIT], 1);
  	  return;

  	case SIGTSTP:
	  tcgetattr (XINT (p->infd), &t);
#if defined (VSWTCH) && !defined (PREFER_VSUSP)
	  write (p->outfd, &t.c_cc[VSWTCH], 1);
#else
	  write (p->outfd, &t.c_cc[VSUSP], 1);
#endif
  	  return;
	}

#else /* ! HAVE_TERMIOS */

      /* On Berkeley descendants, the following IOCTL's retrieve the
	 current control characters.  */
#if defined (TIOCGLTC) && defined (TIOCGETC)

      struct tchars c;
      struct ltchars lc;

      switch (signo)
	{
	case SIGINT:
	  ioctl (XINT (p->infd), TIOCGETC, &c);
	  write (p->outfd, &c.t_intrc, 1);
	  return;
	case SIGQUIT:
	  ioctl (XINT (p->infd), TIOCGETC, &c);
	  write (p->outfd, &c.t_quitc, 1);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (XINT (p->infd), TIOCGLTC, &lc);
	  write (p->outfd, &lc.t_suspc, 1);
	  return;
#endif /* ! defined (SIGTSTP) */
	}

#else /* ! defined (TIOCGLTC) && defined (TIOCGETC) */

      /* On SYSV descendants, the TCGETA ioctl retrieves the current control
	 characters.  */
#ifdef TCGETA
      struct termio t;
      switch (signo)
	{
	case SIGINT:
	  ioctl (XINT (p->infd), TCGETA, &t);
	  write (p->outfd, &t.c_cc[VINTR], 1);
	  return;
	case SIGQUIT:
	  ioctl (XINT (p->infd), TCGETA, &t);
	  write (p->outfd, &t.c_cc[VQUIT], 1);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (XINT (p->infd), TCGETA, &t);
	  write (p->outfd, &t.c_cc[VSWTCH], 1);
	  return;
#endif /* ! defined (SIGTSTP) */
	}
#else /* ! defined (TCGETA) */
      Your configuration files are messed up.
      /* If your system configuration files define SIGNALS_VIA_CHARACTERS,
	 you'd better be using one of the alternatives above!  */
#endif /* ! defined (TCGETA) */
#endif /* ! defined (TIOCGLTC) && defined (TIOCGETC) */
#endif /* ! defined HAVE_TERMIOS */
#endif /* ! defined (SIGNALS_VIA_CHARACTERS) */

#ifdef TIOCGPGRP 
      /* Get the pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.
	 On pfa systems, saka@pfu.fujitsu.co.JP writes:
	 "TIOCGPGRP symbol defined in sys/ioctl.h at E50.
	 But, TIOCGPGRP does not work on E50 ;-P works fine on E60"
	 His patch indicates that if TIOCGPGRP returns an error, then
	 we should just assume that p->pid is also the process group id.  */
      {
	int err;

	if (!NILP (p->subtty))
	  err = ioctl (XFASTINT (p->subtty), TIOCGPGRP, &gid);
	else
	  err = ioctl (XINT (p->infd), TIOCGPGRP, &gid);

#ifdef pfa
	if (err == -1)
	  gid = - XFASTINT (p->pid);
#endif /* ! defined (pfa) */
      }
      if (gid == -1)
	no_pgrp = 1;
      else
	gid = - gid;
#else  /* ! defined (TIOCGPGRP ) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = - XFASTINT (p->pid);
#endif /* ! defined (TIOCGPGRP ) */

      /* If current_group is lambda, and the shell owns the terminal,
	 don't send any signal.  */
      /* if (EQ (current_group, Qlambda) && gid == - XFASTINT (p->pid))
	 return;*/
    }
  else
    gid = - XFASTINT (p->pid);

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      /* ??? This signal is not handled properly currently */
      break;
#endif /* ! defined (SIGCONT) */
    case SIGINT:
#ifdef VMS
      write (proc->outfd, "\003", 1);   /* ^C */
      goto whoosh;
#endif
    case SIGQUIT:
#ifdef VMS
      send_process (proc, "\031", 1, Qnil);	/* ^Y */
      goto whoosh;
#endif
    case SIGKILL:
#ifdef VMS
      sys$forcex (&(XFASTINT (p->pid)), 0, 1);
      whoosh:
#endif
      flush_pending_output (XINT (p->infd));
      break;
    }

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  if (no_pgrp)
    {
      kill (XFASTINT (p->pid), signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (!NILP (current_group)) {
    ioctl (XINT (p->infd), TIOCSIGSEND, signo);
  }
  else
    {
      gid = - XFASTINT (p->pid);
      kill (gid, signo);
    }
#else /* ! defined (TIOCSIGSEND) */
  GVD_KILLPG (-gid, signo);
#endif /* ! defined (TIOCSIGSEND) */
}

int
gvd_interrupt_process (struct GVD_Process* p)
{
  process_send_signal (p, SIGINT, 1);
}

#else /* !WIN32 */

typedef struct _child_process
{
  HWND                  hwnd;
  PROCESS_INFORMATION   *procinfo;
} child_process;

/* The major and minor versions of NT.  */
static int w32_major_version;
static int w32_minor_version;

/* Distinguish between Windows NT and Windows 95.  */
static enum {OS_UNKNOWN, OS_WIN95, OS_NT} os_subtype = OS_UNKNOWN;

/* Cache information describing the NT system for later use.  */
static void
cache_system_info (void)
{
  union
    {
      struct info
        {
          char  major;
          char  minor;
          short platform;
        } info;
      DWORD data;
    } version;

  /* Cache the version of the operating system.  */
  version.data = GetVersion ();
  w32_major_version = version.info.major;
  w32_minor_version = version.info.minor;

  if (version.info.platform & 0x8000)
    os_subtype = OS_WIN95;
  else
    os_subtype = OS_NT;
}

static BOOL CALLBACK
find_child_console (HWND hwnd, child_process * cp)
{
  DWORD thread_id;
  DWORD process_id;

  thread_id = GetWindowThreadProcessId (hwnd, &process_id);
  if (process_id == cp->procinfo->dwProcessId)
    {
      char window_class[32];

      GetClassName (hwnd, window_class, sizeof (window_class));
      if (strcmp (window_class,
                  (os_subtype == OS_WIN95)
                  ? "tty"
                  : "ConsoleWindowClass") == 0)
        {
          cp->hwnd = hwnd;
          return FALSE;
        }
    }
  /* keep looking */
  return TRUE;
}

int
gvd_interrupt_process (struct GVD_Process* p)
{
  volatile child_process cp;
  HANDLE proc_hand;
  int rc = 0;

  cp.procinfo = &p->procinfo;
  proc_hand = cp.procinfo->hProcess;

  if (os_subtype == OS_UNKNOWN)
    cache_system_info ();

  /* Try to locate console window for process. */
  EnumWindows ((ENUMWINDOWSPROC) find_child_console, (LPARAM) &cp);

  if (NILP (Vw32_start_process_share_console) && cp.hwnd)
    {
      BYTE control_scan_code = (BYTE) MapVirtualKey (VK_CONTROL, 0);
      /* Retrieve Ctrl-C scancode */
      BYTE vk_break_code = 'C';
      BYTE break_scan_code = (BYTE) MapVirtualKey (vk_break_code, 0);
      HWND foreground_window;

      foreground_window = GetForegroundWindow ();
      if (foreground_window)
        {
          /* NT 5.0, and apparently also Windows 98, will not allow
             a Window to be set to foreground directly without the
             user's involvement. The workaround is to attach
             ourselves to the thread that owns the foreground
             window, since that is the only thread that can set the
             foreground window.  */
          DWORD foreground_thread, child_thread;

          foreground_thread =
            GetWindowThreadProcessId (foreground_window, NULL);
          if (foreground_thread == GetCurrentThreadId ()
              || !AttachThreadInput (GetCurrentThreadId (),
                                     foreground_thread, TRUE))
            foreground_thread = 0;

          child_thread = GetWindowThreadProcessId (cp.hwnd, NULL);
          if (child_thread == GetCurrentThreadId ()
              || !AttachThreadInput (GetCurrentThreadId (),
                                     child_thread, TRUE))
            child_thread = 0;

          /* Set the foreground window to the child.  */
          if (SetForegroundWindow (cp.hwnd))
            {
              /* Generate keystrokes as if user had typed Ctrl-Break or
                 Ctrl-C.  */
              keybd_event (VK_CONTROL, control_scan_code, 0, 0);
              keybd_event (vk_break_code, break_scan_code,
                (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY), 0);
              keybd_event (vk_break_code, break_scan_code,
                (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY)
                 | KEYEVENTF_KEYUP, 0);
              keybd_event (VK_CONTROL, control_scan_code, KEYEVENTF_KEYUP, 0);

              /* Sleep for a bit to give time for the main frame to respond
              to focus change events.  */
              Sleep (100);

              SetForegroundWindow (foreground_window);
            }
          /* Detach from the foreground and child threads now that
             the foreground switching is over.  */
          if (foreground_thread)
            AttachThreadInput (GetCurrentThreadId (), foreground_thread, FALSE);          if (child_thread)
            AttachThreadInput (GetCurrentThreadId (), child_thread, FALSE);
        }
    }
  /* Ctrl-Break is NT equivalent of SIGINT.  */
  else if (!GenerateConsoleCtrlEvent
             (CTRL_BREAK_EVENT, cp.procinfo->dwProcessId))
    {
      errno = EINVAL;
      rc = -1;
    }

  return rc;
}
#endif /* !WIN32 */
