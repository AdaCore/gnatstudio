/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               C T R L _ C                                *
 *                                                                          *
 *                            $Revision$
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 2002, Free Software Foundation, Inc.              *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). *
 *                                                                          *
 ****************************************************************************/

/* Services to intercept Ctrl-C */

/* __gnat_install_int_handler will install the specified handler.
   If called for the first time, it will also save the original handler */
void __gnat_install_int_handler (void (*) (void));

/* __gnat_uninstall_int_handler will reinstall the original handler */
void __gnat_uninstall_int_handler ();

/* POSIX implementation */

#if (defined (_AIX) || defined (unix)) && !defined (__vxworks)

#include <signal.h>

void (*sigint_intercepted) () = 0;

struct sigaction original_act;

static void
__gnat_int_handler (int sig)
{
  if (sigint_intercepted != 0)
    sigint_intercepted ();
}

/* Install handler and save original handler. */

void
__gnat_install_int_handler (void (*proc)())
{
  struct sigaction act;

  if (sigint_intercepted == 0)
    {
      act.sa_handler = __gnat_int_handler;
      act.sa_flags = SA_RESTART;
      (void) sigemptyset (&act.sa_mask);

      (void) sigaction (SIGINT, &act, &original_act);
    }

  sigint_intercepted = proc;
}

/* Restore original handler */

void
__gnat_uninstall_int_handler ()
{
 if (sigint_intercepted != 0)
   {
     (void) sigaction (SIGINT, &original_act, 0);
     sigint_intercepted = 0;
   }
}

/* Windows implementation */

#elif defined (__MINGW32__)

#include "mingw32.h"
#include <windows.h>

void (*sigint_intercepted) () = NULL;

static BOOL WINAPI
__gnat_int_handler  (DWORD dwCtrlType)
{
  switch (dwCtrlType)
    {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
      if (sigint_intercepted != 0)
        sigint_intercepted ();
      break;

    case CTRL_CLOSE_EVENT:
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
      break;
    }
}

void
__gnat_install_int_handler (void (*proc)())
{
  if (sigint_intercepted == NULL)
    {
      SetConsoleCtrlHandler (__gnat_int_handler, TRUE);
    }

  sigint_intercepted = proc;
}

void
__gnat_uninstall_int_handler ()
{
  if (sigint_intercepted != NULL)
    {
      SetConsoleCtrlHandler (__gnat_int_handler, FALSE);
    }

  sigint_intercepted = NULL;
}

/* Default implementation: do nothing */

#else

void
__gnat_install_int_handler (void (*proc)())
{
}

void
__gnat_uninstall_int_handler ()
{
}
#endif
