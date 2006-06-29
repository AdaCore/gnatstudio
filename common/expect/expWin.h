/*
 * expWin.h --
 *
 *	Useful definitions for Expect on NT.
 *
 * Copyright (c) 2006 AdaCore
 * Copyright (c) 1997 by Mitel, Inc.
 * Copyright (c) 1997 by Gordon Chaffee (chaffee@home.com)
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _EXPWIN
#define _EXPWIN

#include <windows.h>

#define EXP_SLAVE_CREATE 'c'
#define EXP_SLAVE_KEY    'k'
#define EXP_SLAVE_MOUSE  'm'
#define EXP_SLAVE_WRITE  'w'
#define EXP_SLAVE_KILL   'x'

/*
 * Define the types of attempts to use to kill the subprocess
 */
#define EXP_KILL_TERMINATE  0x1
#define EXP_KILL_CTRL_C     0x2
#define EXP_KILL_CTRL_BREAK 0x4


#ifdef EXPLAUNCH_DEBUG
#include <stdio.h>
extern FILE *log_file;
#define EXP_BEGIN(filename) \
    log_file=fopen(filename,"w");
#define EXP_LOG(format, args) \
    if (log_file!=NULL) \
      fprintf(log_file, "Expect SlaveDriver %s: %d " format "\n", \
              __FILE__, __LINE__, args)
#define EXP_LOG_FLUSH fflush (log_file)
#define panic(log) \
    if (log_file!=NULL) \
      fprintf(log_file, "Expect SlaveDriver panic at %s: %d %s\n", \
              __FILE__,__LINE__,log)
#define LOG_ENTRY(log) \
    if (log_file!=NULL) \
      fprintf(log_file, ">>>%s\n", log)
#define LOG_EXIT(log) \
    if (log_file!=NULL) \
      fprintf(log_file, "<<<%s\n", log)
#define EXP_END \
    fclose (log_file)
#define EXP_DEBUG 1

#else /* !SLAVEDRV */

#define EXP_BEGIN(filename)
#define EXP_LOG(format, args)
#define EXP_LOG_FLUSH
#define panic(log)
#define LOG_ENTRY(log)
#define LOG_EXIT(log)
#define EXP_END
#endif /* !SLAVEDRV */


#define ckfree free
#define ckalloc malloc

/*
 * The following defines identify the various types of applications that
 * run under windows.  There is special case code for the various types.
 */

#define EXP_APPL_NONE	0
#define EXP_APPL_DOS	1
#define EXP_APPL_WIN3X	2
#define EXP_APPL_WIN32	3

/* typedef struct { */
/*     Tcl_Channel channelPtr; */
/*     int toWrite; */
/* } ExpSpawnState; */

extern DWORD		ExpApplicationType(const char *originalName,
			    char *fullPath, char *imageName);
extern DWORD		ExpCreateProcess(int argc, char **argv,
			    int allocConsole,
			    int hideConsole, int debug, int newProcessGroup,
			    PHANDLE pidPtr, LPPROCESS_INFORMATION globalProcInfo);
extern HANDLE		Exp_WaitPid(HANDLE pid, LPDWORD statPtr, DWORD options);
extern void		Exp_KillProcess(HANDLE pid);

#endif
