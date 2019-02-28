/*
 * expWinProcess.c --
 *
 *	This file contains utility procedures.  It primarily handled
 *	processes for Expect.
 *
 * Copyright (C) 2006-2019, AdaCore
 * Copyright (c) 1997 Mitel Corporation
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1987-1993 The Regents of the University of California.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "expWin.h"
#include "expDString.h"

#ifndef WNOHANG
#  define WNOHANG 1
#endif

#ifndef ECHILD
#  define ECHILD -1
#endif

/*
 * This list is used to map from pids to process handles.
 */

typedef struct ProcInfo {
    HANDLE hProcess;
    DWORD dwProcessId;
    struct ProcInfo *nextPtr;
} ProcInfo;

static ProcInfo *procList = NULL;

/*
 *----------------------------------------------------------------------
 *
 * HasConsole --
 *
 *	Determines whether the current application is attached to a
 *	console.
 *
 * Results:
 *	Returns TRUE if this application has a console, else FALSE.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static BOOL
HasConsole()
{
    HANDLE handle = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE,
	    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

    if (handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
	return TRUE;
    } else {
        return FALSE;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * BuildCommandLine --
 *
 *	The command line arguments are stored in linePtr separated
 *	by spaces, in a form that CreateProcess() understands.  Special
 *	characters in individual arguments from argv[] must be quoted
 *	when being stored in cmdLine.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
BuildCommandLine(argc, argv, linePtr)
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
    Tcl_DString *linePtr;	/* Initialized Tcl_DString that receives the
				 * command line. */
{
    char *start, *special;
    int quote, i;

    for (i = 0; i < argc; i++) {
	if (i > 0) {
	    Tcl_DStringAppend(linePtr, " ", 1);
	}

	quote = 0;
	for (start = argv[i]; *start != '\0'; start++) {
	    if (isspace(*start)) {
		quote = 1;
		Tcl_DStringAppend(linePtr, "\"", 1);
    		break;
	    }
	}

	start = argv[i];
	for (special = argv[i]; ; ) {
	    if ((*special == '\\') &&
		    (special[1] == '\\' || special[1] == '"')) {
		Tcl_DStringAppend(linePtr, start, special - start);
		start = special;
		while (1) {
		    special++;
		    if (*special == '"') {
			/*
			 * N backslashes followed a quote -> insert
			 * N * 2 + 1 backslashes then a quote.
			 */

			Tcl_DStringAppend(linePtr, start, special - start);
			break;
		    }
		    if (*special != '\\') {
			break;
		    }
		}
		Tcl_DStringAppend(linePtr, start, special - start);
		start = special;
	    }
	    if (*special == '"') {
		Tcl_DStringAppend(linePtr, start, special - start);
		Tcl_DStringAppend(linePtr, "\\\"", 2);
		start = special + 1;
	    }
	    if (*special == '\0') {
		break;
	    }
	    special++;
	}
	Tcl_DStringAppend(linePtr, start, special - start);
	if (quote) {
	    Tcl_DStringAppend(linePtr, "\"", 1);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Exp_WaitPid --
 *
 *	Emulates the waitpid system call.
 *
 * Results:
 *	Returns 0 if the process is still alive, -1 on an error, or
 *	the pid on a clean close.
 *
 * Side effects:
 *	Unless WNOHANG is set and the wait times out, the process
 *	information record will be deleted and the process handle
 *	will be closed.
 *
 *----------------------------------------------------------------------
 */

HANDLE
Exp_WaitPid(pid, statPtr, options)
    HANDLE pid;
    LPDWORD statPtr;
    DWORD options;
{
    ProcInfo *infoPtr, **prevPtrPtr;
    int flags;
    HANDLE result;
    DWORD ret;

    if (options & WNOHANG) {
	flags = 0;
    } else {
	flags = INFINITE;
    }
    if (pid == 0) {
        EXP_LOG ("Exp_WaitPid: pid null", NULL);
	*statPtr = 0;
	return 0;
    }

    /*
     * Find the process on the process list.
     */

    prevPtrPtr = &procList;
    for (infoPtr = procList; infoPtr != NULL;
         prevPtrPtr = &infoPtr->nextPtr, infoPtr = infoPtr->nextPtr) {
	 if (infoPtr->hProcess == (HANDLE) pid) {
	    break;
	}
    }
    if (infoPtr == NULL) {
        EXP_LOG ("Exp_WaitPid: infoPtr null", NULL);
	return 0;
    }

    EXP_LOG ("Exp_WaitPid: pid is %d", infoPtr->dwProcessId);

    ret = WaitForSingleObject(infoPtr->hProcess, flags);
    if (ret == WAIT_TIMEOUT) {
        EXP_LOG ("Exp_WaitPid: Timeout", NULL);
	*statPtr = 0;
	if (options & WNOHANG) {
	    return 0;
	} else {
	    result = 0;
	}
    } else if (ret != WAIT_FAILED) {
      GetExitCodeProcess(infoPtr->hProcess, statPtr);
      if (*statPtr == STILL_ACTIVE) {
        result = 0;
      } else {
        result = infoPtr->hProcess;
      }
      EXP_LOG ("Exp_WaitPid: process exited with code %d", *statPtr);
    } else {
        EXP_LOG ("Exp_WaitPid: ECHILD", NULL);
	errno = ECHILD;
	result = (HANDLE) -1;
    }

    /*
     * Remove the process from the process list and close the process handle.
     */
    CloseHandle(infoPtr->hProcess);
    *prevPtrPtr = infoPtr->nextPtr;
    ckfree((char*)infoPtr);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Exp_KillProcess --
 *
 *	Kills the subprocess
 *
 * Results:
 *	Nothing
 *
 * Side effects:
 *	The subprocess is killed.
 *
 *----------------------------------------------------------------------
 */

void
Exp_KillProcess(pid)
    HANDLE pid;
{
    TerminateProcess((HANDLE) pid, 0xFFFF);
}

void
Exp_GetCurrentDir (char *dir, int *length)
{
  *((LPDWORD)length) = GetCurrentDirectory (MAX_PATH, (TCHAR *)dir);
}

/*
 *----------------------------------------------------------------------
 *
 * ExpCreateProcess --
 *
 *	Create a child process that has the specified files as its
 *	standard input, output, and error.  The child process is set
 *	to run properly under only Windows NT right now, and runs with
 *	the same environment variables as the creating process.
 *
 *	The complete Windows search path is searched to find the specified
 *	executable.  If an executable by the given name is not found,
 *	automatically tries appending ".com", ".exe", ".bat", and ".cmd" to
 *	the executable name.
 *
 * Results:
 *	0 on success, an error value otherwise.
 *
 * Side effects:
 *	A process is created.
 *
 *----------------------------------------------------------------------
 */

DWORD
ExpCreateProcess(argc, argv, allocConsole, hideConsole, debug, newProcessGroup,
		 pidPtr, globalProcInfo)
    int argc;			/* Number of arguments in following array. */
    char **argv;		/* Array of argument strings.  argv[0]
				 * contains the name of the executable
				 * converted to native format (using the
				 * Tcl_TranslateFileName call).  Additional
				 * arguments have not been converted. */
    int allocConsole;		/* Should a console be allocated */
    int hideConsole;		/* Hide or display the created console */
    int debug;			/* Is this process going to be debugged? */
    int newProcessGroup;	/* Create a new process group */
    PHANDLE pidPtr;		/* If this procedure is successful, pidPtr
				 * is filled with the process id of the child
				 * process. */
    LPPROCESS_INFORMATION globalProcInfo;	/* Globally unique pid */
{
    int createFlags;
    Tcl_DString cmdLine;
    STARTUPINFO startInfo;
    HANDLE hProcess;
    char pwd[MAX_PATH];
    int pwdLength;
    LONG result;
    BOOL b;

    Exp_GetCurrentDir (pwd, &pwdLength);
    pwd[pwdLength] = '\0';

    EXP_LOG ("Create process launched from '%s'", pwd);

    result = 0;

    Tcl_DStringInit(&cmdLine);

    hProcess = GetCurrentProcess();

    /*
     * STARTF_USESTDHANDLES must be used to pass handles to child process.
     * Using SetStdHandle() and/or dup2() only works when a console mode
     * parent process is spawning an attached console mode child process.
     */

    ZeroMemory(&startInfo, sizeof(startInfo));
    startInfo.cb = sizeof(startInfo);

    /*
     * If we do not have a console window, then we must run DOS and
     * WIN32 console mode applications as detached processes. This tells
     * the loader that the child application should not inherit the
     * console, and that it should not create a new console window for
     * the child application.  The child application should get its stdio
     * from the redirection handles provided by this application, and run
     * in the background.
     *
     * If we are starting a GUI process, they don't automatically get a
     * console, so it doesn't matter if they are started as foreground or
     * detached processes.  The GUI window will still pop up to the
     * foreground.
     */

    if (!allocConsole && HasConsole()) {
	createFlags = 0;
    } else if (allocConsole) {
	/*
	 * Under NT, 16-bit DOS applications will not run unless they
	 * can be attached to a console.  If we are running without a
	 * console, run the 16-bit program as an normal process inside
	 * of a hidden console application, and then run that hidden
	 * console as a detached process.
	 */

	if (hideConsole) {
	    startInfo.wShowWindow = SW_HIDE;
	} else {
	    /* For debugging, show the sub process console */
	    startInfo.wShowWindow = SW_SHOW;
	}
	startInfo.dwFlags |= STARTF_USESHOWWINDOW;
        createFlags = CREATE_NEW_CONSOLE;
    } else {
	createFlags = DETACHED_PROCESS;
    }
    if (debug) {
	createFlags |= DEBUG_PROCESS;
    }
    if (newProcessGroup) {
	createFlags |= CREATE_NEW_PROCESS_GROUP;
    }

    /*
     * cmdLine gets the full command line used to invoke the executable,
     * including the name of the executable itself.  The command line
     * arguments in argv[] are stored in cmdLine separated by spaces.
     * Special characters in individual arguments from argv[] must be
     * quoted when being stored in cmdLine.
     *
     * When calling any application, bear in mind that arguments that
     * specify a path name are not converted.  If an argument contains
     * forward slashes as path separators, it may or may not be
     * recognized as a path name, depending on the program.  In general,
     * most applications accept forward slashes only as option
     * delimiters and backslashes only as paths.
     *
     * Additionally, when calling a 16-bit dos or windows application,
     * all path names must use the short, cryptic, path format (e.g.,
     * using ab~1.def instead of "a b.default").
     */

    BuildCommandLine(argc, argv, &cmdLine);
    EXP_LOG ("CmdLine : %s", cmdLine);

    b = CreateProcess(NULL, Tcl_DStringValue(&cmdLine), NULL, NULL, FALSE,
                      createFlags, NULL, ".", &startInfo, globalProcInfo);
    if (! b) {
	result = GetLastError();
	goto end;
    }

    /*
     * "When an application spawns a process repeatedly, a new thread
     * instance will be created for each process but the previous
     * instances may not be cleaned up.  This results in a significant
     * virtual memory loss each time the process is spawned.  If there
     * is a WaitForInputIdle() call between CreateProcess() and
     * CloseHandle(), the problem does not occur." PSS ID Number: Q124121
     */

    WaitForInputIdle(globalProcInfo->hProcess, 5000);
    CloseHandle(globalProcInfo->hThread);

    *pidPtr = globalProcInfo->hProcess;
    if (*pidPtr != 0) {
	ProcInfo *procPtr = (ProcInfo *) ckalloc(sizeof(ProcInfo));
	procPtr->hProcess = globalProcInfo->hProcess;
	procPtr->dwProcessId = globalProcInfo->dwProcessId;
	procPtr->nextPtr = procList;
	procList = procPtr;
    }

 end:
    Tcl_DStringFree(&cmdLine);
    if (startInfo.hStdInput != INVALID_HANDLE_VALUE) {
        CloseHandle(startInfo.hStdInput);
    }
    if (startInfo.hStdOutput != INVALID_HANDLE_VALUE) {
        CloseHandle(startInfo.hStdOutput);
    }
    if (startInfo.hStdError != INVALID_HANDLE_VALUE) {
	CloseHandle(startInfo.hStdError);
    }
    return result;
}
