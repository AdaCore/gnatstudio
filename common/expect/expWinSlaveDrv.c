/*
 * expWinSlaveDrv.c --
 *
 *	This file implements the Windows NT specific expect slave driver.
 *	The slave driver is used to control a subprocess, but it does it
 *	in an unshared console.  Because a process can only be attached
 *	to a single console, we need to have a separate executable
 *	driving the slave process.  Hence, a slave driver.
 *
 * Copyright (c) 2006-2008 AdaCore
 * Copyright (c) 1997 by Mitel Corporation
 * Copyright (c) 1997-1998 by Gordon Chaffee
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *
 * XXX: Make sure to check at initialization time in Expect that we are
 * not trying to run on Win32s or Windows 95.  Named pipes will fail
 * rather quickly on Win95 anyway.
 *
 */

/*
 *----------------------------------------------------------------------
 * Communication Protocol Between Master and Slave Driver
 *
 * The master sends over a single byte command.  Depending on
 * the command, further data may follow.  Not all of this is
 * implemented, and some may have changed.
 *
 * EXP_SLAVE_WRITE:
 *	Further Data: Followed by binary 4 bytes specifying length data
 *		to write to slave.
 *	Response: None
 *
 * EXP_SLAVE_KEY:
 *	Further Data: Not defined yet.  Will correspond to the
 *	   KEY_EVENT_RECORD structure.
 *		BOOL bKeyDown;
 *		WORD wRepeatCount;
 *		WORD wVirtualKeyCode;
 *		WORD wVirtualScanCode;
 *		union {
 *			WCHAR UnicodeChar;
 *			CHAR  AsciiChar;
 *		} uChar;
 *		DWORD dwControlKeyState;
 *	Response: None
 *
 *----------------------------------------------------------------------
 */

/*
 * Even though we won't have access to most of the commands, use the
 * normal headers
 */

#include <winsock2.h>
#include "expWin.h"
#include "expWinSlave.h"

#define STATE_WAIT_CMD   0	/* Waiting for the next command */
#define STATE_CREATE     1	/* Doesn't happen currently */
#define STATE_KEY        2	/* Waiting for key params */
#define STATE_KILL       3	/* 1 Param: type of kill to do */
#define STATE_MOUSE      4	/*  */
#define STATE_WRITE      5	/* Wait for the length of the data */
#define STATE_WRITE_DATA 6	/* Wait for the data itself */

#define BUFSIZE 4096
#define CONSOLE_WINDOW_WIDTH 79
HANDLE hShutdown;   /* Event is set when the slave driver is shutting down. */

int    ExpDebug;
char   *ExpReading;

typedef struct ExpFunctionKey {
    char *sequence;
    DWORD keyval;
} ExpFunctionKey;

ExpFunctionKey VtFunctionKeys[] =
{
    {"OP",  EXP_KEY_F1},
    {"OQ",  EXP_KEY_F2},
    {"OR",  EXP_KEY_F3},
    {"OS",  EXP_KEY_F4},
    {"[A",  EXP_KEY_UP},
    {"[B",  EXP_KEY_DOWN},
    {"[C",  EXP_KEY_RIGHT},
    {"[D",  EXP_KEY_LEFT},
    {"[F",  EXP_KEY_END},
    {"[H",  EXP_KEY_HOME},
    {"[2~", EXP_KEY_INSERT},
    {"[3~", EXP_KEY_DELETE},
    {"[4~", EXP_KEY_SELECT},
    {"[5~", EXP_KEY_PAGEUP},
    {"[6~", EXP_KEY_PAGEDOWN},
    {"[11~", EXP_KEY_F1},
    {"[12~", EXP_KEY_F2},
    {"[13~", EXP_KEY_F3},
    {"[14~", EXP_KEY_F4},
    {"[15~", EXP_KEY_F5},
    {"[17~", EXP_KEY_F6},
    {"[18~", EXP_KEY_F7},
    {"[19~", EXP_KEY_F8},
    {"[20~", EXP_KEY_F9},
    {"[21~", EXP_KEY_F10},
    {"[23~", EXP_KEY_F11},
    {"[24~", EXP_KEY_F12},
    {"[25~", EXP_KEY_F13},
    {"[26~", EXP_KEY_F14},
    {"[28~", EXP_KEY_F15},
    {"[29~", EXP_KEY_F16},
    {"[31~", EXP_KEY_F17},
    {"[32~", EXP_KEY_F18},
    {"[33~", EXP_KEY_F19},
    {"[34~", EXP_KEY_F20},
    {"[39~", EXP_WIN_RESIZE},
    {NULL, 0}
};

#define EXP_MAX_QLEN 200
HANDLE ExpWaitEvent;		/* Set after modifying wait queue */
HANDLE ExpWaitMutex;		/* Grab before modifying wait queue */
DWORD  ExpWaitCount;		/* Current number of wait handles */
HANDLE ExpWaitQueue[EXP_MAX_QLEN];/* wait handles */
DWORD  ExpConsoleInputMode;	/* Current flags for the console */

DWORD exitVal;

static void		InitializeWaitQueue(void);
static void		ExpProcessInput(HANDLE hMaster,
			    HANDLE hConsole, HANDLE hConsoleOut,
			    ExpSlaveDebugArg *debugInfo);
static BOOL		WriteBufferToSlave(int noEscapes,
			    HANDLE hConsoleInW, HANDLE hConsoleOut,
			    PUCHAR buf, DWORD n);
static DWORD WINAPI	WaitQueueThread(LPVOID *arg);
static void		SetArgv(char *cmdLine, int *argcPtr, char ***argvPtr);

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	Main entry point from Windows.  The arguments to this program
 *	are used to create the subprocess that this will control.
 *	However, it will not execute the subprocess immediately.
 *	It waits to hear from the expect process about setting
 *	things like the CTRL-C handler and such.
 *
 *	argv[1] is the input named pipe base that we need to connect to.
 *	argv[2] is the output named pipe base that we need to connect to.
 *	argv[3] is the program we will run, and all the following
 *	  are its arguments.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	The launch of this program will have caused a console to be
 *	allocated.  This will then send commands to subprocesses.
 *
 * Notes:
 *	Because we are dealing with anonymous pipes, all actions
 *	on pipes are blocking.  To deal with this, after a process
 *	is created, we need to create two additional threads that
 *	read from the child's STDOUT and STDERR.  Anything read
 *	from the pipe will be forwarded directly to the master on
 *	the appropriate pipes.  This should also take care of the
 *	problems associated with blocking pipes.
 *
 *----------------------------------------------------------------------
 */

#ifdef EXP_DEBUG
FILE *log_file;
#endif

int
main(argc, argv)
    int argc;
    char **argv;
{
  HANDLE hConsoleInW;	/* Console, writeable input handle */
  HANDLE hConsoleOut;	/* Console, readable output handle */
  HANDLE hMasterInput;	/* Pipe between master and us */
  HANDLE hMasterOutput;
  HANDLE hProcess;	/* Current process handle */
  HANDLE hChild;        /* Child process handle */
  HANDLE hInPipeRd;     /* Read side of the process's Input pipe */
  HANDLE hInPipeWr;     /* Write side of the process's Input pipe */
  SECURITY_ATTRIBUTES sec_attrs;
  PROCESS_INFORMATION procinfo;
  UCHAR cmdline[BUFSIZE];
  BOOL bRet;
  DWORD dwResult;
  HANDLE hThread;
  DWORD threadId;
  ExpSlaveDebugArg debugInfo;
  SMALL_RECT consoleWindow;
  CONSOLE_SCREEN_BUFFER_INFO consoleSBInfo;

  int n;

  struct sockaddr_in sin;
  WSADATA	SockData;

  if (argc < 2) {
    exit(1);
  }

  EXP_BEGIN ("./slavedrv.log");

  ExpDebug = FALSE;
  ExpReading = NULL;

  /*
   * After the subprocess is created, send back the status (success or not)
   * and the process id of the child so the master can kill it.
   */

  hShutdown = CreateEvent(NULL, TRUE, FALSE, NULL);

  InitializeWaitQueue();

  debugInfo.argc = argc-3;
  debugInfo.argv = &argv[3];

  /* Set inheritance for the handles */
  sec_attrs.nLength = sizeof (SECURITY_ATTRIBUTES);
  sec_attrs.bInheritHandle = TRUE;
  sec_attrs.lpSecurityDescriptor = NULL;

  /* Make sure a console is there */
  AllocConsole();

  /* Use OVERLAPPED flag so that we can use asynchronous reads */
  hMasterInput = CreateFile(argv[1], GENERIC_READ,
			    FILE_SHARE_READ,
			    NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL);
  if (hMasterInput == NULL) {
    EXP_LOG ("ERROR: could not create master input pipe (%d)", GetLastError());
    exit (1);
  }

  hConsoleInW = CreateFile("CONIN$", GENERIC_WRITE,
			   FILE_SHARE_READ | FILE_SHARE_WRITE,
			   &sec_attrs, OPEN_EXISTING, 0, NULL);
  if (hConsoleInW == NULL) {
    EXP_LOG("Unexpected error 0x%x", GetLastError());
    EXP_END;
    ExitProcess(1);
  }

  hConsoleOut = CreateFile("CONOUT$", GENERIC_READ|GENERIC_WRITE,
			   FILE_SHARE_READ|FILE_SHARE_WRITE,
			   &sec_attrs, OPEN_EXISTING, 0, NULL);
  if (hConsoleOut == NULL) {
    EXP_LOG("Unexpected error 0x%x", GetLastError());
    ExitProcess(1);
  }

  ExpConsoleInputMode = ENABLE_PROCESSED_INPUT;
  SetConsoleMode (hConsoleInW, ExpConsoleInputMode);
  /*
   * Reduce the size of the console window so that LF are correctly transmitted
   * with the LF character instead of a cursor position change
   */
  GetConsoleScreenBufferInfo (hConsoleOut, &consoleSBInfo);
  consoleWindow.Top = 0;
  consoleWindow.Left = 0;
  consoleWindow.Bottom = 1;
  consoleWindow.Right = CONSOLE_WINDOW_WIDTH;
  EXP_LOG ("right edge of console window : %d", consoleWindow.Right);
  SetConsoleWindowInfo (hConsoleOut, TRUE, &consoleWindow);

  /*
   * The subprocess needs to be created in the debugging thread.
   * Set all the args in debugInfo and start it up.
   */

  debugInfo.hConsole = hConsoleOut;
  debugInfo.hMasterPipe = argv[2];
  debugInfo.slaveStdin = NULL;
  debugInfo.slaveStdout = NULL;
  debugInfo.slaveStderr = NULL;
  debugInfo.event = CreateEvent(NULL, TRUE, FALSE, NULL);
  debugInfo.thread = CreateThread
    (NULL, 65536, (LPTHREAD_START_ROUTINE) ExpSlaveDebugThread,
     (LPVOID) &debugInfo, 0, &threadId);
  ExpAddToWaitQueue(debugInfo.thread);

  EXP_LOG ("ExpSlaveDebugThread created", NULL);

  WaitForSingleObject(debugInfo.event, INFINITE);
  CloseHandle(debugInfo.event);

  if (debugInfo.result) {
    EXP_LOG ("debugInfo returned error: %d", debugInfo.lastError);
    EXP_END;
    ExitProcess(1);
  }

  hThread = CreateThread
    (NULL, 8192, (LPTHREAD_START_ROUTINE) WaitQueueThread,
     (LPVOID) &debugInfo.process, 0, &threadId);
  CloseHandle(hThread);

  ExpProcessInput(hMasterInput, hConsoleInW, hConsoleOut,
		  &debugInfo);

  EXP_LOG ("exiting with ret value = %d", exitVal);
  EXP_LOG ("CURRENT PROCESS is %d", GetCurrentProcessId());
  EXP_END;

  if (!TerminateProcess (GetCurrentProcess(), exitVal)) {
    ExitProcess (exitVal);
  }
  // never executed
  return exitVal;
}

/*
 *----------------------------------------------------------------------
 *
 * ExpProcessInput --
 *
 *	The master in this case is Expect.  Drives until everything exits.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
ExpProcessInput(HANDLE hMaster, HANDLE hConsoleInW, HANDLE hConsoleOut,
		ExpSlaveDebugArg *debugInfo)
{
  UCHAR buffer[BUFSIZE];
  DWORD dwState;
  DWORD dwHave;
  DWORD dwNeeded;
  DWORD dwTotalNeeded;
  BOOL bRet;
  DWORD dwResult;
  DWORD driverInCnt;		/* Number of bytes read from expect pipe */

  dwHave = 0;
  dwState = STATE_WAIT_CMD;
  dwNeeded = 1;

  while (1) {
    bRet = ExpReadMaster(hMaster, &buffer[dwHave],
                         dwNeeded-dwHave, &driverInCnt, &dwResult);
    if ((bRet == TRUE && driverInCnt == 0) ||
	(bRet == FALSE && dwResult == ERROR_BROKEN_PIPE))
      {
	/* Nominal exit of the process */
	ExpKillProcessList();
	ExitProcess(exitVal);
      } else if (bRet == FALSE) {
	EXP_LOG("Unexpected error 0x%x", dwResult);
	ExpKillProcessList();
	ExitProcess(1);
      }

    dwHave += driverInCnt;
    if (dwHave != dwNeeded) {
      continue;
    }
    dwHave = 0;
    switch (dwState) {
      case STATE_WAIT_CMD:
        switch (buffer[0]) {
          case EXP_SLAVE_KILL:
            EXP_LOG ("EXP_SLAVE_KILL", NULL);
            dwState = STATE_KILL;
            dwNeeded = 1;
            break;
          case EXP_SLAVE_KEY:
            EXP_LOG ("EXP_SLAVE_KEY", NULL);
            /* XXX: To be implemented */
            dwState = STATE_KEY;
            dwNeeded = 13;
            break;
          case EXP_SLAVE_MOUSE:
            EXP_LOG ("EXP_SLAVE_MOUSE", NULL);
            /* XXX: To be implemeanted */
            dwState = STATE_MOUSE;
            dwNeeded = 13 /* XXX */;
            break;
          case EXP_SLAVE_WRITE:
            EXP_LOG ("EXP_SLAVE_WRITE", NULL);
            dwNeeded = 4;
            dwState = STATE_WRITE;
            break;
        }
        break;
      case STATE_KILL:
        EXP_LOG ("STATE_KILL", NULL);
        dwResult = buffer[0];
        if (dwResult & EXP_KILL_CTRL_C) {
          GenerateConsoleCtrlEvent(CTRL_C_EVENT, debugInfo->globalPid);
        } else if (dwResult & EXP_KILL_CTRL_BREAK) {
          GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT,debugInfo->globalPid);
        } else if (dwResult & EXP_KILL_TERMINATE) {
          Exp_KillProcess(debugInfo->process);
        }
        dwState = STATE_WAIT_CMD;
        dwNeeded = 1;
        break;
      case STATE_WRITE:
        EXP_LOG ("STATE_WRITE", NULL);
        dwTotalNeeded = buffer[0] | (buffer[1] << 8) |
        (buffer[2] << 16) | (buffer[3] << 24);
        dwNeeded = (dwTotalNeeded > BUFSIZE) ? BUFSIZE : dwTotalNeeded;
        dwState = STATE_WRITE_DATA;
        break;
      case STATE_WRITE_DATA:
        EXP_LOG ("STATE_WRITE_DATA: '%s'", buffer);
        if (WriteBufferToSlave(FALSE, hConsoleInW, hConsoleOut,
                               buffer, dwNeeded) == FALSE)
          {
            EXP_LOG("Unable to write to slave: 0x%x", GetLastError());
          }
#if 0
        //  The following is part of an attempt at removing the 'echo' from
        //  the console. Mainly aimed to making MS Telnet working, this is
        //  incorrect as this may remove part of the actual output
        //  (sending 'gnatmake' will remove the first g of 'gcc -c ...')
        ExpReading = (char*) malloc (dwNeeded + 1);
        memcpy (ExpReading, buffer, dwNeeded);
        ExpReading [dwNeeded] = '\0';
#endif
        EXP_LOG_FLUSH;
        dwTotalNeeded -= dwNeeded;
        if (dwTotalNeeded) {
          dwNeeded = (dwTotalNeeded > BUFSIZE) ?
          BUFSIZE : dwTotalNeeded;
        } else {
          dwNeeded = 1;
          dwState = STATE_WAIT_CMD;
        }
        break;
      case STATE_KEY:
      case STATE_MOUSE:
        EXP_LOG ("STATE_KEY or STATE_MOUSE", NULL);
        /* XXX: To be implemented */
        break;
      default:
        /* If we ever get here, there is a problem */
        EXP_LOG("Unexpected state\n", 0);
        break;
    }
  }
}


/*
 *----------------------------------------------------------------------
 *
 * InitializeWaitQueue --
 *
 *	Set up the initial wait queue
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */
static void
InitializeWaitQueue(void)
{
    int i;
    ExpWaitEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    ExpWaitMutex = CreateMutex(NULL, FALSE, NULL);
    ExpWaitCount = 0;
    for (i = 0; i < EXP_MAX_QLEN; i++) {
	ExpWaitQueue[i] = INVALID_HANDLE_VALUE;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ExpAddToWaitQueue --
 *
 *	Adds a handle to the wait queue.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */
void
ExpAddToWaitQueue(HANDLE handle)
{
    DWORD i;

    WaitForSingleObject(ExpWaitMutex, INFINITE);
    for (i = 0; i < EXP_MAX_QLEN; i++) {
	if (ExpWaitQueue[i] == INVALID_HANDLE_VALUE) {
	    ExpWaitQueue[i] = handle;
	    ExpWaitCount++;
	    break;
	}
    }
    ReleaseMutex(ExpWaitMutex);
    SetEvent(ExpWaitEvent);
}

/*
 *----------------------------------------------------------------------
 *
 * ExpWriteMaster --
 *
 *	Write to the Expect master using either a socket or a pipe
 *
 * Results:
 *	TRUE if successful, FALSE if not
 *
 *----------------------------------------------------------------------
 */

BOOL
ExpWriteMaster(HANDLE hFile, LPCVOID buf, DWORD n)
{
  DWORD count, dwResult;
  BOOL bRet;
  WSABUF wsabuf[1];
  CHAR buf2[n+1];
  int start,start2;
  int ExpReadingStart;
  int i;

  EXP_LOG_FLUSH;
  start = 0;
  start2 = 0;
  EXP_LOG ("ExpWriteMaster Received %d bytes", n);
#if 0
  //  The following is part of an attempt at removing the 'echo' from
  //  the console. Mainly aimed to making MS Telnet working, this is
  //  incorrect as this may remove part of the actual output
  //  (sending 'gnatmake' will remove the first g of 'gcc -c ...')
  if (ExpReading != NULL) {
    EXP_LOG ("Need to skip: '%s'", ExpReading);
    while ((start < n) &&
           (ExpReading[start] == ((char*)buf)[start]) &&
           (ExpReading[start]!='\0'))
    {
      start++;
    }
    start2 = start;
    EXP_LOG ("Nb characters = %d", n);
    if ((n == CONSOLE_WINDOW_WIDTH - 2) &&
        (start == 0) &&
        (((char*)buf)[0] == '<'))
    {
      // The Windows console might have scrolled horizontally... damned
      // In this case, the console does a complete refresh of the screen
      // which is 79 characters long - 1 for the cursor, and -1 for the one
      // that is inserted. It starts with a '<'
      // The newly received characters starts at position 51, and the
      // remaining is filled with spaces
      start2 = CONSOLE_WINDOW_WIDTH - 28;
      EXP_LOG ("51st character is %c\n", ((char*)buf)[start2]);
      //  Check if first character is a space...
      if (ExpReading[start] == ' ') {
        // add this one, check for following ones
        start++;
        start2++;
      }
      while ((start2 < n) &&
             (ExpReading[start] == ((char*)buf)[start2]) &&
             (ExpReading[start]!='\0') &&
             (ExpReading[start]!=' '))
      {
        start++;
        start2++;
      }
      while ((start2 < n) && ((char*)buf)[start2] == ' ') {
        start2++;
      }
    }
  }
  EXP_LOG ("start2 is %d", start2);
  if (ExpReading != NULL) {
    if (start2 == n) {
      // Skip all incoming buffer
      char *tmp;
      tmp = malloc (strlen (ExpReading) - start + 1);
      memcpy (tmp, &ExpReading[start], strlen (ExpReading) - start + 1);
      free (ExpReading);
      ExpReading = tmp;
    } else {
      // We received something that has nothing to do with what we sent.
      // or all ExpReading matched.
      free (ExpReading);
      ExpReading = NULL;
    }
  }
#endif

  EXP_LOG ("n-start2 is %d", n-start2);
  if (start2 < n) {
    memcpy (buf2, &((char*)buf)[start2], n-start2);
    buf2[n-start2]='\0';

    EXP_LOG ("ExpWriteMaster :'%s'", buf2);
    // End Debug
    bRet = WriteFile(hFile, buf2, n-start2, &count, NULL);
    if (!bRet) EXP_LOG ("Error writing to master %8x\n", GetLastError());
    return bRet;
  }
  return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * ExpReadMaster --
 *
 *	Read from the Expect master using either a socket or a pipe
 *
 * Results:
 *	TRUE if successful, FALSE if not.  If we hit and end of file
 *	condition, returns TRUE with *pCount = 0
 *
 *----------------------------------------------------------------------
 */

BOOL
ExpReadMaster(HANDLE hFile, void *buf, DWORD n,
	      PDWORD pCount, PDWORD pError)
{
    int x;
    WSABUF wsabuf[1];
    HANDLE hnd[2];
    BOOL bRet;
    DWORD dwResult;
    DWORD flags;
    OVERLAPPED over;

    EXP_LOG ("ExpReadMaster", NULL);
    *pError = 0;
    ZeroMemory (&over, sizeof (over));
    over.hEvent = CreateEvent (NULL, TRUE, FALSE, NULL);
    bRet = ReadFile(hFile, buf, n, pCount, &over);
    if (!bRet) {
      dwResult = GetLastError();
    }
    if (bRet == FALSE) {
      if (dwResult == ERROR_IO_PENDING) {
	EXP_LOG ("IO_PENDING in ExpReadMaster", NULL);
	hnd[0] = hShutdown;
	hnd[1] = over.hEvent;
	bRet = WaitForMultipleObjects(2, hnd, FALSE, INFINITE);
	EXP_LOG ("in ExpReadMaster: Wait returned", NULL);
	if( bRet == WAIT_OBJECT_0 )
	  {
	    EXP_LOG ("WAIT_OBJECT_0", NULL);
	    /*
	     * We have been instructed to shut down.
	     */
	    *pCount = 0;
	    bRet = TRUE;
	  }
	else
	  {
	    bRet = GetOverlappedResult(hFile, &over, pCount, TRUE);
	    if (bRet == FALSE) {
	      dwResult = GetLastError();
	    }
	    EXP_LOG ("over.hEvent returned %d bytes read", *pCount);
	  }
      } else if (dwResult == ERROR_HANDLE_EOF ||
		 dwResult == ERROR_BROKEN_PIPE) {
	EXP_LOG ("PIPE EOF or BROKEN", NULL);
	*pCount = 0;
	bRet = TRUE;
      }
      *pError = dwResult;
    }
    CloseHandle (over.hEvent);
    return bRet;
}

/*
 *----------------------------------------------------------------------
 *
 * PipeRespondToMaster --
 *
 *	Sends back an error response to the Expect master.
 *
 * Results:
 *	TRUE if successful, FALSE if not
 *
 *----------------------------------------------------------------------
 */
BOOL
PipeRespondToMaster(HANDLE handle, DWORD value, DWORD pid)
{
    UCHAR buf[8];

    buf[0] = (UCHAR) (value & 0x000000ff);
    buf[1] = (UCHAR) ((value & 0x0000ff00) >> 8);
    buf[2] = (UCHAR) ((value & 0x00ff0000) >> 16);
    buf[3] = (UCHAR) ((value & 0xff000000) >> 24);
    buf[4] = (UCHAR) (pid & 0x000000ff);
    buf[5] = (UCHAR) ((pid & 0x0000ff00) >> 8);
    buf[6] = (UCHAR) ((pid & 0x00ff0000) >> 16);
    buf[7] = (UCHAR) ((pid & 0xff000000) >> 24);

    return ExpWriteMaster(handle, buf, sizeof(buf));
}

/*
 *----------------------------------------------------------------------
 *
 * ConvertAsciiToKeyEvents --
 *
 *	Converts an ASCII character to an KEY_EVENT_RECORD.
 *
 * Results:
 *	Number of input record that were filled in here.  Currently,
 *	this routine should never be called with less than 6 empty
 *	slots that could be filled.
 *
 *----------------------------------------------------------------------
 */
static DWORD
ConvertAsciiToKeyEvents(UCHAR c, KEY_EVENT_RECORD *keyRecord)
{
    UCHAR lc;
    DWORD mods;
    DWORD n;

    n = 0;
    lc = c < 128 ? c : c - 128;
    mods = ExpAsciiToKeyArray[lc].dwControlKeyState;

    keyRecord->bKeyDown = TRUE;
    keyRecord->wRepeatCount = 1;
    keyRecord->wVirtualKeyCode = ExpAsciiToKeyArray[lc].wVirtualKeyCode;
    keyRecord->wVirtualScanCode = ExpAsciiToKeyArray[lc].wVirtualScanCode;
    keyRecord->dwControlKeyState = ExpAsciiToKeyArray[lc].dwControlKeyState;
    keyRecord->uChar.AsciiChar = c;
    keyRecord++; n++;

    keyRecord->bKeyDown = FALSE;
    keyRecord->wRepeatCount = 1;
    keyRecord->wVirtualKeyCode = ExpAsciiToKeyArray[lc].wVirtualKeyCode;
    keyRecord->wVirtualScanCode = ExpAsciiToKeyArray[lc].wVirtualScanCode;
    keyRecord->dwControlKeyState = ExpAsciiToKeyArray[lc].dwControlKeyState;
    keyRecord->uChar.AsciiChar = c;
    keyRecord++; n++;

    return n;
}

/*
 *----------------------------------------------------------------------
 *
 * ConvertFKeyToKeyEvents --
 *
 *	Converts a function key to an KEY_EVENT_RECORD.
 *
 * Results:
 *	Number of input record that were filled in here.  Currently,
 *	this routine should never be called with less than 6 empty
 *	slots that could be filled.
 *
 *----------------------------------------------------------------------
 */
static DWORD
ConvertFKeyToKeyEvents(DWORD fk, KEY_EVENT_RECORD *keyRecord)
{
    DWORD n;

    n = 0;

    keyRecord->bKeyDown = TRUE;
    keyRecord->wRepeatCount = 1;
    keyRecord->wVirtualKeyCode = ExpFunctionToKeyArray[fk].wVirtualKeyCode;
    keyRecord->wVirtualScanCode = ExpFunctionToKeyArray[fk].wVirtualScanCode;
    keyRecord->dwControlKeyState = ExpFunctionToKeyArray[fk].dwControlKeyState;
    keyRecord->uChar.AsciiChar = 0;
    keyRecord++; n++;

    keyRecord->bKeyDown = FALSE;
    keyRecord->wRepeatCount = 1;
    keyRecord->wVirtualKeyCode = ExpFunctionToKeyArray[fk].wVirtualKeyCode;
    keyRecord->wVirtualScanCode = ExpFunctionToKeyArray[fk].wVirtualScanCode;
    keyRecord->dwControlKeyState = ExpFunctionToKeyArray[fk].dwControlKeyState;
    keyRecord->uChar.AsciiChar = 0;
    keyRecord++; n++;

    return n;
}

/*
 *----------------------------------------------------------------------
 *
 * FindEscapeKey --
 *
 *	Search for a matching escape key sequence
 *
 * Results:
 *	The matching key if found, -1 if not found, -2 if a partial match
 *
 *----------------------------------------------------------------------
 */
static int
FindEscapeKey(PUCHAR buf, DWORD buflen)
{
    DWORD len;
    int i;

    for (i = 0; VtFunctionKeys[i].sequence; i++) {
	len = strlen(VtFunctionKeys[i].sequence);
	if (len == buflen) {
	    if (strncmp(VtFunctionKeys[i].sequence, buf, buflen) == 0) {
		return VtFunctionKeys[i].keyval;
	    }
	} else {
	    if (strncmp(VtFunctionKeys[i].sequence, buf, buflen) == 0) {
		/* Partial match */
		return -2;
	    }
	}
    }
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * FlushInputRecords --
 *
 *	Takes a stack of input records and flushes them
 *
 * Results:
 *	TRUE if successful, FALSE if unsuccessful
 *
 *----------------------------------------------------------------------
 */
static BOOL
FlushInputRecords(HANDLE hConsole, INPUT_RECORD *records, DWORD pos)
{
    DWORD j = 0;
    DWORD nWritten;

    while (j != pos) {
	if (! WriteConsoleInput(hConsole, &records[j], pos-j, &nWritten)) {
	    return FALSE;
	}
	j += nWritten;
    }
    return TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * WriteBufferToSlave --
 *
 *	Takes an input buffer, and it generates key commands to drive
 *	the slave program.
 *
 * Results:
 *	TRUE if successful, FALSE if unsuccessful.
 *
 * Side Effects:
 *	Characters are entered into the console input buffer.
 *
 * Notes:
 *	XXX: We need to be able to timeout if an escape is in
 *	the buffer and no further control sequences come in within
 *	a reasonable amount of time, say a 1/4 second.
 *
 *----------------------------------------------------------------------
 */
static BOOL
WriteBufferToSlave(int noEscapes, HANDLE hConsoleInW, HANDLE hConsoleOut,
		   PUCHAR buf, DWORD n)
{
  INPUT_RECORD ibuf[1024];
  DWORD i;
  DWORD pos;
  PUCHAR p;
  int key, cols, rows;
  static UCHAR saveBuf[10];
  static int savePos = 0;

#define MAX_RECORDS 1000
  for (pos = 0, i = 0; i < n; i++) {

    if (!noEscapes && buf[i] == '\033') {
      if (FlushInputRecords(hConsoleInW, ibuf, pos) == FALSE) {
	return FALSE;
      }
      pos = 0;
      if (savePos) {
	WriteBufferToSlave(TRUE, hConsoleInW, hConsoleOut,
			   saveBuf, savePos);
	savePos = 0;
      }
      saveBuf[savePos++] = buf[i];
      continue;
    }

    if (!noEscapes && savePos) {
      saveBuf[savePos++] = buf[i];
      key = FindEscapeKey(&saveBuf[1], savePos-1);
      /* Check for partial match */
      if (key == -2) {
	continue;
      } else if (key == EXP_WIN_RESIZE) {
	p = &buf[i+1];
	cols = strtoul((char *)p, (char **)&p, 10);
	if (! p) goto flush;
	if (*p++ != ';') goto flush;
	rows = strtoul((char *)p, (char **)&p, 10);
	if (! p) goto flush;
	if (*p++ != 'G') goto flush;
	ExpSetConsoleSize(hConsoleInW, hConsoleOut, cols, rows);
	i += p - &buf[i+1];
      } else if (key >= 0) {
	ibuf[pos].EventType = KEY_EVENT;
	pos += ConvertFKeyToKeyEvents(key, &ibuf[pos].Event.KeyEvent);
      } else {
      flush:
	WriteBufferToSlave(TRUE, hConsoleInW, hConsoleOut,
			   saveBuf, savePos);
      }
      savePos = 0;
      continue;
    }

    ibuf[pos].EventType = KEY_EVENT;
    pos += ConvertAsciiToKeyEvents(buf[i], &ibuf[pos].Event.KeyEvent);
    if (pos >= MAX_RECORDS - 6) {
      if (FlushInputRecords(hConsoleInW, ibuf, pos) == FALSE) {
        return FALSE;
      }
      pos = 0;
    }
  }
  if (FlushInputRecords(hConsoleInW, ibuf, pos) == FALSE) {
    return FALSE;
  }
  return TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * WaitQueueThread --
 *
 *	Wait for all subprocesses to exit before exiting this process.
 *	Exit with the value of the primary child.
 *
 * Results:
 *	None really.  This exits the process.  A zero is return to make
 *	the compiler happy.
 *
 *----------------------------------------------------------------------
 */
static DWORD WINAPI
WaitQueueThread(LPVOID *arg)
{
    HANDLE slavePid = *((PHANDLE *) arg);
    HANDLE hEvents[EXP_MAX_QLEN+1];
    DWORD  posEvents[EXP_MAX_QLEN+1];
    DWORD n, val, err;
    int i;

    ResetEvent(ExpWaitEvent);
    hEvents[0] = ExpWaitEvent;
    while (ExpWaitCount > 0) {
      EXP_LOG ("ExpWaitCount = %d", ExpWaitCount);
      n = 1;
      WaitForSingleObject(ExpWaitMutex, INFINITE);
      for (i = 0; i < EXP_MAX_QLEN; i++) {
	if (ExpWaitQueue[i] != INVALID_HANDLE_VALUE) {
	  hEvents[n] = ExpWaitQueue[i];
	  posEvents[n] = i;
	  n++;
	}
      }
      ReleaseMutex(ExpWaitMutex);
      val = WaitForMultipleObjects(n, hEvents, FALSE, INFINITE);
      EXP_LOG ("WaitForMultipleObjects returned", NULL);
      if (val == WAIT_FAILED) {
	err = GetLastError();
	printf("WAIT_FAILED: 0x%x\n", err);
      } else if (val >= WAIT_ABANDONED_0 && val < WAIT_ABANDONED_0 + n) {
	val -= WAIT_ABANDONED_0;
	err = GetLastError();
	printf("WAIT_ABANDONED %d: 0x%x\n", val, err);
      } else {
	val -= WAIT_OBJECT_0;
	if (val > 0) {
	  EXP_LOG ("Object %d returned", val);
	  if (hEvents[val] == (HANDLE) slavePid) {
	    EXP_LOG ("Slave finished", NULL);
	    Exp_WaitPid(slavePid, &exitVal, 0);
	    EXP_LOG ("Exp_WaitPid returned with exit val %d", exitVal);
	  } else {
	    CloseHandle(hEvents[val]);
	  }
	  ExpWaitCount--;
	  WaitForSingleObject(ExpWaitMutex, INFINITE);
	  ExpWaitQueue[posEvents[val]] = INVALID_HANDLE_VALUE;
	  ReleaseMutex(ExpWaitMutex);
	}
      }
    }

    EXP_LOG ("Closing handles", NULL);
    CloseHandle(ExpWaitEvent);
    CloseHandle(ExpWaitMutex);

    /*
     * When this thread exits, the main thread will be free to return.  Just
     * set the event so that the main thread knows what's up.
     */
    EXP_LOG ("Set shutdown handle", NULL);
    SetEvent(hShutdown);

    return 0;
}

/*
 *-------------------------------------------------------------------------
 *
 * setargv --
 *
 *	Parse the Windows command line string into argc/argv.  Done here
 *	because we don't trust the builtin argument parser in crt0.
 *	Windows applications are responsible for breaking their command
 *	line into arguments.
 *
 *	2N backslashes + quote -> N backslashes + begin quoted string
 *	2N + 1 backslashes + quote -> literal
 *	N backslashes + non-quote -> literal
 *	quote + quote in a quoted string -> single quote
 *	quote + quote not in quoted string -> empty string
 *	quote -> begin quoted string
 *
 * Results:
 *	Fills argcPtr with the number of arguments and argvPtr with the
 *	array of arguments.
 *
 * Side effects:
 *	Memory allocated.
 *
 *--------------------------------------------------------------------------
 */

static void
SetArgv(char *cmdLine, int *argcPtr, char ***argvPtr)
{
    char *p, *arg, *argSpace;
    char **argv;
    int argc, size, inquote, copy, slashes;

    /*
     * Precompute an overly pessimistic guess at the number of arguments
     * in the command line by counting non-space spans.
     */

    size = 2;
    for (p = cmdLine; *p != '\0'; p++) {
	if (isspace(*p)) {
	    size++;
	    while (isspace(*p)) {
		p++;
	    }
	    if (*p == '\0') {
		break;
	    }
	}
    }
    argSpace = (char *) ckalloc((unsigned) (size * sizeof(char *)
	    + strlen(cmdLine) + 1));
    argv = (char **) argSpace;
    argSpace += size * sizeof(char *);
    size--;

    p = cmdLine;
    for (argc = 0; argc < size; argc++) {
	argv[argc] = arg = argSpace;
	while (isspace(*p)) {
	    p++;
	}
	if (*p == '\0') {
	    break;
	}

	inquote = 0;
	slashes = 0;
	while (1) {
	    copy = 1;
	    while (*p == '\\') {
		slashes++;
		p++;
	    }
	    if (*p == '"') {
		if ((slashes & 1) == 0) {
		    copy = 0;
		    if ((inquote) && (p[1] == '"')) {
			p++;
			copy = 1;
		    } else {
			inquote = !inquote;
		    }
                }
                slashes >>= 1;
            }

            while (slashes) {
		*arg = '\\';
		arg++;
		slashes--;
	    }

	    if ((*p == '\0') || (!inquote && isspace(*p))) {
		break;
	    }
	    if (copy != 0) {
		*arg = *p;
		arg++;
	    }
	    p++;
        }
	*arg = '\0';
	argSpace = arg + 1;
    }
    argv[argc] = NULL;

    *argcPtr = argc;
    *argvPtr = argv;
}
