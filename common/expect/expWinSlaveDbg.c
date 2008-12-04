/*
 * expWinSlaveDbg.c --
 *
 *	The slave driver acts as a debugger for the slave program.  It
 *	does this so that we can determine echoing behavior of the
 *	subprocess.  This isn't perfect as the subprocess can change
 *	echoing behavior while our keystrokes are lying in its console
 *	input buffer, but it is much better than nothing.  The debugger
 *	thread sets up breakpoints on the functions we want to intercept,
 *	and it writes data that is written directly to the console to
 *	the master pipe.
 *
 * Copyright (c) 2006-2008 AdaCore
 * Copyright (c) 1997 by Mitel Corporation
 * Copyright (c) 1997-1998 by Gordon Chaffee
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * TODO:
 *  * Maintain cursor information for each console screen buffer.
 *  * Intercept additional console input and output characters to better
 *    keep track of current console state.
 *  * Keep all keyboard strokes within the slave until we see a call
 *    made ReadConsoleInput, ReadConsole, or some call like that.
 *    Maybe a better idea is to not echo characters until we see how
 *    they are read.  So never write more than a line at a time to
 *    the console input, but as soon as wee see a call to ReadConsole, we
 *    echo characters (if necessary) on the way into the call.
 *    If the call is made instead to ReadConsoleInput, then we remove
 *    a character from our echo list (assuming of course the input
 *    event was a key stroke).  This would give the most accurate
 *    accounting of characters.
 *  * I've been having trouble with cmd.exe.  If there is a file such asa
 *    x.in that you try to run, and there is no application tied to .in,
 *    a graphical message pops up telling you there is no program associated
 *    with this file.  For some reason, if I run cmd.exe under apispy32
 *    from Matt Pietrek, the graphical message doesn't pop up.  I tried
 *    starting the program with the same sort of flags as he uses, but it
 *    doesn't seem to work to make the messages go away.  I suspect that
 *    the messages are coming from the shell somehow.
 */

/*
 * Even though we won't have access to most of the commands, use the
 * same headers
 */

#include <windows.h>
#include <imagehlp.h>
#include <stddef.h>
#include "tclHash.h"
#include "expWin.h"
#include "expWinSlave.h"
#include <assert.h>

#ifndef PCOORD
#define PCOORD COORD*
#endif

#define SINGLE_STEP_BIT 0x100;	/* This only works on the x86 processor */

#define EXP_FLAG_APP_NAME		0x01
#define EXP_FLAG_CMD_LINE		0x02
#define EXP_FLAG_PROC_ATTRS		0x04
#define EXP_FLAG_THREAD_ATTRS		0x08
#define EXP_FLAG_ENVIRONMENT		0x10
#define EXP_FLAG_CURR_DIR		0x20
#define EXP_FLAG_SI			0x40
#define EXP_FLAG_PI			0x80

#ifndef UNICODE
SHORT curY = 0;
SHORT curX = 0;
#  define ExpCreateProcessInfo	ExpCreateProcessInfoA
#  define OnWriteConsoleOutput	OnWriteConsoleOutputA
#  define ReadSubprocessString	ReadSubprocessStringA
#  define StartSubprocess	StartSubprocessW
#else
#  define ExpCreateProcessInfo	ExpCreateProcessInfoW
#  define OnWriteConsoleOutput	OnWriteConsoleOutputW
#  define ReadSubprocessString	ReadSubprocessStringW
#  define StartSubprocess	StartSubprocessA
#endif

typedef struct _ExpProcess ExpProcess;
typedef struct _ExpBreakpoint ExpBreakpoint;

typedef struct _ExpCreateProcessInfo {
    TCHAR appName[8192];
    TCHAR cmdLine[8192];
    SECURITY_ATTRIBUTES procAttrs;
    SECURITY_ATTRIBUTES threadAttrs;
    BOOL bInheritHandles;
    DWORD dwCreationFlags;
    LPVOID lpEnvironment;
    TCHAR currDir[8192];
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    PVOID piPtr;		/* Pointer to PROCESS_INFORMATION in slave */
    DWORD flags;
} ExpCreateProcessInfo;

typedef struct _CreateProcessThreadArgs {
    ExpCreateProcessInfo *cp;
    ExpProcess *proc;
    ExpSlaveDebugArg debugInfo;
} CreateProcessThreadArgs;

typedef struct _ExpThreadInfo {
    HANDLE hThread;
    DWORD dwThreadId;
    DWORD nargs;
    DWORD args[16];		/* Space for saving 16 args.  We need this
				 * space while we are waiting for the return
				 * value for the function. */
    LPCONTEXT context;		/* Current context */

    ExpCreateProcessInfo *createProcess; /* Create process structure */
    struct _ExpThreadInfo *nextPtr;
} ExpThreadInfo;

typedef void (ExpBreakProc) (ExpProcess *, ExpThreadInfo *,
    ExpBreakpoint *, PDWORD returnValue, DWORD direction);

typedef struct _ExpBreakInfo {
    PUCHAR funcName;		/* Name of function to intercept */
    DWORD nargs;		/* Number of arguments */
    ExpBreakProc *breakProc;	/* Function to call when a breakpoint is hit */
#define EXP_BREAK_IN	1	/* Call handler on the way in */
#define EXP_BREAK_OUT	2	/* Call handler on the way out */
    DWORD dwFlags;		/* Bits for direction to call handler in */
} ExpBreakInfo;

typedef struct _ExpDllBreakpoints {
    PUCHAR dllName;
    ExpBreakInfo *breakInfo;
} ExpDllBreakpoints;

struct _ExpBreakpoint {
    BOOL returning;		/* Is this a returning breakpoint? */
    UCHAR code;			/* Original code */
    PVOID codePtr;		/* Address of original code */
    PVOID codeReturnPtr;	/* Address of return breakpoint */
    DWORD origRetAddr;		/* Original return address */
    ExpBreakInfo *breakInfo;	/* Information about the breakpoint */
    ExpThreadInfo *threadInfo;	/* If this breakpoint is for a specific
				 * thread */
    struct _ExpBreakpoint *nextPtr;
};

typedef struct {
    BOOL loaded;
    HANDLE hFile;
    LPVOID baseAddr;
    PCHAR  modName;
    PIMAGE_DEBUG_INFORMATION dbgInfo;
} ExpModule;

#define PAGESIZE 0x1000
#define PAGEMASK (PAGESIZE-1)

/*
 * There is one of these structures for each subprocess that we are
 * controlling.
 */
struct _ExpProcess {
    ExpThreadInfo *threadList;	/* List of threads in the subprocess */
    ExpBreakpoint *brkptList;/* List of breakpoints in the subprocess */
    ExpBreakpoint *lastBrkpt;/* Last Breakpoint Hit */
    DWORD offset;		/* Breakpoint offset in allocated mem */
    DWORD nBreakCount;		/* Number of breakpoints hit */
    DWORD consoleHandles[100];	/* A list of input console handles */
    DWORD consoleHandlesMax;
    BOOL  isConsoleApp;		/* Is this a console app? */
    BOOL  isShell;		/* Is this some sort of console shell? */
    HANDLE hProcess;		/* handle to subprocess */
    DWORD hPid;			/* Global process id */
    DWORD threadCount;		/* Number of threads in process */
    DWORD pSubprocessMemory;	/* Pointer to allocated memory in subprocess */
    DWORD pSubprocessBuffer;	/* Pointer to buffer memory in subprocess */
    DWORD pMemoryCacheBase;	/* Base address of memory cache */
    BYTE  pMemoryCache[PAGESIZE]; /* Subprocess memory cache */
    Tcl_HashTable *funcTable;	/* Function table name to address mapping */
    Tcl_HashTable *moduleTable;	/* Win32 modules that have been loaded */
    ExpModule *exeModule;	/* Executable module info */
    struct _ExpProcess *nextPtr;
};

/*
 * List of processes that are being debugged
 */
static ExpProcess *ProcessList = NULL;
static HANDLE HConsole;		/* Shared console */
static HANDLE HMaster;		/* Handle to master output pipe */
static int UseSocket;
static COORD CursorPosition;
static BOOL CursorKnown = FALSE; /* Do we know where the remote cursor is? */
static COORD ConsoleSize = {80, 25};

static char *SymbolPath;

/*
 * Static functions in this file:
 */

extern void		ExpCommonDebugger();
extern BOOL		ReadSubprocessMemory(ExpProcess *proc, LPVOID addr,
			    LPVOID buf, DWORD len);
extern int		ReadSubprocessStringA(ExpProcess *proc, PVOID base,
			    PCHAR buf, int buflen);
extern int		ReadSubprocessStringW(ExpProcess *proc, PVOID base,
			    PWCHAR buf, int buflen);
extern BOOL		WriteSubprocessMemory(ExpProcess *proc, LPVOID addr,
			    LPVOID buf, DWORD len);

static DWORD WINAPI	CreateProcessThread(LPVOID *lparg);
extern void		CreateVtSequence(ExpProcess *, COORD newPos, DWORD n);
static BOOL		SetBreakpoint(ExpProcess *, ExpBreakInfo *);
extern ExpBreakpoint *	SetBreakpointAtAddr(ExpProcess *, ExpBreakInfo *,
			    PVOID funcPtr);
static void		StartSubprocessA(ExpProcess *, ExpThreadInfo *);
static void		StartSubprocessW(ExpProcess *, ExpThreadInfo *);
static void		RefreshScreen();

static void		OnBeep(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnGetStdHandle(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnIsWindowVisible(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnOpenConsoleW(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnReadConsoleInput(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnSetConsoleActiveScreenBuffer(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnSetConsoleCursorPosition(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnSetConsoleMode(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnSetConsoleWindowInfo(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnScrollConsoleScreenBuffer(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnWriteConsoleA(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnWriteConsoleW(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnWriteConsoleOutputA(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
extern void		OnWriteConsoleOutputW(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnWriteConsoleOutputCharacterA(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);
static void		OnWriteConsoleOutputCharacterW(ExpProcess *,
			    ExpThreadInfo *, ExpBreakpoint *, PDWORD, DWORD);

static void		OnXBreakpoint(ExpProcess *, LPDEBUG_EVENT);
static void		OnXCreateProcess(ExpProcess *, LPDEBUG_EVENT);
static void		OnXCreateThread(ExpProcess *, LPDEBUG_EVENT);
static void		OnXDeleteThread(ExpProcess *, LPDEBUG_EVENT);
static void		OnXFirstBreakpoint(ExpProcess *, LPDEBUG_EVENT);
static void		OnXLoadDll(ExpProcess *, LPDEBUG_EVENT);
static void		OnXUnloadDll(ExpProcess *, LPDEBUG_EVENT);
static void		OnXSecondBreakpoint(ExpProcess *, LPDEBUG_EVENT);
static void		OnXSecondChanceException(ExpProcess *,LPDEBUG_EVENT);
static void		OnXSingleStep(ExpProcess *, LPDEBUG_EVENT);

#ifndef UNICODE

/*
 * Functions where we set breakpoints
 */

ExpBreakInfo BreakArrayKernel32[] = {
  {"SetConsoleMode", 2, OnSetConsoleMode, EXP_BREAK_OUT},
  {"WriteConsoleA", 5, OnWriteConsoleA, EXP_BREAK_OUT},
  {"WriteConsoleW", 5, OnWriteConsoleW, EXP_BREAK_OUT},
  {"WriteConsoleOutputA", 5, OnWriteConsoleOutputA, EXP_BREAK_OUT},
  {"WriteConsoleOutputW", 5, OnWriteConsoleOutputW, EXP_BREAK_OUT},
  {"WriteConsoleOutputCharacterA", 5,
   OnWriteConsoleOutputCharacterA, EXP_BREAK_OUT},
  {"WriteConsoleOutputCharacterW", 5,
   OnWriteConsoleOutputCharacterW, EXP_BREAK_OUT|EXP_BREAK_IN},
  {"SetConsoleCursorPosition", 5, OnSetConsoleCursorPosition, EXP_BREAK_OUT},
  {NULL, 0, NULL}
};

ExpBreakInfo BreakArrayUser32[] = {
    {"IsWindowVisible", 1, OnIsWindowVisible, EXP_BREAK_OUT},
    {NULL, 0, NULL}
};

/*
 * Structure with all the breakpoints we want to set
 */
ExpDllBreakpoints BreakPoints[] = {
    {"kernel32.dll", BreakArrayKernel32},
#if 0
    {"user32.dll", BreakArrayUser32},
#endif
    {NULL, NULL}
};

#endif /* !UNICODE */

#ifndef UNICODE


/*
 *----------------------------------------------------------------------
 *
 * ExpProcessNew --
 *
 *	Allocates a new structure for debugging a process and
 *	initializes it.
 *
 * Results:
 *	A new structure
 *
 * Side Effects:
 *	Memory is allocated, an event is created.
 *
 *----------------------------------------------------------------------
 */

static ExpProcess *
ExpProcessNew(void)
{
    ExpProcess *proc;
    proc = malloc(sizeof(ExpProcess));
    proc->threadList = NULL;
    proc->threadCount = 0;
    proc->brkptList = NULL;
    proc->lastBrkpt = NULL;
    proc->offset = 0;
    proc->nBreakCount = 0;
    proc->consoleHandlesMax = 0;
    proc->isConsoleApp = FALSE;
    proc->isShell = FALSE;
    proc->hProcess = NULL;
    proc->pSubprocessMemory = 0;
    proc->pSubprocessBuffer = 0;
    proc->pMemoryCacheBase = 0;
    proc->funcTable = malloc(sizeof(Tcl_HashTable));
    Tcl_InitHashTable(proc->funcTable, TCL_STRING_KEYS);
    proc->moduleTable = malloc(sizeof(Tcl_HashTable));
    Tcl_InitHashTable(proc->moduleTable, TCL_ONE_WORD_KEYS);
    proc->exeModule = NULL;
    proc->nextPtr = ProcessList;
    ProcessList = proc;
    return proc;
}

/*
 *----------------------------------------------------------------------
 *
 * ExpProcessFree --
 *
 *	Frees all allocated memory for a process and closes any
 *	open handles
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
ExpProcessFree(ExpProcess *proc)
{
    ExpThreadInfo *tcurr, *tnext;
    ExpBreakpoint *bcurr, *bnext;
    ExpProcess *pcurr, *pprev;

    EXP_LOG ("ExpProcessFree PID=%d", proc->hPid);
    for (tcurr = proc->threadList; tcurr != NULL; tcurr = tnext) {
	tnext = tcurr->nextPtr;
	proc->threadCount--;
	CloseHandle(tcurr->hThread);
	free(tcurr);
    }
    for (bcurr = proc->brkptList; bcurr != NULL; bcurr = bnext) {
	bnext = bcurr->nextPtr;
	free(bcurr);
    }
    Tcl_DeleteHashTable(proc->funcTable);
    free(proc->funcTable);
    Tcl_DeleteHashTable(proc->moduleTable);
    free(proc->moduleTable);

    for (pprev = NULL, pcurr = ProcessList; pcurr != NULL;
	 pprev = pcurr, pcurr = pprev->nextPtr)
    {
      if (pcurr == proc) {
	if (pprev == NULL) {
	  ProcessList = pcurr->nextPtr;
	} else {
	  pprev->nextPtr = pcurr->nextPtr;
	}
	break;
      }
    }

    free(proc);
}

/*
 *----------------------------------------------------------------------
 *
 * ExpProcessFreeByHandle --
 *
 *	Fine a process structure by its handle and free it.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */
void
ExpProcessFreeByHandle(HANDLE hProcess)
{
    ExpProcess *proc;
    for (proc = ProcessList; proc != NULL; proc = proc->nextPtr) {
	if (proc->hProcess == hProcess) {
	    ExpProcessFree(proc);
	    return;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ExpKillProcessList --
 *
 *	Runs through the current list of slave processes and kills
 *	them all.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Processes are terminated.
 *
 *----------------------------------------------------------------------
 */

void
ExpKillProcessList()
{
    ExpProcess *proc;

    for (proc = ProcessList; proc != NULL; proc = proc->nextPtr) {
	Exp_KillProcess(proc->hProcess);
	if (proc->hProcess != NULL) {
	    if (WaitForSingleObject(proc->hProcess, 10000) == WAIT_TIMEOUT) {
		Exp_KillProcess(proc->hProcess);
	    }
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ExpSlaveDebugThread --
 *
 *	Acts as a debugger for a subprocess created by the spawn command.
 *
 * Results:
 *	None.  This thread exits with ExitThread() when the subprocess dies.
 *
 * Side Effects:
 *	A process is created.
 *
 *----------------------------------------------------------------------
 */

DWORD WINAPI
ExpSlaveDebugThread(LPVOID *lparg)
{
    ExpSlaveDebugArg *arg = (ExpSlaveDebugArg *) lparg;
    ExpProcess *proc;
    BOOLEAN bRet;
    PROCESS_INFORMATION procinfo;

    arg->result = 0;

    HConsole = arg->hConsole;

    HMaster = CreateFile(arg->hMasterPipe, GENERIC_WRITE,
			 FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
    if (HMaster == NULL) {
      arg->result = TRUE;
      arg->lastError = GetLastError();
      EXP_LOG ("Error while opening pipe: 0x%8x", arg->lastError);
    }

    /* Make sure the child does not ignore Ctrl-C */
    if (!arg->result) {
      SetConsoleCtrlHandler(NULL, FALSE);
      arg->result =
	ExpCreateProcess(arg->argc, arg->argv,
			 FALSE, FALSE, /* AllocConsole, HideConsole */
			 TRUE, /* debug */
			 TRUE /* newProcessGroup */,
			 &arg->process, &procinfo);
      arg->globalPid = procinfo.dwProcessId;
      if (arg->result) {
	arg->lastError = GetLastError();
	EXP_LOG ("Error while creating process: 0x%8x", arg->lastError);
      }
    }

    /* Make sure we ignore Ctrl-C */
    SetConsoleCtrlHandler(NULL, TRUE);
    SetEvent(arg->event);

    if (arg->result) {
	ExitThread(0);
    }

    bRet = PipeRespondToMaster(HMaster, arg->result, arg->globalPid);

    proc = ExpProcessNew();
    proc->hPid = arg->globalPid;

    CloseHandle(arg->process);
    proc->hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, arg->globalPid);
    arg->process = proc->hProcess;
    if (proc->hProcess == NULL) {
      arg->lastError = GetLastError();
      ExitThread(0);
    }

    ExpCommonDebugger();

    return 0;			/* Never executes */
}

/*
 *----------------------------------------------------------------------
 *
 * ExpCommonDebugger --
 *
 *	This is the function that is the debugger for all slave processes
 *
 * Results:
 *	None.  This thread exits with ExitThread() when the subprocess dies.
 *
 * Side Effects:
 *	Adds the process to the things being waited for by
 *	WaitForMultipleObjects
 *
 *----------------------------------------------------------------------
 */
void
ExpCommonDebugger()
{
    DEBUG_EVENT debEvent;	/* debugging event info. */
    DWORD dwContinueStatus;	/* exception continuation */
    DWORD err;
    ExpProcess *proc;
    DWORD n, i;

    n = GetEnvironmentVariable("Path", NULL, 0);
    n += GetEnvironmentVariable("_NT_SYMBOL_PATH", NULL, 0) + 1;
    n += GetEnvironmentVariable("_NT_ALT_SYMBOL_PATH", NULL, 0) + 1;
    n += GetEnvironmentVariable("SystemRoot", NULL, 0) + 1;

    SymbolPath = malloc(n);

    i = GetEnvironmentVariable("Path", SymbolPath, n);
    SymbolPath[i++] = ';';
    i += GetEnvironmentVariable("_NT_SYMBOL_PATH", &SymbolPath[i], n-i);
    SymbolPath[i++] = ';';
    i += GetEnvironmentVariable("_NT_ALT_SYMBOL_PATH", &SymbolPath[i], n-i);
    SymbolPath[i++] = ';';
    i += GetEnvironmentVariable("SystemRoot", &SymbolPath[i], n-i);

    for(;;) {
	dwContinueStatus = DBG_CONTINUE;

	/*
	 * Wait for a debugging event to occur. The second parameter
	 * indicates that the function does not return until
	 * a debugging event occurs.
	 */

	if (WaitForDebugEvent(&debEvent, INFINITE) == FALSE) {
	    err = GetLastError();
	    EXP_LOG ("error while waiting for debug event %d", err);
	    *((char *) NULL) = 0;
	}

	/*
	 * Find the process that is responsible for this event.
	 */
	for (proc = ProcessList; proc; proc = proc->nextPtr) {
	  if (proc->hPid == debEvent.dwProcessId) {
	    break;
	  }
	}

	if (!proc && debEvent.dwDebugEventCode != CREATE_PROCESS_DEBUG_EVENT) {
	  char buf[50];
	  sprintf(buf, "%d/%d (%d)",
		  debEvent.dwProcessId, debEvent.dwThreadId,
		  debEvent.dwDebugEventCode);
	  EXP_LOG("Unexpected debug event for %s\n", buf);
	  if (debEvent.dwDebugEventCode == EXCEPTION_DEBUG_EVENT) {
	    EXP_LOG("ExceptionCode: 0x%08x\n",
		    debEvent.u.Exception.ExceptionRecord.ExceptionCode);
	    dwContinueStatus = DBG_EXCEPTION_NOT_HANDLED;
	  }
	  goto skip;
	}

	/* Process the debugging event code. */
	EXP_LOG ("Debug event %d",debEvent.dwDebugEventCode);
	switch (debEvent.dwDebugEventCode) {
	case EXCEPTION_DEBUG_EVENT:
	    /*
	     * Process the exception code. When handling
	     * exceptions, remember to set the continuation
	     * status parameter (dwContinueStatus). This value
	     * is used by the ContinueDebugEvent function.
	     */

	    switch (debEvent.u.Exception.ExceptionRecord.ExceptionCode) {
	    case EXCEPTION_BREAKPOINT:
	    {
		if (proc->nBreakCount < 1000) {
		    proc->nBreakCount++;
		}
		if (proc->nBreakCount == 1) {
		    OnXFirstBreakpoint(proc, &debEvent);
		} else if (proc->nBreakCount == 2) {
		    OnXSecondBreakpoint(proc, &debEvent);
		} else {
		    OnXBreakpoint(proc, &debEvent);
		}
		break;
	    }

	    case EXCEPTION_SINGLE_STEP:
		OnXSingleStep(proc, &debEvent);
		break;

	    case DBG_CONTROL_C:
		/* fprintf(stderr, "Saw DBG_CONTROL_C event\n"); */
		dwContinueStatus = DBG_EXCEPTION_NOT_HANDLED;
		break;

	    case DBG_CONTROL_BREAK:
		/* fprintf(stderr, "Saw DBG_CONTROL_BREAK event\n"); */
		dwContinueStatus = DBG_EXCEPTION_NOT_HANDLED;
		break;

	    case EXCEPTION_DATATYPE_MISALIGNMENT:
	    case EXCEPTION_ACCESS_VIOLATION:
	    default:
		/*
		 * An exception was hit and it was not handled by the program.
		 * Now it is time to get a backtrace.
		 */
		if (! debEvent.u.Exception.dwFirstChance) {
		    OnXSecondChanceException(proc, &debEvent);
		}
		dwContinueStatus = DBG_EXCEPTION_NOT_HANDLED;
	    }
	    break;

	case CREATE_THREAD_DEBUG_EVENT:
#if 0
	    fprintf(stderr, "Process %d creating thread %d\n", proc->hPid,
		    debEvent.dwThreadId);
#endif
	    OnXCreateThread(proc, &debEvent);
	    break;

	case CREATE_PROCESS_DEBUG_EVENT:
#if 0
	    fprintf(stderr, "Process %d starting...\n", debEvent.dwProcessId);
#endif
	    OnXCreateProcess(proc, &debEvent);
	    break;

	case EXIT_THREAD_DEBUG_EVENT:
#if 0
	    fprintf(stderr, "Process %d thread %d exiting\n", proc->hPid,
		    debEvent.dwThreadId);
#endif
	    OnXDeleteThread(proc, &debEvent);
	    break;

	case EXIT_PROCESS_DEBUG_EVENT:
	    err = debEvent.u.ExitProcess.dwExitCode;
	    ExpProcessFree(proc);
	    /*
	     * When the last process exits, we exit.
	     */
	    if (ProcessList == NULL) {
	      ExitThread(0);
            }
	    break;

	case LOAD_DLL_DEBUG_EVENT:
	    OnXLoadDll(proc, &debEvent);
	    break;

	case UNLOAD_DLL_DEBUG_EVENT:
	    OnXUnloadDll(proc, &debEvent);
	    break;

	case OUTPUT_DEBUG_STRING_EVENT:
	    /* Display the output debugging string. */
	    break;
	}

    skip:
	/* Resume executing the thread that reported the debugging event. */
	ContinueDebugEvent(debEvent.dwProcessId,
			   debEvent.dwThreadId, dwContinueStatus);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * LoadedModule --
 *
 *	A module with the specifed name was loaded.  Add it to our
 *	list of loaded modules and print any debugging information
 *	if debugging is enabled.
 *
 * Results:
 *	If the module is known, return TRUE.  Otherwise, return FALSE
 *
 *----------------------------------------------------------------------
 */

static int
LoadedModule(ExpProcess *proc, HANDLE hFile, LPVOID modname, int isUnicode,
    LPVOID baseAddr, DWORD debugOffset)
{
#undef PRINTF
#if 0
#define PRINTF(x) printf x
#else
#define PRINTF(x)
#endif
    int known = 1;
    PVOID ptr;
    Tcl_HashEntry *tclEntry;
    int isNew;
    char mbstr[512];
    char *s = NULL;
    ExpModule *modPtr;

    if (modname) {

	/*
	 * This modname is a pointer to the name of the
	 * DLL in the process space of the subprocess
	 */
	if (ReadSubprocessMemory(proc, modname, &ptr, sizeof(PVOID)) && ptr) {
	    if (isUnicode) {
		WCHAR name[512];
		ReadSubprocessStringW(proc, ptr, name, 512);
		PRINTF(("0x%08x: Loaded %S\n", baseAddr, name));
		wcstombs(mbstr, name, sizeof(mbstr));
	    } else {
		ReadSubprocessStringA(proc, ptr, mbstr, sizeof(mbstr));
		PRINTF(("0x%08x: Loaded %s\n", baseAddr, mbstr));
	    }
	    s = strdup(mbstr);

	} else {
	    known = 0;
	}
	if (debugOffset) {
	    PRINTF((" with debugging info at offset 0x%08x\n",
		   debugOffset));
	}
    } else {
	PRINTF(("0x%08x: Loaded module with no known name\n", baseAddr));
    }
    tclEntry = Tcl_CreateHashEntry(proc->moduleTable, baseAddr, &isNew);

    modPtr = (ExpModule *) malloc(sizeof(ExpModule));
    modPtr->loaded = FALSE;
    modPtr->hFile = hFile;
    modPtr->baseAddr = baseAddr;
    modPtr->modName = s;
    modPtr->dbgInfo = NULL;
    if (proc->exeModule == NULL) {
	proc->exeModule = modPtr;
    }

    Tcl_SetHashValue(tclEntry, modPtr);

    return known;
#undef PRINTF
}


/*
 *----------------------------------------------------------------------
 *
 * OnXCreateProcess --
 *
 *	This routine is called when a CREATE_PROCESS_DEBUG_EVENT
 *	occurs.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnXCreateProcess(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    ExpThreadInfo *threadInfo;
    CREATE_PROCESS_DEBUG_INFO *info = &pDebEvent->u.CreateProcessInfo;
    int known;

    /* Add the child process to the process list */
    if (proc == NULL) {
	proc = ExpProcessNew();

	if (!DuplicateHandle(GetCurrentProcess(),
			     info->hProcess,
			     GetCurrentProcess(),
			     &proc->hProcess, PROCESS_ALL_ACCESS,
                      FALSE, 0)) {
            EXP_LOG ("Unable to duplicate handle", NULL);
	}
	proc->hPid = pDebEvent->dwProcessId;
    }

    known = LoadedModule(proc, info->hFile, info->lpImageName,
			 info->fUnicode, info->lpBaseOfImage,
			 info->dwDebugInfoFileOffset);

    /*
     * As needed, examine or change the registers of the
     * process's initial thread with the GetThreadContext and
     * SetThreadContext functions; read from and write to the
     * process's virtual memory with the ReadProcessMemory and
     * WriteProcessMemory functions; and suspend and resume
     * thread execution with the SuspendThread and ResumeThread
     * functions.
     */

    threadInfo = (ExpThreadInfo *) malloc(sizeof(ExpThreadInfo));
    threadInfo->dwThreadId = pDebEvent->dwThreadId;
    threadInfo->hThread = info->hThread;
    threadInfo->nextPtr = proc->threadList;
    proc->threadCount++;
    proc->threadList = threadInfo;
}

/*
 *----------------------------------------------------------------------
 *
 * OnXCreateThread --
 *
 *	This routine is called when a CREATE_THREAD_DEBUG_EVENT
 *	occurs.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnXCreateThread(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    /*
     * As needed, examine or change the thread's registers
     * with the GetThreadContext and SetThreadContext functions;
     * and suspend and resume thread execution with the
     * SuspendThread and ResumeThread functions.
     */
    ExpThreadInfo *threadInfo;

    threadInfo = (ExpThreadInfo *) malloc(sizeof(ExpThreadInfo));
    threadInfo->dwThreadId = pDebEvent->dwThreadId;
    threadInfo->hThread = pDebEvent->u.CreateThread.hThread;
    proc->threadCount++;
    threadInfo->nextPtr = proc->threadList;
    proc->threadList = threadInfo;
}

/*
 *----------------------------------------------------------------------
 *
 * OnXDeleteThread --
 *
 *	This routine is called when a CREATE_THREAD_DEBUG_EVENT
 *	occurs.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnXDeleteThread(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    /*
     * As needed, examine or change the thread's registers
     * with the GetThreadContext and SetThreadContext functions;
     * and suspend and resume thread execution with the
     * SuspendThread and ResumeThread functions.
     */
    ExpThreadInfo *threadInfo;
    ExpThreadInfo *prev;

    prev = NULL;
    for (threadInfo = proc->threadList; threadInfo;
	 prev = threadInfo, threadInfo = threadInfo->nextPtr)
    {
	if (threadInfo->dwThreadId == pDebEvent->dwThreadId) {
	    if (prev == NULL) {
		proc->threadList = threadInfo->nextPtr;
	    } else {
		prev->nextPtr = threadInfo->nextPtr;
	    }
	    proc->threadCount--;
	    CloseHandle(threadInfo->hThread);
	    free(threadInfo);
	    break;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * OnXFirstBreakpoint --
 *
 *	This routine is called when a EXCEPTION_DEBUG_EVENT with
 *	an exception code of EXCEPTION_BREAKPOINT, and it is the
 *	first one to occur in the program.  This happens when the
 *	process finally gets loaded into memory and is about to
 *	start.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static CONTEXT FirstContext;
static UCHAR   FirstPage[PAGESIZE];
static HANDLE  FirstThread;
#pragma pack(push,1)
typedef struct _InjectCode {
    UCHAR instPush1;
    DWORD argMemProtect;
    UCHAR instPush2;
    DWORD argMemType;
    UCHAR instPush3;
    DWORD argMemSize;
    UCHAR instPush4;
    DWORD argMemAddr;
    UCHAR instCall;
    DWORD argCallAddr;
    DWORD instIntr;
} InjectCode;
#pragma pack(pop)

static void
OnXFirstBreakpoint(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    DWORD base;
    ExpThreadInfo *tinfo;

    for (tinfo = proc->threadList; tinfo != NULL; tinfo = tinfo->nextPtr) {
	if (pDebEvent->dwThreadId == tinfo->dwThreadId) {
	    break;
	}
    }

    /*
     * Set up the memory that will serve as the place for our
     * intercepted function return points.
     */

    {
	InjectCode code;
	Tcl_HashEntry *tclEntry;
	DWORD addr;

	FirstThread = tinfo->hThread;
	FirstContext.ContextFlags = CONTEXT_FULL;
	GetThreadContext(FirstThread, &FirstContext);

	tclEntry = Tcl_FindHashEntry(proc->funcTable, "VirtualAlloc");
	if (tclEntry == NULL) {
	    proc->nBreakCount++;	/* Don't stop at second breakpoint */
	    EXP_LOG("Unable to find entry for VirtualAlloc\n", NULL);
	    return;
	}
	addr = (DWORD) Tcl_GetHashValue(tclEntry);

	code.instPush1     = 0x68;
	code.argMemProtect = PAGE_EXECUTE_READWRITE;
	code.instPush2     = 0x68;
	code.argMemType    = MEM_COMMIT;
	code.instPush3     = 0x68;
	code.argMemSize    = 2048;
	code.instPush4     = 0x68;
	code.argMemAddr    = 0;
	code.instCall      = 0xe8;
	code.argCallAddr   = addr - FirstContext.Eip -
                               offsetof(InjectCode, instCall) - 5;
	code.instIntr      = 0xCC;

	base = FirstContext.Eip;
	if (!ReadSubprocessMemory
               (proc, (PVOID) base, FirstPage, sizeof(InjectCode)))
        {
	    EXP_LOG("Error reading subprocess memory\n", NULL);
	}
	if (!WriteSubprocessMemory
               (proc, (PVOID) base, &code, sizeof(InjectCode)))
        {
	    EXP_LOG("Error reading subprocess memory\n", NULL);
	}
    }
    return;
}

/*
 *----------------------------------------------------------------------
 *
 * OnXSecondBreakpoint --
 *
 *	This routine is called when the second breakpoint is hit.
 *	The second breakpoint is at the end of our call to GlobalAlloc().
 *	Save the returned pointer from GlobalAlloc, then restore the
 *	first page of memory and put everything back the way it was.
 *	Finally, we can start.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnXSecondBreakpoint(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    CONTEXT context;
    UCHAR retbuf[2048];
    DWORD base;
    LPEXCEPTION_DEBUG_INFO exceptInfo;
    ExpBreakInfo *info;
    int i;

    exceptInfo = &pDebEvent->u.Exception;

    context.ContextFlags = CONTEXT_FULL;
    GetThreadContext(FirstThread, &context);
    proc->pSubprocessMemory = context.Eax;

    memset(retbuf, 0xCC, sizeof(retbuf));	/* All breakpoints */
    WriteSubprocessMemory(proc, (PVOID) proc->pSubprocessMemory,
			  retbuf, sizeof(retbuf));

    base = FirstContext.Eip;
    if (!WriteSubprocessMemory
           (proc, (PVOID) base, FirstPage, sizeof(InjectCode)))
    {
	EXP_LOG("Error writing subprocess memory\n", NULL);
    }
    SetThreadContext(FirstThread, &FirstContext);

    /*
     * Set all breakpoints
     */
    for (i = 0; BreakPoints[i].dllName; i++) {
	for (info = BreakPoints[i].breakInfo; info->funcName; info++) {
	    SetBreakpoint(proc, info);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * OnXBreakpoint --
 *
 *	This routine is called when a EXCEPTION_DEBUG_EVENT with
 *	an exception code of EXCEPTION_BREAKPOINT.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnXBreakpoint(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    LPEXCEPTION_DEBUG_INFO exceptInfo;
    CONTEXT context;
    ExpThreadInfo *tinfo;
    ExpBreakpoint *pbrkpt, *brkpt;
    PDWORD pdw;
    DWORD i;
    DWORD dw;

    for (tinfo = proc->threadList; tinfo != NULL; tinfo = tinfo->nextPtr) {
	if (pDebEvent->dwThreadId == tinfo->dwThreadId) {
	    break;
	}
    }
    assert(tinfo != NULL);

    exceptInfo = &pDebEvent->u.Exception;

    pbrkpt = NULL;
    for (brkpt = proc->brkptList; brkpt != NULL;
	 pbrkpt = brkpt, brkpt = brkpt->nextPtr) {
	if (brkpt->codePtr == exceptInfo->ExceptionRecord.ExceptionAddress) {
	    if (brkpt->threadInfo == NULL) {
		break;
	    }
	    if (brkpt->threadInfo == tinfo) {
		break;
	    }
	}
    }

    context.ContextFlags = CONTEXT_FULL;
    GetThreadContext(tinfo->hThread, &context);

    if (! brkpt->returning) {
	ExpBreakpoint *bpt;
	/*
	 * Get the arguments to the function and store them in the thread
	 * specific data structure.
	 */
	for (pdw = tinfo->args, i=0; i < brkpt->breakInfo->nargs; i++, pdw++) {
	    ReadSubprocessMemory(proc, (PVOID) (context.Esp+(4*(i+1))),
				 pdw, sizeof(DWORD));
	}
	tinfo->nargs = brkpt->breakInfo->nargs;
	tinfo->context = &context;

	if (brkpt->breakInfo->dwFlags & EXP_BREAK_IN) {
	    brkpt->breakInfo->breakProc
              (proc, tinfo, brkpt, &context.Eax, EXP_BREAK_IN);
	}

	/*
	 * Only set a return breakpoint if something is interested
	 * in the return value
	 */
	if (brkpt->breakInfo->dwFlags & EXP_BREAK_OUT) {
	    bpt = (ExpBreakpoint *) malloc(sizeof(ExpBreakpoint));
	    ReadSubprocessMemory(proc, (PVOID) context.Esp,
		&bpt->origRetAddr, sizeof(DWORD));
	    dw = (DWORD) brkpt->codeReturnPtr;
	    WriteSubprocessMemory(proc, (PVOID) context.Esp,
		&dw, sizeof(DWORD));
	    bpt->codePtr = brkpt->codeReturnPtr;
	    bpt->returning = TRUE;
	    bpt->codeReturnPtr = NULL;	/* Doesn't matter */
	    bpt->breakInfo = brkpt->breakInfo;
	    bpt->threadInfo = tinfo;
	    bpt->nextPtr = proc->brkptList;
	    proc->brkptList = bpt;

	}

	/*
	 * Now, we need to restore the original code for this breakpoint.
	 * Put the program counter back, then do a single-step and put
	 * the breakpoint back again.
	 */
	WriteSubprocessMemory(proc, brkpt->codePtr,
	    &brkpt->code, sizeof(UCHAR));

	context.EFlags |= SINGLE_STEP_BIT;
	context.Eip--;

	proc->lastBrkpt = brkpt;
    } else {
	/*
	 * Make the callback with the params and the return value
	 */
	if (brkpt->breakInfo->dwFlags & EXP_BREAK_OUT) {
	    brkpt->breakInfo->breakProc
             (proc, tinfo, brkpt, &context.Eax, EXP_BREAK_OUT);
	}
	context.Eip = brkpt->origRetAddr;

	if (pbrkpt == NULL) {
	    proc->brkptList = brkpt->nextPtr;
	} else {
	    pbrkpt->nextPtr = brkpt->nextPtr;
	}
	free(brkpt);
    }
    SetThreadContext(tinfo->hThread, &context);
}

/*
 *----------------------------------------------------------------------
 *
 * OnXSecondChanceException --
 *
 *	Handle a second chance exception
 *
 *----------------------------------------------------------------------
 */

static void
OnXSecondChanceException(ExpProcess *proc,  LPDEBUG_EVENT pDebEvent)
{
    BOOL b;
    STACKFRAME frame;
    CONTEXT context;
    ExpThreadInfo *tinfo;
    Tcl_HashEntry *tclEntry;
    Tcl_HashSearch tclSearch;
    ExpModule *modPtr;
    DWORD displacement;
    BYTE symbolBuffer[sizeof(IMAGEHLP_SYMBOL) + 512];
    PIMAGEHLP_SYMBOL pSymbol = (PIMAGEHLP_SYMBOL)symbolBuffer;
    char *s;

    if (!ExpDebug) {
	return;
    }

    for (tinfo = proc->threadList; tinfo != NULL; tinfo = tinfo->nextPtr) {
	if (pDebEvent->dwThreadId == tinfo->dwThreadId) {
	    break;
	}
    }
    assert(tinfo != NULL);

    context.ContextFlags = CONTEXT_FULL;
    GetThreadContext(tinfo->hThread, &context);

    /*
     * XXX: From what I can tell, SymInitialize is broken on Windows NT 4.0
     * if you try to have it iterate the modules in a process.  It always
     * returns an object mismatch error.  Instead, initialize without iterating
     * the modules.  Contrary to what MSDN documentation says,
     * Microsoft debuggers do not exclusively use the imagehlp API.  In
     * fact, the only thing VC 5.0 uses is the StackWalk function.
     * Windbg uses a few more functions, but it doesn't use SymInitialize.
     * We will then do the hard work of finding all the
     * modules and doing the right thing.
     */

    if (! SymInitialize(proc->hProcess, SymbolPath, FALSE)){
	EXP_LOG ("Unable to get backtrace (Debug 1): 0x%08x\n",
	    GetLastError());
	goto error;
    }

#ifdef _X86_
    memset(&frame, 0, sizeof(frame));
    frame.AddrPC.Mode = AddrModeFlat;
    frame.AddrPC.Segment = 0;
    frame.AddrPC.Offset = context.Eip;

    frame.AddrReturn.Mode = AddrModeFlat;
    frame.AddrReturn.Segment = 0;
    frame.AddrReturn.Offset = context.Ebp; /* I think this is correct */

    frame.AddrFrame.Mode = AddrModeFlat;
    frame.AddrFrame.Segment = 0;
    frame.AddrFrame.Offset = context.Ebp;

    frame.AddrStack.Mode = AddrModeFlat;
    frame.AddrStack.Segment = 0;
    frame.AddrStack.Offset = context.Esp;

    frame.FuncTableEntry = NULL;
    frame.Params[0] = context.Eax;
    frame.Params[1] = context.Ecx;
    frame.Params[2] = context.Edx;
    frame.Params[3] = context.Ebx;
    frame.Far = FALSE;
    frame.Virtual = FALSE;
    frame.Reserved[0] = 0;
    frame.Reserved[1] = 0;
    frame.Reserved[2] = 0;
    /* frame.KdHelp.* is not set */

    /*
     * Iterate through the loaded modules and load symbols for each one.
     */
    tclEntry = Tcl_FirstHashEntry(proc->moduleTable, &tclSearch);
    while (tclEntry != NULL) {
	modPtr = (ExpModule *) Tcl_GetHashValue(tclEntry);
	if (! modPtr->loaded) {
	    modPtr->dbgInfo = MapDebugInformation(modPtr->hFile, NULL,
		SymbolPath, (DWORD)modPtr->baseAddr);

	    SymLoadModule(proc->hProcess, modPtr->hFile,
		NULL, NULL, (DWORD) modPtr->baseAddr, 0);
	    modPtr->loaded = TRUE;
	}

	tclEntry = Tcl_NextHashEntry(&tclSearch);
    }


    if (proc->exeModule && proc->exeModule->dbgInfo &&
	proc->exeModule->dbgInfo->ImageFileName) {
	s = proc->exeModule->dbgInfo->ImageFileName;
    } else {
	s = "";
    }
    EXP_LOG("Backtrace for %s\n", s);
    EXP_LOG("-------------------------------------\n", NULL);
    EXP_LOG("Backtrace for %s\n", s);
    while (1) {
        pSymbol->SizeOfStruct = sizeof(symbolBuffer);
        pSymbol->MaxNameLength = 512;

	b = StackWalk(IMAGE_FILE_MACHINE_I386, proc->hProcess,
	    tinfo->hThread, &frame, &context, NULL,
	    SymFunctionTableAccess, SymGetModuleBase,
	    NULL);

	if (b == FALSE || frame.AddrPC.Offset == 0) {
	    break;
	}

        if (SymGetSymFromAddr(proc->hProcess, frame.AddrPC.Offset,
	    &displacement, pSymbol) )
        {
	    DWORD base;
	    char buf[1024];

	    base = SymGetModuleBase(proc->hProcess, frame.AddrPC.Offset);
	    tclEntry = Tcl_FindHashEntry(proc->moduleTable, (void *) base);
	    modPtr = (ExpModule *) Tcl_GetHashValue(tclEntry);
	    if (modPtr->dbgInfo && modPtr->dbgInfo->ImageFileName) {
		s = modPtr->dbgInfo->ImageFileName;
	    } else {
		s = "";
	    }
	    sprintf(buf, "%.20s %08x\t%s+%X", s, frame.AddrPC.Offset,
		pSymbol->Name, displacement);
	    EXP_LOG("%s\n", buf);
	} else {
	    EXP_LOG("%08x\t\n", frame.AddrPC.Offset);
	}
    }

error:
    if (ExpDebug) {
	Sleep(10000);
    }
#else
#  error "Unsupported architecture"
#endif
}

/*
 *----------------------------------------------------------------------
 *
 * OnXSingleStep --
 *
 *	This routine is called when a EXCEPTION_DEBUG_EVENT with
 *	an exception code of EXCEPTION_SINGLE_STEP.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnXSingleStep(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    UCHAR code;
    /*
     * Now, we need to restore the breakpoint that we had removed.
     */
    code = 0xcc;
    WriteSubprocessMemory
      (proc, proc->lastBrkpt->codePtr, &code, sizeof(UCHAR));
}

/*
 *----------------------------------------------------------------------
 *
 * OnXLoadDll --
 *
 *	This routine is called when a LOAD_DLL_DEBUG_EVENT is seen
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Some information is printed
 *
 *----------------------------------------------------------------------
 */

static void
OnXLoadDll(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    WORD w;
    DWORD dw;
    DWORD ImageHdrOffset;
    PIMAGE_FILE_HEADER pfh;	/* File header image in subprocess memory */
    PIMAGE_SECTION_HEADER psh;
    PIMAGE_OPTIONAL_HEADER poh;
    IMAGE_DATA_DIRECTORY dataDir;
    PIMAGE_EXPORT_DIRECTORY ped;
    IMAGE_EXPORT_DIRECTORY exportDir;
    DWORD n;
    DWORD base;
    CHAR funcName[256];
    CHAR dllname[256];
    PVOID ptr, namePtr, funcPtr;
    DWORD p;
    LPLOAD_DLL_DEBUG_INFO info = &pDebEvent->u.LoadDll;
    Tcl_HashEntry *tclEntry;
    int isNew;
    BOOL bFound;

    int unknown = !LoadedModule(proc, info->hFile,
	info->lpImageName, info->fUnicode,
	info->lpBaseOfDll, info->dwDebugInfoFileOffset);

    base = (DWORD) info->lpBaseOfDll;

    /*
     * Check for the DOS signature
     */
    ReadSubprocessMemory(proc, info->lpBaseOfDll, &w, sizeof(WORD));
    if (w != IMAGE_DOS_SIGNATURE) return;

    /*
     * Skip over the DOS signature and check the NT signature
     */
    p = base;
    p += 15 * sizeof(DWORD);
    ptr = (PVOID) p;
    ReadSubprocessMemory(proc, (PVOID) p, &ImageHdrOffset, sizeof(DWORD));

    p = base;
    p += ImageHdrOffset;
    ReadSubprocessMemory(proc, (PVOID) p, &dw, sizeof(DWORD));
    if (dw != IMAGE_NT_SIGNATURE) {
	return;
    }
    ImageHdrOffset += sizeof(DWORD);
    p += sizeof(DWORD);

    pfh = (PIMAGE_FILE_HEADER) p;
    ptr = &pfh->SizeOfOptionalHeader;
    ReadSubprocessMemory(proc, ptr, &w, sizeof(WORD));

    /*
     * We want to find the exports section.  It can be found in the
     * data directory that is part of the IMAGE_OPTIONAL_HEADER
     */
    if (!w) return;
    p += sizeof(IMAGE_FILE_HEADER);
    poh = (PIMAGE_OPTIONAL_HEADER) p;

    /*
     * Find the number of entries in the data directory
     */
    ptr = &poh->NumberOfRvaAndSizes;
    ReadSubprocessMemory(proc, ptr, &dw, sizeof(DWORD));
    if (dw == 0) return;

    /*
     * Read the export data directory
     */
    ptr = &poh->DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
    ReadSubprocessMemory(proc, ptr, &dataDir, sizeof(IMAGE_DATA_DIRECTORY));

    /*
     * This points us to the exports section
     */
    ptr = (PVOID) (base + dataDir.VirtualAddress);
    ped = (PIMAGE_EXPORT_DIRECTORY) ptr;
    ReadSubprocessMemory
      (proc, ptr, &exportDir, sizeof(IMAGE_EXPORT_DIRECTORY));

    /*
     * See if this is a DLL we are interested in
     */
    ptr = &ped->Name;
    ReadSubprocessMemory(proc, ptr, &dw, sizeof(DWORD));
    ptr = (PVOID) (base + dw);
    ReadSubprocessStringA(proc, ptr, dllname, sizeof(dllname));

#if 0 /* Debugging purposes */
    /*
     * We now have the DLL name, even if it was unknown.
     */
    if (unknown) {
	printf("0x%08x: Loaded %s\n", info->lpBaseOfDll, dllname);
    }
#endif


    bFound = FALSE;
    for (n = 0; BreakPoints[n].dllName; n++) {
	if (stricmp(dllname, BreakPoints[n].dllName) == 0) {
	    bFound = TRUE;
	    break;
	}
    }
    if (!bFound) {
	return;
    }

    ptr = (PVOID) (base + (DWORD) exportDir.AddressOfNames);
    for (n = 0; n < exportDir.NumberOfNames; n++) {
	ReadSubprocessMemory(proc, ptr, &dw, sizeof(DWORD));
	namePtr = (PVOID) (base + dw);
	/*
	 * Now, we should hopefully have a pointer to the name of the
	 * function, so lets get it.
	 */
	ReadSubprocessStringA(proc, namePtr, funcName, sizeof(funcName));
	/* printf("%s\n", funcName); */

	/*
	 * Keep a list of all function names in a hash table
	 */

	funcPtr = (PVOID) (base + n*sizeof(DWORD) +
	    (DWORD) exportDir.AddressOfFunctions);
	ReadSubprocessMemory(proc, funcPtr, &dw, sizeof(DWORD));
	funcPtr = (PVOID) (base + dw);

	tclEntry = Tcl_CreateHashEntry(proc->funcTable, funcName, &isNew);
	Tcl_SetHashValue(tclEntry, funcPtr);

	ptr = (PVOID) (sizeof(DWORD) + (ULONG) ptr);
    }

    /*
     * The IMAGE_SECTION_HEADER comes after the IMAGE_OPTIONAL_HEADER
     * (if the IMAGE_OPTIONAL_HEADER exists)
     */
    p += w;

    psh = (PIMAGE_SECTION_HEADER) p;
}

/*
 *----------------------------------------------------------------------
 *
 * OnXUnloadDll --
 *
 *	This routine is called when a UNLOAD_DLL_DEBUG_EVENT is seen
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Some information is printed
 *
 *----------------------------------------------------------------------
 */

static void
OnXUnloadDll(ExpProcess *proc, LPDEBUG_EVENT pDebEvent)
{
    Tcl_HashEntry *tclEntry;
    ExpModule *modPtr;

    /*
     * Display a message that the DLL has
     * been unloaded.
     */
#if 0
    fprintf(stderr, "0x%08x: Unloading\n", pDebEvent->u.UnloadDll.lpBaseOfDll);
#endif

    tclEntry = Tcl_FindHashEntry(proc->moduleTable,
	pDebEvent->u.UnloadDll.lpBaseOfDll);

    if (tclEntry != NULL) {
	modPtr = (ExpModule *) Tcl_GetHashValue(tclEntry);
	if (modPtr->hFile) {
	    CloseHandle(modPtr->hFile);
	}
	if (modPtr->modName) {
	    free(modPtr->modName);
	}
	if (modPtr->dbgInfo) {
	    UnmapDebugInformation(modPtr->dbgInfo);
	}
	free(modPtr);
	Tcl_DeleteHashEntry(tclEntry);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * SetBreakpoint --
 *
 *	Inserts a single breakpoint
 *
 * Results:
 *	TRUE if successful, FALSE if unsuccessful.
 *
 *----------------------------------------------------------------------
 */

static BOOL
SetBreakpoint(ExpProcess *proc, ExpBreakInfo *info)
{
    Tcl_HashEntry *tclEntry;
    PVOID funcPtr;

    tclEntry = Tcl_FindHashEntry(proc->funcTable, info->funcName);
    if (tclEntry == NULL) {
	EXP_LOG("Unable to set breakpoint at %s\n", info->funcName);
	return FALSE;
    }

#if 0
    fprintf(stderr, "%s: ", info->funcName);
#endif
    /*
     * Set a breakpoint at the function start in the subprocess and
     * save the original code at the function start.
     */
    funcPtr = Tcl_GetHashValue(tclEntry);
    SetBreakpointAtAddr(proc, info, funcPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * SetBreakpointAtAddr --
 *
 *	Inserts a single breakpoint at the given address
 *
 * Results:
 *	TRUE if successful, FALSE if unsuccessful.
 *
 *----------------------------------------------------------------------
 */

ExpBreakpoint *
SetBreakpointAtAddr(ExpProcess *proc, ExpBreakInfo *info, PVOID funcPtr)
{
    ExpBreakpoint *bpt;
    UCHAR code;

#if 0
    fprintf(stderr, "SetBreakpointAtAddr: addr=0x%08x\n", funcPtr);
#endif
    bpt = (ExpBreakpoint *) malloc(sizeof(ExpBreakpoint));
    bpt->returning = FALSE;
    bpt->codePtr = funcPtr;
    bpt->codeReturnPtr =
      (PVOID) (proc->offset + (DWORD) proc->pSubprocessMemory);
    bpt->origRetAddr = 0;
    bpt->breakInfo = info;
    bpt->threadInfo = NULL;
    proc->offset += 2;
    bpt->nextPtr = proc->brkptList;
    proc->brkptList = bpt;

    ReadSubprocessMemory(proc, funcPtr, &bpt->code, sizeof(UCHAR));
    code = 0xcc;	/* Breakpoint opcode on i386 */
    WriteSubprocessMemory(proc, funcPtr, &code, sizeof(UCHAR));
    return bpt;
}

/*
 *----------------------------------------------------------------------
 *
 * OnOpenConsoleW --
 *
 *	This function gets called when an OpenConsoleW breakpoint
 *	is hit.  There is one big problem with this function--it
 *	isn't documented.  However, we only really care about the
 *	return value which is a console handle.  I think this is
 *	what this function declaration should be:
 *
 *	HANDLE OpenConsoleW(LPWSTR lpFileName,
 *			    DWORD dwDesiredAccess,
 *			    DWORD dwShareMode,
 *			    LPSECURITY_ATTRIBUTES lpSecurityAttributes);
 *
 *	So why do we intercept an undocumented function while we
 *	could just intercept CreateFileW and CreateFileA?  Well,
 *	those functions are going to get called alot more than this
 *	one, so limiting the number of intercepted functions
 *	improves performance since fewer breakpoints will be hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Save the return value in an array of known console handles
 *	with their statuses.
 *
 *----------------------------------------------------------------------
 */

static void
OnOpenConsoleW(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    WCHAR name[256];
    PVOID ptr;

    LOG_ENTRY("OpenConsoleW");
    if (*returnValue == (DWORD) INVALID_HANDLE_VALUE) {
	return;
    }

    /*
     * Save any console input handle.  No SetConsoleMode() calls will
     * succeed unless they are really attached to a console input buffer.
     */

    ptr = (PVOID) threadInfo->args[0];
    ReadSubprocessStringW(proc, ptr, name, 256);

    if (wcsicmp(name, L"CONIN$") == 0) {
	if (proc->consoleHandlesMax > 100) {
	    proc->consoleHandlesMax = 100;
	}
	proc->consoleHandles[proc->consoleHandlesMax++] = *returnValue;
    }
    return;
}

/*
 *----------------------------------------------------------------------
 *
 * OnWriteConsoleA --
 *
 *	This function gets called when an WriteConsoleA breakpoint
 *	is hit.  The data is also redirected to expect since expect
 *	normally couldn't see any output going through this interface.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Prints some output.
 *
 *----------------------------------------------------------------------
 */

static void
OnWriteConsoleA(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
  CHAR buf[1024];
  PVOID ptr;
  DWORD i, n;
  PCHAR p, p2;
  BOOL bRet;

  LOG_ENTRY("WriteConsoleA");

  if (*returnValue == 0) {
    return;
  }
  /*
   * Get number of bytes written
   */
  ptr = (PVOID) threadInfo->args[3];
  if (ptr == NULL) {
    n = threadInfo->args[2];
  } else {
    ReadSubprocessMemory(proc, ptr, &n, sizeof(DWORD));
  }
  if (n > 1024) {
    p = malloc(n * sizeof(CHAR));
  } else {
    p = buf;
  }
  EXP_LOG ("Reading %d byte(s)", n);

  ptr = (PVOID) threadInfo->args[1];
  ReadSubprocessMemory(proc, ptr, p, n * sizeof(CHAR));

  for (i = 0; i < n; i++)
    if (p[i] == '\n') {
      curY++;
      curX = 0;
    } else {
      curX++;
    }

#ifdef EXPLAUNCH_DEBUG
  { // DEBUG
    p2 = malloc ((n + 1) * sizeof(CHAR));
    memcpy (p2, p, n);
    p2[n] = '\0';
    if (n == 1) {
      EXP_LOG ("Read from WriteConsoleA: '0x%08x'", p[0]);
    } else {
      EXP_LOG ("Read from WriteConsoleA: '%s'", p2);
    }
    free (p2);
  }
#endif

  bRet = ExpWriteMaster(HMaster, p, n);

  if (p != buf) {
    free(p);
  }
}

/*
 *----------------------------------------------------------------------
 *
 * OnWriteConsoleW --
 *
 *	This function gets called when an WriteConsoleW breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Prints some output.
 *
 *----------------------------------------------------------------------
 */

static void
OnWriteConsoleW(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    WCHAR buf[1024];
    CHAR ansi[2048];
    PVOID ptr;
    DWORD n;
    PWCHAR p;
    PCHAR a;
    int asize;
    BOOL bRet;
    int w, i;

    LOG_ENTRY("WriteConsoleW");

    if (*returnValue == 0) {
	return;
    }

    ptr = (PVOID) threadInfo->args[1];
    n = threadInfo->args[2];

    if (n > 1024) {
	p = malloc(n * sizeof(WCHAR));
	asize = n * 2 * sizeof(CHAR);
	a = malloc(asize);
    } else {
	p = buf;
	a = ansi;
	asize = sizeof(ansi);
    }
    ReadSubprocessMemory(proc, ptr, p, n * sizeof(WCHAR));

    /*
     * Convert to ASCI and write the intercepted data to the pipe.
     */

  w = WideCharToMultiByte(CP_ACP, 0, p, n, a, asize, NULL, NULL);
  bRet = ExpWriteMaster(HMaster, a, w);
  if (p != buf) {
    free(p);
    free(a);
  }
}

/*
 *----------------------------------------------------------------------
 *
 * CreateVtSequence --
 *
 *	When moving the cursor to a new location, this will create
 *	the appropriate VT100 type sequence to get the cursor there.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Characters are written to the pipe going to Expect
 *
 *----------------------------------------------------------------------
 */

void
CreateVtSequence(ExpProcess *proc, COORD newPos, DWORD n)
{
  LOG_ENTRY ("CreateVtSequence (nothing to do)");
  EXP_LOG ("newPos is x= %d, ", newPos.X);
  EXP_LOG ("y= %d, ", newPos.Y);

  if (curX > 0 && newPos.X == 0) {
    EXP_LOG ("\\R !!!", NULL);
    ExpWriteMaster(HMaster, "\r", 1);
  }

  if (newPos.X == 0) {
    // set as 1, so that two consecutive calls will output \r
    curX = 1;
  }
}

/*
 *----------------------------------------------------------------------
 *
 * ExpSetConsoleSize --
 *
 *	Sets the console to the appropriate size
 *
 * Results
 *	None
 *
 *----------------------------------------------------------------------
 */
void
ExpSetConsoleSize(HANDLE hConsoleInW, HANDLE hConsoleOut, int w, int h)
{
  LOG_ENTRY ("ExpSetConsoleSize");
    COORD largest;
    SMALL_RECT winrect;
    INPUT_RECORD resizeRecord;
    DWORD n;

    largest = GetLargestConsoleWindowSize(hConsoleOut);

    if (w > largest.X) w = largest.X;
    if (h > largest.Y) h = largest.Y;

    ConsoleSize.X = w;
    ConsoleSize.Y = h;

    winrect.Left = 0;
    winrect.Right = w-1;
    winrect.Top = 0;
    winrect.Bottom = h-1;

    /* Just in case one depends on the other, do the sequence twice */
    SetConsoleScreenBufferSize(hConsoleOut, ConsoleSize);
    SetConsoleWindowInfo(hConsoleOut, TRUE, &winrect);
    SetConsoleScreenBufferSize(hConsoleOut, ConsoleSize);
    SetConsoleWindowInfo(hConsoleOut, TRUE, &winrect);

    resizeRecord.EventType = WINDOW_BUFFER_SIZE_EVENT;
    resizeRecord.Event.WindowBufferSizeEvent.dwSize = ConsoleSize;
    WriteConsoleInput(hConsoleInW, &resizeRecord, 1, &n);
}

/*
 *----------------------------------------------------------------------
 *
 * OnWriteConsoleOutputCharacterA --
 *
 *	This function gets called when an WriteConsoleOutputCharacterA breakpoint
 *	is hit.  The data is also redirected to expect since expect
 *	normally couldn't see any output going through this interface.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Prints some output.
 *
 *----------------------------------------------------------------------
 */

static void
OnWriteConsoleOutputCharacterA(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    CHAR buf[1024];
    PVOID ptr;
    DWORD n;
    PCHAR p;
    BOOL b;

    LOG_ENTRY("WriteConsoleOutputCharacterA");

    if (*returnValue == 0) {
	return;
    }
    /*
     * Get number of bytes written
     */
    ptr = (PVOID) threadInfo->args[4];
    if (ptr == NULL) {
	n = threadInfo->args[2];
    } else {
	ReadSubprocessMemory(proc, ptr, &n, sizeof(DWORD));
    }

    CreateVtSequence(proc, *((PCOORD) &threadInfo->args[3]), n);

    if (n > 1024) {
	p = malloc(n * sizeof(CHAR));
    } else {
	p = buf;
    }

    ptr = (PVOID) threadInfo->args[1];
    ReadSubprocessMemory(proc, ptr, p, n * sizeof(CHAR));

    EXP_LOG ("(debug): OnWriteConsoleOutputCharacterA >> \n'%s'", p);
    b = ExpWriteMaster(HMaster, p, n);

    if (p != buf) {
	free(p);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * OnWriteConsoleOutputCharacterW --
 *
 *	This function gets called when an WriteConsoleOutputCharacterW breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Prints some output.
 *
 *----------------------------------------------------------------------
 */

static void
OnWriteConsoleOutputCharacterW(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    WCHAR buf[1024];
    CHAR ansi[2048];
    PVOID ptr;
    DWORD n;
    PWCHAR p;
    PCHAR a;
    int asize;
    BOOL b;
    int w;

    if (direction == EXP_BREAK_IN) {
	LOG_ENTRY("WriteConsoleOutputCharacterW (in)a");
	return;
    } else {
	LOG_ENTRY("WriteConsoleOutputCharacterW (out)");
    }

    if (*returnValue == 0) {
	return;
    }
    /*
     * Get number of bytes written
     */
    ptr = (PVOID) threadInfo->args[4];
    if (ptr == NULL) {
	n = threadInfo->args[2];
    } else {
	ReadSubprocessMemory(proc, ptr, &n, sizeof(DWORD));
    }

    CreateVtSequence(proc, *((PCOORD) &threadInfo->args[3]), n);

    if (n > 1024) {
	p = malloc(n * sizeof(WCHAR));
	asize = n * 2 * sizeof(CHAR);
	a = malloc(asize);
    } else {
	p = buf;
	a = ansi;
	asize = sizeof(ansi);
    }

    ptr = (PVOID) threadInfo->args[1];
    ReadSubprocessMemory(proc, ptr, p, n * sizeof(WCHAR));

    /*
     * Convert to ASCI and Write the intercepted data to the pipe.
     */

    w = WideCharToMultiByte(CP_ACP, 0, p, n, a, asize, NULL, NULL);
    b = ExpWriteMaster(HMaster, a, w);

#if 0
    a[w] = 0;
    ExpSyslog("WCOCW: Writing %s", a);
#endif

    if (p != buf) {
	free(p);
	free(a);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * OnReadConsoleInput --
 *
 *	This function gets called when a ReadConsoleInput breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Notes:
 *	If this is ever used for real, there need to be ASCII
 *	and UNICODE versions.
 *
 *----------------------------------------------------------------------
 */

static void
OnReadConsoleInput(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    LOG_ENTRY("ReadConsoleInput");
}

/*
 *----------------------------------------------------------------------
 *
 * OnSetConsoleMode --
 *
 *	This function gets called when a SetConsoleMode breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Sets some flags that are used in determining echoing
 *	characteristics of the slave driver.
 *
 *----------------------------------------------------------------------
 */

static void
OnSetConsoleMode(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
  DWORD i;
  BOOL found;

  LOG_ENTRY("SetConsoleMode");

  /* The console mode seems to get set even if the return value is FALSE */
  if (*returnValue == FALSE) {
    return;
  }
  for (found = FALSE, i = 0; i < proc->consoleHandlesMax; i++) {
    if (threadInfo->args[0] == proc->consoleHandles[i]) {
      found = TRUE;
      break;
    }
  }
  if (found) {
    ExpConsoleInputMode = threadInfo->args[1];
    EXP_LOG("New console mode 0x%x", ExpConsoleInputMode);
  } else {
    EXP_LOG("New console (unknown) mode 0x%x", ExpConsoleInputMode);
  }
}

/*
 *----------------------------------------------------------------------
 *
 * OnSetConsoleActiveScreenBuffer --
 *
 *	This function gets called when a SetConsoleActiveScreenBuffer
 *	breakpoint is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	We reread the entire console and send it to the master.
 *	Updates the current console cursor position
 *
 *----------------------------------------------------------------------
 */

static void
OnSetConsoleActiveScreenBuffer(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    LOG_ENTRY("SetConsoleActiveScreenBuffer");

    if (*returnValue == FALSE) {
	return;
    }

    RefreshScreen();
}

/*
 *----------------------------------------------------------------------
 *
 * OnSetConsoleCursorPosition --
 *
 *	This function gets called when a SetConsoleCursorPosition breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Updates the current console cursor position
 *
 *----------------------------------------------------------------------
 */

static void
OnSetConsoleCursorPosition(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    LOG_ENTRY("SetConsoleCursorPosition");

    if (*returnValue == FALSE) {
	return;
  }

    CreateVtSequence(proc, *((PCOORD) &threadInfo->args[1]), 0);
}

/*
 *----------------------------------------------------------------------
 *
 * OnSetConsoleWindowInfo --
 *
 *	This function gets called when a SetConsoleWindowInfo breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Updates the current console cursor position
 *
 *----------------------------------------------------------------------
 */

static void
OnSetConsoleWindowInfo(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    LOG_ENTRY("SetConsoleWindowInfo");
}

/*
 *----------------------------------------------------------------------
 *
 * OnScrollConsoleScreenBuffer --
 *
 *	This funtions gets called when a ScrollConsoleScreenBuffer
 *	breakpoint is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Generate some VT100 sequences to insert lines
 *
 * Notes:
 *	XXX: Ideally, we should check if the screen buffer is the one that
 *	is currently being displayed.  However, that means we have to
 *	track CONOUT$ handles, so we don't do it for now.
 *
 *----------------------------------------------------------------------
 */

void
OnScrollConsoleScreenBuffer(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    BOOL b;
    CHAR buf[100];
    DWORD count = 0;
    SMALL_RECT scroll, clip, *pClip;
    COORD dest;
    CHAR_INFO fill;
    CHAR c;
    PVOID ptr;
    LOG_ENTRY("ScrollConsoleScreenBuffer");

    if (*returnValue == FALSE) {
	return;
    }
    ptr = (PVOID) threadInfo->args[1];
    ReadSubprocessMemory(proc, ptr, &scroll, sizeof(SMALL_RECT));
    ptr = (PVOID) threadInfo->args[2];
    pClip = NULL;
    if (ptr) {
	pClip = &clip;
	ReadSubprocessMemory(proc, ptr, &clip, sizeof(SMALL_RECT));
    }
    dest = *((PCOORD) &threadInfo->args[3]);
    ptr = (PVOID) threadInfo->args[4];
    ReadSubprocessMemory(proc, ptr, &fill, sizeof(CHAR_INFO));
    c = fill.Char.AsciiChar;

    /* Check for a full line scroll */
    if (c == ' ' && scroll.Left == dest.X &&
	scroll.Left == 0 && scroll.Right >= ConsoleSize.X-1)
    {
	if (dest.Y < scroll.Top) {
	    wsprintfA(&buf[count], "\033[%d;%dr\033[%d;%dH\033[%dM",
		      dest.Y+1,scroll.Bottom+1,dest.Y+1,1,
		      scroll.Top - dest.Y);
	} else {
	    wsprintfA(&buf[count], "\033[%d;%dr\033[%d;%dH\033[%dL",
		      scroll.Top+1,dest.Y+1+(scroll.Bottom - scroll.Top),
		      scroll.Top+1,1,
		      dest.Y - scroll.Top);
	}
	count = strlen(&buf[count]);
	wsprintf(&buf[count], "\033[%d;%dr", 1, ConsoleSize.Y);
	count += strlen(&buf[count]);
	b = ExpWriteMaster(HMaster, buf, count);
    } else {
	RefreshScreen();
    }
}


/*
 *----------------------------------------------------------------------
 *
 * OnGetStdHandle --
 *
 *	This function gets called when a GetStdHandle breakpoint
 *	is hit.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Sets some flags that are used in determining echoing
 *	characteristics of the slave driver.
 *
 *----------------------------------------------------------------------
 */

static void
OnGetStdHandle(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    DWORD i;
    BOOL found;
  LOG_ENTRY ("GetStdHandle");

    if (*returnValue == (DWORD) INVALID_HANDLE_VALUE) {
	return;
    }
    if (threadInfo->args[0] != STD_INPUT_HANDLE) {
	return;
    }
    for (found = FALSE, i = 0; i < proc->consoleHandlesMax; i++) {
	if (proc->consoleHandles[i] == *returnValue) {
	    found = TRUE;
	    break;
	}
    }
    if (! found) {
	if (proc->consoleHandlesMax > 100) {
	    proc->consoleHandlesMax = 100;
	}
	proc->consoleHandles[proc->consoleHandlesMax++] = *returnValue;
    }
    return;
}

/*
 *----------------------------------------------------------------------
 *
 * OnBeep --
 *
 *	This routine gets called when Beep is called.  At least in sshd,
 *	we don't want a beep to show up on the local console.  Instead,
 *	direct it back to the master with a ASCII 7.
 *
 * Results:
 *	None
 *
 * Notes:
 *	XXX: Setting the duration to 0 doesn't seem to make the local
 *	beep go away.  It seems we need to stop the call at this point
 *	(or point it to some other call with the same number of arguments)
 *
 *----------------------------------------------------------------------
 */

static void
OnBeep(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    CHAR buf[50];

    LOG_ENTRY("Beep");

    if (direction == EXP_BREAK_IN) {
	/* Modify the arguments so a beep doesn't sound on the server */
	threadInfo->args[1] = 0;
    } else if (direction == EXP_BREAK_OUT) {
	if (*returnValue == 0) {
	    buf[0] = 7; /* ASCII beep */
	    ExpWriteMaster(HMaster, buf, 1);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * RefreshScreen --
 *
 *	Redraw the entire screen
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */
static void
RefreshScreen()
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    UCHAR buf[4096];
    DWORD bufpos = 0;
    CHAR_INFO consoleBuf[4096];
    COORD size = {ConsoleSize.X, ConsoleSize.Y};
    COORD begin = {0, 0};
    SMALL_RECT rect = {0, 0, ConsoleSize.X-1, ConsoleSize.Y-1};
    BOOL b;
    int x, y, prespaces, postspaces, offset;

    LOG_ENTRY("RefreshScreen");

    /* Clear the screen */
    wsprintfA(&buf[bufpos], "\033[2J");
    bufpos += strlen(&buf[bufpos]);

    wsprintfA(&buf[bufpos], "\033[%d;%dH",
	      CursorPosition.Y+1, CursorPosition.X+1);
    bufpos += strlen(&buf[bufpos]);
    CursorKnown = TRUE;

    b = ExpWriteMaster(HMaster, buf, bufpos);
    bufpos = 0;

    if (GetConsoleScreenBufferInfo(HConsole, &info) != FALSE) {
	return;
    }

    CursorPosition = info.dwCursorPosition;

    if (! ReadConsoleOutput(HConsole, consoleBuf, size, begin, &rect)) {
	return;
    }

    offset = 0;
    for (y = 0; y < ConsoleSize.Y; y++) {
	offset += ConsoleSize.X;
	for (x = 0; x < ConsoleSize.X; x++) {
	    if (consoleBuf[offset+x].Char.AsciiChar != ' ') {
		break;
	    }
	}
	prespaces = x;
	if (prespaces == ConsoleSize.X) {
	    continue;
	}

	for (x = ConsoleSize.X-1; x >= 0; x--) {
	    if (consoleBuf[offset+x].Char.AsciiChar != ' ') {
		break;
	    }
	}
	postspaces = x;
	wsprintfA(&buf[bufpos], "\033[%d;%dH", y+1, prespaces+1);

	for (x = prespaces; x < postspaces; x++) {
	    buf[bufpos] = consoleBuf[offset+x].Char.AsciiChar;
	    bufpos++;
	}
    }

    wsprintfA(&buf[bufpos], "\033[%d;%dH",
	      CursorPosition.Y+1, CursorPosition.X+1);
    bufpos += strlen(&buf[bufpos]);
    CursorKnown = TRUE;
    b = ExpWriteMaster(HMaster, buf, bufpos);
}

/*
 *----------------------------------------------------------------------
 *
 * OnIsWindowVisible --
 *
 *	This routine gets called when IsWindowVisible is called.
 *	The MKS Korn shell uses this as an indication of a window
 *	that can be seen by the user.  If the window can't be seen,
 *	it pops up a graphical error notification.  We really, really
 *	don't want those damn things popping up, so this helps avoid
 *	it.  And there really doesn't seem to be any good reason to
 *	return FALSE given that nobody is ever going to see anything.
 *
 * Results:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void
OnIsWindowVisible(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    LOG_ENTRY("IsWindowVisible");

    *returnValue = TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * ReadSubprocessMemory --
 *
 *	Reads memory from the subprocess.  Takes care of all the
 *	issues with page protection.
 *
 * Results:
 *	FALSE if unsuccessful, TRUE if successful.
 *
 * Notes:
 *	The efficient memory reading routine is disabled here
 *	because it doesn't quite work right.  I don't see the
 *	problem in the code, but there must be something there
 *	since the test suite fails when run with this code
 *	enabled.  When it works, it should be much faster than
 *	the current safe but slow implementation.
 *
 *----------------------------------------------------------------------
 */

#ifdef XXX
BOOL
ReadSubprocessMemory(ExpProcess *proc, LPVOID addr, LPVOID buf, DWORD len)
{
    DWORD oldProtection = 0;
    MEMORY_BASIC_INFORMATION mbi;
    BOOL ret = TRUE;
    DWORD offset;
    DWORD base, curr, end, n;
    HANDLE hProcess;
    PBYTE bufpos = buf;

    hProcess = proc->hProcess;

    end = len + (DWORD) addr;
    for (curr = (DWORD) addr; curr < end; ) {
	base = curr & (~PAGEMASK);
	offset = curr & PAGEMASK;
	if (offset + len > PAGESIZE) {
	    n = PAGESIZE - offset;
	} else {
	    n = len;
	}
	if (proc->pMemoryCacheBase != (curr & PAGEMASK)) {
	    /* if not committed memory abort */
	    if (!VirtualQueryEx(hProcess, (LPVOID) base, &mbi, sizeof(mbi)) ||
		(mbi.State != MEM_COMMIT))
	    {
		return FALSE;
	    }

	    /* if guarded memory, change protection temporarily */
	    if (!(mbi.Protect & PAGE_READONLY) &&
		!(mbi.Protect & PAGE_READWRITE))
	    {
		VirtualProtectEx(hProcess, (LPVOID) base, PAGESIZE,
		    PAGE_READONLY, &oldProtection);
	    }

	    if (!ReadProcessMemory(hProcess, (LPVOID) base, proc->pMemoryCache,
		PAGESIZE, NULL)) {
		ret = FALSE;
	    }

	    /* reset protection if changed */
	    if (oldProtection) {
		VirtualProtectEx(hProcess, (LPVOID) base, PAGESIZE,
		    oldProtection, &oldProtection);
	    }
	    if (ret == FALSE) {
		return FALSE;
	    }
	    proc->pMemoryCacheBase = base;
	}

	memcpy(bufpos, &proc->pMemoryCache[offset], n);
	bufpos += n;
	curr += n;
    }

    return ret;
}


#else
BOOL
ReadSubprocessMemory(ExpProcess *proc, LPVOID addr, LPVOID buf, DWORD len)
{
    DWORD oldProtection = 0;
    MEMORY_BASIC_INFORMATION mbi;
    BOOL ret;
    LONG error;

    /* if not committed memory abort */
    if (!VirtualQueryEx(proc->hProcess, addr, &mbi, sizeof(mbi)) ||
	mbi.State != MEM_COMMIT)
    {
	return FALSE;
    }

    /* if guarded memory, change protection temporarily */
    if (!(mbi.Protect & PAGE_READONLY) && !(mbi.Protect & PAGE_READWRITE)) {
	VirtualProtectEx(proc->hProcess, addr, len, PAGE_READONLY, &oldProtection);
    }

    ret = ReadProcessMemory(proc->hProcess, addr, buf, len, NULL);
    if (ret == FALSE) {
	error = GetLastError();
    }

    /* reset protection if changed */
    if (oldProtection) {
	VirtualProtectEx(proc->hProcess, addr, len, oldProtection, &oldProtection);
	SetLastError(error);
    }
    return ret;
}
#endif /* XXX */

/*
 *----------------------------------------------------------------------
 *
 * WriteSubprocessMemory --
 *
 *	Writes memory from the subprocess.  Takes care of all the
 *	issues with page protection.
 *
 * Results:
 *	0 if unsuccessful, 1 if successful.
 *
 *----------------------------------------------------------------------
 */

BOOL
WriteSubprocessMemory(ExpProcess *proc, LPVOID addr, LPVOID buf, DWORD len)
{
    DWORD oldProtection = 0;
    MEMORY_BASIC_INFORMATION mbi;
    BOOL ret = TRUE;
    DWORD err;
    HANDLE hProcess;

    hProcess = proc->hProcess;

    /* Flush the read cache */
    proc->pMemoryCacheBase = 0;

    /* if not committed memory abort */
    if (!VirtualQueryEx(hProcess, addr, &mbi, sizeof(mbi)) ||
	mbi.State != MEM_COMMIT)
    {
	ret = FALSE;
	/* assert(ret != FALSE); */
	return ret;
    }

    /* if guarded memory, change protection temporarily */
    if (!(mbi.Protect & PAGE_READWRITE)) {
	if (!VirtualProtectEx(hProcess, addr, len, PAGE_READWRITE,
			      &oldProtection)) {
	    err = GetLastError();
	}
    }

    if (!WriteProcessMemory(hProcess, addr, buf, len, NULL)) {
	ret = FALSE;
	err = GetLastError();
    }

    /* reset protection if changed */
    if (oldProtection) {
	VirtualProtectEx(hProcess, addr, len, oldProtection, &oldProtection);
    }
#if 0 /* Debugging purposes only */
    if (ret == FALSE) {
	assert(ret != FALSE);
    }
#endif
    return ret;
}
#endif /* !UNICODE */

/*
 * Everything after this point gets compiled twice, once with the UNICODE flag
 * and once without.  This gets us the Unicode and non-Unicode versions
 * of the code that we need
 */

/*
 *----------------------------------------------------------------------
 *
 * OnWriteConsoleOutput --
 *
 *	This function gets called when an WriteConsoleOutputA breakpoint
 *	is hit.  The data is also redirected to expect since expect
 *	normally couldn't see any output going through this interface.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Prints some output.
 *
 *----------------------------------------------------------------------
 */

void
OnWriteConsoleOutput(ExpProcess *proc, ExpThreadInfo *threadInfo,
    ExpBreakpoint *brkpt, PDWORD returnValue, DWORD direction)
{
    CHAR buf[1024];
    PVOID ptr;
    DWORD n;
    PCHAR p, end;
    int maxbuf;
    BOOL b;
    COORD bufferSize;
    COORD bufferCoord;
    COORD curr;
    SMALL_RECT writeRegion;
    CHAR_INFO *charBuf, *pcb;
    SHORT x, y;

    LOG_ENTRY("WriteConsoleOutput");

    if (*returnValue == 0) {
	return;
    }

    bufferSize = *((PCOORD) &threadInfo->args[2]);
    bufferCoord = *((PCOORD) &threadInfo->args[3]);
    ptr = (PVOID) threadInfo->args[4]; /* Get the rectangle written */
    if (ptr == NULL) return;
    ReadSubprocessMemory(proc, ptr, &writeRegion,sizeof(SMALL_RECT));

    ptr = (PVOID) threadInfo->args[1]; /* Get character array */
    if (ptr == NULL) return;

    n = bufferSize.X * bufferSize.Y * sizeof(CHAR_INFO);
    charBuf = malloc(n);

    ReadSubprocessMemory(proc, ptr, charBuf, n);

    pcb = charBuf;
    for (y = 0; y <= writeRegion.Bottom - writeRegion.Top; y++) {
	pcb = charBuf;
	pcb += (y + bufferCoord.Y) * bufferSize.X;
	pcb += bufferCoord.X;
	p = buf;
	maxbuf = sizeof(buf);
	end = buf + maxbuf;
	for (x = 0; x <= writeRegion.Right - writeRegion.Left; x++, pcb++) {
#ifdef UNICODE
	    *p++ = (CHAR) (pcb->Char.UnicodeChar & 0xff);
#else
	    *p++ = pcb->Char.AsciiChar;
#endif
	    if (p == end) {
	      EXP_LOG ("(debug): OnWriteConsoleOutput >> \n'%s'", buf);
		b = ExpWriteMaster(HMaster, buf, maxbuf);
		p = buf;
	    }
	}
	curr.X = writeRegion.Left;
	curr.Y = writeRegion.Top + y;
	n = writeRegion.Right - writeRegion.Left;
	CreateVtSequence(proc, curr, n);

	maxbuf = p - buf;
	// DEBUG
	buf[maxbuf]='\0';
	EXP_LOG ("(debug): OnWriteConsoleOutput >> \n'%s'", buf);
	// End DEBUG
	b = ExpWriteMaster(HMaster, buf, maxbuf);
	buf[maxbuf] = 0;
#if 0
	ExpSyslog("Writing %s", buf);
#endif
    }

    free(charBuf);
    LOG_EXIT("WriteConsoleOutput");
}

/*
 *----------------------------------------------------------------------
 *
 * ReadSubprocessString --
 *
 *	Read a character string from the subprocess
 *
 * Results:
 *	The length of the string
 *
 *----------------------------------------------------------------------
 */

int
ReadSubprocessString(ExpProcess *proc, PVOID base, PTCHAR buf, int buflen)
{
    PTCHAR ip, op;
    int i;

    ip = base;
    op = buf;
    i = 0;
    while (i < buflen-1) {
	if (! ReadSubprocessMemory(proc, ip, op, sizeof(TCHAR))) {
	    break;
	}
	if (*op == 0) break;
	op++; ip++; i++;
    }
    *op = 0;
    return i;
}
