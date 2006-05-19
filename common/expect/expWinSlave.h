/*
 * expWinSlave.h
 *
 *	Useful definitions used by the slave driver but not useful
 *	for anybody else.
 *
 * Copyright (c) 2006 AdaCore
 * Copyright (c) 1997 by Mitel, Inc.
 * Copyright (c) 1997 by Gordon Chaffee (chaffee@home.com)
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

typedef struct ExpSlaveDebugArg {
  char *hMasterPipe;            /* Master pipe name */
  HANDLE hConsole;		/* Console handle to use */
  HANDLE process;		/* Handle to subprocess */
  DWORD globalPid;		/* Program identifier of slave */
  HANDLE thread;		/* Handle of debugger thread */

  HANDLE event;		/* Gets set when the process has been created */
  DWORD result;		/* Result of process being started */
  DWORD lastError;		/* GetLastError for result */

  /* Args for ExpCreateProcess */
  int argc;			/* Number of args to start slave program */
  char **argv;		/* Argument list of slave program */
  HANDLE slaveStdin;		/* stdin for slave program */
  HANDLE slaveStdout;		/* stdout for slave program */
  HANDLE slaveStderr;		/* stderr for slave program */
} ExpSlaveDebugArg;

typedef struct _EXP_KEY {
    WORD wVirtualKeyCode;
    WORD wVirtualScanCode;
    DWORD dwControlKeyState;
} EXP_KEY;

#define EXP_KEY_CONTROL 0
#define EXP_KEY_SHIFT   1
#define EXP_KEY_LSHIFT  1
#define EXP_KEY_RSHIFT  2
#define EXP_KEY_ALT     3


/* For ExpVtFunctionToKeyArray.  Ordering must match ExpFunctionToKeyArray[] */
#define EXP_KEY_UP		0
#define EXP_KEY_DOWN		1
#define EXP_KEY_RIGHT		2
#define EXP_KEY_LEFT		3
#define EXP_KEY_END		4
#define EXP_KEY_HOME		5
#define EXP_KEY_PAGEUP		6
#define EXP_KEY_PAGEDOWN	7
#define EXP_KEY_INSERT		8
#define EXP_KEY_DELETE		9
#define EXP_KEY_SELECT		10
#define EXP_KEY_F1		11
#define EXP_KEY_F2		12
#define EXP_KEY_F3		13
#define EXP_KEY_F4		14
#define EXP_KEY_F5		15
#define EXP_KEY_F6		16
#define EXP_KEY_F7		17
#define EXP_KEY_F8		18
#define EXP_KEY_F9		19
#define EXP_KEY_F10		20
#define EXP_KEY_F11		21
#define EXP_KEY_F12		22
#define EXP_KEY_F13		23
#define EXP_KEY_F14		24
#define EXP_KEY_F15		25
#define EXP_KEY_F16		26
#define EXP_KEY_F17		27
#define EXP_KEY_F18		28
#define EXP_KEY_F19		29
#define EXP_KEY_F20		30
#define EXP_WIN_RESIZE		31

extern EXP_KEY ExpModifierKeyArray[];
extern EXP_KEY ExpAsciiToKeyArray[];
extern EXP_KEY ExpFunctionToKeyArray[];
extern DWORD   ExpConsoleInputMode;
extern int     ExpDebug;

extern void			ExpAddToWaitQueue(HANDLE handle);
extern void			ExpKillProcessList();
extern DWORD WINAPI		ExpSlaveDebugThread(LPVOID *arg);
extern DWORD WINAPI		ExpGetExecutablePathA(PSTR pathInOut);
extern DWORD WINAPI		ExpGetExecutablePathW(PWSTR pathInOut);
extern BOOL			ExpWriteMaster(HANDLE hFile,
				    LPCVOID buf, DWORD n);
extern BOOL			ExpReadMaster(HANDLE hFile,
				    void *buf, DWORD n, PDWORD pCount,
				    PDWORD pError);
extern void			ExpNewConsoleSequences(HANDLE hMaster);
extern void			ExpProcessFreeByHandle(HANDLE hProcess);
extern void			ExpSetConsoleSize(HANDLE hConsoleInW,
						  HANDLE hConsoleOut,
						  int w, int h);
extern BOOL                     PipeRespondToMaster(HANDLE handle, DWORD value,
                                                    DWORD pid);
