#ifndef _EXPWINPROCESS
#define _EXPWINPROCESS

/*
 *--------------------------------------------------------------------
 *
 * ExpApplicationType --
 *
 *	Search for the specified program and identify if it refers to a DOS,
 *	Windows 3.X, or Win32 program.  Used to determine how to invoke 
 *	a program, or if it can even be invoked.
 *
 *	It is possible to almost positively identify DOS and Windows 
 *	applications that contain the appropriate magic numbers.  However, 
 *	DOS .com files do not seem to contain a magic number; if the program 
 *	name ends with .com and could not be identified as a Windows .com
 *	file, it will be assumed to be a DOS application, even if it was
 *	just random data.  If the program name does not end with .com, no 
 *	such assumption is made.
 *
 *	The Win32 procedure GetBinaryType incorrectly identifies any 
 *	junk file that ends with .exe as a dos executable and some 
 *	executables that don't end with .exe as not executable.  Plus it 
 *	doesn't exist under win95, so I won't feel bad about reimplementing
 *	functionality.
 *
 * Results:
 *	The return value is one of EXP_APPL_DOS, EXP_APPL_WIN3X, or EXP_APPL_WIN32
 *	if the filename referred to the corresponding application type.
 *	If the file name could not be found or did not refer to any known 
 *	application type, EXP_APPL_NONE is returned and the caller can use
 *	GetLastError() to find out what went wrong.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

DWORD
ExpApplicationType(const char *originalName, char *fullPath, char *imagePath);

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
Exp_WaitPid(HANDLE pid, LPDWORD statPtr, DWORD options);

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
Exp_KillProcess(HANDLE pid);

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
ExpCreateProcess
  (int argc, char **argv, 
   HANDLE inputHandle, HANDLE outputHandle, HANDLE errorHandle,
   int allocConsole, int hideConsole, int debug, int newProcessGroup,
   PHANDLE pidPtr, PDWORD globalPidPtr);

#endif
