/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

/* $Id$ */

/* $Log:
 */

/* Ich, Doktor Josef Grosch, Informatiker, Jan. 1992 */

/* interface for machine dependencies */

/* See the header file rSystem.h for comments concerning the external routines!
*/

/* compilation with the option -DUNIX uses UNIX system calls for IO (efficient),
   otherwise the C library routines are used for IO (portable).		*/

#include "rSystem.h"
#include "Reuse.h"

#include <stdlib.h>
#include <tcl.h>

#ifndef rfalse
#define rfalse 0
#endif
#ifndef rtrue
#define rtrue 1
#endif

#ifdef UNIX
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#else

#ifdef __cplusplus
extern "C" {
#include <stdio.h>
}
#else
#include <stdio.h>
#endif

#define NOFILES 64

static rbool IsInitialized = rfalse;

static char IsLineBuffered [NOFILES] = { 1, 1, 1, };

static FILE *	FileStore [NOFILES] = {
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
};

static void InitFileStore ARGS ((void))
{
#ifndef _USRDLL
   FileStore [0] = stdin;
   FileStore [1] = stdout;
   FileStore [2] = stderr;
#endif
   IsInitialized = rtrue;
}

static tFile FileToInt
#if defined __STDC__ | defined __cplusplus
   (FILE * File)
#else
   (File) FILE * File;
#endif
{
   register int	f = fileno (File);
   FileStore [f] = File;
   return f;
}

static FILE * IntToFile
#if defined __STDC__ | defined __cplusplus
   (tFile File)
#else
   (File) tFile File;
#endif
{
   if (! IsInitialized) InitFileStore ();
   return FileStore [File];
}

#endif

/* binary IO */

#ifdef OS2

#ifdef __cplusplus
extern "C" {
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
}
#else
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#endif

#else

#ifdef MVS

#include <sys/times.h>

#else

#ifdef __cplusplus
extern "C" {
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef _MSC_VER
#include <sys/times.h>
#endif
}
#else
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef _MSC_VER
#include <sys/times.h>
#endif
#endif

#endif
#endif

rbool IsCharacterSpecial
#if defined __STDC__ | defined __cplusplus
   (tFile File)
#else
   (File) tFile File;
#endif
{
#ifdef MVS
   return rfalse;
#else
   struct stat	buf;
   (void) fstat (File, & buf);
   return (0020000 & buf.st_mode) == 0020000;
#endif
}

tFile OpenInput
#if defined __STDC__ | defined __cplusplus
   (char * FileName)
#else
   (FileName) char * FileName;
#endif
{
#ifdef UNIX
   return open (FileName, O_RDONLY);
#else
   FILE * FilePtr;
   tFile File;
#ifdef MSDOS
   int i, l = strlen (FileName);
   for (i = 0; i < l; i ++) if (FileName [i] == '/') FileName [i] = '\\';
   FilePtr = fopen (FileName, "rb");
#else
#ifdef MVS
   FilePtr = fopen (FileName, "r+,lrecl=80,blksize=240,recfm=f,type=record");
#else
   FilePtr = fopen (FileName, "r");
#endif
#endif
   if (FilePtr == NULL) return -1;
   if (! IsInitialized) InitFileStore ();
   File = FileToInt (FilePtr);
   IsLineBuffered [File] = IsCharacterSpecial (File);
   return File;
#endif
}

tFile OpenOutput
#if defined __STDC__ | defined __cplusplus
   (char * FileName)
#else
   (FileName) char * FileName;
#endif
{
#ifdef UNIX
   return creat (FileName, 0666);
#else
#ifdef MSDOS
   FILE * FilePtr = fopen (FileName, "wb");
#else
   FILE * FilePtr = fopen (FileName, "w");
#endif
   if (! IsInitialized) InitFileStore ();
   return FilePtr == NULL ? -1 : FileToInt (FilePtr);
#endif
}

int rRead
#if defined __STDC__ | defined __cplusplus
   (tFile File, char * Buffer, int Size)
#else
   (File, Buffer, Size) tFile File; char * Buffer; int Size;
#endif
{
#ifdef UNIX
   return read (File, Buffer, Size);
#else
   if (IsLineBuffered [File]) {
      Buffer [0] = '\0';
      (void) fgets (Buffer, Size, IntToFile (File));
      return strlen (Buffer);
   } else
      return fread (Buffer, 1, Size, IntToFile (File));
#endif
}

int rWrite
#if defined __STDC__ | defined __cplusplus
   (tFile File, char * Buffer, int Size)
#else
   (File, Buffer, Size) tFile File; char * Buffer; int Size;
#endif
{
#ifdef UNIX
   return write (File, Buffer, Size);
#else
   return fwrite (Buffer, 1, Size, IntToFile (File));
#endif
}

void rClose
#if defined __STDC__ | defined __cplusplus
   (tFile File)
#else
   (File) tFile File;
#endif
{
#ifdef UNIX
   (void) close (File);
#else
   (void) fclose (IntToFile (File));
#endif
}

/* calls other than IO */

#ifdef _MSC_VER
#include <time.h>
#endif

char * rAlloc
#if defined __STDC__ | defined __cplusplus
   (long ByteCount)
#else
   (ByteCount) long ByteCount;
#endif
{
   return (char *) ckalloc ((unsigned long) ByteCount);
}

void rFree
#if defined __STDC__ | defined __cplusplus
   (char * ptr)
#else
   (ptr) char * ptr;
#endif
{
   ckfree (ptr);
}

long rTime ARGS ((void))
{
   return 0;
}

static int	argc;
static char * *	argv;

int GetArgCount ARGS ((void))
{ return argc; }

void GetArgument
#if defined __STDC__ | defined __cplusplus
   (int ArgNum, char * Argument)
#else
   (ArgNum, Argument) int ArgNum; char * Argument;
#endif
{
   register int	i = 0;
   for (;; i ++)
      if ((Argument [i] = argv [ArgNum][i]) == '\0') return;
}

char * GetEnvVar
#if defined __STDC__ | defined __cplusplus
   (char * Name)
#else
   (Name) char * Name;
#endif
{ return getenv (Name); }

void PutArgs
#if defined __STDC__ | defined __cplusplus
   (int Argc, char * * Argv)
#else
   (Argc, Argv) int Argc; char * * Argv;
#endif
{
   argc = Argc;
   argv = Argv;
}

#include <errno.h>

int rErrNo ARGS ((void))
{ return errno; }

int rSystem
#if defined __STDC__ | defined __cplusplus
   (char * String)
#else
   (String) char * String;
#endif
{
#ifdef _MSC_VER
   return -1;
#else
   return system (String);
#endif
}

void rExit
#if defined __STDC__ | defined __cplusplus
   (int Status)
#else
   (Status) int Status;
#endif
#ifndef _USRDLL
{ (void) exit (Status); }
#else
{}
#endif

void BEGIN_rSystem ARGS ((void))
{}

