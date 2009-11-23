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

#include <stdio.h>
#include <tcl.h>

#include "Source.h"

extern FILE * yyin;
extern Tcl_Encoding encoding;

static Tcl_Encoding ascii;

int
BeginSource(char * Filename)
{
  if (encoding != NULL && ascii == NULL)
    {
      if ((ascii = Tcl_GetEncoding(NULL, "ascii")) == NULL)
	{
	  printf("ASCII encoding not found\n");
	  return -1;
	}
    }
  return 0;
}

int
GetLine(int File, char * Buffer, int Size)
{
  char * buf;
  int len, nbytes;
  Tcl_DString external, intermediate, internal;

  /* Shortcut if we aren't doing any translation. */
  if (encoding == NULL)
    {
      return fread(Buffer, sizeof(char), Size, yyin);
    }

  /* FIXME: this assumes that the converted string will be no longer
     than what we start with. */

  buf = (char *) ckalloc(Size);
  if (buf == NULL)
    {
      return 0;
    }

  nbytes = fread(buf, sizeof(unsigned char), Size, yyin);
  if (nbytes == 0)
    {
      ckfree(buf);
      return 0;
    }

  Tcl_DStringInit(&internal);
  Tcl_DStringInit(&intermediate);
  Tcl_DStringInit(&external);
  
  Tcl_DStringAppend(&external, buf, nbytes);

  Tcl_ExternalToUtfDString(encoding,
			   Tcl_DStringValue(&external),
			   Tcl_DStringLength(&external),
			   &intermediate);

  Tcl_UtfToExternalDString(ascii,
			   Tcl_DStringValue(&intermediate),
			   Tcl_DStringLength(&intermediate),
			   &external);

  len = Tcl_DStringLength(&external);
  if (len <= Size)
    {
      memcpy(Buffer, Tcl_DStringValue(&external), len);
    }
  else
    {
      /* Since no character can be smaller than one byte, this should
	 never happen.  If it does, give up gracefully. */
      len = 0;
    }

  /* Clean up. */
  Tcl_DStringFree(&internal);
  Tcl_DStringFree(&intermediate);
  Tcl_DStringFree(&external);
  ckfree(buf);

  return len;
}

void
CloseSource(int File)
{
  /* Do nothing, for some reason. */
}

