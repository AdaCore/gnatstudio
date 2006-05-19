/*
 * expDString.h --
 *
 *	DString definition
 *
 * Copyright (c) 2006 AdaCore
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _EXP_DSTRING
#define _EXP_DSTRING

#define TCL_DSTRING_STATIC_SIZE 200
typedef struct Tcl_DString {
    char *string;		/* Points to beginning of string:  either
				 * staticSpace below or a malloced array. */
    int length;			/* Number of non-NULL characters in the
				 * string. */
    int spaceAvl;		/* Total number of bytes available for the
				 * string and its terminating NULL char. */
    char staticSpace[TCL_DSTRING_STATIC_SIZE];
				/* Space to use in common case where string
				 * is small. */
} Tcl_DString;

#define Tcl_DStringLength(dsPtr) ((dsPtr)->length)
#define Tcl_DStringValue(dsPtr) ((dsPtr)->string)
#define Tcl_DStringTrunc Tcl_DStringSetLength

#endif
