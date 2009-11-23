#ifndef _TCL_
#define _TCL_

#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus) || defined(USE_PROTOTYPE)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)       x
#   define CONST const
#else
#   define _ANSI_ARGS_(x)       ()
#   define CONST
#endif


/*
 * Macro to use instead of "void" for arguments that must have
 * type "void *" in ANSI C;  maps them to type "char *" in
 * non-ANSI systems.
 */
#ifndef __WIN32__
#ifndef VOID
#   ifdef __STDC__
#       define VOID void
#   else
#       define VOID char
#   endif
#endif
#else /* __WIN32__ */
/*
 * The following code is copied from winnt.h
 */
#ifndef VOID
#define VOID void
#endif
#endif /* __WIN32__ */

#define TCL_OK          0

#define TCL_ENCODING_START              0x01
#define TCL_ENCODING_END                0x02
#define TCL_ENCODING_STOPONERROR        0x04

#define TCL_DSTRING_STATIC_SIZE	200

typedef struct Tcl_DString {
    char *string;               /* Points to beginning of string:  either
                                 * staticSpace below or a malloced array. */
    int length;                 /* Number of non-NULL characters in the
                                 * string. */
    int spaceAvl;               /* Total number of bytes available for the
                                 * string and its terminating NULL char. */
    char staticSpace[TCL_DSTRING_STATIC_SIZE];
                                /* Space to use in common case where string
                                 * is small. */
} Tcl_DString;

#define Tcl_DStringLength(dsPtr) ((dsPtr)->length)
#define Tcl_DStringValue(dsPtr) ((dsPtr)->string)

EXTERN void             Tcl_DStringInit _ANSI_ARGS_((Tcl_DString *dsPtr));
EXTERN void             Tcl_DStringFree _ANSI_ARGS_((Tcl_DString *dsPtr));
EXTERN char *           Tcl_DStringAppend _ANSI_ARGS_((Tcl_DString *dsPtr,
                            CONST char *string, int length));

// not implemented
typedef char*	Tcl_Encoding;
typedef char*	Tcl_Interp;
typedef char*   Tcl_EncodingState;

EXTERN char *           Tcl_ExternalToUtfDString _ANSI_ARGS_((
                            Tcl_Encoding encoding, CONST char *src,
                            int srcLen, Tcl_DString *dsPtr));

EXTERN char *           Tcl_UtfToExternalDString _ANSI_ARGS_((
                            Tcl_Encoding encoding, CONST char *src,
                            int srcLen, Tcl_DString *dsPtr));
EXTERN void             Tcl_FindExecutable _ANSI_ARGS_((CONST char *argv0));

EXTERN Tcl_Encoding     Tcl_GetEncoding _ANSI_ARGS_((Tcl_Interp *interp,
                            CONST char *name));

EXTERN void             Tcl_FreeEncoding _ANSI_ARGS_((Tcl_Encoding encoding));
EXTERN void             Tcl_Finalize _ANSI_ARGS_((void));

EXTERN int              Tcl_ExternalToUtf _ANSI_ARGS_((Tcl_Interp *interp,
                            Tcl_Encoding encoding, CONST char *src, int srcLen,
                            int flags, Tcl_EncodingState *statePtr, char *dst,
                            int dstLen, int *srcReadPtr, int *dstWrotePtr,
                            int *dstCharsPtr));

EXTERN int              Tcl_UtfToExternal _ANSI_ARGS_((Tcl_Interp *interp,
                            Tcl_Encoding encoding, CONST char *src, int srcLen,
                            int flags, Tcl_EncodingState *statePtr, char *dst,
                            int dstLen, int *srcReadPtr, int *dstWrotePtr,
                            int *dstCharsPtr));



#define ckalloc		malloc
#define ckfree		free
#define ckrealloc	realloc

#endif

