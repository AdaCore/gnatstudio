#include "tcl.h"
#include <stdlib.h>

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringInit --
 *
 *      Initializes a dynamic string, discarding any previous contents
 *      of the string (Tcl_DStringFree should have been called already
 *      if the dynamic string was previously in use).
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The dynamic string is initialized to be empty.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DStringInit(dsPtr)
    Tcl_DString *dsPtr;         /* Pointer to structure for dynamic string. */
{
    dsPtr->string = dsPtr->staticSpace;
    dsPtr->length = 0;
    dsPtr->spaceAvl = TCL_DSTRING_STATIC_SIZE;
    dsPtr->staticSpace[0] = '\0';
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringFree --
 *
 *      Frees up any memory allocated for the dynamic string and
 *      reinitializes the string to an empty state.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The previous contents of the dynamic string are lost, and
 *      the new value is an empty string.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DStringFree(dsPtr)
    Tcl_DString *dsPtr;         /* Structure describing dynamic string. */
{
    if (dsPtr->string != dsPtr->staticSpace) {
        free(dsPtr->string);
    }
    dsPtr->string = dsPtr->staticSpace;
    dsPtr->length = 0;
    dsPtr->spaceAvl = TCL_DSTRING_STATIC_SIZE;
    dsPtr->staticSpace[0] = '\0';
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringAppend --
 *
 *      Append more characters to the current value of a dynamic string.
 *
 * Results:
 *      The return value is a pointer to the dynamic string's new value.
 *
 * Side effects:
 *      Length bytes from string (or all of string if length is less
 *      than zero) are added to the current value of the string. Memory
 *      gets reallocated if needed to accomodate the string's new size.
 *
 *----------------------------------------------------------------------
 */

char *
Tcl_DStringAppend(dsPtr, string, length)
    Tcl_DString *dsPtr;         /* Structure describing dynamic string. */
    CONST char *string;         /* String to append.  If length is -1 then
                                 * this must be null-terminated. */
    int length;                 /* Number of characters from string to
                                 * append.  If < 0, then append all of string,
                                 * up to null at end. */
{
    int newSize;
    char *dst;
    CONST char *end;

    if (length < 0) {
        length = strlen(string);
    }
    newSize = length + dsPtr->length;

    /*
     * Allocate a larger buffer for the string if the current one isn't
     * large enough. Allocate extra space in the new buffer so that there
     * will be room to grow before we have to allocate again.
     */

    if (newSize >= dsPtr->spaceAvl) {
        dsPtr->spaceAvl = newSize * 2;
        if (dsPtr->string == dsPtr->staticSpace) {
            char *newString;

            newString = (char *) malloc((unsigned) dsPtr->spaceAvl);
            memcpy((VOID *) newString, (VOID *) dsPtr->string,
                    (size_t) dsPtr->length);
            dsPtr->string = newString;
        } else {
            dsPtr->string = (char *) realloc((VOID *) dsPtr->string,
                    (size_t) dsPtr->spaceAvl);
        }
    }

    /*
     * Copy the new string into the buffer at the end of the old
     * one.
     */

    for (dst = dsPtr->string + dsPtr->length, end = string+length;
            string < end; string++, dst++) {
        *dst = *string;
    }
    *dst = '\0';
    dsPtr->length += length;
    return dsPtr->string;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringSetLength --
 *
 *      Change the length of a dynamic string.  This can cause the
 *      string to either grow or shrink, depending on the value of
 *      length.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The length of dsPtr is changed to length and a null byte is
 *      stored at that position in the string.  If length is larger
 *      than the space allocated for dsPtr, then a panic occurs.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DStringSetLength(dsPtr, length)
    Tcl_DString *dsPtr;         /* Structure describing dynamic string. */
    int length;                 /* New length for dynamic string. */
{
    if (length < 0) {
        length = 0;
    }
    if (length >= dsPtr->spaceAvl) {
        dsPtr->spaceAvl = length + 1;
        if (dsPtr->string == dsPtr->staticSpace) {
            char *newString;

            newString = (char *) malloc((unsigned) dsPtr->spaceAvl);
            memcpy((VOID *) newString, (VOID *) dsPtr->string,
                    (size_t) dsPtr->length);
            dsPtr->string = newString;
        } else {
            dsPtr->string = (char *) realloc((VOID *) dsPtr->string,
                    (size_t) dsPtr->spaceAvl);
        }
    }
    dsPtr->length = length;
    dsPtr->string[length] = 0;
}



char* Tcl_ExternalToUtfDString (
                            Tcl_Encoding encoding, CONST char *src,
                            int srcLen, Tcl_DString *dsPtr) {
    int len = srcLen < 0 ? strlen (src) : srcLen;
    Tcl_DStringInit (dsPtr);
    Tcl_DStringSetLength (dsPtr, len);
    memcpy (Tcl_DStringValue (dsPtr), src, len);
    return 0;
}

char* Tcl_UtfToExternalDString (
                            Tcl_Encoding encoding, CONST char *src,
                            int srcLen, Tcl_DString *dsPtr) {
    int len = srcLen < 0 ? strlen (src) : srcLen;
    Tcl_DStringInit (dsPtr);
    Tcl_DStringSetLength (dsPtr, len);
    memcpy (Tcl_DStringValue (dsPtr), src, len);
    return 0;
}

void Tcl_FindExecutable (CONST char *argv0) {
    return;
}


Tcl_Encoding Tcl_GetEncoding (Tcl_Interp *interp, CONST char *name) {
    return 0;
}

void Tcl_FreeEncoding (Tcl_Encoding encoding) {
    return;
}

void Tcl_Finalize (void) {
    return;
}

/*
 *-------------------------------------------------------------------------
 *
 * Tcl_UtfToExternal --
 *
 *      Convert a buffer from UTF-8 into the specified encoding.
 *
 * Results:
 *      The return value is one of TCL_OK, TCL_CONVERT_MULTIBYTE,
 *      TCL_CONVERT_SYNTAX, TCL_CONVERT_UNKNOWN, or TCL_CONVERT_NOSPACE,
 *      as documented in tcl.h.
 *
 * Side effects:
 *      The converted bytes are stored in the output buffer.  
 *
 *-------------------------------------------------------------------------
 */

int
Tcl_UtfToExternal(interp, encoding, src, srcLen, flags, statePtr, dst,
        dstLen, srcReadPtr, dstWrotePtr, dstCharsPtr)
    Tcl_Interp *interp;         /* Interp for error return, if not NULL. */
    Tcl_Encoding encoding;      /* The encoding for the converted string,
                                 * or NULL for the default system encoding. */
    CONST char *src;            /* Source string in UTF-8. */
    int srcLen;                 /* Source string length in bytes, or < 0 for
                                 * strlen(). */
    int flags;                  /* Conversion control flags. */
    Tcl_EncodingState *statePtr;/* Place for conversion routine to store
                                 * state information used during a piecewise
                                 * conversion.  Contents of statePtr are
                                 * initialized and/or reset by conversion
                                 * routine under control of flags argument. */
    char *dst;                  /* Output buffer in which converted string
                                 * is stored. */
    int dstLen;                 /* The maximum length of output buffer in
                                 * bytes. */
    int *srcReadPtr;            /* Filled with the number of bytes from the
                                 * source string that were converted.  This
                                 * may be less than the original source length
                                 * if there was a problem converting some
                                 * source characters. */
    int *dstWrotePtr;           /* Filled with the number of bytes that were
                                 * stored in the output buffer as a result of
                                 * the conversion. */
    int *dstCharsPtr;           /* Filled with the number of characters that
                                 * correspond to the bytes stored in the
                                 * output buffer. */
{
    int result, srcRead, dstWrote, dstChars;
    Tcl_EncodingState state;
    
    if (src == NULL) {
        srcLen = 0;
    } else if (srcLen < 0) {
        srcLen = strlen(src);
    }
    if (statePtr == NULL) {
        flags |= TCL_ENCODING_START | TCL_ENCODING_END;
        statePtr = &state;
    }
    if (srcReadPtr == NULL) {
        srcReadPtr = &srcRead;
    }
    if (dstWrotePtr == NULL) {
        dstWrotePtr = &dstWrote;
    }
    if (dstCharsPtr == NULL) {
        dstCharsPtr = &dstChars;
    }

    result = TCL_OK;
    memcpy (dst, src, dstLen < srcLen ? dstLen : srcLen);
    *dstWrotePtr = *srcReadPtr = *dstCharsPtr = dstLen < srcLen ? dstLen : srcLen;
    dst[*dstWrotePtr] = '\0';
    
    return result;
}

/*
 *-------------------------------------------------------------------------
 *
 * Tcl_ExternalToUtf --
 *
 *      Convert a source buffer from the specified encoding into UTF-8,
 *
 * Results:
 *      The return value is one of TCL_OK, TCL_CONVERT_MULTIBYTE,
 *      TCL_CONVERT_SYNTAX, TCL_CONVERT_UNKNOWN, or TCL_CONVERT_NOSPACE,
 *      as documented in tcl.h.
 *
 * Side effects:
 *      The converted bytes are stored in the output buffer.  
 *
 *-------------------------------------------------------------------------
 */

int
Tcl_ExternalToUtf(interp, encoding, src, srcLen, flags, statePtr, dst,
        dstLen, srcReadPtr, dstWrotePtr, dstCharsPtr)
    Tcl_Interp *interp;         /* Interp for error return, if not NULL. */
    Tcl_Encoding encoding;      /* The encoding for the source string, or
                                 * NULL for the default system encoding. */
    CONST char *src;            /* Source string in specified encoding. */
    int srcLen;                 /* Source string length in bytes, or < 0 for
                                 * encoding-specific string length. */
    int flags;                  /* Conversion control flags. */
    Tcl_EncodingState *statePtr;/* Place for conversion routine to store
                                 * state information used during a piecewise
                                 * conversion.  Contents of statePtr are
                                 * initialized and/or reset by conversion
                                 * routine under control of flags argument. */
    char *dst;                  /* Output buffer in which converted string
                                 * is stored. */
    int dstLen;                 /* The maximum length of output buffer in
                                 * bytes. */
    int *srcReadPtr;            /* Filled with the number of bytes from the
                                 * source string that were converted.  This
                                 * may be less than the original source length
                                 * if there was a problem converting some
                                 * source characters. */
    int *dstWrotePtr;           /* Filled with the number of bytes that were
                                 * stored in the output buffer as a result of
                                 * the conversion. */
    int *dstCharsPtr;           /* Filled with the number of characters that
                                 * correspond to the bytes stored in the
                                 * output buffer. */
{
    int result, srcRead, dstWrote, dstChars;
    Tcl_EncodingState state;
    
    if (src == NULL) {
        srcLen = 0;
    } else if (srcLen < 0) {
        srcLen = strlen(src);
    }
    if (statePtr == NULL) {
        flags |= TCL_ENCODING_START | TCL_ENCODING_END;
        statePtr = &state;
    }
    if (srcReadPtr == NULL) {
        srcReadPtr = &srcRead;
    }
    if (dstWrotePtr == NULL) {
        dstWrotePtr = &dstWrote;
    }
    if (dstCharsPtr == NULL) {
        dstCharsPtr = &dstChars;
    }

    /*
     * If there are any null characters in the middle of the buffer, they will
     * converted to the UTF-8 null character (\xC080).  To get the actual 
     * \0 at the end of the destination buffer, we need to append it manually.
     */

    result = TCL_OK;
    memcpy (dst, src, dstLen < srcLen ? dstLen : srcLen);
    *dstWrotePtr = *srcReadPtr = *dstCharsPtr = dstLen < srcLen ? dstLen : srcLen;
    dst[*dstWrotePtr] = '\0';
    return result;
}

