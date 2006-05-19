#ifndef __TCL_HASH
#define __TCL_HASH

#ifndef _CLIENTDATA
#   if defined(__STDC__) || defined(__cplusplus)
    typedef void *ClientData;
#   else
    typedef int *ClientData;
#   endif /* __STDC__ */
#define _CLIENTDATA
#endif

#ifndef CONST
# define CONST const
#endif

/*
 * Structure definition for an entry in a hash table.  No-one outside
 * Tcl should access any of these fields directly;  use the macros
 * defined below.
 */

typedef struct Tcl_HashEntry {
    struct Tcl_HashEntry *nextPtr;	/* Pointer to next entry in this
					 * hash bucket, or NULL for end of
					 * chain. */
    struct Tcl_HashTable *tablePtr;	/* Pointer to table containing entry. */
    struct Tcl_HashEntry **bucketPtr;	/* Pointer to bucket that points to
					 * first entry in this entry's chain:
					 * used for deleting the entry. */
    ClientData clientData;		/* Application stores something here
					 * with Tcl_SetHashValue. */
    union {				/* Key has one of these forms: */
	char *oneWordValue;		/* One-word value for key. */
	int words[1];			/* Multiple integer words for key.
					 * The actual size will be as large
					 * as necessary for this table's
					 * keys. */
	char string[4];			/* String for key.  The actual size
					 * will be as large as needed to hold
					 * the key. */
    } key;				/* MUST BE LAST FIELD IN RECORD!! */
} Tcl_HashEntry;

/*
 * Structure definition for a hash table.  Must be in tcl.h so clients
 * can allocate space for these structures, but clients should never
 * access any fields in this structure.
 */

#define TCL_SMALL_HASH_TABLE 4
typedef struct Tcl_HashTable {
    Tcl_HashEntry **buckets;		/* Pointer to bucket array.  Each
					 * element points to first entry in
					 * bucket's hash chain, or NULL. */
    Tcl_HashEntry *staticBuckets[TCL_SMALL_HASH_TABLE];
					/* Bucket array used for small tables
					 * (to avoid mallocs and frees). */
    int numBuckets;			/* Total number of buckets allocated
					 * at **bucketPtr. */
    int numEntries;			/* Total number of entries present
					 * in table. */
    int rebuildSize;			/* Enlarge table when numEntries gets
					 * to be this large. */
    int downShift;			/* Shift count used in hashing
					 * function.  Designed to use high-
					 * order bits of randomized keys. */
    int mask;				/* Mask value used in hashing
					 * function. */
    int keyType;			/* Type of keys used in this table. 
					 * It's either TCL_STRING_KEYS,
					 * TCL_ONE_WORD_KEYS, or an integer
					 * giving the number of ints that
                                         * is the size of the key.
					 */
    Tcl_HashEntry *(*findProc) (struct Tcl_HashTable *tablePtr,
	    CONST char *key);
    Tcl_HashEntry *(*createProc) (struct Tcl_HashTable *tablePtr,
	    CONST char *key, int *newPtr);
} Tcl_HashTable;

/*
 * Structure definition for information used to keep track of searches
 * through hash tables:
 */

typedef struct Tcl_HashSearch {
    Tcl_HashTable *tablePtr;		/* Table being searched. */
    int nextIndex;			/* Index of next bucket to be
					 * enumerated after present one. */
    Tcl_HashEntry *nextEntryPtr;	/* Next entry to be enumerated in the
					 * the current bucket. */
} Tcl_HashSearch;

/*
 * Acceptable key types for hash tables:
 */

#define TCL_STRING_KEYS		0
#define TCL_ONE_WORD_KEYS	1

/*
 * Macros for clients to use to access fields of hash entries:
 */

#define Tcl_GetHashValue(h) ((h)->clientData)
#define Tcl_SetHashValue(h, value) ((h)->clientData = (ClientData) (value))
#define Tcl_GetHashKey(tablePtr, h) \
    ((char *) (((tablePtr)->keyType == TCL_ONE_WORD_KEYS) ? (h)->key.oneWordValue \
						: (h)->key.string))

/*
 * Macros to use for clients to use to invoke find and create procedures
 * for hash tables:
 */

#define Tcl_FindHashEntry(tablePtr, key) \
	(*((tablePtr)->findProc))(tablePtr, key)
#define Tcl_CreateHashEntry(tablePtr, key, newPtr) \
	(*((tablePtr)->createProc))(tablePtr, key, newPtr)

extern Tcl_HashEntry *	Tcl_FirstHashEntry (Tcl_HashTable *tablePtr,
					    Tcl_HashSearch *searchPtr);
extern Tcl_HashEntry *	Tcl_NextHashEntry (Tcl_HashSearch *searchPtr);

#endif
