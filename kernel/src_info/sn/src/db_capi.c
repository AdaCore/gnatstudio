#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <config.h>
#include <db.h>
#include <sn.h>

/* RESTRICTED_CURSOR is used to check if all cursors were released after use */
/*
#define RESTRICTED_CURSOR
*/

/* EINPROGRESS is not defined on all systems, e.g Windows */
#ifndef EINPROGRESS
#define EINPROGRESS EINVAL
#endif

/* Cursor positions */
#define POS_FIRST       1	/* first key/data pair */
#define POS_LAST        2	/* last key/data pair  */
#define POS_BY_KEY      3	/* first key/data pair matched to key pattern
				 * (see db_api.ads, Set_Cursor) */

#define MOVE_PREV   4		/* move to the previous  pair */
#define MOVE_NEXT   5		/* move to the next pair      */
#define MOVE_BY_KEY 6		/* move to the next key/data pair matched to
				   key pattern */

/* C presentation of Ada type DB_File */
typedef struct DB_File_struct {
    DB *db;			/* database handler */
    char *fname;                /* file name (a copy) */
    int last_errno;		/* 0 or last error number */
    char *key_p;		/* key pattern */
    int exact_match;		/* 1/0, if exact key match needed/not needed */
    int pos;			/* starting cursor position */
} DB_File;


/* C presentation of Ada type CSF,
 * CSF stands for "Character Separated Fields".
 * Fields are separated by DB_FLDSEP_CHR character, defined in sn.h
 */
typedef struct CSF_struct {
    int num_of_fields;
    char *value;		/* 0-terminated string with all fields
				 * separated by 0 */

    char **fields;		/* num_of_fields pointers to char */
} CSF;

typedef struct DB_Pair_struct {
    CSF *key;
    CSF *data;
} DB_Pair;

/* Release CSF from memory */
void csf_free(CSF * csf)
{
    if (csf == 0)
	return;
    free(csf->value);
    if (csf->fields) {		/* if field pointers exist, free them too */
	free(csf->fields);
    }
    free(csf);
}

/* Constructs CSF from given string, makes a copy of given string */
CSF *csf_init(char *v)
{
    CSF *csf;
    char **fields;
    char *p, *q;
    int nof, cf;

    if (v == 0)
	return 0;

    csf = (CSF *) malloc(sizeof(CSF));
    if (csf == 0)
	return 0;

    nof = 0;
    for (p = v; *p; p++)
	if (*p == DB_FLDSEP_CHR)
	    nof++;

    csf->value = (char *) malloc(p - v + 1);
    fields = (char **) malloc(sizeof(char *) * (nof + 1));

    cf = 0;
    fields[cf++] = q = csf->value;
    for (p = v; *p; p++, q++)
	if (*p == DB_FLDSEP_CHR) {
	    *q = 0;
	    if (cf <= nof)
		fields[cf++] = q + 1;
	} else {
	    *q = *p;
	}
    *q = 0;

    csf->num_of_fields = nof + 1;
    csf->fields = fields;

    return csf;
}

/* Returns number of fields. */
int csf_get_field_count(CSF * csf)
{
    if (csf == 0)
	return 0;
    return csf->num_of_fields;
}

int csf_get_total_length(CSF *csf)
{
    int i, n=0;
    if (csf == 0)
	return 0;
    for (i=0; i<csf->num_of_fields; i++)
    {
       n += strlen(csf->fields[i]);
    }
    return n;
}

/* Return string at index, index is 1 .. get_field_count.
 * Returns NULL if index is out of range */
char *csf_get_field(CSF * csf, int index)
{
    if (csf == 0)
	return 0;
    if (csf->num_of_fields < index || index < 1)
	return 0;
    return (csf->fields[index - 1]);
}

int csf_get_field_length(CSF * csf, int index)
{
    if (csf == 0)
	return 0;
    if (csf->num_of_fields < index || index < 1)
	return 0;
    return (strlen(csf->fields[index - 1]));
}

DB_File *ada_db_open(const char *file_name)
{
    DB_File *file;
    file = (DB_File *) malloc(sizeof(DB_File));
    if (file == 0) {
	return 0;
    }
    file->db = dbopen(file_name, O_RDONLY, 0644, DB_BTREE, 0);
    file->last_errno = 0;
    file->key_p = 0;
    file->fname = strdup(file_name);
    if (file->db == 0) {
	file->last_errno = errno;
    }
    return file;
}

DB_File *ada_db_dup(const DB_File * file)
{
    DB_File *new_file;
    new_file = ada_db_open (file->fname);
    return new_file;
}

int ada_get_last_errno(const DB_File * file)
{
    return file->last_errno;
}

char *ada_get_errstr(const DB_File * file)
{
    return strerror(file->last_errno);
}

void ada_db_close(DB_File * file)
{
    file->last_errno = 0;
    if (file->key_p) {
	free(file->key_p);
    }
    if (file->fname) {
        free(file->fname);
    }
    if (file->db) {
	if (file->db->close(file->db) != 0)
	    file->last_errno = errno;
    }
}

void ada_db_set_cursor(DB_File * file, int pos, char *key_p,
		       int exact_match)
{
    file->last_errno = 0;
    if (pos == POS_BY_KEY) {
        if (file->key_p) { /* key pattern was already set, free it before */
#ifdef RESTRICTED_CURSOR
            file->last_errno = EINPROGRESS;
#endif
            free(file->key_p);
    }
        file->key_p = strdup(key_p);
        file->exact_match = exact_match;
    }
    file->pos = pos;
}

void ada_db_free_cursor(DB_File * file)
{
    if (file->key_p) {
        free(file->key_p);
        file->key_p = 0;
    }
}

DB_Pair *ada_db_get_pair(DB_File * file, int move)
{

    DB_Pair *pair;
    DBT key, data;
    int len, matched;
    int flag;
    int result;

    file->last_errno = 0;

    if (move == MOVE_BY_KEY
	&& !(file->pos == POS_BY_KEY || file->pos == MOVE_BY_KEY)) {
	/* key matching requested, but key pattern was not set */
	return 0;
    }

    switch (file->pos) {
    case POS_FIRST:
	flag = R_FIRST;
	break;
    case POS_LAST:
	flag = R_LAST;
	break;
    case POS_BY_KEY:
        if (!file->key_p) {
	    file->last_errno = EINPROGRESS;
	    return 0;
        }
	flag = R_CURSOR;
	key.data = file->key_p;
	key.size = strlen(file->key_p) + 1;
	break;
    case MOVE_PREV:
	flag = R_PREV;
	break;
    case MOVE_NEXT:
	flag = R_NEXT;
	break;
    case MOVE_BY_KEY:
	flag = R_NEXT;
	break;
    default:
	flag = R_FIRST;
    }

    file->pos = move;
    result = file->db->seq(file->db, &key, &data, flag);
    if (result == 1) {		/* no more key/data pairs */
	return 0;
    } else if (result == -1) {	/* error, errno is set    */
	file->last_errno = errno;
	return 0;
    }

    /* here result == 0 */

    if (move == MOVE_BY_KEY) {
        if (!file->key_p) {
	    file->last_errno = EINPROGRESS;
	    return 0;
        }
	/* check if retrieved key is matched */
	len = strlen(file->key_p);
	if (strncmp(key.data, file->key_p, len) != 0) {
	    /* not matched */
	    return 0;
	}
	if (file->exact_match != 0 && (key.size - 1) != len) {
	    /* not matched */
	    return 0;
	}

	/* matched */
    }

    pair = (DB_Pair *) malloc(sizeof(DB_Pair));
    if (pair == 0) {
	file->last_errno = errno;
	return 0;
    }
    pair->key = csf_init((char *) key.data);
    pair->data = csf_init((char *) data.data);

    if (pair->key == 0 || pair->data == 0) {
	csf_free(pair->key);
	csf_free(pair->data);
	free(pair);
	return 0;
    }

    return pair;
}

CSF *ada_get_key(DB_Pair * pair)
{
    return pair->key;
}

CSF *ada_get_data(DB_Pair * pair)
{
    return pair->data;
}
