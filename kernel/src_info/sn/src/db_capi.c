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
  int dbc;                    /* number of database files - 1 */
  DB **db;                    /* database handlers */
  char **fname;    		/* file names (copies) */
  int last_errno;		/* 0 or last error number */
  char *key_p;		/* key pattern */
  int exact_match;		/* 1/0, if exact key match needed/not needed */
  int pos;			/* starting cursor position */
  int saved_pos;		/* saved starting cursor position */
  int dbi;			/* index of current database handler */
} DB_File;

#define CSF_MAX_FIELDS 15	/* maximum of fields in csf */

/* C presentation of Ada type CSF,
 * CSF stands for "Character Separated Fields".
 * Fields are separated by DB_FLDSEP_CHR character, defined in sn.h
*/
typedef struct CSF_struct {
  int num_of_fields;
  char *fields[CSF_MAX_FIELDS + 1];	/* num_of_fields pointers to char;
					 * +1 to store pointer after the last '\0'
  * to unify field length calculation */
} CSF;

typedef struct DB_Pair_struct {
  char* key;
  char* data;
  int dbi;
} DB_Pair;


/* Constructs CSF from given string, makes a copy of given string */
void csf_init (char* v, CSF* csf) {
  char *p;
  int cf;

  if (v == 0)
    return;

  cf = 0;
  csf->fields[cf++] = v;
  for (p = v; *p; p++)
    if (*p == DB_FLDSEP_CHR) {
      if (cf < CSF_MAX_FIELDS)
	csf->fields[cf++] = p + 1;
    }
  csf->fields[cf] = p + 1; /* note: this last pointer is not legal:
  * it is used only for field length calculation */

  csf->num_of_fields = cf; /* see note above */
}

/* Returns number of fields. */
int csf_get_field_count(CSF * csf)
{
  if (csf == 0)
    return 0;
  return csf->num_of_fields;
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
  return csf->fields[index] - csf->fields[index - 1] - 1;
}

DB_File *ada_db_open(const int num_of_files, const char **file_names)
{
  DB_File *file;
  int i,j = 0;

  file = (DB_File *) malloc(sizeof(DB_File));
  file->fname = (char **) malloc(sizeof(char *) * num_of_files);
  file->db = (DB **) malloc(sizeof(DB *) * num_of_files);

  file->dbi = -1;
  file->pos = POS_FIRST;

  for (i = 0; i < num_of_files; i++) {
    file->db[i] = dbopen(file_names[i], O_RDONLY, 0644, DB_BTREE, 0);
    if (file->db[i] == 0) {
      file->last_errno = errno;
      file->fname[i] = 0;
    } else  {
      file->fname[i] = strdup(file_names[i]);
      j++;
    }
  }

  if (j == 0) { /* no open files at all */
    return file;
  }

  file->last_errno = 0; /* reset error flag if there is
  * at least one open file */
  file->dbc = num_of_files - 1;
  file->key_p = 0;

  return file;
}

DB_File *ada_db_dup(const DB_File * file)
{
  DB_File *new_file;
  new_file = ada_db_open (file->dbc + 1, (const char **) file->fname);
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
  int i;

  file->last_errno = 0;
  if (file->key_p) {
    free(file->key_p);
  }

  for (i = file->dbc; i >= 0; i--) {
    if (file->fname[i]) {
      free(file->fname[i]);
    }
    if (file->db[i]) {
      if (file->db[i]->close(file->db[i]) != 0)
	file->last_errno = errno;
    }
  }

  free (file->fname);
  free (file->db);
}

void ada_db_set_cursor(DB_File * file, int pos, char *key_p,
		       int exact_match)
{
  file->last_errno = 0;
#ifdef RESTRICTED_CURSOR
  if (file->dbi >= 0) {
    file->last_errno = EINPROGRESS;
  }
#endif
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
  file->dbi = 0;
}

void ada_db_free_cursor(DB_File * file)
{
  if (file->key_p) {
    free(file->key_p);
    file->key_p = 0;
  }
  file->dbi = -1;
}

void ada_db_get_pair (DB_File * file, int move, DB_Pair * output) {
  DBT key, data;
  int len, matched;
  int flag;
  int result;
  int next_file;

  file->last_errno = 0;

  if (move == MOVE_BY_KEY
      && !(file->pos == POS_BY_KEY || file->pos == MOVE_BY_KEY)) {
      /* key matching requested, but key pattern was not set */
      output->dbi = -1;
      return;
    }

  switch (file->pos) {
    case POS_FIRST:
      flag = R_FIRST;
      file->saved_pos = R_FIRST;
      break;
    case POS_LAST:
      flag = R_LAST;
      file->saved_pos = R_LAST;
      break;
    case POS_BY_KEY:
      if (!file->key_p) {
	file->last_errno = EINPROGRESS;
	output->dbi = -1;
	return;
      }
      flag = R_CURSOR;
      file->saved_pos = R_CURSOR;
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
      file->saved_pos = R_FIRST;
  }

  file->pos = move;

  next_file = 0;
  do {
    if (file->db[file->dbi] == 0) {
      result = 1;
      if (file->dbi < file->dbc) {
	file->dbi++;
	next_file = 1;
      } else {
	next_file = 0;
      }
      continue;
    }
    if (next_file) {
      if (file->saved_pos == R_CURSOR) {
	key.data = file->key_p;
	key.size = strlen(file->key_p) + 1;
      }
      result = file->db[file->dbi]->seq(file->db[file->dbi], &key, &data, file->saved_pos);
    } else {
      result = file->db[file->dbi]->seq(file->db[file->dbi], &key, &data, flag);
    }
    next_file = 0;
    if (result == 1 || result == -1) {
      if (file->dbi < file->dbc) {
	file->dbi++;
	next_file = 1;
	continue;
      }
    } else if (move == MOVE_BY_KEY) {
      /* check if retrieved key is matched */
      len = strlen(file->key_p);
      if (strncmp(key.data, file->key_p, len) != 0) {
	/* not matched */
	result = 1;
	if (file->dbi < file->dbc) {
	  file->dbi++;
	  next_file = 1;
	  continue;
	}
      }
      if (file->exact_match != 0 && (key.size - 1) != len) {
	/* not matched */
	result = 1;
	if (file->dbi < file->dbc) {
	  file->dbi++;
	  next_file = 1;
	  continue;
	}
      }
      /* mached */
    }

  } while (next_file);

  if (result == 1) {		/* no more key/data pairs */
    output->dbi = -1;
    return;
  } else if (result == -1) {	/* error, errno is set    */
    file->last_errno = errno;
    output->dbi = -1;
    return;
  }

  /* here result == 0 */

  output->key  = key.data;
  output->data = data.data;

  output->dbi = file->dbi;

  if (output->key == 0 || output->data == 0) {
    output->dbi = -1;
  }
}
