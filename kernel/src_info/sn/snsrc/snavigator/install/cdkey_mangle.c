#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



static char* argv0 = "";


void
exit_usage ()
{
  fprintf (stderr,
	   "Usage: %s [KEY]\n"
	   "  copies stdin to stdout\n"
	   "  if no KEY supplied, mangles.\n"
	   "  if KEY supplied, validates KEY and demangles.\n"
	   "  exits non-zero if KEY is invalid.\n",
	   argv0);
  exit (1);
}


static char*
cdkey_validate (char* key)
{
  char* endp;
  unsigned int a, b, c;

  if (strlen (key) != 10) {
    return "must be 10 digits long";
  }

  if (strspn (key, "0123456789") != 10) {
    return "must contain only digits";
  }

  if (sscanf (key, "%4d%3d%3d", &a, &b, &c) != 3) {
    return "parse error";
  }

  if (a == 0
      || b == 0
      || c == 0
      || (a % 56) != 0
      || (b % 12) != 0
      || (c % 3) != 0
  ) {
    return "wrong number";
  }
  return 0;
}


static char magic[] = "cdkey_mangle_1\n";

static char*
cdkey_mangle (FILE* in_fp, FILE* out_fp, int mangling)
{

  char in_buf[1024];
  char out_buf[1024];
  int p;
  size_t n;

  /* leading magic. */

  if (mangling) {
    fputs (magic, out_fp);

  } else {
    assert (sizeof (magic) < sizeof (in_buf));
    in_buf [sizeof (in_buf) - 1] = '\0';
    if (fgets (in_buf, sizeof (in_buf) - 1, in_fp) == NULL) {
      /* special case, null input is ok. */
      return 0;
    } else if (strcmp (in_buf, magic) != 0) {
      return "bad version header";
    }
  }

  /* do it. */

  p = 0;
  while (0 < (n = fread (in_buf, 1, sizeof in_buf, in_fp))) {
    int k;
    for (k = 0; k < n; ++k) {
      out_buf[k] = in_buf[k] ^ magic[p];
      p = (p + 1) % sizeof (magic);
    }
    if (n != fwrite (out_buf, 1, n, out_fp)) {
      return "write failed";
    }
  }

  return 0;
}


int
main (int argc, char** argv)
{
  char* error;
  int mangling = 0;

  argv0 = argv[0];

  if (argc == 1) {
    mangling = 1;
  } else if (argc == 2 && argv[1][0] != '-') {
    mangling = 0;
  } else {
    exit_usage ();
  }

  if (! mangling) {
    error = cdkey_validate (argv[1]);
    if (error) {
      fprintf (stderr, "\"%s\" is an invalid key: %s\n", argv[1], error);
      exit (3);
    }
  }
  error = cdkey_mangle (stdin, stdout, mangling);
  if (error) {
    fprintf (stderr, "%s failed: %s\n",
	     (mangling ? "encrypt" : "decrypt"), error);
    exit (4);
  }

  return 0;
}


