#include <string.h>
#include <stdlib.h>
#include "language_custom.h"

char *
gpr_comment_line (char *line, char comment, int length)
{
  char *s;

  if (comment)
    {
      s = malloc (length + 4 + 1);

      strncpy (s, "--  ", 4);
      strncpy (s + 4, line, length);
      s [length + 4] = '\0';
      return s;
    }
  else
    {
      if (length > 4 && !strncmp (line, "--  ", 4))
        {
          s = malloc (length - 3);
          strncpy (s, line + 4, length - 4);
          s [length - 4] = '\0';
          return s;
        }
      else
        {
          s = malloc (length + 1);
          strncpy (s, line, length);
          s [length] = '\0';
          return s;
        }
    }
}
