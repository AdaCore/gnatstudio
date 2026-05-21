// write "hello" to "output.txt"
#include <stdio.h>
int
main ()
{
  FILE *file = fopen ("output.txt", "w");
  if (file == NULL)
    {
      return 1; // Error opening file
    }
  fprintf (file, "ERROR: the fake gnatlist program was executed\n");
  fclose (file);
  return 0;
}