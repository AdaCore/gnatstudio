#include "matrix.h"
#include <stdlib.h>

char * matrixPrint (Matrix *m)
{
  int row = m->rows;
  int column = m->columns;
  char * result;

  result = (char *) malloc (1024 * sizeof (char));

  if (m == NULL)
    {
      printf("NULL\n");
    }
  else
    {
      while (--row >= 0)
        {
          column = m->columns;
          while (--column >= 0)
            {
              sprintf (result, "%s%d   ", result,
                       matrixGet (m, m->rows - row - 1, m->columns - column - 1));
            }

          sprintf(result, "%s\n", result);
        }
    }

  return result;
}
