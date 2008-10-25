/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
***************************************************************************/

typedef struct matrix
{
  int rows, columns;
  int **value;
} Matrix;

int matrixGet (const Matrix *matrix, int row, int column);
/* Get the value of the element at row, column */

void matrixSet (Matrix *matrix, int row, int column, int value);
/* Set the value of the element at row, column */

Matrix *matrixAdd (const Matrix *a, const Matrix *b);
Matrix *matrixMul (const Matrix *a, const Matrix *b);
/* Basic matrix operations */

Matrix *matrixAllocId (int rows, int columns);
/* Allocate an Identity matrix of size rows*columns */

Matrix *matrixAlloc(short rows, short columns);
/* Allocate a null matrix of size rows*columns */

Matrix *matrixCopy (const Matrix *matrix);
/* Return a copy of matrix */
