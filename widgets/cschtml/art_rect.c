/* Libart_LGPL - library of basic graphic primitives
 * Copyright (C) 1998 Raph Levien
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>
#include "art_rect.h"

#ifndef MAX
#define MAX(a, b)  (((a) > (b)) ? (a) : (b))
#endif /* MAX */

#ifndef MIN
#define MIN(a, b)  (((a) < (b)) ? (a) : (b))
#endif /* MIN */

/**
 * art_irect_intersection: Find intersection of two integer rectangles.
 * @dest: Where the result is stored.
 * @src1: A source rectangle.
 * @src2: Another source rectangle.
 *
 * Finds the intersection of @src1 and @src2.
 **/
void
art_irect_intersect (ArtIRect *dest, const ArtIRect *src1, const ArtIRect *src2) {
  dest->x0 = MAX (src1->x0, src2->x0);
  dest->y0 = MAX (src1->y0, src2->y0);
  dest->x1 = MIN (src1->x1, src2->x1);
  dest->y1 = MIN (src1->y1, src2->y1);
}

/**
 * art_irect_empty: Determine whether integer rectangle is empty.
 * @src: The source rectangle.
 *
 * Return value: TRUE if @src is an empty rectangle, FALSE otherwise.
 **/
int
art_irect_empty (const ArtIRect *src) {
  return (src->x1 <= src->x0 || src->y1 <= src->y0);
} 

