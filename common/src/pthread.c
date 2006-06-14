/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                      Copyright (C) 2002-2004                      *
 *                            ACT-Europe                             *
 *                                                                   *
 * GPS is free  software;  you can redistribute it and/or modify  it *
 * under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is  distributed in the hope that it will be  useful, *
 * but  WITHOUT ANY WARRANTY;  without even the  implied warranty of *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *
 * General Public License for more details. You should have received *
 * a copy of the GNU General Public License along with this program; *
 * if not,  write to the  Free Software Foundation, Inc.,  59 Temple *
 * Place - Suite 330, Boston, MA 02111-1307, USA.                    *
 *********************************************************************/

/* Provide dummy symbols needed by gcc on some platforms */

#ifdef hpux

/* These symbols are expected by libgcc under HP-UX when GCC is
   configure with --enable-threads */

int
pthread_once ()
{
  return -1;
}

int
pthread_mutex_lock ()
{
  return 0;
}

int
pthread_mutex_unlock ()
{
  return 0;
}

int
pthread_create ()
{
  return 0;
}

#endif
