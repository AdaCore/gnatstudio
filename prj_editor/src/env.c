/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                     Copyright (C) 2001-2002                       *
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

#ifdef _WIN32

#ifndef __MSVCRT__
#define __MSVCRT__
#endif

#include <stdlib.h>
#define gnat_envp environ

#else
extern char** gnat_envp;
#endif

char*
get_nth_environment (int index)
{
  if (gnat_envp == (char**) 0)
    return (char*) 0;
  else
    return gnat_envp [index];
}
