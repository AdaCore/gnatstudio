/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                      Copyright (C) 2002-2005                      *
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

/******************************************
 ** Mouse handling                       **
 ******************************************/

#ifdef _WIN32

#include <windows.h>
#include <gdk/gdkwin32.h>
void
ada_gdk_move_pointer (gint x, gint y)
{
  SetCursorPos (x, y);
}

#else
#include <gdk/gdkx.h>

void
ada_gdk_move_pointer (gint x, gint y)
{
  GdkDisplay *display = gdk_display_get_default ();
  Display *xdisplay = GDK_DISPLAY_XDISPLAY (display);
  Window xroot_window = GDK_WINDOW_XID (gdk_get_default_root_window ());

  XWarpPointer (xdisplay, None, xroot_window, 0, 0, 0, 0, x, y);
}

#endif
