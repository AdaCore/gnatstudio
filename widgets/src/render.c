/*----------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

#ifdef _WIN32
#elif defined (__APPLE__)
#include <gdk/gdk.h>
int
gps_have_render (GdkDrawable *drawable)
{
  return 1;
}
#else

#include <gdk/gdk.h>
#include <gdk/gdkx.h>
#include <cairo-xlib.h>

int
gps_have_render (GdkDrawable *drawable)
{
  int event_base, error_base;

  if (drawable != NULL)
    return XRenderQueryExtension
      (GDK_DISPLAY_XDISPLAY (gdk_drawable_get_display (drawable)),
       &event_base, &error_base);
  else
    return 1;
}

#endif
