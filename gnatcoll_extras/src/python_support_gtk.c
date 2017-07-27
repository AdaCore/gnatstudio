/*----------------------------------------------------------------------------
--                                  G N A T C O L L                         --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

#include <gtk/gtk.h>
#include <pygobject.h>

/******************************************************
 Support for pygtk and pygobject
*******************************************************/

int ada_build_with_pygtk() {
   return 1;
}

GObject* ada_widget_from_pyobject (PyObject* object) {
   return g_object_ref (pygobject_get (object));
}

PyObject* ada_pyobject_from_widget (GObject* object) {
   return pygobject_new (object);
}

GdkWindow* ada_window_from_pyobject (PyObject* object) {
   GObject* obj = ada_widget_from_pyobject (object);
   if (obj && GDK_IS_WINDOW(obj)) {
      return (GdkWindow*)obj;
   } else {
      return (GdkWindow*)NULL;
   }
}

char* ada_load_pygtk() {
#ifdef PYGTK
   return "import pygtk; pygtk.require('2.0'); import gtk";
#else
   return "import gi, sys; gi.require_version('Gtk', '3.0'); from gi.repository import Gtk,GObject; sys.modules['gtk']=Gtk; sys.modules['gobject']=GObject";
#endif
}

void ada_init_pygtk (void) {
#ifdef PYGTK
  init_pygtk();
#endif
  pygobject_init(-1, -1, -1);
}
