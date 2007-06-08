#include <gtk/gtk.h>

#ifdef PYGTK
#include <pygobject.h>
#include <pygtk/pygtk.h>
#else
typedef void* PyObject;
#endif

/******************************************************
 Support for pygtk
*******************************************************/

#ifdef PYGTK
int
ada_build_with_pygtk()
{
   return 1;
}

GObject*
ada_widget_from_pyobject (PyObject* object)
{
   return g_object_ref (pygobject_get (object));
}

PyObject*
ada_pyobject_from_widget (GObject* object)
{
   return pygobject_new (object);
}

void
ada_init_pygtk (void)
{
  init_pygtk();
  init_pygobject();
}

/******************************************************
 No support for pygtk
 ******************************************************/

#else  /* PYGTK */

int
ada_build_with_pygtk()
{
   return 0;
}

GObject*
ada_widget_from_pyobject (PyObject* object)
{
   return NULL;
}

PyObject*
ada_pyobject_from_widget (GObject* object)
{
   return NULL;
}

void
ada_init_pygtk (void)
{
}

#endif

