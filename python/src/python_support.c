/*-------------------------------------------------------------------
                               G P S                               --
                                                                   --
                     Copyright (C) 2003-2006                       --
                            AdaCore                                --
                                                                   --
 GPS is free  software; you can  redistribute it and/or modify  it --
 under the terms of the GNU General Public License as published by --
 the Free Software Foundation; either version 2 of the License, or --
 (at your option) any later version.                               --
                                                                   --
 This program is  distributed in the hope that it will be  useful, --
 but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
 General Public License for more details. You should have received --
 a copy of the GNU General Public License along with this library; --
 if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
 Place - Suite 330, Boston, MA 02111-1307, USA.                    --
---------------------------------------------------------------------*/

#include <Python.h>
#include <gtk/gtk.h>

#ifdef PYGTK
#include <pygobject.h>
#include <pygtk/pygtk.h>
#endif

#undef DEBUG
/* #define DEBUG */

PyObject *
ada_Py_InitModule4
  (char *name, PyMethodDef *methods,
   char *doc, PyObject *self,
   int apiver)
{
  return Py_InitModule4 (name, methods, doc, self, apiver);
}

PyObject *
ada_pycfunction_newex (PyMethodDef *ml, PyObject *self, PyObject *module)
{
  PyObject *method = PyCFunction_New (ml, self);

#if (PY_MAJOR_VERSION > 2 \
     || (PY_MAJOR_VERSION == 2 \
	 && (PY_MINOR_VERSION > 3 \
	     || (PY_MINOR_VERSION == 3 \
		 && PY_MICRO_VERSION >= 3))))
  ((PyCFunctionObject*)method)->m_module = module;
  Py_XINCREF (module);
#endif
  return method;
}

int
ada_pyget_refcount (PyObject* obj)
{
   return obj->ob_refcnt;
}

char*
ada_py_refcount_msg (PyObject* obj)
{
   static char msg[200];
   if (obj) {
      snprintf (msg, 199, "%p (%s, rc=%d)",
                obj, obj->ob_type->tp_name, obj->ob_refcnt);
   } else {
      msg[0] = '\0';
   }
   return msg;
}

void
ada_py_print_refcount (PyObject* obj, char* msg)
{
  if (obj)
    printf ("DEBUG %s %s\n", msg, ada_py_refcount_msg (obj));
}

void
ada_py_incref (PyObject* obj)
{
  Py_INCREF (obj);
#ifdef DEBUG
  ada_py_print_refcount (obj, "after incref");
#endif
}

void
ada_py_decref (PyObject* obj)
{
#ifdef DEBUG
  ada_py_print_refcount (obj, "before decref");
#endif
  Py_DECREF (obj);
}

void
ada_py_xincref (PyObject* obj)
{
  Py_XINCREF (obj);
#ifdef DEBUG
  ada_py_print_refcount (obj, "after xincref");
#endif
}

void
ada_py_xdecref (PyObject* obj)
{
#ifdef DEBUG
  ada_py_print_refcount (obj, "before xdecref");
#endif
  Py_XDECREF (obj);
}

int
ada_pystring_check (PyObject* obj)
{
  return PyString_Check (obj);
}

int
ada_pyint_check (PyObject* obj)
{
  return PyInt_Check (obj);
}

int
ada_pyfunction_check (PyObject* obj)
{
  return PyFunction_Check (obj);
}

PyObject*
ada_pyfunction_get_globals (PyObject* obj)
{
  return PyFunction_GET_GLOBALS (obj);
}

PyObject*
ada_pyfunction_get_code (PyObject* obj)
{
  return PyFunction_GET_CODE (obj);
}

PyObject*
ada_pyfunction_get_closure (PyObject* obj)
{
  return PyFunction_GET_CLOSURE (obj);
}

PyObject*
ada_pyfunction_get_defaults (PyObject* obj)
{
  return PyFunction_GET_DEFAULTS (obj);
}

PyObject* ada_PyEval_EvalCodeEx
  (PyCodeObject *co,
   PyObject *globals,
   PyObject *locals,
   PyObject *args,
   PyObject *kwds,
   PyObject *defs,
   PyObject *closure)
{
   /* Code copied from funcobject.c::function_call() */

  PyObject **k, **d;
  PyObject* result;
  int nk, nd;

  if (defs != NULL && PyTuple_Check(defs)) {
     d = &PyTuple_GET_ITEM((PyTupleObject *)defs, 0);
     nd = PyTuple_Size(defs);
  } else {
     d = NULL;
     nd = 0;
  }

  if (kwds != NULL && PyDict_Check(kwds)) {
     int pos, i;
     nk = PyDict_Size(kwds);
     k  = PyMem_NEW(PyObject *, 2*nk);
     if (k == NULL) {
        PyErr_NoMemory();
        return NULL;
     }
     pos = i = 0;
     while (PyDict_Next(kwds, &pos, &k[i], &k[i+1]))
        i += 2;
      nk = i/2;
      /* XXX This is broken if the caller deletes dict items! */
  } else {
     k = NULL;
     nk = 0;
  }

  result = (PyObject*) PyEval_EvalCodeEx
    (co, globals, locals,
    &PyTuple_GET_ITEM (args, 0), PyTuple_Size (args), k, nk, d, nd, closure);

  if (k != NULL) {
    PyMem_DEL (k);
  }

  return result;
}

/*
PyObject*
ada_py_object_new (PyObject* base)
{
   if (PyClass_Check (base)) {
      return PyInstance_NewRaw (base, NULL);
   } else {
      if (base->ob_type->tp_new != NULL) {
           Py_INCREF (Py_None);
           Py_INCREF (Py_None);
           return base->ob_type->tp_new (base->ob_type, Py_None, Py_None);
      } else {
           return NULL;
      }
   }
}
*/

int
ada_pycobject_check (PyObject* obj)
{
  return PyCObject_Check (obj);
}

int
ada_pytuple_check (PyObject* obj)
{
  return PyTuple_Check (obj);
}

int
ada_pylist_check (PyObject* obj)
{
  return PyList_Check (obj);
}

int
ada_pyinstance_check (PyObject* obj)
{
  return PyInstance_Check (obj);
}

int
ada_pymethod_check (PyObject* obj)
{
  return PyMethod_Check (obj);
}

PyTypeObject*
ada_gettypeobject (PyObject* obj)
{
  return (PyTypeObject*)(obj->ob_type);
}

int
ada_python_api_version ()
{
  return PYTHON_API_VERSION;
}

PyObject* ada_py_none ()
{
  return Py_None;
}

PyObject* ada_py_false()
{
  return Py_False;
}

PyObject*
ada_py_true()
{
  return Py_True;
}

PyObject*
ada_pyclass_name(PyObject* obj)
{
  if (PyClass_Check (obj)) {
      return ((PyClassObject*)obj)->cl_name;
  } else {
     /* Derives from object, not a real class */
     return PyObject_GetAttrString (obj, "__name__");
  }
}

int
ada_pyclass_is_subclass (PyObject* class, PyObject* base)
{
  if (PyClass_Check (class)) {
      return PyClass_IsSubclass (class, base);
  } else {
      return PyObject_TypeCheck (class, base->ob_type);
  }
}

PyObject *
ada_py_object_callmethod (PyObject *o, char *m)
{
  return PyObject_CallMethod (o, m, "");
}

PyObject *
ada_py_object_callmethod_obj (PyObject *o, char *m, PyObject *arg)
{
  return PyObject_CallMethod (o, m, "(O)", arg);
}

PyObject *
ada_py_object_callmethod_int (PyObject *o, char *m, int arg)
{
  PyObject_CallMethod (o, m, "(i)", arg);
}

int
ada_py_arg_parsetuple_ptr (PyObject *o, char *fmt, void *arg1)
{
  PyArg_ParseTuple (o, fmt, arg1);
}

int
ada_py_arg_parsetuple_ptr2 (PyObject *o, char *fmt, void *arg1, void *arg2)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2);
}

int
ada_py_arg_parsetuple_ptr3
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2, arg3);
}

int
ada_py_arg_parsetuple_ptr4
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3, void *arg4)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4);
}

int
ada_py_arg_parsetuple_ptr5
  (PyObject *o, char *fmt,
   void *arg1, void * arg2, void *arg3, void *arg4, void *arg5)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4, arg5);
}

/******************************************************
 Support for pygtk
 The code below is duplicated from pygobject.h. However,
 since we do not want to depend on the latter when compiling
 GPS, we have simply copied it here. This means that we
 might not be compatible with pygtk for ever. This was
 tested with pygtk 2.4 and 2.6
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
   /* return ((PyGObject*)object)->obj; */
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

