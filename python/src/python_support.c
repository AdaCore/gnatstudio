#include <Python.h>

void ada_py_incref (PyObject* obj) {
  Py_INCREF (obj);
}

void ada_py_decref (PyObject* obj) {
  Py_DECREF (obj);
}

void ada_py_xincref (PyObject* obj) {
  Py_XINCREF (obj);
}

void ada_py_xdecref (PyObject* obj) {
  Py_XDECREF (obj);
}

int ada_pystring_check (PyObject* obj) {
  return PyString_Check (obj);
}

PyTypeObject* ada_gettypeobject (PyObject* obj) {
  return (PyTypeObject*)(obj->ob_type);
}

int ada_python_api_version = PYTHON_API_VERSION;

