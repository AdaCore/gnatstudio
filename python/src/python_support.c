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

int ada_pyint_check (PyObject* obj) {
  return PyInt_Check (obj);
}

int ada_pycobject_check (PyObject* obj) {
  return PyCObject_Check (obj);
}

int ada_pytuple_check (PyObject* obj) {
  return PyTuple_Check (obj);
}

int ada_pylist_check (PyObject* obj) {
  return PyList_Check (obj);
}

int ada_pyinstance_check (PyObject* obj) {
  return PyInstance_Check (obj);
}

PyTypeObject* ada_gettypeobject (PyObject* obj) {
  return (PyTypeObject*)(obj->ob_type);
}

int ada_python_api_version () {
  return PYTHON_API_VERSION;
}

PyObject* ada_py_none () {
  return Py_None;
}

PyObject* ada_py_false() {
  return Py_False;
}

PyObject* ada_py_true() {
  return Py_True;
}
