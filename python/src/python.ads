-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Standard interface to the python interpreter.
--  This requires at least python 2.0 to be installed on your system.

with System;
with Interfaces.C.Strings;

package Python is

   procedure Py_Initialize;
   --  Initialize the python interpreter. You must call Py_SetProgramName first

   procedure Py_SetProgramName (Name : String);
   --  Set the program name. This must be called before Py_Initialize

   -------------
   -- Objects --
   -------------

   type Dummy is limited private;
   type PyObject is access Dummy;
   pragma Convention (C, PyObject);

   type PyObject_Array is array (Natural range <>) of PyObject;

   function Py_None return PyObject;
   --  Return the python's variable Py_None, which should be returned by
   --  procedures. Generally, one need to call Py_INCREF before returning this
   --  value.

   type PyCodeObject is new PyObject;

   procedure Py_INCREF (Obj : PyObject);
   procedure Py_DECREF (Obj : PyObject);
   --  Increment or decrement the reference count for Obj. Obj mustn't be null.

   procedure Py_XINCREF (Obj : PyObject);
   procedure Py_XDECREF (Obj : PyObject);
   --  Same as above, but Obj can be null.

   function PyObject_Str (Obj : PyObject) return PyObject;
   --  Compute the string representation of Obj.  Returns the string
   --  representation on success, NULL on failure.  This is the equivalent of
   --  the Python expression "str(obj)".

   function PyObject_Repr (Obj : PyObject) return PyObject;
   --  Similar to PyObject_Str.
   --  ???

   function PyObject_CallMethod
     (Object : PyObject; Name : String) return PyObject;
   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : PyObject) return PyObject;
   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : Integer) return PyObject;
   --  A few examples of functions to call a method.
   --  In C, the profile of this method is:
   --     PyObject* PyObject_CallMethod
   --        (PyObject* object, char* name, char* format, ...);
   --  For instance, to call it with an object and a integer as a parameter,
   --  you would use:
   --    result = PyObject_CallMethod (object, "method", "(Oi)", other_obj, 1);
   --
   --  format has the same form as in the calls to Py_BuildValue

   function PyObject_SetAttrString
     (Object : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr;
      Attr   : PyObject) return Integer;
   pragma Import (C, PyObject_SetAttrString, "PyObject_SetAttrString");
   --  Set the value of the attribute named Name, for Object, to the value
   --  Attr. Returns -1 on failure. This is the equivalent of the Python
   --  statement "Object.Name = Attr".

   procedure PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject);
   --  Same as above

   function PyObject_GetAttrString
     (Object : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr) return PyObject;
   pragma Import (C, PyObject_GetAttrString, "PyObject_GetAttrString");
   --  Lookup an attribute in the object's dictionnary.
   --  No need to Py_DECREF the return type, this is a borrowed decision.

   function PyObject_GetAttrString
     (Object : PyObject; Name : String) return PyObject;
   --  Same as above

   function PyObject_Dir (Object : PyObject) return PyObject;
   --  A list of strings for all entries in Object's dictionary.

   --------------
   -- Integers --
   --------------
   --  Not bound: PyInt_FromString and PyInt_FromUnicode

   function PyInt_FromLong (Value : Interfaces.C.long) return PyObject;
   --  Create a new integer object from its value

   function PyInt_AsLong (Int : PyObject) return Interfaces.C.long;
   --  Return the value of Int.
   --  Return -1 and set PyErr_Occurred if Int is not an integer object.

   function PyInt_GetMax return Interfaces.C.long;
   --  Return the maximum value an integer can have

   function PyInt_Check (Obj : PyObject) return Boolean;
   --  Returns true if the Obj is an integer object.

   ------------
   -- Tuples --
   ------------
   --  The following subprograms are in fact simple examples of importing the C
   --  function in your C code, depending on your exact requirement. In C,
   --  these are function with unknown number of parameters
   --
   --  The C function is:
   --      int PyArg_ParseTuple(PyObject *arg, char *format, ...);

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4, Value5 : System.Address) return Boolean;
   --  Parses Format, and stores each of the tuple element in each of the
   --  Values. The number of elements in Format must be the same as the number
   --  of Value parameter
   --  The exact description of the format should be found in the python
   --  documentation.
   --  Note: there is *no* type safety in these functions, but then neither is
   --  there in C.

   subtype PyTuple is PyObject;

   function PyTuple_New (Size : Integer) return PyObject;
   --  Create a new tuple that contains Size elements.

   function PyTuple_GetItem (Tuple : PyTuple; Index : Integer) return PyObject;
   --  Get the item at a specific location in the tuple, starting at index 0.

   procedure PyTuple_SetItem
     (Tuple : PyTuple; Index : Integer; Value : PyObject);
   --  Set an item in the tuple.

   function PyTuple_Size (Tuple : PyTuple) return Integer;
   --  Return the size of the tuple.

   function Create_Tuple (Objects : PyObject_Array) return PyObject;
   --  Return a new tuple made of Objects.

   function PyTuple_Check (Obj : PyObject) return Boolean;
   --  Whether Object is a tuple.

   -----------
   -- Lists --
   -----------

   function PyList_New (Size : Integer := 0) return PyObject;
   --  Create a new empty list, with an initialize size.

   function PyList_Append (List : PyObject; Obj : PyObject) return Integer;
   --  Append Obj at the end of List, and return the index of the newly
   --  inserted item

   -------------
   -- Strings --
   -------------

   function PyString_Check (Obj : PyObject) return Boolean;
   --  Returns true if the Obj is a string object.

   function PyString_AsString (Str : PyObject)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, PyString_AsString, "PyString_AsString");
   --  Returns a NULL terminated representation of the contents of string.
   --  Do not free the returned value.

   function PyString_AsString (Str : PyObject) return String;
   --  Same as above, higher-level

   function PyString_FromString (Str : String) return PyObject;
   --  Return a python object representing Str

   --------------
   -- Booleans --
   --------------

   function Py_False return PyObject;
   function Py_True  return PyObject;
   --  These two functions return one of the only two booleans in existence in
   --  the interpreter.
   --  You need to Py_INCREF them before returning them from your functions.

   -------------
   -- Modules --
   -------------

   function PyImport_AddModule (Module_Name : String) return PyObject;
   --  Return the module object corresponding to a module name.  The name
   --  argument may be of the form package.module.  First check the modules
   --  dictionary if there's one there, and if not, create a new one and insert
   --  in in the modules dictionary.  Because the former action is most common,
   --  this does not return a new reference, and you do not own the returned
   --  reference.
   --
   --  Warning: this function does not load or import the module; if the module
   --  wasn't already loaded, you will get an empty module object.  Use
   --  PyImport_ImportModule() or one of its variants to import a module.
   --  Return NULL with an exception set on failure.

   function PyImport_ImportModule (Module_Name : String) return PyObject;
   --  Import a new module in the interpreter

   function PyModule_GetDict (Module : PyObject) return PyObject;
   --  Return the dictionary object that implements module's namespace; this
   --  object is the same as the __dict__ attribute of the module object.  This
   --  function never fails.
   --  It is recommended that you use the other PyModule_* subprograms rather
   --  than manipulate this dictionnary directly.
   --  The returned dictionary is a borrow reference, so you shouldn't
   --  Py_DECREF it.

   function PyModule_AddObject
     (Module : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr;
      Object : PyObject) return Integer;
   pragma Import (C, PyModule_AddObject, "PyModule_AddObject");
   --  Add a new object to the module's directory. Object can be a subprogram,
   --  integer, ... Do not Py_DECREF Object afterward, this is only a borrowed
   --  reference.
   --  Return 0 in case of success, -1 in case of error.

   function PyModule_AddObject
     (Module : PyObject; Name : String;  Object : PyObject) return Integer;
   --  Same as above

   ------------------
   -- Dictionaries --
   ------------------
   --  Dictionaries are hash tables, used internally by python to associate
   --  functions with modules or methods with objects.
   --  See PyModule_GetDict to see how to get the dictionary from a module.

   subtype PyDictObject is PyObject;

   function PyDict_New return PyDictObject;
   --  Create a new empty dictionary.

   function PyDict_SetItemString
     (Dict : PyDictObject;
      Key  : Interfaces.C.Strings.chars_ptr;
      Obj  : PyObject)
      return Integer;
   pragma Import (C, PyDict_SetItemString, "PyDict_SetItemString");
   --  Store a new object in Dict. Obj should be Py_DECREF after the call.
   --  Return 0 if all went well, -1 otherwise
   --  Key should be deallocated.

   procedure PyDict_SetItemString
     (Dict : PyDictObject; Key : String; Obj : PyObject);
   --  Same as above

   function PyDict_GetItemString
     (Dict : PyDictObject;
      Key  : Interfaces.C.Strings.chars_ptr) return PyObject;
   pragma Import (C, PyDict_GetItemString, "PyDict_GetItemString");
   --  Get an object from a dictionary

   function PyDict_GetItemString
     (Dict : PyDictObject; Key  : String) return PyObject;
   --  Same as above

   ----------------
   -- Exceptions --
   ----------------

   procedure PyErr_Print;
   --  Print the current exception and its traceback to sys.stderr.
   --  This also clears the error indicator.
   --  Call this procedure only if the error indicator is set

   procedure PyErr_SetInterrupt;
   --  Interrupt the current command in the interpreter. This is the equivalent
   --  of Control-C in a terminal executing python.

   procedure PyErr_Fetch
     (EType      : out PyObject;
      Occurrence : out PyObject;
      Traceback  : out PyObject);
   --  Get the current exception information.
   --  Occurrence is a tuple, made of the following information:
   --    (msg, ('input_stream_name', line, column, input_text))
   --    where msg is the exception's message, and the second tuple is the
   --    location where the exception occurred.
   --  EType is the type of the exception, like "exceptions.SyntaxError".
   --
   --  This calls clears the current exception. If you want to call PyErr_Print
   --  later on, you will need to call PyErr_Restore with the same parameters
   --  to restore the current exception.

   procedure PyErr_NormalizeException
     (EType      : in out PyObject;
      Occurrence : in out PyObject;
      Traceback  : in out PyObject);
   --  Normalize a raised exception. This generally needs to be called after
   --  PyErr_Fetch.
   --  This ensure that if EType is an class, Occurrence is an instance.

   procedure PyErr_Restore
     (EType      : PyObject;
      Occurrence : PyObject;
      Traceback  : PyObject);
   --  Set the current exception

   procedure PyErr_Clear;
   --  Clear the current exception. This must be called at the end of your
   --  exception handlers, although it is called automatically by PyErr_Print

   procedure PyErr_BadArgument;
   --  Set the current exception as a "bad argument" exception. The function
   --  should also return null to its caller.

   function PyErr_Occurred return PyObject;
   --  Return the current exception, or null if no exception was raised.

   function PyErr_NewException
     (Name : String; Base : PyObject := null; Dict : PyObject := null)
     return PyObject;
   --  Create a new exception, which can then be raised by:
   --   - calling PyErr_SetString (Except, "message");
   --   - returning null from your subprogram
   --  Name must be of the form "module.name"

   procedure PyErr_SetString (Except : PyObject; Msg : String);
   --  Raise Except, and associate it with a specific message.

   ---------
   -- Sys --
   ---------

   procedure PySys_SetObject (Name : String; Object : PyObject);
   --  Set one of the predefined objects in the python interpreter. See the
   --  module "sys".
   --  Among these objects are:
   --    - "stdin", "stdout", "stderr": standard file objects
   --    - "_stdin", _stdout", "_stderr": initial values for standard files
   --    - "modules": dictionary of modules
   --    - "path": module search path
   --    - "ps1", "ps2": prompts
   --    - "displayhook":  ???
   --    - "excepthook": ???

   function PySys_GetObject (Name : String) return PyObject;
   --  Return an object from the sys module,
   --  Returned object must not be Py_DECREF by the caller.

   ------------------------
   -- Executing commands --
   ------------------------

   function PyRun_SimpleString (Cmd : String) return Boolean;
   --  Executes Cmd in the __main__ module.
   --  Return True on success, False if an exception occured (it is your
   --  responsability to check the current exception)

   type Interpreter_State is  (Py_Single_Input, Py_File_Input, Py_Eval_Input);
   pragma Convention (C, Interpreter_State);
   --  The state of the interpreter when evaluating a string.
   --    - Single_Input: evaluate any command in the interpreter. This will
   --      return the resulting object.
   --    - Eval_Input: evaluate an expression. None is always returned
   --    - File_Input: evaluate a whole file, and return None.

   function PyRun_String
     (Str     : String;
      Start   : Interpreter_State;
      Globals : PyObject;
      Locals  : PyObject) return PyObject;
   --  Execute Python source code from str in the context specified by the
   --  dictionaries globals and locals.  The parameter start specifies the
   --  start token that should be used to parse the source code.
   --
   --  Returns NULL if an exception occured, None otherwise.

   function Py_CompileString
     (Cmd : String; Name : String; State : Interpreter_State)
      return PyCodeObject;
   --  Compile Cmd into a code object. Null is returned if Cmd couldn't be
   --  compiled, either because of a syntax error or because Cmd is incomplete

   function PyEval_EvalCode
     (Code    : PyCodeObject;
      Globals : PyObject;
      Locals  : PyObject) return PyObject;
   --  Evaluate a precompiled code object

   --------------------------------------
   -- Evaluating and Tracing execution --
   --------------------------------------
   --  Python will periodically call two functions that you can register: a
   --  profile function, called every time a subprogram is called or returns,
   --  and a trace function called for every instruction.
   --  These can be used to trace the execution of your program, but also to
   --  interrupt a parser embedded in your application:
   --   - register a trace function, and every n calls, check for gtk events
   --     and call PyErr_SetInterrupt if necessary
   --   - a profile function would not be called for an infinite loop that
   --     never calls another subprogram, so is not appropriate for for such
   --     usage.
   --  There is still a catch: you will not be able to interrupt a long sleep()
   --  operation with this method, since the interpret itself is paused. The
   --  best solution to handle this is to have your own Control-C handler,
   --  although the user would have to type this in the terminal used to start
   --  your application.

   type Why_Trace_Func is
     (PyTrace_Call,
      PyTrace_Exception,
      PyTrace_Line,
      PyTrace_Return);

   type Py_Trace_Func is access function
     (User_Arg : PyObject;
      Frame    : System.Address;
      Why      : Why_Trace_Func;
      Object   : PyObject) return Integer;
   --  Return 0 in case of success, or -1 if an exception is raised.
   --  Objects's value depends on the type of callback. For PyTrace_Return,
   --  this is the returned value. For PyTrace_Exception, this is the
   --  exception.  PyTrace_Line is called for all instructions, but only for
   --  the trace function, not the profile function.

   procedure PyEval_SetProfile (Proc : Py_Trace_Func; User_Arg : PyObject);
   --  Register a new profiling function.

   procedure PyEval_SetTrace (Proc : Py_Trace_Func; User_Arg : PyObject);
   --  Register a new tracing function.

private

   type Dummy is null record;

   for Interpreter_State use (Py_Single_Input => 256,
                              Py_File_Input   => 257,
                              Py_Eval_Input   => 258);
   --  Values are copied from Python.h, and must be synchronized. They will
   --  probably never change, though, so this should be safe.

   pragma Convention (C, Why_Trace_Func);
   pragma Convention (C, Py_Trace_Func);
   pragma Import (C, PyDict_New, "PyDict_New");
   pragma Import (C, PyEval_SetProfile, "PyEval_SetProfile");
   pragma Import (C, PyEval_SetTrace, "PyEval_SetTrace");
   pragma Inline (PyImport_AddModule);
   pragma Inline (PyRun_SimpleString);
   pragma Inline (PyArg_ParseTuple);
   pragma Inline (PyString_Check);
   pragma Inline (PyInt_Check);
   pragma Import (C, Py_Initialize, "Py_Initialize");
   pragma Import (C, PyModule_GetDict, "PyModule_GetDict");
   pragma Import (C, Py_INCREF, "ada_py_incref");
   pragma Import (C, Py_DECREF, "ada_py_decref");
   pragma Import (C, Py_XINCREF, "ada_py_xincref");
   pragma Import (C, Py_XDECREF, "ada_py_xdecref");
   pragma Import (C, PyErr_Print, "PyErr_Print");
   pragma Import (C, PyObject_Str, "PyObject_Str");
   pragma Import (C, PyEval_EvalCode, "PyEval_EvalCode");
   pragma Import (C, PyErr_SetInterrupt, "PyErr_SetInterrupt");
   pragma Import (C, PyTuple_New, "PyTuple_New");
   pragma Import (C, PyTuple_GetItem, "PyTuple_GetItem");
   pragma Import (C, PyTuple_SetItem, "PyTuple_SetItem");
   pragma Import (C, Py_None, "ada_py_none");
   pragma Import (C, PyErr_Clear, "PyErr_Clear");
   pragma Import (C, PyErr_Fetch, "PyErr_Fetch");
   pragma Import (C, PyTuple_Size, "PyTuple_Size");
   pragma Import (C, PyInt_FromLong, "PyInt_FromLong");
   pragma Import (C, PyInt_AsLong, "PyInt_AsLong");
   pragma Import (C, PyInt_GetMax, "PyInt_GetMax");
   pragma Import (C, PyErr_Occurred, "PyErr_Occurred");
   pragma Import (C, PyList_New, "PyList_New");
   pragma Import (C, PyList_Append, "PyList_Append");
   pragma Import (C, PyErr_BadArgument, "PyErr_BadArgument");
   pragma Import (C, PyErr_NormalizeException, "PyErr_NormalizeException");
   pragma Import (C, PyObject_Dir, "PyObject_Dir");
   pragma Import (C, PyObject_Repr, "PyObject_Repr");
   pragma Import (C, PyErr_Restore, "PyErr_Restore");
   pragma Import (C, Py_False, "ada_py_false");
   pragma Import (C, Py_True, "ada_py_true");
end Python;
