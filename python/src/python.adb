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

with System;  use System;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body Python is

   ------------------------
   -- PyRun_SimpleString --
   ------------------------

   function PyRun_SimpleString (Cmd : String) return Boolean is
      function Internal (Cmd : String) return Integer;
      pragma Import (C, Internal, "PyRun_SimpleString");
   begin
      return Internal (Cmd & ASCII.NUL) = 0;
   end PyRun_SimpleString;

   ------------------------
   -- PyImport_AddModule --
   ------------------------

   function PyImport_AddModule (Module_Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PyImport_AddModule");
   begin
      return Internal (Module_Name & ASCII.NUL);
   end PyImport_AddModule;

   ---------------------------
   -- PyImport_ImportModule --
   ---------------------------

   function PyImport_ImportModule (Module_Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PyImport_AddModule");
   begin
      return Internal (Module_Name & ASCII.NUL);
   end PyImport_ImportModule;

   ------------------
   -- PyRun_String --
   ------------------

   function PyRun_String
     (Str     : String;
      Start   : Interpreter_State;
      Globals : PyObject;
      Locals  : PyObject) return PyObject
   is
      function Internal
        (Str     : String;
         Start   : Interpreter_State;
         Globals : PyObject;
         Locals  : PyObject) return PyObject;
      pragma Import (C, Internal, "PyRun_String");
   begin
      return Internal (Str & ASCII.LF, Start, Globals, Locals);
   end PyRun_String;

   ----------------------
   -- PyArg_ParseTuple --
   ----------------------

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1 : System.Address) return Boolean
   is
      function Internal (Arg : PyObject; Format : String; V1 : System.Address)
         return Integer;
      pragma Import (C, Internal, "PyArg_ParseTuple");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1) = 1;
   end PyArg_ParseTuple;

   ----------------------
   -- PyArg_ParseTuple --
   ----------------------

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2 : System.Address)
         return Integer;
      pragma Import (C, Internal, "PyArg_ParseTuple");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1, Value2) = 1;
   end PyArg_ParseTuple;

   ----------------------
   -- PyArg_ParseTuple --
   ----------------------

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3 : System.Address)
         return Integer;
      pragma Import (C, Internal, "PyArg_ParseTuple");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1, Value2, Value3) = 1;
   end PyArg_ParseTuple;

   ----------------------
   -- PyArg_ParseTuple --
   ----------------------

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3, V4 : System.Address)
         return Integer;
      pragma Import (C, Internal, "PyArg_ParseTuple");
   begin
      return Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4) = 1;
   end PyArg_ParseTuple;

   ----------------------
   -- PyArg_ParseTuple --
   ----------------------

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4, Value5 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3, V4, V5 : System.Address)
         return Integer;
      pragma Import (C, Internal, "PyArg_ParseTuple");
   begin
      return Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4, Value5) = 1;
   end PyArg_ParseTuple;

   --------------------
   -- PyString_Check --
   --------------------

   function PyString_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pystring_check");
   begin
      return Internal (Obj) = 1;
   end PyString_Check;

   -----------------
   -- PyInt_Check --
   -----------------

   function PyInt_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyint_check");
   begin
      return Internal (Obj) = 1;
   end PyInt_Check;

   -------------------
   -- PyTuple_Check --
   -------------------

   function PyTuple_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pytuple_check");
   begin
      return Internal (Obj) = 1;
   end PyTuple_Check;

   -----------------------
   -- PyString_AsString --
   -----------------------

   function PyString_AsString (Str : PyObject) return String is
   begin
      return Value (PyString_AsString (Str));
   end PyString_AsString;

   -------------------------
   -- PyString_FromString --
   -------------------------

   function PyString_FromString (Str : String) return PyObject is
      function Internal (Str : String) return PyObject;
      pragma Import (C, Internal, "PyString_FromString");
   begin
      return Internal (Str & ASCII.NUL);
   end PyString_FromString;

   ---------------------
   -- PySys_SetObject --
   ---------------------

   procedure PySys_SetObject (Name : String; Object : PyObject) is
      procedure Internal (Name : String; Object : PyObject);
      pragma Import (C, Internal, "PySys_SetObject");
   begin
      Internal (Name & ASCII.NUL, Object);
   end PySys_SetObject;

   ---------------------
   -- PySys_GetObject --
   ---------------------

   function PySys_GetObject (Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PySys_GetObject");
   begin
      return Internal (Name & ASCII.NUL);
   end PySys_GetObject;

   -------------------------
   -- PyObject_CallMethod --
   -------------------------

   function PyObject_CallMethod
     (Object : PyObject; Name : String) return PyObject
   is
      function Internal (Object : PyObject; Name : String; Format : String)
         return PyObject;
      pragma Import (C, Internal, "PyObject_CallMethod");
   begin
      return Internal (Object, Name & ASCII.NUL, "" & ASCII.NUL);
   end PyObject_CallMethod;

   -------------------------
   -- PyObject_CallMethod --
   -------------------------

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : PyObject) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Format : String; Arg : PyObject)
         return PyObject;
      pragma Import (C, Internal, "PyObject_CallMethod");
   begin
      return Internal (Object, Name & ASCII.NUL, "(O)", Arg1);
   end PyObject_CallMethod;

   -------------------------
   -- PyObject_CallMethod --
   -------------------------

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : Integer) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Format : String; Arg : Integer)
         return PyObject;
      pragma Import (C, Internal, "PyObject_CallMethod");
   begin
      return Internal (Object, Name & ASCII.NUL, "(i)", Arg1);
   end PyObject_CallMethod;

   -----------------------
   -- Py_SetProgramName --
   -----------------------

   procedure Py_SetProgramName (Name : String) is
      procedure Internal (Name : String);
      pragma Import (C, Internal, "Py_SetProgramName");
   begin
      Internal (Name & ASCII.NUL);
   end Py_SetProgramName;

   ----------------------
   -- Py_CompileString --
   ----------------------

   function Py_CompileString
     (Cmd : String; Name : String; State : Interpreter_State)
      return PyCodeObject
   is
      function Internal (Cmd, Name : String; State : Interpreter_State)
         return PyCodeObject;
      pragma Import (C, Internal, "Py_CompileString");
   begin
      return Internal (Cmd & ASCII.NUL, Name & ASCII.NUL, State);
   end Py_CompileString;

   --------------------------
   -- PyDict_SetItemString --
   --------------------------

   procedure PyDict_SetItemString
     (Dict : PyDictObject; Key : String; Obj : PyObject)
   is
      S : chars_ptr := New_String (Key);
      Result : constant Integer := PyDict_SetItemString (Dict, S, Obj);
      pragma Unreferenced (Result);
   begin
      Free (S);
   end PyDict_SetItemString;

   ------------------------
   -- PyModule_AddObject --
   ------------------------

   function PyModule_AddObject
     (Module : PyObject; Name : String;  Object : PyObject) return Integer
   is
      S : chars_ptr := New_String (Name);
      Result : Integer;
   begin
      Result := PyModule_AddObject (Module, S, Object);
      Free (S);
      return Result;
   end PyModule_AddObject;

   --------------------------
   -- PyDict_GetItemString --
   --------------------------

   function PyDict_GetItemString
     (Dict : PyDictObject; Key : String) return PyObject
   is
      S : chars_ptr := New_String (Key);
      Result : constant PyObject := PyDict_GetItemString (Dict, S);
   begin
      Free (S);
      return Result;
   end PyDict_GetItemString;

   ------------------
   -- Create_Tuple --
   ------------------

   function Create_Tuple (Objects : PyObject_Array) return PyObject is
      Tuple : constant PyObject := PyTuple_New (Objects'Length);
   begin
      for O in Objects'Range loop
         PyTuple_SetItem (Tuple, O - Objects'First, Objects (O));
      end loop;
      return Tuple;
   end Create_Tuple;

   ------------------------
   -- PyErr_NewException --
   ------------------------

   function PyErr_NewException
     (Name : String; Base : PyObject := null; Dict : PyObject := null)
      return PyObject
   is
      function Internal (Name : String; Base, Dict : PyObject) return PyObject;
      pragma Import (C, Internal, "PyErr_NewException");
   begin
      return Internal (Name & ASCII.NUL, Base, Dict);
   end PyErr_NewException;

   ---------------------
   -- PyErr_SetString --
   ---------------------

   procedure PyErr_SetString (Except : PyObject; Msg : String) is
      procedure Internal (Except : PyObject; Msg : String);
      pragma Import (C, Internal, "PyErr_SetString");
   begin
      Internal (Except, Msg & ASCII.NUL);
   end PyErr_SetString;

   ----------------------------
   -- PyObject_GetAttrString --
   ----------------------------

   function PyObject_GetAttrString
     (Object : PyObject; Name : String) return PyObject
   is
      S : chars_ptr := New_String (Name);
      Result : constant PyObject := PyObject_GetAttrString (Object, S);
   begin
      Free (S);
      return Result;
   end PyObject_GetAttrString;

   ----------------------------
   -- PyObject_SetAttrString --
   ----------------------------

   procedure PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject)
   is
      procedure Internal (Obj : PyObject; Name : String; Val : PyObject);
      pragma Import (C, Internal, "PyObject_SetAttrString");
   begin
      Internal (Obj, Attr_Name & ASCII.NUL, Value);
   end PyObject_SetAttrString;

end Python;
