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

end Python;
