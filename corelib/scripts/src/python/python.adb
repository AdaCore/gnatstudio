-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007                       --
--                             AdaCore                               --
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

with System;               use System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Python is

   No_Method_Def : constant PyMethodDef := (Null_Ptr, null, 0, Null_Ptr);

   function Python_API_Version return Integer;
   pragma Import (C, Python_API_Version, "ada_python_api_version");

   type Methods_Access is access PyMethodDef_Array;
   type MethodDef_Access is access PyMethodDef;
   pragma Convention (C, MethodDef_Access);

   function PyCFunction_New
     (MethodDef : MethodDef_Access;
      Self      : PyObject;
      Module    : PyObject := null) return PyObject;
   pragma Import (C, PyCFunction_New, "ada_pycfunction_newex");
   --  Create a new callable object, which, when called from python, will call
   --  the Ada subprogram.
   --  This should be used only for standard functions, not for object methods
   --  Self is the first argument that will be passed to the Ada subprogram.

   function PyMethod_New
     (Func : PyObject; Self : PyObject := null; Klass : PyObject)
      return PyObject;
   pragma Import (C, PyMethod_New, "PyMethod_New");
   --  Create a new method, which calls Func.
   --  The method is unbounded if Self is null (and will be bound when the
   --  method is called). It is automatically bound if Self is not null.

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
      function Internal
        (Arg : PyObject; Format : String; V1 : System.Address) return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr2");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1, Value2) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr3");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1, Value2, Value3) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3, V4 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr4");
   begin
      return Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4, Value5 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3, V4, V5 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr5");
   begin
      return Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4, Value5) = 1;
   end PyArg_ParseTuple;

   ----------------------
   -- PyFunction_Check --
   ----------------------

   function PyFunction_Check (Func : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyfunction_check");
   begin
      return Internal (Func) = 1;
   end PyFunction_Check;

   --------------------
   -- PyString_Check --
   --------------------

   function PyString_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pystring_check");
   begin
      return Internal (Obj) = 1;
   end PyString_Check;

   ------------------
   -- PyList_Check --
   ------------------

   function PyList_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pylist_check");
   begin
      return Internal (Obj) = 1;
   end PyList_Check;

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
      function Internal (Object : PyObject; Name : String) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod");
   begin
      return Internal (Object, Name & ASCII.NUL);
   end PyObject_CallMethod;

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : PyObject) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Arg : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod_obj");
   begin
      return Internal (Object, Name & ASCII.NUL, Arg1);
   end PyObject_CallMethod;

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : Integer) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Arg : Integer) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod_int");
   begin
      return Internal (Object, Name & ASCII.NUL, Arg1);
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
      S      : chars_ptr := New_String (Key);
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
      S      : chars_ptr := New_String (Name);
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
      S      : chars_ptr := New_String (Key);
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
   -- PyObject_HasAttrString --
   ----------------------------

   function PyObject_HasAttrString
     (Obj : PyObject; Attr_Name : String) return Boolean
   is
      function Internal (Object : PyObject; S : String) return Integer;
      pragma Import (C, Internal, "PyObject_HasAttrString");
   begin
      return Boolean'Val (Internal (Obj, Attr_Name & ASCII.NUL));
   end PyObject_HasAttrString;

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

   -----------------
   -- PyDict_Next --
   -----------------

   procedure PyDict_Next
     (Dict  : PyObject;
      Pos   : in out Integer;
      Key   : out PyObject;
      Value : out PyObject)
   is
      function Internal
        (Dict : PyObject; Pos, Key, Value : System.Address) return Integer;
      pragma Import (C, Internal, "PyDict_Next");
   begin
      if Internal (Dict, Pos'Address, Key'Address, Value'Address) = 0 then
         Pos := -1;
      end if;
   end PyDict_Next;

   --------------------
   -- Print_Refcount --
   --------------------

   procedure Print_Refcount (Obj : PyObject; Msg : String) is
      procedure Internal (Obj : PyObject; Msg : String);
      pragma Import (C, Internal, "ada_py_print_refcount");
   begin
      Internal (Obj, Msg & ASCII.NUL);
   end Print_Refcount;

   ------------------------
   -- PyFile_WriteString --
   ------------------------

   function PyFile_WriteString
     (Text : String; File : PyObject) return Boolean
   is
      function Internal (Text : String; File : PyObject) return Integer;
      pragma Import (C, Internal, "PyFile_WriteString");
   begin
      return Internal (Text & ASCII.NUL, File) /= 0;
   end PyFile_WriteString;

   -------------------
   -- Py_InitModule --
   -------------------

   function Py_InitModule
     (Module_Name : String;
      Methods     : PyMethodDef_Array := No_MethodDef_Array;
      Doc         : String := "") return PyObject
   is
      function Internal
        (N       : String;
         Methods : System.Address;
         Doc     : String;
         Self    : PyObject := null;
         Apiver  : Integer := Python_API_Version) return PyObject;
      pragma Import (C, Internal, "ada_Py_InitModule4");

      M : Methods_Access;
   begin
      if Methods /= No_MethodDef_Array then
         --  ??? Memory is never freed, but Python is not supposed to be killed
         --  before the end of the application
         M := new PyMethodDef_Array'(Methods & No_Method_Def);
         return Internal
           (Module_Name & ASCII.NUL, M.all'Address,
            Doc & ASCII.NUL);

      else
         return Internal
           (Module_Name & ASCII.NUL, System.Null_Address,
            Doc & ASCII.NUL);
      end if;
   end Py_InitModule;

   ----------
   -- Free --
   ----------

   procedure Free (Method : in out PyMethodDef) is
      procedure C_Free (C : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, C_Free, "free");

   begin
      C_Free (Method.Name);
      C_Free (Method.Doc);
      Method.Name := Null_Ptr;
      Method.Doc := Null_Ptr;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Methods : in out PyMethodDef_Array) is
   begin
      for M in Methods'Range loop
         Free (Methods (M));
      end loop;
   end Free;

   ------------------
   -- Add_Function --
   ------------------

   procedure Add_Function
     (Module : PyObject; Func : PyMethodDef; Self : PyObject := null)
   is
      C_Func : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      if Self /= null then
         C_Func := PyCFunction_New
           (new PyMethodDef'(Func), Self, PyString_FromString ("GPS"));
      else
         C_Func := PyCFunction_New
           (new PyMethodDef'(Func), Module, PyString_FromString ("GPS"));
      end if;

      if C_Func /= null then
         Result := PyModule_AddObject (Module, Func.Name, C_Func);
      end if;
   end Add_Function;

   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method
     (Class : PyClassObject;
      Func  : PyMethodDef;
      Self  : PyObject := null)
   is
      C_Func  : constant PyObject :=
        PyCFunction_New (new PyMethodDef'(Func), Self,
                         PyString_FromString ("GPS"));
      C_Meth  : constant PyObject := PyMethod_New (C_Func, null, Class);
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      Ignored := PyObject_SetAttrString (Class, Func.Name, C_Meth);
   end Add_Method;

   -----------------------
   -- Add_Static_Method --
   -----------------------

   procedure Add_Static_Method
     (Class : PyClassObject; Func : PyMethodDef; Self : PyObject := null)
   is
      function PyStaticMethod_New (Method : PyObject) return PyObject;
      pragma Import (C, PyStaticMethod_New, "PyStaticMethod_New");

      Def    : constant MethodDef_Access := new PyMethodDef'(Func);
      C_Func : PyObject;
      Static : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_STATIC;

      --  If the documentation is not specified, store the fully qualified
      --  name instead. There is no way otherwise to retrieve the class name
      --  from the python shell.
      if Def.Doc = Null_Ptr then
         Def.Doc := New_String
           (PyString_AsString (PyObject_GetAttrString (Class, "__module__"))
            & "." & PyString_AsString (PyClass_Name (Class))
            & "." & Value (Func.Name));
      end if;

      C_Func := PyCFunction_New (Def, Self, PyString_FromString ("GPS"));
      if C_Func /= null then
         Static := PyStaticMethod_New (C_Func);
         Result := PyObject_SetAttrString (Class, Func.Name, Static);
      end if;
   end Add_Static_Method;

   ----------------------
   -- Add_Class_Method --
   ----------------------

   procedure Add_Class_Method (Class : PyClassObject; Func : PyMethodDef) is
      function PyClassMethod_New (Method : PyObject) return PyObject;
      pragma Import (C, PyClassMethod_New, "PyClassMethod_New");

      Def    : constant MethodDef_Access := new PyMethodDef'(Func);
      C_Func : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_CLASS;

      --  If the documentation is not specified, store the fully qualified
      --  name instead. There is no way otherwise to retrieve the class name
      --  from the python shell.
      if Def.Doc = Null_Ptr then
         Def.Doc := New_String
           (PyString_AsString (PyObject_GetAttrString (Class, "__module__"))
            & "." & PyString_AsString (PyClass_Name (Class))
            & "." & Value (Func.Name));
      end if;

      C_Func := PyCFunction_New (Def, null, PyString_FromString ("GPS"));
      if C_Func /= null then
         Result := PyObject_SetAttrString
           (Class, Func.Name, PyClassMethod_New (C_Func));
      end if;
   end Add_Class_Method;

   -----------------------
   -- Create_Method_Def --
   -----------------------

   function Create_Method_Def
     (Name : String;
      Func : C_Method_Vargs;
      Doc  : String := "")
      return PyMethodDef is
   begin
      return (Name  => New_String (Name),
              Func  => To_Callback (Func),
              Flags => METH_VARGS,
              Doc   => New_String (Doc));
   end Create_Method_Def;

   -----------------------
   -- Create_Method_Def --
   -----------------------

   function Create_Method_Def
     (Name : String;
      Func : C_Method_Keywords;
      Doc  : String := "")
      return PyMethodDef
   is
      D : chars_ptr := Null_Ptr;
   begin
      if Doc /= "" then
         D := New_String (Doc);
      end if;

      return (Name  => New_String (Name),
              Func  => To_Callback (Func),
              Flags => METH_KEYWORDS,
              Doc   => D);
   end Create_Method_Def;

   -------------------------
   -- Lookup_Class_Object --
   -------------------------

   function Lookup_Class_Object
     (Module : String; Name : String) return PyObject is
   begin
      return Lookup_Class_Object (PyImport_AddModule (Module), Name);
   end Lookup_Class_Object;

   -------------------------
   -- Lookup_Class_Object --
   -------------------------

   function Lookup_Class_Object
     (Module : PyObject; Name : String) return PyObject
   is
      Dict : PyObject;
   begin
      if Module = null then
         return null;
      end if;

      Dict := PyModule_GetDict (Module);
      return PyDict_GetItemString (Dict, Name);
   end Lookup_Class_Object;

   ---------------------
   -- PyCObject_Check --
   ---------------------

   function PyCObject_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pycobject_check");
   begin
      return Internal (Obj) = 1;
   end PyCObject_Check;

   ----------------------
   -- PyInstance_Check --
   ----------------------

   function PyInstance_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyinstance_check");
   begin
      return Internal (Obj) = 1;
   end PyInstance_Check;

   --------------------
   -- PyMethod_Check --
   --------------------

   function PyMethod_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pymethod_check");
   begin
      return Internal (Obj) = 1;
   end PyMethod_Check;

   ------------------------
   -- PyClass_IsSubclass --
   ------------------------

   function PyClass_IsSubclass
     (Class : PyObject; Base : PyObject) return Boolean
   is
      function Internal (Class, Base : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyclass_is_subclass");
   begin
      return Internal (Class, Base) /= 0;
   end PyClass_IsSubclass;

end Python;
