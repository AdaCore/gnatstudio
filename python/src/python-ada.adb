-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
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

--  This package provides various subprograms to extend Python with new classes
--  written in Ada.

with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body Python.Ada is

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

      M : constant Methods_Access := new PyMethodDef_Array'
        (Methods & No_Method_Def);
   begin
      --  ??? Memory is never freed, but Python is not supposed to be killed
      --  before the end of the application
      return Internal
        (Module_Name & ASCII.NUL, M.all'Address,
         Doc & ASCII.NUL);
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
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_CLASS;
      C_Func := PyCFunction_New (Def, Self, PyString_FromString ("GPS"));
      if C_Func /= null then
         Result := PyObject_SetAttrString
           (Class, Func.Name, PyStaticMethod_New (C_Func));
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
      Def.Flags := Def.Flags or METH_STATIC;
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

   ------------------------
   -- PyClass_IsSubclass --
   ------------------------

   function PyClass_IsSubclass
     (Class : PyObject; Base : PyObject) return Boolean
   is
      function Internal (Class, Base : PyObject) return Integer;
      pragma Import (C, Internal, "PyClass_IsSubclass");
   begin
      return Internal (Class, Base) /= 0;
   end PyClass_IsSubclass;

end Python.Ada;
