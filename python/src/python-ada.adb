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

--  This package provides various subprograms to extend Python with new classes
--  written in Ada.

with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body Python.Ada is

   No_Method_Def : constant PyMethodDef := (Null_Ptr, null, 0, Null_Ptr);

   function Python_API_Version return Integer;
   pragma Import (C, Python_API_Version, "ada_python_api_version");

--   PyInstance_Type_Record : aliased PyTypeObject_Record;
--   pragma Import (C, PyInstance_Type_Record, "PyInstance_Type");
--   PyInstance_Type : constant PyTypeObject := PyInstance_Type_Record'Access;
   --  The type of instances created by a PyClassObject class, when called.

   type Methods_Access is access PyMethodDef_Array;
   type MethodDef_Access is access PyMethodDef;
   pragma Convention (C, MethodDef_Access);

   function PyCFunction_New (MethodDef : MethodDef_Access; Self : PyObject)
      return PyObject;
   pragma Import (C, PyCFunction_New, "PyCFunction_New");
   --  Create a new callable object, which, when called from python, will call
   --  the Ada subprogram.
   --  This should be used only for standard functions, not for object methods
   --  Self is the first argument that will be passed to the Ada subprogram.

--   function PyDescr_NewMethod
--     (Instance_Class : PyTypeObject;
--      Method         : MethodDef_Access) return PyObject;
--   pragma Import (C, PyDescr_NewMethod, "PyDescr_NewMethod");
   --  Create a new method, bound to an Ada subprogram.
   --  This is different from PyCFunction_New since, when called, it is bound
   --  to a specific class instance, which then acts as the first parameter to
   --  the function. Instance_Class is the type description for the instance
   --  that is allowed (or one of its subclasses).

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
        (N : String;
         Methods : System.Address;
         Doc : String;
         Self : PyObject := null;
         Apiver : Integer := Python_API_Version) return PyObject;
      pragma Import (C, Internal, "Py_InitModule4");

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
   begin
      Free (Method.Name);
      Free (Method.Doc);
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

   procedure Add_Function (Module : PyObject; Func : PyMethodDef) is
      C_Func : constant PyObject :=
        PyCFunction_New (new PyMethodDef'(Func), Module);
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      if C_Func /= null then
         Result := PyModule_AddObject (Module, Func.Name, C_Func);
      end if;
   end Add_Function;

   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method
     (Klass : PyClassObject;
      Func  : PyMethodDef;
      Self  : PyObject := null)
   is
      C_Func : constant PyObject :=
        PyCFunction_New (new PyMethodDef'(Func), Self);
      C_Meth : constant PyObject := PyMethod_New (C_Func, Self, Klass);
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      Ignored := PyObject_SetAttrString (Klass, Func.Name, C_Meth);
   end Add_Method;

   -----------------------
   -- Add_Static_Method --
   -----------------------

   procedure Add_Static_Method (Class : PyClassObject; Func : PyMethodDef) is
      function PyStaticMethod_New (Method : PyObject) return PyObject;
      pragma Import (C, PyStaticMethod_New, "PyStaticMethod_New");

      Def    : MethodDef_Access := new PyMethodDef'(Func);
      C_Func : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_CLASS;
      C_Func := PyCFunction_New (Def, null);
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

      Def    : MethodDef_Access := new PyMethodDef'(Func);
      C_Func : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_STATIC;
      C_Func := PyCFunction_New (Def, null);
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
      return PyMethodDef is
   begin
      return (Name  => New_String (Name),
              Func  => To_Callback (Func),
              Flags => METH_KEYWORDS,
              Doc   => New_String (Doc));
   end Create_Method_Def;

   -------------------------
   -- Lookup_Class_Object --
   -------------------------

   function Lookup_Class_Object
     (Module : String; Name : String) return PyObject
   is
      M : constant PyObject := PyImport_AddModule (Module);
      Dict : PyObject;
   begin
      if M = null then
         return null;
      end if;

      Dict   := PyModule_GetDict (M);
      return PyDict_GetItemString (Dict, Name);
   end Lookup_Class_Object;

end Python.Ada;
