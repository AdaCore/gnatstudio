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

with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package Python.Ada is

   type Argument_Methods is mod 2 ** Integer'Size;
   METH_VARGS    : constant Argument_Methods := 16#0001#;
   METH_KEYWORDS : constant Argument_Methods := 16#0002#;
   METH_NOARGS   : constant Argument_Methods := 16#0004#;
   METH_CLASS    : constant Argument_Methods := 16#0010#;
   METH_STATIC   : constant Argument_Methods := 16#0020#;
   --  How arguments are passed to callbacks:
   --   - METH_VARGS: only positional arguments in the form of a tuple are
   --     accepted
   --   - "METH_VARGS or METH_KEYWORDS": a function accepting keyword
   --     arguments.
   --   - METH_CLASS and METH_STATIC can only be used for class methods, not
   --     for module methods. They both indicate that a method is a class-wide
   --     method. They are callable from the class or an instance, but the
   --     instance is ignored and not passed as a parameter.

   type C_Method_Vargs is access function
     (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, C_Method_Vargs);
   --  A callback for a METH_VARGS method.
   --  The first argument is the object on which the method is applied, or null
   --  if this is a standard function.
   --  The second argument is a tuple of the parameters. They can be extracted
   --  through a call to PyArg_ParseTuple.

   type C_Method_Keywords is access function
     (Self : PyObject; Args : PyObject; Kwargs : PyObject) return PyObject;
   pragma Convention (C, C_Method_Keywords);
   --  A callback for a METH_KEYWORDS method.
   --  The first argument is the object on which the method is applied, or null
   --  if this is a standard function.
   --  The second argument is a tuplie of the positional parameters.
   --  The third argument is a hash table of the named parameters.
   --  Parameters can be extracted through a call to
   --  PyArg_ParseTupleAndKeywords.

   type C_Callback_Record is private;
   type C_Callback is access C_Callback_Record;
   --  The exact type doesn't matter, we only want to cover all possible cases
   --  of callbacks (C_Method_Vargs, C_Method_Keywords)

   function To_Callback is new Standard.Ada.Unchecked_Conversion
     (C_Method_Vargs, C_Callback);
   function To_Callback is new Standard.Ada.Unchecked_Conversion
     (C_Method_Keywords, C_Callback);

   type PyMethodDef is record
      Name  : Interfaces.C.Strings.chars_ptr;
      Func  : C_Callback;
      Flags : Argument_Methods;
      Doc   : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, PyMethodDef);
   --  Definition for one of the methods of an object.
   --  Name is the name used in the python interpreter to reference the method
   --  (one would use the syntax   self.Name (...))
   --  Func is the callback in the Ada code that should be called when the
   --  method is invoked.
   --  Flags indicates how the arguments should be passed.
   --  Doc is the optional documentation string for the method

   No_MethodDef : constant PyMethodDef;

   type PyMethodDef_Array is array (Natural range <>) of PyMethodDef;
   pragma Convention (C, PyMethodDef_Array);
   --  The full list of methods supported by a type.
   --  You do not need to terminate this array by a null element, as is done in
   --  C. This is automatically taken care of by Ada.

   No_MethodDef_Array : constant PyMethodDef_Array;

   procedure Free (Method : in out PyMethodDef);
   procedure Free (Methods : in out PyMethodDef_Array);
   --  Free the memory occupied by Method

   -------------
   -- Modules --
   -------------

   function Py_InitModule
     (Module_Name : String;
      Methods     : PyMethodDef_Array := No_MethodDef_Array;
      Doc         : String := "") return PyObject;
   --  Create and initialize a new module, which a set of predefined methods.
   --  Do not free Methods while the module is in use.
   --  The module is not visible in the GPS interpreter until you have done a
   --  "import MODULE_NAME" in the interpreter.
   --
   --  The first parameter to the methods declared in Methods will be null.

   procedure Add_Function (Module : PyObject; Func : PyMethodDef);
   --  Add a new function to Module.
   --  Do not free Func while this function is registered.
   --  The first parameter to Func will be Module

   ------------------
   -- Object types --
   ------------------

   type PyTypeObject_Record is private;
   type PyTypeObject is access all PyTypeObject_Record;
   --  The internal structure that describes a Python type (and all the default
   --  primitive subprograms like __getattr__, __setattr__, ...

   function GetTypeObject (Obj : PyObject) return PyTypeObject;
   --  Return the type object that describes the class Obj belongs to.

   -----------------
   -- Class types --
   -----------------

   subtype PyClassObject is PyObject;

   function PyClass_New
     (Bases : PyObject;
      Dict  : PyObject;
      Name  : PyObject) return PyClassObject;
   --  Create a new class.
   --  Bases should be either null or a tuple of PyClassObject (See
   --  Lookup_Class_Object below)
   --  Dict must be a dictionary, used to store the attributes of the class,
   --  including subprograms. The following keys are automatically extracted
   --  and use for the class:
   --    - "__doc__"     (documentation for the class)
   --    - "__module__"  (module in which the class is defined)
   --    - "__getattr__" (default subprogram to retrieve attributes)
   --    - "__setattr__" (default subprogram to set attributes)
   --    - "__delattr__" (default subprogram to remove an attribute)
   --  Name is the name of the class.
   --  The class must be added to the module, through PyModule_AddObject.
   --
   --  Typical use to build a new class:
   --     Dict := PyDict_New;
   --     PyDict_SetItemString
   --       (Dict, "__module__", PyString_FromString ("mymodule"));
   --     Klass := PyClass_New
   --       (Create_Tuple ((1 => Lookup_Class_Object ("__builtin__", "file"))),
   ---       Dict,
   --        PyString_FromString ("Myclass"));
   --     Add_Method (Dict, Create_Method_Def (...), Klass);
   --     Add_Method (Dict, Create_Method_Def (...), Klass);

   function Lookup_Class_Object
     (Module : String; Name : String) return PyObject;
   --  Lookup a class object.
   --  Typical use is
   --     Klass := Lookup_Class_Object ("__builtin__", "file");
   --  null is returned if the class is not found

   procedure Add_Method
     (Klass : PyClassObject; Func : PyMethodDef; Self : PyObject := null);
   --  Add a new method to the class.
   --  The method is an instance method.
   --  When the method is called from the python interpreter, its Self argument
   --  is set to the value of Self.
   --  Its first argument will always be the instance itself. Therefore the
   --  first character in the argument to PyArg_ParseTuple should be "O".

   procedure Add_Static_Method (Class : PyClassObject; Func : PyMethodDef);
   --  Return a static version of Method. This method doesn't receive an
   --  instance or the class as its first parameter. This is similar to C++ or
   --  Java's static methods

   procedure Add_Class_Method (Class : PyClassObject; Func : PyMethodDef);
   --  Return a class version of Method.
   --  This is a method that recieves the class as implicit first argument,
   --  just like an instance method receives the instance.
   --  It can be called either on the class or an instance. If a class method
   --  is called for a derived class, the derived class object is passed as the
   --  implied first argument.

   ------------------------------------
   -- Creating and declaring methods --
   ------------------------------------

   function Create_Method_Def
     (Name  : String;
      Func  : C_Method_Vargs;
      Doc   : String           := "")
      return PyMethodDef;
   --  Convenience function to create method definitions.
   --  See the description of the parameters in the declaration of PyMethodDef
   --  The flags are automatically set to METH_VARGS, which is the appropriate
   --  type for callbacks of this form.
   --  The returned value must be freed by the caller.

   function Create_Method_Def
     (Name  : String;
      Func  : C_Method_Keywords;
      Doc   : String           := "")
      return PyMethodDef;
   --  Same as above, for methods accepting keywords.
   --  The returned value must be freed by the caller

   -------------------------------------
   -- Embedding Ada objects in python --
   -------------------------------------

   subtype PyCObject is PyObject;
   --  This type represents an opaque value that contains any kind of data,
   --  transparent for python.

   type PyCObject_Destructor is access procedure (Obj : System.Address);
   type PyCObject_Destructor2 is access
     procedure (Obj : System.Address; Desc : System.Address);

   function PyCObject_FromVoidPtr
     (Obj : System.Address;
      Destr : PyCObject_Destructor := null)
     return PyObject;
   --  Create a new PyCObject that encapsulate Obj. Dest is called when the
   --  object is reclaimed, unless it is null.
   --  Returns a newly referenced object.

   function PyCObject_FromVoidPtrAndDesc
     (Obj   : System.Address;
      Desc  : System.Address;
      Destr : PyCObject_Destructor2 := null)
     return PyObject;
   --  Same as above, except Desc is also passed to Destr.

   function PyCObject_AsVoidPtr (Self : PyObject) return System.Address;
   --  Return the Ada object embedded in Self

   function PyCObject_GetDesc (Self : PyObject) return System.Address;
   --  Return the Desc object that Self was created with, or null.

private
   type C_Callback_Record is new Integer; --  whatever

   No_MethodDef : constant PyMethodDef :=
     (Interfaces.C.Strings.Null_Ptr, null, 0,
      Interfaces.C.Strings.Null_Ptr);
   No_MethodDef_Array : constant PyMethodDef_Array := (1 .. 0 => No_MethodDef);

   type PyTypeObject_Record is new Integer; --  whatever
   pragma Convention (C, PyTypeObject);

   pragma Import (C, GetTypeObject, "ada_gettypeobject");
   pragma Import (C, PyClass_New, "PyClass_New");
   pragma Import (C, PyCObject_FromVoidPtr, "PyCObject_FromVoidPtr");
   pragma Import
     (C, PyCObject_FromVoidPtrAndDesc, "PyCObject_FromVoidPtrAndDesc");
   pragma Import (C, PyCObject_AsVoidPtr, "PyCObject_AsVoidPtr");
   pragma Import (C, PyCObject_GetDesc, "PyCObject_GetDesc");
end Python.Ada;
