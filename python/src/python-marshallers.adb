
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with System;

package body Python.Marshallers is

   function Convert is new Ada.Unchecked_Conversion
     (Integer_Callback, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Integer_Callback);

   type PyMarshaller is new PyObject;

   function PyMarshaller_New
     (Marshaller : System.Address;
      Func_Name  : Interfaces.C.Strings.chars_ptr;
      Func       : System.Address;
      Params     : Interfaces.C.Strings.chars_ptr_attr) return PyMarshaller;
   pragma Import (C, PyMarshaller_New, "PyMarshaller_New");
   --  Implemented through new callable C Python type.

   ------------------
   -- Add_Function --
   ------------------

   procedure Add_Function (Module : PyObject; Marsh : PyMarshaller) is
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      if Marsh /= null then
         Result := PyModule_AddObject (Module, Get_Name (Marsh), Marsh);
      end if;
   end Add_Function;

   ------------------------
   -- Integer_Marshaller --
   ------------------------

   function Integer_Marshaller
     (Self, Args, Kw : PyObject; Marsh : PyMarshaller) return PyObject
   is
      pragma Unreferenced (Self);
      A  : aliased Integer;
   begin
      if not Pyarg_ParseTupleAndKeywords
        (Args, Kw, "i:" & Value (Get_Name (Marsh)), Get_Keywords (Marsh),
         A'Address)
      then
         return null;
      end if;

      Convert (Get_Func (Marsh)) (A);
      return Py_None;
   end Integer_Marshaller;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Lang : access Python_Language;
      Func : Integer_Callback;
      Name, Param1 : String) is
   begin
      Add_Function
        (Lang.GPS_Module,
         Pymarshaller_New (Integer_Marshaller'Address,
                           Convert (Func),
                           Name & ASCII.NUL,
                           (0 => New_String (Param1), 1 => Null_Ptr)));
   end Register_Command;
end Python.Marshallers;
