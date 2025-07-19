------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Python;
with GNATCOLL.Scripts.Python;

with VSS.Implementation.Interfaces_C;
with VSS.Implementation.Python3;
with VSS.Implementation.Text_Storages.Python;
with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Strings;
with VSS.Strings.Conversions;
with VSS.Strings.Internals;
with VSS.Unicode;

package body GNATCOLL.Scripts.VSS_Utils is

   function PyUnicode_FromStringAndSize
     (Str : VSS.Strings.Virtual_String) return GNATCOLL.Python.PyObject;

   function Nth_Arg
     (Data    : GNATCOLL.Scripts.Python.Python_Callback_Data'Class;
      N       : Positive;
      Success : aliased out Boolean) return VSS.Strings.Virtual_String;

   procedure Initialize
     (Text   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Object : GNATCOLL.Python.PyObject);

   procedure Initialize
     (Text   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Object : GNATCOLL.Python.PyObject)
   is
      use type GNATCOLL.Python.PyObject;

      Bytes   : GNATCOLL.Python.PyObject;
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if not GNATCOLL.Python.PyUnicode_Check (Object) then
         Text := (others => <>);

         return;
      end if;

      Bytes := VSS.Implementation.Python3.PyUnicode_AsUTF8String (Object);

      if Bytes = null then
         Text := (others => <>);

         return;
      end if;

      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen
      end if;

      declare
         Manager :
           VSS.Implementation.Text_Storages.Python.Python_Text_Storage :=
             (others => <>)
           with Address => Text.Manager'Address;
      begin
         Manager.Initialize (Text.Storage_Address, Bytes);
         Text.Size   :=
           VSS.Unicode.UTF8_Code_Unit_Offset
             (VSS.Implementation.Python3.PyBytes_Size (Bytes));
         Text.Length :=
           VSS.Implementation.Strings.Character_Count
             (VSS.Implementation.Python3.PyUnicode_GetLength (Object));
         Text.Flags  := 1;
      end;
   end Initialize;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : GNATCOLL.Scripts.Python.Python_Callback_Data'Class;
      N    : Positive; Success : aliased out Boolean)
      return VSS.Strings.Virtual_String
   is
      Item : GNATCOLL.Python.PyObject;

   begin
      GNATCOLL.Scripts.Python.Get_Param (Data, N, Item, Success);
      --  `Item` is borrowed reference, not need to be Py_DECREFed

      if not Success then
         return VSS.Strings.Empty_Virtual_String;
      end if;

      if GNATCOLL.Python.PyUnicode_Check (Item) then
         return Result : VSS.Strings.Virtual_String do
            Initialize
              (VSS.Strings.Internals.Data_Access_Variable (Result).all,
               Item);
         end return;

      else
         raise Invalid_Parameter
           with "Parameter" & Integer'Image (N) & " should be a unicode";
      end if;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data'Class;
      N    : Positive) return VSS.Strings.Virtual_String
   is
      Success : aliased Boolean;

   begin
      if Data in GNATCOLL.Scripts.Python.Python_Callback_Data'Class then
         return Result : constant VSS.Strings.Virtual_String :=
           Nth_Arg
             (GNATCOLL.Scripts.Python.Python_Callback_Data'Class (Data),
              N,
              Success)
         do
            if not Success then
               raise No_Such_Parameter with N'Img;
            end if;
         end return;

      else
         return
           VSS.Strings.Conversions.To_Virtual_String
             (String'(Data.Nth_Arg (N)));
      end if;
   end Nth_Arg;

   ---------------------------------
   -- PyUnicode_FromStringAndSize --
   ---------------------------------

   function PyUnicode_FromStringAndSize
     (Str : VSS.Strings.Virtual_String) return GNATCOLL.Python.PyObject
   is
      Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data
        renames VSS.Strings.Internals.Data_Access_Constant (Str).all;

   begin
      if VSS.Implementation.UTF8_Strings.Is_Empty (Text) then
         return
           VSS.Implementation.Python3.PyUnicode_FromStringAndSize (null, 0);

      else
         declare
            D : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access
              with Import, Address => Text.Storage_Address'Address;

         begin
            return
              VSS.Implementation.Python3.PyUnicode_FromStringAndSize
                (D,
                 VSS.Implementation.Python3.Py_ssize_t (Text.Size));
         end;
      end if;
   end PyUnicode_FromStringAndSize;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data  : in out Callback_Data'Class;
      N     : Positive;
      Value : VSS.Strings.Virtual_String) is
   begin
      if Data in GNATCOLL.Scripts.Python.Python_Callback_Data'Class then
         GNATCOLL.Scripts.Python.Set_Nth_Arg
           (GNATCOLL.Scripts.Python.Python_Callback_Data'Class (Data),
            N,
            PyUnicode_FromStringAndSize (Value));

      else
         Data.Set_Nth_Arg (N, VSS.Strings.Conversions.To_UTF_8_String (Value));
      end if;
   end Set_Nth_Arg;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data  : in out Callback_Data'Class;
      Value : VSS.Strings.Virtual_String) is
   begin
      if Data in GNATCOLL.Scripts.Python.Python_Callback_Data'Class then
         declare
            V : constant GNATCOLL.Python.PyObject :=
              PyUnicode_FromStringAndSize (Value);

         begin
            GNATCOLL.Scripts.Python.Set_Return_Value
              (GNATCOLL.Scripts.Python.Python_Callback_Data'Class (Data), V);
            GNATCOLL.Python.Py_DECREF (V);
         end;

      else
         Data.Set_Return_Value
           (VSS.Strings.Conversions.To_UTF_8_String (Value));
      end if;
   end Set_Return_Value;

end GNATCOLL.Scripts.VSS_Utils;
