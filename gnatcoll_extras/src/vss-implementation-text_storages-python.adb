--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

with VSS.Implementation.Interfaces_C;
with VSS.Implementation.Python3;
with VSS.Implementation.Text_Storages.Heap;
with VSS.Implementation.UTF8_Encoding;

package body VSS.Implementation.Text_Storages.Python is

   function Get_Bytes
     (Self : Python_Text_Storage'Class) return GNATCOLL.Python.PyObject;

   procedure Set_Bytes
     (Self  : in out Python_Text_Storage'Class;
      Bytes : GNATCOLL.Python.PyObject);

   --------------
   -- Capacity --
   --------------

   overriding function Capacity
     (Self : in out Python_Text_Storage)
      return VSS.Unicode.UTF8_Code_Unit_Count is
   begin
      return 0;
   end Capacity;

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes
     (Self : Python_Text_Storage'Class) return GNATCOLL.Python.PyObject
   is
      package Conversions is
        new System.Address_To_Access_Conversions
          (GNATCOLL.Python.PyObject_Opaque);

   begin
      return GNATCOLL.Python.PyObject (Conversions.To_Pointer (Self.Pointer));
   end Get_Bytes;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Python_Text_Storage'Class;
      Storage_Address : out System.Address;
      Bytes           : GNATCOLL.Python.PyObject)
   is
      function To_Address is
        new Ada.Unchecked_Conversion
          (VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access,
           System.Address);

   begin
      Self.Set_Bytes (Bytes);
      Storage_Address :=
        To_Address (VSS.Implementation.Python3.PyBytes_AsString (Bytes));
   end Initialize;

   ------------
   -- Mutate --
   ------------

   overriding procedure Mutate
     (Self            : in out Python_Text_Storage;
      Storage_Address : in out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Bytes   : constant GNATCOLL.Python.PyObject := Self.Get_Bytes;
      Size    : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        VSS.Unicode.UTF8_Code_Unit_Count
          (VSS.Implementation.Python3.PyBytes_Size (Bytes));
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Size - 1)
        with Import, Address => Storage_Address;

      Manager : VSS.Implementation.Text_Storages.Heap.Heap_Storage :=
        (others => <>)
        with Address => Self'Address;

   begin
      Manager.Initialize
        (Storage_Address,
         Storage,
         Size);
      GNATCOLL.Python.Py_DECREF (Bytes);
   end Mutate;

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference (Self : in out Python_Text_Storage) is
      use type GNATCOLL.Python.PyObject;

      Bytes : constant GNATCOLL.Python.PyObject := Self.Get_Bytes;

   begin
      if Bytes /= null then
         GNATCOLL.Python.Py_INCREF (Bytes);
      end if;
   end Reference;

   ---------------
   -- Set_Bytes --
   ---------------

   procedure Set_Bytes
     (Self  : in out Python_Text_Storage'Class;
      Bytes : GNATCOLL.Python.PyObject)
   is
      package Conversions is
        new System.Address_To_Access_Conversions
          (GNATCOLL.Python.PyObject_Opaque);

   begin
      Self.Pointer :=
        Conversions.To_Address (Conversions.Object_Pointer (Bytes));
   end Set_Bytes;

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference (Self : in out Python_Text_Storage) is
      use type GNATCOLL.Python.PyObject;

      Bytes : constant GNATCOLL.Python.PyObject := Self.Get_Bytes;

   begin
      if Bytes /= null then
         Self.Pointer := System.Null_Address;
         GNATCOLL.Python.Py_DECREF (Bytes);
      end if;
   end Unreference;

end VSS.Implementation.Text_Storages.Python;
