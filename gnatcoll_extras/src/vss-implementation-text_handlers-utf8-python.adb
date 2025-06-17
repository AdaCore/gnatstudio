--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Python3;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8.Python is

   procedure Unsafe_Convert (Text : in out Python_UTF8_Text'Class);
   --  Converts given constant `Python_UTF8_Text` into variable object of
   --  default text type.

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : Python_UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Length + 1,
         UTF8_Offset  =>
           VSS.Unicode.UTF8_Code_Unit_Offset
             (VSS.Implementation.Python3.PyBytes_Size (Self.Bytes)),
         UTF16_Offset => 0);
   end After_Last_Character;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Python_UTF8_Text;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      Unsafe_Convert (Self);

      declare
         pragma Warnings (Off, "overlays smaller object");
         Data : VSS.Implementation.Strings.String_Data
           with Import, Convention => Ada, Address => Self'Address;
         pragma Warnings (On, "overlays smaller object");

      begin
         VSS.Implementation.Strings.Variable_Handler
           (Data).Append (Code, Offset);
      end;
   end Append;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : Python_UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean
   is
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if Position.Index = 0 then
         return False;
      end if;

      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Self.Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen.
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Unicode.UTF8_Code_Unit_Count (Size))
             with Import, Convention => Ada, Address => Pointer.all'Address;

      begin
         Unchecked_Backward (Storage, Position);

         return Position.Index > 0;
      end;
   end Backward;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   overriding procedure Before_First_Character
     (Self     : Python_UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out Python_UTF8_Text;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      Unsafe_Convert (Self);

      declare
         pragma Warnings (Off, "overlays smaller object");
         Data : VSS.Implementation.Strings.String_Data
           with Import, Convention => Ada, Address => Self'Address;
         pragma Warnings (On, "overlays smaller object");

      begin
         VSS.Implementation.Strings.Variable_Handler
           (Data).Delete (From, Size);
      end;
   end Delete;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : Python_UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base
   is
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if Position.Index < 1
        or else Position.Index > Self.Length
      then
         return VSS.Implementation.Strings.No_Character;
      end if;

      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Self.Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen.
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Unicode.UTF8_Code_Unit_Count (Size))
             with Import, Convention => Ada, Address => Pointer.all'Address;

      begin
         return
           VSS.Implementation.UTF8_Encoding.Unchecked_Decode
             (Storage, Position.UTF8_Offset);
      end;
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : Python_UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean
   is
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if Position.Index > Self.Length then
         return False;
      end if;

      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Self.Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen.
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Unicode.UTF8_Code_Unit_Count (Size))
             with Import, Convention => Ada, Address => Pointer.all'Address;

      begin
         Unchecked_Forward (Storage, Position);

         return Position.Index <= Self.Length;
      end;
   end Forward;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out Python_UTF8_Text;
      Item    : Wide_Wide_String;
      Success : out Boolean) is
   begin
      raise Program_Error;
      --  Must never be used
   end From_Wide_Wide_String;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : Python_UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Self.Length;
   end Has_Character;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out Python_UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      Unsafe_Convert (Self);

      declare
         pragma Warnings (Off, "overlays smaller object");
         Data : VSS.Implementation.Strings.String_Data
           with Import, Convention => Ada, Address => Self'Address;
         pragma Warnings (On, "overlays smaller object");

      begin
         VSS.Implementation.Strings.Variable_Handler
           (Data).Insert (From, Item, Offset);
      end;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : Python_UTF8_Text) return Boolean is
      use type VSS.Implementation.Python3.Py_ssize_t;

   begin
      return VSS.Implementation.Python3.PyBytes_Size (Self.Bytes) = 0;
   end Is_Empty;

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference (Self : in out Python_UTF8_Text) is
   begin
      GNATCOLL.Python.Py_INCREF (Self.Bytes);
   end Reference;

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : Python_UTF8_Text;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Self.Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen.
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Unicode.UTF8_Code_Unit_Count (Size))
               with Import, Convention => Ada, Address => Pointer.all'Address;

      begin
         Split_Lines_Common
           (Text            => Self,
            Data            => Data,
            Storage         => Storage,
            Terminators     => Terminators,
            Keep_Terminator => Keep_Terminator,
            Lines           => Lines);
      end;
   end Split_Lines;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : Python_UTF8_Text)
      return Ada.Strings.UTF_Encoding.UTF_8_String
   is
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Self.Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen.
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Unicode.UTF8_Code_Unit_Count (Size))
             with Import, Convention => Ada, Address => Pointer.all'Address;

      begin
         return Result : Ada.Strings.UTF_Encoding.UTF_8_String
                           (1 .. Natural (Size))
         do
            for J in Result'Range loop
               Result (J) :=
                 Standard.Character'Val
                   (Storage (VSS.Unicode.UTF8_Code_Unit_Count (J - 1)));
            end loop;
         end return;
      end;
   end To_UTF_8_String;

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference (Self : in out Python_UTF8_Text) is
   begin
      GNATCOLL.Python.Py_DECREF (Self.Bytes);
   end Unreference;

   --------------------
   -- Unsafe_Convert --
   --------------------

   procedure Unsafe_Convert (Text : in out Python_UTF8_Text'Class) is
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Text.Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen.
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. VSS.Unicode.UTF8_Code_Unit_Count (Size))
             with Import, Convention => Ada, Address => Pointer.all'Address;
         String  : constant Ada.Strings.UTF_Encoding.UTF_8_String
                              (1 .. Integer (Size) + 1)
             with Import, Convention => Ada, Address => Storage'Address;

         Aux     : VSS.Implementation.Strings.String_Data;
         Success : Boolean;

      begin
         VSS.Implementation.Strings.Variable_Handler (Aux).From_UTF_8_String
           (String, Success);

         Text.Unreference;

         declare
            pragma Warnings (Off, "overlays smaller object");
            Overlay : VSS.Implementation.Strings.String_Data
              with Import, Convention => Ada, Address => Text'Address;
            pragma Warnings (On, "overlays smaller object");

         begin
            Overlay.Storage := Aux.Storage;
         end;
      end;
   end Unsafe_Convert;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize
     (Data   : out VSS.Implementation.Strings.String_Data;
      Object : GNATCOLL.Python.PyObject)
   is
      use type GNATCOLL.Python.PyObject;
      use type VSS.Implementation.Python3.Py_ssize_t;

      Bytes   : GNATCOLL.Python.PyObject;
      Pointer : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size    : VSS.Implementation.Python3.Py_ssize_t;

   begin
      if not GNATCOLL.Python.PyUnicode_Check (Object) then
         Data := (others => <>);

         return;
      end if;

      Bytes := VSS.Implementation.Python3.PyUnicode_AsUTF8String (Object);

      if Bytes = null then
         Data := (others => <>);

         return;
      end if;

      if VSS.Implementation.Python3.PyBytes_AsStringAndSize
        (Bytes, Pointer, Size) /= 0
      then
         raise Program_Error;
         --  Should never happen
      end if;

      declare
         Overlay : Python_UTF8_Text :=
           (Size    =>
              VSS.Unicode.UTF8_Code_Unit_Offset
                (VSS.Implementation.Python3.PyBytes_Size (Bytes)),
            Length  =>
              VSS.Implementation.Strings.Character_Count
                (VSS.Implementation.Python3.PyUnicode_GetLength (Object)),
            Storage => VSS.Implementation.Python3.PyBytes_AsString (Bytes),
            Bytes   => Bytes)
           with Address => Data'Address;

      begin
         null;
      end;
   end Unsafe_Initialize;

end VSS.Implementation.Text_Handlers.UTF8.Python;
