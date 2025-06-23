--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  String implementation on top of Python3's UTF8 encoded string as PyBytes
--  object.

with GNATCOLL.Python;

private with VSS.Implementation.Interfaces_C;

package VSS.Implementation.Text_Handlers.UTF8.Python is

   type Python_UTF8_Text is new Abstract_UTF8_Text with private;

   procedure Unsafe_Initialize
     (Data   : out VSS.Implementation.Strings.String_Data;
      Object : GNATCOLL.Python.PyObject);

private

   type Python_UTF8_Text is new Abstract_UTF8_Text with record
      Storage :
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Bytes   : GNATCOLL.Python.PyObject;
   end record with Object_Size => 256;

   overriding procedure Reference (Self : in out Python_UTF8_Text);

   overriding procedure Unreference (Self : in out Python_UTF8_Text);

   overriding function Is_Empty (Self : Python_UTF8_Text) return Boolean;

   --  not overriding procedure Hash
   --    (Self      : Abstract_Text_Handler;
   --     Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator);
   --  --  Compute hash value of the string as little-endian UTF-32 encoded
   --  --  character sequence.

   overriding function Element
     (Self     : Python_UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base;

   overriding function Has_Character
     (Self     : Python_UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure Before_First_Character
     (Self     : Python_UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding procedure After_Last_Character
     (Self     : Python_UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor);

   overriding function Forward
     (Self     : Python_UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean;

   overriding function Backward
     (Self     : Python_UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean;

   overriding procedure From_Wide_Wide_String
     (Self    : in out Python_UTF8_Text;
      Item    : Wide_Wide_String;
      Success : out Boolean);

   overriding function To_UTF_8_String
     (Self : Python_UTF8_Text)
      return Ada.Strings.UTF_Encoding.UTF_8_String;

   overriding procedure Append
     (Self   : in out Python_UTF8_Text;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Insert
     (Self   : in out Python_UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Delete
     (Self : in out Python_UTF8_Text;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset);

   overriding procedure Split_Lines
     (Self            : Python_UTF8_Text;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

end VSS.Implementation.Text_Handlers.UTF8.Python;
