------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with System.Tasking;
pragma Warnings (On);

with Parse_Controlled;
with Gen_Cplx;
with Ada.Strings.Unbounded;

procedure Parse is
   pragma Warnings (Off);

   PROCEDURE Foo is begin null; end Foo;
   function Foo2 return Integer is begin return 10; end Foo2;

   A  : aliased Integer := 1; -- End of line comment
   B  : Float   := 2.0; --  End of line comment (should be highlighted)
   C  : Character := 'A';
   Sh : Short_Integer := 65;
   SSh : Short_Short_Integer := 65;
   S  : String  := "abcd";
   S2 : String  := "ab" & ASCII.LF & "c";
   S3 : String  := "ab[""c";
   S4 : String  := "ab[""c""]";
   Dur : Duration := 0.5;

   C2 : Character := Character'('A');

   type My_Range is range 3 .. 6;
   R : My_Range := 5;

   type My_Mod is mod 10;
   M : My_Mod := 8;

   type Access_Type is access all Integer;
   Act : Access_Type := A'Access;

   type My_Enum is (Blue, Red, My_Green);
   --  R914-023: My_Green should be renamed back to Green
   My_Enum_Variable : My_Enum := Blue;

   type Integer_Array is array (1 .. 4) of Integer;
   T : aliased Integer_Array := (2, 3, 4, 5);
   --  Printed as (2, 3, 4, 5) in gdb

   type Empty_Array is array (1 .. 0) of Integer;
   Ea : Empty_Array;

   type Empty_Array2 is array (Integer range <>) of Integer;
   Ea2 : Empty_Array2 (0 .. -1);

   type Array_Of_Access is array (4 .. 6) of Access_Type;
   Aoa : Array_Of_Access := (A'Access, null, A'Access);

   type First_Index_Integer_Array is array (24 .. 26) of Integer;
   Fiia : First_Index_Integer_Array := (24 => 3, 25 => 4, 26 => 5);
   --  Printed as (24 => 3, 4, 5) in gdb

   type Integer_Array_Access is access all Integer_Array;
   Iaa : Integer_Array_Access := T'Access;

   type Integer_Array2 is array (1 .. 2, 1 .. 3) of Integer;
   U : Integer_Array2 := ((2, 3, 4), (5, 6, 7));

   type Array_3d is array (3 .. 4, 1 .. 2, 6 .. 7) of Integer;
   A3d : Array_3d := (others => (others => (1, 2)));

   type Enum_Array is array (My_Enum'Range) of My_Enum;
   Enum_Array_Variable : Enum_Array := (Blue     => Red,
                                        Red      => My_Green,
                                        My_Green => Blue);

   type Enum_Range is new My_Enum range Blue .. Red;
   type Enum_Range_Matrix is
     array (Enum_Range, Enum_Range) of Integer;
   Erm : Enum_Range_Matrix := (others => (others => 0));

   type Negative_Array is array (-50 .. -46) of Character;
   Negative_Array_Variable : Negative_Array := ('A', 'B', 'C', 'D', 'E');

   type Array_Of_Array is array (1 .. 2) of First_Index_Integer_Array;
   Aa : Array_Of_Array := (1 => (24 => 3, 25 => 4, 26 => 5),
                           2 => (6, 7, 8));

   type String_Access is access String;
   type Array_Of_String is array (1 .. 2) of String_Access;
   Aos : Array_Of_String := (new String'("ab"), new String'("cd"));

   S5 : String_Access := new String'("Hello");
   S6 : String_Access := S5;

   type Null_Record is null record;
   Nr : Null_Record;

   type My_Record is record
      Field1 : Access_Type;
      Field2 : String (1 .. 2);
   end record;
   V : aliased My_Record := (Field1 => A'Access, Field2 => "ab");

   type Complex_Repeat_Type is array (1 .. 100) of My_Record;
   Crt : Complex_Repeat_Type := (others => V);

   type My_Record_Access is access all My_Record;
   Mra : My_Record_Access := V'Access;

   type My_Record_Array is array (Natural range <>) of My_Record;
   W : My_Record_Array := ((Field1 => A'Access, Field2 => "ab"),
                           (Field1 => A'Access, Field2 => "rt"));

   type Record_Of_Record is record
      caseField1 : Access_Type;  --  Trap: start with keyword 'case'
      Field2 : My_Record;
   end record;
   Rr : Record_Of_Record := (caseField1 => Act, Field2 => V);

   type Record_Of_Array is record
      Field1 : Integer_Array;
      Field2 : Integer_Array;
      Field3 : Integer;
   end record;
   Roa : Record_Of_Array := (T, T, 1234);

   type Integer_Array3 is array (1 .. 10, 1 .. 20) of Integer;
   X : Integer_Array3 := ((1, 2, others => 0),
                          others => (others => 0));

   type Array_Of_Record is array (1 .. 2) of My_Record;
   Ar : Array_Of_Record := (1 => (Field1 => A'Access, Field2 => "ab"),
                            2 => (Field1 => A'Access, Field2 => "cd"));

   type Discriminants_Record (A : Integer; B : Boolean) is record
      C : Float;
   end record;
   Z : Discriminants_Record := (A => 1, B => False, C => 2.0);

   type Access_Subprogram is access procedure;
   As : Access_Subprogram := Foo'Access;

   type Variable_Record (A : Boolean := True) is record
      case A is
         when True =>
            B : Integer;
         when False =>
            C : Float;
            D : Integer;
      end case;
   end record;
   Y  : Variable_Record := (A => True, B => 1);
   Y2 : Variable_Record := (A => False, C => 1.0, D => 2);
   Nv : Parse_Controlled.Null_Variant;

   type Union_Type (A : Boolean := False) is record
      case A is
         when True  => B : Integer;
         when False => C : Float;
      end case;
   end record;
   pragma Unchecked_Union (Union_Type);
   Ut : Union_Type := (A => True, B => 3);

   --  A big array of access types
   type Big_Array is array (1 .. 1000) of Access_Type;
   Ba : Big_Array := (4 => A'Access, 900 => A'Access, others => null);

   type Big_Array2 is array (1 .. 1000) of Integer;
   Ba2 : Big_Array2 := (4 => 1, 900 => 2, others => 0);

   type Tagged_Type is tagged record
      A : Integer;
      B : Character;
   end record;
   Tt : Tagged_Type := (A => 2, B => 'C');

   type Child_Tagged_Type is new Tagged_Type with record
      C : Float;
   end record;
   Ctt : Child_Tagged_Type := (Tt with C => 4.5);

   type Child_Tagged_Type2 is new Tagged_Type with null record;
   Ctt2 : Child_Tagged_Type2 := (Tt with null record);

   type Child_Tagged_Type3 is new Child_Tagged_Type2 with record
      D : Integer;
   end record;
   type Child_Tagged_Type4 is new Child_Tagged_Type3 with record
      E : Character;
   end record;
   type Child_Tagged_Type5 is new Child_Tagged_Type4 with record
      F : Integer;
   end record;
   Ctt5 : Child_Tagged_Type5 := (Tt with D => 3, E => 'A', F => 2);

   --  Very long identifier

   Very_Loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooog_Num : Integer := 0;

   --   Parsing (null) access types

   type String_Access_Array is array (1 .. 3) of String_Access;
   Str_Access_Array : String_Access_Array := (others => null);

   --  Check that backslashes are correctly displayed in Ada mode.
   RegExp : String := "(\w)=(\w)";

   --  Arrays whose dimensions are not numbers
   type Discriminant_Record (Num1, Num2,
                             Num3, Num4 : Natural) is record
      Field1 : My_Record_Array (1 .. Num2);
      Field2 : My_Record_Array (Num1 .. 10);
      Field3 : My_Record_Array (Num1 .. Num2);
      Field4 : My_Record_Array (Num3 .. Num2);
      Field5 : My_Record_Array (Num4 .. Num2);
   end record;
   Dire : Discriminant_Record (1, 7, 3, 0);

   --  Variant parts (1.25 in testsuite)
   type Null_Variant_Part (Discr : Integer) is record
      case Discr is
         when 1 => Var_1 : Integer;
         when 2 => Var_2 : Boolean;
         when others => null;
      end case;
   end record;
   Nvp : Null_Variant_Part (3);

   --  Parsing unconstrained arrays (implied bounds, gdb reports 0 as first
   --  bound)
   type T_Type is array (Positive range <>) of Integer;
   type T_Ptr_Type is access T_Type;

   T_Ptr : T_Ptr_Type := new T_Type' (13, 17);
   T_Ptr2 : T_Ptr_Type := new T_Type' (2 => 13, 3 => 17);
   Null_Ptr : T_Ptr_Type;

   type Point is record X, Y : Integer; end record;
   type Record_Array is array (1 .. 10) of Point;
   Ra : Record_Array := (others => (X => 0, Y => 0));

   function Foos return String is
   begin
      return "string";
   end Foos;

   My_Str : String := Foos;

   --  CA01-002
   Final_Result3 : String (1 .. 20) := "The result is:      ";
   Final_Result : String (1 .. 20);
   for Final_Result'Address use Final_Result3'Address;
   Final_Result2 : String (1 .. 20) := "The result is:      ";

   --  D319-003

   type Attribute_Type is (New_Attr, Old_Attr);

   type Channel_Type is (Channel_1, Channel_2);
   for Channel_Type'Size use 8;
   for Channel_Type use (Channel_1 => 1,Channel_2 => 2);

   type Object2;
   type Pointer is access all Object2;

   type Object2 is record
      Map_Id           : Integer := 0;
      Map_Pos_Lat      : Float := 0.0;
      Map_Pos_Lon      : Float := 0.0;
      Map_Zoom         : Float := 1.0;
   end record;

   type Attribute_Arr is array (Attribute_Type, Channel_Type) of Pointer;

   type Object is record
      Attributes : Attribute_Arr;
   end record;

   This : Object := (Attributes => (others => (others => new Object2)));

   --  D525-012

   type Font_Type is (Small, Large);
   type Color_Type is (Cyan, Red, Yellow, Green, Magenta, Amber, White);

   type Screen_Element_Type is record
      Char          : Character := ' ';
      Color         : Color_Type := Cyan;
      Font          : Font_Type := Small;
      Reverse_Video : Boolean := True;
   end record;

   Max_Columns : constant := 24;
   Max_Lines : constant := 14;
   type Column_Number_Type is range 1 .. Max_Columns;
   type Line_Number_Type is range 1 .. Max_Lines;

   type Line_Buffer_Type is
     array (Column_Number_Type) of Screen_Element_Type;

   type Screen_Image_Type_0 is
     array (Line_Number_Type) of Line_Buffer_Type;

   type Screen_Image_Type is record
      New_Screen_Image : Screen_Image_Type_0;
   end record;

   Scr : Screen_Image_Type;

   --  Test 9305-014
   procedure TN_9305_014 is
      type String_Access is access all String;
      type Argument_List is array (Positive range <>) of String_Access;
      type Argument_List2 is array (Positive range <>,
                                    Positive range <>) of String_Access;
      Gcc_Switches : Integer := 3;
   begin
      Block1 :
      declare
         Args : Argument_List (1 .. Gcc_Switches);
         Args2 : Argument_List2 (1 .. Gcc_Switches, 1 .. Gcc_Switches);
      begin
         raise Constraint_Error;
      end Block1;
   exception
      when others => null;
   end TN_9305_014;

   --  F223-004

   type Choice_Type is (Apple,Peach);

   type Fruit (Choice :Choice_Type:=Peach) is record
      case Choice is
        when Peach =>
         ABC : Integer := 10;
        when Apple =>
         YXZ : Integer := 1000;
      end case;
   end record;

   type Fruits_Array_Type is array (Choice_Type) of Fruit;

   Fruits_Init : constant Fruits_Array_Type :=
     (Apple=>(Choice => Apple, YXZ=> 20),
      Peach=>(Choice => Peach, ABC=> 2000));

   More_Fruits : Fruits_Array_Type := Fruits_Init;

   --  Complex type
   Tcb : System.Tasking.Ada_Task_Control_Block (2);

   procedure Bar (A : in out Integer;
                  T : in out Tagged_Type;
                  R : in out My_Record;
                  S : String)
   is
      --  Array of Controlled types
      --  For some reason, this must be in a separate context than Parse, or
      --  test_parse.adb can not execute.
      type Matrix_Map is array (1 .. 2, 1 .. 1) of
        Ada.Strings.Unbounded.Unbounded_String;
      Ustring : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("not_set");

      Asu_Test : Matrix_Map := (others => (others => Ustring));

      type Matrix_Map_Instance is tagged record
         Map : Matrix_Map := (others => (others => Ustring));
      end record;
      Asu_Test2 : Matrix_Map_Instance;

      type Matrix_Map_Instance2 is record
         Map : Matrix_Map := (others => (others => Ustring));
      end record;
      Asu_Test3 : Matrix_Map_Instance2;

   begin
      A := 4;
      A := A + 1;
      raise Constraint_Error;
   exception
      when others => null;
   end Bar;

   --  G328-021

   type Value_Var_Type is ( V_Null, V_Boolean, V_Integer );
   --  Use a default value for the discriminant, so that the compiler
   --  never optimizes the debug info for NBI_* below (otherwise, since
   --  the discrimant cannot change, on some targets the compiler only
   --  lists this as a standard record with two fields (on taff).
   type Value_Type( Var : Value_Var_Type := V_Null ) is
      record
         case Var is
            when V_Null =>
               null;
            when V_Boolean =>
               Boolean_Value : Boolean;
            when V_Integer =>
               Integer_Value : Integer;
         end case;
      end record;
   NBI_N : Value_Type := (Var => V_Null);
   NBI_I : Value_Type := (Var => V_Integer, Integer_Value => 18);
   NBI_B : Value_Type := (Var => V_Boolean, Boolean_Value => True);

   type Access_Procedure is access
      procedure (I1 : Integer; I2 : Integer);
   procedure Proc (I1 : Integer; I2 : Integer) is
   begin
      null;
   end Proc;
   AP : Access_Procedure := Proc'Access;

   type Access_Function is access
     function (I : Integer) return Integer;
   function Func (I  : Integer) return Integer is
   begin
      return I;
   end Func;
   AF : Access_Function := Func'Access;

   type Record_Access_Function is record
      I  : Integer;
      AF : Access_Function;
      I2 : Integer;
   end record;
   RAF : Record_Access_Function :=
           (I   => 0,
            AF  => Func'Access,
            I2  => 0);

   -- JC21-017

   procedure Swap (Word : in out String) is
   begin
      raise Constraint_Error;
   end Swap;

   Name : String (1 .. 7);

begin
   pragma Assert (A = 1);

   TN_9305_014;
   Bar (A, Tt, V, "Hello world");
   Name := "qeaLfjb";
   Swap (Name);
end Parse;
