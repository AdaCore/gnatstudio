-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Tags; use Ada.Tags;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Odd.Strings; use Odd.Strings;

with Ada.Text_IO; use Ada.Text_IO;

package body Debugger.Gdb.C is

   use Language;

   Record_Start : Character := '{';
   Record_End   : Character := '}';
   Array_Start  : Character := '{';
   Array_End    : Character := '}';
   Record_Field : String    := "=";
   --  how are record field names separated from their values

   procedure Parse_Array_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Generic_Type_Access);
   --  Parse the description of an array type.
   --  Index should point at the opening character of the array in Type_Str
   --  (ie "int [4]" in gdb).
   --  Start_Of_Dim should point to the beginning of the definition of the
   --  dimensions ("[4]" in the above example)

   procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Generic_Type_Access);
   --  Parse the type describing a record.
   --  Index should pointer after the initial "{".
   --  This function is also used to parse the variant part of a record.
   --  If Is_Union is True, then a union type is created instead of a record
   --  type.

   procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access);
   --  Parse the value of an array.

   ---------------------
   -- Break Exception --
   ---------------------

   function Break_Exception
     (Debugger  : access Gdb_C_Language;
      Name      : String  := "";
      Unhandled : Boolean := False) return String is
   begin
      --  ??? Unsupported, should we raise an exception, so that the menu
      --  can be disabled ?
      return "";
   end Break_Exception;

   ----------------
   -- Parse_Type --
   ----------------

   procedure Parse_Type
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access)
   is
      Tmp  : Natural := Index;
      Save : Natural;
   begin

      --  First: Skip the type itself, to check whether we have in fact an
      --  array or access type.

      if Looking_At (Type_Str, Index, "struct ")
        or else Looking_At (Type_Str, Index, "union ")
      then
         Skip_To_Char (Type_Str, Index, Record_End);
         Index := Index + 1;
      else
         Skip_Word (Type_Str, Index);
      end if;
      Skip_Blanks (Type_Str, Index);

      --  Skip to the right-most access or array definition
      --  For instance, when looking at 'int* [4]' we should detect an array
      --  type, not an access type.
      --  However, we should leave Index at the beginning of a
      --  multi-dimensional array, as in 'int [2][3]'.

      Save := Index;
      while Index <= Type_Str'Last loop
         if Type_Str (Index) = '*' then
            Save := Index;
            Index := Index + 1;
         elsif Type_Str (Index) = '[' then
            if Type_Str (Save) = '*' then
               Save := Index;
            end if;
            Skip_To_Char (Type_Str, Index, ']');
            Index := Index + 1;
         else
            exit;
         end if;
         Skip_Blanks (Type_Str, Index);
      end loop;
      Index := Save;

      --  An access type ?
      if Index <= Type_Str'Last and then Type_Str (Index) = '*' then
         Index := Index + 1;
         Result := New_Access_Type;
         return;
      end if;

      --  An array type ?
      if Index < Type_Str'Last and then Type_Str (Index) = '[' then
         Parse_Array_Type (Lang, Type_Str, Entity, Tmp, Index, Result);
         Index := Tmp;
         return;
      end if;

      --  Else a simple type

      Index := Tmp;

      case Type_Str (Index) is
         when 'e' =>

            --  Enumeration type

            if Looking_At (Type_Str, Index, "enum ") then
               Skip_To_Char (Type_Str, Index, '}');
               Index := Index + 1;
               Result := New_Enum_Type;
               return;
            end if;
            --  Else falls through

         when 's' =>

            --  Structures.
            --  There are two possible cases here:
            --      "struct My_Record { ... }"
            --   or "struct My_Record a"
            --  The second case needs a further ptype to get the real
            --  definition.

            if Looking_At (Type_Str, Index, "struct ") then
               Tmp := Index;
               Index := Index + 7;           --  skips "struct "
               Skip_Word (Type_Str, Index);  --  skips struct name
               Skip_Blanks (Type_Str, Index);
               if Index <= Type_Str'Last
                 and then Type_Str (Index) = Record_Start
               then
                  Index := Index + 1;
                  Parse_Record_Type
                    (Lang, Type_Str, Entity, Index, False, Result);
               else
                  Result := Parse_Type
                    (Get_Debugger (Lang), Type_Str (Tmp .. Index - 1));
               end if;
               return;
            end if;
            --  Else falls through

         when 'u' =>
            if Looking_At (Type_Str, Index, "union ") then
               Tmp := Index;
               Index := Index + 6;           --  skips "union "
               Skip_Word (Type_Str, Index);  --  skips union name
               Skip_Blanks (Type_Str, Index);
               if Index <= Type_Str'Last
                 and then Type_Str (Index) = Record_Start
               then
                  Index := Index + 1;
                  Parse_Record_Type
                    (Lang, Type_Str, Entity, Index, True, Result);
               else
                  Result := Parse_Type
                    (Get_Debugger (Lang), Type_Str (Tmp .. Index - 1));
               end if;
               return;
            end if;
            --  Else falls through

         when others =>
            null;
      end case;

      --  Do we have a simple type ?

      if Is_Simple_Type (Lang, Type_Str (Tmp .. Type_Str'Last)) then
         Result := New_Simple_Type;
         return;
      end if;

--      Index := Tmp;
      raise Unexpected_Type;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Lang       : access Gdb_C_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Values.Generic_Type_Access;
      Repeat_Num : out Positive)
   is
   begin
      Repeat_Num := 1;

      -------------------
      -- Simple values --
      -------------------

      if Result'Tag = Simple_Type'Tag
        or else Result'Tag = Range_Type'Tag
        or else Result'Tag = Mod_Type'Tag
        or else Result'Tag = Enum_Type'Tag
      then
         if Type_Str /= "" then
            declare
               Int : constant Natural := Index;
            begin
               Skip_Simple_Value (Type_Str, Index,
                                  Array_Item_Separator => ',',
                                  End_Of_Array         => Array_End,
                                  Repeat_Item_Start    => '<');
               Set_Value (Simple_Type (Result.all),
                          Type_Str (Int .. Index - 1));

               -------------------
               -- Repeat values --
               -------------------
               --  This only happens inside arrays, so we can simply replace
               --  Result (it will be deleted the next time the item is
               --  parsed anyway).

               if Looking_At (Type_Str, Index, "<repeats ") then
                  Index := Index + 9;
                  Parse_Num (Type_Str,
                             Index,
                             Long_Integer (Repeat_Num));
                  Index := Index + 7;  --  skips " times>"
               end if;

            end;
         else
            Set_Value (Simple_Type (Result.all), "<???>");
         end if;

      -------------------
      -- Access values --
      -------------------
      --  The value looks like:   (access integer) 0xbffff54c
      --  or                  :    0x0

      elsif Result'Tag = Access_Type'Tag then

         --  Skip the parenthesis contents if needed
         if Index <= Type_Str'Last and then Type_Str (Index) = '(' then
            Skip_To_Char (Type_Str, Index, ')');
            Index := Index + 2;
         end if;

         declare
            Int : constant Natural := Index;
         begin
            Skip_Hexa_Digit (Type_Str, Index);
            Set_Value (Simple_Type (Result.all), Type_Str (Int .. Index - 1));
         end;

      -------------------
      -- String values --
      -------------------

      elsif Result'Tag = Array_Type'Tag
        and then Num_Dimensions (Array_Type (Result.all)) = 1
        and then Type_Str'Length /= 0
        and then Type_Str (Index) = '"'
      then
         declare
            Dim : Dimension := Get_Dimensions (Array_Type (Result.all), 1);
            S : String (1 .. Integer (Dim.Last - Dim.First + 1));
            Simple : Simple_Type_Access;

         begin
            Parse_Cst_String (Type_Str, Index, S);
            Simple := Simple_Type_Access (New_Simple_Type);
            Set_Value (Simple.all, S);
            Set_Value (Item       => Array_Type (Result.all),
                       Elem_Value => Simple,
                       Elem_Index => 0);
            Shrink_Values (Array_Type (Result.all));
         end;

      ------------------
      -- Array values --
      ------------------

      elsif Result'Tag = Array_Type'Tag
        and then Type_Str'Length /= 0   --  for empty Arrays
      then
         --  Some array types can in fact be transformed into access types.
         --  This is the case for instance in C for empty arrays ("int[0]" can
         --  have a value of "0x..."), or in Ada for unconstrained arrays
         --  ("array (1..1) of string" can have a value of "(0x0").
         --  For such cases, we change the type once and for all, since we will
         --  never need to go back to an array type.

         if Type_Str (Index) /= Array_Start then
            Free (Result);
            Result := New_Access_Type;
            Parse_Value (Lang, Type_Str, Index, Result, Repeat_Num);
         else
            Parse_Array_Value
              (Lang, Type_Str, Index, Array_Type_Access (Result));
         end if;

      -------------------
      -- Record values --
      -------------------

      elsif Result'Tag = Record_Type'Tag
        or else Result'Tag = Union_Type'Tag
      then
         declare
            R : Record_Type_Access := Record_Type_Access (Result);
            Int : Natural;
         begin

            --  Skip initial Record_Start if we are still looking at it (we
            --  might not if we are parsing a variant part)

            if Index <= Type_Str'Last
              and then Type_Str (Index) = Record_Start
            then
               Index := Index + 1;
            end if;

            for J in 1 .. Num_Fields (R.all) loop

               --  If we are expecting a field

               if Get_Variant_Parts (R.all, J) = 0 then
                  declare
                     V          : Generic_Type_Access := Get_Value (R.all, J);
                     Repeat_Num : Positive;
                  begin
                     --  Skips '='
                     --  This also skips the address part in some "in out"
                     --  parameters, like:
                     --    (<ref> gnat.expect.process_descriptor) @0x818a990: (
                     --     pid => 2012, ...

                     Skip_To_Char
                       (Type_Str, Index, Record_Field (Record_Field'First));
                     Index := Index + 1 + Record_Field'Length;
                     Parse_Value (Lang, Type_Str, Index, V, Repeat_Num);
                  end;

               --  Else we have a variant part record

               else

                  if Type_Str (Index) = ',' then
                     Index := Index + 1;
                     Skip_Blanks (Type_Str, Index);
                  end if;

                  --  Find which part is active
                  --  We simply get the next field name and search for the
                  --  part that defines it
                  Int := Index;
                  Skip_To_Char (Type_Str, Int, ' ');

                  declare
                     Repeat_Num : Positive;
                     V : Generic_Type_Access;
                  begin
                     V := Find_Variant_Part
                       (Item     => R.all,
                        Field    => J,
                        Contains => Type_Str (Index .. Int - 1));

                     Parse_Value (Lang, Type_Str, Index, V, Repeat_Num);
                  end;
               end if;
            end loop;
         end;

         Skip_Blanks (Type_Str, Index);

         --  Skip closing Record_End, if seen
         if Index <= Type_Str'Last
           and then Type_Str (Index) = Record_End
         then
            Index := Index + 1;
         end if;

      ------------------
      -- Class values --
      ------------------

      elsif Result'Tag = Class_Type'Tag then
         declare
            R : Generic_Type_Access;
         begin
            for A in 1 .. Get_Num_Ancestors (Class_Type (Result.all)) loop
               R := Get_Ancestor (Class_Type (Result.all), A);
               Parse_Value (Lang, Type_Str, Index, R, Repeat_Num);
            end loop;
            R := Get_Child (Class_Type (Result.all));
            Parse_Value (Lang, Type_Str, Index, R, Repeat_Num);
         end;
      end if;
   end Parse_Value;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   procedure Parse_Array_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Generic_Type_Access)
   is
      Num_Dim   : Integer := 0;
      Initial   : Natural := Index;
      Tmp_Index : Natural;
      R         : Array_Type_Access;
      Last      : Long_Integer;
      Item_Type : Generic_Type_Access;
   begin

      --  Find the number of dimensions
      Index := Start_Of_Dim;
      Tmp_Index := Index;
      while Tmp_Index <= Type_Str'Last
        and then Type_Str (Tmp_Index) = '['
      loop
         Num_Dim := Num_Dim + 1;
         Skip_To_Char (Type_Str, Tmp_Index, ']');
         Tmp_Index := Tmp_Index + 1;
      end loop;

      --  Create the type

      Result := New_Array_Type (Num_Dimensions => Num_Dim);
      R := Array_Type_Access (Result);

      --  Then parse the dimensions.
      Num_Dim := 0;
      while Index <= Type_Str'Last
        and then Type_Str (Index) = '['
      loop
         Num_Dim := Num_Dim + 1;
         Index := Index + 1;
         Parse_Num (Type_Str, Index, Last);
         Set_Dimensions (R.all, Num_Dim, (0, Last - 1));
         Index := Index + 1;
      end loop;

      --  Finally parse the type of items

      Parse_Type (Lang,
                  Type_Str (Initial .. Start_Of_Dim - 1),
                  Array_Item_Name (Lang, Entity, "0"),
                  Initial,
                  Item_Type);
      Set_Item_Type (R.all, Item_Type);
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Generic_Type_Access)
   is
      Num_Fields : Natural := 0;
      Field      : Natural := 1;
      Initial    : constant Natural := Index;
      R          : Record_Type_Access;
      Field_Value : Generic_Type_Access;
      Tmp,
      Save       : Natural;
   begin
      --  Count the number of fields

      while Index <= Type_Str'Last
        and then Type_Str (Index) /= '}'
      loop
         if Type_Str (Index) = ';' then
            Num_Fields := Num_Fields + 1;
         end if;
         Index := Index + 1;
      end loop;

      --  Create the type

      if Is_Union then
         Result := New_Union_Type (Num_Fields);
      else
         Result := New_Record_Type (Num_Fields);
      end if;
      R := Record_Type_Access (Result);

      --  Parse the type

      Index := Initial;
      while Field <= Num_Fields loop
         Skip_Blanks (Type_Str, Index);

         --  Get the field name (last word before ;)
         Tmp := Index;
         Skip_To_Char (Type_Str, Index, ';');
         Save := Index;
         Index := Index - 1;
         Skip_Word (Type_Str, Index, Step => -1);
         Set_Field_Name (R.all, Field, Type_Str (Index + 1 .. Save - 1),
                         Variant_Parts => 0);
         Parse_Type
           (Lang, Type_Str (Tmp .. Index),
            Record_Field_Name (Lang, Entity, Type_Str (Index + 1 .. Save - 1)),
            Tmp,
            Field_Value);
         Set_Value (R.all, Field_Value, Field);
         Index := Save + 1;
         Field := Field + 1;
      end loop;
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
      Dim     : Natural := 0;            --  current dimension
      Current_Index : Long_Integer := 0; --  Current index in the parsed array

      procedure Parse_Item;
      --  Parse the value of a single item, and add it to the contents of
      --  Result.

      ----------------
      -- Parse_Item --
      ----------------

      procedure Parse_Item is
         Int        : Natural := Index;
         Tmp        : Generic_Type_Access;
         Repeat_Num : Integer;
      begin
         --  Parse the next item
         Tmp := Clone (Get_Item_Type (Result.all).all);
         Parse_Value (Lang, Type_Str, Index, Tmp, Repeat_Num);
         Set_Value (Item       => Result.all,
                    Elem_Value => Tmp,
                    Elem_Index => Current_Index,
                    Repeat_Num => Repeat_Num);
         Current_Index := Current_Index + Long_Integer (Repeat_Num);
      end Parse_Item;

   begin
      loop
         case Type_Str (Index) is
            when '}' =>
               Dim := Dim - 1;
               Index := Index + 1;

            when '{' =>
               --  A parenthesis is either the start of a sub-array (for
               --  other dimensions, or one of the items in case it is a
               --  record or an array. The distinction can be made by
               --  looking at the current dimension being parsed.

               if Dim = Num_Dimensions (Result.all) then
                  Parse_Item;
               else
                  Dim := Dim + 1;
                  Index := Index + 1;
               end if;

            when ',' | ' ' =>
               Index := Index + 1;

            when others =>
               Parse_Item;
         end case;
         exit when Dim = 0;
      end loop;

      --  Shrink the table of values.
      Shrink_Values (Result.all);
   end Parse_Array_Value;

   -----------------
   -- Thread_List --
   -----------------

   function Thread_List (Lang : access Gdb_C_Language) return String is
   begin
      --  ??? Unsupported, should we raise an exception ?
      return "";
   end Thread_List;

   -------------------
   -- Thread_Switch --
   -------------------

   function Thread_Switch
     (Lang   : access Gdb_C_Language;
      Thread : Natural) return String is
   begin
      --  ??? Unsupported, should we raise an exception ?
      return "";
   end Thread_Switch;

   -----------------------
   -- Parse_Thread_List --
   -----------------------

   function Parse_Thread_List
     (Lang   : access Gdb_C_Language;
      Output : String) return Thread_Information_Array
   is
      Result      : Thread_Information_Array (1 .. 0);
   begin
      --  ??? Unsupported, should we raise an exception ?
      return Result;
   end Parse_Thread_List;

end Debugger.Gdb.C;
