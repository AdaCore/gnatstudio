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

with Odd.Strings;    use Odd.Strings;
with Generic_Values; use Generic_Values;
with System;         use System;
with GNAT.IO;        use GNAT.IO;
with GNAT.Regpat;    use GNAT.Regpat;
with GNAT.Expect;    use GNAT.Expect;
with GNAT.OS_Lib;    use GNAT.OS_Lib;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Tags;       use Ada.Tags;

package body Debugger.Gdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher := Compile ("\(gdb\) ");
   --  Regular expressions used to recognize the prompt.

   Unexpected_Type : exception;

   procedure Parse_Type_Gdb_Ada_From_String
     (Debugger : in Gdb_Debugger;
      Type_Str : in String;
      Entity   : in String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access);
   --  Parse the type of Entity.
   --  Type_Str should contain the type as returned by the debugger.
   --  Entity is used to get the type of the fields or array items.

   procedure Parse_Type_Gdb_C_From_String
     (Debugger : in Gdb_Debugger;
      Type_Str : in String;
      Entity   : in String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access);
   --  Same as above, but for the C language.

   procedure Parse_Value_Gdb_Ada_From_String
     (Debugger  : in Gdb_Debugger;
      Type_Str  : in String;
      Index     : in out Natural;
      Result    : in out Generic_Values.Generic_Type_Access);
   --  Parse the value of an entity, for the Ada language.
   --  Type_Str should contain the value, as returned by the debugger itself.

   procedure Parse_Array_Type_Ada (Debugger  : in Gdb_Debugger;
                                   Type_Str  : in String;
                                   Entity    : in String;
                                   Index     : in out Natural;
                                   Result    : out Generic_Type_Access);
   --  Parse the description of an array type.
   --  Index should point at the opening character of the array in Type_Str
   --  (ie "array " in gdb).

   procedure Parse_Record_Type_Ada (Debugger  : in Gdb_Debugger;
                                    Type_Str  : in String;
                                    Entity    : in String;
                                    Index     : in out Natural;
                                    Result    : out Generic_Type_Access;
                                    End_On    : in String);
   --  Parse the type describing a record.
   --  Index should pointer after the initial "record ", and the record is
   --  assumed to end on a string like End_On.
   --  This function is also used to parse the variant part of a record.

   function Is_Simple_Ada_Type (Str : String) return Boolean;
   --  Return True if Str is a simple Ada type, like integer, ...

   pragma Inline (Is_Simple_Ada_Type);


   generic
      Array_Item_Separator : Character := ',';
      --  Item separate in an array value (ie "5, 2, 3")
      End_Of_Array         : Character := ')';
      --  End of array value (ie "((1, 2), (3, 4))")
      Repeat_Item_Start    : Character := '<';
      --  Start of repeat statements (ie "<repeats .. times>")
   procedure Skip_Simple_Value (Type_Str : in String;
                                Index    : in out Natural);
   --  Skip the value of a simple value ("65 'A'" for instance).
   --  This stops at the first special character.


   procedure Parse_Array_Value (Debugger : in Gdb_Debugger;
                                Type_Str : in String;
                                Index    : in out Natural;
                                Result   : in out Array_Type_Access);
   --  Parse the value of an array.

   --------------------------
   -- Parse_Array_Type_Ada --
   --------------------------

   procedure Parse_Array_Type_Ada (Debugger  : in Gdb_Debugger;
                                   Type_Str  : in String;
                                   Entity    : in String;
                                   Index     : in out Natural;
                                   Result    : out Generic_Type_Access)
   is
      Item_Separator : constant Character := ',';
      Dimension_End  : constant Character := ')';
      Num_Dim   : Integer := 1;
      I         : Natural := Index;
      R         : Array_Type_Access;
      Index_Str : Unbounded_String;
   begin

      --  As a special case, if we have (<>) for the dimensions (ie an
      --  unconstrained array), this is treated as an access type and not an
      --  array type).

      if Looking_At (Type_Str, I, "array (<>)") then
         Result := new Access_Type;
         return;
      end if;

      --  First, find the number of dimensions

      while I <= Type_Str'Last
        and then Type_Str (I) /= Dimension_End
      loop
         if Type_Str (I) = Item_Separator then
            Num_Dim := Num_Dim + 1;
         end if;
         I := I + 1;
      end loop;

      --  Create the type

      R := new Array_Type (Num_Dimensions => Num_Dim);

      --  Then parse the dimensions.

      Num_Dim := 1;
      Index := Index + 7;

      while Num_Dim <= R.Num_Dimensions loop
         Parse_Num (Type_Str, Index, R.Dimensions (Num_Dim).First);
         Index := Index + 4;  --  skips ' .. '
         Parse_Num (Type_Str, Index, R.Dimensions (Num_Dim).Last);
         Num_Dim := Num_Dim + 1;
         Index := Index + 2;  --  skips ', ' or ') '
      end loop;

      --  Skip the type of the items

      Index := Index + 3; --  skips 'of '
      I := Index;
      Skip_To_Char (Type_Str, Index, ' ');

      --  If we have a simple type, no need to ask gdb, for efficiency reasons.

      if Is_Simple_Ada_Type (Type_Str (I .. Index - 1)) then
         R.Item_Type := new Simple_Type;

      else

         --  Get the type of the items.
         --  Note that we can not simply do a "ptype" on the string we read
         --  after "of ", since we might not be in the right context, for
         --  instance if Entity is something like "Foo::entity".
         --  Thus, we have to do a "ptype" directly on the first item of the
         --  array.

         for J in 1 .. R.Num_Dimensions loop
            Append (Index_Str, Long_Integer'Image (R.Dimensions (J).First));
            if J /= R.Num_Dimensions then
               Append (Index_Str, ",");
            end if;
         end loop;

         R.Item_Type := Parse_Type
           (Debugger, Entity & "(" & To_String (Index_Str) & ")");
      end if;

      Result := Generic_Type_Access (R);
   end Parse_Array_Type_Ada;

   ------------------------
   -- Is_Simple_Ada_Type --
   ------------------------

   function Is_Simple_Ada_Type (Str : String) return Boolean is
   begin
      return Str = "boolean"
        or else Str = "integer"
        or else Str = "natural"
        or else Str = "character";
   end Is_Simple_Ada_Type;

   ---------------------------
   -- Parse_Record_Type_Ada --
   ---------------------------

   procedure Parse_Record_Type_Ada (Debugger  : in Gdb_Debugger;
                                    Type_Str  : in String;
                                    Entity    : in String;
                                    Index     : in out Natural;
                                    Result    : out Generic_Type_Access;
                                    End_On    : in String)
   is
      I : Natural;
      Num_Fields : Natural := 0;
      R : Record_Type_Access;
      Num_Parts  : Natural := 0;
   begin
      Skip_Blanks (Type_Str, Index);
      I := Index;

      --  Count the number of fields

      while not Looking_At (Type_Str, I, End_On) loop

         --  A null field ? Do no increment the count
         if Looking_At (Type_Str, I, "null;") then
            I := I + 5;

         --  A record with a variant part ? This counts as
         --  only one field

         elsif Looking_At (Type_Str, I, "case ") then
            I := I + 5;
            Skip_To_String (Type_Str, I, "end case;");
            Num_Fields := Num_Fields + 1;
            I := I + 9;

         --  Else a standard field

         else
            Skip_To_Char (Type_Str, I, ';');
            I := I + 1;
            Num_Fields := Num_Fields + 1;
         end if;

         Skip_Blanks (Type_Str, I);
      end loop;

      R := new Record_Type (Num_Fields);

      --  Now parse all the fields

      Num_Fields := 1;

      while Num_Fields <= R.Num_Fields loop

         if Looking_At (Type_Str, Index, "null;") then
            Index := Index + 5;

         elsif Looking_At (Type_Str, Index, "case ") then
            Index := Index + 5;
            I := Index;

            Skip_To_Char (Type_Str, Index, ' ');

            R.Fields (Num_Fields).Name
              := new String'(Type_Str (I .. Index - 1));
            R.Fields (Num_Fields).Value := null;

            --  Count the number of alternatives in the variant part.

            I := Index;
            while not Looking_At (Type_Str, I, "end case") loop
               if Type_Str (I .. I + 1) = "=>" then
                  Num_Parts := Num_Parts + 1;
               end if;
               I := I + 1;
            end loop;

            R.Fields (Num_Fields).Variant_Part
              := new  Record_Type_Array (1 .. Num_Parts);

            --  Parses the parts, and create a record for each

            Num_Parts := 0;
            while not Looking_At (Type_Str, Index, "end ") loop
               Skip_To_String (Type_Str, Index, "=>");

               Index := Index + 2;
               Num_Parts := Num_Parts + 1;

               declare
                  Part : Generic_Type_Access;
               begin
                  if Num_Parts = R.Fields (Num_Fields).Variant_Part'Last then
                     Parse_Record_Type_Ada (Debugger, Type_Str, Entity,
                                            Index, Part, "end case");
                  else
                     Parse_Record_Type_Ada (Debugger, Type_Str, Entity,
                                            Index, Part, "when ");
                  end if;
                  R.Fields (Num_Fields).Variant_Part (Num_Parts)
                    := Record_Type_Access (Part);
               end;

               Skip_Blanks (Type_Str, Index);
            end loop;

            Index := Index + 9;
            Num_Fields := Num_Fields + 1;

         --  Else a standard field

         else

            --  Get the name of the field

            I := Index;
            Skip_To_Char (Type_Str, Index, ':');
            R.Fields (Num_Fields).Name
              := new String'(Type_Str (I .. Index - 1));

            --  Get the type of the field
            Index := Index + 2;
            I := Index;
            Skip_To_Char (Type_Str, Index, ';');

            --  If we have a simple type, no need to ask gdb, for efficiency
            --  reasons.

            if Is_Simple_Ada_Type (Type_Str (I .. Index - 1)) then
               R.Fields (Num_Fields).Value := new Simple_Type;
            else
               R.Fields (Num_Fields).Value := Parse_Type
                 (Debugger, Entity & "." & R.Fields (Num_Fields).Name.all);
            end if;
            Index := Index + 1;
            Num_Fields := Num_Fields + 1;
         end if;

         Skip_Blanks (Type_Str, Index);
      end loop;

      Result := Generic_Type_Access (R);
   end Parse_Record_Type_Ada;

   -----------------------
   -- Skip_Simple_Value --
   -----------------------

   procedure Skip_Simple_Value
     (Type_Str : in String;
      Index    : in out Natural) is
   begin
      while Index <= Type_Str'Last
        and then Type_Str (Index) /= Array_Item_Separator
        and then Type_Str (Index) /= End_Of_Array
        and then Type_Str (Index) /= ASCII.LF --  always the end of a field
        and then Type_Str (Index) /= Repeat_Item_Start
      loop
         Index := Index + 1;
      end loop;
   end Skip_Simple_Value;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   procedure Parse_Array_Value (Debugger : in Gdb_Debugger;
                                Type_Str : in String;
                                Index    : in out Natural;
                                Result   : in out Array_Type_Access)
   is
      Dim     : Natural := 0;            --  current dimension
      Value_Index : Positive := 1;       --  Current index in Arr.Values
      Item    : Array_Item_Array_Access;
      Current_Index : Long_Integer := 0; --  Current index in the parsed array

      procedure Parse_Item;
      --  Parse the value of a single item, and add it to the contents of
      --  Result.

      ----------------
      -- Parse_Item --
      ----------------

      procedure Parse_Item is
         Int : Natural := Index;
      begin
         --  If we haven't allocated enough items, resize the array.

         if Value_Index > Result.Values'Last then
            Item := Result.Values;
            Result.Values := new Array_Item_Array
              (1 .. 2 * Result.Values'Last);
            Result.Values (1 .. Item'Last) := Item.all;
            Free (Item);
         end if;

         --  Does gdb indicate the number of the item (as in '24 =>')
         --  We search the first character that does not belong to the item
         --  value.

         while Int <= Type_Str'Last
           and then Type_Str (Int) /= ','  --  item separator
           and then Type_Str (Int) /= ')'  --  end of sub-array
           and then Type_Str (Int) /= '('  --  start of sub-array
           and then Type_Str (Int) /= '='  --  index of the item
         loop
            Int := Int + 1;
         end loop;

         if Type_Str (Int) = '=' then
            Index := Int + 3;  --  skip "field_name => ";
         end if;

         --  Parse the next item
         Result.Values (Value_Index).Index := Current_Index;
         Result.Values (Value_Index).Value := Clone (Result.Item_Type.all);
         Parse_Value_Gdb_Ada_From_String
           (Debugger, Type_Str, Index, Result.Values (Value_Index).Value);

         if Result.Values (Value_Index).Value'Tag = Repeat_Type'Tag then
            Current_Index := Current_Index + Long_Integer (Repeat_Type_Access
              (Result.Values (Value_Index).Value).Repeat_Num);
         else
            Current_Index := Current_Index + 1;
         end if;

         Value_Index := Value_Index + 1;
      end Parse_Item;

   begin
      --  Allocate some items in advance, just in case.
      Result.Values := new Array_Item_Array (1 .. 100);

      loop
         case Type_Str (Index) is

            when ')' =>
               Dim := Dim - 1;
               Index := Index + 1;

            when '(' =>
               --  A parenthesis is either the start of a sub-array (for
               --  other dimensions, or one of the items in case it is a
               --  record or an array. The distinction can be made by
               --  looking at the current dimension being parsed.

               if Dim = Result.Dimensions'Last then
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

      Item := Result.Values;
      Result.Values := new Array_Item_Array (1 .. Value_Index - 1);
      Result.Values.all := Item (1 .. Value_Index - 1);
      Free (Item);
   end Parse_Array_Value;

   ------------------------------------
   -- Parse_Type_Gdb_Ada_From_String --
   ------------------------------------

   procedure Parse_Type_Gdb_Ada_From_String
     (Debugger : in Gdb_Debugger;
      Type_Str : in String;
      Entity   : in String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access)
   is
   begin
      case Type_Str (Index) is

         when '<' =>

            --  A union type

            if Looking_At (Type_Str, Index, "<union ") then
               Index := Index + 7;
               Skip_To_Char (Type_Str, Index, '{');
               Index := Index + 1;
               Parse_Record_Type_Ada (Debugger, Type_Str, Entity,
                                      Index, Result, "}>");

            --  Simple types, like <4-byte integer> and <4-byte float>

            else
               Skip_To_Char (Type_Str, Index, '>');
               Index := Index + 1;
               Result := new Simple_Type;
            end if;

         when 'a' =>

            --  Arrays, as in "array (1 .. 4, 3 .. 5) of character"

            if Looking_At (Type_Str, Index, "array ") then
               Parse_Array_Type_Ada (Debugger, Type_Str, Entity,
                                     Index, Result);

            --  Access types

            elsif Looking_At (Type_Str, Index, "access ") then
               Result := new Access_Type;
            else
               raise Unexpected_Type;
            end if;

         when 'm' =>

            --  Modular types

            if Looking_At (Type_Str, Index, "mod ") then
               Index := Index + 4;
               Result := new Mod_Type;
               Parse_Num (Type_Str, Index, Mod_Type_Access (Result).Max);
            end if;

         when 'r' =>

            --  A record type, as in 'record field1: integer; end record'

            if Looking_At (Type_Str, Index, "record") then
               Index := Index + 7;
               Parse_Record_Type_Ada (Debugger, Type_Str, Entity,
                                      Index, Result, "end record");

            --  Range types

            elsif Looking_At (Type_Str, Index, "range ") then
               Index := Index + 6;
               Result := new Range_Type;
               Parse_Num (Type_Str, Index, Range_Type_Access (Result).Min);
               Index := Index + 4; --  skips ' .. '
               Parse_Num (Type_Str, Index, Range_Type_Access (Result).Max);

            else
               raise Unexpected_Type;
            end if;

         when '(' =>

            --  Enumeration type
            Skip_To_Char (Type_Str, Index, ')');
            Index := Index + 1;
            Result := new Enum_Type;

         --  A type we do not expect.

         when others =>
            raise Unexpected_Type;
      end case;
   end Parse_Type_Gdb_Ada_From_String;

   ----------------------------------
   -- Parse_Type_Gdb_C_From_String --
   ----------------------------------

   procedure Parse_Type_Gdb_C_From_String
     (Debugger : in Gdb_Debugger;
      Type_Str : in String;
      Entity   : in String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access)
   is
   begin
      case Type_Str (Index) is

--           when 'i' =>
--              if Looking_At (Debugger, Type_Str, Index, "int") then
--                 Result := new Simple_Type'(Address  => System.Null_Address,
--                                            Value    => null);
--                 Index := Index + 3;
--              else
--                 raise Unexpected_Type;
--              end if;

         when others =>
            raise Unexpected_Type;

      end case;
      Result := null;
   end Parse_Type_Gdb_C_From_String;

   -------------------------------------
   -- Parse_Value_Gdb_Ada_From_String --
   -------------------------------------

   procedure Parse_Value_Gdb_Ada_From_String
     (Debugger  : in Gdb_Debugger;
      Type_Str  : in String;
      Index     : in out Natural;
      Result    : in out Generic_Values.Generic_Type_Access)
   is
      procedure Ada_Skip_Simple_Value is new Skip_Simple_Value
        (Array_Item_Separator => ',',
         End_Of_Array         => ')',
         Repeat_Item_Start    => '<');

   begin
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
               Ada_Skip_Simple_Value (Type_Str, Index);
               Simple_Type_Access (Result).Value
                 := new String'(Type_Str (Int .. Index - 1));

               -------------------
               -- Repeat values --
               -------------------
               --  This only happens inside arrays, so we can simply replace
               --  Result (it will be delete the next time the item is
               --  parsed anyway).

               if Looking_At (Type_Str, Index, "<repeats ") then
                  Result := new Repeat_Type'(Address => null,
                                             Repeat_Num => 0,
                                             Value   => Result);
                  Index := Index + 9;
                  Parse_Num (Type_Str,
                             Index,
                             Long_Integer
                             (Repeat_Type_Access (Result).Repeat_Num));
                  Index := Index + 7;  --  skips " times>"
               end if;

            end;
         else
            Simple_Type_Access (Result).Value := new String'("<???>");
         end if;

      -------------------
      -- Access values --
      -------------------
      --  The value looks like:   (access integer) 0xbffff54c
      --  or                  :    0x0

      elsif Result'Tag = Access_Type'Tag then

         --  Skip the parenthesis contents if needed
         if Type_Str (Index) = '(' then
            Skip_To_Char (Type_Str, Index, ')');
            Index := Index + 2;
         end if;
         declare
            Int : constant Natural := Index;
         begin
            Skip_Hexa_Digit (Type_Str, Index);
            Simple_Type_Access (Result).Value
              := new String'(Type_Str (Int .. Index - 1));
         end;

      -------------------
      -- String values --
      -------------------

      elsif Result'Tag = Array_Type'Tag
        and then Array_Type_Access (Result).Num_Dimensions = 1
        and then Type_Str'Length /= 0
        and then Type_Str (Index) = '"'
      then
         declare
            First : constant Integer
              := Integer (Array_Type_Access (Result).Dimensions (1).First);
            Last : constant Integer
              := Integer (Array_Type_Access (Result).Dimensions (1).Last);
            S : String (1 .. Last - First + 1);
         begin
            Parse_Cst_String (Type_Str, Index, S);

            --  We need to allocate a single item in the array, that
            --  contains the whole string.

            Array_Type_Access (Result).Values
              := new Array_Item_Array (1 .. 1);
            Array_Type_Access (Result).Values (1)
              := (Index => 0,
                  Value => new Simple_Type'(Address => null,
                                            Value   => new String'(S)));
         end;

      ------------------
      -- Array values --
      ------------------

      elsif Result'Tag = Array_Type'Tag
        and then Type_Str'Length /= 0   --  for empty Arrays
      then
         Parse_Array_Value (Debugger, Type_Str, Index,
                            Array_Type_Access (Result));

      -------------------
      -- Record values --
      -------------------

      elsif Result'Tag = Record_Type'Tag then
         declare
            R : Record_Type_Access := Record_Type_Access (Result);
            Int : Natural;
         begin

            --  Skip initial '(' if we are still looking at it (we might not
            --  if we are parsing a variant part)
            if Type_Str (Index) = '(' then
               Index := Index + 1;
            end if;

            for J in R.Fields'Range loop
               if R.Fields (J).Value /= null then

                  --  Skips '=>'
                  Skip_To_Char (Type_Str, Index, '=');
                  Index := Index + 3;

                  Parse_Value_Gdb_Ada_From_String
                    (Debugger, Type_Str, Index, R.Fields (J).Value);

               elsif R.Fields (J).Variant_Part /= null then

                  if Type_Str (Index) = ',' then
                     Index := Index + 1;
                     Skip_Blanks (Type_Str, Index);
                  end if;

                  --  Find which part is active
                  --  We simply get the next field name and search for the
                  --  part that defines it
                  Int := Index;
                  Skip_To_Char (Type_Str, Int, ' ');

                  for V in R.Fields (J).Variant_Part'Range loop
                     if R.Fields (J).Variant_Part (V).Fields (1).Name.all
                       = Type_Str (Index .. Int - 1)
                     then
                        Parse_Value_Gdb_Ada_From_String
                          (Debugger, Type_Str, Index,
                           Generic_Type_Access
                           (R.Fields (J).Variant_Part (V)));
                     end if;
                  end loop;
               end if;
            end loop;
         end;
         Skip_Blanks (Type_Str, Index);

         --  Skip closing ')'
         Index := Index + 1;

      end if;
   end Parse_Value_Gdb_Ada_From_String;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type (Debugger : Gdb_Debugger;
                        Entity   : String)
                       return Generic_Values.Generic_Type_Access
   is
      Result : Generic_Type_Access;
      Type_Str : String := Type_Of (Debugger, Entity);
      Index  : Natural := Type_Str'First;
   begin
      if Type_Str'Length /= 0 then
         Parse_Type_Gdb_Ada_From_String (Debugger, Type_Str, Entity,
                                         Index, Result);
      end if;
      return Result;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Debugger  : Gdb_Debugger;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access)
   is
      Type_Str : String := Value_Of (Debugger, Entity);
      Index    : Natural := Type_Str'First;
   begin
      --  Clear the value previously parsed.
      Clear_Value (Value.all);
      Parse_Value_Gdb_Ada_From_String (Debugger, Type_Str, Index, Value);
   end Parse_Value;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Debugger : Gdb_Debugger;
                     Entity : String)
                    return String
   is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);

      Send (Debugger.Process.all, "ptype " & Entity);
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Debugger.Process.all);
      begin
         if S'Length > 14 then
            return S (S'First + 7 .. S'Last - 6);
         else
            return "";
         end if;
      end;
   end Type_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Debugger : Gdb_Debugger;
                      Entity   : String;
                      Format   : Value_Format := Decimal)
                     return String
   is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);

      Send (Debugger.Process.all, "print " & Entity);
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Debugger.Process.all);
         Index : Natural := S'First;
      begin

         --  Skip the '$nn =' part
         while Index <= S'Last
           and then S (Index) /= '='
         loop
            Index := Index + 1;
         end loop;
         Index := Index + 1;

         return S (Index + 1 .. S'Last - 6);
      end;
   end Value_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : in out Gdb_Debugger) is
      Null_List : Gnat.OS_Lib.Argument_List (1 .. 0);
   begin
      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      Debugger.Process :=
        new Pipes_Id'
          (Non_Blocking_Spawn ("gdb", Null_List, Buffer_Size => 0));

--        Add_Output_Filter (Debugger.Process.all, Trace_Filter'Access);
--        Add_Input_Filter (Debugger.Process.all, Trace_Filter'Access);
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set prompt (gdb) ");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set width 0");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set height 0");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set annotate 1");
      Wait_Prompt (Debugger);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : in out Gdb_Debugger) is
   begin
      Close (Debugger.Process.all);
      Free (Debugger.Process);
   end Close;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Debugger : Gdb_Debugger;
                             Executable : String)
   is
   begin
      Send (Debugger.Process.all, "file " & Executable);
      Wait_Prompt (Debugger);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : Gdb_Debugger) is
      Num : Expect_Match;
   begin
      Expect (Debugger.Process.all, Num, Prompt_Regexp);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : Gdb_Debugger) is
   begin
      Send (Debugger.Process.all, "run");
   end Run;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception (Debugger  : Gdb_Debugger;
                              Name      : String  := "";
                              Unhandled : Boolean := False)
   is
   begin
      --  ??? If language = "Ada"
      if Unhandled then
         Send (Debugger.Process.all, "break exception unhandled");
      elsif Name /= "" then
         Send (Debugger.Process.all, "break exception " & Name);
      else
         raise Unknown_Command;
      end if;
      Wait_Prompt (Debugger);
   end Break_Exception;

   ---------------
   -- Backtrace --
   ---------------

   function Backtrace (Debugger : Gdb_Debugger) return String is
      Result : Expect_Match;
   begin
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);
      Send (Debugger.Process.all, "bt");
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Debugger.Process.all);
      begin
         return S (S'First .. S'Last - 6);
      end;
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger : Gdb_Debugger; Name : String) is
   begin
      Send (Debugger.Process.all, "break " & Name);
      Wait_Prompt (Debugger);
   end Break_Subprogram;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : Gdb_Debugger) is
   begin
      Send (Debugger.Process.all, "finish");
      Wait_Prompt (Debugger);
   end Finish;

end Debugger.Gdb;
