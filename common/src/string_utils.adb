-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2000-2010, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Scripts.Utils;     use GNATCOLL.Scripts.Utils;
with GNATCOLL.Utils;

with Glib.Unicode;               use Glib, Glib.Unicode;

package body String_Utils is

   -----------------
   -- Lines_Count --
   -----------------

   function Lines_Count (Text : String) return Natural is
      Count : Natural := 1;
   begin
      for T in Text'Range loop
         if Text (T) = ASCII.LF then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Lines_Count;

   -----------------
   -- Blank_Slice --
   -----------------

   function Blank_Slice
     (Count     : Integer;
      Use_Tabs  : Boolean := False;
      Tab_Width : Natural := 8) return String is
   begin
      if Count <= 0 then
         return "";
      elsif Use_Tabs then
         return (1 .. Count / Tab_Width => ASCII.HT) &
           (1 .. Count mod Tab_Width => ' ');
      else
         return (1 .. Count => ' ');
      end if;
   end Blank_Slice;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (S     : in out GNAT.Strings.String_Access;
      Value : String) is
   begin
      if S /= null then
         if S'Length = Value'Length then
            --  Let's try to avoid memory fragmentation
            S.all := Value;
            return;
         else
            GNAT.Strings.Free (S);
         end if;
      end if;

      S := new String'(Value);
   end Replace;

   procedure Replace
     (S     : in out GNAT.Strings.String_Access;
      Value : GNAT.Strings.String_Access) is
   begin
      if Value = null then
         GNAT.Strings.Free (S);
      else
         Replace (S, Value.all);
      end if;
   end Replace;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1) is
   begin
      while (Index <= Type_Str'Last and then Index >= Type_Str'First)
        and then (Type_Str (Index) = ' '
                  or else Type_Str (Index) = ASCII.HT
                  or else Type_Str (Index) = ASCII.LF
                  or else Type_Str (Index) = ASCII.CR)
      loop
         Index := Index + Step;
      end loop;
   end Skip_Blanks;

   -------------------
   -- Skip_To_Blank --
   -------------------

   procedure Skip_To_Blank
     (Type_Str : String;
      Index    : in out Natural) is
   begin
      while Index <= Type_Str'Last
        and then Type_Str (Index) /= ' '
        and then Type_Str (Index) /= ASCII.HT
        and then Type_Str (Index) /= ASCII.LF
        and then Type_Str (Index) /= ASCII.CR
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_Blank;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.LF
        or else C = ASCII.CR or else C = ASCII.HT;
   end Is_Blank;

   -------------------
   -- Is_Blank_Line --
   -------------------

   function Is_Blank_Line
     (Buffer : String; Index : Natural := 0) return Boolean
   is
      It : Natural := Index;
   begin
      if It = 0 then
         It := Buffer'First;
      end if;

      if It >= Buffer'First then
         while It <= Buffer'Last
           and then Buffer (It) /= ASCII.CR
           and then Buffer (It) /= ASCII.LF
         loop
            if Buffer (It) /= ' '
              and then Buffer (It) /= ASCII.HT
            then
               return False;
            end if;

            It := It + 1;
         end loop;
      end if;

      return True;
   end Is_Blank_Line;

   ----------------
   -- Skip_Lines --
   ----------------

   procedure Skip_Lines
     (Buffer        : String;
      Lines         : Integer;
      Index         : in out Natural;
      Lines_Skipped : out Natural)
   is
      Index_Saved : Natural;
   begin
      Lines_Skipped := 0;

      if Lines >= 0 then
         while Lines_Skipped < Lines loop
            Index := Next_Line (Buffer, Index);

            if Index = Buffer'Last then
               Index := Line_Start (Buffer, Index);
               exit;
            end if;

            Lines_Skipped := Lines_Skipped + 1;
         end loop;
      else
         Index_Saved := Line_Start (Buffer, Index);

         while Lines_Skipped < -Lines loop
            Index := Previous_Line (Buffer, Index);

            exit when Index = Index_Saved;

            Lines_Skipped := Lines_Skipped + 1;
         end loop;
      end if;
   end Skip_Lines;

   --------------------
   -- Skip_To_Column --
   --------------------

   procedure Skip_To_Column
     (Buffer        : String;
      Columns       : Natural := 0;
      Index         : in out Natural;
      Tab_Width     : Integer := 8)
   is
      Current_Col   : Natural := 1;
   begin
      while Current_Col < Columns
        and then Index <= Buffer'Last
        and then Buffer (Index) /= ASCII.LF
      loop
         if Index < Buffer'Last
           and then Buffer (Index) = ASCII.HT
         then
            Current_Col := Current_Col
              + Tab_Width - ((Current_Col - 1) mod Tab_Width);
         else
            Current_Col := Current_Col + 1;
         end if;

         Index := Index + 1;
      end loop;
   end Skip_To_Column;

   -------------------
   -- Skip_To_Index --
   -------------------

   procedure Skip_To_Index
     (Buffer        : String;
      Columns       : out Natural;
      Index_In_Line : Natural;
      Index         : in out Natural;
      Tab_Width     : Integer := 8)
   is
      Start_Of_Line : constant Integer := Index;
   begin
      Columns := 1;

      loop
         exit when Index - Start_Of_Line + 1 >= Index_In_Line;

         if Index <= Buffer'Last
           and then Buffer (Index) = ASCII.HT
         then
            Columns := Columns + Tab_Width - ((Columns - 1) mod Tab_Width);
         else
            Columns := Columns + 1;
         end if;

         Index := Index + 1;
      end loop;
   end Skip_To_Index;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (Buffer : String; P : Natural) return Natural is
      Index : Natural := Natural'Min (Buffer'Last, P);
   begin
      if P <= Buffer'First then
         return P;
      end if;

      if Buffer (Index) = ASCII.LF then
         Index := Index - 1;

         if Buffer (Index) = ASCII.LF then
            return Index + 1;
         elsif Buffer (Index) = ASCII.CR then
            if Index > Buffer'First then
               Index := Index - 1;

               if Buffer (Index) = ASCII.LF then
                  return Index + 1;
               end if;
            else
               return Buffer'First;
            end if;
         end if;

      elsif Buffer (Index) = ASCII.CR then
         Index := Index - 1;

         if Buffer (Index) = ASCII.LF then
            return Index + 1;
         end if;
      end if;

      for J in reverse Buffer'First .. Index loop
         if Buffer (J) = ASCII.LF or else Buffer (J) = ASCII.CR then
            if J < Buffer'Last then
               return J + 1;
            else
               return Buffer'Last;
            end if;
         end if;
      end loop;

      return Buffer'First;
   end Line_Start;

   --------------
   -- Line_End --
   --------------

   function Line_End (Buffer : String; P : Natural) return Natural is
   begin
      for J in P .. Buffer'Last loop
         if Buffer (J) = ASCII.LF or else Buffer (J) = ASCII.CR then
            return J - 1;
         end if;
      end loop;

      return Buffer'Last;
   end Line_End;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (Buffer : String; P : Natural) return Natural is
   begin
      for J in P .. Buffer'Last - 1 loop
         if Buffer (J) = ASCII.LF then
            return J + 1;
         end if;
      end loop;

      return Buffer'Last;
   end Next_Line;

   procedure Next_Line
     (Buffer  : String;
      P       : Natural;
      Next    : out Natural;
      Success : out Boolean) is
   begin
      for J in P .. Buffer'Last - 1 loop
         if Buffer (J) = ASCII.LF then
            Next := J + 1;
            Success := True;
            return;
         end if;
      end loop;

      Success := False;
      Next    := Buffer'Last;
   end Next_Line;

   -------------------
   -- Previous_Line --
   -------------------

   function Previous_Line (Buffer : String; P : Natural) return Natural is
      Index : constant Natural := Line_Start (Buffer, P);
   begin
      if Index > Buffer'First then
         return Line_Start (Buffer, Index - 1);
      else
         return Buffer'First;
      end if;
   end Previous_Line;

   ---------------------
   -- Skip_Hexa_Digit --
   ---------------------

   procedure Skip_Hexa_Digit
     (Type_Str : String;
      Index    : in out Natural) is
   begin
      --  skips initial 0x if present

      if Index + 1 <= Type_Str'Last
        and then Type_Str (Index) = '0'
        and then Type_Str (Index + 1) = 'x'
      then
         Index := Index + 2;
      end if;

      while Index <= Type_Str'Last
        and then Is_Hexadecimal_Digit (Type_Str (Index))
      loop
         Index := Index + 1;
      end loop;
   end Skip_Hexa_Digit;

   ------------------
   -- Skip_To_Char --
   ------------------

   procedure Skip_To_Char
     (Type_Str : String;
      Index    : in out Natural;
      Char     : Character;
      Step     : Integer := 1) is
   begin
      while Index <= Type_Str'Last
        and then Index >= Type_Str'First
        and then Type_Str (Index) /= Char
      loop
         Index := Index + Step;
      end loop;
   end Skip_To_Char;

   --------------------
   -- Skip_To_String --
   --------------------

   procedure Skip_To_String
     (Type_Str  : String;
      Index     : in out Natural;
      Substring : String)
   is
      L : constant Natural := Substring'Length - 1;
   begin
      while Index + L <= Type_Str'Last
        and then Type_Str (Index .. Index + L) /= Substring
      loop
         Index := Index + 1;
      end loop;
   end Skip_To_String;

   ---------------
   -- Parse_Num --
   ---------------

   procedure Parse_Num
     (Type_Str : String;
      Index    : in out Natural;
      Result   : out Long_Integer)
   is
      Tmp_Index : constant Natural := Index;
   begin
      --  Recognize negative numbers as well

      if Type_Str (Index) = '-' then
         Index := Index + 1;
      end if;

      while Index <= Type_Str'Last
        and then Type_Str (Index) in '0' .. '9'
      loop
         Index := Index + 1;
      end loop;

      --  If at least one valid character was found, we have a number

      if Index > Tmp_Index then
         Result := Long_Integer'Value (Type_Str (Tmp_Index .. Index - 1));
      else
         Result := 0;
      end if;

   exception
      when Constraint_Error =>
         Result := Long_Integer'Last;
   end Parse_Num;

   ----------------
   -- Looking_At --
   ----------------

   function Looking_At
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Boolean is
   begin
      return Index + Substring'Length - 1 <= Type_Str'Last
        and then Type_Str (Index .. Index + Substring'Length - 1) = Substring;
   end Looking_At;

   ----------------------
   -- Parse_Cst_String --
   ----------------------

   procedure Parse_Cst_String
     (Type_Str          : String;
      Index             : in out Natural;
      Str               : out String;
      Str_Last          : out Natural;
      Backslash_Special : Boolean := True)
   is
      procedure Parse_Next_Char
        (Index : in out Natural;
         Char  : out Character);
      --  Parse the character pointed to by Index, including special characters

      In_String : Boolean;

      ---------------------
      -- Parse_Next_Char --
      ---------------------

      procedure Parse_Next_Char
        (Index : in out Natural;
         Char  : out Character)
      is
         Int : Natural;
      begin
         --  Special characters are represented as ["00"] or ["""]
         --  Note that we can have '[" ' that represents the character
         --  '[' followed by the end of the string

         if Index + 4 <= Type_Str'Last
           and then Type_Str (Index) = '['
           and then Type_Str (Index + 1) = '"'
           and then (Type_Str (Index + 2 .. Index + 4) = """""]"
                     or else Type_Str (Index + 2) in '0' .. '9'
                     or else Type_Str (Index + 2) in 'a' .. 'f')
         then
            if Type_Str (Index + 2) = '"' then
               Index := Index + 5;
               Char := '"';

            else
               if Type_Str (Index + 2) in 'a' .. 'f' then
                  Int := 16 * (Character'Pos (Type_Str (Index + 2))
                               - Character'Pos ('a') + 10);
               else
                  Int := 16 * (Character'Pos (Type_Str (Index + 2))
                               - Character'Pos ('0'));
               end if;

               if Type_Str (Index + 3) in 'a' .. 'f' then
                  Int := Int + Character'Pos (Type_Str (Index + 3))
                    - Character'Pos ('a') + 10;
               else
                  Int := Int + Character'Pos (Type_Str (Index + 3))
                    - Character'Pos ('0');
               end if;

               Char  := Character'Val (Int);
               Index := Index + 6;
            end if;

         --  Else, a standard character

         else
            Char := Type_Str (Index);
            Index := Index + 1;
         end if;
      end Parse_Next_Char;

      S_Index   : Natural := Str'First;
      Char      : Character;
      Num       : Long_Integer;
      Last      : Natural := Str'Last;

   begin  --  Parse_Cst_String
      if Str'Length = 0 then
         Last := Natural'Last;
      end if;

      In_String := Type_Str (Index) = '"';
      if In_String then
         Index := Index + 1;
      end if;

      --  Note: this is a slightly complex loop, since a string might not
      --  appear as a single string in gdb, but can be made of multiple
      --  elements, including characters repeated a number of times, as in:
      --  "["af"]["c7"]", '["00"]' <repeats 12 times>, "BA"

      while S_Index <= Last
        and then Index <= Type_Str'Last
        and then Type_Str (Index) /= ASCII.LF
      loop
         case Type_Str (Index) is
            when '"' =>
               --  Handling of Ada-style strings:   A""double quote
               if In_String
                 and then Index < Type_Str'Last
                 and then Type_Str (Index + 1) = '"'
               then
                  Index := Index + 2;
                  Str (S_Index) := '"';
                  S_Index := S_Index + 1;

               else
                  In_String := not In_String;
                  Index := Index + 1;

                  --  In cases like {field = 0x8048f88 "bar"}, we need to
                  --  consider the string finished, but not for
                  --     "bar", 'cd' <repeats 12 times>
                  if not In_String
                    and then Index <= Type_Str'Last
                    and then Type_Str (Index) /= ' '
                    and then Type_Str (Index) /= ','
                  then
                     Index := Index + 1;
                     Str_Last  := S_Index - 1;
                     return;
                  end if;
               end if;

            when ''' =>
               if In_String then
                  if Str'Length /= 0 then
                     Str (S_Index) := ''';
                  end if;

                  S_Index := S_Index + 1;
                  Index := Index + 1;

               else
                  Index := Index + 1;  --  skips initial '''
                  Parse_Next_Char (Index, Char);

                  if Str'Length /= 0 then
                     Str (S_Index) := Char;
                  end if;

                  Index := Index + 2;     --  skips "' " at the end

                  if Looking_At (Type_Str, Index, "<repeats ") then
                     Index := Index + 9;
                     Parse_Num (Type_Str, Index, Num);

                     if Str'Length /= 0 then
                        Str (S_Index .. S_Index + Integer (Num) - 1) :=
                          (others => Char);
                     end if;

                     S_Index := S_Index + Integer (Num);
                     Index := Index + 7; --  skips " times>"

                  else
                     S_Index := S_Index + 1;
                  end if;
               end if;

            when '\' =>
               if Backslash_Special then
                  if Str'Length /= 0 then
                     Str (S_Index) := Type_Str (Index + 1);
                     S_Index := S_Index + 1;
                  end if;

                  Index := Index + 2;

               else
                  Str (S_Index) := Type_Str (Index);
                  S_Index := S_Index + 1;
                  Index := Index + 1;
               end if;

            when ' ' | ',' =>
               if In_String then
                  if Str'Length /= 0 then
                     Str (S_Index) := ' ';
                  end if;

                  S_Index := S_Index + 1;

               --  ',' is still part of the string output only if it is
               --  followed by a constant string or character (repeats).
               --  Otherwise, ',' simply denotes the end of a struct field,
               --  as in "field3 = "ab", field4 = 1"

               elsif Type_Str (Index) = ','
                 and then
                 (Index >= Type_Str'Last - 1
                  or else (Type_Str (Index + 2) /= '''
                           and then Type_Str (Index + 2) /= '"'))
               then
                  Index := Index + 1;
                  Str_Last  := S_Index - 1;
                  return;
               end if;

               Index := Index + 1;

            when others =>
               Parse_Next_Char (Index, Char);

               if Str'Length /= 0 then
                  Str (S_Index) := Char;
               end if;

               S_Index := S_Index + 1;
         end case;
      end loop;

      Index := Index + 1;
      Str_Last  := S_Index - 1;
   end Parse_Cst_String;

   -----------------------
   -- Skip_Simple_Value --
   -----------------------

   procedure Skip_Simple_Value
     (Type_Str             : String;
      Index                : in out Natural;
      Array_Item_Separator : Character := ',';
      End_Of_Array         : Character := ')';
      Repeat_Item_Start    : Character := '<') is
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

   ---------------
   -- Skip_Word --
   ---------------

   procedure Skip_Word
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1)
   is
      Initial : constant Natural := Index;
   begin
      while Index <= Type_Str'Last
        and then Index >= Type_Str'First
        and then (Is_Alphanumeric (Type_Str (Index))
                  or else
                  Type_Str (Index) = '_')
      loop
         Index := Index + Step;
      end loop;

      --  Move at least one character

      if Index = Initial then
         Index := Index + Step;
      end if;
   end Skip_Word;

   --------------------
   -- Skip_CPP_Token --
   --------------------

   procedure Skip_CPP_Token
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1)
   is
      Initial : constant Natural := Index;
   begin
      while Index <= Type_Str'Last
        and then Index >= Type_Str'First
        and then (Is_Alphanumeric (Type_Str (Index))
                  or else
                  Type_Str (Index) = '_'
                  or else
                  Type_Str (Index) = '.')
      loop
         Index := Index + Step;
      end loop;

      --  Move at least one character

      if Index = Initial then
         Index := Index + Step;
      end if;
   end Skip_CPP_Token;

   ------------
   -- Reduce --
   ------------

   function Reduce
     (S            : String;
      Max_Length   : Positive := Positive'Last;
      Continuation : String := "...") return String
   is
      Result : String (S'Range);
      Len    : Positive := Result'First;
      Blank  : Boolean  := False;

      Max    : Natural;
      --  Max if the position of the last character to be returned
      Cut    : Boolean := False;
      --  Cut set to true if string was cut before the end at Max characters
      Char   : Natural := S'First;
      Next   : Natural := Char;
   begin

      if Max_Length = Positive'Last then
         Max := Positive'Last;
      else
         Max := S'First + Max_Length - Continuation'Length - 1;
      end if;

      while Next <= S'Last loop
         Char := Next;
         Next := UTF8_Next_Char (S, Char);
         if Next > S'Last then
            Next := S'Last + 1;
         end if;

         if S (Char) = ASCII.LF or else S (Char) = ASCII.CR
           or else S (Char) = ASCII.HT or else S (Char) = ' '
         then
            if not Blank then
               Result (Len) := ' ';
               Len := Len + 1;
               Blank := True;
            end if;

         else
            Blank := False;

            Result (Len .. Len + Next - Char - 1) := S (Char .. Next - 1);
            Len := Len + Next - Char;
         end if;

         if Len >= Max then
            Cut := True;
            exit;
         end if;
      end loop;

      if Cut then
         return Result (Result'First .. Len - 1) & Continuation;
      else
         return Result (Result'First .. Len - 1);
      end if;
   end Reduce;

   ------------
   -- Krunch --
   ------------

   function Krunch
     (S                 : String;
      Max_String_Length : Positive := 20) return String
   is
      Ellipsis_UTF8 : constant UTF8_String :=
                        Character'Val (16#E2#) & Character'Val (16#80#)
                        & Character'Val (16#A6#);
      --  UTF8 encoding for the ellipsis character (8230 in Decimal)

      First_Half          : Natural;
      Second_Half         : Natural;

   begin
      if S'Length <= Max_String_Length then
         return S;
      end if;

      if Max_String_Length <= Ellipsis_UTF8'Length then
         return S (S'First .. S'First + Max_String_Length - 1);
      else
         First_Half := S'First;

         for J in 1 .. (Max_String_Length - Ellipsis_UTF8'Length + 1) / 2 loop
            First_Half := UTF8_Next_Char (S, First_Half);
         end loop;

         Second_Half := S'Last;

         for J in 1 .. (Max_String_Length - Ellipsis_UTF8'Length - 1) / 2 loop
            Second_Half := UTF8_Find_Prev_Char (S, Second_Half);
         end loop;

         if First_Half > S'Last
           or else Second_Half < S'First
         then
            return S;
         else
            return S (S'First .. First_Half - 1)
              & Ellipsis_UTF8 & S (Second_Half .. S'Last);
         end if;
      end if;
   end Krunch;

   ---------------------
   -- Strip_Character --
   ---------------------

   function Strip_Character (Text : String; C : Character) return String is
      pragma Suppress (All_Checks);

      To       : String (1 .. Text'Length);
      Index_To : Positive := 1;

   begin
      for Index in Text'Range loop
         if Text (Index) /= C then
            To (Index_To) := Text (Index);
            Index_To := Index_To + 1;
         end if;
      end loop;

      return To (1 .. Index_To - 1);
   end Strip_Character;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Text : String) return String is
   begin
      return Strip_Character (Text, ASCII.CR);
   end Strip_CR;

   procedure Strip_CR
     (Text     : in out String;
      Last     : out Integer;
      CR_Found : out Boolean)
   is
      pragma Suppress (All_Checks);

      J : Natural := Text'First;
   begin
      CR_Found := False;

      if Text'Length = 0 then
         Last := 0;
         return;
      end if;

      loop
         --  Manual unrolling for efficiency

         exit when Text (J) = ASCII.CR or J = Text'Last;
         J := J + 1;

         exit when Text (J) = ASCII.CR or J = Text'Last;
         J := J + 1;

         exit when Text (J) = ASCII.CR or J = Text'Last;
         J := J + 1;
      end loop;

      if Text (J) /= ASCII.CR then
         Last := J;
         return;
      end if;

      CR_Found := True;
      Last := J - 1;

      for Index in J + 1 .. Text'Last loop
         if Text (Index) /= ASCII.CR then
            Last := Last + 1;
            Text (Last) := Text (Index);
         end if;
      end loop;
   end Strip_CR;

   ----------------------
   -- Strip_CR_And_NUL --
   ----------------------

   procedure Strip_CR_And_NUL
     (Text      : in out String;
      Last      : out Integer;
      CR_Found  : out Boolean;
      NUL_Found : out Boolean)
   is
      pragma Suppress (All_Checks);

      J : Natural := Text'First;
   begin
      CR_Found := False;
      NUL_Found := False;

      if Text'Length = 0 then
         Last := 0;
         return;
      end if;

      loop
         --  Manual unrolling for efficiency

         exit when Text (J) = ASCII.CR
           or else Text (J) = ASCII.NUL
           or else J = Text'Last;
         J := J + 1;

         exit when Text (J) = ASCII.CR
           or else Text (J) = ASCII.NUL
           or else J = Text'Last;
         J := J + 1;

         exit when Text (J) = ASCII.CR
           or else Text (J) = ASCII.NUL
           or else J = Text'Last;
         J := J + 1;
      end loop;

      case Text (J) is
         when ASCII.NUL | ASCII.CR =>
            Last := J - 1;
         when others =>
            Last := J;
            return;
      end case;

      for Index in J + 1 .. Text'Last loop
         case Text (Index) is
            when ASCII.NUL =>
               NUL_Found := True;
            when ASCII.CR  =>
               CR_Found := True;
            when others =>
               Last := Last + 1;
               Text (Last) := Text (Index);
         end case;
      end loop;
   end Strip_CR_And_NUL;

   -----------------------------
   -- Strip_Ending_Linebreaks --
   -----------------------------

   function Strip_Ending_Linebreaks (Text : String) return String is
   begin
      --  Loop to make sure we have removed all of the ending CRs and LFs

      for J in reverse Text'Range loop
         if Text (J) /= ASCII.CR and then Text (J) /= ASCII.LF then
            return Text (Text'First .. J);
         end if;
      end loop;

      return "";
   end Strip_Ending_Linebreaks;

   ----------------------
   -- Do_Tab_Expansion --
   ----------------------

   function Do_Tab_Expansion
     (Text     : String;
      Tab_Size : Integer) return String
   is
      Num_Tabs : Natural := 0;
      Col      : Integer := 1;

   begin
      --  Count the number of tabs in the string

      for K in Text'Range loop
         if Text (K) = ASCII.HT then
            Num_Tabs := Num_Tabs + 1;
         end if;
      end loop;

      if Num_Tabs = 0 then
         return Text;
      else
         declare
            S       : String (1 .. Num_Tabs * Tab_Size + Text'Length);
            S_Index : Integer := 1;
            Bound   : Integer;

         begin
            for K in Text'Range loop
               case Text (K) is
                  when ASCII.LF =>
                     S (S_Index) := Text (K);
                     S_Index := S_Index + 1;
                     Col := 1;

                  when ASCII.HT =>
                     if Col mod Tab_Size /= 0 then
                        Bound := (1 + Col / Tab_Size) * Tab_Size - Col + 1;
                        S (S_Index .. S_Index + Bound - 1) := (others => ' ');
                        S_Index := S_Index + Bound;
                        Col := Col + Bound;

                     else
                        S (S_Index) := ' ';
                        S_Index := S_Index + 1;
                        Col := Col + 1;
                     end if;

                  when others =>
                     S (S_Index) := Text (K);
                     S_Index := S_Index + 1;
                     Col := Col + 1;
               end case;
            end loop;

            return S (S'First .. S_Index - 1);
         end;
      end if;
   end Do_Tab_Expansion;

   ------------------
   -- Strip_Quotes --
   ------------------

   function Strip_Quotes (S : String) return String is
      S_First : Integer := S'First;
      S_Last  : Integer := S'Last;

   begin
      if S = "" then
         return "";
      end if;

      while S_First <= S'Last
        and then (S (S_First) = ' ' or else S (S_First) = '"')
      loop
         S_First := S_First + 1;
      end loop;

      while S_Last >= S'First
        and then (S (S_Last) = ' ' or else S (S_Last) = '"')
      loop
         S_Last := S_Last - 1;
      end loop;

      return S (S_First .. S_Last);
   end Strip_Quotes;

   -----------
   -- Image --
   -----------

   function Image (N : Integer) return String is
   begin
      return GNATCOLL.Utils.Image (N, Min_Width => 1);
   end Image;

   ----------------------
   -- Number_Of_Digits --
   ----------------------

   function Number_Of_Digits (N : Integer) return Natural is
   begin
      case N is
         when 0 .. 9 =>
            return 1;
         when 10 .. 99 =>
            return 2;
         when 100 .. 999 =>
            return 3;
         when 1_000 .. 9_999 =>
            return 4;
         when 10_000 .. 99_999 =>
            return 5;
         when others =>
            return Image (N)'Length;
      end case;
   end Number_Of_Digits;

   ----------------------
   -- Is_Entity_Letter --
   ----------------------

   function Is_Entity_Letter (Char : Gunichar) return Boolean is
   begin
      return Char = Character'Pos ('_') or else Is_Alnum (Char)
        or else Unichar_Type (Char) = Unicode_Lowercase_Letter
        or else Unichar_Type (Char) = Unicode_Uppercase_Letter;
   end Is_Entity_Letter;

   ------------------------
   -- Is_Operator_Letter --
   ------------------------

   function Is_Operator_Letter (Char : Gunichar) return Boolean is
   begin
      case Char is
         when Character'Pos ('<')
              | Character'Pos ('=')
              | Character'Pos ('>')
              | Character'Pos ('+')
              | Character'Pos ('-')
              | Character'Pos ('*')
              | Character'Pos ('/') =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Operator_Letter;

   function Is_File_Letter (Char : Glib.Gunichar) return Boolean is
   begin
      if Unichar_Type (Char) = Unicode_Control then
         return False;
      end if;

      case Char is
         when Character'Pos ('<')
              | Character'Pos ('/')
              | Character'Pos ('\')
              | Character'Pos ('>')
              | Character'Pos ('"')
              | Character'Pos (' ') =>
            return False;

         when others =>
            return True;
      end case;
   end Is_File_Letter;

   -----------------
   -- Copy_String --
   -----------------

   procedure Copy_String
     (Item : Interfaces.C.Strings.chars_ptr;
      Str  : out String;
      Len  : Natural)
   is
      procedure Strncpy
        (Dest : out String;
         Src  : Interfaces.C.Strings.chars_ptr;
         Len  : Interfaces.C.size_t);
      pragma Import (C, Strncpy, "strncpy");

   begin
      Strncpy (Str, Item, Interfaces.C.size_t (Len));
   end Copy_String;

   -----------------------------
   -- Argument_List_To_String --
   -----------------------------

   function Argument_List_To_String
     (List           : GNAT.Strings.String_List;
      Protect_Quotes : Boolean := True) return String
   is
      Length : Natural := 0;
   begin
      for L in List'Range loop
         Length := Length + List (L)'Length + 1;

         if Protect_Quotes then
            for S in List (L)'Range loop
               if List (L)(S) = '"'
                 or else List (L)(S) = ' '
               then
                  Length := Length + 1;
               end if;
            end loop;
         end if;
      end loop;

      declare
         S     : String (1 .. Length);
         Index : Positive := S'First;
      begin
         for L in List'Range loop
            for J in List (L)'Range loop
               if Protect_Quotes then
                  if List (L)(J) = '"' or else List (L)(J) = ' ' then
                     S (Index) := '\';
                     Index := Index + 1;
                  end if;
               end if;
               S (Index) := List (L)(J);
               Index := Index + 1;
            end loop;
            S (Index) := ' ';
            Index := Index + 1;
         end loop;
         return S (1 .. S'Last - 1);  -- Ignore last space
      end;
   end Argument_List_To_String;

   -----------
   -- Clone --
   -----------

   function Clone
     (List : GNAT.Strings.String_List) return GNAT.Strings.String_List
   is
      L : String_List (List'Range);
   begin
      for J in List'Range loop
         L (J) := new String'(List (J).all);
      end loop;
      return L;
   end Clone;

   ------------
   -- Append --
   ------------

   procedure Append
     (List  : in out GNAT.Strings.String_List_Access;
      List2 : GNAT.Strings.String_List)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String_List, String_List_Access);
      L : String_List_Access := List;
   begin
      if List = null then
         List := new String_List (1 .. List2'Length);
      else
         List := new String_List (L'First .. L'Last + List2'Length);
         List (L'Range) := L.all;
         Unchecked_Free (L);
      end if;

      List (List'Last - List2'Length + 1 .. List'Last) := List2;
   end Append;

   ----------------
   -- Safe_Value --
   ----------------

   function Safe_Value (S : String; Default : Integer := 1) return Integer is
   begin
      if S = "" then
         return Default;
      else
         return Integer'Value (S);
      end if;
   exception
      when Constraint_Error =>
         return Default;
   end Safe_Value;

   -------------
   -- Protect --
   -------------

   function Protect
     (S                   : String;
      Protect_Quotes      : Boolean := True;
      Protect_Spaces      : Boolean := False;
      Protect_Backslashes : Boolean := True) return String
   is
      S2    : String (1 .. S'Length * 2);
      Index : Natural := 1;
   begin
      for J in S'Range loop
         if (Protect_Quotes and then S (J) = '"')
           or else (Protect_Backslashes and then S (J) = '\')
           or else (Protect_Spaces and then S (J) = ' ')
         then
            S2 (Index .. Index + 1) := '\' & S (J);
            Index := Index + 2;
         else
            S2 (Index) := S (J);
            Index := Index + 1;
         end if;
      end loop;

      return S2 (1 .. Index - 1);
   end Protect;

   ---------------
   -- Unprotect --
   ---------------

   function Unprotect (S : String) return String is
   begin
      return GNATCOLL.Scripts.Utils.Unprotect (S);
   end Unprotect;

   -------------
   -- Unquote --
   -------------

   function Unquote (S : String) return String is
   begin
      if S'Length > 1
        and then S (S'First) = '"'
        and then S (S'Last) = '"'
      then
         return S (S'First + 1 .. S'Last - 1);
      else
         return S;
      end if;
   end Unquote;

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (Hex : String) return Natural is
   begin
      return Integer'Value ("16#" & Hex & "#");
   end Hex_Value;

   ------------
   -- Decode --
   ------------

   function URL_Decode (URL : String) return String is
      Res : String (1 .. URL'Length);
      K   : Natural := 0;
      J   : Positive := URL'First;

   begin
      if URL = "" then
         return "";
      end if;

      loop
         K := K + 1;

         if URL (J) = '%'
           and then J + 2 <= URL'Last
           and then Is_Hexadecimal_Digit (URL (J + 1))
           and then Is_Hexadecimal_Digit (URL (J + 2))
         then
            Res (K) := Character'Val (Hex_Value (URL (J + 1 .. J + 2)));
            J := J + 2;
         else
            Res (K) := URL (J);
         end if;

         J := J + 1;
         exit when J > URL'Last;
      end loop;

      return Res (1 .. K);
   end URL_Decode;

   ------------
   -- Revert --
   ------------

   function Revert (S : String) return String is
      Result : String (S'Range);
      Index  : Natural := S'First;
      Last   : Natural := S'Last;
      Len    : Natural;

   begin
      for J in reverse S'Range loop
         if S (J) = '.' then
            Len := Last - J;
            Result (Index .. Index + Len - 1) := S (J + 1 .. Last);
            Last := J - 1;
            Index := Index + Len;
            Result (Index) := '.';
            Index := Index + 1;
         end if;
      end loop;

      if Last >= S'First then
         Result (Index .. Result'Last) := S (S'First .. Last);
      end if;

      return Result;
   end Revert;

   ------------------------------
   -- Strip_Single_Underscores --
   ------------------------------

   function Strip_Single_Underscores (S : String) return String is
      S2 : String := S;
      J2 : Natural := S'First;
   begin
      for J in S'Range loop
         S2 (J2) := S (J);

         if S (J) /= '_'
           or else (J > S'First and then S (J - 1) = '_')
         then
            J2 := J2 + 1;
         end if;
      end loop;

      return S2 (S2'First .. J2 - 1);
   end Strip_Single_Underscores;

   -------------
   -- Compare --
   -------------

   function Compare (A, B : String) return Glib.Gint is
   begin
      if A < B then
         return -1;
      elsif A > B then
         return 1;
      else
         return 0;
      end if;
   end Compare;

   function Compare (A, B : Integer) return Glib.Gint is
   begin
      if A < B then
         return -1;
      elsif A > B then
         return 1;
      else
         return 0;
      end if;
   end Compare;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Header_Num is
      Tmp : constant Ada.Containers.Hash_Type := Ada.Strings.Hash (Key);
   begin
      return Header_Num'First +
               Header_Num'Base (Tmp mod Header_Num'Range_Length);
   end Hash;

   ---------------------------
   -- Case_Insensitive_Hash --
   ---------------------------

   function Case_Insensitive_Hash (Key : String) return Header_Num is
      Tmp : constant Ada.Containers.Hash_Type :=
        Ada.Strings.Hash_Case_Insensitive (Key);
   begin
      return Header_Num'First +
               Header_Num'Base (Tmp mod Header_Num'Range_Length);
   end Case_Insensitive_Hash;

   -------------------
   -- Remove_Markup --
   -------------------

   Esc_Quote : constant String := "&quot;";

   function Remove_Markup (Text : String) return String is
      Result     : String (Text'Range);
      Result_Cur : Integer := Result'First;
      Text_Cur   : Integer := Text'First;
   begin
      while Text_Cur <= Text'Last loop
         if Text (Text_Cur) = '&' then
            if Text_Cur + Esc_Quote'Length - 1 <= Text'Last
              and then
                Text (Text_Cur .. Text_Cur + Esc_Quote'Length - 1)
                = Esc_Quote
            then
               Result (Result_Cur) := '"';

               Text_Cur := Text_Cur + Esc_Quote'Length;
            else
               Result (Result_Cur) := Text (Text_Cur);
               Text_Cur := Text_Cur + 1;
            end if;
         else
            Result (Result_Cur) := Text (Text_Cur);
            Text_Cur := Text_Cur + 1;
         end if;

         Result_Cur := Result_Cur + 1;
      end loop;

      return Result (Result'First .. Result_Cur - 1);
   end Remove_Markup;

   ---------------------------
   -- Has_Include_Directive --
   ---------------------------

   function Has_Include_Directive (Str : String) return Boolean is
   begin
      return Str'Length > 11
        and then Str (Str'First) = '#'
        and then Ada.Strings.Fixed.Index
          (Str (Str'First + 1 .. Str'Last), "include") /= 0;
   end Has_Include_Directive;

end String_Utils;
