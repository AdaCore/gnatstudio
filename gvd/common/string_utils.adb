-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
--                                AdaCore                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Glib.Unicode;            use Glib, Glib.Unicode;

package body String_Utils is

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

   ----------------
   -- Skip_Lines --
   ----------------

   procedure Skip_Lines
     (Buffer : String;
      Lines  : Integer;
      Index  : in out Natural) is
   begin
      if Lines >= 0 then
         for Line in 1 .. Lines loop
            Index := Next_Line (Buffer, Index);
         end loop;
      else
         Index := Line_Start (Buffer, Index) - 2;
         for Line in 1 .. -Lines loop
            while Index >= Buffer'First
              and then Buffer (Index) /= ASCII.LF
              and then Buffer (Index) /= ASCII.CR
            loop
               Index := Index - 1;
            end loop;
         end loop;
         Index := Index + 1;
      end if;
   end Skip_Lines;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (Buffer : String; P : Natural) return Natural is
   begin
      for J in reverse Buffer'First .. P loop
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

      --  If at least one valid character was found, we have a number.

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
      Backslash_Special : Boolean := True)
   is
      procedure Parse_Next_Char
        (Index : in out Natural;
         Char  : out Character);
      --  Parse the character pointed to by Index, including special characters

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
           and then (Type_Str (Index + 2) = '"'
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

         --  Else, a standard character.

         else
            Char := Type_Str (Index);
            Index := Index + 1;
         end if;
      end Parse_Next_Char;


      S_Index   : Natural := Str'First;
      Char      : Character;
      Num       : Long_Integer;
      In_String : Boolean;
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
               In_String := not In_String;
               Index := Index + 1;

               --  In cases like {field = 0x8048f88 "bar"}, we need to consider
               --  the string finished, but not for
               --     "bar", 'cd' <repeats 12 times>
               if not In_String
                 and then Index <= Type_Str'Last
                 and then Type_Str (Index) /= ' '
                 and then Type_Str (Index) /= ','
               then
                  Index := Index + 1;
                  return;
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
   end Parse_Cst_String;

   -----------------------
   -- Skip_Simple_Value --
   -----------------------

   procedure Skip_Simple_Value
     (Type_Str             : in String;
      Index                : in out Natural;
      Array_Item_Separator : in Character := ',';
      End_Of_Array         : in Character := ')';
      Repeat_Item_Start    : in Character := '<') is
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
      Initial  : constant Natural := Index;
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

   ------------
   -- Reduce --
   ------------

   function Reduce (S : String) return String is
      Result : String (S'Range);
      Len    : Positive := Result'First;
      Blank  : Boolean  := False;

   begin
      for J in S'Range loop
         if S (J) = ASCII.LF or else S (J) = ASCII.CR
           or else S (J) = ASCII.HT or else S (J) = ' '
         then
            if not Blank then
               Result (Len) := ' ';
               Len := Len + 1;
               Blank := True;
            end if;

         else
            Blank := False;
            Result (Len) := S (J);
            Len := Len + 1;
         end if;
      end loop;

      return Result (Result'First .. Len - 1);
   end Reduce;

   ------------
   -- Krunch --
   ------------

   function Krunch
     (S                 : String;
      Max_String_Length : Positive := 20) return String
   is
      Krunch_Pattern : constant String := "[...]";
      Half           : constant Natural :=
        (Max_String_Length - Krunch_Pattern'Length + 1) / 2;

   begin
      if S'Length <= Max_String_Length then
         return S;
      elsif Max_String_Length <= Krunch_Pattern'Length then
         return S (S'First .. S'First + Max_String_Length - 1);
      else
         return S (S'First .. S'First + Half - 1) &
           Krunch_Pattern & S (S'Last - Half + 1 .. S'Last);
      end if;
   end Krunch;

   --------------
   -- Strip_CR --
   --------------

   function Strip_CR (Text : String) return String is
      pragma Suppress (All_Checks);

      To       : String (1 .. Text'Length);
      Index_To : Positive := 1;

   begin
      for Index in Text'Range loop
         if Text (Index) /= ASCII.CR then
            To (Index_To) := Text (Index);
            Index_To := Index_To + 1;
         end if;
      end loop;

      return To (1 .. Index_To - 1);
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

   function Strip_Quotes (S : in String) return String is
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
      return Ada.Strings.Fixed.Trim (Integer'Image (N), Ada.Strings.Left);
   end Image;

   function Image (N : Integer; Length : Positive) return String is
      Pad         : constant Character := ' ';
      Small_Image : constant String := Image (N);

   begin
      if Small_Image'Length >= Length then
         return Small_Image;
      else
         declare
            Padded_Image : String (1 .. Length);
         begin
            for Index in 1 .. Length - Small_Image'Length loop
               Padded_Image (Index) := Pad;
            end loop;

            Padded_Image
              (Length - Small_Image'Length + 1 ..  Length) :=
              Small_Image;

            return Padded_Image;
         end;
      end if;
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
      return Char = Character'Pos ('_') or else Is_Alnum (Char);
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

   ----------------------------
   -- Case_Insensitive_Equal --
   ----------------------------

   function Case_Insensitive_Equal (S1, S2 : String) return Boolean is
      J1 : Natural;
      J2 : Natural;
   begin
      if S1'Length /= S2'Length then
         return False;
      end if;

      J1 := S1'First;
      J2 := S2'First;

      while J1 <= S1'Last loop
         if To_Lower (S1 (J1)) /= To_Lower (S2 (J2)) then
            return False;
         end if;

         J1 := J1 + 1;
         J2 := J2 + 1;
      end loop;

      return True;
   end Case_Insensitive_Equal;

   -----------------------------
   -- Argument_List_To_String --
   -----------------------------

   function Argument_List_To_String
     (List : GNAT.OS_Lib.Argument_List;
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
         return S;
      end;
   end Argument_List_To_String;

   -----------
   -- Clone --
   -----------

   function Clone (List : GNAT.OS_Lib.Argument_List)
      return GNAT.OS_Lib.Argument_List
   is
      L : Argument_List (List'Range);
   begin
      for J in List'Range loop
         L (J) := new String'(List (J).all);
      end loop;
      return L;
   end Clone;

   ------------
   -- Append --
   ------------

   procedure Append (List  : in out GNAT.OS_Lib.Argument_List_Access;
                     List2 : GNAT.OS_Lib.Argument_List)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);
      L : Argument_List_Access := List;
   begin
      if List = null then
         List := new Argument_List (1 .. List2'Length);
      else
         List := new Argument_List (L'First .. L'Last + List2'Length);
         List (L'Range) := L.all;
         Unchecked_Free (L);
      end if;

      List (List'Last - List2'Length + 1 .. List'Last) := List2;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (Substrings : in out Substitution_Array) is
   begin
      for S in Substrings'Range loop
         Free (Substrings (S).Name);
         Free (Substrings (S).Value);
      end loop;
   end Free;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Str               : String;
      Substitution_Char : Character;
      Callback          : Substitute_Callback;
      Recursive         : Boolean := False) return String
   is
      Result        : Unbounded_String;
      First, Last   : Natural := Str'First;
      Between_Curly : Boolean;
      Quoted        : Boolean := False;
   begin
      while First <= Str'Last loop
         Last := First;
         while Last <= Str'Last
           and then Str (Last) /= Substitution_Char
         loop
            if Str (Last) = '"' then
               Quoted := not Quoted;
            end if;

            Last := Last + 1;
         end loop;

         Result := Result & Str (First .. Last - 1);

         exit when Last > Str'Last;

         First := Last + 1;

         Between_Curly := False;

         if Str (First) = Substitution_Char then
            --  We are escaping the Substitution_Char by doubling it.
            Result := Result & Substitution_Char;
            First := First + 1;

         elsif Str (First) = '{' then
            Between_Curly := True;
            Last := Last + 1;

            while First <= Str'Last
              and then Str (First) /= '}'
            loop
               First := First + 1;
            end loop;
         else
            --  Special case for -, since it used for $2-, and *.
            while First <= Str'Last
              and then (Is_Alphanumeric (Str (First))
                        or else Str (First) = '-'
                        or else Str (First) = '*')
              and then Str (First) /= ' '
              and then Str (First) /= '"'
            loop
               First := First + 1;
            end loop;
         end if;

         if Last + 1 <= First - 1 then
            declare
               Sub : constant String :=
                 Callback
                   (Str (Last + 1 .. First - 1),
                    Quoted => Quoted);
            begin
               if Recursive then
                  Result := Result & Substitute
                    (Sub, Substitution_Char, Callback, Recursive);
               else
                  Result := Result & Sub;
               end if;
            end;
         else
            Result := Result & Str (Last .. First - 1);
         end if;

         if Between_Curly then
            First := First + 1;
         end if;
      end loop;

      return To_String (Result);
   exception
      when Invalid_Substitution =>
         return Str;
   end Substitute;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Str               : String;
      Substitution_Char : Character;
      Substrings        : Substitution_Array;
      Recursive         : Boolean := False) return String
   is
      Result : Unbounded_String;
      First, Last : Natural := Str'First;
      Found : Boolean;
   begin
      while First <= Str'Last loop
         Last := First;
         while Last <= Str'Last
           and then Str (Last) /= Substitution_Char
         loop
            Last := Last + 1;
         end loop;

         Result := Result & Str (First .. Last - 1);

         exit when Last > Str'Last;

         Found := False;

         for S in Substrings'Range loop
            if Last + Substrings (S).Name'Length <= Str'Last
              and then Substrings (S).Name.all =
              Str (Last + 1 .. Last + Substrings (S).Name'Length)
            then
               if Recursive then
                  Result := Result & Substitute
                    (Substrings (S).Value.all,
                     Substitution_Char, Substrings, Recursive);
               else
                  Result := Result & Substrings (S).Value.all;
               end if;

               Found := True;
               First := Last + Substrings (S).Name'Length + 1;
               exit;
            end if;
         end loop;

         if not Found then
            Result := Result & Str (Last);
            First := Last + 1;
         end if;

      end loop;

      return To_String (Result);
   end Substitute;

   ------------------------------------
   -- Argument_List_To_Quoted_String --
   ------------------------------------

   function Argument_List_To_Quoted_String
     (Args  : GNAT.OS_Lib.Argument_List;
      Quote : Character := '"';
      Quote_Backslash : Boolean := True) return String
   is
      Len : Natural := 1;
   begin
      --  Compute the maximum length of the output.

      for J in Args'Range loop
         --  For each argument we append at most 3 characters, two quotes
         --  plus an ending space.

         if Args (J) /= null then
            Len := Len + Args (J)'Length + 3;

            for T in Args (J)'Range loop
               if Args (J)(T) = Quote or else Args (J)(T) = '\' then
                  Len := Len + 1;
               end if;
            end loop;
         end if;
      end loop;

      declare
         Result : String (1 .. Len + 1);
         Ind    : Natural := Result'First;

         procedure Append (Str : String);
         --  Append the contents of Str to Result, protecting quote characters

         ------------
         -- Append --
         ------------

         procedure Append (Str : String) is
         begin
            for J in Str'Range loop
               if Str (J) = Quote
                 or else (Quote_Backslash and then Str (J) = '\')
               then
                  Result (Ind)     := '\';
                  Result (Ind + 1) := Str (J);
                  Ind := Ind + 2;
               else
                  Result (Ind) := Str (J);
                  Ind := Ind + 1;
               end if;
            end loop;
         end Append;

      begin
         for J in Args'Range loop
            if Args (J) /= null then
               if Index (Args (J).all, " ") > 0 then
                  Result (Ind) := Quote;
                  Ind := Ind + 1;
                  Append (Args (J).all);
                  Result (Ind) := Quote;
                  Result (Ind + 1) := ' ';
                  Ind := Ind + 2;

               else
                  Append (Args (J).all);
                  Result (Ind) := ' ';
                  Ind := Ind + 1;
               end if;
            end if;
         end loop;

         return Result (1 .. Ind - 1);
      end;
   end Argument_List_To_Quoted_String;

   ----------------
   -- Safe_Value --
   ----------------

   function Safe_Value
     (S : String; Default : Integer := 1) return Integer is
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

   ------------------------------------------------
   -- Argument_String_To_List_With_Triple_Quotes --
   ------------------------------------------------

   function Argument_String_To_List_With_Triple_Quotes
     (Arg_String : String)
      return       Argument_List_Access
   is
      Max_Args : Integer := 128;
      New_Argv : Argument_List_Access := new Argument_List (1 .. Max_Args);
      New_Argc : Natural := 0;
      Idx      : Integer;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);
      Backslashed   : Boolean;
      Quoted        : Boolean;
      Triple_Quoted : Boolean;
      Has_Triple    : Boolean;
      Start_Idx     : Integer;
      Start_With_Triple : Boolean;
      End_With_Triple   : Boolean;

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         Backslashed   := False;
         Quoted        := False;
         Triple_Quoted := False;
         Start_Idx     := Idx;
         Start_With_Triple := False;
         End_With_Triple   := False;

         while Idx <= Arg_String'Last
           and then (Backslashed
                     or else Quoted
                     or else Triple_Quoted
                     or else Arg_String (Idx) /= ' ')
         loop
            End_With_Triple := False;

            if Backslashed then
               Backslashed := False;
            else

               case Arg_String (Idx) is
                  when '\' =>
                     Backslashed := True;

                  when '"' =>
                     if Quoted then
                        Quoted := False;
                     else
                        Has_Triple := Idx + 2 <= Arg_String'Last
                          and then Arg_String (Idx) = '"'
                          and then Arg_String (Idx + 1) = '"'
                          and then Arg_String (Idx + 2) = '"';
                        if Has_Triple then
                           Triple_Quoted := not Triple_Quoted;
                           if Idx = Start_Idx then
                              Start_With_Triple := Triple_Quoted;
                           end if;
                           End_With_Triple := True;
                           Idx := Idx + 2;
                        else
                           Quoted := True;
                        end if;
                     end if;

                  when others =>
                     null;
               end case;
            end if;

            Idx := Idx + 1;
         end loop;

         New_Argc := New_Argc + 1;

         --  Resize the table if needed
         if New_Argc > Max_Args then
            declare
               New_New_Argv : Argument_List (1 .. Max_Args * 2);
            begin
               New_New_Argv (1 .. Max_Args) := New_Argv.all;
               Unchecked_Free (New_Argv);
               New_Argv := new Argument_List'(New_New_Argv);
            end;

            Max_Args := Max_Args * 2;
         end if;

         if Start_With_Triple and End_With_Triple then
            New_Argv (New_Argc) :=
              new String'(Arg_String (Start_Idx + 3 .. Idx - 4));
         else
            New_Argv (New_Argc) :=
              new String'(Arg_String (Start_Idx .. Idx - 1));
         end if;

         --  Skip extraneous spaces

         while Idx <= Arg_String'Last and then Arg_String (Idx) = ' ' loop
            Idx := Idx + 1;
         end loop;
      end loop;

      declare
         Result : constant Argument_List := New_Argv (1 .. New_Argc);
      begin
         Unchecked_Free (New_Argv);
         return new Argument_List'(Result);
      end;
   end Argument_String_To_List_With_Triple_Quotes;

   -------------
   -- Protect --
   -------------

   function Protect
     (S : String; Protect_Quotes : Boolean := True) return String
   is
      S2    : String (1 .. S'Length * 2);
      Index : Natural := 1;
   begin
      for J in S'Range loop
         if (Protect_Quotes and then S (J) = '"')
           or else S (J) = '\'
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
      Result : String (S'Range);
      Index  : Natural := Result'First;
      N      : Natural := S'First;
   begin
      while N <= S'Last loop
         if S (N) = '\' then
            if N < S'Last then
               Result (Index) := S (N + 1);
            end if;

            N := N + 2;
         else
            Result (Index) := S (N);
            N := N + 1;
         end if;

         Index := Index + 1;
      end loop;

      if Result (Result'First) = '"'
        and then Result (Index - 1) = '"'
      then
         return Result (Result'First + 1 .. Index - 2);
      else
         return Result (Result'First .. Index - 1);
      end if;
   end Unprotect;

end String_Utils;
