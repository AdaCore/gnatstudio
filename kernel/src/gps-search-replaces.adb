------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Strings;          use GNAT.Strings;

with Glib.Unicode;          use Glib.Unicode;
with Glib;                  use Glib;

package body GPS.Search.Replaces is

   procedure Free is new Ada.Unchecked_Deallocation
     (Regexp_Reference_Array, Regexp_Reference_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Subexpression'Class, Subexpression_Access);

   ---------------------------
   -- Regexp_Subexpressions --
   ---------------------------

   package Regexp_Subexpressions is
      --  This type provide text of regexp subexpression number Match
      type Regexp_Subexpression is new Subexpression with record
         Match : Positive;
      end record;

      procedure Create
        (Self   : in out Subexpression_Access;
         Origin : String);
      --  Initialize Self as Regexp_Subexpression, use Origin as image of
      --  regexp subexpression number

      overriding function Origin_Length
        (Self : Regexp_Subexpression) return Positive;

      overriding function Replace
        (Self    : access Regexp_Subexpression;
         Context : GPS.Search.Search_Context;
         Matched : String) return String;

   end Regexp_Subexpressions;

   package body Regexp_Subexpressions is

      ------------
      -- Create --
      ------------

      procedure Create
        (Self   : in out Subexpression_Access;
         Origin : String)
      is
         Match : constant Positive := Natural'Value (Origin);
      begin
         --  Avoid deallocation then allocation of Regexp_Subexpression if we
         --  already have correct object. Check EXACT match of type here.
         if Self = null or else Self.all not in Regexp_Subexpression then
            Free (Self);
            Self := new Regexp_Subexpression'(Match => Match);
         else
            Regexp_Subexpression (Self.all).Match := Match;
         end if;
      end Create;

      -------------------
      -- Origin_Length --
      -------------------

      overriding function Origin_Length
        (Self : Regexp_Subexpression) return Positive
      is
         pragma Unreferenced (Self);
      begin
         return 2;  --  Length of "\1" .. "\9" pattern
      end Origin_Length;

      -------------
      -- Replace --
      -------------

      overriding function Replace
        (Self    : access Regexp_Subexpression;
         Context : GPS.Search.Search_Context;
         Matched : String) return String
      is
         From, To : Natural;
      begin
         Matched_Subexpression
           (Context, Index => Self.Match, First => From, Last => To);

         return Matched (From .. To);
      end Replace;

   end Regexp_Subexpressions;

   --------------------------
   -- Whole_Subexpressions --
   --------------------------

   package Whole_Subexpressions is
      --  This type provide text of whole matched regexp
      type Whole_Subexpression is new Subexpression with null record;

      procedure Create (Self : in out Subexpression_Access);
      --  Initialize Self as Whole_Subexpression

      overriding function Origin_Length
        (Self : Whole_Subexpression) return Positive;

      overriding function Replace
        (Self    : access Whole_Subexpression;
         Context : GPS.Search.Search_Context;
         Matched : String) return String;

   end Whole_Subexpressions;

   package body Whole_Subexpressions is

      ------------
      -- Create --
      ------------

      procedure Create (Self : in out Subexpression_Access) is
      begin
         --  Avoid deallocation then allocation of Whole_Subexpression if we
         --  already have correct object. Check EXACT match of type here.
         if Self = null or else Self.all not in Whole_Subexpression then
            Free (Self);
            Self := new Whole_Subexpression;
         end if;
      end Create;

      -------------------
      -- Origin_Length --
      -------------------

      overriding function Origin_Length
        (Self : Whole_Subexpression) return Positive
      is
         pragma Unreferenced (Self);
      begin
         return 2;  --  Length of "\0" pattern
      end Origin_Length;

      -------------
      -- Replace --
      -------------

      overriding function Replace
        (Self    : access Whole_Subexpression;
         Context : GPS.Search.Search_Context;
         Matched : String) return String
      is
         pragma Unreferenced (Self);
         pragma Unreferenced (Context);
      begin
         return Matched;
      end Replace;

   end Whole_Subexpressions;

   -----------------------------
   -- Sequence_Subexpressions --
   -----------------------------

   package Sequence_Subexpressions is
      --  This type provide text of Sequence subexpression number Match
      type Sequence_Subexpression is new Subexpression with record
         Start         : Natural;
         Saved_Start   : Natural;
         Increment     : Integer;
         Origin_Length : Positive;
      end record;

      procedure Create
        (Self   : in out Subexpression_Access;
         Origin : String);
      --  Initialize Self as Sequence_Subexpression, use Origin as parameters
      --  in form (start, increment)

      overriding function Origin_Length
        (Self : Sequence_Subexpression) return Positive;

      overriding function Replace
        (Self    : access Sequence_Subexpression;
         Context : GPS.Search.Search_Context;
         Matched : String) return String;

      overriding procedure Reset (Self : access Sequence_Subexpression);

   end Sequence_Subexpressions;

   package body Sequence_Subexpressions is

      ------------
      -- Create --
      ------------

      procedure Create
        (Self   : in out Subexpression_Access;
         Origin : String)
      is
         Pos           : Positive;
         Close         : constant Natural := Index (Origin, ")");
         Start         : Natural := 1;
         Increment     : Integer := 1;
         Origin_Length : Positive := 2;
      begin
         if Origin'Last > Origin'First
           and then Origin (Origin'First + 1) = '('
           and then Close > 0
         then
            Pos := Origin'First + 2;
            while Origin (Pos) in '0' .. '9' loop
               Pos := Pos + 1;
            end loop;

            Start := Natural'Value (Origin (Origin'First + 2 .. Pos - 1));

            if Pos /= Close then
               Increment := Natural'Value (Origin (Pos + 1 .. Close - 1));
            end if;

            Origin_Length := Close - Origin'First + 2;
         end if;

         --  Avoid deallocation then allocation of Sequence_Subexpression if we
         --  already have correct object. Check EXACT match of type here.
         if Self = null or else Self.all not in Sequence_Subexpression then
            Free (Self);
            Self := new Sequence_Subexpression'
              (Start         => Start,
               Saved_Start   => Start,
               Increment     => Increment,
               Origin_Length => Origin_Length);
         else
            Sequence_Subexpression (Self.all).Start := Start;
            Sequence_Subexpression (Self.all).Increment := Increment;
            Sequence_Subexpression (Self.all).Origin_Length := Origin_Length;
         end if;
      end Create;

      -------------------
      -- Origin_Length --
      -------------------

      overriding function Origin_Length
        (Self : Sequence_Subexpression) return Positive is
      begin
         return Self.Origin_Length;
      end Origin_Length;

      -------------
      -- Replace --
      -------------

      overriding function Replace
        (Self    : access Sequence_Subexpression;
         Context : GPS.Search.Search_Context;
         Matched : String) return String
      is
         pragma Unreferenced (Context);
         pragma Unreferenced (Matched);

         Image : constant String := Natural'Image (Self.Start);
      begin
         if Self.Start + Self.Increment >= 0 then
            Self.Start := Self.Start + Self.Increment;
         end if;

         return Image (Image'First + 1 .. Image'Last);
      end Replace;

      -----------
      -- Reset --
      -----------

      overriding procedure Reset (Self : access Sequence_Subexpression) is
      begin
         Self.Start := Self.Saved_Start;
      end Reset;

   end Sequence_Subexpressions;

   ----------
   -- Free --
   ----------

   procedure Free (Result : in out Replacement_Pattern) is
   begin
      if Result.References /= null then
         for J in Result.References'Range loop
            Free (Result.References (J).Object);
         end loop;

         Free (Result.References);
      end if;

      Free (Result.Replace_String);
      for Casing in Lower .. Smart_Mixed loop
         Free (Result.Casings (Casing));
      end loop;
   end Free;

   ------------------
   -- Guess_Casing --
   ------------------

   function Guess_Casing (S : String) return Casing_Type
   is
      Index_1    : Integer;
      Index_2    : Integer;
      First_Char : Gunichar;
   begin
      if S = "" then
         return Lower;
      end if;

      Index_1 := UTF8_Next_Char (S, S'First);

      First_Char := UTF8_Get_Char (S (S'First .. Index_1 - 1));

      if not Is_Alpha (First_Char) then
         return Unchanged;
      end if;

      --  First character is lower: this string is Lower
      if Is_Lower (First_Char) then
         return Lower;
      end if;

      --  There is only one character: this string is Upper
      if Index_1 > S'Last then
         return Upper;
      end if;

      Index_2 := UTF8_Next_Char (S, Index_1);

      --  The first character is not lower and the second character is:
      --  this string is Smart_Mixed
      if Is_Lower (UTF8_Get_Char (S (Index_1 .. Index_2 - 1))) then
         return Smart_Mixed;
      end if;

      --  The first two characters are upper: this string is Upper
      return Upper;
   end Guess_Casing;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Replacement_Pattern;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Is_Regexp       : Boolean)
   is
      procedure Fill_References (Result : in out Replacement_Pattern);

      ---------------------
      -- Fill_References --
      ---------------------

      procedure Fill_References (Result : in out Replacement_Pattern) is
         Mask  : Boolean := False;
         Count : Natural := 0;
         Pos   : Positive;
      begin
         for J in Replace_String'First .. Replace_String'Last - 1 loop
            if not Mask and Replace_String (J) = '\' then
               if Replace_String (J + 1) in '0' .. '9' | 'i' then
                  Count := Count + 1;
               elsif Replace_String (J + 1) = '\' then
                  Mask := True;
               end if;
            else
               Mask := False;
            end if;
         end loop;

         if Count > Result.Last then
            Free (Result.References);
            Result.References := new Regexp_Reference_Array (1 .. Count);
         end if;

         Count := 0;
         Mask := False;

         Pos := Replace_String'First;

         while Pos < Replace_String'Last loop
            if Replace_String (Pos) = '\' then
               if Replace_String (Pos + 1) = '0' then
                  Count := Count + 1;
                  Result.References (Count).Offset := Pos;

                  Whole_Subexpressions.Create
                    (Result.References (Count).Object);

                  Pos := Pos + 2;
               elsif Replace_String (Pos + 1) in '1' .. '9' then
                  Count := Count + 1;
                  Result.References (Count).Offset := Pos;

                  Regexp_Subexpressions.Create
                    (Self   => Result.References (Count).Object,
                     Origin => Replace_String (Pos + 1 .. Pos + 1));

                  Pos := Pos + 2;
               elsif Replace_String (Pos + 1) = 'i' then
                  Count := Count + 1;
                  Result.References (Count).Offset := Pos;

                  Sequence_Subexpressions.Create
                    (Result.References (Count).Object,
                     Replace_String (Pos + 1 .. Replace_String'Last));

                  Pos := Pos + Result.References (Count).Object.Origin_Length;
               elsif Replace_String (Pos + 1) = '\' then
                  Pos := Pos + 2;
               else
                  --  Not a known pattern, do not interpret the \.
                  Pos := Pos + 1;
               end if;
            else
               Pos := Pos + 1;
            end if;
         end loop;

         Result.Last := Count;
      end Fill_References;

   begin
      if Self.Replace_String = null
        or else Self.Replace_String.all /= Replace_String
      then
         Free (Self.Replace_String);
         Self.Replace_String := new String'(Replace_String);

         for Casing in Casing_Type loop
            Free (Self.Casings (Casing));
            Self.Casings (Casing) :=
              new String'(To_Casing (Replace_String, Casing));
         end loop;

         Fill_References (Self);
      end if;

      Self.Case_Preserving := Case_Preserving;
      Self.Is_Regexp := Is_Regexp;
   end Initialize;

   ----------------------
   -- Replacement_Text --
   ----------------------

   function Replacement_Text
     (Pattern         : Replacement_Pattern;
      Result          : GPS.Search.Search_Context;
      Matched_Text    : String) return String
   is
      Current_Casing : Casing_Type := Unchanged;
      Regexp_Result  : Unbounded_String;
      From, To       : Natural;
      Last           : Natural := 1;
   begin
      --  if there are some references to regexp subexpressions
      --  and search in regexp mode
      if Pattern.Last > 0 and then Pattern.Is_Regexp then
         --  Read bounds of original Matched_Text into From, To
         Matched_Subexpression (Result, Index => 0, First => From, Last => To);
         To := From + Matched_Text'Length - 1;

         for J in 1 .. Pattern.Last loop
            declare
               subtype Matched is String (From .. To);

               Ref : constant Regexp_Reference := Pattern.References (J);
               Obj : constant Subexpression_Access :=
                 Pattern.References (J).Object;
            begin
               Append
                 (Regexp_Result,
                  Pattern.Replace_String (Last .. Ref.Offset - 1));

               Append
                 (Regexp_Result,
                  Obj.Replace (Result, Matched (Matched_Text)));

               Last := Ref.Offset + Obj.Origin_Length;
            end;
         end loop;

         Append
           (Regexp_Result,
            Pattern.Replace_String (Last .. Pattern.Replace_String'Last));

         if Pattern.Case_Preserving then
            Current_Casing := Guess_Casing (Matched_Text);

            return To_Casing
              (UTF8_Strdown (To_String (Regexp_Result)), Current_Casing);
         end if;

         return To_String (Regexp_Result);
      end if;

      if Pattern.Case_Preserving then
         Current_Casing := Guess_Casing (Matched_Text);
      end if;

      return Pattern.Casings (Current_Casing).all;
   end Replacement_Text;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Replacement_Pattern) is
   begin
      if Self.References /= null then
         for J in 1 .. Self.Last loop
            Self.References (J).Object.Reset;
         end loop;
      end if;
   end Reset;

   ---------------
   -- To_Casing --
   ---------------

   function To_Casing (S : String; Casing : Casing_Type) return String is
      Lower_S : constant String := UTF8_Strdown (S);

   begin
      --  If S is empty, return empty
      if S = "" then
         return "";
      end if;

      --  If S is not all lower case: return S
      if Lower_S /= S then
         return S;
      end if;

      case Casing is
         when Unchanged =>
            return S;

         when Lower =>
            return Lower_S;

         when Upper =>
            return UTF8_Strup (S);

         when Smart_Mixed =>
            declare
               O       : String (S'Range);
               I1, I2  : Natural;
               Capitalize_Next : Boolean := True;

            begin
               I2 := S'First;
               loop
                  I1 := I2;
                  I2 := UTF8_Next_Char (S, I1);

                  if Capitalize_Next then
                     O (I1 .. I2 - 1) := UTF8_Strup (S (I1 .. I2 - 1));
                  else
                     O (I1 .. I2 - 1) := S (I1 .. I2 - 1);
                  end if;

                  Capitalize_Next := not Is_Alpha
                    (UTF8_Get_Char (S (I1 .. I2 - 1)));

                  if I2 > S'Last then
                     return O;
                  end if;
               end loop;
            end;
      end case;
   end To_Casing;

end GPS.Search.Replaces;
