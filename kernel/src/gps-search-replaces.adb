------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2015, AdaCore                     --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Strings;          use GNAT.Strings;

with Glib.Unicode;          use Glib.Unicode;
with Glib;                  use Glib;

package body GPS.Search.Replaces is

   procedure Free is new Ada.Unchecked_Deallocation
     (Regexp_Reference_Array, Regexp_Reference_Array_Access);

   ----------
   -- Free --
   ----------

   procedure Free (Result : in out Replacement_Pattern) is
   begin
      Free (Result.Replace_String);
      Free (Result.References);
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
      begin
         for J in Replace_String'First .. Replace_String'Last - 1 loop
            if not Mask and Replace_String (J) = '\' then
               if Replace_String (J + 1) in '0' .. '9' then
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

         for J in Replace_String'First .. Replace_String'Last - 1 loop
            if not Mask and Replace_String (J) = '\' then
               if Replace_String (J + 1) in '0' .. '9' then
                  Count := Count + 1;
                  Result.References (Count) :=
                    (J, Natural'Value (Replace_String (J + 1 .. J + 1)));
               elsif Replace_String (J + 1) = '\' then
                  Mask := True;
               end if;
            else
               Mask := False;
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
      Offset         : Integer;
      From, To       : Natural;
      Last           : Natural := 1;
   begin
      --  if there are some references to regexp subexpressions
      --  and search in regexp mode
      if Pattern.Last > 0 and then Pattern.Is_Regexp then
         --  Read offset of Matched_Text into Offset
         Matched_Subexpression
           (Result, Index => 0, First => Offset, Last => Last);
         Offset := Offset - Matched_Text'First;
         Last := 1;
         for J in 1 .. Pattern.Last loop
            declare
               Ref : constant Regexp_Reference := Pattern.References (J);
            begin
               Append
                 (Regexp_Result,
                  Pattern.Replace_String (Last .. Ref.Offset - 1));

               if Ref.Match = 0 then  --  Whole matched string
                  Append (Regexp_Result, Matched_Text);
               else
                  Matched_Subexpression
                    (Result, Index => Ref.Match, First => From, Last => To);
                  Append (Regexp_Result,
                          Matched_Text (From - Offset .. To - Offset));
               end if;

               Last := Ref.Offset + 2;
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
