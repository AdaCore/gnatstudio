------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
package body Message_Kinds is

   function Is_Suffix (S, Suffix : String) return Boolean;
   --  Return True if Str ends with Suffix

   ---------------
   -- Is_Suffix --
   ---------------

   function Is_Suffix (S, Suffix : String) return Boolean is
   begin
      return Suffix'Length <= S'Length
        and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix;
   end Is_Suffix;

   -------------
   -- CWE_Ids --
   -------------

   function CWE_Ids
     (Kind : Message_Kinds.BE_Message_Subkind;
      Msg  : String := "") return String is
   begin
      case Kind is
         when Invalid_Check =>
            return "232,236,475";
         when Invalid_Or_Null_Check =>
            return "252-253,476";
         when Divide_By_Zero_Check =>
            return "189";
         when Array_Indexing_Check =>
            return "124-127,129-131,135,170,193";
         when Tag_Check | Type_Variant_Check =>
            return "136,137";
         when Numeric_Range_Check =>
            return "118";
         when Numeric_Overflow_Check |
              User_Assign_Stm_Check |
              Pre_Assign_Stm_Check |
              Post_Assign_Stm_Check =>
            return "128,190-192,197";
         when XSS_Check =>
            return "80,81,82,83,84,85,87";
         when SQL_Injection_Check =>
            return "89";
         when Unlocked_Reentrant_Update_Error |
              Unlocked_Shared_Daemon_Update_Error =>
            return "362,366,367,374,820";
         when Mismatched_Locked_Update_Error =>
            return "362,366,367,374,821";
         when Dead_Store_Warning |
              Dead_Outparam_Store_Warning |
              Same_Value_Dead_Store_Warning |
              Potentially_Dead_Store_Warning =>
            return "563";
         when Dead_Block_Warning |
              Dead_Edge_Warning |
              Plain_Dead_Edge_Warning |
              True_Dead_Edge_Warning |
              False_Dead_Edge_Warning |
              True_Condition_Dead_Edge_Warning |
              False_Condition_Dead_Edge_Warning |
              Unrepeatable_While_Loop_Warning =>
            return "561";
         when Infinite_Loop_Warning =>
            return "835";
         when Precondition_Check =>
            if Is_Suffix (Msg, "to be initialized") then
               --  Validity check message
               return "232,236,475";
            elsif Is_Suffix (Msg, "/= null") then
               --  Access check message
               return "252-253,476";
            else
               --  We cannot differentiate the other cases, so list all
               --  possibilities. ??? Would be good to refine this.
               return "118,124-131,135-137,170,190-193,197,252,253,476";
            end if;

         when others =>
            return "";
      end case;
   end CWE_Ids;

   function Is_Security_Relevant
     (Kind          : Message_Kinds.BE_Message_Subkind;
      Msg           : String;
      Runtime_Check : Boolean) return Boolean is
   begin
      case Kind is
         when Invalid_Check |
              XSS_Check | SQL_Injection_Check |
              Unlocked_Reentrant_Update_Error |
              Unlocked_Shared_Daemon_Update_Error |
              Mismatched_Locked_Update_Error |
              Dead_Store_Warning |
              Dead_Outparam_Store_Warning |
              Same_Value_Dead_Store_Warning |
              Potentially_Dead_Store_Warning |
              Dead_Block_Warning |
              Dead_Edge_Warning |
              Plain_Dead_Edge_Warning |
              True_Dead_Edge_Warning |
              False_Dead_Edge_Warning |
              True_Condition_Dead_Edge_Warning |
              False_Condition_Dead_Edge_Warning |
              Unrepeatable_While_Loop_Warning |
              Infinite_Loop_Warning =>
            return True;

         when Invalid_Or_Null_Check |
              Divide_By_Zero_Check |
              Array_Indexing_Check |
              Tag_Check | Type_Variant_Check |
              Numeric_Range_Check |
              Numeric_Overflow_Check | User_Assign_Stm_Check |
              Pre_Assign_Stm_Check | Post_Assign_Stm_Check =>
            return Runtime_Check;

         when Precondition_Check =>
            if Is_Suffix (Msg, "to be initialized") then
               --  Validity check message
               return True;
            else
               return Runtime_Check;
            end if;

         when others =>
            return False;
      end case;
   end Is_Security_Relevant;

   function Primary_Original_Checks
     (Original_Checks : Check_Kinds_Array) return Check_Kinds_Array
   is
      Overflow_Checks_In_Set     : Boolean := False;
      Non_Overflow_Checks_In_Set : Boolean := False;
      Range_Checks_In_Set        : Boolean := False;
      Primary_Checks             : Check_Kinds_Array :=
        Check_Kinds_Array_Default;

   begin
      --  Fill in the Primary_Checks, postponing invalid and overflow for this
      --  first step. See second loop below, where we give those checks a low
      --  precedence in annotations output relative to range checks and other
      --  checks.

      for C in Check_Kind_Enum'Range loop
         if Original_Checks (C) then
            case C is
               when Precondition_Check =>
                  pragma Assert (False);
                  --  Should have been replaced with the original
                  --  precondition's checks.
                  null;

               when Floating_Point_Underflow_Check =>
                  --  Never consider underflow check as a primary check
                  null;

               when Invalid_Or_Null_Check
                  | Freed_Check
                  | Divide_By_Zero_Check
                  | Boolean_Check
                  | Non_Neg_Check
                  | Negative_Exponent_Check
                  | Postcondition_Check
                  | Raise_Check
                  | Conditional_Raise_Check
                  | Assertion_Check
                  | Tag_Check
                  | Aliasing_Check
                  | User_Precondition_Check
               =>
                  Primary_Checks (C) := True;
                  Non_Overflow_Checks_In_Set := True;

               when Invalid_Check
               =>
                  --  Postpone processing of invalid checks, see loop below
                  null;

               when User_Assign_Stm_Check
                  | Pre_Assign_Stm_Check
                  | Post_Assign_Stm_Check
                  | Numeric_Overflow_Check
               =>
                  --  Postpone processing of overflow checks, see loop below
                  Overflow_Checks_In_Set := True;

               when Numeric_Range_Check
                  | Array_Indexing_Check
                  | Type_Variant_Check
               =>
                  Range_Checks_In_Set := True;
                  Non_Overflow_Checks_In_Set := True;
                  Primary_Checks (C) := True;
            end case;
         end if;
      end loop;

      --  Add invalid and overflow if needed:
      --  - overflow hides validity if we have other checks as well;
      --  - range check/discriminant_check/array_index_check hide overflow.

      for C in Check_Kind_Enum'Range loop
         if Original_Checks (C) then
            case C is
               when Invalid_Check =>
                  --  See comment above: we want to keep overflow+validity
                  --  if there are no other checks, so that we do not lose
                  --  validity information on e.g. tests for uninitialized
                  --  variables, as found in the qualkit test suite.

                  if not Range_Checks_In_Set
                    and then not (Overflow_Checks_In_Set
                                  and then Non_Overflow_Checks_In_Set)
                  then
                     Primary_Checks (C) := True;
                  end if;

               when User_Assign_Stm_Check
                  | Pre_Assign_Stm_Check
                  | Post_Assign_Stm_Check
                  | Numeric_Overflow_Check
               =>
                  if not Range_Checks_In_Set then
                     Primary_Checks (C) := True;
                  end if;

               when others => null;
            end case;
         end if;
      end loop;

      return Primary_Checks;
   end Primary_Original_Checks;

   function Image (Int : Integer) return String;
   --  return a printable image of Int without a leading space.

   function Image (Int : Integer) return String is
      Raw_Image : constant String := Integer'Image (Int);
   begin
      return Ada.Strings.Fixed.Trim (Raw_Image, Ada.Strings.Left);
   end Image;

   function Improve_Number_Readability_In_Messages
     (S      : String;
      For_HTML_Output : Boolean := True)
      return   String
   is
      --  Make various improvements to numbers, such as
      --  replacing near powers-of-2 by 2<sup>X +/- n

      --  TBD: Should we handle various GNAT name encodings such as Uxx
      --       to mean Latin-1 character with hex code "xx"?

      Max_Readable_Set_Length : constant := 64;
      --  Maximum length of acceptable [ xxx .. yyy ] string before
      --  truncation.

      Exponent : Natural := 0;
      Offset   : Integer := 0;

      function Offset_Part (Is_Negated : Boolean) return String;
      --  Return +/- Offset, or empty str if = 0
      --  Reverse the sign if number preceded by '-'
      --  TBD: We aren't worrying about any other surrounding
      --      operators that might be higher precedence

      function Offset_Part (Is_Negated : Boolean) return String is
      begin
         if Offset = 0 then
            --  exact match with power of 2
            return "";
         elsif (Offset < 0) xor Is_Negated then
            --  Precede with '-'
            return '-' & Image (abs (Offset));
         else
            --  Precede with '+'
            return '+' & Image (abs (Offset));
         end if;
      end Offset_Part;

      I : Positive := S'First;

   begin --  Improve_Number_Readability_In_Messages

      while I < S'Last loop
         case S (I) is
            when '0' .. '9' =>
               --  find end of number
               Loop_Over_Digits : for J in I + 1 .. S'Last + 1 loop

                  --  Look for end of number
                  if J > S'Last
                    or else (S (J) not in '0' .. '9' and then S (J) /= '_')
                  then
                     --  end of number
                     declare
                        Head : String renames S (I .. J - 3);
                        --  All but last two digits of number

                        function Compute_Offset (Ref  : Natural)
                        return Integer;

                        function Compute_Offset (Ref  : Natural)
                        return Integer is
                           Char_0 : constant Natural := Character'Pos ('0');
                           Tail   : constant Natural :=
                              (Character'Pos (S (J - 2)) - Char_0) * 10 +
                              Character'Pos (S (J - 1)) -
                              Char_0;

                        begin
                           return Tail - Ref;
                        end Compute_Offset;

                     begin
                        case J - I is
                           when 3 =>
                              --  check 128, 256 and environs
                              null;

                           when 5 | 6 =>
                              --  check 32_768, 65_536 and environs
                              if Head = "32_7" or else Head = "327" then
                                 Exponent := 15;
                                 Offset   := Compute_Offset (68);
                              elsif Head = "65_5" or else Head = "655" then
                                 Exponent := 16;
                                 Offset   := Compute_Offset (36);
                              end if;

                           when 10 | 13 =>
                              --  check 2**31, 2**32 and environs
                              if Head = "2_147_483_6"
                                or else Head = "21474836"
                              then
                                 Exponent := 31;
                                 Offset   := Compute_Offset (48);
                              elsif Head = "4_294_967_2"
                                or else Head = "42949672"
                              then
                                 Exponent := 32;
                                 Offset   := Compute_Offset (96);
                              end if;

                           when 19 | 25 =>
                              --  check 2**63 and environs
                              if Head = "9_223_372_036_854_775_8"
                                or else Head = "92233720368547758"
                              then
                                 Exponent := 63;
                                 Offset   := Compute_Offset (8);
                              end if;

                           when 20 | 26 =>
                              --  check 2**64 and environs
                              if Head = "18_446_744_073_709_551_6"
                                or else Head = "184467440737095516"
                              then
                                 Exponent := 64;
                                 Offset   := Compute_Offset (16);
                              end if;

                           when others =>
                              null;
                        end case;

                        if Exponent = 0 then
                           --  Nothing special
                           I := J - 1;  --  skip past this number and exit loop
                           exit Loop_Over_Digits;
                        elsif For_HTML_Output then
                           --  Replace with 2<sup>exponent</sup>+/-offset
                           return S (S'First .. I - 1) &
                                     "2<SUP>" &
                                     Image (Exponent) &
                                     "</SUP>" &
                                     Offset_Part
                                        (Is_Negated => I > S'First
                                             and then S (I - 1) = '-') &
                                     Improve_Number_Readability_In_Messages
                                        (S (J .. S'Last), For_HTML_Output);
                        else  --  Text output
                           --  Replace with 2**exponent+/-offset
                           return S (S'First .. I - 1) &
                                    "2**" &
                                    Image (Exponent) &
                                    Offset_Part
                                       (Is_Negated => I > S'First
                                          and then S (I - 1) = '-') &
                                    Improve_Number_Readability_In_Messages
                                       (S (J .. S'Last), For_HTML_Output);
                        end if;
                     end;

                  end if;

               end loop Loop_Over_Digits;
            --  If we get here, the number was nothing special

            when '[' | '(' =>
               --  Check for "[others{...}]"
               if I + 7 <= S'Last and then S (I .. I + 7) = "[others{" then
                  for J in I + 8 .. S'Last loop
                     if S (J) = '}' then
                        --  Strip out "others{}" part
                        return S (S'First .. I) &
                               Improve_Number_Readability_In_Messages
                                  (S (I + 8 .. J - 1), For_HTML_Output) &
                               Improve_Number_Readability_In_Messages
                                  (S (J + 1 .. S'Last),
                                   For_HTML_Output);
                     end if;
                  end loop;
                  --  Weird => no ending '}'
                  I := I + 7;  --  skip past "[others{"
               else
                  --  Check for [ blah .. blah ] or [ blah .. blah )
                  --  and change it to be [...] or [...)
                  --  ie replace the complex display of
                  --  the set to just be an ellipsis
                  declare
                     bi            : Positive := I + 1;
                     Found_Dot_Dot : Boolean  := False;
                  begin
                     while bi <= S'Last
                       and then S (bi) /= ']'
                       and then S (bi) /= ')'
                     loop
                        --  Is it true that there won't be
                        --  nested index expressions?
                        --  No, apparently not!
                        --  TBD:                            pragma Assert(
                        --  S(bi) /= '[' );
                        if S (bi) = '.'
                          and then bi + 1 < S'Last
                          and then S (bi + 1) = '.'
                          and then S (bi - 1) /= '.'
                          and then S (bi + 2) /= '.'
                        then
                           Found_Dot_Dot := True;
                        end if;

                        bi := bi + 1;
                     end loop;

                     if bi <= S'Last
                       and then Found_Dot_Dot
                       and then (S (bi) = ']' or else S (bi) = ')')
                       and then bi - I > Max_Readable_Set_Length
                     then
                        return S (S'First .. I) &
                               "..." &
                               S (bi) &
                               Improve_Number_Readability_In_Messages
                                  (S (bi + 1 .. S'Last),
                                   For_HTML_Output);
                     end if;
                  end;
               end if;

            --  If we get here, it is not "[others{...}"
            --  and it is not a long [ blah .. blah ]

            when 'V' =>
               --  Check for VN_Set{...}
               if I + 6 <= S'Last and then S (I .. I + 6) = "VN_Set{" then
                  --  Replace with "One-of{"
                  return S (S'First .. I - 1) &
                         "One-of{" &
                         Improve_Number_Readability_In_Messages
                            (S (I + 7 .. S'Last),
                             For_HTML_Output);
               end if;
            when others =>
               null;
         end case;
         I := I + 1;
      end loop;

      --  No improvement necessary
      return S;
   end Improve_Number_Readability_In_Messages;

end Message_Kinds;
