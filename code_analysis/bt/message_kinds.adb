------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                        Copyright (C) 2013-2023, AdaCore                  --
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
with Ada.Containers.Ordered_Sets;

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
     (Kind : Message_Kinds.Message_Subkind;
      Orig_Checks : Check_Kinds_Array := Check_Kinds_Array_Default;
      Tag  : String := "";
      Sep  : String := ",";
      HTML : Boolean := False) return String
   is
      function To_String (Ids : CWE_Id_Array; HTML : Boolean) return String;
      --  Add html reference if HTML is True and transform into a string

      function CWE_Image (Id : Positive; HTML : Boolean) return String;
      --  Return an image of CWE Id, taking into account HTML.

      function Image (Val : Natural) return String;
      --  Return an image of Val with no leading space.

      function CWE_Image (Id : Positive; HTML : Boolean) return String is
         S : constant String := Image (Id);
      begin
         if HTML then
            return "<a class=""reference external"" href=""" &
              "http://cwe.mitre.org/data/definitions/" & S &
              ".html"">" & S & "</a>";
         else
            return S;
         end if;
      end CWE_Image;

      function Image (Val : Natural) return String is
         Result : constant String := Natural'Image (Val);
      begin
         return Result (Result'First + 1 .. Result'Last);
      end Image;

      function To_String (Ids : CWE_Id_Array; HTML : Boolean) return String is
      begin
         case Ids'Length is
            when 0 => return "";
            when 1 => return CWE_Image (Ids (Ids'First), HTML);
            when others =>
               return CWE_Image (Ids (Ids'First), HTML) & Sep &
                 To_String (Ids (Ids'First + 1 .. Ids'Last), HTML);
         end case;
      end To_String;

   begin
      return To_String
         (CWE_Ids (Kind, Orig_Checks, Tag), HTML);
   end CWE_Ids;

   function CWE_Ids
     (Kind : Message_Kinds.Message_Subkind;
      Orig_Checks : Check_Kinds_Array := Check_Kinds_Array_Default;
      Tag  : String := "") return CWE_Id_Array
   is
      package CWE_Id_Sets is new
         Ada.Containers.Ordered_Sets (Positive);
      Result : CWE_Id_Sets.Set;
      procedure Include_Corresponding_CWE
        (Kind : Message_Kinds.Message_Subkind);

      procedure Include_Corresponding_CWE
         (Kind : Message_Kinds.Message_Subkind)
      is
      begin
         case Kind is
            when Invalid_Check =>
               Result.Include (457);
            when Invalid_Or_Null_Check =>
               Result.Include (476);
            when Divide_By_Zero_Check =>
               Result.Include (369);
            when Array_Indexing_Check =>
               --  Return only the main id to avoid clutering the output
               --  120,124-127,129-131
               Result.Include (120);
            when Tag_Check | Type_Variant_Check =>
               --  136,137
               Result.Include (136);
            when Numeric_Range_Check =>
               Result.Include (682);
            when Numeric_Overflow_Check |
                 User_Assign_Stm_Check |
                 Pre_Assign_Stm_Check |
                 Post_Assign_Stm_Check =>
               --  190,191;
               Result.Include (190);
            when Unlocked_Reentrant_Update_Error |
                 Unlocked_Shared_Daemon_Update_Error =>
               --  362,366,820
               Result.Include (362);
            when Mismatched_Locked_Update_Error =>
               --  362,366,821
               Result.Include (821);
            when Dead_Store_Warning |
                 Dead_Outparam_Store_Warning |
                 Same_Value_Dead_Store_Warning |
                 Potentially_Dead_Store_Warning =>
               Result.Include (563);
            when Dead_Block_Warning |
                 Dead_Edge_Warning |
                 Plain_Dead_Edge_Warning |
                 Unrepeatable_While_Loop_Warning =>
               Result.Include (561);
            when False_Dead_Edge_Warning |
                 False_Condition_Dead_Edge_Warning =>
               Result.Include (570);
            when True_Dead_Edge_Warning |
                 True_Condition_Dead_Edge_Warning =>
               Result.Include (571);
            when Infinite_Loop_Warning =>
               Result.Include (835);
            when External_Message_Subkind =>
               if Tag = "length check" then
                  Result.Include (120);
               end if;
            when others =>
               null;
         end case;
      end Include_Corresponding_CWE;
   begin
      if Kind = Precondition_Check then
         for Check in Orig_Checks'Range loop
            if Orig_Checks (Check) then
               Include_Corresponding_CWE (Check);
            end if;
         end loop;
      else
         Include_Corresponding_CWE (Kind);
      end if;

      declare
         CWE_Ids : CWE_Id_Array (1 .. Integer (Result.Length));
         I : Positive := 1;
      begin
         for Id of Result loop
            CWE_Ids (I) := Id;
            I := I + 1;
         end loop;

         return CWE_Ids;
      end;
   end CWE_Ids;

   function Is_Security_Relevant
     (Kind          : Message_Kinds.BE_Message_Subkind;
      Msg           : String;
      Runtime_Check : Boolean) return Boolean is
   begin
      case Kind is
         when Invalid_Check |
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
      Some_Check          : Boolean := False;
      Range_Checks_In_Set : Boolean := False;
      Primary_Checks      : Check_Kinds_Array := Check_Kinds_Array_Default;

   begin
      --  Fill in the Primary_Checks, postponing invalid and overflow for this
      --  first step. See second loop below, where we give those checks a low
      --  precedence in annotations output relative to range checks and other
      --  checks.

      for C in Check_Subkind loop
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
                  Some_Check := True;

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
                  Some_Check := True;

               when Numeric_Range_Check
                  | Array_Indexing_Check
                  | Type_Variant_Check
               =>
                  Primary_Checks (C) := True;
                  Range_Checks_In_Set := True;
                  Some_Check := True;
            end case;
         end if;
      end loop;

      --  Add invalid and overflow if needed:
      --  - range check/discriminant check/array index check hide overflow
      --  - any check hides validity

      for C in Check_Subkind loop
         if Original_Checks (C) then
            case C is
               when Invalid_Check =>
                  if not Some_Check then
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
     (S               : String;
      For_HTML_Output : Boolean := True) return String
   is
      --  TBD: Should we handle various GNAT name encodings such as Uxx
      --       to mean Latin-1 character with hex code "xx"?

      Exponent : Natural := 0;
      Offset   : Integer := 0;

      function Attribute_Img (Is_Negative : Boolean) return String is
        (if Is_Negative then "'First" else "'Last");

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
         elsif Offset < 0 xor Is_Negated then
            --  Precede with '-'
            return '-' & Image (abs (Offset));
         else
            --  Precede with '+'
            return '+' & Image (abs (Offset));
         end if;
      end Offset_Part;

      Is_Negative : Boolean;
      I           : Integer := S'First;
      Prev        : Integer;

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

                        function Compute_Offset
                          (Ref  : Natural) return Integer;

                        function Compute_Offset
                          (Ref  : Natural) return Integer
                        --  For a n bits integer boundary, Ref must be
                        --  2^n mod 100
                        is
                           Char_0 : constant Natural := Character'Pos ('0');
                           Tail   : constant Natural :=
                              (Character'Pos (S (J - 2)) - Char_0) * 10 +
                              Character'Pos (S (J - 1)) - Char_0;

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

                           when 39 | 51 =>
                              pragma Style_Checks (Off);
                              if Head = "170_141_183_460_469_231_731_687_303_715_884_105_7"
                                or else Head = "1701411834604692317316873037158841057"
                              then
                                 Exponent := 127;
                                 Offset := Compute_Offset (28);
                              elsif
                                Head = "340_282_366_920_938_463_463_374_607_431_768_211_4"
                                or else Head = "3402823669209384634633746074317682114"
                              then
                                 Exponent := 128;
                                 Offset := Compute_Offset (56);
                              end if;
                              pragma Style_Checks (On);

                           when others =>
                              null;
                        end case;

                        if Exponent = 0 then
                           --  Nothing special
                           I := J - 1;  --  skip past this number and exit loop
                           exit Loop_Over_Digits;

                        else
                           Is_Negative := I > S'First and then S (I - 1) = '-';
                           Prev := I - 1;

                           if Exponent in 15 | 31 | 63 | 127 then
                              if Is_Negative then
                                 Prev := I - 2;
                              else
                                 Offset := Offset + 1;
                              end if;
                           end if;

                           return S (S'First .. Prev) &
                             (case Exponent is
                                 when 15 =>
                                    "Integer_16" & Attribute_Img (Is_Negative),
                                 when 31 =>
                                    "Integer_32" & Attribute_Img (Is_Negative),
                                 when 63 =>
                                    "Integer_64" & Attribute_Img (Is_Negative),
                                 when 127 =>
                                   "Integer_128" &
                                   Attribute_Img (Is_Negative),
                                 when others =>
                                    (if For_HTML_Output
                                     then "2<SUP>" & Image (Exponent) &
                                          "</SUP>"
                                     else "2**" & Image (Exponent))) &
                             Offset_Part (Is_Negated => Is_Negative) &
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
               end if;

            when others =>
               null;
         end case;

         I := I + 1;
      end loop;

      --  No improvement necessary
      return S;
   end Improve_Number_Readability_In_Messages;

   ------------

   function Is_Stored_In_DB_Method_Annotation
     (Subkind : Message_Subkind)
      return    Boolean
   is
   --  same as above, but only keep certain messages
   begin
      return Subkind = Procedure_Annotation
        or else Subkind = End_Procedure_Annotation
        or else Subkind in Pre_Post_Annotation_Subkind
        or else Subkind in In_Out_Annotation_Subkind
        or else Subkind = Locally_Unused_Store_Annotation
        or else Subkind = Unknown_Call_Annotation
        or else Subkind = Test_Vector_Annotation;
   end Is_Stored_In_DB_Method_Annotation;

end Message_Kinds;
