------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                        Copyright (C) 2013-2015, AdaCore                  --
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
            return "124-127,129-132,135,193";
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
              Unlocked_Shared_Daemon_Update_Error |
              Mismatched_Locked_Update_Error =>
            return "366,367,373,374";
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
               return "118,124-132,135-137,190-193,197,252,253,476";
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

end Message_Kinds;
