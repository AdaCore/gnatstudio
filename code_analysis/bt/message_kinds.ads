------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  This package isolates the enumeration of message_kinds produced by the
--  the backend and potential other checkers.

--  IMPORTANT Note: do NOT add any with clause in this package, so that
--  Message_Kinds can be used as a standalone package, with no dependencies.

package Message_Kinds is

   type Message_Subkind is
   --  Each Checker_Message_Kind has a Subkind field, which tells whether it's
   --  an annotation or an error, and which kind of annotation or error.
   --  There is exactly one Module_Annotation/Procedure_Annotation per SCIL
   --  module/procedure; these aren't really "messages", but just
   --  placeholders to hold the name and source position.
   --  The Module_Annotation comes before all Procedure_Annotations
   --  for procedures in that module.
   --  The Procedure_Annotation comes before all other messages
   --  for that procedure.

   --  Note: avoid deleting message kinds so that old messages stay valid.

   --  Note: adding a new message subkind may require adding a corresponding
   --  new element to the enumeration type
   --  BE.SCIL.Auxiliary_Types.Annotation_Check_Kind.
   --  For details, see lengthy comment in
   --  BE.PVP.Mess.User_Review_For_Message.Kinds_Matching_Table_Is_Ok

   --  When adding a new kind, please also consider updating the various
   --  Is_xxx functions below, in particular Is_Documented_Kind.

     (
   --  BE_Message_Subkind
   --  Annotation_Subkind
   --  Place_Holder_Subkind

      Module_Annotation,
      Procedure_Annotation,
      End_Module_Annotation, --  TBD: Not sure of exact rules for these.
      End_Procedure_Annotation, --  TBD.

   --  Place_Holder_Subkind end
   --  Method_Annotation_Subkind
   --  In_Out_Annotation_Subkind

      Input_Annotation,
      Output_Annotation,

   --  In_Out_Annotation_Subkind end

      New_Obj_Annotation,

   --  Pre_Post_Annotation_Subkind

      Precondition_Annotation,
      Presumption_Annotation,
      Postcondition_Annotation,

   --  Pre_Post_Annotation_Subkind end

      Unknown_Call_Annotation,
      Test_Vector_Annotation,

      --  used to represent the obj_ids that have been modified but not used
      --  locally (do not appear in the listing)
      Locally_Unused_Store_Annotation,

   --  Method_Annotation_Subkind end
   --  Annotation_Subkind end
   --  Informational_Subkind

      --  Implicit warnings
      Non_Analyzed_Call_Warning,    --  We are making a call that is
      --  "too_complex"
      Unknown_Call_Warning,      --  We are making a call on an unknown proc

      Dead_Block_Continuation_Warning,  --  Block that is dead because
      --  its predecessors are dead.

      --  limitation warnings
      Analyzed_Module_Warning,           --  info
      Non_Analyzed_Module_Warning,       --  module was poisoned
      Non_Analyzed_Procedure_Warning,    --  procedure was poisoned
      Incompletely_Analyzed_Procedure_Warning, --  incomplete analysis

   --  Informational_Subkind end
   --  Warning_Subkind
   --  Suspicious_Precondition_Subkind

      Suspicious_Precondition_Warning,       --  NOTE: No longer used
      Suspicious_Range_Precondition_Warning, --  Precondition looks fishy
      Suspicious_First_Precondition_Warning, --  Precondition looks fishy

   --  Suspicious_Precondition_Subkind end

      Suspicious_Input_Warning,              --  Out param read before assigned
      Suspicious_Constant_Operation_Warning, --  Operation computes a constant
      Unread_In_Out_Parameter_Warning,       --  Mode in out could be mode out
      Unassigned_In_Out_Parameter_Warning,   --  In out param never assigned

   --  Dead_Store_Subkind

      Dead_Store_Warning,
      Dead_Outparam_Store_Warning,    --  assign into outparam
      Potentially_Dead_Store_Warning,  --  assign into dmod
      Same_Value_Dead_Store_Warning,

   --  Dead_Store_Subkind end
   --  Dead_Control_Flow_Subkind

      Dead_Block_Warning,  --  "Interesting" Basic block is dead
      Infinite_Loop_Warning,  --  Infinite loop

   --  Dead_Edge_Subkind
   --  Decision_Dead_Edge_Subkind

      Dead_Edge_Warning, --  unused, replaced with the following warnings
      Plain_Dead_Edge_Warning, --  Test leading to interesting Basic Block
      --  always goes the same way
      True_Dead_Edge_Warning,  --  A test is always "true", so that the other
      --  edge is dead
      False_Dead_Edge_Warning, --  A test is always "false", so that the other
      --  edge is dead

   --  Decision_Dead_Edge_Subkind end
   --  Condition_Dead_Edge_Subkind

      True_Condition_Dead_Edge_Warning,  --  A condition is always "true", so
      --  that the other edge is dead
      False_Condition_Dead_Edge_Warning, --  A condition is always "false", so
      --  that the other edge is dead
      Unrepeatable_While_Loop_Warning,
      --  A while-loop's body will never execute more than once

   --  Condition_Dead_Edge_Subkind end
   --  Dead_Edge_Subkind end
   --  Dead_Control_Flow_Subkind end
   --  Always_Fail_Subkind

      Procedure_Does_Not_Return_Error,  --  Procedure never returns
      Check_Fails_On_Every_Call_Error,  --  Procedure fails a check

   --  Always_Fail_Subkind end
   --  Warning_Subkind end
   --  Race_Condition_Subkind

      Unlocked_Reentrant_Update_Error,
      Unlocked_Shared_Daemon_Update_Error,
      Mismatched_Locked_Update_Error,

   --  Race_Condition_Subkind end
   --  Check_Subkind
   --  Pre_Or_Post_Check_Subkind

      --  Implicit checks.  See the documentation of Check_Stm in
      --  SCIL.Statements for the semantics of these.
      Precondition_Check,    --  Must be first check so subranges work
      Postcondition_Check,

   --  Pre_Or_Post_Check_Subkind end
   --  Local_Check_Subkind

      User_Precondition_Check,
      Invalid_Check,
      Invalid_Or_Null_Check,
      Freed_Check,  --  Invalid access value due to unchecked_deallocation
      Divide_By_Zero_Check,
      Boolean_Check,
      Non_Neg_Check,
      Negative_Exponent_Check,

   --  Assign_Stm_Check_Subkind

      User_Assign_Stm_Check,
      Pre_Assign_Stm_Check,  --  Implicitly generated pre-call assignments
      Post_Assign_Stm_Check, --  ...post-call...

   --  Assign_Stm_Check_Subkind end

      Aliasing_Check,        --  For checking parameter aliasing

      --  Explicit checks (corresponding to Check_Stm/Check_Exp).
      Raise_Check, --  Unconditional explicit raise/throw
      Conditional_Raise_Check, --  Conditional raise/throw (presuming FE
      --  recognizes user-written "if ... then raise")
      Array_Indexing_Check, --  Array bounds or length check
      Assertion_Check, --  Assert statement/pragma
      Numeric_Overflow_Check, --  Check against physical bounds of type
      Numeric_Range_Check, --  Check against declared range
      Floating_Point_Underflow_Check,
      Type_Variant_Check, --  Variant record check
      Tag_Check, --  Type tag check

   --  Local_Check_Subkind end
   --  Check_Subkind end
   --  BE_Message_Subkind end

   --  External_Message_Subkind

      --  GNAT Warning messages
      GNAT_Warning,

      --  GNATcheck messages
      GNATcheck,

      --  LAL checkers
      LAL_Checkers);

   --  Mesagge_Subkind ranges

   subtype BE_Message_Subkind is Message_Subkind range
     Module_Annotation .. Tag_Check;
   subtype External_Message_Subkind is Message_Subkind range
     GNAT_Warning .. Message_Subkind'Last;

   --  BE_Message_Subkind main ranges

   subtype Annotation_Subkind is BE_Message_Subkind range
     Module_Annotation .. Locally_Unused_Store_Annotation;
   subtype Informational_Subkind is BE_Message_Subkind range
     --  An informational message is NOT counted in the error counts,
     --  nor is it part of the next/prev chain in the message-window.
     --  However, it is "printable", and if you click on it, in the
     --  source window, an informational message will be printed in the
     --  message window.
     Non_Analyzed_Call_Warning .. Incompletely_Analyzed_Procedure_Warning;
   subtype Warning_Subkind is BE_Message_Subkind range
     Suspicious_Precondition_Warning .. Check_Fails_On_Every_Call_Error;
   subtype Race_Condition_Subkind is BE_Message_Subkind range
     Unlocked_Reentrant_Update_Error .. Mismatched_Locked_Update_Error;
   subtype Check_Subkind is BE_Message_Subkind range
     --  TBD rename in Subkind
     Precondition_Check .. Tag_Check;

   --  Annotation_Subkind ranges

   subtype Place_Holder_Subkind is Annotation_Subkind range
     Annotation_Subkind'First .. End_Procedure_Annotation;
   subtype Method_Annotation_Subkind is Annotation_Subkind range
     Input_Annotation .. Annotation_Subkind'Last;

   --  Method_Annotation_Subkind ranges

   subtype In_Out_Annotation_Subkind is Method_Annotation_Subkind range
     Input_Annotation .. Output_Annotation;
   subtype Pre_Post_Annotation_Subkind is Method_Annotation_Subkind range
     Precondition_Annotation .. Postcondition_Annotation;

   --  Warning_Subkind ranges

   subtype Suspicious_Precondition_Subkind is Warning_Subkind range
     Suspicious_Precondition_Warning ..
       Suspicious_First_Precondition_Warning;
   subtype Dead_Store_Subkind is Warning_Subkind range
     Dead_Store_Warning .. Same_Value_Dead_Store_Warning;
   subtype Dead_Control_Flow_Subkind is Warning_Subkind range
     Dead_Block_Warning .. Unrepeatable_While_Loop_Warning;
   subtype Always_Fail_Subkind is Warning_Subkind range
     Procedure_Does_Not_Return_Error .. Check_Fails_On_Every_Call_Error;

   --  Dead_Control_Flow_Subkind ranges

   subtype Dead_Edge_Subkind is Dead_Control_Flow_Subkind range
     Dead_Edge_Warning .. Unrepeatable_While_Loop_Warning;

   --  Dead_Edge_Subkind ranges
   subtype Decision_Dead_Edge_Subkind is Dead_Edge_Subkind range
     Dead_Edge_Warning .. False_Dead_Edge_Warning;
   subtype Condition_Dead_Edge_Subkind is Dead_Edge_Subkind range
     True_Condition_Dead_Edge_Warning .. Unrepeatable_While_Loop_Warning;

   --  Check_Subkind ranges

   subtype Pre_Or_Post_Check_Subkind is Check_Subkind range
     Precondition_Check .. Postcondition_Check;
   subtype Local_Check_Subkind is Check_Subkind
     with Static_Predicate =>
       Local_Check_Subkind not in Pre_Or_Post_Check_Subkind;

   --  Local_Check_Subkind ranges

   subtype Assign_Stm_Check_Subkind is Local_Check_Subkind range
     User_Assign_Stm_Check .. Post_Assign_Stm_Check;

   --  transversal BE_Message_Subkind ranges

   subtype Error_Subkind is BE_Message_Subkind
     with Static_Predicate => Error_Subkind in
       Check_Subkind | Race_Condition_Subkind | Always_Fail_Subkind;

   subtype Warning_Or_Error_Subkind is BE_Message_Subkind
     with Static_Predicate => Warning_Or_Error_Subkind in
                              Informational_Subkind | Warning_Subkind |
                              Race_Condition_Subkind | Check_Subkind;

   subtype All_Checks_With_External_Subkind is Message_Subkind
     with Static_Predicate => All_Checks_With_External_Subkind in
       Check_Subkind | External_Message_Subkind;

   subtype Countable_Subkind is Message_Subkind
     --  messages to be counted in the count of *all* messages. This includes
     --  race condition messages, dead stores, etc.
     with Static_Predicate =>
       Countable_Subkind not in Annotation_Subkind;

   --  end of subkind ranges

   type Check_Kinds_Array is array (Check_Subkind) of Boolean;
   pragma Pack (Check_Kinds_Array);
   Check_Kinds_Array_Default  : constant Check_Kinds_Array :=
     (others => False);
   Check_Kinds_Array_No_Check : constant Check_Kinds_Array :=
     (others => False);
   Check_Kinds_String_Default : constant String := "";

   type Message_Subkind_Set is array (Message_Subkind) of Boolean;

   function Is_Documented_Kind (M : Message_Subkind) return Boolean is
     (case M is
         when Non_Analyzed_Call_Warning ..
             Incompletely_Analyzed_Procedure_Warning
           | Suspicious_First_Precondition_Warning ..
             Infinite_Loop_Warning
           | Plain_Dead_Edge_Warning .. True_Condition_Dead_Edge_Warning
           | Procedure_Does_Not_Return_Error .. Invalid_Or_Null_Check
           | Divide_By_Zero_Check
           | Aliasing_Check .. Numeric_Range_Check
           | Type_Variant_Check | Tag_Check  => True,
         when others                         => False);
   --  Lists all warning/checks/info messages that should be documented

   type CWE_Id_Array is array (Positive range <>) of Positive;

   function CWE_Ids
     (Kind : Message_Subkind;
      Msg  : String := "";
      HTML : Boolean := False) return String;
   function CWE_Ids
     (Kind : Message_Subkind;
      Msg  : String := "") return CWE_Id_Array;
   --  Return the set of applicate CWE ids for Kind, or a null array if none.
   --  Msg if not null is the message string associated with the message
   --  which can be used to e.g. differentiate between precondition messages.
   --  If HTML is True, return an HTML string, with links to relevant
   --  cwe.org URLs.

   function Is_Security_Relevant
     (Kind          : BE_Message_Subkind;
      Msg           : String;
      Runtime_Check : Boolean) return Boolean;
   --  Return True if the given Kind/Msg is relevant for security.
   --  Runtime_Check tells whether runtime checks (e.g. range/overflow/index)
   --  should be considered as security relevant or not.

   function Primary_Original_Checks
     (Original_Checks : Check_Kinds_Array) return Check_Kinds_Array;
   --  Returns a subset of Original_Checks with only the check_kinds
   --  with the highest precedence (which add the strictest constraints)

   function Improve_Number_Readability_In_Messages
     (S      : String;
      For_HTML_Output : Boolean := True)
      return   String;
   --  Make various improvements to numbers, such as
   --  replacing near powers-of-2 by 2<sup>X +/- n, or
   --  Integer_xx'First/Last +/- n.
   --  If For_HTML_Output is True, will use HTML (e.g. "2<sup>X"), otherwise
   --  Text (e.g. "2**X").

   function Is_Stored_In_DB_Method_Annotation
     (Subkind : Message_Subkind)
      return    Boolean;
   --  same as above, but only keep certain messages (no input/output)

end Message_Kinds;
