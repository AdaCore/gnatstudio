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

   --  Each new checker needs to add a value in the Checker type; complete
   --  Message_Subkind with new corresponding subkinds; define a subtype of
   --  Message_Subkind with these new values; define a new instance of
   --  Checker_Message_Kind with discriminant type the new subtype of
   --  Message_Sukbind; define and register a reader function though
   --  DBIO.Xml_Readers.Register_Reader
   type Checker is (Backend, GNAT);

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

     (Module_Annotation,
      Procedure_Annotation,
      End_Module_Annotation, --  TBD: Not sure of exact rules for these.
      End_Procedure_Annotation, --  TBD.
      Input_Annotation,
      Output_Annotation,
      New_Obj_Annotation,
      Precondition_Annotation,
      Presumption_Annotation,
      Postcondition_Annotation,

      --  another annotation:
      Unknown_Call_Annotation,

      --  another annotation:
      Test_Vector_Annotation,

   --  Implicit warnings
      Non_Analyzed_Call_Warning,    --  We are making a call that is
                                    --  "too_complex"
      Suspicious_Precondition_Warning,       --  NOTE: No longer used
      Suspicious_Range_Precondition_Warning, --  Precondition looks fishy
      Suspicious_First_Precondition_Warning, --  Precondition looks fishy
      Suspicious_Input_Warning,              --  Out param read before assigned
      Suspicious_Constant_Operation_Warning, --  Operation computes a constant
      Unread_In_Out_Parameter_Warning,       --  Mode in out could be mode out
      Unassigned_In_Out_Parameter_Warning,   --  In out param never assigned

   --  Implicit checks.  See the documentation of Check_Stm in
   --  SCIL.Statements for the semantics of these.
      Precondition_Check,    --  Must be first check so subranges work
      Postcondition_Check,
      User_Precondition_Check,
      Invalid_Check,
      Invalid_Or_Null_Check,
      Freed_Check,  --  Invalid access value due to unchecked_deallocation
      Divide_By_Zero_Check,
      Boolean_Check,
      Non_Neg_Check,
      Negative_Exponent_Check,
      User_Assign_Stm_Check,
      Pre_Assign_Stm_Check,  --  Implicitly generated pre-call assignments
      Post_Assign_Stm_Check, --  ...post-call...
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

      Procedure_Does_Not_Return_Error,  --  Procedure never returns
      Check_Fails_On_Every_Call_Error,  --  Procedure fails a check

      Unlocked_Reentrant_Update_Error,
      Unlocked_Shared_Daemon_Update_Error,
      Mismatched_Locked_Update_Error,

      Unknown_Call_Warning,      --  We are making a call on an unknown proc

      Dead_Store_Warning,
      Dead_Outparam_Store_Warning,    --  assign into outparam
      Potentially_Dead_Store_Warning,  --  assign into dmod
      Same_Value_Dead_Store_Warning,

      Dead_Block_Warning,  --  "Interesting" Basic block is dead
      Infinite_Loop_Warning,  --  Infinite loop
      Dead_Edge_Warning, --  unused, replaced with the following warnings
      Plain_Dead_Edge_Warning, --  Test leading to interesting Basic Block
   --  always goes the same way
      True_Dead_Edge_Warning,  --  A test is always "true", so that the other
   --  edge is dead
      False_Dead_Edge_Warning, --  A test is always "false", so that the other
   --  edge is dead
      True_Condition_Dead_Edge_Warning,  --  A condition is always "true", so
   --  that the other edge is dead
      False_Condition_Dead_Edge_Warning, --  A condition is always "false", so
   --  that the other edge is dead
      Unrepeatable_While_Loop_Warning,
   --  A while-loop's body will never execute more than once

   --  used to represent the obj_ids that have been modified but not used
   --  locally (do not appear in the listing)
      Locally_Unused_Store_Annotation,
      Dead_Block_Continuation_Warning,  --  Block that is dead because
   --  its predecessors are dead.
      Local_Lock_Of_Global_Object,  --  message detected during race_condition

   --  limitation warnings
      Analyzed_Module_Warning,           --  info
      Non_Analyzed_Module_Warning,       --  module was poisoned
      Non_Analyzed_Procedure_Warning,    --  procedure was poisoned
      Incompletely_Analyzed_Procedure_Warning, --  incomplete analysis

   --  Two security-related checks
      SQL_Injection_Check,  --  using tainted data in an SQL command
      XSS_Check,  --  Cross-site scripting; using tainted data in HTML output

   --  GNAT Warning messages:
      GW_Default, --  Default GNAT Warnings

      --  -gnatw...
      GW_B,       --  Warn_On_Bad_Fixed_Value
      GW_C,       --  Constant_Condition_Warnings
      GW_D,       --  Warn_On_Dereference
      GW_F,       --  Check_Unreferenced_Formals
      GW_G,       --  Warn_On_Unrecognized_Pragma
      GW_H,       --  Warn_On_Hiding
      GW_I,       --  Implementation_Unit_Warnings
      GW_J,       --  Warn_On_Obsolescent_Feature
      GW_K,       --  Warn_On_Constant
      GW_L,       --  Elab_Warnings
      GW_M,       --  Warn_On_Modified_Unread
      GW_O,       --  Address_Clause_Overlay_Warnings
      GW_P,       --  Ineffective_Inline_Warnings
      GW_Q,       --  Warn_On_Questionable_Missing_Parens
      GW_R,       --  Warn_On_Redundant_Constructs
      GW_T,       --  Warn_On_Deleted_Code
      GW_U,       --  Check_Unreferenced/Withs/Unreferenced_Formals
      GW_V,       --  Warn_On_No_Value_Assigned
      GW_W,       --  Warn_On_Assumed_Low_Bound
      GW_X,       --  Warn_On_Export_Import
      GW_Y,       --  Warn_On_Ada_2005/2012_Compatibility
      GW_Z,       --  Warn_On_Unchecked_Conversion
      GW_Dot_A,   --  Warn_On_Assertion_Failure
      GW_Dot_B,   --  Warn_On_Biased_Representation
      GW_Dot_C,   --  Warn_On_Unrepped_Components
      GW_Dot_F,   --  Warn_On_Elab_Access  # not in .e
      GW_Dot_H,   --  Warn_On_Record_Holes
      GW_Dot_I,   --  Warn_On_Overlap
      GW_Dot_J,   --  Warn_On_Late_Primitives
      GW_Dot_K,   --  Warn_On_Standard_Redefinition
      GW_Dot_M,   --  Warn_On_Suspicious_Modulus_Value
      GW_Dot_O,   --  Warn_On_All_Unread_Out_Parameters
      GW_Dot_P,   --  Warn_On_Parameter_Order
      GW_Dot_Q,   --  Warn_On_Questionable_Layout
      GW_Dot_R,   --  Warn_On_Object_Renames_Function
      GW_Dot_S,   --  Warn_On_Overridden_Size
      GW_Dot_T,   --  Warn_On_Suspicious_Contract
      GW_Dot_U,   --  Warn_On_Unordered_Enumeration_Type
      GW_Dot_W,   --  Warn_On_Warnings_Off
      GW_Dot_X,   --  Warn_On_Non_Local_Exception
      GW_Dot_Z    --  Warn_On_Size_Alignment
      );

   subtype BE_Message_Subkind is Message_Subkind range
     Module_Annotation .. XSS_Check;
   subtype GW_Message_Subkind is Message_Subkind range
     GW_Default .. GW_Dot_Z;

   --  NOTE: These subranges are generally *not* to be used
   --       to distinguish, e.g., "informational" from "warning" messages,
   --       because we no longer guarantee that all messages of the
   --       same category will get contiguous enumeration literals
   --       (because we want to avoid invalidating existing databases).
   --       Use functions Is_Warning, Is_Warning_Or_Check,
   --       Is_Informational, etc.  for that purpose (see functions below).
   --       Those functions will make use of these subranges as appropriate.

   subtype Place_Holder_Subkind is BE_Message_Subkind range
     Module_Annotation .. End_Procedure_Annotation;
   subtype Annotation_Subkind is BE_Message_Subkind range
      Module_Annotation .. Postcondition_Annotation;
   subtype Method_Annotation_Subkind is BE_Message_Subkind range
     Input_Annotation .. Postcondition_Annotation;
   subtype Pre_Post_Annotation_Subkind is BE_Message_Subkind range
     Precondition_Annotation .. Postcondition_Annotation;
   subtype In_Out_Annotation_Subkind is BE_Message_Subkind range
     Input_Annotation .. Output_Annotation;
   --  cannot have this subtype as the range is no longer contiguous
   --  subtype Warning_Subkind is BE_Message_Subkind
   --  range Unknown_Call_Warning .. Suspicious_Precondition_Warning;
   subtype Suspicious_Precondition_Subkind is BE_Message_Subkind range
     Suspicious_Precondition_Warning ..
      Suspicious_First_Precondition_Warning;
   subtype Pre_Or_Post_Check is BE_Message_Subkind range
      Precondition_Check .. Postcondition_Check;
   subtype Error_Subkind is BE_Message_Subkind range
      Precondition_Check .. Mismatched_Locked_Update_Error;
   subtype Warning_Or_Error_Subkind is BE_Message_Subkind range
     Non_Analyzed_Call_Warning .. Unrepeatable_While_Loop_Warning;
   subtype Check_Kind_Enum is Error_Subkind range
      Precondition_Check .. Tag_Check;
   subtype Local_Check is Check_Kind_Enum
   --  all but Precondition_Check and Postcondition_Check
     range
      Check_Kind_Enum'Succ (Postcondition_Check) .. Check_Kind_Enum'Last;
   subtype Assign_Stm_Check is Check_Kind_Enum range
      User_Assign_Stm_Check .. Post_Assign_Stm_Check;
   subtype Race_Condition_Subkind is BE_Message_Subkind range
     Unlocked_Reentrant_Update_Error .. Mismatched_Locked_Update_Error;
   --  Note: do not include the Local_Lock_Of_Global_Object
   subtype Dead_Store_Subkind is BE_Message_Subkind range
      Dead_Store_Warning .. Same_Value_Dead_Store_Warning;
   subtype Dead_Control_Flow_Subkind is BE_Message_Subkind range
     Dead_Block_Warning .. Unrepeatable_While_Loop_Warning;
   subtype Condition_Dead_Edge_Subkind is BE_Message_Subkind range
     True_Condition_Dead_Edge_Warning .. Unrepeatable_While_Loop_Warning;
   subtype Decision_Dead_Edge_Subkind is BE_Message_Subkind range
     Dead_Edge_Warning .. False_Dead_Edge_Warning;
   subtype Dead_Edge_Subkind is BE_Message_Subkind range
      Dead_Edge_Warning .. Unrepeatable_While_Loop_Warning;
   subtype Security_Check_Subkind is BE_Message_Subkind range
     SQL_Injection_Check .. XSS_Check;
   subtype All_Checks_Subkind is BE_Message_Subkind range
      Check_Kind_Enum'First .. Security_Check_Subkind'Last;
   --  This subrange is meant to cover *all* "checks" though
   --  it might not be contiguous
   subtype Non_Checks_Subkind is All_Checks_Subkind range
      All_Checks_Subkind'Succ (Check_Kind_Enum'Last) ..
      All_Checks_Subkind'Pred (Security_Check_Subkind'First);
   --  This subrange covers the "hole" in All_Checks_Subkind

   subtype All_Checks_With_External_Subkinds is Message_Subkind range
     All_Checks_Subkind'First .. Message_Subkind'Last;

   type Check_Kinds_Array is array (Check_Kind_Enum) of Boolean;
   pragma Pack (Check_Kinds_Array);
   Check_Kinds_Array_Default : constant Check_Kinds_Array :=
     (others => False);
   Check_Kinds_String_Default : constant String := "";

   function CWE_Ids
     (Kind : Message_Subkind;
      Msg  : String := "") return String;
   --  Return the set of applicate CWE ids for Kind, or """ if none.
   --  Msg if not null is the message string associated with the message
   --  which can be used to e.g. differentiate between precondition messages.

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

   function Is_Annotation (Subkind : Message_Subkind) return Boolean;
   function Is_Method_Annotation (Subkind : Message_Subkind) return Boolean;
   --  true if subkind is in annotation subkinds, or locally_unused_assignment

   function Is_Stored_In_DB_Method_Annotation
     (Subkind : Message_Subkind)
      return    Boolean;
   --  same as above, but only keep certain messages (no input/output)

   function Is_Warning (Subkind : Message_Subkind) return Boolean;
   --  Return True if message is not considered a check, which
   --  means there is no reason to add "check that ..." in front
   --  of the text of the message.  Also, these messages are
   --  *not* counted as one of the "check-related" messages.

   function Is_Check (Subkind : Message_Subkind) return Boolean;
   --  Returns True if message is a check.

   function Is_Warning_Or_Check (Subkind : Message_Subkind) return Boolean;
   --  Return True if message is to be counted in the count of
   --  all messages.  This includes race condition messages,
   --  dead stores, external checkers, GNAT warnings etc.

   function Is_Informational (Subkind : Message_Subkind) return Boolean;
   --  Return True if "Subkind" is an informational message.
   --  An informational message is NOT counted in the error counts,
   --  nor is it part of the next/prev chain in the message-window.
   --  However, it is "printable", and if you click on it, in the
   --  source window, an informational message will be printed in the
   --  message window.

   function Subkind_To_Checker (Subkind : Message_Subkind) return Checker is
     (case Subkind is
         when BE_Message_Subkind  => Backend,
         when GW_Message_Subkind  => GNAT);
   --  Returns the Checker that issue the given Message_Subkind

end Message_Kinds;
