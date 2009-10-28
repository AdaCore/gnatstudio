"""This plug-in provides routines that insert the text of Ada
language-defined pragmas, as well as GNAT-defined pragmas and attributes.
They are used by contextual menus to insert the name (etc) so that the user
need not remember the name or exactly how to spell it.

Note that you can use the smart completion for completing package names.
"""


import GPS
import string

# returns the input in the letter-casing specified by the user's reserved word preference
def word_case (word):
   pref = string.lower (GPS.Preference ("Ada-Reserved-Casing").get())
   if pref == "upper":
      return string.upper (word)
   elif pref == "mixed":
      return word.title ()
   elif pref == "lower":
      return string.lower (word)
   elif pref == "unchanged":
      # we aren't pretty-printing so we aren't examining their existing code; hence
      # we don't know what 'unchanged' would really mean here.  Therefore we leave it
      # as-is, which means lower case since that's the way this module is written.
      return word
   elif pref == "Smart_Mixed":
      return word.title ()  #since we are only working with "pragma" anyway...
   else:
      return word
   #end if
#end word_case

# returns the input in the letter-casing specified by the user's Identifier preference
def identifier_case (id):
   pref = string.lower (GPS.Preference ("Ada-Ident-Casing").get())
   if pref == "upper":
      return string.upper (id)
   elif pref == "mixed":
      return id.title ()
   elif pref == "lower":
      return string.lower (id)
   elif pref == "unchanged":
      return id
   else:
      # unchanged for Smart_Mixed since that's what we used for input
      return id
   #end if
#end identifier_case

def indented (column):
   return (column * ' ') + (GPS.Preference ("Ada-Indent-Level").get() * ' ')
#end indented

#
# language-defined pragmas
#

def insert_All_Calls_Remote ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" All_Calls_Remote[(library_unit_name)]; -- See E.2.3."));

def insert_Asynchronous ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Asynchronous(local_name); -- See E.4.1."));

def insert_Assert ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Assert ( boolean_EXPRESSION [, static_string_EXPRESSION]);"));

def insert_Atomic ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Atomic(local_name); -- See C.6."));

def insert_Atomic_Components ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Atomic_Components(array_local_name); -- See C.6."));

def insert_Attach_Handler ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Attach_Handler(handler_name, expression); -- See C.3.1."));

def insert_Controlled ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Controlled(first_subtype_local_name); -- See 13.11.3."));

def insert_Convention ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Convention([Convention =>] convention_identifier,[Entity =>] local_name); -- See B.1."));

def insert_Discard_Names ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Discard_Names[([On => ] local_name)]; -- See C.5."));

def insert_Elaborate ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Elaborate(library_unit_name{, library_unit_name}); -- See 10.2.1."));

def insert_Elaborate_All ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Elaborate_All(library_unit_name{, library_unit_name}); -- See 10.2.1."));


def insert_Elaborate_Body ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Elaborate_Body[(library_unit_name)]; -- See 10.2.1."));


def insert_Export ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export( Convention => convention_identifier, Entity => local_name, External_Name => string_expression, Link_Name => string_expression ); -- See B.1."));


def insert_Import ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Import( Convention => convention_identifier, Entity => local_name, External_Name => string_expression, Link_Name => string_expression ); -- See B.1." ));


def insert_Inline ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Inline(name {, name}); -- See 6.3.2."));


def insert_Inspection_Point ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Inspection_Point[(object_name {, object_name})]; -- See H.3.2."));

def insert_Interrupt_Handler ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Interrupt_Handler(handler_name); -- See C.3.1."));

def insert_Interrupt_Priority ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Interrupt_Priority[(expression)]; -- See D.1."));

def insert_Linker_Options ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Linker_Options(string_expression); -- See B.1."));

def insert_List ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" List(identifier); -- See 2.8."));

def insert_Locking_Policy ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Locking_Policy(Ceiling_Locking | policy_identifier); -- See D.3."));

def insert_Normalize_Scalars ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Normalize_Scalars; -- See H.1."));

def insert_Optimize ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Optimize(identifier); -- See 2.8."));

def insert_Pack ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Pack(first_subtype_local_name); -- See 13.2."));

def insert_Page ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Page; -- See 2.8."));

def insert_Preelaborate ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Preelaborate[(library_unit_name)]; -- See 10.2.1."));

def insert_Priority ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Priority(expression); -- See D.1."));

def insert_Pure ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Pure[(library_unit_name)]; -- See 10.2.1."));

def insert_Queuing_Policy ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Queuing_Policy(FIFO_Queuing | Priority_Queuing | policy_identifier); -- See D.4."));

def insert_Remote_Call_Interface ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Remote_Call_Interface[(library_unit_name)]; -- See E.2.3."));

def insert_Remote_Types ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Remote_Types[(library_unit_name)]; -- See E.2.2."));

def insert_Restrictions ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Restrictions(restriction{, restriction}); -- See 13.12."));

def insert_Reviewable ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Reviewable; -- See H.3.1."));

def insert_Shared_Passive ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Shared_Passive[(library_unit_name)]; -- See E.2.1."));

def insert_Storage_Size ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Storage_Size(expression); -- See 13.3."));

def insert_Suppress ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Suppress(identifier [, [On =>] name]); -- See 11.5."));

def insert_Task_Dispatching_Policy ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Task_Dispatching_Policy(FIFO_Within_Priorities | policy_identifier); -- See D.2.2."));

def insert_Unchecked_Union ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Unchecked_Union (first_subtype_LOCAL_NAME);"));

def insert_Volatile ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Volatile(local_name); -- See C.6."));

def insert_Volatile_Components ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Volatile_Components(array_local_name); -- See C.6."));

#
# gnat-defined pragmas
#

def insert_Abort_Defer ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Abort_Defer;"));

def insert_Ada_83 ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Ada_83;"));

def insert_Ada_95 ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Ada_95;"));

def insert_Annotate ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Annotate (IDENTIFIER {, ARG});"));

def insert_Ast_Entry ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" AST_Entry (entry_IDENTIFIER);"));

def insert_C_Pass_By_Copy ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" C_Pass_By_Copy ([Max_Size =>] static_integer_EXPRESSION);"));

def insert_Comment ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Comment (static_string_EXPRESSION);"));

def insert_Compile_Time_Warning ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Compile_Time_Warning (boolean_EXPRESSION, static_string_EXPRESSION);"));

def insert_Complex_Representation ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Complex_Representation ([Entity =>] LOCAL_NAME);"));

def insert_Component_Alignment ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Component_Alignment ([Form =>] ALIGNMENT_CHOICE [, [Name =>] type_LOCAL_NAME]);"));

def insert_Convention_Identifier ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Convention_Identifier ([Name =>]       IDENTIFIER,"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("[Convention =>] convention_IDENTIFIER);"));

def insert_CPP_Constructor ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" CPP_Constructor ([Entity =>] LOCAL_NAME);"));

def insert_Debug ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Debug (PROCEDURE_CALL_WITHOUT_SEMICOLON);"));

def insert_Elaboration_Checks ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Elaboration_Checks (RM | Static);"));

def insert_Eliminate ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Eliminate"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Unit_Name =>]  IDENTIFIER | SELECTED_COMPONENT"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [,[Entity  =>]  IDENTIFIER | SELECTED_COMPONENT | STRING_LITERAL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [,[Parameter_Types =>]  PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [,[Result_Type =>]  result_SUBTYPE_NAME]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [,[Homonym_Number =>]  INTEGER_LITERAL]);"));

def insert_Finalize_Storage_Only ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Finalize_Storage_Only (first_subtype_LOCAL_NAME);"));

def insert_Float_Representation ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Float_Representation (FLOAT_REP);"));

def insert_Ident ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Ident (static_string_EXPRESSION);"));

def insert_Initialize_Scalars ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Initialize_Scalars;"));

def insert_Inline_Always ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Inline_Always (NAME [, NAME]);"));

def insert_Interrupt_State ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Interrupt_State (Name => value, State => SYSTEM | RUNTIME | USER);"));

def insert_Keep_Names ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Keep_Names ([On =>] enumeration_first_subtype_LOCAL_NAME);"));

def insert_License ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" License (Unrestricted | GPL | Modified_GPL | Restricted);"));

def insert_Long_Float ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Long_Float (FLOAT_FORMAT);"));

def insert_Machine_Attribute ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Machine_Attribute"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Attribute_Name =>] string_EXPRESSION"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Entity       =>] LOCAL_NAME);"));

def insert_No_Return ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" No_Return (procedure_LOCAL_NAME);"));

def insert_Obsolescent ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Obsolescent [(static_string_EXPRESSION)];"));

def insert_Polling ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Polling (ON | OFF);"));

def insert_Pure_Function ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Pure_Function ([Entity =>] function_LOCAL_NAME);"));

def insert_Restriction_Warnings ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Restriction_Warnings (restriction_IDENTIFIER {, restriction_IDENTIFIER});"));

def insert_Source_File_Name ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Source_File_Name ([Unit_Name =>] unit_NAME, Spec_File_Name => STRING_LITERAL);"));

def insert_Source_Reference ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Source_Reference (INTEGER_LITERAL, STRING_LITERAL);"));

def insert_Stream_Convert ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Stream_Convert"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity =>] type_LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Read   =>] function_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Write  =>] function_NAME);"));

def insert_Style_Checks ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Style_Checks (string_LITERAL | ALL_CHECKS | On | Off [, LOCAL_NAME]);"));

def insert_Suppress_Exception_Locations ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Suppress_Exception_Locations;"));

def insert_Suppress_Initialization ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Suppress_Initialization ([Entity =>] type_Name);"));

def insert_Task_Info ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Task_Info (expression);"));

def insert_Task_Name ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Task_Name (string_EXPRESSION);"));

def insert_Task_Storage ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Task_Storage"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Task_Type =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Top_Guard =>] static_integer_EXPRESSION);"));

def insert_Time_Slice ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Time_Slice (static_duration_EXPRESSION);"));

def insert_Unimplemented_Unit ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Unimplemented_Unit;"));

def insert_Universal_Data ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Universal_Data [(library_unit_Name)];"));

def insert_Unreferenced ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Unreferenced (local_Name {, local_Name});"));

def insert_Unreserve_All_Interrupts ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Unreserve_All_Interrupts;"));

def insert_Unsuppress ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Unsuppress (IDENTIFIER [, [On =>] NAME]);"));

def insert_Use_VADS_Size ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Use_VADS_Size;"));

def insert_Validity_Checks ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Validity_Checks (string_LITERAL | ALL_CHECKS | On | Off);"));

def insert_Warnings ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Warnings (On | Off [, LOCAL_NAME]);"));

def insert_Weak_External ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Weak_External ([Entity =>] LOCAL_NAME);"));

#
# gnat-defined attributes
#

def insert_Address_Size ():
   GPS.Editor.insert_text (identifier_case ("Standard'Address_Size"));

def insert_Asm_Input ():
   GPS.Editor.insert_text (identifier_case ("Asm_Input"));

def insert_Asm_Output ():
   GPS.Editor.insert_text (identifier_case ("Asm_Output"));

def insert_AST_Entry ():
   GPS.Editor.insert_text (identifier_case ("AST_Entry"));

def insert_Bit ():
   GPS.Editor.insert_text (identifier_case ("Bit"));

def insert_Bit_Position ():
   GPS.Editor.insert_text (identifier_case ("Bit_Position"));

def insert_Code_Address ():
   GPS.Editor.insert_text (identifier_case ("Code_Address"));

def insert_Default_Bit_Order ():
   GPS.Editor.insert_text (identifier_case ("Standard'Default_Bit_Order"));

def insert_Elaborated ():
   GPS.Editor.insert_text (identifier_case ("Elaborated"));

def insert_Elab_Body ():
   GPS.Editor.insert_text (identifier_case ("Elab_Body"));

def insert_Elab_Spec ():
   GPS.Editor.insert_text (identifier_case ("Elab_Spec"));

def insert_Emax ():
   GPS.Editor.insert_text (identifier_case ("Emax"));

def insert_Enum_Rep ():
   GPS.Editor.insert_text (identifier_case ("Enum_Rep"));

def insert_Epsilon ():
   GPS.Editor.insert_text (identifier_case ("Epsilon"));

def insert_Fixed_Value ():
   GPS.Editor.insert_text (identifier_case ("Fixed_Value"));

def insert_Has_Discriminants ():
   GPS.Editor.insert_text (identifier_case ("Has_Discriminants"));

def insert_Img ():
   GPS.Editor.insert_text (identifier_case ("Img"));

def insert_Integer_Value ():
   GPS.Editor.insert_text (identifier_case ("Integer_Value"));

def insert_Large ():
   GPS.Editor.insert_text (identifier_case ("Large"));

def insert_Machine_Size ():
   GPS.Editor.insert_text (identifier_case ("Machine_Size"));

def insert_Mantissa ():
   GPS.Editor.insert_text (identifier_case ("Mantissa"));

def insert_Max_Interrupt_Priority ():
   GPS.Editor.insert_text (identifier_case ("Standard'Max_Interrupt_Priority"));

def insert_Max_Priority ():
   GPS.Editor.insert_text (identifier_case ("Standard'Max_Priority"));

def insert_Maximum_Alignment ():
   GPS.Editor.insert_text (identifier_case ("Standard'Maximum_Alignment"));

def insert_Mechanism_Code ():
   GPS.Editor.insert_text (identifier_case ("Mechanism_Code"));

def insert_Null_Parameter ():
   GPS.Editor.insert_text (identifier_case ("Null_Parameter"));

def insert_Object_Size ():
   GPS.Editor.insert_text (identifier_case ("Object_Size"));

def insert_Passed_By_Reference ():
   GPS.Editor.insert_text (identifier_case ("Passed_By_Reference"));

def insert_Range_Length ():
   GPS.Editor.insert_text (identifier_case ("Range_Length"));

def insert_Safe_Emax ():
   GPS.Editor.insert_text (identifier_case ("Safe_Emax"));

def insert_Safe_Large ():
   GPS.Editor.insert_text (identifier_case ("Safe_Large"));

def insert_Small ():
   GPS.Editor.insert_text (identifier_case ("Small"));

def insert_Storage_Unit ():
   GPS.Editor.insert_text (identifier_case ("Standard'Storage_Unit"));

def insert_Target_Name ():
   GPS.Editor.insert_text (identifier_case ("Standard'Target_Name"));

def insert_Tick ():
   GPS.Editor.insert_text (identifier_case ("Standard'Tick"));

def insert_To_Address ():
   GPS.Editor.insert_text (identifier_case ("System'To_Address"));

def insert_Type_Class ():
   GPS.Editor.insert_text (identifier_case ("Type_Class"));

def insert_Unconstrained_Array ():
   GPS.Editor.insert_text (identifier_case ("Unconstrained_Array"));

def insert_Universal_Literal_String ():
   GPS.Editor.insert_text (identifier_case ("Universal_Literal_String"));

def insert_Unrestricted_Access ():
   GPS.Editor.insert_text (identifier_case ("Unrestricted_Access"));

def insert_VADS_Size ():
   GPS.Editor.insert_text (identifier_case ("VADS_Size"));

def insert_Value_Size ():
   GPS.Editor.insert_text (identifier_case ("Value_Size"));

def insert_Wchar_T_Size ():
   GPS.Editor.insert_text (identifier_case ("Standard'Wchar_T_Size"));

def insert_Word_Size ():
   GPS.Editor.insert_text (identifier_case ("Standard'Word_Size"));

# the "Ada pragmas" submenu

GPS.parse_xml ("""
     <action name="insert All_Calls_Remote" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_All_Calls_Remote()</shell>
     </action>
     <contextual action="insert All_Calls_Remote" >
        <Title>Insert/Standard pragma/All_Calls_Remote</Title>
     </contextual>

     <action name="insert Assert" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Assert()</shell>
     </action>
     <contextual action="insert Assert" >
        <Title>Insert/Standard pragma/Assert</Title>
     </contextual>

     <action name="insert Asynchronous" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asynchronous()</shell>
     </action>
     <contextual action="insert Asynchronous" >
        <Title>Insert/Standard pragma/Asynchronous</Title>
     </contextual>

     <action name="insert Atomic" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Atomic()</shell>
     </action>
     <contextual action="insert Atomic" >
        <Title>Insert/Standard pragma/Atomic</Title>
     </contextual>

     <action name="insert Atomic_Components" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Atomic_Components()</shell>
     </action>
     <contextual action="insert Atomic_Components" >
        <Title>Insert/Standard pragma/Atomic_Components</Title>
     </contextual>

     <action name="insert Attach_Handler" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Attach_Handler()</shell>
     </action>
     <contextual action="insert Attach_Handler" >
        <Title>Insert/Standard pragma/Attach_Handler</Title>
     </contextual>

     <action name="insert Convention" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Convention()</shell>
     </action>
     <contextual action="insert Convention" >
        <Title>Insert/Standard pragma/Convention</Title>
     </contextual>

     <action name="insert Controlled" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Controlled()</shell>
     </action>
     <contextual action="insert Controlled" >
        <Title>Insert/Standard pragma/Controlled</Title>
     </contextual>

     <action name="insert Discard_Names" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Discard_Names()</shell>
     </action>
     <contextual action="insert Discard_Names" >
        <Title>Insert/Standard pragma/Discard_Names</Title>
     </contextual>

     <action name="insert Elaborate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborate()</shell>
     </action>
     <contextual action="insert Elaborate" >
        <Title>Insert/Standard pragma/Elaborate</Title>
     </contextual>

     <action name="insert Elaborate_All" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborate_All()</shell>
     </action>
     <contextual action="insert Elaborate_All" >
        <Title>Insert/Standard pragma/Elaborate_All</Title>
     </contextual>

     <action name="insert Elaborate_Body" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborate_Body()</shell>
     </action>
     <contextual action="insert Elaborate_Body" >
        <Title>Insert/Standard pragma/Elaborate_Body</Title>
     </contextual>

     <action name="insert Export" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export()</shell>
     </action>
     <contextual action="insert Export" >
        <Title>Insert/Standard pragma/Export</Title>
     </contextual>

     <action name="insert Import" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import()</shell>
     </action>
     <contextual action="insert Import" >
        <Title>Insert/Standard pragma/Import</Title>
     </contextual>

     <action name="insert Inline" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inline()</shell>
     </action>
     <contextual action="insert Inline" >
        <Title>Insert/Standard pragma/Inline</Title>
     </contextual>

     <action name="insert Inspection_Point" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inspection_Point()</shell>
     </action>
     <contextual action="insert Inspection_Point" >
        <Title>Insert/Standard pragma/Inspection_Point</Title>
     </contextual>

     <action name="insert Interrupt_Handler" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupt_Handler()</shell>
     </action>
     <contextual action="insert Interrupt_Handler" >
        <Title>Insert/Standard pragma/Interrupt_Handler</Title>
     </contextual>

     <action name="insert Interrupt_Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupt_Priority()</shell>
     </action>
     <contextual action="insert Interrupt_Priority" >
        <Title>Insert/Standard pragma/Interrupt_Priority</Title>
     </contextual>

     <action name="insert Linker_Options" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Linker_Options()</shell>
     </action>
     <contextual action="insert Linker_Options" >
        <Title>Insert/Standard pragma/Linker_Options</Title>
     </contextual>

     <action name="insert List" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_List()</shell>
     </action>
     <contextual action="insert List" >
        <Title>Insert/Standard pragma/List</Title>
     </contextual>

     <action name="insert Locking_Policy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Locking_Policy()</shell>
     </action>
     <contextual action="insert Locking_Policy" >
        <Title>Insert/Standard pragma/Locking_Policy</Title>
     </contextual>

     <action name="insert Normalize_Scalars" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Normalize_Scalars()</shell>
     </action>
     <contextual action="insert Normalize_Scalars" >
        <Title>Insert/Standard pragma/Normalize_Scalars</Title>
     </contextual>

     <action name="insert Optimize" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Optimize()</shell>
     </action>
     <contextual action="insert Optimize" >
        <Title>Insert/Standard pragma/Optimize</Title>
     </contextual>

     <action name="insert Pack" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Pack()</shell>
     </action>
     <contextual action="insert Pack" >
        <Title>Insert/Standard pragma/Pack</Title>
     </contextual>

     <action name="insert Page" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Page()</shell>
     </action>
     <contextual action="insert Page" >
        <Title>Insert/Standard pragma/Page</Title>
     </contextual>

     <action name="insert Preelaborate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Preelaborate()</shell>
     </action>
     <contextual action="insert Preelaborate" >
        <Title>Insert/Standard pragma/Preelaborate</Title>
     </contextual>

     <action name="insert Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Priority()</shell>
     </action>
     <contextual action="insert Priority" >
        <Title>Insert/Standard pragma/Priority</Title>
     </contextual>

     <action name="insert Pure" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Pure()</shell>
     </action>
     <contextual action="insert Pure" >
        <Title>Insert/Standard pragma/Pure</Title>
     </contextual>

     <action name="insert Queuing_Policy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Queuing_Policy()</shell>
     </action>
     <contextual action="insert Queuing_Policy" >
        <Title>Insert/Standard pragma/Queuing_Policy</Title>
     </contextual>

     <action name="insert Remote_Call_Interface" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Remote_Call_Interface()</shell>
     </action>
     <contextual action="insert Remote_Call_Interface" >
        <Title>Insert/Standard pragma/Remote_Call_Interface</Title>
     </contextual>

     <action name="insert Remote_Types" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Remote_Types()</shell>
     </action>
     <contextual action="insert Remote_Types" >
        <Title>Insert/Standard pragma/Remote_Types</Title>
     </contextual>

     <action name="insert Restrictions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Restrictions()</shell>
     </action>
     <contextual action="insert Restrictions" >
        <Title>Insert/Standard pragma/Restrictions</Title>
     </contextual>

     <action name="insert Reviewable" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Reviewable()</shell>
     </action>
     <contextual action="insert Reviewable" >
        <Title>Insert/Standard pragma/Reviewable</Title>
     </contextual>

     <action name="insert Shared_Passive" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Shared_Passive()</shell>
     </action>
     <contextual action="insert Shared_Passive" >
        <Title>Insert/Standard pragma/Shared_Passive</Title>
     </contextual>

     <action name="insert Storage_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_Size()</shell>
     </action>
     <contextual action="insert Storage_Size" >
        <Title>Insert/Standard pragma/Storage_Size</Title>
     </contextual>

     <action name="insert Suppress" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Suppress()</shell>
     </action>
     <contextual action="insert Suppress" >
        <Title>Insert/Standard pragma/Suppress</Title>
     </contextual>

     <action name="insert Task_Dispatching_Policy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Dispatching_Policy()</shell>
     </action>
     <contextual action="insert Task_Dispatching_Policy" >
        <Title>Insert/Standard pragma/Task_Dispatching_Policy</Title>
     </contextual>

     <action name="insert Unchecked_Union" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unchecked_Union()</shell>
     </action>
     <contextual action="insert Unchecked_Union" >
        <Title>Insert/Standard pragma/Unchecked_Union</Title>
     </contextual>

     <action name="insert Volatile" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Volatile()</shell>
     </action>
     <contextual action="insert Volatile" >
        <Title>Insert/Standard pragma/Volatile</Title>
     </contextual>

     <action name="insert Volatile_Components" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Volatile_Components()</shell>
     </action>
     <contextual action="insert Volatile_Components" >
        <Title>Insert/Standard pragma/Volatile_Components</Title>
     </contextual>
""")

# the GNAT-defined pragmas

GPS.parse_xml ("""
     <action name="insert Abort_Defer" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Abort_Defer()</shell>
     </action>
     <contextual action="insert Abort_Defer" >
        <Title>Insert/GNAT pragma/Abort_Defer</Title>
     </contextual>

     <action name="insert Ada_83" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_83()</shell>
     </action>
     <contextual action="insert Ada_83" >
        <Title>Insert/GNAT pragma/Ada_83</Title>
     </contextual>

     <action name="insert Ada_95" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_95()</shell>
     </action>
     <contextual action="insert Ada_95" >
        <Title>Insert/GNAT pragma/Ada_95</Title>
     </contextual>

     <action name="insert Annotate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Annotate()</shell>
     </action>
     <contextual action="insert Annotate" >
        <Title>Insert/GNAT pragma/Annotate</Title>
     </contextual>

     <action name="insert C_Pass_By_Copy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_C_Pass_By_Copy()</shell>
     </action>
     <contextual action="insert C_Pass_By_Copy" >
        <Title>Insert/GNAT pragma/C_Pass_By_Copy</Title>
     </contextual>

     <action name="insert CPP_Constructor" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_CPP_Constructor()</shell>
     </action>
     <contextual action="insert CPP_Constructor" >
        <Title>Insert/GNAT pragma/CPP_Constructor</Title>
     </contextual>

     <action name="insert Comment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Comment()</shell>
     </action>
     <contextual action="insert Comment" >
        <Title>Insert/GNAT pragma/Comment</Title>
     </contextual>

     <action name="insert Compile_Time_Warning" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Compile_Time_Warning()</shell>
     </action>
     <contextual action="insert Compile_Time_Warning" >
        <Title>Insert/GNAT pragma/Compile_Time_Warning</Title>
     </contextual>

     <action name="insert Complex_Representation" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Complex_Representation()</shell>
     </action>
     <contextual action="insert Complex_Representation" >
        <Title>Insert/GNAT pragma/Complex_Representation</Title>
     </contextual>

     <action name="insert Component_Alignment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Component_Alignment()</shell>
     </action>
     <contextual action="insert Component_Alignment" >
        <Title>Insert/GNAT pragma/Component_Alignment</Title>
     </contextual>

     <action name="insert Convention_Identifier" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Convention_Identifier()</shell>
     </action>
     <contextual action="insert Convention_Identifier" >
        <Title>Insert/GNAT pragma/Convention_Identifier</Title>
     </contextual>

     <action name="insert Debug" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Debug()</shell>
     </action>
     <contextual action="insert Debug" >
        <Title>Insert/GNAT pragma/Debug</Title>
     </contextual>

     <action name="insert Elaboration_Checks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaboration_Checks()</shell>
     </action>
     <contextual action="insert Elaboration_Checks" >
        <Title>Insert/GNAT pragma/Elaboration_Checks</Title>
     </contextual>

     <action name="insert Eliminate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Eliminate()</shell>
     </action>
     <contextual action="insert Eliminate" >
        <Title>Insert/GNAT pragma/Eliminate</Title>
     </contextual>

     <action name="insert Finalize_Storage_Only" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Finalize_Storage_Only()</shell>
     </action>
     <contextual action="insert Finalize_Storage_Only" >
        <Title>Insert/GNAT pragma/Finalize_Storage_Only</Title>
     </contextual>

     <action name="insert Float_Representation" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Float_Representation()</shell>
     </action>
     <contextual action="insert Float_Representation" >
        <Title>Insert/GNAT pragma/Float_Representation</Title>
     </contextual>

     <action name="insert Ident" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ident()</shell>
     </action>
     <contextual action="insert Ident" >
        <Title>Insert/GNAT pragma/Ident</Title>
     </contextual>

     <action name="insert Initialize_Scalars" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Initialize_Scalars()</shell>
     </action>
     <contextual action="insert Initialize_Scalars" >
        <Title>Insert/GNAT pragma/Initialize_Scalars</Title>
     </contextual>

     <action name="insert Inline_Always" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inline_Always()</shell>
     </action>
     <contextual action="insert Inline_Always" >
        <Title>Insert/GNAT pragma/Inline_Always</Title>
     </contextual>

     <action name="insert Interrupt_State" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupt_State()</shell>
     </action>
     <contextual action="insert Interrupt_State" >
        <Title>Insert/GNAT pragma/Interrupt_State</Title>
     </contextual>

     <action name="insert Keep_Names" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Keep_Names()</shell>
     </action>
     <contextual action="insert Keep_Names" >
        <Title>Insert/GNAT pragma/Keep_Names</Title>
     </contextual>

     <action name="insert License" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_License()</shell>
     </action>
     <contextual action="insert License" >
        <Title>Insert/GNAT pragma/License</Title>
     </contextual>

     <action name="insert Long_Float" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Long_Float()</shell>
     </action>
     <contextual action="insert Long_Float" >
        <Title>Insert/GNAT pragma/Long_Float</Title>
     </contextual>

     <action name="insert Machine_Attribute" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Machine_Attribute()</shell>
     </action>
     <contextual action="insert Machine_Attribute" >
        <Title>Insert/GNAT pragma/Machine_Attribute</Title>
     </contextual>

     <action name="insert No_Return" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_No_Return()</shell>
     </action>
     <contextual action="insert No_Return" >
        <Title>Insert/GNAT pragma/No_Return</Title>
     </contextual>

     <action name="insert Obsolescent" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Obsolescent()</shell>
     </action>
     <contextual action="insert Obsolescent" >
        <Title>Insert/GNAT pragma/Obsolescent</Title>
     </contextual>

     <action name="insert Polling" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Polling()</shell>
     </action>
     <contextual action="insert Polling" >
        <Title>Insert/GNAT pragma/Polling</Title>
     </contextual>

     <action name="insert Pure_Function" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Pure_Function()</shell>
     </action>
     <contextual action="insert Pure_Function" >
        <Title>Insert/GNAT pragma/Pure_Function</Title>
     </contextual>

     <action name="insert Restriction_Warnings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Restriction_Warnings()</shell>
     </action>
     <contextual action="insert Restriction_Warnings" >
        <Title>Insert/GNAT pragma/Restriction_Warnings</Title>
     </contextual>

     <action name="insert Source_File_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Source_File_Name()</shell>
     </action>
     <contextual action="insert Source_File_Name" >
        <Title>Insert/GNAT pragma/Source_File_Name</Title>
     </contextual>

     <action name="insert Source_Reference" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Source_Reference()</shell>
     </action>
     <contextual action="insert Source_Reference" >
        <Title>Insert/GNAT pragma/Source_Reference</Title>
     </contextual>

     <action name="insert Stream_Convert" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Stream_Convert()</shell>
     </action>
     <contextual action="insert Stream_Convert" >
        <Title>Insert/GNAT pragma/Stream_Convert</Title>
     </contextual>

     <action name="insert Style_Checks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Style_Checks()</shell>
     </action>
     <contextual action="insert Style_Checks" >
        <Title>Insert/GNAT pragma/Style_Checks</Title>
     </contextual>

     <action name="insert Suppress_Exception_Locations" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Suppress_Exception_Locations()</shell>
     </action>
     <contextual action="insert Suppress_Exception_Locations" >
        <Title>Insert/GNAT pragma/Suppress_Exception_Locations</Title>
     </contextual>

     <action name="insert Suppress_Initialization" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Suppress_Initialization()</shell>
     </action>
     <contextual action="insert Suppress_Initialization" >
        <Title>Insert/GNAT pragma/Suppress_Initialization</Title>
     </contextual>

     <action name="insert Task_Info" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Info()</shell>
     </action>
     <contextual action="insert Task_Info" >
        <Title>Insert/GNAT pragma/Task_Info</Title>
     </contextual>

     <action name="insert Task_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Name()</shell>
     </action>
     <contextual action="insert Task_Name" >
        <Title>Insert/GNAT pragma/Task_Name</Title>
     </contextual>

     <action name="insert Task_Storage" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Storage()</shell>
     </action>
     <contextual action="insert Task_Storage" >
        <Title>Insert/GNAT pragma/Task_Storage</Title>
     </contextual>

     <action name="insert Time_Slice" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Time_Slice()</shell>
     </action>
     <contextual action="insert Time_Slice" >
        <Title>Insert/GNAT pragma/Time_Slice</Title>
     </contextual>

     <action name="insert Title" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Title()</shell>
     </action>
     <contextual action="insert Title" >
        <Title>Insert/GNAT pragma/Title</Title>
     </contextual>

     <action name="insert Unimplemented_Unit" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unimplemented_Unit()</shell>
     </action>
     <contextual action="insert Unimplemented_Unit" >
        <Title>Insert/GNAT pragma/Unimplemented_Unit</Title>
     </contextual>

     <action name="insert Universal_Data" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Universal_Data()</shell>
     </action>
     <contextual action="insert Universal_Data" >
        <Title>Insert/GNAT pragma/Universal_Data</Title>
     </contextual>

     <action name="insert Unreferenced" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unreferenced()</shell>
     </action>
     <contextual action="insert Unreferenced" >
        <Title>Insert/GNAT pragma/Unreferenced</Title>
     </contextual>

     <action name="insert Unreserve_All_Interrupts" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unreserve_All_Interrupts()</shell>
     </action>
     <contextual action="insert Unreserve_All_Interrupts" >
        <Title>Insert/GNAT pragma/Unreserve_All_Interrupts</Title>
     </contextual>

     <action name="insert Unsuppress" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unsuppress()</shell>
     </action>
     <contextual action="insert Unsuppress" >
        <Title>Insert/GNAT pragma/Unsuppress</Title>
     </contextual>

     <action name="insert Use_VADS_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Use_VADS_Size()</shell>
     </action>
     <contextual action="insert Use_VADS_Size" >
        <Title>Insert/GNAT pragma/Use_VADS_Size</Title>
     </contextual>

     <action name="insert Validity_Checks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Validity_Checks()</shell>
     </action>
     <contextual action="insert Validity_Checks" >
        <Title>Insert/GNAT pragma/Validity_Checks</Title>
     </contextual>

     <action name="insert Warnings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Warnings()</shell>
     </action>
     <contextual action="insert Warnings" >
        <Title>Insert/GNAT pragma/Warnings</Title>
     </contextual>

     <action name="insert Weak_External" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Weak_External()</shell>
     </action>
     <contextual action="insert Weak_External" >
        <Title>Insert/GNAT pragma/Weak_External</Title>
     </contextual>
""")

# the GNAT-defined attributes

GPS.parse_xml ("""
     <action name="insert Address_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Address_Size()</shell>
     </action>
     <contextual action="insert Address_Size" >
        <Title>Insert/GNAT attribute/Address_Size</Title>
     </contextual>

     <action name="insert Asm_Input" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asm_Input()</shell>
     </action>
     <contextual action="insert Asm_Input" >
        <Title>Insert/GNAT attribute/Asm_Input</Title>
     </contextual>

     <action name="insert Asm_Output" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asm_Output()</shell>
     </action>
     <contextual action="insert Asm_Output" >
        <Title>Insert/GNAT attribute/Asm_Output</Title>
     </contextual>

     <action name="insert AST_Entry" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_AST_Entry()</shell>
     </action>
     <contextual action="insert AST_Entry" >
        <Title>Insert/GNAT attribute/AST_Entry</Title>
     </contextual>

     <action name="insert Bit" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Bit()</shell>
     </action>
     <contextual action="insert Bit" >
        <Title>Insert/GNAT attribute/Bit</Title>
     </contextual>

     <action name="insert Bit_Position" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Bit_Position()</shell>
     </action>
     <contextual action="insert Bit_Position" >
        <Title>Insert/GNAT attribute/Bit_Position</Title>
     </contextual>

     <action name="insert Code_Address" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Code_Address()</shell>
     </action>
     <contextual action="insert Code_Address" >
        <Title>Insert/GNAT attribute/Code_Address</Title>
     </contextual>

     <action name="insert Default_Bit_Order" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Default_Bit_Order()</shell>
     </action>
     <contextual action="insert Default_Bit_Order" >
        <Title>Insert/GNAT attribute/Default_Bit_Order</Title>
     </contextual>

     <action name="insert Elaborated" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborated()</shell>
     </action>
     <contextual action="insert Elaborated" >
        <Title>Insert/GNAT attribute/Elaborated</Title>
     </contextual>

     <action name="insert Elab_Body" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elab_Body()</shell>
     </action>
     <contextual action="insert Elab_Body" >
        <Title>Insert/GNAT attribute/Elab_Body</Title>
     </contextual>

     <action name="insert Elab_Spec" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elab_Spec()</shell>
     </action>
     <contextual action="insert Elab_Spec" >
        <Title>Insert/GNAT attribute/Elab_Spec</Title>
     </contextual>

     <action name="insert Emax" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Emax()</shell>
     </action>
     <contextual action="insert Emax" >
        <Title>Insert/GNAT attribute/Emax</Title>
     </contextual>

     <action name="insert Enum_Rep" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Enum_Rep()</shell>
     </action>
     <contextual action="insert Enum_Rep" >
        <Title>Insert/GNAT attribute/Enum_Rep</Title>
     </contextual>

     <action name="insert Epsilon" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Epsilon()</shell>
     </action>
     <contextual action="insert Epsilon" >
        <Title>Insert/GNAT attribute/Epsilon</Title>
     </contextual>

     <action name="insert Fixed_Value" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Fixed_Value()</shell>
     </action>
     <contextual action="insert Fixed_Value" >
        <Title>Insert/GNAT attribute/Fixed_Value</Title>
     </contextual>

     <action name="insert Has_Discriminants" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Has_Discriminants()</shell>
     </action>
     <contextual action="insert Has_Discriminants" >
        <Title>Insert/GNAT attribute/Has_Discriminants</Title>
     </contextual>

     <action name="insert Img" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Img()</shell>
     </action>
     <contextual action="insert Img" >
        <Title>Insert/GNAT attribute/Img</Title>
     </contextual>

     <action name="insert Integer_Value" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Integer_Value()</shell>
     </action>
     <contextual action="insert Integer_Value" >
        <Title>Insert/GNAT attribute/Integer_Value</Title>
     </contextual>

     <action name="insert Large" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Large()</shell>
     </action>
     <contextual action="insert Large" >
        <Title>Insert/GNAT attribute/Large</Title>
     </contextual>

     <action name="insert Machine_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Machine_Size()</shell>
     </action>
     <contextual action="insert Machine_Size" >
        <Title>Insert/GNAT attribute/Machine_Size</Title>
     </contextual>

     <action name="insert Mantissa" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Mantissa()</shell>
     </action>
     <contextual action="insert Mantissa" >
        <Title>Insert/GNAT attribute/Mantissa</Title>
     </contextual>

     <action name="insert Max_Interrupt_Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Max_Interrupt_Priority()</shell>
     </action>
     <contextual action="insert Max_Interrupt_Priority" >
        <Title>Insert/GNAT attribute/Max_Interrupt_Priority</Title>
     </contextual>

     <action name="insert Max_Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Max_Priority()</shell>
     </action>
     <contextual action="insert Max_Priority" >
        <Title>Insert/GNAT attribute/Max_Priority</Title>
     </contextual>

     <action name="insert Maximum_Alignment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Maximum_Alignment()</shell>
     </action>
     <contextual action="insert Maximum_Alignment" >
        <Title>Insert/GNAT attribute/Maximum_Alignment</Title>
     </contextual>

     <action name="insert Mechanism_Code" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Mechanism_Code()</shell>
     </action>
     <contextual action="insert Mechanism_Code" >
        <Title>Insert/GNAT attribute/Mechanism_Code</Title>
     </contextual>

     <action name="insert Null_Parameter" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Null_Parameter()</shell>
     </action>
     <contextual action="insert Null_Parameter" >
        <Title>Insert/GNAT attribute/Null_Parameter</Title>
     </contextual>

     <action name="insert Object_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Object_Size()</shell>
     </action>
     <contextual action="insert Object_Size" >
        <Title>Insert/GNAT attribute/Object_Size</Title>
     </contextual>

     <action name="insert Passed_By_Reference" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Passed_By_Reference()</shell>
     </action>
     <contextual action="insert Passed_By_Reference" >
        <Title>Insert/GNAT attribute/Passed_By_Reference</Title>
     </contextual>

     <action name="insert Range_Length" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Range_Length()</shell>
     </action>
     <contextual action="insert Range_Length" >
        <Title>Insert/GNAT attribute/Range_Length</Title>
     </contextual>

     <action name="insert Safe_Emax" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Safe_Emax()</shell>
     </action>
     <contextual action="insert Safe_Emax" >
        <Title>Insert/GNAT attribute/Safe_Emax</Title>
     </contextual>

     <action name="insert Safe_Large" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Safe_Large()</shell>
     </action>
     <contextual action="insert Safe_Large" >
        <Title>Insert/GNAT attribute/Safe_Large</Title>
     </contextual>

     <action name="insert Small" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Small()</shell>
     </action>
     <contextual action="insert Small" >
        <Title>Insert/GNAT attribute/Small</Title>
     </contextual>

     <action name="insert Storage_Unit" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_Unit()</shell>
     </action>
     <contextual action="insert Storage_Unit" >
        <Title>Insert/GNAT attribute/Storage_Unit</Title>
     </contextual>

     <action name="insert Target_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Target_Name()</shell>
     </action>
     <contextual action="insert Target_Name" >
        <Title>Insert/GNAT attribute/Target_Name</Title>
     </contextual>

     <action name="insert Tick" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Tick()</shell>
     </action>
     <contextual action="insert Tick" >
        <Title>Insert/GNAT attribute/Tick</Title>
     </contextual>

     <action name="insert To_Address" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_To_Address()</shell>
     </action>
     <contextual action="insert To_Address" >
        <Title>Insert/GNAT attribute/To_Address</Title>
     </contextual>

     <action name="insert Type_Class" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Type_Class()</shell>
     </action>
     <contextual action="insert Type_Class" >
        <Title>Insert/GNAT attribute/Type_Class</Title>
     </contextual>

     <action name="insert Unconstrained_Array" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unconstrained_Array()</shell>
     </action>
     <contextual action="insert Unconstrained_Array" >
        <Title>Insert/GNAT attribute/Unconstrained_Array</Title>
     </contextual>

     <action name="insert Universal_Literal_String" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Universal_Literal_String()</shell>
     </action>
     <contextual action="insert Universal_Literal_String" >
        <Title>Insert/GNAT attribute/Universal_Literal_String</Title>
     </contextual>

     <action name="insert Unrestricted_Access" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unrestricted_Access()</shell>
     </action>
     <contextual action="insert Unrestricted_Access" >
        <Title>Insert/GNAT attribute/Unrestricted_Access</Title>
     </contextual>

     <action name="insert VADS_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_VADS_Size()</shell>
     </action>
     <contextual action="insert VADS_Size" >
        <Title>Insert/GNAT attribute/VADS_Size</Title>
     </contextual>

     <action name="insert Value_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Value_Size()</shell>
     </action>
     <contextual action="insert Value_Size" >
        <Title>Insert/GNAT attribute/Value_Size</Title>
     </contextual>

     <action name="insert Wchar_T_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Wchar_T_Size()</shell>
     </action>
     <contextual action="insert Wchar_T_Size" >
        <Title>Insert/GNAT attribute/Wchar_T_Size</Title>
     </contextual>

     <action name="insert Word_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Word_Size()</shell>
     </action>
     <contextual action="insert Word_Size" >
        <Title>Insert/GNAT attribute/Word_Size</Title>
     </contextual>
""")

