"""This plug-in provides routines that insert the text of Ada 95
language-defined packages, subprograms, and pragmas, as well as
GNAT-defined packages, pragmas, and attributes.  They are used by
contextual menus (located at the end of this file) to insert the 
name (etc) so that the user need not remember the name or exactly 
how to spell it.
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


def insert_Volatile ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Volatile(local_name); -- See C.6."));


def insert_Volatile_Components ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Volatile_Components(array_local_name); -- See C.6."));


#   
# language-defined packages
#

def insert_Asynchronous_Task_Control():
   GPS.Editor.insert_text (identifier_case ("Ada.Asynchronous_Task_Control"));


def insert_Calendar():
   GPS.Editor.insert_text (identifier_case ("Ada.Calendar"));


def insert_Characters():
   GPS.Editor.insert_text (identifier_case ("Ada.Characters"));


def insert_Characters_Handling():
   GPS.Editor.insert_text (identifier_case ("Ada.Characters.Handling"));


def insert_Characters_Latin_1():
   GPS.Editor.insert_text (identifier_case ("Ada.Characters.Latin_1"));


def insert_Command_Line():
   GPS.Editor.insert_text (identifier_case ("Ada.Command_Line"));


def insert_Decimal():
   GPS.Editor.insert_text (identifier_case ("Ada.Decimal"));


def insert_Direct_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Direct_IO"));


def insert_Dynamic_Priorities():
   GPS.Editor.insert_text (identifier_case ("Ada.Dynamic_Priorities"));


def insert_Exceptions():
   GPS.Editor.insert_text (identifier_case ("Ada.Exceptions"));


def insert_Finalization():
   GPS.Editor.insert_text (identifier_case ("Ada.Finalization"));


def insert_Finalization_Controlled():
   GPS.Editor.insert_text (identifier_case ("Ada.Finalization.Controlled"));


def insert_Finalization_Limited_Controlled():
   GPS.Editor.insert_text (identifier_case ("Ada.Finalization.Limited_Controlled"));


def insert_Interrupts():
   GPS.Editor.insert_text (identifier_case ("Ada.Interrupts"));


def insert_Interrupts_Names():
   GPS.Editor.insert_text (identifier_case ("Ada.Interrupts.Names"));


def insert_Numerics():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics"));


def insert_Complex_Elementary_Functions():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Complex_Elementary_Functions"));


def insert_Complex_Types():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Complex_Types"));


def insert_Discrete_Random():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Discrete_Random"));


def insert_Elementary_Functions():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Elementary_Functions"));


def insert_Float_Random():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Float_Random"));


def insert_Generic_Complex_Elementary_Functions():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Generic_Complex_Elementary_Functions"));


def insert_Generic_Complex_Types():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Generic_Complex_Types"));


def insert_Generic_Elementary_Functions():
   GPS.Editor.insert_text (identifier_case ("Ada.Numerics.Generic_Elementary_Functions"));


def insert_Real_Time():
   GPS.Editor.insert_text (identifier_case ("Ada.Real_Time"));


def insert_Sequential_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Sequential_IO"));


def insert_Storage_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Storage_IO"));


def insert_Streams():
   GPS.Editor.insert_text (identifier_case ("Ada.Streams"));


def insert_Streams_Stream_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Streams.Stream_IO"));


def insert_Strings():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings"));


def insert_Strings_Bounded():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Bounded"));


def insert_Strings_Fixed():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Fixed"));


def insert_Strings_Maps():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Maps"));


def insert_Strings_Maps_Constants():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Maps.Constants"));


def insert_Strings_Unbounded():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Unbounded"));


def insert_Strings_Wide_Bounded():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Wide_Bounded"));


def insert_Strings_Wide_Fixed():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Wide_Fixed"));


def insert_Strings_Wide_Maps():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Wide_Maps"));


def insert_Strings_Wide_Maps_Wide_Constants():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Wide_Maps.Wide_Constants"));


def insert_Strings_Wide_Unbounded():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Wide_Unbounded"));


def insert_Synchronous_Task_Control():
   GPS.Editor.insert_text (identifier_case ("Ada.Synchronous_Task_Control"));


def insert_Tags():
   GPS.Editor.insert_text (identifier_case ("Ada.Tags"));


def insert_Task_Attributes():
   GPS.Editor.insert_text (identifier_case ("Ada.Task_Attributes"));


def insert_Task_Identification():
   GPS.Editor.insert_text (identifier_case ("Ada.Task_Identification"));


def insert_Text_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO"));


def insert_Text_IO_Complex_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Complex_IO"));


def insert_Text_IO_Editing():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Editing"));


def insert_Text_IO_Enumeration_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Enumeration_IO"));


def insert_Text_IO_Fixed_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Fixed_IO"));


def insert_Text_IO_Float_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Float_IO"));


def insert_Text_IO_Integer_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Integer_IO"));


def insert_Text_IO_Modular_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Modular_IO"));


def insert_Text_IO_Text_Streams():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.Text_Streams"));


def insert_Unchecked_Conversion():
   GPS.Editor.insert_text (identifier_case ("Ada.Unchecked_Conversion"));


def insert_Unchecked_Deallocation():
   GPS.Editor.insert_text (identifier_case ("Ada.Unchecked_Deallocation"));


def insert_Wide_Text_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Wide_Text_IO"));


def insert_Wide_Text_IO_Complex_IO():
   GPS.Editor.insert_text (identifier_case ("Ada.Wide_Text_IO.Complex_IO"));


def insert_Wide_Text_IO_Editing():
   GPS.Editor.insert_text (identifier_case ("Ada.Wide_Text_IO.Editing"));


def insert_Wide_Text_IO_Text_Streams():
   GPS.Editor.insert_text (identifier_case ("Ada.Wide_Text_IO.Text_Streams"));


def insert_Interfaces():
   GPS.Editor.insert_text (identifier_case ("Interfaces"));


def insert_Interfaces_C():
   GPS.Editor.insert_text (identifier_case ("Interfaces.C"));


def insert_Interfaces_C_Pointers():
   GPS.Editor.insert_text (identifier_case ("Interfaces.C.Pointers"));


def insert_Interfaces_C_Strings():
   GPS.Editor.insert_text (identifier_case ("Interfaces.C.Strings"));


def insert_Interfaces_Cobol():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Cobol"));


def insert_Interfaces_Fortran():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Fortran"));


def insert_System():
   GPS.Editor.insert_text (identifier_case ("System"));


def insert_Address_To_Access_Conversions():
   GPS.Editor.insert_text (identifier_case ("System.Address_To_Access_Conversions"));


def insert_Machine_Code():
   GPS.Editor.insert_text (identifier_case ("System.Machine_Code"));


# def insert_RPC():
#    GPS.Editor.insert_text (identifier_case ("System.RPC"));


def insert_Storage_Elements():
   GPS.Editor.insert_text (identifier_case ("System.Storage_Elements"));


def insert_Storage_Pools():
   GPS.Editor.insert_text (identifier_case ("System.Storage_Pools"));


#   
# gnat-defined names
#

def insert_System_Wch_Con ():
   GPS.Editor.insert_text (identifier_case ("System.Wch_Con"));


def insert_System_Task_Info ():
   GPS.Editor.insert_text (identifier_case ("System.Task_Info"));


def insert_System_Partition_Interface ():
   GPS.Editor.insert_text (identifier_case ("System.Partition_Interface"));


def insert_System_Memory ():
   GPS.Editor.insert_text (identifier_case ("System.Memory"));


def insert_System_Assertions ():
   GPS.Editor.insert_text (identifier_case ("System.Assertions"));


def insert_System_Address_Image ():
   GPS.Editor.insert_text (identifier_case ("System.Address_Image"));


def insert_Interfaces_VxWorks_IO ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.VxWorks_IO"));


def insert_Interfaces_VxWorks ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.VxWorks"));


def insert_Interfaces_Packed_Decimal ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Packed_Decimal"));


def insert_Interfaces_Os2lib_Threads ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Os2lib_Threads"));


def insert_Interfaces_Os2lib_Synchronization ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Os2lib_Synchronization"));


def insert_Interfaces_Os2lib_Errors ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Os2lib_Errors"));


def insert_Interfaces_Os2lib ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.Os2lib"));


def insert_Interfaces_CPP ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.CPP"));


def insert_Interfaces_C_Streams ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.C_Streams"));


def insert_Interfaces_C_Extensions ():
   GPS.Editor.insert_text (identifier_case ("Interfaces.C_Extensions"));


def insert_GNAT_Wide_String_Split ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Wide_String_Split"));


def insert_GNAT_Traceback_Symbolic ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Traceback_Symbolic"));


def insert_GNAT_Traceback ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Traceback"));


def insert_GNAT_Threads ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Threads"));


def insert_GNAT_Task_Lock ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Task_Lock"));


def insert_GNAT_Table ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Table"));


def insert_GNAT_Strings ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Strings"));


def insert_GNAT_String_Split ():
   GPS.Editor.insert_text (identifier_case ("GNAT.String_Split"));


def insert_GNAT_Spitbol_Table_VString ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Spitbol_Table_VString"));


def insert_GNAT_Spitbol_Table_Integer ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Spitbol_Table_Integer"));


def insert_GNAT_Spitbol_Table_Boolean ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Spitbol_Table_Boolean"));


def insert_GNAT_Spitbol_Patterns ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Spitbol_Patterns"));


def insert_GNAT_Spitbol ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Spitbol"));


def insert_GNAT_Spell_Checker ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Spell_Checker"));


def insert_GNAT_Source_Info ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Source_Info"));


def insert_GNAT_Sockets ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Sockets"));


def insert_GNAT_Signals ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Signals"));


def insert_GNAT_Semaphores ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Semaphores"));


def insert_GNAT_Regpat ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Regpat"));


def insert_GNAT_Registry ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Registry"));


def insert_GNAT_Regexp ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Regexp"));


def insert_GNAT_Perfect_Hash_Generators ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Perfect_Hash_Generators"));


def insert_GNAT_OS_Lib ():
   GPS.Editor.insert_text (identifier_case ("GNAT.OS_Lib"));


def insert_GNAT_Most_Recent_Exception ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Most_Recent_Exception"));


def insert_GNAT_Memory_Dump ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Memory_Dump"));


def insert_GNAT_MD5 ():
   GPS.Editor.insert_text (identifier_case ("GNAT.MD5"));


def insert_GNAT_Lock_Files ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Lock_Files"));


def insert_GNAT_IO_Aux ():
   GPS.Editor.insert_text (identifier_case ("GNAT.IO_Aux"));


def insert_GNAT_IO ():
   GPS.Editor.insert_text (identifier_case ("GNAT.IO"));


def insert_GNAT_Heap_Sort_G ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Heap_Sort_G"));


def insert_GNAT_Heap_Sort_A ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Heap_Sort_A"));


def insert_GNAT_Heap_Sort ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Heap_Sort"));


def insert_GNAT_HTable ():
   GPS.Editor.insert_text (identifier_case ("GNAT.HTable"));


def insert_GNAT_Float_Control ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Float_Control"));


def insert_GNAT_Expect ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Expect"));


def insert_GNAT_Exceptions ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Exceptions"));


def insert_GNAT_Exception_Traces ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Exception_Traces"));


def insert_GNAT_Exception_Actions ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Exception_Actions"));


def insert_GNAT_Dynamic_Tables ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Dynamic_Tables"));


def insert_GNAT_Dynamic_HTables ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Dynamic_HTables"));


def insert_GNAT_Directory_Operations ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Directory_Operations"));


def insert_GNAT_Debug_Utilities ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Debug_Utilities"));


def insert_GNAT_Debug_Pools ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Debug_Pools"));


def insert_GNAT_Current_Exception ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Current_Exception"));


def insert_GNAT_Ctrl_C ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Ctrl_C"));


def insert_GNAT_Compiler_Version ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Compiler_Version"));


def insert_GNAT_Command_Line ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Command_Line"));


def insert_GNAT_Case_Util ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Case_Util"));


def insert_GNAT_Calendar_Time_IO ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Calendar.Time_IO"));


def insert_GNAT_Calendar ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Calendar"));


def insert_GNAT_CRC32 ():
   GPS.Editor.insert_text (identifier_case ("GNAT.CRC32"));


def insert_GNAT_CGI_Debug ():
   GPS.Editor.insert_text (identifier_case ("GNAT.CGI_Debug"));


def insert_GNAT_CGI_Cookie ():
   GPS.Editor.insert_text (identifier_case ("GNAT.CGI_Cookie"));


def insert_GNAT_CGI ():
   GPS.Editor.insert_text (identifier_case ("GNAT.CGI"));


def insert_GNAT_Bubble_Sort_G ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Bubble_Sort_G"));


def insert_GNAT_Bubble_Sort_A ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Bubble_Sort_A"));


def insert_GNAT_Bubble_Sort ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Bubble_Sort"));


def insert_GNAT_Bounded_Mailboxes ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Bounded_Mailboxes"));


def insert_GNAT_Bounded_Buffers ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Bounded_Buffers"));


def insert_GNAT_Array_Split ():
   GPS.Editor.insert_text (identifier_case ("GNAT.Array_Split"));


def insert_GNAT_AWK ():
   GPS.Editor.insert_text (identifier_case ("GNAT.AWK"));


def insert_Ada_Wide_Text_IO_C_Streams ():
   GPS.Editor.insert_text (identifier_case ("Ada.Wide_Text_IO.C_Streams"));


def insert_Ada_Text_IO_C_Streams ():
   GPS.Editor.insert_text (identifier_case ("Ada.Text_IO.C_Streams"));


def insert_Ada_Strings_Wide_Unbounded_Wide_Text_IO ():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Wide_Unbounded.Wide_Text_IO"));


def insert_Ada_Strings_Unbounded_Text_IO ():
   GPS.Editor.insert_text (identifier_case ("Ada.Strings.Unbounded.Text_IO"));


def insert_Ada_Streams_Stream_IO_C_Streams ():
   GPS.Editor.insert_text (identifier_case ("Ada.Streams.Stream_IO.C_Streams"));


def insert_Ada_Sequential_IO_C_Streams ():
   GPS.Editor.insert_text (identifier_case ("Ada.Sequential_IO.C_Streams"));


def insert_Ada_Exceptions_Traceback ():
   GPS.Editor.insert_text (identifier_case ("Ada.Exceptions.Traceback"));


def insert_Ada_Exceptions_Is_Null_Occurrence ():
   GPS.Editor.insert_text (identifier_case ("Ada.Exceptions.Is_Null_Occurrence"));


def insert_Ada_Direct_IO_C_Streams ():
   GPS.Editor.insert_text (identifier_case ("Ada.Direct_IO.C_Streams"));


def insert_Ada_Command_Line_Remove ():
   GPS.Editor.insert_text (identifier_case ("Ada.Command_Line.Remove"));


def insert_Ada_Command_Line_Environment ():
   GPS.Editor.insert_text (identifier_case ("Ada.Command_Line.Environment"));


def insert_Ada_Characters_Wide_Latin_9 ():
   GPS.Editor.insert_text (identifier_case ("Ada.Characters.Wide_Latin_9"));


def insert_Ada_Characters_Wide_Latin_1 ():
   GPS.Editor.insert_text (identifier_case ("Ada.Characters.Wide_Latin_1"));


def insert_Ada_Characters_Latin_9 ():
   GPS.Editor.insert_text (identifier_case ("Ada.Characters.Latin_9"));


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


def insert_Assert ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Assert ( boolean_EXPRESSION [, static_string_EXPRESSION]);"));


def insert_Ast_Entry ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" AST_Entry (entry_IDENTIFIER);"));


def insert_C_Pass_By_Copy ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" C_Pass_By_Copy ([Max_Size =>] static_integer_EXPRESSION);"));


def insert_Comment ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Comment (static_string_EXPRESSION);"));


def insert_Common_Object ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Common_Object"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME,"))
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Size     =>] EXTERNAL_SYMBOL]);"));


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


def insert_CPP_Class ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" CPP_Class ([Entity =>] LOCAL_NAME);"));


def insert_CPP_Constructor ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" CPP_Constructor ([Entity =>] LOCAL_NAME);"));


def insert_CPP_Virtual ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" CPP_Virtual"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity     =>] ENTITY"))
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Vtable_Ptr =>] vtable_ENTITY]"))
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Position   =>] static_integer_EXPRESSION]);"));


def insert_CPP_Vtable ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" CPP_Vtable"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity      =>] ENTITY,"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Vtable_Ptr  =>] vtable_ENTITY"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Entry_Count =>] static_integer_EXPRESSION);"));


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


def insert_Export_Exception ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export_Exception"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External =>] EXTERNAL_SYMBOL,]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Form     =>] Ada | VMS]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Code     =>] static_integer_EXPRESSION]);"));


def insert_Export_Function ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export_Function"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External         =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Parameter_Types  =>] PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Result_Type      =>] result_SUBTYPE_MARK]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Mechanism        =>] MECHANISM]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Result_Mechanism =>] MECHANISM_NAME]"));


def insert_Export_Object ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export_Object"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Size     =>] EXTERNAL_SYMBOL]"));


def insert_Export_Procedure ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export_Procedure"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External        =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Parameter_Types =>] PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Mechanism       =>] MECHANISM]);"));


def insert_Export_Value ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export_Value"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Value =>] static_integer_EXPRESSION"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Link_Name =>] static_string_EXPRESSION);"));


def insert_Export_Valued_Procedure ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Export_Valued_Procedure"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External        =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Parameter_Types =>] PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Mechanism       =>] MECHANISM]);"));


def insert_Extend_System ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Extend_System ([Name =>] IDENTIFIER);"));


def insert_External ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" External"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Convention =>] convention_IDENTIFIER"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Entity         =>] local_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External_Name =>] static_string_EXPRESSION]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Link_Name     =>] static_string_EXPRESSION ]);"));


def insert_External_Name_Casing ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" External_Name_Casing (Uppercase | Lowercase [, Uppercase | Lowercase | As_Is]);"));


def insert_Finalize_Storage_Only ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Finalize_Storage_Only (first_subtype_LOCAL_NAME);"));


def insert_Float_Representation ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Float_Representation (FLOAT_REP);"));


def insert_Ident ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Ident (static_string_EXPRESSION);"));


def insert_Import_Exception ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Import_Exception"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal    =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Form     =>] Ada | VMS]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Code     =>] static_integer_EXPRESSION]);"));


def insert_Import_Function ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Import_Function"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal                    =>] LOCAL_NAME,"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External                 =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Parameter_Types          =>] PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Result_Type              =>] SUBTYPE_MARK]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Mechanism                =>] MECHANISM]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Result_Mechanism         =>] MECHANISM_NAME]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [First_Optional_Parameter =>] IDENTIFIER]);"));


def insert_Import_Object ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Import_Object"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal    =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Size     =>] EXTERNAL_SYMBOL]);"));


def insert_Import_Procedure ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Import_Procedure"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal                   =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("[, [External                 =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("[, [Parameter_Types          =>] PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("[, [Mechanism                =>] MECHANISM]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("[, [First_Optional_Parameter =>] IDENTIFIER]);"));


def insert_Import_Valued_Procedure ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Import_Valued_Procedure"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal                    =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External                 =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Parameter_Types          =>] PARAMETER_TYPES]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Mechanism                =>] MECHANISM]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [First_Optional_Parameter =>] IDENTIFIER]);"));


def insert_Initialize_Scalars ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Initialize_Scalars;"));


def insert_Inline_Always ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Inline_Always (NAME [, NAME]);"));


def insert_Inline_Generic ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Inline_Generic (generic_package_NAME);"));


def insert_Interface ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Interface"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Convention       =>] convention_identifier"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Entity         =>] local_name"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External_Name =>] static_string_expression]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Link_Name     =>] static_string_expression]);"));


def insert_Interface_Name ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Interface_Name"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity           =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External_Name =>] static_string_EXPRESSION]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Link_Name     =>] static_string_EXPRESSION]);"));


def insert_Interrupt_State ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Interrupt_State (Name => value, State => SYSTEM | RUNTIME | USER);"));


def insert_Keep_Names ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Keep_Names ([On =>] enumeration_first_subtype_LOCAL_NAME);"));


def insert_License ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" License (Unrestricted | GPL | Modified_GPL | Restricted);"));


def insert_Link_With ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Link_With (static_string_EXPRESSION {,static_string_EXPRESSION});"));


def insert_Linker_Alias ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Linker_Alias"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Alias  =>] static_string_EXPRESSION);"));


def insert_Linker_Section ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Linker_Section"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity    =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Section =>] static_string_EXPRESSION);"));


def insert_Long_Float ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Long_Float (FLOAT_FORMAT);"));


def insert_Machine_Attribute ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Machine_Attribute"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Attribute_Name =>] string_EXPRESSION"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, Entity       =>] LOCAL_NAME);"));


def insert_Main_Storage ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Main_Storage (MAIN_STORAGE_OPTION [, MAIN_STORAGE_OPTION]);"));


def insert_No_Return ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" No_Return (procedure_LOCAL_NAME);"));


def insert_Obsolescent ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Obsolescent [(static_string_EXPRESSION)];"));


def insert_Polling ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Polling (ON | OFF);"));


def insert_Propagate_Exceptions ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Propagate_Exceptions (subprogram_LOCAL_NAME);"));


def insert_Psect_Object ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Psect_Object"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Internal =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [External =>] EXTERNAL_SYMBOL]"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Size     =>] EXTERNAL_SYMBOL]);"));


def insert_Pure_Function ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Pure_Function ([Entity =>] function_LOCAL_NAME);"));


def insert_Ravenscar ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Ravenscar;"));


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


def insert_Thread_Body ():
   col = GPS.current_context().location().column()-1
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Thread_Body"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case ("([Entity =>] LOCAL_NAME"));
   GPS.Editor.insert_text ("\n" + indented(col) + identifier_case (" [, [Secondary_Stack_Size =>] static_integer_EXPRESSION)];"));


def insert_Time_Slice ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Time_Slice (static_duration_EXPRESSION);"));


def insert_Unchecked_Union ():
   GPS.Editor.insert_text (word_case ("pragma") + identifier_case (" Unchecked_Union (first_subtype_LOCAL_NAME);"));


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

def insert_Abort_Signal ():
   GPS.Editor.insert_text (identifier_case ("Standard'Abort_Signal"));


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



# the GNAT-defined attributes

GPS.parse_xml ("""
     <action name="insert Word_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Word_Size()</shell>
     </action>
     <contextual action="insert Word_Size" >
        <Title>Insert/GNAT attribute/Word_Size</Title>
     </contextual>

     <action name="insert Wchar_T_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Wchar_T_Size()</shell>
     </action>
     <contextual action="insert Wchar_T_Size" >
        <Title>Insert/GNAT attribute/Wchar_T_Size</Title>
     </contextual>

     <action name="insert Value_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Value_Size()</shell>
     </action>
     <contextual action="insert Value_Size" >
        <Title>Insert/GNAT attribute/Value_Size</Title>
     </contextual>

     <action name="insert VADS_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_VADS_Size()</shell>
     </action>
     <contextual action="insert VADS_Size" >
        <Title>Insert/GNAT attribute/VADS_Size</Title>
     </contextual>

     <action name="insert Unrestricted_Access" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unrestricted_Access()</shell>
     </action>
     <contextual action="insert Unrestricted_Access" >
        <Title>Insert/GNAT attribute/Unrestricted_Access</Title>
     </contextual>

     <action name="insert Universal_Literal_String" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Universal_Literal_String()</shell>
     </action>
     <contextual action="insert Universal_Literal_String" >
        <Title>Insert/GNAT attribute/Universal_Literal_String</Title>
     </contextual>

     <action name="insert Unconstrained_Array" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unconstrained_Array()</shell>
     </action>
     <contextual action="insert Unconstrained_Array" >
        <Title>Insert/GNAT attribute/Unconstrained_Array</Title>
     </contextual>

     <action name="insert Type_Class" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Type_Class()</shell>
     </action>
     <contextual action="insert Type_Class" >
        <Title>Insert/GNAT attribute/Type_Class</Title>
     </contextual>

     <action name="insert To_Address" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_To_Address()</shell>
     </action>
     <contextual action="insert To_Address" >
        <Title>Insert/GNAT attribute/To_Address</Title>
     </contextual>

     <action name="insert Tick" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Tick()</shell>
     </action>
     <contextual action="insert Tick" >
        <Title>Insert/GNAT attribute/Tick</Title>
     </contextual>

     <action name="insert Target_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Target_Name()</shell>
     </action>
     <contextual action="insert Target_Name" >
        <Title>Insert/GNAT attribute/Target_Name</Title>
     </contextual>

     <action name="insert Storage_Unit" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_Unit()</shell>
     </action>
     <contextual action="insert Storage_Unit" >
        <Title>Insert/GNAT attribute/Storage_Unit</Title>
     </contextual>

     <action name="insert Small" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Small()</shell>
     </action>
     <contextual action="insert Small" >
        <Title>Insert/GNAT attribute/Small</Title>
     </contextual>

     <action name="insert Safe_Large" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Safe_Large()</shell>
     </action>
     <contextual action="insert Safe_Large" >
        <Title>Insert/GNAT attribute/Safe_Large</Title>
     </contextual>

     <action name="insert Safe_Emax" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Safe_Emax()</shell>
     </action>
     <contextual action="insert Safe_Emax" >
        <Title>Insert/GNAT attribute/Safe_Emax</Title>
     </contextual>

     <action name="insert Range_Length" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Range_Length()</shell>
     </action>
     <contextual action="insert Range_Length" >
        <Title>Insert/GNAT attribute/Range_Length</Title>
     </contextual>

     <action name="insert Passed_By_Reference" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Passed_By_Reference()</shell>
     </action>
     <contextual action="insert Passed_By_Reference" >
        <Title>Insert/GNAT attribute/Passed_By_Reference</Title>
     </contextual>

     <action name="insert Object_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Object_Size()</shell>
     </action>
     <contextual action="insert Object_Size" >
        <Title>Insert/GNAT attribute/Object_Size</Title>
     </contextual>

     <action name="insert Null_Parameter" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Null_Parameter()</shell>
     </action>
     <contextual action="insert Null_Parameter" >
        <Title>Insert/GNAT attribute/Null_Parameter</Title>
     </contextual>

     <action name="insert Mechanism_Code" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Mechanism_Code()</shell>
     </action>
     <contextual action="insert Mechanism_Code" >
        <Title>Insert/GNAT attribute/Mechanism_Code</Title>
     </contextual>

     <action name="insert Maximum_Alignment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Maximum_Alignment()</shell>
     </action>
     <contextual action="insert Maximum_Alignment" >
        <Title>Insert/GNAT attribute/Maximum_Alignment</Title>
     </contextual>

     <action name="insert Max_Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Max_Priority()</shell>
     </action>
     <contextual action="insert Max_Priority" >
        <Title>Insert/GNAT attribute/Max_Priority</Title>
     </contextual>

     <action name="insert Max_Interrupt_Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Max_Interrupt_Priority()</shell>
     </action>
     <contextual action="insert Max_Interrupt_Priority" >
        <Title>Insert/GNAT attribute/Max_Interrupt_Priority</Title>
     </contextual>

     <action name="insert Mantissa" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Mantissa()</shell>
     </action>
     <contextual action="insert Mantissa" >
        <Title>Insert/GNAT attribute/Mantissa</Title>
     </contextual>

     <action name="insert Machine_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Machine_Size()</shell>
     </action>
     <contextual action="insert Machine_Size" >
        <Title>Insert/GNAT attribute/Machine_Size</Title>
     </contextual>

     <action name="insert Large" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Large()</shell>
     </action>
     <contextual action="insert Large" >
        <Title>Insert/GNAT attribute/Large</Title>
     </contextual>

     <action name="insert Integer_Value" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Integer_Value()</shell>
     </action>
     <contextual action="insert Integer_Value" >
        <Title>Insert/GNAT attribute/Integer_Value</Title>
     </contextual>

     <action name="insert Img" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Img()</shell>
     </action>
     <contextual action="insert Img" >
        <Title>Insert/GNAT attribute/Img</Title>
     </contextual>

     <action name="insert Has_Discriminants" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Has_Discriminants()</shell>
     </action>
     <contextual action="insert Has_Discriminants" >
        <Title>Insert/GNAT attribute/Has_Discriminants</Title>
     </contextual>

     <action name="insert Fixed_Value" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Fixed_Value()</shell>
     </action>
     <contextual action="insert Fixed_Value" >
        <Title>Insert/GNAT attribute/Fixed_Value</Title>
     </contextual>

     <action name="insert Epsilon" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Epsilon()</shell>
     </action>
     <contextual action="insert Epsilon" >
        <Title>Insert/GNAT attribute/Epsilon</Title>
     </contextual>

     <action name="insert Enum_Rep" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Enum_Rep()</shell>
     </action>
     <contextual action="insert Enum_Rep" >
        <Title>Insert/GNAT attribute/Enum_Rep</Title>
     </contextual>

     <action name="insert Emax" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Emax()</shell>
     </action>
     <contextual action="insert Emax" >
        <Title>Insert/GNAT attribute/Emax</Title>
     </contextual>

     <action name="insert Elab_Spec" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elab_Spec()</shell>
     </action>
     <contextual action="insert Elab_Spec" >
        <Title>Insert/GNAT attribute/Elab_Spec</Title>
     </contextual>

     <action name="insert Elab_Body" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elab_Body()</shell>
     </action>
     <contextual action="insert Elab_Body" >
        <Title>Insert/GNAT attribute/Elab_Body</Title>
     </contextual>

     <action name="insert Elaborated" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborated()</shell>
     </action>
     <contextual action="insert Elaborated" >
        <Title>Insert/GNAT attribute/Elaborated</Title>
     </contextual>

     <action name="insert Default_Bit_Order" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Default_Bit_Order()</shell>
     </action>
     <contextual action="insert Default_Bit_Order" >
        <Title>Insert/GNAT attribute/Default_Bit_Order</Title>
     </contextual>

     <action name="insert Code_Address" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Code_Address()</shell>
     </action>
     <contextual action="insert Code_Address" >
        <Title>Insert/GNAT attribute/Code_Address</Title>
     </contextual>

     <action name="insert Bit_Position" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Bit_Position()</shell>
     </action>
     <contextual action="insert Bit_Position" >
        <Title>Insert/GNAT attribute/Bit_Position</Title>
     </contextual>

     <action name="insert Bit" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Bit()</shell>
     </action>
     <contextual action="insert Bit" >
        <Title>Insert/GNAT attribute/Bit</Title>
     </contextual>

     <action name="insert AST_Entry attr" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_AST_Entry()</shell>
     </action>
     <contextual action="insert AST_Entry" >
        <Title>Insert/GNAT attribute/AST_Entry</Title>
     </contextual>

     <action name="insert Asm_Output" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asm_Output()</shell>
     </action>
     <contextual action="insert Asm_Output" >
        <Title>Insert/GNAT attribute/Asm_Output</Title>
     </contextual>

     <action name="insert Asm_Input" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asm_Input()</shell>
     </action>
     <contextual action="insert Asm_Input" >
        <Title>Insert/GNAT attribute/Asm_Input</Title>
     </contextual>

     <action name="insert Address_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Address_Size()</shell>
     </action>
     <contextual action="insert Address_Size" >
        <Title>Insert/GNAT attribute/Address_Size</Title>
     </contextual>

     <action name="insert Abort_Signal" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Abort_Signal()</shell>
     </action>
     <contextual action="insert Abort_Signal" >
        <Title>Insert/GNAT attribute/Abort_Signal</Title>
     </contextual>

""")




# the GNAT-defined pragmas

GPS.parse_xml ("""
     <action name="insert Weak_External" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Weak_External()</shell>
     </action>
     <contextual action="insert Weak_External" >
        <Title>Insert/GNAT pragma/Weak_External</Title>
     </contextual>

     <action name="insert Warnings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Warnings()</shell>
     </action>
     <contextual action="insert Warnings" >
        <Title>Insert/GNAT pragma/Warnings</Title>
     </contextual>

     <action name="insert Validity_Checks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Validity_Checks()</shell>
     </action>
     <contextual action="insert Validity_Checks" >
        <Title>Insert/GNAT pragma/Validity_Checks</Title>
     </contextual>

     <action name="insert Use_VADS_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Use_VADS_Size()</shell>
     </action>
     <contextual action="insert Use_VADS_Size" >
        <Title>Insert/GNAT pragma/Use_VADS_Size</Title>
     </contextual>

     <action name="insert Unsuppress" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unsuppress()</shell>
     </action>
     <contextual action="insert Unsuppress" >
        <Title>Insert/GNAT pragma/Unsuppress</Title>
     </contextual>

     <action name="insert Unreserve_All_Interrupts" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unreserve_All_Interrupts()</shell>
     </action>
     <contextual action="insert Unreserve_All_Interrupts" >
        <Title>Insert/GNAT pragma/Unreserve_All_Interrupts</Title>
     </contextual>

     <action name="insert Unreferenced" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unreferenced()</shell>
     </action>
     <contextual action="insert Unreferenced" >
        <Title>Insert/GNAT pragma/Unreferenced</Title>
     </contextual>

     <action name="insert Universal_Data" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Universal_Data()</shell>
     </action>
     <contextual action="insert Universal_Data" >
        <Title>Insert/GNAT pragma/Universal_Data</Title>
     </contextual>

     <action name="insert Unimplemented_Unit" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unimplemented_Unit()</shell>
     </action>
     <contextual action="insert Unimplemented_Unit" >
        <Title>Insert/GNAT pragma/Unimplemented_Unit</Title>
     </contextual>

     <action name="insert Unchecked_Union" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unchecked_Union()</shell>
     </action>
     <contextual action="insert Unchecked_Union" >
        <Title>Insert/GNAT pragma/Unchecked_Union</Title>
     </contextual>

     <action name="insert Title" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Title()</shell>
     </action>
     <contextual action="insert Title" >
        <Title>Insert/GNAT pragma/Title</Title>
     </contextual>

     <action name="insert Time_Slice" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Time_Slice()</shell>
     </action>
     <contextual action="insert Time_Slice" >
        <Title>Insert/GNAT pragma/Time_Slice</Title>
     </contextual>

     <action name="insert Thread_Body" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Thread_Body()</shell>
     </action>
     <contextual action="insert Thread_Body" >
        <Title>Insert/GNAT pragma/Thread_Body</Title>
     </contextual>

     <action name="insert Task_Storage" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Storage()</shell>
     </action>
     <contextual action="insert Task_Storage" >
        <Title>Insert/GNAT pragma/Task_Storage</Title>
     </contextual>

     <action name="insert Task_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Name()</shell>
     </action>
     <contextual action="insert Task_Name" >
        <Title>Insert/GNAT pragma/Task_Name</Title>
     </contextual>

     <action name="insert Task_Info" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Info()</shell>
     </action>
     <contextual action="insert Task_Info" >
        <Title>Insert/GNAT pragma/Task_Info</Title>
     </contextual>

     <action name="insert Suppress_Initialization" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Suppress_Initialization()</shell>
     </action>
     <contextual action="insert Suppress_Initialization" >
        <Title>Insert/GNAT pragma/Suppress_Initialization</Title>
     </contextual>

     <action name="insert Suppress_Exception_Locations" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Suppress_Exception_Locations()</shell>
     </action>
     <contextual action="insert Suppress_Exception_Locations" >
        <Title>Insert/GNAT pragma/Suppress_Exception_Locations</Title>
     </contextual>

     <action name="insert Style_Checks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Style_Checks()</shell>
     </action>
     <contextual action="insert Style_Checks" >
        <Title>Insert/GNAT pragma/Style_Checks</Title>
     </contextual>

     <action name="insert Stream_Convert" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Stream_Convert()</shell>
     </action>
     <contextual action="insert Stream_Convert" >
        <Title>Insert/GNAT pragma/Stream_Convert</Title>
     </contextual>

     <action name="insert Source_Reference" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Source_Reference()</shell>
     </action>
     <contextual action="insert Source_Reference" >
        <Title>Insert/GNAT pragma/Source_Reference</Title>
     </contextual>

     <action name="insert Source_File_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Source_File_Name()</shell>
     </action>
     <contextual action="insert Source_File_Name" >
        <Title>Insert/GNAT pragma/Source_File_Name</Title>
     </contextual>

     <action name="insert Restriction_Warnings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Restriction_Warnings()</shell>
     </action>
     <contextual action="insert Restriction_Warnings" >
        <Title>Insert/GNAT pragma/Restriction_Warnings</Title>
     </contextual>

     <action name="insert Ravenscar" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ravenscar()</shell>
     </action>
     <contextual action="insert Ravenscar" >
        <Title>Insert/GNAT pragma/Ravenscar</Title>
     </contextual>

     <action name="insert Pure_Function" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Pure_Function()</shell>
     </action>
     <contextual action="insert Pure_Function" >
        <Title>Insert/GNAT pragma/Pure_Function</Title>
     </contextual>

     <action name="insert Psect_Object" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Psect_Object()</shell>
     </action>
     <contextual action="insert Psect_Object" >
        <Title>Insert/GNAT pragma/Psect_Object</Title>
     </contextual>

     <action name="insert Propagate_Exceptions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Propagate_Exceptions()</shell>
     </action>
     <contextual action="insert Propagate_Exceptions" >
        <Title>Insert/GNAT pragma/Propagate_Exceptions</Title>
     </contextual>

     <action name="insert Polling" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Polling()</shell>
     </action>
     <contextual action="insert Polling" >
        <Title>Insert/GNAT pragma/Polling</Title>
     </contextual>

     <action name="insert Obsolescent" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Obsolescent()</shell>
     </action>
     <contextual action="insert Obsolescent" >
        <Title>Insert/GNAT pragma/Obsolescent</Title>
     </contextual>

     <action name="insert No_Return" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_No_Return()</shell>
     </action>
     <contextual action="insert No_Return" >
        <Title>Insert/GNAT pragma/No_Return</Title>
     </contextual>

     <action name="insert Main_Storage" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Main_Storage()</shell>
     </action>
     <contextual action="insert Main_Storage" >
        <Title>Insert/GNAT pragma/Main_Storage</Title>
     </contextual>

     <action name="insert Machine_Attribute" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Machine_Attribute()</shell>
     </action>
     <contextual action="insert Machine_Attribute" >
        <Title>Insert/GNAT pragma/Machine_Attribute</Title>
     </contextual>

     <action name="insert Long_Float" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Long_Float()</shell>
     </action>
     <contextual action="insert Long_Float" >
        <Title>Insert/GNAT pragma/Long_Float</Title>
     </contextual>

     <action name="insert Linker_Section" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Linker_Section()</shell>
     </action>
     <contextual action="insert Linker_Section" >
        <Title>Insert/GNAT pragma/Linker_Section</Title>
     </contextual>

     <action name="insert Linker_Alias" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Linker_Alias()</shell>
     </action>
     <contextual action="insert Linker_Alias" >
        <Title>Insert/GNAT pragma/Linker_Alias</Title>
     </contextual>

     <action name="insert Link_With" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Link_With()</shell>
     </action>
     <contextual action="insert Link_With" >
        <Title>Insert/GNAT pragma/Link_With</Title>
     </contextual>

     <action name="insert License" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_License()</shell>
     </action>
     <contextual action="insert License" >
        <Title>Insert/GNAT pragma/License</Title>
     </contextual>

     <action name="insert Keep_Names" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Keep_Names()</shell>
     </action>
     <contextual action="insert Keep_Names" >
        <Title>Insert/GNAT pragma/Keep_Names</Title>
     </contextual>

     <action name="insert Interrupt_State" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupt_State()</shell>
     </action>
     <contextual action="insert Interrupt_State" >
        <Title>Insert/GNAT pragma/Interrupt_State</Title>
     </contextual>

     <action name="insert Interface_Name" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interface_Name()</shell>
     </action>
     <contextual action="insert Interface_Name" >
        <Title>Insert/GNAT pragma/Interface_Name</Title>
     </contextual>

     <action name="insert Interface" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interface()</shell>
     </action>
     <contextual action="insert Interface" >
        <Title>Insert/GNAT pragma/Interface</Title>
     </contextual>

     <action name="insert Inline_Generic" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inline_Generic()</shell>
     </action>
     <contextual action="insert Inline_Generic" >
        <Title>Insert/GNAT pragma/Inline_Generic</Title>
     </contextual>

     <action name="insert Inline_Always" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inline_Always()</shell>
     </action>
     <contextual action="insert Inline_Always" >
        <Title>Insert/GNAT pragma/Inline_Always</Title>
     </contextual>

     <action name="insert Initialize_Scalars" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Initialize_Scalars()</shell>
     </action>
     <contextual action="insert Initialize_Scalars" >
        <Title>Insert/GNAT pragma/Initialize_Scalars</Title>
     </contextual>

     <action name="insert Import_Valued_Procedure" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import_Valued_Procedure()</shell>
     </action>
     <contextual action="insert Import_Valued_Procedure" >
        <Title>Insert/GNAT pragma/Import_Valued_Procedure</Title>
     </contextual>

     <action name="insert Import_Procedure" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import_Procedure()</shell>
     </action>
     <contextual action="insert Import_Procedure" >
        <Title>Insert/GNAT pragma/Import_Procedure</Title>
     </contextual>

     <action name="insert Import_Object" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import_Object()</shell>
     </action>
     <contextual action="insert Import_Object" >
        <Title>Insert/GNAT pragma/Import_Object</Title>
     </contextual>

     <action name="insert Import_Function" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import_Function()</shell>
     </action>
     <contextual action="insert Import_Function" >
        <Title>Insert/GNAT pragma/Import_Function</Title>
     </contextual>

     <action name="insert Import_Exception" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import_Exception()</shell>
     </action>
     <contextual action="insert Import_Exception" >
        <Title>Insert/GNAT pragma/Import_Exception</Title>
     </contextual>

     <action name="insert Ident" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ident()</shell>
     </action>
     <contextual action="insert Ident" >
        <Title>Insert/GNAT pragma/Ident</Title>
     </contextual>

     <action name="insert Float_Representation" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Float_Representation()</shell>
     </action>
     <contextual action="insert Float_Representation" >
        <Title>Insert/GNAT pragma/Float_Representation</Title>
     </contextual>

     <action name="insert Finalize_Storage_Only" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Finalize_Storage_Only()</shell>
     </action>
     <contextual action="insert Finalize_Storage_Only" >
        <Title>Insert/GNAT pragma/Finalize_Storage_Only</Title>
     </contextual>

     <action name="insert External_Name_Casing" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_External_Name_Casing()</shell>
     </action>
     <contextual action="insert External_Name_Casing" >
        <Title>Insert/GNAT pragma/External_Name_Casing</Title>
     </contextual>

     <action name="insert External" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_External()</shell>
     </action>
     <contextual action="insert External" >
        <Title>Insert/GNAT pragma/External</Title>
     </contextual>

     <action name="insert Extend_System" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Extend_System()</shell>
     </action>
     <contextual action="insert Extend_System" >
        <Title>Insert/GNAT pragma/Extend_System</Title>
     </contextual>

     <action name="insert Export_Valued_Procedure" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export_Valued_Procedure()</shell>
     </action>
     <contextual action="insert Export_Valued_Procedure" >
        <Title>Insert/GNAT pragma/Export_Valued_Procedure</Title>
     </contextual>

     <action name="insert Export_Value" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export_Value()</shell>
     </action>
     <contextual action="insert Export_Value" >
        <Title>Insert/GNAT pragma/Export_Value</Title>
     </contextual>

     <action name="insert Export_Procedure" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export_Procedure()</shell>
     </action>
     <contextual action="insert Export_Procedure" >
        <Title>Insert/GNAT pragma/Export_Procedure</Title>
     </contextual>

     <action name="insert Export_Object" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export_Object()</shell>
     </action>
     <contextual action="insert Export_Object" >
        <Title>Insert/GNAT pragma/Export_Object</Title>
     </contextual>

     <action name="insert Export_Function" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export_Function()</shell>
     </action>
     <contextual action="insert Export_Function" >
        <Title>Insert/GNAT pragma/Export_Function</Title>
     </contextual>

     <action name="insert Export_Exception" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export_Exception()</shell>
     </action>
     <contextual action="insert Export_Exception" >
        <Title>Insert/GNAT pragma/Export_Exception</Title>
     </contextual>

     <action name="insert Eliminate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Eliminate()</shell>
     </action>
     <contextual action="insert Eliminate" >
        <Title>Insert/GNAT pragma/Eliminate</Title>
     </contextual>

     <action name="insert Elaboration_Checks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaboration_Checks()</shell>
     </action>
     <contextual action="insert Elaboration_Checks" >
        <Title>Insert/GNAT pragma/Elaboration_Checks</Title>
     </contextual>

     <action name="insert Debug" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Debug()</shell>
     </action>
     <contextual action="insert Debug" >
        <Title>Insert/GNAT pragma/Debug</Title>
     </contextual>

     <action name="insert Convention_Identifier" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Convention_Identifier()</shell>
     </action>
     <contextual action="insert Convention_Identifier" >
        <Title>Insert/GNAT pragma/Convention_Identifier</Title>
     </contextual>

     <action name="insert Component_Alignment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Component_Alignment()</shell>
     </action>
     <contextual action="insert Component_Alignment" >
        <Title>Insert/GNAT pragma/Component_Alignment</Title>
     </contextual>

     <action name="insert Complex_Representation" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Complex_Representation()</shell>
     </action>
     <contextual action="insert Complex_Representation" >
        <Title>Insert/GNAT pragma/Complex_Representation</Title>
     </contextual>

     <action name="insert Compile_Time_Warning" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Compile_Time_Warning()</shell>
     </action>
     <contextual action="insert Compile_Time_Warning" >
        <Title>Insert/GNAT pragma/Compile_Time_Warning</Title>
     </contextual>

     <action name="insert Common_Object" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Common_Object()</shell>
     </action>
     <contextual action="insert Common_Object" >
        <Title>Insert/GNAT pragma/Common_Object</Title>
     </contextual>

     <action name="insert Comment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Comment()</shell>
     </action>
     <contextual action="insert Comment" >
        <Title>Insert/GNAT pragma/Comment</Title>
     </contextual>

     <action name="insert C_Pass_By_Copy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_C_Pass_By_Copy()</shell>
     </action>
     <contextual action="insert C_Pass_By_Copy" >
        <Title>Insert/GNAT pragma/C_Pass_By_Copy</Title>
     </contextual>

     <action name="insert CPP_Vtable" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_CPP_Vtable()</shell>
     </action>
     <contextual action="insert CPP_Vtable" >
        <Title>Insert/GNAT pragma/CPP_Vtable</Title>
     </contextual>

     <action name="insert CPP_Virtual" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_CPP_Virtual()</shell>
     </action>
     <contextual action="insert CPP_Virtual" >
        <Title>Insert/GNAT pragma/CPP_Virtual</Title>
     </contextual>

     <action name="insert CPP_Constructor" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_CPP_Constructor()</shell>
     </action>
     <contextual action="insert CPP_Constructor" >
        <Title>Insert/GNAT pragma/CPP_Constructor</Title>
     </contextual>

     <action name="insert CPP_Class" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_CPP_Class()</shell>
     </action>
     <contextual action="insert CPP_Class" >
        <Title>Insert/GNAT pragma/CPP_Class</Title>
     </contextual>

     <action name="insert Ast_Entry" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ast_Entry()</shell>
     </action>
     <contextual action="insert Ast_Entry" >
        <Title>Insert/GNAT pragma/Ast_Entry</Title>
     </contextual>

     <action name="insert Assert" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Assert()</shell>
     </action>
     <contextual action="insert Assert" >
        <Title>Insert/GNAT pragma/Assert</Title>
     </contextual>

     <action name="insert Annotate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Annotate()</shell>
     </action>
     <contextual action="insert Annotate" >
        <Title>Insert/GNAT pragma/Annotate</Title>
     </contextual>

     <action name="insert Ada_95" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_95()</shell>
     </action>
     <contextual action="insert Ada_95" >
        <Title>Insert/GNAT pragma/Ada_95</Title>
     </contextual>

     <action name="insert Ada_83" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_83()</shell>
     </action>
     <contextual action="insert Ada_83" >
        <Title>Insert/GNAT pragma/Ada_83</Title>
     </contextual>

     <action name="insert Abort_Defer" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Abort_Defer()</shell>
     </action>
     <contextual action="insert Abort_Defer" >
        <Title>Insert/GNAT pragma/Abort_Defer</Title>
     </contextual>
""")





# the GNAT-defined names

GPS.parse_xml ("""
     <action name="insert System_Wch_Con" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Wch_Con()</shell>
     </action>
     <contextual action="insert System_Wch_Con" >
        <Title>Insert/GNAT name/System.Wch_Con</Title>
     </contextual>

     <action name="insert System_Wch_Cnv" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Wch_Cnv()</shell>
     </action>
     <contextual action="insert System_Wch_Cnv" >
        <Title>Insert/GNAT name/System.Wch_Cnv</Title>
     </contextual>

     <action name="insert System_Task_Info" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Task_Info()</shell>
     </action>
     <contextual action="insert System_Task_Info" >
        <Title>Insert/GNAT name/System.Task_Info</Title>
     </contextual>

     <action name="insert System_Partition_Interface" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Partition_Interface()</shell>
     </action>
     <contextual action="insert System_Partition_Interface" >
        <Title>Insert/GNAT name/System.Partition_Interface</Title>
     </contextual>

     <action name="insert System_Memory" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Memory()</shell>
     </action>
     <contextual action="insert System_Memory" >
        <Title>Insert/GNAT name/System.Memory</Title>
     </contextual>

     <action name="insert System_Assertions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Assertions()</shell>
     </action>
     <contextual action="insert System_Assertions" >
        <Title>Insert/GNAT name/System.Assertions</Title>
     </contextual>

     <action name="insert System_Address_Image" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System_Address_Image()</shell>
     </action>
     <contextual action="insert System_Address_Image" >
        <Title>Insert/GNAT name/System.Address_Image</Title>
     </contextual>

     <action name="insert Interfaces_VxWorks_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_VxWorks_IO()</shell>
     </action>
     <contextual action="insert Interfaces_VxWorks_IO" >
        <Title>Insert/GNAT name/Interfaces.VxWorks.IO</Title>
     </contextual>

     <action name="insert Interfaces_VxWorks" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_VxWorks()</shell>
     </action>
     <contextual action="insert Interfaces_VxWorks" >
        <Title>Insert/GNAT name/Interfaces.VxWorks</Title>
     </contextual>

     <action name="insert Interfaces_Packed_Decimal" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Packed_Decimal()</shell>
     </action>
     <contextual action="insert Interfaces_Packed_Decimal" >
        <Title>Insert/GNAT name/Interfaces.Packed_Decimal</Title>
     </contextual>

     <action name="insert Interfaces_Os2lib_Threads" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Os2lib_Threads()</shell>
     </action>
     <contextual action="insert Interfaces_Os2lib_Threads" >
        <Title>Insert/GNAT name/Interfaces.Os2lib.Threads</Title>
     </contextual>

     <action name="insert Interfaces_Os2lib_Synchronization" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Os2lib_Synchronization()</shell>
     </action>
     <contextual action="insert Interfaces_Os2lib_Synchronization" >
        <Title>Insert/GNAT name/Interfaces.Os2lib.Synchronization</Title>
     </contextual>

     <action name="insert Interfaces_Os2lib_Errors" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Os2lib_Errors()</shell>
     </action>
     <contextual action="insert Interfaces_Os2lib_Errors" >
        <Title>Insert/GNAT name/Interfaces.Os2lib.Errors</Title>
     </contextual>

     <action name="insert Interfaces_Os2lib" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Os2lib()</shell>
     </action>
     <contextual action="insert Interfaces_Os2lib" >
        <Title>Insert/GNAT name/Interfaces.Os2lib</Title>
     </contextual>

     <action name="insert Interfaces_CPP" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_CPP()</shell>
     </action>
     <contextual action="insert Interfaces_CPP" >
        <Title>Insert/GNAT name/Interfaces.CPP</Title>
     </contextual>

     <action name="insert Interfaces_C_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_C_Streams()</shell>
     </action>
     <contextual action="insert Interfaces_C_Streams" >
        <Title>Insert/GNAT name/Interfaces.C.Streams</Title>
     </contextual>

     <action name="insert Interfaces_C_Extensions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_C_Extensions()</shell>
     </action>
     <contextual action="insert Interfaces_C_Extensions" >
        <Title>Insert/GNAT name/Interfaces.C.Extensions</Title>
     </contextual>

     <action name="insert GNAT_Wide_String_Split" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Wide_String_Split()</shell>
     </action>
     <contextual action="insert GNAT_Wide_String_Split" >
        <Title>Insert/GNAT name/GNAT.Wide_String_Split</Title>
     </contextual>

     <action name="insert GNAT_Traceback_Symbolic" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Traceback_Symbolic()</shell>
     </action>
     <contextual action="insert GNAT_Traceback_Symbolic" >
        <Title>Insert/GNAT name/GNAT.Traceback.Symbolic</Title>
     </contextual>

     <action name="insert GNAT_Traceback" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Traceback()</shell>
     </action>
     <contextual action="insert GNAT_Traceback" >
        <Title>Insert/GNAT name/GNAT.Traceback</Title>
     </contextual>

     <action name="insert GNAT_Threads" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Threads()</shell>
     </action>
     <contextual action="insert GNAT_Threads" >
        <Title>Insert/GNAT name/GNAT.Threads</Title>
     </contextual>

     <action name="insert GNAT_Task_Lock" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Task_Lock()</shell>
     </action>
     <contextual action="insert GNAT_Task_Lock" >
        <Title>Insert/GNAT name/GNAT.Task_Lock</Title>
     </contextual>

     <action name="insert GNAT_Table" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Table()</shell>
     </action>
     <contextual action="insert GNAT_Table" >
        <Title>Insert/GNAT name/GNAT.Table</Title>
     </contextual>

     <action name="insert GNAT_Strings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Strings()</shell>
     </action>
     <contextual action="insert GNAT_Strings" >
        <Title>Insert/GNAT name/GNAT.Strings</Title>
     </contextual>

     <action name="insert GNAT_String_Split" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_String_Split()</shell>
     </action>
     <contextual action="insert GNAT_String_Split" >
        <Title>Insert/GNAT name/GNAT.String_Split</Title>
     </contextual>

     <action name="insert GNAT_Spitbol_Table_VString" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Spitbol_Table_VString()</shell>
     </action>
     <contextual action="insert GNAT_Spitbol_Table_VString" >
        <Title>Insert/GNAT name/GNAT.Spitbol.Table_VString</Title>
     </contextual>

     <action name="insert GNAT_Spitbol_Table_Integer" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Spitbol_Table_Integer()</shell>
     </action>
     <contextual action="insert GNAT_Spitbol_Table_Integer" >
        <Title>Insert/GNAT name/GNAT.Spitbol.Table_Integer</Title>
     </contextual>

     <action name="insert GNAT_Spitbol_Table_Boolean" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Spitbol_Table_Boolean()</shell>
     </action>
     <contextual action="insert GNAT_Spitbol_Table_Boolean" >
        <Title>Insert/GNAT name/GNAT.Spitbol.Table_Boolean</Title>
     </contextual>

     <action name="insert GNAT_Spitbol_Patterns" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Spitbol_Patterns()</shell>
     </action>
     <contextual action="insert GNAT_Spitbol_Patterns" >
        <Title>Insert/GNAT name/GNAT.Spitbol.Patterns</Title>
     </contextual>

     <action name="insert GNAT_Spitbol" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Spitbol()</shell>
     </action>
     <contextual action="insert GNAT_Spitbol" >
        <Title>Insert/GNAT name/GNAT.Spitbol</Title>
     </contextual>

     <action name="insert GNAT_Spell_Checker" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Spell_Checker()</shell>
     </action>
     <contextual action="insert GNAT_Spell_Checker" >
        <Title>Insert/GNAT name/GNAT.Spell_Checker</Title>
     </contextual>

     <action name="insert GNAT_Source_Info" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Source_Info()</shell>
     </action>
     <contextual action="insert GNAT_Source_Info" >
        <Title>Insert/GNAT name/GNAT.Source_Info</Title>
     </contextual>

     <action name="insert GNAT_Sockets" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Sockets()</shell>
     </action>
     <contextual action="insert GNAT_Sockets" >
        <Title>Insert/GNAT name/GNAT.Sockets</Title>
     </contextual>

     <action name="insert GNAT_Signals" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Signals()</shell>
     </action>
     <contextual action="insert GNAT_Signals" >
        <Title>Insert/GNAT name/GNAT.Signals</Title>
     </contextual>

     <action name="insert GNAT_Semaphores" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Semaphores()</shell>
     </action>
     <contextual action="insert GNAT_Semaphores" >
        <Title>Insert/GNAT name/GNAT.Semaphores</Title>
     </contextual>

     <action name="insert GNAT_Regpat" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Regpat()</shell>
     </action>
     <contextual action="insert GNAT_Regpat" >
        <Title>Insert/GNAT name/GNAT.Regpat</Title>
     </contextual>

     <action name="insert GNAT_Registry" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Registry()</shell>
     </action>
     <contextual action="insert GNAT_Registry" >
        <Title>Insert/GNAT name/GNAT.Registry</Title>
     </contextual>

     <action name="insert GNAT_Regexp" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Regexp()</shell>
     </action>
     <contextual action="insert GNAT_Regexp" >
        <Title>Insert/GNAT name/GNAT.Regexp</Title>
     </contextual>

     <action name="insert GNAT_Perfect_Hash_Generators" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Perfect_Hash_Generators()</shell>
     </action>
     <contextual action="insert GNAT_Perfect_Hash_Generators" >
        <Title>Insert/GNAT name/GNAT.Perfect_Hash_Generators</Title>
     </contextual>

     <action name="insert GNAT_OS_Lib" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_OS_Lib()</shell>
     </action>
     <contextual action="insert GNAT_OS_Lib" >
        <Title>Insert/GNAT name/GNAT.OS_Lib</Title>
     </contextual>

     <action name="insert GNAT_Most_Recent_Exception" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Most_Recent_Exception()</shell>
     </action>
     <contextual action="insert GNAT_Most_Recent_Exception" >
        <Title>Insert/GNAT name/GNAT.Most_Recent_Exception</Title>
     </contextual>

     <action name="insert GNAT_Memory_Dump" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Memory_Dump()</shell>
     </action>
     <contextual action="insert GNAT_Memory_Dump" >
        <Title>Insert/GNAT name/GNAT.Memory_Dump</Title>
     </contextual>

     <action name="insert GNAT_MD5" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_MD5()</shell>
     </action>
     <contextual action="insert GNAT_MD5" >
        <Title>Insert/GNAT name/GNAT.MD5</Title>
     </contextual>

     <action name="insert GNAT_Lock_Files" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Lock_Files()</shell>
     </action>
     <contextual action="insert GNAT_Lock_Files" >
        <Title>Insert/GNAT name/GNAT.Lock_Files</Title>
     </contextual>

     <action name="insert GNAT_IO_Aux" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_IO_Aux()</shell>
     </action>
     <contextual action="insert GNAT_IO_Aux" >
        <Title>Insert/GNAT name/GNAT.IO_Aux</Title>
     </contextual>

     <action name="insert GNAT_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_IO()</shell>
     </action>
     <contextual action="insert GNAT_IO" >
        <Title>Insert/GNAT name/GNAT.IO</Title>
     </contextual>

     <action name="insert GNAT_Heap_Sort_G" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Heap_Sort_G()</shell>
     </action>
     <contextual action="insert GNAT_Heap_Sort_G" >
        <Title>Insert/GNAT name/GNAT.Heap_Sort_G</Title>
     </contextual>

     <action name="insert GNAT_Heap_Sort_A" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Heap_Sort_A()</shell>
     </action>
     <contextual action="insert GNAT_Heap_Sort_A" >
        <Title>Insert/GNAT name/GNAT.Heap_Sort_A</Title>
     </contextual>

     <action name="insert GNAT_Heap_Sort" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Heap_Sort()</shell>
     </action>
     <contextual action="insert GNAT_Heap_Sort" >
        <Title>Insert/GNAT name/GNAT.Heap_Sort</Title>
     </contextual>

     <action name="insert GNAT_HTable" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_HTable()</shell>
     </action>
     <contextual action="insert GNAT_HTable" >
        <Title>Insert/GNAT name/GNAT.HTable</Title>
     </contextual>

     <action name="insert GNAT_Float_Control" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Float_Control()</shell>
     </action>
     <contextual action="insert GNAT_Float_Control" >
        <Title>Insert/GNAT name/GNAT.Float_Control</Title>
     </contextual>

     <action name="insert GNAT_Expect" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Expect()</shell>
     </action>
     <contextual action="insert GNAT_Expect" >
        <Title>Insert/GNAT name/GNAT.Expect</Title>
     </contextual>

     <action name="insert GNAT_Exceptions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Exceptions()</shell>
     </action>
     <contextual action="insert GNAT_Exceptions" >
        <Title>Insert/GNAT name/GNAT.Exceptions</Title>
     </contextual>

     <action name="insert GNAT_Exception_Traces" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Exception_Traces()</shell>
     </action>
     <contextual action="insert GNAT_Exception_Traces" >
        <Title>Insert/GNAT name/GNAT.Exception_Traces</Title>
     </contextual>

     <action name="insert GNAT_Exception_Actions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Exception_Actions()</shell>
     </action>
     <contextual action="insert GNAT_Exception_Actions" >
        <Title>Insert/GNAT name/GNAT.Exception_Actions</Title>
     </contextual>

     <action name="insert GNAT_Dynamic_Tables" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Dynamic_Tables()</shell>
     </action>
     <contextual action="insert GNAT_Dynamic_Tables" >
        <Title>Insert/GNAT name/GNAT.Dynamic_Tables</Title>
     </contextual>

     <action name="insert GNAT_Dynamic_HTables" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Dynamic_HTables()</shell>
     </action>
     <contextual action="insert GNAT_Dynamic_HTables" >
        <Title>Insert/GNAT name/GNAT.Dynamic_HTables</Title>
     </contextual>

     <action name="insert GNAT_Directory_Operations" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Directory_Operations()</shell>
     </action>
     <contextual action="insert GNAT_Directory_Operations" >
        <Title>Insert/GNAT name/GNAT.Directory_Operations</Title>
     </contextual>

     <action name="insert GNAT_Debug_Utilities" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Debug_Utilities()</shell>
     </action>
     <contextual action="insert GNAT_Debug_Utilities" >
        <Title>Insert/GNAT name/GNAT.Debug_Utilities</Title>
     </contextual>

     <action name="insert GNAT_Debug_Pools" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Debug_Pools()</shell>
     </action>
     <contextual action="insert GNAT_Debug_Pools" >
        <Title>Insert/GNAT name/GNAT.Debug_Pools</Title>
     </contextual>

     <action name="insert GNAT_Current_Exception" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Current_Exception()</shell>
     </action>
     <contextual action="insert GNAT_Current_Exception" >
        <Title>Insert/GNAT name/GNAT.Current_Exception</Title>
     </contextual>

     <action name="insert GNAT_Ctrl_C" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Ctrl_C()</shell>
     </action>
     <contextual action="insert GNAT_Ctrl_C" >
        <Title>Insert/GNAT name/GNAT.Ctrl_C</Title>
     </contextual>

     <action name="insert GNAT_Compiler_Version" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Compiler_Version()</shell>
     </action>
     <contextual action="insert GNAT_Compiler_Version" >
        <Title>Insert/GNAT name/GNAT.Compiler_Version</Title>
     </contextual>

     <action name="insert GNAT_Command_Line" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Command_Line()</shell>
     </action>
     <contextual action="insert GNAT_Command_Line" >
        <Title>Insert/GNAT name/GNAT.Command_Line</Title>
     </contextual>

     <action name="insert GNAT_Case_Util" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Case_Util()</shell>
     </action>
     <contextual action="insert GNAT_Case_Util" >
        <Title>Insert/GNAT name/GNAT.Case_Util</Title>
     </contextual>

     <action name="insert GNAT_Calendar_Time_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Calendar_Time_IO()</shell>
     </action>
     <contextual action="insert GNAT_Calendar_Time_IO" >
        <Title>Insert/GNAT name/GNAT.Calendar.Time_IO</Title>
     </contextual>

     <action name="insert GNAT_Calendar" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Calendar()</shell>
     </action>
     <contextual action="insert GNAT_Calendar" >
        <Title>Insert/GNAT name/GNAT.Calendar</Title>
     </contextual>

     <action name="insert GNAT_CRC32" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_CRC32()</shell>
     </action>
     <contextual action="insert GNAT_CRC32" >
        <Title>Insert/GNAT name/GNAT.CRC32</Title>
     </contextual>

     <action name="insert GNAT_CGI_Debug" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_CGI_Debug()</shell>
     </action>
     <contextual action="insert GNAT_CGI_Debug" >
        <Title>Insert/GNAT name/GNAT.CGI.Debug</Title>
     </contextual>

     <action name="insert GNAT_CGI_Cookie" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_CGI_Cookie()</shell>
     </action>
     <contextual action="insert GNAT_CGI_Cookie" >
        <Title>Insert/GNAT name/GNAT.CGI.Cookie</Title>
     </contextual>

     <action name="insert GNAT_CGI" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_CGI()</shell>
     </action>
     <contextual action="insert GNAT_CGI" >
        <Title>Insert/GNAT name/GNAT.CGI</Title>
     </contextual>

     <action name="insert GNAT_Bubble_Sort_G" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Bubble_Sort_G()</shell>
     </action>
     <contextual action="insert GNAT_Bubble_Sort_G" >
        <Title>Insert/GNAT name/GNAT.Bubble_Sort_G</Title>
     </contextual>

     <action name="insert GNAT_Bubble_Sort_A" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Bubble_Sort_A()</shell>
     </action>
     <contextual action="insert GNAT_Bubble_Sort_A" >
        <Title>Insert/GNAT name/GNAT.Bubble_Sort_A</Title>
     </contextual>

     <action name="insert GNAT_Bubble_Sort" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Bubble_Sort()</shell>
     </action>
     <contextual action="insert GNAT_Bubble_Sort" >
        <Title>Insert/GNAT name/GNAT.Bubble_Sort</Title>
     </contextual>

     <action name="insert GNAT_Bounded_Mailboxes" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Bounded_Mailboxes()</shell>
     </action>
     <contextual action="insert GNAT_Bounded_Mailboxes" >
        <Title>Insert/GNAT name/GNAT.Bounded_Mailboxes</Title>
     </contextual>

     <action name="insert GNAT_Bounded_Buffers" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Bounded_Buffers()</shell>
     </action>
     <contextual action="insert GNAT_Bounded_Buffers" >
        <Title>Insert/GNAT name/GNAT.Bounded_Buffers</Title>
     </contextual>

     <action name="insert GNAT_Array_Split" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_Array_Split()</shell>
     </action>
     <contextual action="insert GNAT_Array_Split" >
        <Title>Insert/GNAT name/GNAT.Array_Split</Title>
     </contextual>

     <action name="insert GNAT_AWK" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_GNAT_AWK()</shell>
     </action>
     <contextual action="insert GNAT_AWK" >
        <Title>Insert/GNAT name/GNAT.AWK</Title>
     </contextual>

     <action name="insert Ada_Wide_Text_IO_C_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Wide_Text_IO_C_Streams()</shell>
     </action>
     <contextual action="insert Ada_Wide_Text_IO_C_Streams" >
        <Title>Insert/GNAT name/Ada.Wide_Text_IO.C_Streams</Title>
     </contextual>

     <action name="insert Ada_Text_IO_C_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Text_IO_C_Streams()</shell>
     </action>
     <contextual action="insert Ada_Text_IO_C_Streams" >
        <Title>Insert/GNAT name/Ada.Text_IO.C_Streams</Title>
     </contextual>

     <action name="insert Ada_Strings_Wide_Unbounded_Wide_Text_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Strings_Wide_Unbounded_Wide_Text_IO()</shell>
     </action>
     <contextual action="insert Ada_Strings_Wide_Unbounded_Wide_Text_IO" >
        <Title>Insert/GNAT name/Ada.Strings.Wide_Unbounded.Wide_Text_IO</Title>
     </contextual>

     <action name="insert Ada_Strings_Unbounded_Text_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Strings_Unbounded_Text_IO()</shell>
     </action>
     <contextual action="insert Ada_Strings_Unbounded_Text_IO" >
        <Title>Insert/GNAT name/Ada.Strings.Unbounded.Text_IO</Title>
     </contextual>

     <action name="insert Ada_Streams_Stream_IO_C_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Streams_Stream_IO_C_Streams()</shell>
     </action>
     <contextual action="insert Ada_Streams_Stream_IO_C_Streams" >
        <Title>Insert/GNAT name/Ada.Streams.Stream_IO.C_Streams</Title>
     </contextual>

     <action name="insert Ada_Sequential_IO_C_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Sequential_IO_C_Streams()</shell>
     </action>
     <contextual action="insert Ada_Sequential_IO_C_Streams" >
        <Title>Insert/GNAT name/Ada.Sequential_IO.C_Streams</Title>
     </contextual>

     <action name="insert Ada_Exceptions_Traceback" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Exceptions_Traceback()</shell>
     </action>
     <contextual action="insert Ada_Exceptions_Traceback" >
        <Title>Insert/GNAT name/Ada.Exceptions.Traceback</Title>
     </contextual>

     <action name="insert Ada_Exceptions_Is_Null_Occurrence" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Exceptions_Is_Null_Occurrence()</shell>
     </action>
     <contextual action="insert Ada_Exceptions_Is_Null_Occurrence" >
        <Title>Insert/GNAT name/Ada.Exceptions.Is_Null_Occurrence</Title>
     </contextual>

     <action name="insert Ada_Direct_IO_C_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Direct_IO_C_Streams()</shell>
     </action>
     <contextual action="insert Ada_Direct_IO_C_Streams" >
        <Title>Insert/GNAT name/Ada.Direct_IO.C_Streams</Title>
     </contextual>

     <action name="insert Ada_Command_Line_Remove" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Command_Line_Remove()</shell>
     </action>
     <contextual action="insert Ada_Command_Line_Remove" >
        <Title>Insert/GNAT name/Ada.Command_Line.Remove</Title>
     </contextual>

     <action name="insert Ada_Command_Line_Environment" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Command_Line_Environment()</shell>
     </action>
     <contextual action="insert Ada_Command_Line_Environment" >
        <Title>Insert/GNAT name/Ada.Command_Line.Environment</Title>
     </contextual>

     <action name="insert Ada_Characters_Wide_Latin_9" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Characters_Wide_Latin_9()</shell>
     </action>
     <contextual action="insert Ada_Characters_Wide_Latin_9" >
        <Title>Insert/GNAT name/Ada.Characters.Wide_Latin_9</Title>
     </contextual>

     <action name="insert Ada_Characters_Wide_Latin_1" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Characters_Wide_Latin_1()</shell>
     </action>
     <contextual action="insert Ada_Characters_Wide_Latin_1" >
        <Title>Insert/GNAT name/Ada.Characters.Wide_Latin_1</Title>
     </contextual>

     <action name="insert Ada_Characters_Latin_9" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Ada_Characters_Latin_9()</shell>
     </action>
     <contextual action="insert Ada_Characters_Latin_9" >
        <Title>Insert/GNAT name/Ada.Characters.Latin_9</Title>
     </contextual>

""")




# the "Ada pragmas" submenu

GPS.parse_xml ("""
     <action name="insert Volatile_Components" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Volatile_Components()</shell>
     </action>
     <contextual action="insert Volatile_Components" >
        <Title>Insert/Standard pragma/Volatile_Components</Title>
     </contextual>

     <action name="insert Volatile" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Volatile()</shell>
     </action>
     <contextual action="insert Volatile" >
        <Title>Insert/Standard pragma/Volatile</Title>
     </contextual>

     <action name="insert Task_Dispatching_Policy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Dispatching_Policy()</shell>
     </action>
     <contextual action="insert Task_Dispatching_Policy" >
        <Title>Insert/Standard pragma/Task_Dispatching_Policy</Title>
     </contextual>

     <action name="insert Suppress" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Suppress()</shell>
     </action>
     <contextual action="insert Suppress" >
        <Title>Insert/Standard pragma/Suppress</Title>
     </contextual>

     <action name="insert Storage_Size" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_Size()</shell>
     </action>
     <contextual action="insert Storage_Size" >
        <Title>Insert/Standard pragma/Storage_Size</Title>
     </contextual>

     <action name="insert Shared_Passive" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Shared_Passive()</shell>
     </action>
     <contextual action="insert Shared_Passive" >
        <Title>Insert/Standard pragma/Shared_Passive</Title>
     </contextual>

     <action name="insert Reviewable" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Reviewable()</shell>
     </action>
     <contextual action="insert Reviewable" >
        <Title>Insert/Standard pragma/Reviewable</Title>
     </contextual>

     <action name="insert Restrictions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Restrictions()</shell>
     </action>
     <contextual action="insert Restrictions" >
        <Title>Insert/Standard pragma/Restrictions</Title>
     </contextual>

     <action name="insert Remote_Types" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Remote_Types()</shell>
     </action>
     <contextual action="insert Remote_Types" >
        <Title>Insert/Standard pragma/Remote_Types</Title>
     </contextual>

     <action name="insert Remote_Call_Interface" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Remote_Call_Interface()</shell>
     </action>
     <contextual action="insert Remote_Call_Interface" >
        <Title>Insert/Standard pragma/Remote_Call_Interface</Title>
     </contextual>

     <action name="insert Queuing_Policy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Queuing_Policy()</shell>
     </action>
     <contextual action="insert Queuing_Policy" >
        <Title>Insert/Standard pragma/Queuing_Policy</Title>
     </contextual>

     <action name="insert Pure" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Pure()</shell>
     </action>
     <contextual action="insert Pure" >
        <Title>Insert/Standard pragma/Pure</Title>
     </contextual>

     <action name="insert Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Priority()</shell>
     </action>
     <contextual action="insert Priority" >
        <Title>Insert/Standard pragma/Priority</Title>
     </contextual>

     <action name="insert Preelaborate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Preelaborate()</shell>
     </action>
     <contextual action="insert Preelaborate" >
        <Title>Insert/Standard pragma/Preelaborate</Title>
     </contextual>

     <action name="insert Page" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Page()</shell>
     </action>
     <contextual action="insert Page" >
        <Title>Insert/Standard pragma/Page</Title>
     </contextual>

     <action name="insert Pack" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Pack()</shell>
     </action>
     <contextual action="insert Pack" >
        <Title>Insert/Standard pragma/Pack</Title>
     </contextual>

     <action name="insert Optimize" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Optimize()</shell>
     </action>
     <contextual action="insert Optimize" >
        <Title>Insert/Standard pragma/Optimize</Title>
     </contextual>

     <action name="insert Normalize_Scalars" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Normalize_Scalars()</shell>
     </action>
     <contextual action="insert Normalize_Scalars" >
        <Title>Insert/Standard pragma/Normalize_Scalars</Title>
     </contextual>

     <action name="insert Locking_Policy" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Locking_Policy()</shell>
     </action>
     <contextual action="insert Locking_Policy" >
        <Title>Insert/Standard pragma/Locking_Policy</Title>
     </contextual>

     <action name="insert List" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_List()</shell>
     </action>
     <contextual action="insert List" >
        <Title>Insert/Standard pragma/List</Title>
     </contextual>

     <action name="insert Linker_Options" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Linker_Options()</shell>
     </action>
     <contextual action="insert Linker_Options" >
        <Title>Insert/Standard pragma/Linker_Options</Title>
     </contextual>

     <action name="insert Interrupt_Priority" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupt_Priority()</shell>
     </action>
     <contextual action="insert Interrupt_Priority" >
        <Title>Insert/Standard pragma/Interrupt_Priority</Title>
     </contextual>

     <action name="insert Interrupt_Handler" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupt_Handler()</shell>
     </action>
     <contextual action="insert Interrupt_Handler" >
        <Title>Insert/Standard pragma/Interrupt_Handler</Title>
     </contextual>

     <action name="insert Inspection_Point" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inspection_Point()</shell>
     </action>
     <contextual action="insert Inspection_Point" >
        <Title>Insert/Standard pragma/Inspection_Point</Title>
     </contextual>

     <action name="insert Inline" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Inline()</shell>
     </action>
     <contextual action="insert Inline" >
        <Title>Insert/Standard pragma/Inline</Title>
     </contextual>

     <action name="insert Import" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Import()</shell>
     </action>
     <contextual action="insert Import" >
        <Title>Insert/Standard pragma/Import</Title>
     </contextual>

     <action name="insert Export" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Export()</shell>
     </action>
     <contextual action="insert Export" >
        <Title>Insert/Standard pragma/Export</Title>
     </contextual>

     <action name="insert Elaborate_Body" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborate_Body()</shell>
     </action>
     <contextual action="insert Elaborate_Body" >
        <Title>Insert/Standard pragma/Elaborate_Body</Title>
     </contextual>

     <action name="insert Elaborate_All" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborate_All()</shell>
     </action>
     <contextual action="insert Elaborate_All" >
        <Title>Insert/Standard pragma/Elaborate_All</Title>
     </contextual>

     <action name="insert Elaborate" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elaborate()</shell>
     </action>
     <contextual action="insert Elaborate" >
        <Title>Insert/Standard pragma/Elaborate</Title>
     </contextual>

     <action name="insert Discard_Names" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Discard_Names()</shell>
     </action>
     <contextual action="insert Discard_Names" >
        <Title>Insert/Standard pragma/Discard_Names</Title>
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

     <action name="insert Attach_Handler" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Attach_Handler()</shell>
     </action>
     <contextual action="insert Attach_Handler" >
        <Title>Insert/Standard pragma/Attach_Handler</Title>
     </contextual>

     <action name="insert Atomic_Components" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Atomic_Components()</shell>
     </action>
     <contextual action="insert Atomic_Components" >
        <Title>Insert/Standard pragma/Atomic_Components</Title>
     </contextual>

     <action name="insert Atomic" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Atomic()</shell>
     </action>
     <contextual action="insert Atomic" >
        <Title>Insert/Standard pragma/Atomic</Title>
     </contextual>

     <action name="insert Asynchronous" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asynchronous()</shell>
     </action>
     <contextual action="insert Asynchronous" >
        <Title>Insert/Standard pragma/Asynchronous</Title>
     </contextual>

     <action name="insert All_Calls_Remote" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_All_Calls_Remote()</shell>
     </action>
     <contextual action="insert All_Calls_Remote" >
        <Title>Insert/Standard pragma/All_Calls_Remote</Title>
     </contextual>
""")


# the predefined names

GPS.parse_xml ("""
     <action name="insert Storage_Pools" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_Pools()</shell>
     </action>
     <contextual action="insert Storage_Pools" >
        <Title>Insert/Standard name/Low Level Programming/Storage_Pools</Title>
     </contextual>

     <action name="insert Storage_Elements" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_Elements()</shell>
     </action>
     <contextual action="insert Storage_Elements" >
        <Title>Insert/Standard name/Low Level Programming/Storage_Elements</Title>
     </contextual>

     <action name="insert Machine_Code" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Machine_Code()</shell>
     </action>
     <contextual action="insert Machine_Code" >
        <Title>Insert/Standard name/Low Level Programming/Machine_Code</Title>
     </contextual>

     <action name="insert Address_To_Access_Conversions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Address_To_Access_Conversions()</shell>
     </action>
     <contextual action="insert Address_To_Access_Conversions" >
        <Title>Insert/Standard name/Low Level Programming/Address_To_Access_Conversions</Title>
     </contextual>

     <action name="insert System" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_System()</shell>
     </action>
     <contextual action="insert System" >
        <Title>Insert/Standard name/Low Level Programming/System</Title>
     </contextual>

     <action name="insert Interfaces_Fortran" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Fortran()</shell>
     </action>
     <contextual action="insert Interfaces_Fortran" >
        <Title>Insert/Standard name/Interfacing/Interfaces.Fortran</Title>
     </contextual>

     <action name="insert Interfaces_Cobol" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_Cobol()</shell>
     </action>
     <contextual action="insert Interfaces_Cobol" >
        <Title>Insert/Standard name/Interfacing/Interfaces.Cobol</Title>
     </contextual>

     <action name="insert Interfaces_C_Strings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_C_Strings()</shell>
     </action>
     <contextual action="insert Interfaces_C_Strings" >
        <Title>Insert/Standard name/Interfacing/Interfaces.C.Strings</Title>
     </contextual>

     <action name="insert Interfaces_C_Pointers" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_C_Pointers()</shell>
     </action>
     <contextual action="insert Interfaces_C_Pointers" >
        <Title>Insert/Standard name/Interfacing/Interfaces.C.Pointers</Title>
     </contextual>

     <action name="insert Interfaces_C" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces_C()</shell>
     </action>
     <contextual action="insert Interfaces_C" >
        <Title>Insert/Standard name/Interfacing/Interfaces.C</Title>
     </contextual>

     <action name="insert Interfaces" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interfaces()</shell>
     </action>
     <contextual action="insert Interfaces" >
        <Title>Insert/Standard name/Interfacing/Interfaces</Title>
     </contextual>

     <action name="insert Wide_Text_IO_Text_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Wide_Text_IO_Text_Streams()</shell>
     </action>
     <contextual action="insert Wide_Text_IO_Text_Streams" >
        <Title>Insert/Standard name/Input+Output/Wide_Text_IO.Text_Streams</Title>
     </contextual>

     <action name="insert Wide_Text_IO_Editing" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Wide_Text_IO_Editing()</shell>
     </action>
     <contextual action="insert Wide_Text_IO_Editing" >
        <Title>Insert/Standard name/Input+Output/Wide_Text_IO.Editing</Title>
     </contextual>

     <action name="insert Wide_Text_IO_Complex_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Wide_Text_IO_Complex_IO()</shell>
     </action>
     <contextual action="insert Wide_Text_IO_Complex_IO" >
        <Title>Insert/Standard name/Input+Output/Wide_Text_IO.Complex_IO</Title>
     </contextual>

     <action name="insert Wide_Text_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Wide_Text_IO()</shell>
     </action>
     <contextual action="insert Wide_Text_IO" >
        <Title>Insert/Standard name/Input+Output/Wide_Text_IO</Title>
     </contextual>

     <action name="insert Unchecked_Deallocation" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unchecked_Deallocation()</shell>
     </action>
     <contextual action="insert Unchecked_Deallocation" >
        <Title>Insert/Standard name/Low Level Programming/Unchecked_Deallocation</Title>
     </contextual>

     <action name="insert Unchecked_Conversion" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Unchecked_Conversion()</shell>
     </action>
     <contextual action="insert Unchecked_Conversion" >
        <Title>Insert/Standard name/Low Level Programming/Unchecked_Conversion</Title>
     </contextual>

     <action name="insert Text_IO_Text_Streams" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Text_Streams()</shell>
     </action>
     <contextual action="insert Text_IO_Text_Streams" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Text_Streams</Title>
     </contextual>

     <action name="insert Text_IO_Modular_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Modular_IO()</shell>
     </action>
     <contextual action="insert Text_IO_Modular_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Modular_IO</Title>
     </contextual>

     <action name="insert Text_IO_Integer_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Integer_IO()</shell>
     </action>
     <contextual action="insert Text_IO_Integer_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Integer_IO</Title>
     </contextual>

     <action name="insert Text_IO_Float_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Float_IO()</shell>
     </action>
     <contextual action="insert Text_IO_Float_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Float_IO</Title>
     </contextual>

     <action name="insert Text_IO_Fixed_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Fixed_IO()</shell>
     </action>
     <contextual action="insert Text_IO_Fixed_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Fixed_IO</Title>
     </contextual>

     <action name="insert Text_IO_Enumeration_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Enumeration_IO()</shell>
     </action>
     <contextual action="insert Text_IO_Enumeration_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Enumeration_IO</Title>
     </contextual>

     <action name="insert Text_IO_Editing" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Editing()</shell>
     </action>
     <contextual action="insert Text_IO_Editing" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Editing</Title>
     </contextual>

     <action name="insert Text_IO_Complex_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO_Complex_IO()</shell>
     </action>
     <contextual action="insert Text_IO_Complex_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO.Complex_IO</Title>
     </contextual>

     <action name="insert Text_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Text_IO()</shell>
     </action>
     <contextual action="insert Text_IO" >
        <Title>Insert/Standard name/Input+Output/Text_IO</Title>
     </contextual>

     <action name="insert Task_Identification" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Identification()</shell>
     </action>
     <contextual action="insert Task_Identification" >
        <Title>Insert/Standard name/Tasking/Task_Identification</Title>
     </contextual>

     <action name="insert Task_Attributes" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Task_Attributes()</shell>
     </action>
     <contextual action="insert Task_Attributes" >
        <Title>Insert/Standard name/Tasking/Task_Attributes</Title>
     </contextual>

     <action name="insert Tags" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Tags()</shell>
     </action>
     <contextual action="insert Tags" >
        <Title>Insert/Standard name/Misc/Tags</Title>
     </contextual>

     <action name="insert Synchronous_Task_Control" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Synchronous_Task_Control()</shell>
     </action>
     <contextual action="insert Synchronous_Task_Control" >
        <Title>Insert/Standard name/Tasking/Synchronous_Task_Control</Title>
     </contextual>

     <action name="insert Strings_Wide_Unbounded" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Wide_Unbounded()</shell>
     </action>
     <contextual action="insert Strings_Wide_Unbounded" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Wide_Unbounded</Title>
     </contextual>

     <action name="insert Strings_Wide_Maps_Wide_Constants" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Wide_Maps_Wide_Constants()</shell>
     </action>
     <contextual action="insert Strings_Wide_Maps_Wide_Constants" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Wide_Maps_Wide_Constants</Title>
     </contextual>

     <action name="insert Strings_Wide_Maps" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Wide_Maps()</shell>
     </action>
     <contextual action="insert Strings_Wide_Maps" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Wide_Maps</Title>
     </contextual>

     <action name="insert Strings_Wide_Fixed" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Wide_Fixed()</shell>
     </action>
     <contextual action="insert Strings_Wide_Fixed" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Wide_Fixed</Title>
     </contextual>

     <action name="insert Strings_Wide_Bounded" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Wide_Bounded()</shell>
     </action>
     <contextual action="insert Strings_Wide_Bounded" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Wide_Bounded</Title>
     </contextual>

     <action name="insert Strings_Unbounded" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Unbounded()</shell>
     </action>
     <contextual action="insert Strings_Unbounded" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Unbounded</Title>
     </contextual>

     <action name="insert Strings_Maps_Constants" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Maps_Constants()</shell>
     </action>
     <contextual action="insert Strings_Maps_Constants" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Maps_Constants</Title>
     </contextual>

     <action name="insert Strings_Maps" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Maps()</shell>
     </action>
     <contextual action="insert Strings_Maps" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Maps</Title>
     </contextual>

     <action name="insert Strings_Fixed" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Fixed()</shell>
     </action>
     <contextual action="insert Strings_Fixed" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Fixed</Title>
     </contextual>

     <action name="insert Strings_Bounded" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings_Bounded()</shell>
     </action>
     <contextual action="insert Strings_Bounded" >
        <Title>Insert/Standard name/Char+String Handling/Strings.Bounded</Title>
     </contextual>

     <action name="insert Strings" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Strings()</shell>
     </action>
     <contextual action="insert Strings" >
        <Title>Insert/Standard name/Char+String Handling/Strings</Title>
     </contextual>

     <action name="insert Streams_Stream_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Streams_Stream_IO()</shell>
     </action>
     <contextual action="insert Streams_Stream_IO" >
        <Title>Insert/Standard name/Input+Output/Streams.Stream_IO</Title>
     </contextual>

     <action name="insert Storage_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Storage_IO()</shell>
     </action>
     <contextual action="insert Storage_IO" >
        <Title>Insert/Standard name/Low Level Programming/Storage_IO</Title>
     </contextual>

     <action name="insert Sequential_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Sequential_IO()</shell>
     </action>
     <contextual action="insert Sequential_IO" >
        <Title>Insert/Standard name/Input+Output/Sequential_IO</Title>
     </contextual>

     <action name="insert Real_Time" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Real_Time()</shell>
     </action>
     <contextual action="insert Real_Time" >
        <Title>Insert/Standard name/Tasking/Real_Time</Title>
     </contextual>

     <action name="insert Generic_Elementary_Functions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Generic_Elementary_Functions()</shell>
     </action>
     <contextual action="insert Generic_Elementary_Functions" >
        <Title>Insert/Standard name/Numerics/Generic_Elementary_Functions</Title>
     </contextual>

     <action name="insert Generic_Complex_Types" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Generic_Complex_Types()</shell>
     </action>
     <contextual action="insert Generic_Complex_Types" >
        <Title>Insert/Standard name/Numerics/Generic_Complex_Types</Title>
     </contextual>

     <action name="insert Generic_Complex_Elementary_Functions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Generic_Complex_Elementary_Functions()</shell>
     </action>
     <contextual action="insert Generic_Complex_Elementary_Functions" >
        <Title>Insert/Standard name/Numerics/Generic_Complex_Elementary_Functions</Title>
     </contextual>

     <action name="insert Float_Random" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Float_Random()</shell>
     </action>
     <contextual action="insert Float_Random" >
        <Title>Insert/Standard name/Numerics/Float_Random</Title>
     </contextual>

     <action name="insert Elementary_Functions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Elementary_Functions()</shell>
     </action>
     <contextual action="insert Elementary_Functions" >
        <Title>Insert/Standard name/Numerics/Elementary_Functions</Title>
     </contextual>

     <action name="insert Discrete_Random" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Discrete_Random()</shell>
     </action>
     <contextual action="insert Discrete_Random" >
        <Title>Insert/Standard name/Numerics/Discrete_Random</Title>
     </contextual>

     <action name="insert Complex_Types" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Complex_Types()</shell>
     </action>
     <contextual action="insert Complex_Types" >
        <Title>Insert/Standard name/Numerics/Complex_Types</Title>
     </contextual>

     <action name="insert Complex_Elementary_Functions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Complex_Elementary_Functions()</shell>
     </action>
     <contextual action="insert Complex_Elementary_Functions" >
        <Title>Insert/Standard name/Numerics/Complex_Elementary_Functions</Title>
     </contextual>

     <action name="insert Numerics" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Numerics()</shell>
     </action>
     <contextual action="insert Numerics" >
        <Title>Insert/Standard name/Numerics/Numerics</Title>
     </contextual>

     <action name="insert Interrupts_Names" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupts_Names()</shell>
     </action>
     <contextual action="insert Interrupts_Names" >
        <Title>Insert/Standard name/Low Level Programming/Interrupts.Names</Title>
     </contextual>

     <action name="insert Interrupts" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Interrupts()</shell>
     </action>
     <contextual action="insert Interrupts" >
        <Title>Insert/Standard name/Low Level Programming/Interrupts</Title>
     </contextual>

     <action name="insert Finalization_Limited_Controlled" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Finalization_Limited_Controlled()</shell>
     </action>
     <contextual action="insert Finalization_Limited_Controlled" >
        <Title>Insert/Standard name/Initialization+Finalization/Finalization_Limited_Controlled</Title>
     </contextual>

     <action name="insert Finalization_Controlled" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Finalization_Controlled()</shell>
     </action>
     <contextual action="insert Finalization_Controlled" >
        <Title>Insert/Standard name/Initialization+Finalization/Finalization_Controlled</Title>
     </contextual>

     <action name="insert Finalization" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Finalization()</shell>
     </action>
     <contextual action="insert Finalization" >
        <Title>Insert/Standard name/Initialization+Finalization/Finalization</Title>
     </contextual>

     <action name="insert Exceptions" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Exceptions()</shell>
     </action>
     <contextual action="insert Exceptions" >
        <Title>Insert/Standard name/Misc/Exceptions</Title>
     </contextual>

     <action name="insert Dynamic_Priorities" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Dynamic_Priorities()</shell>
     </action>
     <contextual action="insert Dynamic_Priorities" >
        <Title>Insert/Standard name/Tasking/Dynamic_Priorities</Title>
     </contextual>

     <action name="insert Direct_IO" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Direct_IO()</shell>
     </action>
     <contextual action="insert Direct_IO" >
        <Title>Insert/Standard name/Input+Output/Direct_IO</Title>
     </contextual>

     <action name="insert Decimal" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Decimal()</shell>
     </action>
     <contextual action="insert Decimal" >
        <Title>Insert/Standard name/Input+Output/Decimal</Title>
     </contextual>

     <action name="insert Command_Line" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Command_Line()</shell>
     </action>
     <contextual action="insert Command_Line" >
        <Title>Insert/Standard name/Misc/Command_Line</Title>
     </contextual>

     <action name="insert Characters_Latin_1" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Characters_Latin_1()</shell>
     </action>
     <contextual action="insert Characters_Latin_1" >
        <Title>Insert/Standard name/Char+String Handling/Characters.Latin_1</Title>
     </contextual>

     <action name="insert Characters_Handling" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Characters_Handling()</shell>
     </action>
     <contextual action="insert Characters_Handling" >
        <Title>Insert/Standard name/Char+String Handling/Characters.Handling</Title>
     </contextual>

     <action name="insert Characters" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Characters()</shell>
     </action>
     <contextual action="insert Characters" >
        <Title>Insert/Standard name/Char+String Handling/Characters</Title>
     </contextual>

     <action name="insert Calendar" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Calendar()</shell>
     </action>
     <contextual action="insert Calendar" >
        <Title>Insert/Standard name/Misc/Calendar</Title>
     </contextual>

     <action name="insert Asynchronous_Task_Control" output="none">
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">predef_ada_entity_insertions.insert_Asynchronous_Task_Control()</shell>
     </action>
     <contextual action="insert Asynchronous_Task_Control" >
        <Title>Insert/Standard name/Tasking/Asynchronous_Task_Control</Title>
     </contextual>
""")


