------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  parent package of the backtrace XML read/write packages

package BT.Xml is

   --  The backtrace xml files reside for now in a directory under the
   --  output directory and hold the results of the last inspection.

   --  This xml file, in combination with the historical database,
   --  should allow answers to the questions:
   --    for a given procedure:
   --      *preconditions, assumptions and check messages are identified in
   --       the historical database by a message_id and vn_id.
   --       for each of these we want to highlight the checks that contributed
   --       to the annotations' final values or message being emitted.

   --      *for each source location, we want to access the value_sets of all
   --       the active value_numbers at this point. The interesting
   --       value_numbers are the ones that represent an obj_id (closest to
   --       source variable)

   --  XML format
   --  There are two kinds of files in the bts subdirectory of the output
   --  directory:

   --  Backtraces (file_bts.xml)

   --  <all_infos>
   --  For xml compactness, we first have a table defining values for
   --  check_kinds and event_kinds, referenced by the backtrace info:
   --
   --    <checks>
   --      <check id="18" name="PRECONDITION_CHECK" text="precondition"/>
   --      <check id="19" name="POSTCONDITION_CHECK" text="postcondition"/>
   --      <check id="20" name="USER_PRECONDITION_CHECK"
   --                                            text="user precondition"/>
   --      <check id="21" name="INVALID_CHECK" text="validity check"/>
   --      ...
   --      <check id="39" name="TYPE_VARIANT_CHECK" text="discriminant check"/>
   --      <check id="40" name="TAG_CHECK" text="tag check"/>
   --    </checks>
   --    <events>
   --      <event id="0" name="POSTCONDITION_ASSUME_EVENT"
   --                                      text="postcond_assume" />
   --      <event id="1" name="INDUCTION_VAR_ASSUME_EVENT"
   --                                      text="induction_var" />
   --      <event id="2" name="OTHER_FROM_ASSUME_EVENT" text="other_assume" />
   --      <event id="3" name="NON_INVALID_INPUT_ASSUME_EVENT"
   --                                      text="non_invalid_input_values" />
   --      <event id="4" name="PRECOND_ASSUME_EVENT" text="precond_assume" />
   --      <event id="5" name="JUMP_EVENT" text="jump" />
   --      <event id="6" name="CHECK_EVENT" text="check" />
   --      <event id="7" name="PRECONDITION_EVENT" text="precondition check" />
   --    </events>
   --
   --  <file name="test.adb">
   --     <proc name="test.proc">
   --        <vn id = "4">
   --           <bt line="4" col="36" check="21" (Note: check # comes from
   --                                             the check_kinds table)
   --              if source file name for check message is not the same as
   --              this module's file name, then check's file name is present:
   --                 file_name="path-to-check-source-file"
   --           />
   --           <bt line="4" col="36" event="7"> (Note: event # comes from
   --                                             the event_kinds table)
   --              if event is a precondition_check, give enough info for the
   --              GUI to find the corresponding precondition in the called
   --              procedure:
   --              <callee file="test.adb" name="called_proc" vn="30"
   --               line="7" col="42" file_name="path-to-callee-source-file"/>
   --           </bt>
   --           ... more backtraces
   --        </vn>
   --        ... one per VN, with corresponding backtraces
   --     </proc>
   --     ... one per proc
   --  </file>
   --  </all_infos>

   --  Value_Sets (file_vals.xml)

   --  <all_infos>
   --  <file name="test.adb">
   --     <proc name="test.proc">
   --        <srcpos line="4" col="34">
   --           <vn name="this_variable" vals="{-Inf .. 0}" />
   --           <vn name="other_variable" vals="{-10 .. 0}" />
   --           ...
   --        </srcpos>
   --        ... one per line with VNs
   --     </proc>
   --     ... one per proc
   --  </file>
   --  </all_infos>

   --  Examples of queries:
   --     Given a precondition_message for a procedure,
   --             - find the vn_id in the historical database
   --             - with this vn_id, find all the backtrace events that
   --             contributed. Find source location of each event in the
   --             Locations table based on the event's scil_node

   --     Same for any error_message.

   --     Given a file/procedure/source_location,
   --             - find all the scil_nodes for this location
   --             - for each scil_node, find the associated VNs and their
   --             and their value_sets. Use the Value_Sets and VN_Images
   --             tables to produce a string of the form
   --             "VN_Image => Value_Set"
   --             Note that there can be multiple scil_nodes / VN/ Value_Sets
   --             for the same source location, and I am not sure how this
   --             can be represented in the GUI.

   Events_Tag    : constant String := "events";
   Event_Tag     : constant String := "event";
   Checks_Tag    : constant String := "checks";
   Check_Tag     : constant String := "check";
   File_Tag      : constant String := "file";
   Proc_Tag      : constant String := "proc";
   Vn_Tag        : constant String := "vn";
   Bt_Tag        : constant String := "bt";
   Callee_Tag    : constant String := "callee";
   All_Infos_Tag : constant String := "all_infos";
   Srcpos_Tag    : constant String := "srcpos";

   Name_Attribute       : constant String := "name";
   File_Name_Attribute  : constant String := "file_name";
   Id_Attribute         : constant String := "id";
   Vn_Attribute         : constant String := "vn";
   Pre_Index_Attribute  : constant String := "precond_index";
   Line_Attribute       : constant String := "line";
   Col_Attribute        : constant String := "col";
   Check_Attribute      : constant String := "check";
   Event_Attribute      : constant String := "event";
   Text_Attribute       : constant String := "text";
   Vals_Attribute       : constant String := "vals";

   function Xml_Directory
     (Output_Dir     : String) return String;
   --  Return the path to the directory where the backtrace and value_set
   --  xml files are stored.

   function Xml_BT_File_Name
     (Output_Dir     : String;
      File_Name      : String) return String;
   --  Return the path to the xml file holding the backtraces.

   function Xml_Vals_File_Name
     (Output_Dir     : String;
      File_Name      : String) return String;
   --  Return the path to the xml file holding the value sets.

end BT.Xml;
