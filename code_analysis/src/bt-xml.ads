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
   --  2 separate files in the bts subdirectory of the output directory
   --  backtraces (file_bts.xml)
   --  <file   name="test.adb" >
   --     <proc name="test.proc" >
   --        <VN  vn_id = "4" >
   --           <backtrace line="4" column="36" event="check">
   --              if event = check, list the check_kind
   --              <check kind="overflow" />
   --           </backtrace>
   --           <backtrace line="4" column="36" event="precondition">
   --              if event is a precondition_check, give enough info
   --              for the GUI to find the corresponding precondition in the
   --              called procedure
   --              <callee file="test.adb" proc="called_proc" vn_id="30" />
   --           </backtrace>
   --            ...
   --        </VN>
   --        ...  one per VN with corresponding backtraces
   --     </proc>
   --   </file>

   --  to make the xml more compact, we can have a table of check_kinds
   --  event_kind, and set representation.

   --  Value_Sets (file_vals.xml)
   --  <file   name="test.adb" >
   --     <line num="4" >
   --        <column num="34">
   --           <VN name="this_variable" set="{-Inf .. 0}" />
   --           <VN name="other_variable" set="{-10 .. 0}" />
   --           ...
   --        </column>
   --        ...
   --     </line>
   --     ...
   --  </file>

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

   function Xml_File_Name
     (Output_Dir     : String;
      File_Name      : String;
      For_Backtraces : Boolean) return String;
   --  Return the path to the xml file holding either the backtraces or
   --  the value_sets.

end BT.Xml;
