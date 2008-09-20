-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles the data for managing builder configurations.
--  It introduces the notions of Target, Target_Model, and Target_Value
--
--   A target represents one build action. Targets can be, for instance:
--    - one target in a Makefile
--    - build of a main source in a project
--    - compile of one source file
--    - clean of a project
--    - etc
--
--  NOTE: this package is meant to remain independent of GPS in order to
--  facilitate factorizing code with other IDEs.

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with Switches_Chooser; use Switches_Chooser;

with Glib.Xml_Int;     use Glib.Xml_Int;

package Build_Configurations is

   type Build_Config_Registry is private;
   type Build_Config_Registry_Access is access Build_Config_Registry;
   --  A target registry contains a list of targets and current values for
   --  those targets. Each target in a target registry is identified by an
   --  unique name.

   type Target_Type is private;
   type Target_Access is access Target_Type;

   -------------------
   -- Target models --
   -------------------

   --  A target model is a description of the switches that can be given to
   --  a target.
   --  These target models are described in XML format and stored in the
   --  registry. Each target model is identified by a unique name.
   --
   --  A target is conceptually the combination of
   --      - one target model
   --      - one command line
   --  The command line can be modified by the user, and is stored in an
   --  history.

   type Target_Model_Type is private;
   type Target_Model_Access is access Target_Model_Type;

   procedure Create_Model_From_XML
     (Registry : Build_Config_Registry_Access;
      XML      : Node_Ptr);
   --  Create a target model from XML and add it to the registry
   --
   --  The XML format is the following:
   --
   --  <target-model name="NAME" category="CATEGORY">
   --          <icon>ICON</icon>
   --          <description>DESCRIPTION</description>
   --          <command-line>
   --             <arg>COMMAND</arg>
   --             <arg>ARG1</arg>
   --                 ...
   --             <arg>ARGN</arg>
   --          </command-line>
   --          <switches command="COMMAND_1"  ... </switches>
   --          <switches command="COMMAND_2"  ... </switches>
   --          ...
   --  </target-model>
   --
   --  Where
   --     NAME is the name of the target model
   --     CATEGORY is the model category
   --     DESCRIPTION is the description of the target model
   --     COMMAND_N are the various commands supported by the target_model
   --     DEFAULT_COMMAND_LINE is the initial command line
   --     ICON (optional) represents a stock icon to serve as default for
   --                     targets of this model
   --     COMMAND      (optional) is a string containing the default executable
   --     ARG1..ARGN   (optional) default arguments

   --------------------------
   -- Manipulating targets --
   --------------------------

   procedure Create_Target
     (Registry : Build_Config_Registry_Access;
      Name     : String;
      Category : String;
      Model    : String);
   --  Create a target from the given model and add it to the registry

   procedure Change_Model
     (Registry : Build_Config_Registry_Access;
      Target   : String;
      Model    : String);
   --  Change registered target Target to have model Model.
   --  The configuration line for this target is reset to the model defaults.

   procedure Remove_Target
     (Registry    : Build_Config_Registry_Access;
      Target_Name : String);
   --  Remove target named Target_Name from registry

   function Get_Target_From_Name
     (Registry : Build_Config_Registry_Access;
      Name     : String) return Target_Access;
   --  Get the target corresponding to Name in the Registry; return null if
   --  no such target was found.

   procedure Duplicate_Target
     (Registry     : Build_Config_Registry_Access;
      Src_Name     : String;
      New_Name     : String;
      New_Category : String);
   --  Utility function to create a duplicate of target named Src_Name.
   --  New_Name must be a name which does not correspond to an already defined
   --  target.

   ----------------
   -- Build Mode --
   ----------------

   type Build_Mode is private;
   type Build_Mode_Access is access Build_Mode;
   --  The Build Mode is a global setting that affects all builds.
   --  For example, the following Build Modes can be implemented:
   --     - "default" : the project decides of all compile switches
   --     - "debug"   : switches "-g -O0 --subdirs=debug" added to all builds
   --     - "gcov"    : same thing for gcov switches
   --  and so forth.

   procedure Create_Mode
     (Registry : Build_Config_Registry_Access;
      Name     : String;
      Switches : GNAT.OS_Lib.Argument_List);
   --  Create a new mode and register it

   ------------------------------------
   -- Accessing target-specific data --
   ------------------------------------

   function Get_Command_Line_Unexpanded
     (Registry : Build_Config_Registry_Access;
      Mode     : String;
      Target   : Target_Access)
      return GNAT.OS_Lib.Argument_List;
   --  Return the full command line associated with Target, with macros not
   --  expanded (in other words, the returned list may still contain "%f"
   --  where the final command should expand this to a file name)
   --  The first element in the returned list is the executable, followed by
   --  a list of arguments.
   --  Mode is the name of the mode with which to launch Target (the empty
   --  string can be passed to get the command line with no mode switches).
   --  Caller should NOT free the result.

   procedure Set_Command_Line
     (Registry     : Build_Config_Registry_Access;
      Target       : Target_Access;
      Command_Line : GNAT.OS_Lib.Argument_List);
   --  Set the current unexpanded command line of Target to Command_Line.
   --  Note that Command_Line should include the executable as well as the
   --  arguments

   function Get_Switch_Value
     (Target : Target_Access;
      Switch : String) return String;
   --  Return the value of Switch as configured in Target

   -----------------------
   -- XML import/export --
   -----------------------

   --  The XML format for targets is the following
   --
   --  <target name="TARGET_NAME" model="MODEL_NAME" category="CATEGORY">
   --     <icon>ICON</icon>
   --     <command-line>
   --           <arg>COMMAND</arg>
   --           <arg>ARG1</arg>
   --                ...
   --           <arg>ARGN</arg>
   --     </command-line>
   --  </target>
   --
   --  Where
   --     TARGET_NAME  is the name of the target
   --     CATEGORY     is the category of the target
   --     MODEL_NAME   is the name of the target model
   --     COMMAND      (optional) is a string containing the executable
   --     ARG1..ARGN   (optional) arguments
   --     ICON         (optional) is a stock identifier

   function Save_Target_To_XML
     (Registry : Build_Config_Registry_Access;
      Target   : Target_Access) return Node_Ptr;
   --  Write Target to XML format

   procedure Load_Target_From_XML
     (Registry : Build_Config_Registry_Access;
      XML      : Node_Ptr);
   --  Read a target from a XML node and load it in Registry.
   --  Note: this must be called only after all necessary target models have
   --  been loaded.

   function Save_All_Targets_To_XML
     (Registry : Build_Config_Registry_Access) return Node_Ptr;
   --  Save all targets to a node of the format
   --    <targets>
   --       <target ... (format described above)
   --       <target ...
   --    </targets>

   procedure Load_All_Targets_From_XML
     (Registry : Build_Config_Registry_Access;
      XML      : Node_Ptr);
   --  Load multiple targets from XML
   --  See Save_All_Targets_To_XML for the format XML should be in.

   -------------------------
   -- Creating a registry --
   -------------------------

   type Message_Mode is (Info, Error);
   type Logger_Type is access procedure (M : String; Mode : Message_Mode);

   function Create (Logger : Logger_Type) return Build_Config_Registry_Access;
   --  Create a new registry

private

   -- Packages --

   package Target_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Target_Access);

   package Build_Mode_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Build_Mode_Access);

   package Model_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Target_Model_Access);

   package Switches_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Switches_Editor_Config);

   -- Types --

   type Target_Model_Type is record
      Name : Unbounded_String;
      --  The name of a target model

      Category : Unbounded_String;
      --  The category of the model, used for purposes of displaying the
      --  models in a hierarchical fashion

      Description : Unbounded_String;
      --  A one-line description of the target model

      Icon     : Unbounded_String;
      --  The string contains a stock identifier

      Switches : Switches_Editor_Config;
      --  The configuration of switches to display in the target

      Default_Command_Line : GNAT.OS_Lib.Argument_List_Access;
      --  The command line to use when creating targets of this model
   end record;

   type Build_Config_Registry is record
      Models  : Model_Map.Map;
      --  Contains all registered models

      Targets : Target_Map.Map;
      --  Contains all registered targets

      Modes   : Build_Mode_Map.Map;
      --  Contains all registered modes

      Logger  : Logger_Type := null;
      --  A procedure to log messages
   end record;

   type Build_Mode is record
      Name     : Unbounded_String;
      --  The name of the mode

      Switches : GNAT.OS_Lib.Argument_List_Access;
      --  The additional switches appended at the end of the command
   end record;

   type Launch_Mode_Type is
     (Manually_With_Dialog,
      --  Target is build when requested by user, and display a dialog for
      --  allowing extra arguments whenever launched

      Manually,
      --  Target should be built when requested by user

      On_File_Save
      --  Target should be built when saving a source file
     );

   type Target_Properties is record
      Launch_Mode     : Launch_Mode_Type := Manually;
      Icon_In_Toolbar : Boolean          := False;
      --  Whether to display an icon in the toolbar

      Predefined      : Boolean          := False;
      --  When set to True, the target cannot be renamed or removed

      Key             : Unbounded_String;
      --  The key to which the target is bound
   end record;

   type Target_Type is record
      Name : Unbounded_String;
      --  The name of the Target

      Category : Unbounded_String;
      --  The category of the target, used for purposes of displaying the
      --  targets in a hierarchical fashion

      Model    : Target_Model_Access;
      --  The model of which the Target is an instance

      Icon     : Unbounded_String;
      --  The string contains a stock identifier

      Command_Line : GNAT.OS_Lib.Argument_List_Access;
      --  This stores the command line between launches of the graphical editor

      Properties : Target_Properties;
      --  The set of target properties
   end record;

   -- Private subprograms --

   procedure Log
     (Registry : Build_Config_Registry_Access;
      Message  : String;
      Mode     : Message_Mode := Error);
   --  Log Message

   procedure Free (Target : in out Target_Type);
   --  Free memory associated with Target

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Target_Type, Target_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Build_Config_Registry, Build_Config_Registry_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.OS_Lib.Argument_List, GNAT.OS_Lib.Argument_List_Access);

end Build_Configurations;
