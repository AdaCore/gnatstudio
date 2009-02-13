-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with Remote;           use Remote;
with Switches_Chooser; use Switches_Chooser;
with XML_Utils;        use XML_Utils;

package Build_Configurations is

   type Build_Config_Registry is private;
   type Build_Config_Registry_Access is access Build_Config_Registry;
   --  A target registry contains a list of targets and current values for
   --  those targets. Each target in a target registry is identified by an
   --  unique name.
   --  Contains also a list of build modes.

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
   --          <server>SERVER</server>
   --          <uses-shell>USES_SHELL</uses_shell>
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
   --     SERVER       (optional, default "Build_Server") the server used for
   --                     executing the action.
   --     USES_SHELL   (optional, default False) - whether the commands need
   --                     to be launched through $SHELL -c "command line"
   --     COMMAND_N are the various commands supported by the target_model
   --     DEFAULT_COMMAND_LINE is the initial command line
   --     ICON         (optional) represents a stock icon to serve as default
   --                     for targets of this model
   --     COMMAND      (optional) is a string containing the default executable
   --     ARG1..ARGN   (optional) default arguments

   function Is_Registered_Model
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean;
   --  Return True if name corresponds to a registered model

   --------------------------
   -- Manipulating targets --
   --------------------------

   procedure Create_Target
     (Registry     : Build_Config_Registry_Access;
      Name         : String;
      Category     : String;
      Model        : String;
      Command_Line : GNAT.OS_Lib.Argument_List := (1 .. 0 => null));
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

   -----------------
   -- Build Modes --
   -----------------

   type Model_Record is record
      Model  : Unbounded_String;
      Filter : Unbounded_String;
   end record;

   package Model_List is new Ada.Containers.Doubly_Linked_Lists
     (Model_Record);

   type Mode_Record is record
      Name        : Unbounded_String;
      Description : Unbounded_String;
      Models      : Model_List.List;
      Args        : GNAT.OS_Lib.Argument_List_Access;
      Subst_Src   : GNAT.OS_Lib.Argument_List_Access;
      Subst_Dest  : GNAT.OS_Lib.Argument_List_Access;
      Shadow      : Boolean := False;
      Active      : Boolean := False;
      Is_Server   : Boolean := False;
      Server      : Remote.Server_Type := Remote.GPS_Server;
      --  Relevant only for Shadow modes. Indicates whether the mode is active
      Subdir      : Unbounded_String;
   end record;

   package Mode_Map is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Mode_Record);

   function Element_Mode
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Mode_Record;
   --  Return the mode element from Registry corresponding to Name

   function Contains_Mode
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean;
   --  Return the mode element from Registry corresponding to Name

   function First_Mode
     (Registry : Build_Config_Registry_Access) return Mode_Map.Cursor;
   --  Return the first mode element from Registry

   function Number_Of_Modes
     (Registry : Build_Config_Registry_Access) return Natural;
   --  Return the number of mode from Registry

   procedure Insert_Mode
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String;
      Mode     : Mode_Record);
   --  Insert the given mode

   procedure Replace_Mode
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String;
      Mode     : Mode_Record);
   --  Replace the given mode (given by its name) by contents of Mode

   -----------------------
   -- Target properties --
   -----------------------

   type Launch_Mode_Type is
     (Manually,
      --  Target should be built when requested by user. Won't use a dialog for
      --  buttons, but will for menus.

      Manually_With_Dialog,
      --  Target is built when requested by user, and display a dialog for
      --  allowing extra arguments whenever launched.

      Manually_With_No_Dialog,
      --  Target should be built when requested by user, never using an
      --  extra dialog.

      On_File_Save
      --  Target should be built when saving a source file
     );

   type Target_Properties is record
      Launch_Mode     : Launch_Mode_Type := Manually;

      In_Toolbar : Boolean          := False;
      --  Whether to display an icon in the toolbar

      In_Menu    : Boolean          := True;
      --  Whether to display an item in the menu

      Read_Only       : Boolean          := False;
      --  When set to True, the target cannot be renamed or removed

      Server           : Server_Type := GPS_Server;
      --  Server used to launch the target. If equal to GPS_Server, then its
      --  model's server is used.

      Target_Type : Unbounded_String;
      --  If the target represents a series of mains/targets, hold the
      --  parameter to the compute_build_targets hook.

      Key             : Unbounded_String;
      --  The key to which the target is bound

      Icon     : Unbounded_String;
      --  The string contains a stock identifier

      Menu_Name : Unbounded_String;
      --  The name of the menu to display target

      Category : Unbounded_String;
      --  The category of the target, used for purposes of displaying the
      --  targets in a hierarchical fashion.
   end record;

   function Get_Properties (Target : Target_Access) return Target_Properties;
   --  Return the properties for Target

   ------------------------------------
   -- Accessing target-specific data --
   ------------------------------------

   function Get_Command_Line_Unexpanded
     (Registry : Build_Config_Registry_Access;
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

   function Get_Name (Target : Target_Access) return String;
   --  Return the name of Target

   function Get_Menu_Name (Target : Target_Access) return String;
   --  Return the menu name of Target, ie a name where an underscore indicates
   --  the position of the key shortcut.

   function Get_Category (Target : Target_Access) return String;
   --  Return the category of Target

   function Get_Icon (Target : Target_Access) return String;
   --  Return the stock-id corresponding to the icon for target

   function Get_Server (Target : Target_Access) return Server_Type;
   --  Return the server_type that will run the target

   function Uses_Shell (Target : Target_Access) return Boolean;
   --  Return True if the target uses the $SHELL -cmd "command" to build

   function Get_Model (Target : Target_Access) return String;
   --  Return the name of the model for Target

   -----------------------
   -- XML import/export --
   -----------------------

   --  The XML format for targets is the following
   --
   --  <target name="TARGET_NAME" model="MODEL_NAME" category="CATEGORY">
   --     <icon>ICON</icon>
   --     <in-toolbar>IN_TOOLBAR</in-toolbar>
   --     <in-menu>IN_MENU</in-menu>
   --     <read-only>RO</read-only>
   --     <represents-mains>RM</represents-mains>
   --     <key>KEY</key>
   --     <launch-mode>LAUNCH_MODE</launch-mode>
   --     <server>SERVER</server>
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
   --     CATEGORY     is the category of the targe
   --     RO           (boolean) indicates whether the target can be modified
   --     RM           (boolean) indicates whether the target represents in
   --                            fact a series of mains.
   --     LAUNCH_MODE  is the launch mode
   --     IN_TOOLBAR   (boolean) indicates whether the target should show up
   --                  in the toolbar - False by default
   --     IN_MENU      (boolean) indicates whether the target should show up
   --                  in the menu - True by default
   --     KEY          the key bound to the target
   --     SERVER       the server used to launch the target

   --     MODEL_NAME   is the name of the target model
   --     COMMAND      (optional) is a string containing the executable
   --     ARG1..ARGN   (optional) arguments
   --     ICON         (optional) is a stock identifier

   function Save_Target_To_XML
     (Registry : Build_Config_Registry_Access;
      Target   : Target_Access) return Node_Ptr;
   --  Write Target to XML format

   function Load_Target_From_XML
     (Registry  : Build_Config_Registry_Access;
      XML       : Node_Ptr;
      From_User : Boolean) return Target_Access;
   --  Read a target from a XML node and load it in Registry.
   --  Note: this must be called only after all necessary target models have
   --  been loaded.
   --  From_User indicates whether the target comes from the user configuration
   --  file.
   --      - if From_User is False, the target is created and copied
   --           to the original targets. If the target already exists, an
   --           error is raised
   --      - if From_User is True, then the existing target is updated with
   --           the properties and command line described in XML. The original
   --           targets list is not modified.
   --  Return the new Target, or null if the target could not be created.

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

   type Message_Mode is (Info, Error, Trace);
   --  Info is an information message for the user.
   --  Error is an error that should be reported to the user.
   --  Trace is a debugging trace.
   type Logger_Type is access procedure (M : String; Mode : Message_Mode);

   function Create (Logger : Logger_Type) return Build_Config_Registry_Access;
   --  Create a new registry

   procedure Free (Registry : in out Build_Config_Registry_Access);
   --  Free the memory used by the registry

   -----------------------------------
   -- Iterating through the targets --
   -----------------------------------

   type Target_Cursor is limited private;

   function Get_First_Target
     (Registry : Build_Config_Registry_Access) return Target_Cursor;
   --  Get a cursor to the first registered target

   function Get_Target (Cursor : Target_Cursor) return Target_Access;
   --  Return the target pointed to by cursor, or null if it doesn't exist

   procedure Next (Cursor : in out Target_Cursor);
   --  Iterate to the next target

   procedure Revert_Target
     (Registry : Build_Config_Registry_Access;
      Target   : String);
   --  Revert Target to its original

private

   -- Packages --

   package Target_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type    => Target_Access);

   function Contains
     (List : Target_List.List; Key : Unbounded_String) return Boolean;
   --  Return true if List contains a target with name Key

   type Target_Cursor is new Target_List.Cursor;

   package Model_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Target_Model_Access);

   package Switches_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Switches_Editor_Config);

   -- Types --

   type Target_Model_Type is record
      Name                 : Unbounded_String;
      --  The name of a target model

      Category             : Unbounded_String;
      --  The category of the model, used for purposes of displaying the
      --  models in a hierarchical fashion.

      Description          : Unbounded_String;
      --  A one-line description of the target model

      Icon                 : Unbounded_String;
      --  The string contains a stock identifier

      Switches             : Switches_Editor_Config;
      --  The configuration of switches to display in the target

      Default_Command_Line : GNAT.OS_Lib.Argument_List_Access;
      --  The command line to use when creating targets of this model

      Server               : Server_Type      := Build_Server;
      --  The server used for executing this target

      Uses_Shell           : Boolean := False;
      --  If True, the commands inheriting from this model need to be launched
      --  throuhg $SHELL -c "command line".
   end record;

   type Build_Config_Registry is record
      Models  : Model_Map.Map;
      --  Contains all registered models

      Targets : Target_List.List;
      --  Contains all registered targets

      Original_Targets : Target_List.List;
      --  Contains a copy of all targets as they were originally created

      Modes : Mode_Map.Map;
      --  The registered modes

      Logger  : Logger_Type := null;
      --  A procedure to log messages
   end record;

   type Target_Type is record
      Name         : Unbounded_String;
      --  The name of the Target. This is the unique name that identifies
      --  the Target: there is only one target for each Name in the Registry.

      Model        : Target_Model_Access;
      --  The model of which the Target is an instance

      Command_Line : GNAT.OS_Lib.Argument_List_Access;
      --  This stores the command line between launches of the graphical editor

      Properties   : Target_Properties;
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
     (Build_Config_Registry, Build_Config_Registry_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.OS_Lib.Argument_List, GNAT.OS_Lib.Argument_List_Access);

end Build_Configurations;
