------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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
------------------------------------------------------------------------------

--  This package describes how to graphically edit switches. It does not
--  perform any actual gtk+ operation, so that it can be shared among multiple
--  GUI backends as much as possible.

with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Command_Lines;
with GNAT.Strings;      use GNAT.Strings;
private with Shared_Macros;

package Switches_Chooser is

   type Switches_Editor_Config_Record (<>) is tagged private;
   type Switches_Editor_Config is
      access all Switches_Editor_Config_Record'Class;

   type Popup_Index is private;
   Main_Window : constant Popup_Index;

   type Switch_Type is
     (Switch_Check,
      Switch_Field,
      Switch_Spin,
      Switch_Radio,
      Switch_Combo,
      Switch_Popup);

   function Create
     (Default_Separator : String;
      Switch_Char       : Character := '-';
      Scrolled_Window   : Boolean := False;
      Show_Command_Line : Boolean := True;
      Sections          : String := "") return Switches_Editor_Config;
   --  A switches editor can be split into several lines and columns. Each cell
   --  act as a group for some switches, to help make the interface clearer for
   --  the user.
   --  Default_Separator is the string that goes between a switch and its
   --  attribute.
   --  If Scrolled_Window is true, the editor will be contained in a
   --  scrolling window, which is useful if the number of switches is
   --  especially important.
   --  Sections contains the list of sections defined for the tool's command
   --  line.
   --   for example: "cargs bargs largs"

   procedure Set_Size
     (Self      : not null access Switches_Editor_Config_Record'Class;
      Lines     : Integer;
      Columns   : Integer;
      For_Popup : Popup_Index := Main_Window);
   --  Set the size of teh config

   procedure Free (Config : in out Switches_Editor_Config);
   --  Free the memory associated with Config.
   --  Does nothing for now, but at least ensures that appropriate calls are
   --  done where needed.

   procedure Define_Prefix
     (Config : Switches_Editor_Config;
      Prefix : String);
   procedure Define_Alias
     (Config   : Switches_Editor_Config;
      Switch   : String;
      Expanded : String);
   --  Define_Prefix and Define_Alias are wrappers to actual calls performed
   --  on Config.Config. See Gnat.Command_Line for documentation.

   procedure Set_Frame_Title
     (Config    : Switches_Editor_Config;
      Title     : String;
      Line      : Positive := 1;
      Column    : Positive := 1;
      Line_Span : Natural := 1;
      Col_Span  : Natural := 1;
      Popup     : Popup_Index := Main_Window);
   --  Specify the title for a group of switches within the editor. It also
   --  defines how big the group is, since a cell can be merged with one or
   --  more of its neighbors through the *_Span parameters.

   procedure Add_Check
     (Config        : Switches_Editor_Config;
      Label         : String;
      Switch_Set    : String;
      Switch_Unset  : String;
      Default_State : Boolean;
      Active        : Boolean;
      Section       : String := "";
      Tip           : String := "";
      Line          : Positive := 1;
      Column        : Positive := 1;
      Add_Before    : Boolean := False;
      Popup         : Popup_Index := Main_Window;
      Filter        : String := "");
   --  Adds a check button in a specific area of the editor.
   --  When the button is different from its default state, then the switch
   --  corresponding to the activation state (set or unset) is present,
   --  otherwise it is omitted

   procedure Add_Field
     (Config       : Switches_Editor_Config;
      Label        : String;
      Switch       : String;
      Separator    : String := ""; --  no separator
      Section      : String := "";
      Tip          : String := "";
      As_Directory : Boolean := False;
      As_File      : Boolean := False;
      Line         : Positive := 1;
      Column       : Positive := 1;
      Add_Before   : Boolean := False;
      Popup        : Popup_Index := Main_Window;
      Filter       : String := "");
   --  Add a text field

   procedure Add_Spin
     (Config     : Switches_Editor_Config;
      Label      : String;
      Switch     : String;
      Separator  : String := ""; --  no separator
      Min        : Integer;
      Max        : Integer;
      Default    : Integer;
      Section    : String := "";
      Tip        : String := "";
      Line       : Positive := 1;
      Column     : Positive := 1;
      Add_Before : Boolean := False;
      Popup      : Popup_Index := Main_Window;
      Filter     : String := "");
   --  Add a switch that takes a numeric argument

   type Radio_Switch is private;
   function Add_Radio
     (Config    : Switches_Editor_Config;
      Label     : String;
      Tip       : String;
      Line      : Positive := 1;
      Column    : Positive := 1;
      Popup     : Popup_Index := Main_Window) return Radio_Switch;
   procedure Add_Radio_Entry
     (Config     : Switches_Editor_Config;
      Radio      : Radio_Switch;
      Label      : String;
      Switch     : String;
      Section    : String := "";
      Tip        : String := "";
      Add_Before : Boolean := False;
      Filter     : String := "");
   --  Create a radio button: only one of these switches is active at any time.
   --  A radio_entry is in all ways similar to a check button.

   type Combo_Switch is record
      Label : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Combo_Switch_Array is array (Positive range <>) of Combo_Switch;

   package Combo_Switch_Vectors is new
     Ada.Containers.Vectors (Natural, Combo_Switch);

   function Get_Label (Value : Combo_Switch) return String;
   --  return the combo value text to display to the user

   function Get_Value (Value : Combo_Switch) return String;
   --  return the combo value text to use in the commandline

   procedure Add_Combo
     (Config     : Switches_Editor_Config;
      Label      : String;
      Switch     : String;
      Separator  : String := ""; --  no separator
      No_Switch  : String;
      No_Digit   : String;
      Entries    : Combo_Switch_Array;
      Section    : String := "";
      Tip        : String := "";
      Line       : Positive := 1;
      Column     : Positive := 1;
      Add_Before : Boolean := False;
      Popup      : Popup_Index := Main_Window;
      Filter     : String := "");
   --  Add a combo box.
   --  When selected, the switch inserted in the command line will be
   --  Switch & Separator & <value of current entry>
   --  If the current entry is the same as No_Digit, then only the  text of
   --  the switch attribute is put on the command line (this is used for
   --  instance to interpret "-O" as "-O1").
   --  If the current entry is the same as No_Switch, then nothing is put in
   --  the command line.

   function Add_Popup
     (Config        : Switches_Editor_Config;
      Label         : String;
      Line          : Positive := 1;
      Column        : Positive := 1;
      Popup         : Popup_Index := Main_Window) return Popup_Index;
   --  Adds a new button, which, when clicked, displays a popup window with
   --  additional switches. These additional switches can be set by passing
   --  the returned value as the Popup parameter to the subprograms in this
   --  package.
   --  (Lines, Columns) are the number of lines and columns in the popup.

   procedure Add_Hidden
     (Config    : Switches_Editor_Config;
      Switch    : String;
      Separator : String := "");
   --  Add switch which is not visible for user but handled by switch editor.

   procedure Add_Dependency
     (Config         : Switches_Editor_Config;
      Switch         : String;
      Section        : String;
      Status         : Boolean;
      Slave_Tool     : String;
      Slave_Switch   : String;
      Slave_Section  : String;
      Slave_Activate : Boolean := True);
   --  Add dependency between two switches: if Switch's status becomes
   --  Status, then Slave_Switch will be automatically set to a new
   --  state (Activate), and set insensitive until Switch is set
   --  insensitive again.
   --  For instance: if Switch is "-g" for the builder, and Slave_Switch
   --  is "-g" for the compiler, with Status=True and
   --  Slave_Activate=True, then everytime the user selects "-g" for the
   --  builder, "-g" will also be forced for the compiler.

   procedure Add_Default_Value_Dependency
     (Config         : Switches_Editor_Config;
      Switch         : String;
      Section        : String;
      Slave_Switch   : String;
      Slave_Section  : String;
      Slave_Status   : Boolean := True);
   --  Add dependency between two switches: if Switch's status becomes
   --  Status, then Slave_Switch will have its default value set to
   --  Slave_Status.
   --  For instance: if Switch is "-gnatwa" for the compiler, and Slave_Switch
   --  is "-gnatwc", with Status=True and
   --  Slave_Default=True, then everytime the user selects "-gnatwa" for the
   --  builder, "-gnatwc" will be defaulted to True. Unselecting it will
   --  then add its Switch_Unset value: "-gnatwC"

   generic
      type Root_Widget_Record is tagged private;
      --  The general type used for widget in the graphical toolkit

      type Root_Editor is new Root_Widget_Record with private;
      --  So that your editor itself is a widget
   package Switches_Editors is
      type Root_Switches_Editor is abstract new Root_Editor with private;
      type Root_Switches_Editor_Access is access all
        Root_Switches_Editor'Class;
      --  A graphical window representing a switches editor. This type is
      --  abstract because it isn't directly related to any GUI toolkit, and
      --  thus needs to be instanced. But it provides services to be used by
      --  the various GUI implementations. In a GUI widget, each switch is
      --  associated with a graphical widget, represented here as a
      --  Root_Widget

      procedure Set_Command_Line
        (Editor   : access Root_Switches_Editor;
         Cmd_Line : String);
      procedure Set_Command_Line
        (Editor         : access Root_Switches_Editor;
         Cmd_Line       : GNAT.Strings.String_List;
         Protect_Quotes : Boolean := True);
      --  Set the switches to display on the command line. This can be used to
      --  initialize the widget.
      --  If Protect_Quotes is True, protect the quotes when converting the
      --  command line to a string to be displayed in the entry.

      function "="
        (Editor : access Root_Switches_Editor;
         Args   : GNAT.Strings.String_List) return Boolean;
      --  Whether Editor's command line is exactly equivalent to Args.
      --  This properly ungroup arguments from Args, so that the expanded
      --  command lines are compared.

      function Get_Config
        (Editor : access Root_Switches_Editor)
         return Switches_Editor_Config;
      --  Return the switches configuration used for this editor

      function Get_Command_Line
        (Editor   : access Root_Switches_Editor;
         Expanded : Boolean) return GNAT.Strings.String_List_Access;
      --  Return the command line. Result value must be freed by the user

      ------------------------------
      --  The subprograms below are only useful when you are implementing a
      --  switches editor for a specific toolkit
      ------------------------------

      procedure Initialize
        (Editor    : in out Root_Switches_Editor;
         Config    : Switches_Editor_Config);
      --  Initialize the editor

      procedure Set_Widget
        (Editor       : in out Root_Switches_Editor;
         Switch_Index : Integer;
         Widget       : access Root_Widget_Record'Class);
      --  Set the widget used for the Switch_Index-th switch as described in
      --  the editors's configuration

      procedure Change_Switch
        (Editor    : in out Root_Switches_Editor;
         Widget    : access Root_Widget_Record'Class;
         Parameter : String);
      --  Widget was changed interactively by the user, and therefore its
      --  switch needs to be updated on the command line. Parameter indicates
      --  the parameter to be set for the switch, and is interpreted
      --  differently depending on the type of the switch.
      --  For a check button, it should be either "TRUE" or "FALSE" to indicate
      --  whether the switch should be on the command line.

      function Get_Tool_By_Name
        (Editor          : Root_Switches_Editor;
         Dummy_Tool_Name : String)
         return Root_Switches_Editor_Access is (null);
      --  Return the editor for the switches of Tool. By default, this returns
      --  null. When the editor is found, it is possible that changing some
      --  switches in Editor will also impact switches from the returned editor
      --  depending on how the dependencies were set up.

      procedure On_Command_Line_Changed
        (Editor    : in out Root_Switches_Editor;
         Cmd_Line  : String);
      procedure On_Command_Line_Changed
        (Editor    : in out Root_Switches_Editor'Class);
      --  The command line widget was typed in by the user, and we need to
      --  reflect the new list of switches on the widgets.
      --  The second version is only used to refresh the widgets, from an
      --  already parsed command line.

      procedure Update_Graphical_Command_Line
        (Editor    : in out Root_Switches_Editor);
      --  Recompute what should be displayed in the command line widget

      procedure Set_Graphical_Command_Line
        (Editor    : in out Root_Switches_Editor;
         Cmd_Line  : String) is abstract;
      --  Show Cmd_Line in the graphical widget showing the current command
      --  line. No update of the other widgets should take place

      procedure Set_Graphical_Widget
        (Editor     : in out Root_Switches_Editor;
         Widget     : access Root_Widget_Record'Class;
         Switch     : Switch_Type;
         Parameter  : String;
         Is_Default : Boolean := False) is abstract;
      --  Change Widget so that it shows the value of Parameter. The exact
      --  meaning of Parameter depends on the type of Switch
      --  Is_Default tells if the parameter corresponds to a default value.

   private
      type Root_Widget is access all Root_Widget_Record'Class;
      type Widget_Array is array (Natural range <>) of Root_Widget;
      type Widget_Array_Access is access Widget_Array;

      type Root_Switches_Editor is abstract new Root_Editor with record
         Config     : Switches_Editor_Config;
         Cmd_Line   : Command_Lines.Command_Line;
         Widgets    : Widget_Array_Access;
         Block      : Boolean := False;
      end record;
   end Switches_Editors;

   type Switch_Description (<>) is tagged private;

   function Get_Switches_Element
     (Switches : Switches_Editor_Config;
      Index : Natural)
      return Switch_Description;
   --  Switches_Editor_Config object Switches field Element at Index getter

   function Get_Switch
     (Switch : Switch_Description) return String;
   --  Switch_Description object Switch field getter

   function Get_Label
     (Switch : Switch_Description) return String;
   --  Switch_Description object Label field getter

   function Get_Tip
     (Switch : Switch_Description) return String;
   --  Switch_Description object Tip field getter

   function Get_Section
     (Switch : Switch_Description) return String;
   --  Switch_Description object Section field getter

   function Is_Add_First
     (Switch : Switch_Description) return Boolean;
   --  Switch_Description object Add_First field getter

   function Get_Line
     (Switch : Switch_Description) return Positive;
   --  Switch_Description object Line field getter

   function Get_Column
     (Switch : Switch_Description) return Positive;
   --  Switch_Description object Column field getter

   function Get_Separator
     (Switch : Switch_Description) return Character;
   --  Switch_Description object Separator field getter

   function Get_Type
     (Switch : Switch_Description) return Switch_Type;
   --  Switch_Description object Switches_Element field getter

   function Get_Switch_Unset
     (Switch : Switch_Description) return String;
   --  Switch_Description object Switch_Unset field getter (Type=Switch_Check)

   function Get_Default_State
     (Switch : Switch_Description) return Boolean;
   --  Switch_Description object Default_State field getter (Type=Switch_Check)

   function Get_Initial_State
     (Switch : Switch_Description) return Boolean;
   --  Switch_Description object Initial_State field getter (Type=Switch_Check)

   function Is_Field_As_Directory
     (Switch : Switch_Description) return Boolean;
   --  Switch_Description object Field_As_Directory field getter (Switch_Field)

   function Is_Field_As_File
     (Switch : Switch_Description) return Boolean;
   --  Switch_Description object Field_As_File field getter (Switch_Field)

   function Get_Spin_Min
     (Switch : Switch_Description) return Integer;
   --  Switch_Description object Spin_Min field getter

   function Get_Spin_Max
     (Switch : Switch_Description) return Integer;
   --  Switch_Description object Spin_Max field getter

   function Get_Spin_Default
     (Switch : Switch_Description) return Integer;
   --  Switch_Description object Spin_Default field getter

   function Get_Combo_No_Switch
     (Switch : Switch_Description) return String;
   --  Switch_Description object Combo_No_Switch field getter

   function Get_Combo_No_Digit
     (Switch : Switch_Description) return String;
   --  Switch_Description object Combo_No_Digit field getter

   function Get_Combo_Entries
     (Switch : Switch_Description) return Combo_Switch_Vectors.Vector;
   --  Switch_Description object Combo_Entries field getter

   type Frame_Description is tagged private;

   function Get_Frames_Element
     (Switches : Switches_Editor_Config;
      Index : Natural)
      return Frame_Description;
   --  Switches_Editor_Config object Frames field Element at index getter

   function Get_Title
     (Frame : Frame_Description) return String;
   --  Frame_Desription object Title field getter

   function Get_Line
     (Frame : Frame_Description) return Positive;
   --  Frame_Desription object Line field getter

   function Get_Column
     (Frame : Frame_Description) return Positive;
   --  Frame_Desription object Column field getter

   function Get_Lines
     (Switches : Switches_Editor_Config)
      return Positive;
   --  Switches_Editor_Config object Lines field getter

   function Get_Columns
     (Switches : Switches_Editor_Config)
      return Positive;
   --  Switches_Editor_Config object Columns field getter

   function Is_Show_Command_Line
     (Switches : Switches_Editor_Config)
      return Boolean;
   --  Switches_Editor_Config object Show_Command_Line field getter

   function Get_Default_Separator
     (Switches : Switches_Editor_Config)
      return String;
   --  Switches_Editor_Config object Default_Separator field getter

   function Get_Sections
     (Switches : Switches_Editor_Config)
      return String;
   --  Switches_Editor_Config object Sections field getter

   function Is_Scrolled_Window
     (Switches : Switches_Editor_Config)
      return Boolean;
   --  Switches_Editor_Config object Scrolled_Window field getter

   function Get_Switch_Char
     (Switches : Switches_Editor_Config)
      return Character;
   --  Switches_Editor_Config object Switch_Char field getter

   function Get_Frames_Length
     (Switches : Switches_Editor_Config)
      return Ada.Containers.Count_Type;
   --  Switches_Editor_Config object Frames vector length getter

   function Get_Switches_Length
     (Switches : Switches_Editor_Config)
     return Ada.Containers.Count_Type;
   --  Switches_Editor_Config object Switches vector length getter

   function Empty_Command_Line
     (Switches : access Switches_Editor_Config_Record'Class)
     return Command_Lines.Command_Line;
   --  Return empty command line configured using current settings of
   --  given Switches_Editor_Config

   Command_Line_Editor_Tooltip_Text : aliased constant String;
   --  builder target command line field editor tool tip text

   type Switch_Filter_Description_Record is tagged private;
   type Switch_Filter_Description is
     access all Switch_Filter_Description_Record'Class;

   function Get_Name
     (Filter : not null access Switch_Filter_Description_Record) return String;
   --  Return the name of the filter

   function Get_Switch
     (Config : not null access Switches_Editor_Config_Record'Class;
      Filter : not null Switch_Filter_Description) return Switch_Description;
   --  Return the switch associated to the given filter

   procedure Apply
     (Config       : not null access Switches_Editor_Config_Record'Class;
      Filter       : not null Switch_Filter_Description;
      Matches      : Boolean;
      Before_Save  : Boolean;
      Default_Line : GNAT.Strings.String_List;
      Command_Line : in out GNAT.Strings.String_List_Access);
   --  Enable or disable the switch associated to Filter depending on Matches.
   --  Modify Command_Line according to Matches, Before_Save and Default_Line.
   --  Default_Line is default value of command line.
   --  If Before_Save is True, the non-matched switch is put back on
   --  the command line if it is present in the default command line:
   --  this is done in order to save only the modifications that
   --  have been explicitly set by the user.

   type Switch_Filter_Cursor is private;
   function First
     (Config : Switches_Editor_Config) return Switch_Filter_Cursor;
   procedure Next (Cursor : in out Switch_Filter_Cursor);
   function Has_Element
     (Cursor : Switch_Filter_Cursor) return Boolean;
   function Element
     (Cursor : Switch_Filter_Cursor) return Switch_Filter_Description;
   --  Standard iteration support for switch filters

private
   LF : constant Character := ASCII.LF;
   Command_Line_Editor_Tooltip_Text : aliased constant String :=
        "%subdir subdirectory of obj dir where object files are stored" & LF
      & "%subdirsarg  switch --subdirs=..." & LF
      & "%X      switches -Xname=value for scenario variables" & LF
      & "%vars   list of name=value for scenario variables" & LF
      & "%vars(-D) switches -Dname=value for scenario variables" & LF
      & "%eL     switch -eL, if fast project loading is set" & LF

      & LF & "Project attributes" & LF
      & "%attr(pkg'name[,default])     value of a specific attribute" & LF
      & "%dirattr(pkg'name[,default])  dirname of a specific attribute" & LF
      & "%baseattr(pkg'name[,default]) base name of a specific attribute" & LF
      & "%switches(tool)  value for attribute Ide'default_switches(tool)" & LF

      & LF & "Compilation (when building, only)" & LF
      & "%builder  default language builder (gnatmake or gprbuild)" & LF
      & "%gprbuild gprbuild command" & LF
      & "%gnatmake gnatmake command" & LF
      & "%gprclean clean command (gprclean or gnat clean)" & LF
      & "%external  the ""execute command"" preference" & LF
      & "[exec_dir] exec dir from the project" & LF
      & "%fp        base name of the file to compile" & LF
      & "%TT        absolute path of main source to compile" & LF
      & "%TP        absolute path of main source's project" & LF
      & "%python(cmd) a python command that returns one or more strings" & LF
      & "%T         base name of main source to compile" & LF
      & "%E         full path to the executable name corresponding"
      & " to the target" & LF
      & "%config    configuration for builder" & LF
      & "%autoconf  autogenerated configuration for builder" & LF
      & LF & Shared_Macros.Doc;

   type Radio_Switch is new Integer;
   type Popup_Index is new Integer;
   Main_Window : constant Popup_Index := 0;

   type Default_Value_Dependency_Record;
   type Default_Value_Dependency is access all Default_Value_Dependency_Record;

   type Switch_Description (Typ : Switch_Type) is tagged record
      Switch    : Ada.Strings.Unbounded.Unbounded_String;
      Label     : Ada.Strings.Unbounded.Unbounded_String;
      Tip       : Ada.Strings.Unbounded.Unbounded_String;
      Section   : Ada.Strings.Unbounded.Unbounded_String;
      Add_First : Boolean;
      Line      : Positive := 1;
      Column    : Positive := 1;
      Separator : Character;
      Popup     : Popup_Index := Main_Window;

      Active    : Boolean := True;
      --  Boolean indicating if the switch is currently valid. A non-valid
      --  switch should not be editable by the user: it's the switches editor
      --  GUI's responsability to implement this behavior.

      case Typ is
         when Switch_Check =>
            Switch_Unset  : Ada.Strings.Unbounded.Unbounded_String;
            Default_State : Boolean;
            Initial_State : Boolean;
            Dependencies  : Default_Value_Dependency;
         when Switch_Field =>
            As_Directory : Boolean;
            As_File      : Boolean;
         when Switch_Spin =>
            Min, Max, Default : Integer;
         when Switch_Radio =>
            Is_Entry : Boolean;
            Group    : Radio_Switch;
         when Switch_Combo =>
            No_Switch : Ada.Strings.Unbounded.Unbounded_String;
            No_Digit  : Ada.Strings.Unbounded.Unbounded_String;
            Entries   : Combo_Switch_Vectors.Vector;
         when Switch_Popup =>
            To_Popup  : Popup_Index;
            Lines     : Positive;
            Columns   : Positive;
      end case;
   end record;

   package Switch_Description_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Switch_Description);

   type Default_Value_Dependency_Record is record
      Enable        : Boolean;
      Master_Switch : Switch_Description_Vectors.Extended_Index;
      Master_State  : Boolean;
      Next          : Default_Value_Dependency;
   end record;

   type Frame_Description is tagged record
      Title     : Ada.Strings.Unbounded.Unbounded_String;
      Line      : Positive;
      Column    : Positive;
      Line_Span : Natural := 1;
      Col_Span  : Natural := 1;
      Popup     : Popup_Index := Main_Window;
   end record;
   package Frame_Description_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Frame_Description);

   type Dependency_Description;
   type Dependency_Description_Access is access Dependency_Description;
   type Dependency_Description is record
      Slave_Tool                    : GNAT.Strings.String_Access;
      Master_Switch, Slave_Switch   : GNAT.Strings.String_Access;
      Master_Section, Slave_Section : GNAT.Strings.String_Access;
      Master_Status, Slave_Status   : Boolean;
      Next                          : Dependency_Description_Access;
   end record;
   --  Description of a dependency (see Add_Dependency). This is needed because
   --  the dependencies can only be fully setup once all pages have been
   --  created.

   type Switch_Filter_Description_Record is tagged record
      Name         : Ada.Strings.Unbounded.Unbounded_String;
      Switch       : Switch_Description_Vectors.Extended_Index;
      Add_On_Match : Boolean;
   end record;
   package Switch_Filter_Description_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Switch_Filter_Description);
   --  Description for switch filters that can be defined via XML. The filter's
   --  name is used to identify and retrieve the filter to execute at a higher
   --  level.

   type Switch_Filter_Cursor is record
      C : Switch_Filter_Description_Vectors.Cursor;
   end record;

   type Switches_Editor_Config_Record is tagged record
      Lines             : Positive;
      Columns           : Positive;
      Config            : Command_Lines.Command_Line_Configuration;
      Show_Command_Line : Boolean := True;
      Default_Separator : Ada.Strings.Unbounded.Unbounded_String;
      Sections          : Ada.Strings.Unbounded.Unbounded_String;
      Scrolled_Window   : Boolean := False;
      Switch_Char       : Character;
      Frames            : Frame_Description_Vectors.Vector;
      Switches          : Switch_Description_Vectors.Vector;
      Filters           : Switch_Filter_Description_Vectors.Vector;
      Max_Radio         : Radio_Switch := 0;
      Max_Popup         : Popup_Index := Main_Window;
      Dependencies      : Dependency_Description_Access := null;
   end record;

end Switches_Chooser;
