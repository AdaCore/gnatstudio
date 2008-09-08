-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007-2008, AdaCore             --
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

--  This package describes how to graphically edit switches. It does not
--  perform any actual gtk+ operation, so that it can be shared among multiple
--  GUI backends as much as possible.

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GComLin;   use GComLin;
with GNAT.Strings; use GNAT.Strings;

package Switches_Chooser is

   type Switches_Editor_Config_Record (<>) is private;
   type Switches_Editor_Config is access all Switches_Editor_Config_Record;

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
      Lines             : Positive := 1;
      Columns           : Positive := 1;
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

   procedure Free (Config : in out Switches_Editor_Config) is null;
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
     (Config     : Switches_Editor_Config;
      Label      : String;
      Switch     : String;
      Section    : String := "";
      Tip        : String := "";
      Line       : Positive := 1;
      Column     : Positive := 1;
      Add_Before : Boolean := False;
      Popup      : Popup_Index := Main_Window);
   --  Adds a check button in a specific area of the editor.
   --  When the button is active, the corresponding command line switch is
   --  present, otherwise it is omitted.

   procedure Add_Check
     (Config        : Switches_Editor_Config;
      Label         : String;
      Switch_Set    : String;
      Switch_Unset  : String;
      Default_State : Boolean;
      Section       : String := "";
      Tip           : String := "";
      Line          : Positive := 1;
      Column        : Positive := 1;
      Add_Before    : Boolean := False;
      Popup         : Popup_Index := Main_Window);
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
      Popup        : Popup_Index := Main_Window);
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
      Popup      : Popup_Index := Main_Window);
   --  Add a switch that takes a numeric argument

   type Radio_Switch is private;
   function Add_Radio
     (Config    : Switches_Editor_Config;
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
      Add_Before : Boolean := False);
   --  Create a radio button: only one of these switches is active at any time.
   --  A radio_entry is in all ways similar to a check button.

   type Combo_Switch is record
      Label : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Combo_Switch_Array is array (Positive range <>) of Combo_Switch;

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
      Popup      : Popup_Index := Main_Window);
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
      Lines         : Positive := 1;
      Columns       : Positive := 1;
      Line          : Positive := 1;
      Column        : Positive := 1;
      Popup         : Popup_Index := Main_Window) return Popup_Index;
   --  Adds a new button, which, when clicked, displays a popup window with
   --  additional switches. These additional switches can be set by passing
   --  the returned value as the Popup parameter to the subprograms in this
   --  package.
   --  (Lines, Columns) are the number of lines and columns in the popup.

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
      Slave_Section  : String);
   --  Add dependency between two switches: if Switch's status becomes
   --  Status, then Slave_Switch will have its default value set to
   --  Slave_Default.
   --  For instance: if Switch is "-gnatwa" for the compiler, and Slave_Switch
   --  is "-gnatwc", with Status=True and
   --  Slave_Default=True, then everytime the user selects "-gnatwa" for the
   --  builder, "-gnatwc" will be defaulted to True. Unselecting it will
   --  then add its Switch_Unset value: "-gnatwC"

   procedure Get_Command_Line
     (Cmd      : in out Command_Line;
      Expanded : Boolean;
      Result   : out GNAT.Strings.String_List_Access);
   --  Return the arguments of the command line. Expanded indicates whether
   --  the expanded command line, or the shortest command line, is returned.

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
        (Editor   : access Root_Switches_Editor;
         Cmd_Line : GNAT.Strings.String_List);
      --  Set the switches to display on the command line. This can be used to
      --  initialize the widget

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
        (Editor : access Root_Switches_Editor)
         return Command_Line;
      --  Return the current command line

      function Get_Command_Line
        (Editor   : access Root_Switches_Editor;
         Expanded : Boolean) return GNAT.Strings.String_List_Access;
      --  Return the command line. Result value must be freed by the user

      ------------------------------
      --  The subprograms below are only useful when you are implementing a
      --  switches editor for a specific toolkit
      ------------------------------

      procedure Initialize
        (Editor : in out Root_Switches_Editor;
         Config : Switches_Editor_Config);
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
        (Editor : Root_Switches_Editor;
         Tool_Name : String) return Root_Switches_Editor_Access;
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
         Cmd_Line   : Command_Line;
         Widgets    : Widget_Array_Access;
         Block      : Boolean := False;
      end record;
   end Switches_Editors;

private
   type Radio_Switch is new Integer;
   type Popup_Index is new Integer;
   Main_Window : constant Popup_Index := 0;

   package Combo_Switch_Vectors is new
     Ada.Containers.Vectors (Natural, Combo_Switch);

   type Default_Value_Dependency_Record;
   type Default_Value_Dependency is access all Default_Value_Dependency_Record;

   type Switch_Description (Typ : Switch_Type) is record
      Switch    : Ada.Strings.Unbounded.Unbounded_String;
      Label     : Ada.Strings.Unbounded.Unbounded_String;
      Tip       : Ada.Strings.Unbounded.Unbounded_String;
      Section   : Ada.Strings.Unbounded.Unbounded_String;
      Add_First : Boolean;
      Line      : Positive := 1;
      Column    : Positive := 1;
      Separator : Character;
      Popup     : Popup_Index := Main_Window;

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
            Group : Radio_Switch;
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

   type Frame_Description is record
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

   type Switches_Editor_Config_Record is record
      Lines             : Positive;
      Columns           : Positive;
      Config            : Command_Line_Configuration;
      Show_Command_Line : Boolean := True;
      Default_Separator : Ada.Strings.Unbounded.Unbounded_String;
      Sections          : Ada.Strings.Unbounded.Unbounded_String;
      Scrolled_Window   : Boolean := False;
      Switch_Char       : Character;
      Frames            : Frame_Description_Vectors.Vector;
      Switches          : Switch_Description_Vectors.Vector;
      Max_Radio         : Radio_Switch := 0;
      Max_Popup         : Popup_Index := Main_Window;
      Dependencies      : Dependency_Description_Access := null;
   end record;

end Switches_Chooser;
