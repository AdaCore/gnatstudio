-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

--  <description>
--  This package implements a new widget to interactively edit the switches
--  for the GNAT tools (currently supported are gnatmake, gcc, gnatbind and
--  gnatlink).
--  A GUI is provided for the more common switches, but the user can always
--  edit them through an interactive command line.
--  </description>

with Glib;
with Gtk.Box;
with Gtk.Table;        use Gtk.Table;
with Gtk.GEntry;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Size_Group;
with Gtk.Tooltips;
with Gtk.Widget;
with GNAT.OS_Lib;
with Glib.Object;
with Glide_Kernel;
with Projects;
with VFS;

package Switches_Editors is

   type Switches_Edit_Record is new Gtk_Notebook_Record with private;
   type Switches_Edit is access all Switches_Edit_Record'Class;

   -----------
   -- Pages --
   -----------

   type Switches_Editor_Page_Record is new Gtk_Table_Record with private;
   type Switches_Editor_Page is access all Switches_Editor_Page_Record'Class;

   procedure Gtk_New
     (Page             : out Switches_Editor_Page;
      Title            : String;
      Project_Package  : String;
      Attribute_Index  : String := "";
      Lines, Cols      : Glib.Guint;
      Tips             : access Gtk.Tooltips.Gtk_Tooltips_Record'Class);
   --  Create a new page, that should be displayed.
   --  You can restrict the display of this page to specific languages by
   --  calling Add_Language below. However, by default it is displayed for
   --  all languages.
   --  The page is setup as a table of (Line + 1) x Cols, the last line
   --  being automatically created for the command line.
   --  Title is displayed in the notebook tab of the switches editor.
   --
   --  Project_Package is the name of the package, in the project files, where
   --  the switches are stored. Attribute_Index is the index of the attribute
   --  to be created in Project_Package.

   procedure Add_Language
     (Page : access Switches_Editor_Page_Record;
      Language_Filter : String);
   --  Add a new language to the list of languages for which this page should
   --  be displayed.
   --  By default, the page is displayed for all languages, until you have
   --  added at least one filter.

   procedure Create_Check
     (Page   : access Switches_Editor_Page_Record;
      Box    : access Gtk.Box.Gtk_Box_Record'Class;
      Label  : String;
      Switch : String;
      Tip    : String := "");
   --  Create a new check button for a simple switch in Page.
   --  The new button is added at the end of Box.
   --  Switch is the switch to put on the command line when the button is
   --  activated.

   procedure Create_Spin
     (Page              : access Switches_Editor_Page_Record;
      Box               : access Gtk.Box.Gtk_Box_Record'Class;
      Label             : String;
      Switch            : String;
      Min, Max, Default : Integer;
      Tip               : String := "";
      Label_Size_Group : Gtk.Size_Group.Gtk_Size_Group := null);
   --  Create a new spin button for a switch with multiple levels.
   --  The actual switch on the command line is "-" & Switch & Level, as in
   --  "-j2".
   --  If Default is selected, then no switch is needed on the command line.

   type Cst_String_Access is access constant String;
   type Cst_Argument_List is array (Positive range <>) of Cst_String_Access;

   type Radio_Switch is record
      Label  : Cst_String_Access;
      Switch : Cst_String_Access;
      Tip    : Cst_String_Access := null;
   end record;

   type Radio_Switch_Array is array (Positive range <>) of Radio_Switch;

   procedure Create_Radio
     (Page    : access Switches_Editor_Page_Record;
      Box     : access Gtk.Box.Gtk_Box_Record'Class;
      Buttons : Radio_Switch_Array);
   --  Create a series of radio buttons. Only one of them can be active at any
   --  time. No copy is made of the string accesses in Buttons.

   type Combo_Switch is record
      Label : Cst_String_Access;
      Value : Cst_String_Access;
   end record;

   type Combo_Switch_Array is array (Positive range <>) of Combo_Switch;

   function Create_Combo
     (Page                 : access Switches_Editor_Page_Record;
      Label                : String;
      Switch               : String;
      Default_No_Switch    : String;
      Default_No_Digit     : String;
      Buttons              : Combo_Switch_Array;
      Tip                  : String := "";
      Label_Size_Group     : Gtk.Size_Group.Gtk_Size_Group := null)
      return Gtk.Widget.Gtk_Widget;
   --  Create a new combo button. Switch is displayed on the left of the combo
   --  box. The newly created widget is returned, and it includes the label,
   --  combo,...
   --  Switch is always used when the button is actived, followed by the value
   --  for the specific line selected in the combo.
   --
   --  If the value is Default_No_Switch, no switch is necessary on the command
   --  line.
   --  If the value is Default_No_Digit, no additional digit is necessary in
   --  addition to Switch.
   --
   --  if Label_Size_Group is not null, then the label is added to that
   --  group. This can be used to provide a nicer layout of the widgets.

   function Create_Popup
     (Label  : String;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Create a new button. This will open a popup window, which displays
   --  additional switches.

   procedure Add_Dependency
     (Page           : access Switches_Editor_Page_Record;
      Master_Page    : String;
      Master_Switch  : String;
      Master_Status  : Boolean;
      Slave_Page     : String;
      Slave_Switch   : String;
      Slave_Activate : Boolean := True);
   --  Add dependency between two switches: if Master switch's status becomes
   --  Master_Status, then Slave_Switch will be automatically set to a new
   --  state (Activate), and set insensitive until Master_Switch is set
   --  insensitive again.
   --  For instance: if Master_Switch is "-g" for the builder, and Slave_Switch
   --  is "-g" for the compiler, with Master_Status=True and
   --  Slave_Activate=True, then everytime the user selects "-g" for the
   --  builder, "-g" will also be forced for the compiler.

   function Get_Page
     (Editor : access Switches_Edit_Record'Class;
      Title  : String) return Switches_Editor_Page;
   --  Return the page with the given title, or null if no such page has been
   --  added yet.

   procedure Add_Coalesce_Switch
     (Page              : access Switches_Editor_Page_Record'Class;
      Switch            : String;
      Default_As_String : String := "");
   --  Defines Switch as a common switch (ie all other switches that start with
   --  Switch will be collapse into one (for instance: if Switch is "-gnaty",
   --  then "-gnatya" and "-gnatyl" are collapsed into "-gnatyal").
   --  Default_As_String is the switch that Switch by itself on the command
   --  line is equal to (e.g. -gnaty = -gnaty3abcefhiklmnprst). In case the
   --  result of coalescing switches comes down to Default_As_String, then only
   --  Switch will be put on the command line
   --
   --  See also Add_Custom_Expansion below.

   procedure Add_Custom_Expansion
     (Page : access Switches_Editor_Page_Record'Class;
      Switch  : String;
      Default : Cst_Argument_List);
   --  Default are the switches used when Switch is found on the command line
   --  by itself.
   --  This works in close coordination with coalesce switches: when updating
   --  the command line, for instance after the user has selected a button,
   --  this package does the following:
   --   - Expand the switches found on the command line.
   --      => e.g.:  -gnaty => -gnaty3 -gnatya -gnatyb -gnatyc
   --   - Add or remove switches from the command line, depending on the
   --     currently activated GUI elements.
   --      => e.g.:  If the button for -gnatyc is not activated, the command
   --                line becomes   -gnatya -gnatyb
   --   - The command line is post-processed to coalesce switches.
   --      => e.g.:  -gnatya -gnatyb  => -gnatyab
   --   - Simplify the command line if possible through the
   --     Default_As_String parameter to Add_Coalesce_Switch
   --
   --  Therefore, the purpose of this expansion step is to decompose complex
   --  switches in the list of their components so that each component is
   --  associated with only one GUI element.
   --  The purpose of the coalesce step is to make the switches more readable
   --  by the user.

   ---------------------
   -- Switches editor --
   ---------------------

   procedure Gtk_New
     (Editor : out Switches_Edit;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new switches editor.

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record;
      Languages : GNAT.OS_Lib.Argument_List);
   --  Set the visible pages based on the specific languages

   function Generate_Project
     (Switches           : access Switches_Edit_Record'Class;
      Project            : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Files              : VFS.File_Array)
      return Boolean;
   --  Generate the information in Project to represent the status of Switches.
   --  True is returned if at least one project was modified.
   --  Project can be No_Project, in which case the return value will
   --  always be non empty, after modification of the project.

   -----------------------------------------------------
   -- Editing switches for a specific file or project --
   -----------------------------------------------------
   --  The subprograms below are convenience subprogram to edit some specific
   --  switches. They provide a higher-level framework over the standard
   --  switches editor.

   procedure Edit_Switches
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Callback suitable for a contextual menu item.  If the file name is the
   --  empty string, then the default switches for the project are edited,
   --  otherwise the switches for the specific file are edited.

   function Edit_Switches_For_Files
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Files        : VFS.File_Array) return Boolean;
   --  Edit the switches for a list of files. All the files will be assigned
   --  the same switches.
   --  If there are no files in Files, the default switches are edited.
   --  Return true if the switches were modified.

   procedure Set_Switches
     (Editor : access Switches_Edit_Record; Project : Projects.Project_Type);
   --  Set the initial value for the switches, based on the contents
   --  of Project_View. If a page doesn't exist in Editor, it will not be
   --  automatically created.
   --  Project_View can be No_Project, in which case only Ada-related pages are
   --  displayed.

private

   type Switch_Basic_Widget_Record (Switch_Length : Natural) is abstract
   tagged record
      Switch : String (1 .. Switch_Length);
   end record;
   type Switch_Basic_Widget is access all Switch_Basic_Widget_Record'Class;

   function Get_Switch (Switch : Switch_Basic_Widget_Record) return String
      is abstract;
   --  Return the string to add to the command line if the widget is active. If
   --  the widget is not active, the empty string is returned

   procedure Filter_Switch
     (Switch : Switch_Basic_Widget_Record;
      List   : in out GNAT.OS_Lib.Argument_List) is abstract;
   --  Remove (and free) from List the switches that would correspond to the
   --  one edited by Switch.

   procedure Set_And_Filter_Switch
     (Switch : Switch_Basic_Widget_Record;
      List   : in out GNAT.OS_Lib.Argument_List) is abstract;
   --  If one of the switches in List matches Switch, the widget is actived as
   --  appropriate, and the entry in List is freed and set to null.

   type Widget_Array is array (Natural range <>) of Switch_Basic_Widget;
   type Widget_Array_Access is access Widget_Array;

   type String_List_Array is array (Natural range <>) of
     GNAT.OS_Lib.Argument_List_Access;
   type String_List_Array_Access is access all String_List_Array;

   type Dependency_Description;
   type Dependency_Description_Access is access Dependency_Description;
   type Dependency_Description is record
      Master_Page, Slave_Page     : GNAT.OS_Lib.String_Access;
      Master_Switch, Slave_Switch : GNAT.OS_Lib.String_Access;
      Master_Status, Slave_Status : Boolean;
      Next                        : Dependency_Description_Access;
   end record;
   --  Description of a dependency (see Add_Dependency). This is needed because
   --  the dependencies can only be fully setup once all pages have been
   --  created.

   type Switches_Editor_Page_Record is new Gtk_Table_Record with record
      Lang_Filter : GNAT.OS_Lib.Argument_List_Access;
      --  List of languages for which this page applies

      Dependencies : Dependency_Description_Access;

      Attribute_Index : GNAT.OS_Lib.String_Access;
      Title    : GNAT.OS_Lib.String_Access;
      Pkg      : GNAT.OS_Lib.String_Access;
      Switches : Widget_Array_Access;
      Cmd_Line : Gtk.GEntry.Gtk_Entry;
      Tips     : Gtk.Tooltips.Gtk_Tooltips;

      Coalesce_Switches : GNAT.OS_Lib.String_List_Access;
      --  List of coalesce switches (see Add_Coalesce_Switch). This is never
      --  null once the widget has been created.

      Coalesce_Switches_Default : GNAT.OS_Lib.String_List_Access;
      --  This array has the same size as Coalesce_Switches, and contains the
      --  values of Default_As_String passed to Add_Coalesce_Switch

      Expansion_Switches : String_List_Array_Access;
      --  List of custom expansions. The first string in each element is the
      --  command line switch we are replacing

      Block_Refresh : Boolean := False;
      --  Used to avoid infinite recursion in the handling of signals
   end record;

   type Pages_Array is array (Natural range <>) of Switches_Editor_Page;
   type Page_Array_Access is access Pages_Array;

   type Switches_Edit_Record is new Gtk_Notebook_Record with record
      Kernel       : Glide_Kernel.Kernel_Handle;
      Files        : VFS.File_Array_Access;
      Project      : Projects.Project_Type;
      Pages        : Page_Array_Access;
   end record;

end Switches_Editors;
