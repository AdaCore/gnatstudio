------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

with Gdk.Event;                 use Gdk.Event;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;

with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Editable;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;     use Gtk.Tree_Model_Filter;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Basic_Types;               use Basic_Types;
with File_Utils;                use File_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Core_Kernels;          use GPS.Core_Kernels;
with GPS.Project_Properties;    use GPS.Project_Properties;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Language_Handlers;         use Language_Handlers;
with Projects;                  use Projects;
with Scenario_Selectors;        use Scenario_Selectors;
with Switches_Editors;          use Switches_Editors;
with Toolchains_Editor;         use Toolchains_Editor;
with GPR;
with Wizards;                   use Wizards;

package body Project_Properties is
   use Widget_List;

   Me : constant Trace_Handle := Create ("Project_Properties");

   type Attribute_Editor_Record;
   type Attribute_Editor is access all Attribute_Editor_Record'Class;

   subtype File_Filter is GPS.Project_Properties.File_Filter;

   type Editable_Attribute_Description (Indexed : Boolean := False) is
     new Attribute_Description (Indexed) with
   record
      Editor : Attribute_Editor;
   end record;

   type Editable_Attribute_Description_Access is
     access all Editable_Attribute_Description;

   package Attribute_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Editable_Attribute_Description_Access);

   function Is_Valid (Page : Attribute_Page) return String;
   --  Whether all editors on the page have a valid value for their attribute.
   --  See the description of Is_Valid for an Attribute_Description

   Column_Page_Name     : constant := 0;
   Column_Page_Contents : constant := 1;   --  a Project_Editor_Page
   Column_Visible       : constant := 2;
   Column_Path          : constant := 3;  --  e.g. 'Naming/Ada' (testsuite)
   --  The contents of the tree model that describes all the pages.

   Response_Edit : constant Gtk_Response_Type := 1;
   --  Response from a dialog to edit the source file

   -----------------------
   -- Properties module --
   -----------------------

   type Properties_Module_ID_Record is new Base_Properties_Module
     with null record;

   overriding function New_Attribute_Description
     (Module  : access Properties_Module_ID_Record;
      Indexed : Boolean)
      return Attribute_Description_Access;

   type Properties_Module_ID_Access
     is access all Properties_Module_ID_Record'Class;
   Properties_Module_ID : Properties_Module_ID_Access;

   -----------------------
   -- Attribute editors --
   -----------------------

   type Attribute_Editor_Record is abstract new Root_Attribute_Editor_Record
   with record
      Kernel    : Kernel_Handle;
      Project   : Project_Type;
      Attribute : Editable_Attribute_Description_Access;
      --  Description of the attribute

      Wiz       : Wizard := null;
      --  Wiz is the wizard in which the page is displayed. It should be null
      --  if we are not in a wizard, but in the project properties editor. It
      --  is used when validating fields, to display appropriate error
      --  messages.

      Active_Check : Gtk_Check_Button;
      --  Set if the editor is not always activated, and this indicates the
      --  state of the editor in this case.
   end record;

   procedure Create_Widget_Attribute
     (Kernel      : access Kernel_Handle_Record'Class;
      Wiz         : Wizard;
      Project     : Project_Type;
      Attr        : Editable_Attribute_Description_Access;
      Size_Group  : in out Gtk_Size_Group;
      Read_Only   : Boolean;
      Path_Widget : Gtk_Entry;
      Widget      : out Gtk_Widget;
      Expandable  : out Boolean;
      Context     : String);
   --  Create the widget used to edit Attr.
   --  Size_Group, if specified, is used to properly align all the labels. It
   --  is created as needed.
   --  Expandable indicates whether the resulting widget should be expandable
   --  in its container box when the latter is resized, or whether it should
   --  keep its current size.
   --  Path_Widget should contain the full directory to the project file's
   --  location, and is used to resolve relative names.
   --  See the description of Attribute_Editor_Record.Wiz for info on the Wiz
   --  parameter.

   function Create_Widget_Attribute
     (Kernel          : access Kernel_Handle_Record'Class;
      Wiz             : Wizard;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Path_Widget     : Gtk_Entry;
      Is_List         : Boolean) return Attribute_Editor;
   --  Create the widget used to edit the attribute. This edits a specific
   --  attribute type, associated with Description (since for all indexes in
   --  the attribute the type can be different).
   --  See above for the description of Path_Widget
   --  See the description of Attribute_Editor_Record.Wiz for info on the Wiz
   --  parameter.

   function Create_Attribute_Dialog
     (Kernel          : access Kernel_Handle_Record'Class;
      Toplevel        : access Gtk.Window.Gtk_Window_Record'Class;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Project_Path    : Filesystem_String) return GNAT.Strings.String_List;
   --  Ask the user (through a dialog) for a new value for the attribute.
   --  Multiple values can be returned if the attribute is a list. Returned
   --  value must be freed by the user.

   -----------------------------------------------
   -- Attribute editors (files and directories) --
   -----------------------------------------------

   type File_Attribute_Editor_Record is new Attribute_Editor_Record with record
      As_Directory : Boolean := False;
      Filter       : File_Filter := Filter_None;
      Ent          : Gtk_Entry;
      Model        : Gtk_Tree_Store;
      View         : Gtk_Tree_View;
      Path_Widget  : Gtk_Entry;
      Use_History  : Boolean;
   end record;
   type File_Attribute_Editor is access all File_Attribute_Editor_Record'Class;
   --  Use_History is true if the value should be saved in the history, for
   --  easy retrieval later on.

   function Create_File_Attribute_Editor
     (Kernel          : access Kernel_Handle_Record'Class;
      Wiz             : Wizard;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Path_Widget     : Gtk_Entry;
      Is_List         : Boolean) return File_Attribute_Editor;
   --  Create a new attribute editor for a string attribute (any string, file
   --  or directory).
   --  Is_List should be true if the attribute is a list of simple strings
   --  Path_Widget is the widget that contains the full path to the project
   --  file's location, and is used to resolve relative names.
   --  See the description of Attribute_Editor_Record.Wiz for info on the Wiz
   --  parameter.

   overriding function Get_Value_As_List
     (Editor          : access File_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.Strings.String_List;
   overriding function Get_Value_As_String
     (Editor : access File_Attribute_Editor_Record;
      Attribute_Index : String := "") return String;
   overriding procedure Generate_Project
     (Editor             : access File_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean);
   overriding function Is_Valid
     (Editor : access File_Attribute_Editor_Record) return String;
   --  See doc from inherited subprogram

   procedure Select_File (Editor : access Gtk_Widget_Record'Class);
   --  Called when the user selects the "Browse" button in the
   --  File_Attribute_Editor

   procedure Remove_String_From_List (Editor : access Gtk_Widget_Record'Class);
   procedure Add_String_In_List (Editor : access Gtk_Widget_Record'Class);
   --  Add or remove a string from a list of strings, as a result of a user
   --  pressing a button.

   procedure Move_String_Up (Editor : access Gtk_Widget_Record'Class);
   procedure Move_String_Down (Editor : access Gtk_Widget_Record'Class);
   --  Change the position of the current item in the list

   procedure Recursive_Directory_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a directory is made recursive

   procedure Project_Path_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the location of the project is changed, to update the
   --  relative paths' display

   -------------------------------
   -- Attribute editors (lists) --
   -------------------------------

   type List_Attribute_Editor_Record is new Attribute_Editor_Record with record
      Model : Gtk_Tree_Store;
      Combo : Gtk_Combo_Box_Text;
   end record;
   type List_Attribute_Editor is access all List_Attribute_Editor_Record'Class;

   overriding function Get_Value_As_List
     (Editor          : access List_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.Strings.String_List;
   overriding function Get_Value_As_String
     (Editor : access List_Attribute_Editor_Record;
      Attribute_Index : String := "") return String;
   overriding procedure Generate_Project
     (Editor             : access List_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean);
   --  See doc from inherited subprogram

   function Create_List_Attribute_Editor
     (Kernel          : access Kernel_Handle_Record'Class;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Is_List         : Boolean) return List_Attribute_Editor;
   --  Create the widget used to edit an attribute with a dynamic or static
   --  list type.
   --  Is_List indicates whether the value of the attribute is a list of
   --  values from this list, as opposed to a single value from the list.

   procedure Attribute_List_Changed
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a selection has changed in the editor

   procedure Select_Attribute_In_List
     (Project     : Project_Type;
      Index_Pkg   : String;
      Index_Name  : String;
      Index_Value : String;
      Is_Selected : Boolean);
   --  A specific value is selected in a list.
   --  Check whether any other attribute used this one as an index, and update
   --  their editor

   ---------------------------------
   -- Attribute editors (indexed) --
   ---------------------------------

   type Indexed_Values is record
      Index  : GNAT.Strings.String_Access;
      Values : GNAT.Strings.String_List_Access;
   end record;
   type Indexed_Values_Array is array (Natural range <>) of Indexed_Values;
   type Indexed_Values_Array_Access is access Indexed_Values_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Indexed_Values_Array, Indexed_Values_Array_Access);

   type Indexed_Attribute_Editor_Record is new Attribute_Editor_Record with
      record
         View           : Gtk_Tree_View;
         Model          : Gtk_Tree_Store;
         Path_Widget    : Gtk_Entry;
         Current_Values : Indexed_Values_Array_Access;
      end record;
   type Indexed_Attribute_Editor is
     access all Indexed_Attribute_Editor_Record'Class;

   overriding function Get_Value_As_List
     (Editor          : access Indexed_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.Strings.String_List;
   overriding function Get_Value_As_String
     (Editor : access Indexed_Attribute_Editor_Record;
      Attribute_Index : String := "") return String;
   overriding procedure Generate_Project
     (Editor             : access Indexed_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean);
   --  See doc from inherited subprogram

   procedure On_Indexed_Editor_Destroy
     (Editor : access Gtk_Widget_Record'Class);
   --  Called when the editor for indexed attributes is destroyed

   function Create_Indexed_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Attr        : Editable_Attribute_Description_Access;
      Path_Widget : Gtk_Entry) return Indexed_Attribute_Editor;
   --  Create the widget used to edit an indexed attribute

   function Edit_Indexed_Attribute
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Called when double-clicking on the value of an indexed attribute, and
   --  open a dialog to edit its value (that dialog contains one the standard
   --  widgets like combo boxes,... depending on the type of the attribute)

   type General_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Scenarios)
   with record
      Use_Relative_Paths : Gtk.Check_Button.Gtk_Check_Button;
      Name               : Gtk.GEntry.Gtk_Entry;
      Path               : Gtk.GEntry.Gtk_Entry;
   end record;
   type General_Page_Access is access all General_Page_Record'Class;
   overriding procedure Initialize
     (Self         : not null access General_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access General_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding function Is_Valid
     (Self         : not null access General_Page_Record) return String;
   --  Create the "General" page for the project properties

   type Properties_Editor_Record is new Gtk.Dialog.Gtk_Dialog_Record with
   record
      Tree               : Gtk_Tree_View;
      List_Of_Pages      : Gtk_Tree_Store;

      Filter             : Gtk_Tree_Model_Filter;
      --  The filter used to hide the pages that do not apply to the selected
      --  languages.

      Current_Page       : Gtk_Box;
      --  The location where the current selected page should be displayed

      Errors             : Gtk_Label;

      Selector           : Scenario_Selector;
      Prj_Selector       : Project_Selector;

      General_Page       : General_Page_Access;
      Languages_Editor   : Languages_Page_Access;

      Project            : Project_Type;
      Kernel             : Kernel_Handle;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Gtk_New
     (Editor  : out Properties_Editor;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Read_Only : Boolean);
   --  Create a new properties editor

   procedure Initialize
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Read_Only : Boolean);
   --  Internal initialization function

   procedure On_Languages_Change
     (Editor : access GObject_Record'Class;
      Path   : String);
   --  Called when the list of selected languages changes

   procedure For_Each_Page
     (Self         : not null access Properties_Editor_Record'Class;
      Callback     : Page_Iterator_Callback;
      Visible_Only : Boolean := True);
   --  Calls Callback for each visible page in the editor

   procedure For_All_Pages
     (Data : access GObject_Record'Class;
      Callback : Page_Iterator_Callback);
   --  Iterate over all pages of a properties editor.
   --  Profile compatible with a Tool_From_Name_Getter, to handle switch
   --  dependencies between tools.

   procedure Find_Or_Create_Page
     (Editor  : not null access Properties_Editor_Record'Class;
      Name    : String;
      Page    : not null access Project_Editor_Page_Record'Class);
   --  Add a new page in the properties editor

   procedure Editor_Destroyed
     (Editor : access Gtk_Widget_Record'Class;
      Attr   : Editable_Attribute_Description_Access);
   --  Called when an editor is destroyed

   function Row_Is_Visible
     (Self : Gtk_Tree_Model; Iter : Gtk_Tree_Iter) return Boolean;
   --  Whether a row should be visible (i.e. whether the page applies to the
   --  selected languages)

   procedure Set_Visible_Pages
     (Self      : not null access Properties_Editor_Record'Class;
      Languages : GNAT.Strings.String_List;
      Parent    : Gtk_Tree_Iter := Null_Iter);
   --  Refresh which page should be visible, and the contents of pages.

   function Get_Languages
     (Editor : Properties_Editor) return String_List_Access;
   --  Return the list of languages currently set in the editor

   procedure Toggle_Sensitive
     (Check : access Gtk_Widget_Record'Class;
      Attr  : Editable_Attribute_Description_Access);
   --  Toggle the sensitivity state of an editor

   function Get_Current_Page
     (Self : not null access Properties_Editor_Record'Class)
      return Project_Editor_Page;
   --  Get the page that is currently displayed to the user

   procedure On_Selection_Changed (Editor : access GObject_Record'Class);
   --  Called when a new page is selected by the user

   function Paths_Are_Relative (Project : Project_Type) return Boolean;
   --  Return True if the paths in the project should be relative paths

   function Select_Files_Or_Directories
     (Toplevel       : access Gtk_Window_Record'Class;
      Project        : Project_Type;
      Default        : Filesystem_String;
      Project_Path   : Filesystem_String;
      As_Directory   : Boolean;
      Filter         : File_Filter;
      Allow_Multiple : Boolean := False) return GNATCOLL.VFS.File_Array;
   --  Open a dialog to select one or more file or directory names.
   --  Filter is used to select what kind of files should be shown to the user

   function Get_Current_Value
     (Project       : Project_Type;
      Attr          : Editable_Attribute_Description_Access;
      Index         : String) return String;
   function Get_Current_Value
     (Kernel        : access Core_Kernel_Record'Class;
      Project       : Project_Type;
      Attr          : Editable_Attribute_Description_Access;
      Index         : String := "") return String_List_Access;
   --  Get the current value for the given attribute. This value is extracted
   --  from one of three sources, in that order:
   --    - Either the current editor for that attribute. This reflects the
   --      changes that the user is currently doing
   --    - The value in the current project, if such project exists. This
   --      reflects the value this attribute had before the editor was started
   --    - The default value as specified in the attribute definition

   procedure Delete_Attribute_Value
     (Attr               : Editable_Attribute_Description_Access;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean;
      Attribute_Index    : String := "");
   --  Remove the declaration for Attr in Project

   function Get_Attribute_Type_From_Name
     (Pkg : String; Name : String)
      return Editable_Attribute_Description_Access;
   --  Find the description of an attribute given its package and name

   procedure Update_Attribute_Value
     (Attr               : Editable_Attribute_Description_Access;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Value              : String;
      Project_Changed    : in out Boolean;
      Entry_Value        : String := "";
      Attribute_Index    : String := "");
   --  Entry value is the value as entered into the form. We need this value to
   --  check it against the default value that must be omitted. If Entry_Value
   --  is not specified then Value is used instead.

   procedure Update_Attribute_Value
     (Kernel             : access Kernel_Handle_Record'Class;
      Attr               : Editable_Attribute_Description_Access;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Values             : GNAT.Strings.String_List;
      Attribute_Index    : String := "";
      Project_Changed    : in out Boolean);
   --  Change, if needed, a project to reflect the new value of the attribute

   function To_String (List : GNAT.Strings.String_List) return String;
   --  Convert List into a string suitable to represent it. The string need not
   --  be parsable again to extract the values.

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   function Get_Safe_Text (Ent : Gtk_Entry) return String;
   --  Return text contained in Ent, stripped of any LF

   function History_Name
     (Description : access Attribute_Description'Class) return History_Key;
   --  Return the name of the history key associated with Description

   ------------------
   -- Wizard pages --
   ------------------

   type XML_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios)
   with record
      Descr   : Attribute_Page;
      Context : GNAT.Strings.String_Access;  --  where is this page visible ?

      Has_Contents : Boolean := False;

      Path   : Gtk_GEntry;
      --  Widget that contains the path of the project, so that the various
      --  pages can edit the project.
      --  ??? Is this needed, it seems this info could be extracted from the
      --  project itself when necessary
   end record;
   type XML_Page_Access is access all XML_Page_Record'Class;
   overriding procedure Initialize
     (Self         : not null access XML_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access XML_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding function Is_Valid
     (Self         : not null access XML_Page_Record) return String;
   overriding procedure Destroy (Self : in out XML_Page_Record);

   -------------------
   -- Get_Safe_Text --
   -------------------

   function Get_Safe_Text (Ent : Gtk_Entry) return String is
   begin
      --  In the dialogs, ASCII.LFs may have been introduced through
      --  copy-pasting: strip them otherwise the resulting project file
      --  will not be syntactically valid
      return Strip_Character (Gtk.GEntry.Get_Text (Ent), ASCII.LF);
   end Get_Safe_Text;

   -------------------------------
   -- On_Indexed_Editor_Destroy --
   -------------------------------

   procedure On_Indexed_Editor_Destroy
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed : constant Indexed_Attribute_Editor :=
             Indexed_Attribute_Editor (Editor);
   begin
      if Ed.Current_Values /= null then
         for C in Ed.Current_Values'Range loop
            Free (Ed.Current_Values (C).Index);
            Free (Ed.Current_Values (C).Values);
         end loop;
         Unchecked_Free (Ed.Current_Values);
      end if;
   end On_Indexed_Editor_Destroy;

   ----------------------------
   -- Delete_Attribute_Value --
   ----------------------------

   procedure Delete_Attribute_Value
     (Attr               : Editable_Attribute_Description_Access;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean;
      Attribute_Index    : String := "")
   is
      Attribute             : constant Attribute_Pkg_String :=
                                Build (Attr.Pkg.all, Attr.Name.all);
      Lower_Attribute_Index : String := Attribute_Index;
   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      if Attribute_Exists (Attr, Project, Attribute_Index) then
         Trace (Me, "Project changed since attribute "
                & Attr.Pkg.all & "'" & Attr.Name.all
                & " was removed");

         Project.Delete_Attribute
           (Scenario  => Scenario_Variables,
            Attribute => Attribute,
            Index     => Lower_Attribute_Index);
         Project_Changed := True;
      end if;
   end Delete_Attribute_Value;

   ----------------------------
   -- Update_Attribute_Value --
   ----------------------------

   procedure Update_Attribute_Value
     (Attr               : Editable_Attribute_Description_Access;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Value              : String;
      Project_Changed    : in out Boolean;
      Entry_Value        : String := "";
      Attribute_Index    : String := "")
   is
      Attribute             : constant Attribute_Pkg_String :=
                                Build (Attr.Pkg.all, Attr.Name.all);
      Lower_Attribute_Index : String := Attribute_Index;
   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      declare
         Default_Value : constant String := Get_Default_Value
           (Attr         => Attr,
            Index        => Lower_Attribute_Index);
         Old_Value     : constant String := Get_Value_From_Project
           (Project       => Project,
            Attr          => Attr,
            Index         => Lower_Attribute_Index);
      begin
         if Value /= Old_Value then
            if Attr.Omit_If_Default
              and then
                ((Entry_Value /= "" and then Entry_Value = Default_Value)
                   or else Value = Default_Value)
            then
               Project.Delete_Attribute
                 (Scenario  => Scenario_Variables,
                  Attribute => Attribute,
                  Index     => Lower_Attribute_Index);
            else
               Project.Set_Attribute
                 (Scenario  => Scenario_Variables,
                  Attribute => Attribute,
                  Value     => Value,
                  Index     => Lower_Attribute_Index);
            end if;

            if Active (Me) then
               Trace (Me, "Change for string attribute "
                      & Attr.Pkg.all & "'" & Attr.Name.all
                      & " (" & Lower_Attribute_Index
                      & ") Old=""" & Old_Value
                      & """ Default=""" & Default_Value
                      & """ New=""" & Value & """");
            end if;

            Project_Changed := True;
         end if;
      end;
   end Update_Attribute_Value;

   ----------------------------
   -- Update_Attribute_Value --
   ----------------------------

   procedure Update_Attribute_Value
     (Kernel             : access Kernel_Handle_Record'Class;
      Attr               : Editable_Attribute_Description_Access;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Values             : GNAT.Strings.String_List;
      Attribute_Index    : String := "";
      Project_Changed    : in out Boolean)
   is
      Attribute             : constant Attribute_Pkg_List :=
                                Build (Attr.Pkg.all, Attr.Name.all);
      Lower_Attribute_Index : String := Attribute_Index;
      Equal                 : Boolean;

   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      declare
         Old_Values : String_List_Access :=
           Get_Value_From_Project
             (Kernel        => Kernel,
              Project       => Project,
              Attr          => Attr,
              Index         => Lower_Attribute_Index);

      begin
         if (Old_Values = null or else Old_Values'Length = 0)
           and then Values'Length = 0
         then
            Equal := True;

         elsif Old_Values /= null then
            Equal := Is_Equal
              (Values, Old_Values.all, Case_Sensitive => False,
               Ordered => Attr.Ordered_List);

         else
            Old_Values :=
              Get_Default_Value
                (Kernel       => Kernel,
                 Attr         => Attr,
                 Index        => Lower_Attribute_Index);

            Equal := Old_Values /= null
              and then Is_Equal
                (Values, Old_Values.all, Case_Sensitive => False,
                    Ordered => Attr.Ordered_List);
         end if;

         if not Equal then
            Project.Set_Attribute
              (Scenario  => Scenario_Variables,
               Attribute => Attribute,
               Values    => Values,
               Index     => Lower_Attribute_Index);

            if Active (Me) then
               Trace (Me, "Change for list attribute "
                      & Attr.Pkg.all & "'" & Attr.Name.all & "("
                      & Lower_Attribute_Index & ")");
            end if;

            Project_Changed := True;
         end if;

         Free (Old_Values);
      end;
   end Update_Attribute_Value;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Page : Attribute_Page) return String is
      Attr  : Editable_Attribute_Description_Access;
   begin
      if Page.Sections /= null then
         for Section in Page.Sections'Range loop
            if Page.Sections (Section).Attributes /= null then
               for J in Page.Sections (Section).Attributes'Range loop
                  Attr := Editable_Attribute_Description_Access
                    (Page.Sections (Section).Attributes (J));
                  if Attr.Editor /= null then
                     declare
                        Msg : constant String := Is_Valid (Attr.Editor);
                     begin
                        if Msg /= "" then
                           return Msg;
                        end if;
                     end;
                  end if;
               end loop;
            end if;
         end loop;
      end if;
      return "";
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Editor : access Root_Attribute_Editor_Record) return String
   is
      pragma Unreferenced (Editor);
   begin
      return "";
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Editor : access File_Attribute_Editor_Record) return String
   is
   begin
      if not Editor.Attribute.Non_Index_Type.Allow_Empty
        and then Get_Safe_Text (Editor.Ent) = ""
      then
         return -"Empty value not allowed for "
           & Attribute_Name (Editor.Attribute.all);
      else
         return "";
      end if;
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Self : not null access XML_Page_Record) return String is
   begin
      return Is_Valid (Self.Descr);
   end Is_Valid;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out XML_Page_Record) is
   begin
      Free (Self.Context);
   end Destroy;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Editor             : access File_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean)
   is
      Relative : constant Boolean :=
                   Get_Paths_Type (Project) = Projects.Relative
                     or else (Get_Paths_Type (Project) = From_Pref
                              and then Generate_Relative_Paths.Get_Pref);
      Iter     : Gtk_Tree_Iter;
      F1, F2   : Virtual_File;
   begin
      if Editor.Ent /= null then
         if Editor.Attribute.Base_Name_Only then
            Update_Attribute_Value
              (Attr               => Editor.Attribute,
               Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Value              => Base_Name (Get_Safe_Text (Editor.Ent)),
               Project_Changed    => Project_Changed);

         elsif Relative
           and then Editor.Attribute.Non_Index_Type.Typ /=
             Attribute_As_String
         then
            --  ??? We really should handle paths on the Build server directly
            F1 := Create_From_UTF8 (Get_Safe_Text (Editor.Ent));
            F2 := Create_From_UTF8 (Get_Safe_Text (Editor.Path_Widget));
            Update_Attribute_Value
              (Attr               => Editor.Attribute,
               Project            => Project,
               Scenario_Variables => Scenario_Variables,
               --  The paths in projects should be UNIX-style; the call to
               --  Create_From_UTF8 above could create Windows-style separators
               --  so replace this here.
               Value              => Replace
                 (+Relative_Path (F1, F2), "\", "/"),
               Entry_Value        => Get_Safe_Text (Editor.Ent),
               Project_Changed    => Project_Changed);

         else
            F1 := Create_From_UTF8 (Get_Safe_Text (Editor.Ent));
            Update_Attribute_Value
              (Attr               => Editor.Attribute,
               Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Value              => Get_Safe_Text (Editor.Ent),
               Project_Changed    => Project_Changed);
         end if;

      else
         declare
            Num    : constant Gint := N_Children (Editor.Model);
            Values : GNAT.OS_Lib.Argument_List (1 .. Integer (Num));
            N      : Integer := Values'First;
            File   : Virtual_File;
            Path   : Virtual_File;
         begin
            Iter := Get_Iter_First (Editor.Model);

            while Iter /= Null_Iter loop
               if Editor.Attribute.Non_Index_Type.Typ =
                 Attribute_As_String
               then
                  Values (N) := new String'
                    (Get_String (Editor.Model, Iter, 0));

               elsif Editor.Attribute.Non_Index_Type.Typ =
                 Attribute_As_Directory
                 and then Get_Boolean (Editor.Model, Iter, 1)
               then
                  File :=
                    Create_From_UTF8 (Get_String (Editor.Model, Iter, 0));

                  if Relative then
                     Path := Create_From_UTF8
                       (Get_Safe_Text (Editor.Path_Widget));

                     Ensure_Directory (File);

                     --  The build server might be unix and the local machine
                     --  Windows. As a result, it is better to always use
                     --  Unix paths in the project files so that the file is
                     --  correctly interpreted on all machines (K930-020)

                     Values (N) := new String'
                       (Format_Pathname
                          (+Name_As_Directory (Relative_Path (File, Path)),
                           UNIX)
                        & "**");
                  else
                     Values (N) := new String'
                       (Format_Pathname
                          (+Name_As_Directory (Full_Name (File)),
                           UNIX)
                        & "**");
                  end if;

               elsif Editor.Attribute.Base_Name_Only then
                  Values (N) := new String'
                    (Get_String (Editor.Model, Iter, 0));

               elsif Relative then
                  File :=
                    Create_From_UTF8 (Get_String (Editor.Model, Iter, 0));
                  Path := Create_From_UTF8
                    (Get_Safe_Text (Editor.Path_Widget));

                  Values (N) := new String'(+Relative_Path (File, Path));

               else
                  Values (N) := new String'
                    (Get_String (Editor.Model, Iter, 0));
               end if;
               N := N + 1;
               Next (Editor.Model, Iter);
            end loop;

            Update_Attribute_Value
              (Kernel             => Editor.Kernel,
               Attr               => Editor.Attribute,
               Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Values             => Values,
               Project_Changed    => Project_Changed);

            Free (Values);
         end;
      end if;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Editor             : access List_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean) is
   begin
      if Editor.Combo /= null then
         Update_Attribute_Value
           (Attr               => Editor.Attribute,
            Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Value              => Get_Active_Text (Editor.Combo),
            Project_Changed    => Project_Changed);

      else
         declare
            Values : String_List_Access := Get_Current_Value
              (Kernel  => Editor.Kernel,
               Project => Project,
               Attr    => Editor.Attribute);
         begin
            Update_Attribute_Value
              (Kernel             => Editor.Kernel,
               Attr               => Editor.Attribute,
               Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Values             => Values.all,
               Project_Changed    => Project_Changed);
            Free (Values);
         end;
      end if;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Editor             : access Indexed_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
      Project_Changed    : in out Boolean)
   is
      use type GPR.Name_Id;
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
      Attr  : constant Attribute_Pkg_String :=
                Build
                  (Package_Name   => Editor.Attribute.Pkg.all,
                   Attribute_Name => Editor.Attribute.Name.all);
      Indexes : GNAT.Strings.String_List := Project.Attribute_Indexes (Attr);

   begin
      --  Remove all the values that are no longer in the list. We keep those
      --  that are still in the list, so that we can compare them with their
      --  new value and detect changes (E308-006).

      while Iter /= Null_Iter loop
         declare
            Index : constant String := Get_String (Editor.Model, Iter, 0);
         begin
            for A in Indexes'Range loop
               if Indexes (A) /= null
                 and then Equal (Indexes (A).all, Index,
                                 Editor.Attribute.Case_Sensitive_Index)
               then
                  Free (Indexes (A));
                  exit;
               end if;
            end loop;
         end;
         Next (Editor.Model, Iter);
      end loop;

      --  The non-null entries remaining in Indexes are the ones that were not
      --  edited graphically (ie that have been removed by the user)

      for A in Indexes'Range loop
         if Indexes (A) /= null then
            Trace (Me, "Removing obsolete value "
                   & Editor.Attribute.Pkg.all
                   & ":" & Editor.Attribute.Name.all
                   & " (" & Indexes (A).all & ")");
            Project.Delete_Attribute
              (Scenario  => Scenario_Variables,
               Attribute => Attr,
               Index     => Indexes (A).all);
            Project_Changed := True;
         end if;
      end loop;

      Free (Indexes);

      --  Now set the proper value for the remaining variables

      Iter := Get_Iter_First (Editor.Model);

      while Iter /= Null_Iter loop
         declare
            Index : String := Get_String (Editor.Model, Iter, 0);
         begin
            if not Editor.Attribute.Case_Sensitive_Index then
               To_Lower (Index);
            end if;

            if Editor.Attribute.Is_List then
               declare
                  Values : String_List_Access := Get_Current_Value
                    (Kernel  => Editor.Kernel,
                     Project => Project,
                     Attr    => Editor.Attribute,
                     Index   => Index);
               begin
                  Update_Attribute_Value
                    (Kernel             => Editor.Kernel,
                     Attr               => Editor.Attribute,
                     Project            => Project,
                     Scenario_Variables => Scenario_Variables,
                     Values             => Values.all,
                     Attribute_Index    => Index,
                     Project_Changed    => Project_Changed);
                  Free (Values);
               end;

            else
               Update_Attribute_Value
                 (Attr               => Editor.Attribute,
                  Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Value              => Get_String (Editor.Model, Iter, 1),
                  Attribute_Index    => Index,
                  Project_Changed    => Project_Changed);
            end if;

            Next (Editor.Model, Iter);
         end;
      end loop;
   end Generate_Project;

   ------------------------------------
   -- Create_Project_Command_Handler --
   ------------------------------------

   procedure Create_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel        : constant Kernel_Handle := Get_Kernel (Data);
      Attribute_Cst : aliased constant String := "attribute";
      Package_Cst   : aliased constant String := "package";
      Index_Cst     : aliased constant String := "index";
      Tool_Cst      : aliased constant String := "tool";
      Value_Cst     : aliased constant String := "value";
      Recursive_Cst : aliased constant String := "recursive";
      Get_Attributes_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access);
      Tool_Parameters : constant Cst_Argument_List :=
        (1 => Tool_Cst'Unchecked_Access);
      Set_Attribute_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);
      Add_Attribute_Values_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);
      Remove_Attribute_Values_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);
      Clear_Attribute_Values_Parameters : constant Cst_Argument_List :=
        (1 => Attribute_Cst'Unchecked_Access,
         2 => Package_Cst'Unchecked_Access,
         3 => Index_Cst'Unchecked_Access,
         4 => Value_Cst'Unchecked_Access);

      procedure Set_Return_Attribute
        (Project           : Project_Type;
         Attr, Pkg, Index  : String;
         Attribute_Is_List : Boolean;
         As_List           : Boolean);
      --  Store in Data the value of a specific attribute.
      --  Attribute_Is_List indicates the type of the attribute. This is the
      --  first type that will be tested, although if no match is found the
      --  other type will also be tested.
      --  As_List indicates the format of the returned value

      procedure Set_Return_Attribute (List : Argument_List; As_List : Boolean);
      --  Sets the contents of List into the return value

      procedure Set_Return_Attribute
        (Value : String; As_List : Boolean);
      --  Sets the contents of Value into the return value

      --------------------------
      -- Set_Return_Attribute --
      --------------------------

      procedure Set_Return_Attribute
        (List : Argument_List; As_List : Boolean)
      is
         Result : Unbounded_String;
      begin
         if As_List then
            Set_Return_Value_As_List (Data);
            for L in List'Range loop
               Set_Return_Value (Data, List (L).all);
            end loop;

         else
            for L in List'Range loop
               Append (Result, List (L).all);

               if L /= List'Last then
                  Append (Result, " ");
               end if;
            end loop;
            Set_Return_Value (Data, To_String (Result));
         end if;
      end Set_Return_Attribute;

      --------------------------
      -- Set_Return_Attribute --
      --------------------------

      procedure Set_Return_Attribute
        (Value : String; As_List : Boolean) is
      begin
         if As_List then
            Set_Return_Value_As_List (Data);
         end if;
         Set_Return_Value (Data, Value);
      end Set_Return_Attribute;

      --------------------------
      -- Set_Return_Attribute --
      --------------------------

      procedure Set_Return_Attribute
        (Project           : Project_Type;
         Attr, Pkg, Index  : String;
         Attribute_Is_List : Boolean;
         As_List           : Boolean)
      is
         Descr  : constant Editable_Attribute_Description_Access :=
           Get_Attribute_Type_From_Name (Pkg, Attr);
      begin
         if Descr = null then
            --  Test whether the attribute is known anyway. Not all attributes
            --  are declared in projects.xml, in particular the predefined ones
            --  related to switches and naming, that have their own editor

            if Attribute_Is_List then
               declare
                  List : String_List_Access := Project.Attribute_Value
                    (Attribute_Pkg_List'(Build (Pkg, Attr)), Index,
                     Use_Extended => True);
                  Var  : constant String := Project.Attribute_Value
                    (Attribute_Pkg_String'(Build (Pkg, Attr)),
                     Default => "", Index => Index,
                     Use_Extended => True);
               begin
                  if List = null
                    and then Var /= ""
                  then
                     --  Did we have a string attribute in fact ?
                     Set_Return_Attribute (Var, As_List);

                  else
                     if List = null then
                        List := new GNAT.Strings.String_List'(1 .. 0 => null);
                     end if;

                     Set_Return_Attribute (List.all, As_List);
                  end if;

                  Free (List);
               end;

            else
               declare
                  Val : constant String := Project.Attribute_Value
                    (Attribute_Pkg_String'(Build (Pkg, Attr)),
                     Default => "", Index => Index,
                     Use_Extended => True);
               begin
                  if Val = "" then
                     --  Did we have a list attribute in fact ?
                     declare
                        List : String_List_Access := Project.Attribute_Value
                          (Attribute_Pkg_List'(Build (Pkg, Attr)), Index);
                     begin
                        if List /= null then
                           Set_Return_Attribute (List.all, As_List);
                        else
                           Set_Return_Attribute (Val, As_List);
                        end if;
                        Free (List);
                     end;

                  else
                     Set_Return_Attribute (Val, As_List);
                  end if;
               end;
            end if;
            return;
         end if;

         --  Else use the description from projects.xml, which also provides
         --  the default value for attributes not declared in the project
         if Descr.Is_List then
            declare
               List : String_List_Access := Get_Current_Value
                 (Kernel, Project, Descr, Index);
            begin
               if List /= null then
                  Set_Return_Attribute (List.all, As_List);
               else
                  Set_Return_Attribute
                    (Argument_List'(1 .. 0 => null), As_List);
               end if;
               Free (List);
            end;
         else
            Set_Return_Attribute
              (Get_Current_Value (Project, Descr, Index), As_List);
         end if;
      end Set_Return_Attribute;

   begin
      if Command = "get_attribute_as_list"
        or else Command = "get_attribute_as_string"
      then
         Name_Parameters (Data, Get_Attributes_Parameters);
         Set_Return_Attribute
           (Project => Get_Data (Data, 1),
            Attr    => Nth_Arg (Data, 2),
            Pkg     => Nth_Arg (Data, 3, ""),
            Index   => Nth_Arg (Data, 4, ""),
            Attribute_Is_List => Command = "get_attribute_as_list",
            As_List           => Command = "get_attribute_as_list");

      elsif Command = "get_tool_switches_as_list"
        or else Command = "get_tool_switches_as_string"
      then
         Name_Parameters (Data, Tool_Parameters);
         declare
            Tool  : constant String := Nth_Arg (Data, 2);
            Props : constant Tool_Properties :=
              Get_Tool_Properties (Kernel, Tool);
         begin
            if Props = null then
               Set_Error_Msg (Data, -"No such tool: " & Tool);

            else
               Set_Return_Attribute
                 (Project => Get_Data (Data, 1),
                  Attr    => To_String (Props.Project_Attribute),
                  Pkg     => To_String (Props.Project_Package),
                  Index   => To_String (Props.Project_Index),
                  Attribute_Is_List => True,
                  As_List => Command = "get_tool_switches_as_list");
            end if;
         end;

      elsif Command = "properties_editor" then
         Edit_Properties (Get_Data (Data, 1), Kernel);

      elsif Command = "is_modified" then
         Name_Parameters (Data, (  --  1 => Self,
                                 2 => Recursive_Cst'Unchecked_Access));
         declare
            Project   : constant Project_Type := Get_Data (Data, 1);
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
         begin
            Set_Return_Value (Data, Project.Modified (Recursive));
         end;

      elsif Command = "set_attribute_as_string" then
         Name_Parameters (Data, Set_Attribute_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3);
            Index          : constant String := Nth_Arg (Data, 4);
            Value          : constant String := Nth_Arg (Data, 5);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Project.Set_Attribute
                 (Attribute => Build (Package_Name, Attribute_Name),
                  Value     => Value,
                  Index     => Index);
            end if;
         end;

         Recompute_View (Get_Kernel (Data));

      elsif Command = "add_attribute_values" then
         Name_Parameters (Data, Add_Attribute_Values_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3);
            Index          : constant String := Nth_Arg (Data, 4);
            Values         : GNAT.OS_Lib.Argument_List
              (1 .. Number_Of_Arguments (Data) - 4);
            Attribute      : constant Attribute_Pkg_List :=
              Build (Package_Name, Attribute_Name);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               for J in 5 .. Number_Of_Arguments (Data) loop
                  Values (J - 4) := new String'(Nth_Arg (Data, J));
               end loop;

               if Project.Has_Attribute (Attribute, Index) then
                  Project.Set_Attribute
                    (Attribute => Attribute,
                     Values    => Values,
                     Index     => Index,
                     Prepend   => True);
               else
                  Project.Set_Attribute
                    (Attribute => Attribute,
                     Values    => Values,
                     Index     => Index,
                     Prepend   => False);
               end if;

               for J in Values'Range loop
                  Free (Values (J));
               end loop;
            end if;
         end;

         Recompute_View (Get_Kernel (Data));

      elsif Command = "remove_attribute_values" then
         Name_Parameters (Data, Remove_Attribute_Values_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3);
            Index          : constant String := Nth_Arg (Data, 4);
            Values         : GNAT.OS_Lib.Argument_List
              (1 .. Number_Of_Arguments (Data) - 4);
            Attribute      : constant Attribute_Pkg_List :=
              Build (Package_Name, Attribute_Name);
            List           : String_List_Access := Project.Attribute_Value
              (Attribute, Index);
            Found          : Boolean := False;
            First_Added    : Boolean := False;
         begin
            if not Is_Editable (Project) or else List = null then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               for J in 5 .. Number_Of_Arguments (Data) loop
                  Values (J - 4) := new String'(Nth_Arg (Data, J));
               end loop;

               Project.Delete_Attribute
                 (Attribute => Attribute,
                  Index     => Index);

               for J in reverse List'Range loop
                  Found := False;

                  for K in Values'Range loop
                     if List (J).all = Values (K).all then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Project.Set_Attribute
                       (Attribute => Attribute,
                        Values    => (1 => List (J)),
                        Index     => Index,
                        Prepend   => First_Added);

                     First_Added := True;
                  end if;
               end loop;

               for J in Values'Range loop
                  Free (Values (J));
               end loop;
            end if;

            Free (List);
         end;

         Recompute_View (Get_Kernel (Data));

      elsif Command = "clear_attribute_values" then
         Name_Parameters (Data, Clear_Attribute_Values_Parameters);
         declare
            Project        : constant Project_Type := Get_Data (Data, 1);
            Attribute_Name : constant String := Nth_Arg (Data, 2);
            Package_Name   : constant String := Nth_Arg (Data, 3, "");
            Index          : constant String := Nth_Arg (Data, 4, "");
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Project.Delete_Attribute
                 (Attribute => Attribute_Pkg_String'(Build
                    (Package_Name, Attribute_Name)),
                  Index     => Index);
            end if;
         end;

         Recompute_View (Get_Kernel (Data));
      end if;
   end Create_Project_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Properties_Module_ID := new Properties_Module_ID_Record (Kernel);
      Kernel.Register_Module (Abstract_Module (Properties_Module_ID));

      Register_Command
        (Kernel, "properties_editor",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);

      --  Redefine command to take into account attribute descriptions from
      --  projects.xml, which also provides the default values for attributes
      Override_Command
        (Kernel.Scripts, "get_attribute_as_string",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Override_Command
        (Kernel.Scripts, "get_attribute_as_list",
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);

      Register_Command
        (Kernel, "get_tool_switches_as_list",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "get_tool_switches_as_string",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "set_attribute_as_string",
         Minimum_Args => 4,
         Maximum_Args => 4,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "add_attribute_values",
         Minimum_Args => 4,
         Maximum_Args => Natural'Last,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_attribute_values",
         Minimum_Args => 4,
         Maximum_Args => Natural'Last,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "clear_attribute_values",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_Project_Class (Kernel),
         Handler      => Create_Project_Command_Handler'Access);
      Register_Command
        (Kernel, "is_modified",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => Get_Project_Class (Kernel),
         Handler       => Create_Project_Command_Handler'Access);
   end Register_Module;

   ------------------------
   -- Paths_Are_Relative --
   ------------------------

   function Paths_Are_Relative (Project : Project_Type) return Boolean is
   begin
      case Get_Paths_Type (Project) is
         when Relative  => return True;
         when Absolute  => return False;
         when From_Pref => return Generate_Relative_Paths.Get_Pref;
      end case;
   end Paths_Are_Relative;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor  : out Properties_Editor;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Read_Only : Boolean) is
   begin
      Editor := new Properties_Editor_Record;
      Initialize (Editor, Project, Kernel, Read_Only);
   end Gtk_New;

   ----------------------
   -- Editor_Destroyed --
   ----------------------

   procedure Editor_Destroyed
     (Editor : access Gtk_Widget_Record'Class;
      Attr   : Editable_Attribute_Description_Access)
   is
      pragma Unreferenced (Editor);
   begin
      Attr.Editor := null;
   end Editor_Destroyed;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self      : not null access General_Page_Record;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Read_Only : Boolean;
      Project   : Project_Type := No_Project)
   is
      pragma Unreferenced (Kernel);
      Button2   : Gtk_Button;
      Label     : Gtk_Label;
      Frame     : Gtk_Frame;
      Group     : Gtk_Size_Group;
      Box, Hbox : Gtk_Box;
      Event     : Gtk_Event_Box;

      use Gtk.Enums.String_List;

   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Group, Both);

      --  Name and location frame

      Gtk_New (Frame, -"Name & Location");
      Set_Border_Width (Frame, 5);
      Self.Pack_Start (Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      --  Name

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Name:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Add_Widget (Group, Label);
      Set_Tooltip_Text
        (Event,
         (-"Name of the project. ") &
         (-"Only applies to the project you selected initially"));

      Gtk_New (Self.Name);
      Self.Name.Set_Sensitive (not Read_Only);
      Set_Width_Chars (Self.Name, 0);
      Set_Text (Self.Name, Project.Name);
      Pack_Start (Hbox, Self.Name, Expand => True);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      --  Path

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Path:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Add_Widget (Group, Label);
      Set_Tooltip_Text
        (Event,
         -("Directory containing the project file. Changing this field"
           & " will move the project file. This field only applies to"
           & " the project you selected initially"));

      Gtk_New (Self.Path);
      Self.Path.Set_Sensitive (not Read_Only);
      Set_Width_Chars (Self.Path, 0);
      Set_Text (Self.Path, Display_Full_Name (Project_Directory (Project)));
      Pack_Start (Hbox, Self.Path, Expand => True);

      Gtk_New (Button2, -"Browse");
      Button2.Set_Sensitive (not Read_Only);
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, Gtk.Button.Signal_Clicked, Browse_Location'Access,
         Slot_Object => Self.Path);

      Gtk_New (Self.Use_Relative_Paths, -"Paths should be relative paths");
      Self.Use_Relative_Paths.Set_Sensitive (not Read_Only);
      Set_Active
        (Self.Use_Relative_Paths, Paths_Are_Relative (Project));
      Pack_Start (Box, Self.Use_Relative_Paths);
      Set_Tooltip_Text
        (Self.Use_Relative_Paths,
         -("If this field is activated, then all the path information in"
           & " the project (source and build directories, dependencies"
           & " between projects,...) will be stored as paths relative"
           & " to the location of the project file. It will thus be"
           & " easier to move the project file to another directory"));
   end Initialize;

   ------------------------------
   -- Select_Attribute_In_List --
   ------------------------------

   procedure Select_Attribute_In_List
     (Project     : Project_Type;
      Index_Pkg   : String;
      Index_Name  : String;
      Index_Value : String;
      Is_Selected : Boolean)
   is
      New_Iter : Gtk_Tree_Iter;
   begin
      --  Find all attributes with this one as an index. Add (or remove) new
      --  entries in their editing widget

      for P in Properties_Module_ID.Pages'Range loop
         for S in Properties_Module_ID.Pages (P).Sections'Range loop
            declare
               Sect : Attribute_Page_Section renames
                 Properties_Module_ID.Pages (P).Sections (S);
            begin
               for A in Sect.Attributes'Range loop
                  if Sect.Attributes (A).Indexed
                    and then Sect.Attributes (A).Index_Package.all = Index_Pkg
                    and then
                      Sect.Attributes (A).Index_Attribute.all = Index_Name
                  then
                     declare
                        Att  : constant Editable_Attribute_Description_Access
                          := Editable_Attribute_Description_Access
                             (Sect.Attributes (A));
                        Ed   : constant Indexed_Attribute_Editor :=
                          Indexed_Attribute_Editor (Att.Editor);
                     begin
                        if Ed /= null
                          and then Ed.Model /= null
                          and then Is_Selected
                        then
                           Append (Ed.Model, New_Iter, Null_Iter);
                           Set (Ed.Model, New_Iter, 0, Index_Value);
                           Set
                             (Ed.Model, New_Iter, 1,
                              Get_Value_From_Project
                                (Project       => Project,
                                 Attr          => Att,
                                 Index         => Index_Value));
                           Set (Ed.Model, New_Iter, 2,
                                Is_Any_String (Att, Index_Value));

                        elsif Ed /= null
                          and then Ed.Model /= null
                        then  --  Remove
                           New_Iter := Get_Iter_First (Ed.Model);
                           while New_Iter /= Null_Iter loop
                              if Get_String (Ed.Model, New_Iter, 0) =
                                Index_Value
                              then
                                 Remove (Ed.Model, New_Iter);
                                 exit;
                              end if;
                              Next (Ed.Model, New_Iter);
                           end loop;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end loop;
   end Select_Attribute_In_List;

   ----------------------------
   -- Attribute_List_Changed --
   ----------------------------

   procedure Attribute_List_Changed
     (Editor  : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues)
   is
      Ed       : constant List_Attribute_Editor :=
                   List_Attribute_Editor (Editor);
      Path     : constant String := Get_String (Nth (Params, 1));
      Iter     : constant Gtk_Tree_Iter :=
                   Get_Iter_From_String (Ed.Model, Path);
      Selected : constant Boolean := not Get_Boolean (Ed.Model, Iter, 1);
   begin
      Set (Ed.Model, Iter, 1, Selected);
      Select_Attribute_In_List
        (Index_Pkg   => Ed.Attribute.Pkg.all,
         Index_Name  => Ed.Attribute.Name.all,
         Project     => Ed.Project,
         Index_Value => Get_String (Ed.Model, Iter, 0),
         Is_Selected => Selected);
   end Attribute_List_Changed;

   ----------------------------------
   -- Create_List_Attribute_Editor --
   ----------------------------------

   function Create_List_Attribute_Editor
     (Kernel          : access Kernel_Handle_Record'Class;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Is_List         : Boolean) return List_Attribute_Editor
   is
      use Gtk.Enums.String_List;
      Editor     : List_Attribute_Editor;
      Scrolled   : Gtk_Scrolled_Window;
      View       : Gtk_Tree_View;
      Toggle     : Gtk_Cell_Renderer_Toggle;
      Text       : Gtk_Cell_Renderer_Text;
      Col        : Gtk_Tree_View_Column;
      Ignore     : Gint;
      pragma Unreferenced (Ignore);

      Current_Value : GNAT.Strings.String_List_Access;

      procedure Value_Cb (Value : String; Is_Default : Boolean);
      --  Called for each possible value of the attribute

      --------------
      -- Value_Cb --
      --------------

      procedure Value_Cb (Value : String; Is_Default : Boolean) is
         pragma Unreferenced (Is_Default);
         Iter     : Gtk_Tree_Iter;
         Selected : Boolean := False;
      begin
         if Is_List then
            Append (Editor.Model, Iter, Null_Iter);
            Set (Editor.Model, Iter, 0, Value);

            for C in Current_Value'Range loop
               if Equal (Value, Current_Value (C).all,
                         Case_Sensitive => Description.Case_Sensitive_Index)
               then
                  Selected := True;
                  exit;
               end if;
            end loop;

            Set (Editor.Model, Iter, 1, Selected);
         else
            Editor.Combo.Append_Text (Value);
         end if;
      end Value_Cb;

      Attr     : constant Attribute_Type :=
                   Get_Attribute_Type_From_Description
                     (Description, Attribute_Index);
      Editable : Boolean;

   begin
      Editor := new List_Attribute_Editor_Record;
      Initialize_Vbox (Editor, Homogeneous => False);

      Editor.Attribute := Description;
      Editor.Kernel    := Kernel_Handle (Kernel);
      Editor.Project   := Project;

      if Is_List then
         Gtk_New (Editor.Model,
           (0 => GType_String,    --  Attribute value
            1 => GType_Boolean)); --  Selected ?
      else
         if Attr.Typ = Attribute_As_Static_List then
            Editable := Attr.Static_Allows_Any_String;
         else
            Editable := Attr.Dynamic_Allows_Any_String;
         end if;

         if Editable then
            Gtk_New_With_Entry (Editor.Combo);
            Set_Activates_Default (Gtk_Entry (Editor.Combo.Get_Child), True);
         else
            Gtk_New (Editor.Combo);
         end if;
      end if;

      if Is_List then
         Current_Value := Get_Current_Value
           (Kernel  => Kernel,
            Project => Project,
            Attr    => Description,
            Index   => Attribute_Index);
         For_Each_Item_In_List (Kernel, Attr, Value_Cb'Unrestricted_Access);
         Free (Current_Value);

         Gtk_New (Scrolled);
         Pack_Start (Editor, Scrolled, Expand => True, Fill => True);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

         Gtk_New (View, Editor.Model);
         Set_Headers_Visible (View, False);
         Add (Scrolled, View);

         Gtk_New (Text);
         Gtk_New (Toggle);
         Widget_Callback.Object_Connect
           (Toggle, Signal_Toggled, Attribute_List_Changed'Access, Editor);

         Gtk_New (Col);
         Ignore := Append_Column (View, Col);
         Pack_Start (Col, Toggle, False);
         Add_Attribute (Col, Toggle, "active", 1);
         Set_Reorderable (Col, False);

         Pack_Start (Col, Text, True);
         Add_Attribute (Col, Text, "text", 0);

         if not Description.Ordered_List then
            Set_Sort_Column_Id (Col, 0);
            Clicked (Col);
         else
            Set_Reorderable (Col, False);
         end if;

      else
         For_Each_Item_In_List (Kernel, Attr, Value_Cb'Unrestricted_Access);

         Pack_Start (Editor, Editor.Combo, Expand => True, Fill => True);

         GUI_Utils.Add_Unique_Combo_Entry
           (Editor.Combo,
            Get_Current_Value
              (Project => Project,
               Attr    => Description,
               Index   => ""),
            Select_Text    => True,
            Case_Sensitive => Description.Case_Sensitive_Index);
      end if;

      return Editor;
   end Create_List_Attribute_Editor;

   ---------------------------------
   -- Select_Files_Or_Directories --
   ---------------------------------

   function Select_Files_Or_Directories
     (Toplevel       : access Gtk_Window_Record'Class;
      Project        : Project_Type;
      Default        : Filesystem_String;
      Project_Path   : Filesystem_String;
      As_Directory   : Boolean;
      Filter         : File_Filter;
      Allow_Multiple : Boolean := False) return GNATCOLL.VFS.File_Array
   is
      pragma Unreferenced (Allow_Multiple);
      Dialog   : Gtk_Dialog;
      Tree     : Gtk_Tree_View;
      Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Source   : Natural;
      File     : GNATCOLL.VFS.Virtual_File;
      Button   : Gtk_Widget;
      Ignore   : Gtk_Widget;
      pragma Unreferenced (Ignore);
      Prj      : Project_Type;
      Scrolled : Gtk_Scrolled_Window;
   begin
      if As_Directory then
         File := Select_Directory
           (Parent            => Gtk_Window (Toplevel),
            Use_Native_Dialog => Use_Native_Dialogs.Get_Pref);

         if File = GNATCOLL.VFS.No_File then
            return (1 .. 0 => GNATCOLL.VFS.No_File);
         else
            return (1 => File);
         end if;

      else
         --  Ignore the case where we don't know the list of sources yet
         if Filter /= Filter_None
           and then Direct_Sources_Count (Project) /= 0
         then
            Gtk_New
              (Dialog => Dialog,
               Title  => -"Select files",
               Parent => Gtk_Window (Toplevel),
               Flags  => Destroy_With_Parent);
            Set_Default_Size (Dialog, 500, 400);

            Gtk_New (Scrolled);
            Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
            Pack_Start (Get_Content_Area (Dialog), Scrolled, Expand => True);

            Tree := Create_Tree_View
              (Column_Types       => (0 => GType_Boolean,
                                      1 => GType_String,
                                      2 => GType_String),
               Column_Names       => (null, null),
               Show_Column_Titles => False,
               Initial_Sort_On    => 2);
            Add (Scrolled, Tree);
            Model := -Get_Model (Tree);

            case Filter is
               when Filter_None
                  | Filter_From_Project
                  | Filter_From_All_Projects
               =>
                  Prj := Project;

               when Filter_From_Extended =>
                  Prj := Extended_Project (Project);
            end case;

            while Prj /= GNATCOLL.Projects.No_Project loop
               declare
                  Sources : File_Array_Access :=
                    Prj.Source_Files (Filter = Filter_From_All_Projects);
                  Sort_Id : Gint;

               begin
                  Sort_Id := Freeze_Sort (Model);
                  for S in Sources'Range loop
                     Append (Model, Iter, Null_Iter);
                     Set (Model, Iter, 0, False);
                     Set (Model, Iter, 1, Sources (S).Display_Base_Name);
                     Set (Model, Iter, 2, Sources (S).Display_Full_Name);
                  end loop;
                  Thaw_Sort (Model, Sort_Id);
                  Unchecked_Free (Sources);
               end;

               --  If we had an extending project, look at the parent's sources
               --  as well
               Prj := Extended_Project (Prj);
            end loop;

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            if Run (Dialog) = Gtk_Response_OK then
               Source := 0;
               Iter := Get_Iter_First (Model);
               while Iter /= Null_Iter loop
                  if Get_Boolean (Model, Iter, 0) then
                     Source := Source + 1;
                  end if;

                  Next (Model, Iter);
               end loop;

               if Source = 0 then
                  Destroy (Dialog);
                  return (1 .. 0 => GNATCOLL.VFS.No_File);

               else
                  declare
                     Result : File_Array (1 .. Source);
                  begin
                     Source := Result'First;
                     Iter := Get_Iter_First (Model);
                     while Iter /= Null_Iter loop
                        if Get_Boolean (Model, Iter, 0) then
                           Result (Source) :=
                             Create (Full_Filename =>
                                     +Get_String (Model, Iter, 2));
                           --  ??? What if the filesystem path is non-UTF8?
                           Source := Source + 1;
                        end if;

                        Next (Model, Iter);
                     end loop;

                     Destroy (Dialog);
                     return Result;
                  end;
               end if;
            end if;

            Destroy (Dialog);
            return (1 .. 0 => GNATCOLL.VFS.No_File);

         else
            if Default = "" then
               --  Not set yet, the directory should default to the project
               --  directory (E617-011)
               File := Select_File
                 (Parent            => Gtk_Window (Toplevel),
                  Base_Directory    => Create (Project_Path),
                  Default_Name      => "",
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File);
            else
               declare
                  Def : constant Filesystem_String :=
                    Normalize_Pathname
                      (Default,
                       Directory => Project_Path,
                       Resolve_Links => False);
               begin
                  File := Select_File
                    (Parent            => Gtk_Window (Toplevel),
                     Base_Directory    => Create (Dir_Name (Def)),
                     Default_Name      => Base_Name (Def),
                     Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                     Kind              => Open_File);
               end;
            end if;

            if File = GNATCOLL.VFS.No_File then
               return (1 .. 0 => GNATCOLL.VFS.No_File);
            else
               return (1 => File);
            end if;
         end if;
      end if;
   end Select_Files_Or_Directories;

   -----------------
   -- Select_File --
   -----------------

   procedure Select_File
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed    : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      Files : constant GNATCOLL.VFS.File_Array :=
                Select_Files_Or_Directories
                  (Toplevel       => Gtk_Window (Get_Toplevel (Editor)),
                   Project        => Ed.Project,
                   Default        =>
                     +Histories.Most_Recent
                       (Hist => Ed.Kernel.Get_History,
                        Key  => History_Name (Ed.Attribute),
                        Default => Get_Safe_Text (Ed.Ent)),
                   Project_Path   => +Get_Safe_Text (Ed.Path_Widget),
                   --  ??? What if the filesystem path is non-UTF8?
                   As_Directory   => Ed.As_Directory,
                   Filter         => Ed.Filter,
                   Allow_Multiple => False);
   begin
      if Files'Length /= 0 then
         Set_Text (Ed.Ent, Display_Full_Name (Files (Files'First)));
         --  ??? What if the filesystem path is non-UTF8?

         Histories.Save_Text
           (Self => Ed.Ent,
            Hist => Ed.Kernel.Get_History,
            Key  => History_Name (Ed.Attribute));
      end if;
   end Select_File;

   ------------------------
   -- Add_String_In_List --
   ------------------------

   procedure Add_String_In_List (Editor : access Gtk_Widget_Record'Class) is
      Ed    : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      Value : GNAT.Strings.String_List :=
                Create_Attribute_Dialog
                  (Ed.Kernel,
                   Gtk_Window (Get_Toplevel (Editor)),
                   Ed.Project,
                   Ed.Attribute,
                   Attribute_Index => "",
                   Project_Path    => +Get_Safe_Text (Ed.Path_Widget));
      --  ??? What if the filesystem path is non-UTF8?
      Iter  : Gtk_Tree_Iter;
   begin
      for V in Value'Range loop
         Append (Ed.Model, Iter, Null_Iter);
         if Ed.As_Directory then
            Set (Ed.Model, Iter, 0, Normalize_Pathname
              (Value (V).all,
                 Directory     => Get_Safe_Text (Ed.Path_Widget),
                 Resolve_Links => False));

         elsif Ed.Attribute.Base_Name_Only then
            Set (Ed.Model, Iter, 0, Base_Name (Value (V).all));

         else
            Set (Ed.Model, Iter, 0, Value (V).all);
         end if;

         Set (Ed.Model, Iter, 1, False);
         Set (Ed.Model, Iter, 2, Value (V).all);

         if Ed.Attribute.Base_Name_Only then
            Select_Attribute_In_List
              (Project     => Ed.Project,
               Index_Pkg   => Ed.Attribute.Pkg.all,
               Index_Name  => Ed.Attribute.Name.all,
               Index_Value => Base_Name (Value (V).all),
               Is_Selected => True);
         else
            Select_Attribute_In_List
              (Project     => Ed.Project,
               Index_Pkg   => Ed.Attribute.Pkg.all,
               Index_Name  => Ed.Attribute.Name.all,
               Index_Value => Value (V).all,
               Is_Selected => True);
         end if;
      end loop;
      Free (Value);
   end Add_String_In_List;

   -----------------------------
   -- Remove_String_From_List --
   -----------------------------

   procedure Remove_String_From_List
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed   : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      M    : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Ed.View), M, Iter);
      if Iter /= Null_Iter then
         if Ed.Attribute.Base_Name_Only then
            Select_Attribute_In_List
              (Project     => Ed.Project,
               Index_Pkg   => Ed.Attribute.Pkg.all,
               Index_Name  => Ed.Attribute.Name.all,
               Index_Value => Get_String (Ed.Model, Iter, 0),
               Is_Selected => False);
         else
            Select_Attribute_In_List
              (Project     => Ed.Project,
               Index_Pkg   => Ed.Attribute.Pkg.all,
               Index_Name  => Ed.Attribute.Name.all,
               Index_Value => Get_String (Ed.Model, Iter, 0),
               Is_Selected => False);
         end if;

         Remove (Ed.Model, Iter);
      end if;
   end Remove_String_From_List;

   --------------------
   -- Move_String_Up --
   --------------------

   procedure Move_String_Up (Editor : access Gtk_Widget_Record'Class) is
      Ed          : constant File_Attribute_Editor :=
                      File_Attribute_Editor (Editor);
      M           : Gtk_Tree_Model;
      Iter, Iter2 : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
   begin
      Get_Selected (Get_Selection (Ed.View), M, Iter);
      if Iter /= Null_Iter then
         Path := Get_Path (Ed.Model, Iter);

         declare
            Value    : constant String := Get_String (Ed.Model, Iter, 0);
            Recurse  : constant Boolean := Get_Boolean (Ed.Model, Iter, 1);
            Relative : constant String := Get_String (Ed.Model, Iter, 2);
         begin
            if Prev (Path) then
               Remove (Ed.Model, Iter);
               Iter := Get_Iter (Ed.Model, Path);
               Insert_Before
                 (Ed.Model, Iter2, Parent => Parent (Ed.Model, Iter),
                  Sibling                 => Iter);
               Set (Ed.Model, Iter2, 0, Value);
               Set (Ed.Model, Iter2, 1, Recurse);
               Set (Ed.Model, Iter2, 2, Relative);
               Select_Iter (Get_Selection (Ed.View), Iter2);
            end if;
         end;

         Path_Free (Path);
      end if;
   end Move_String_Up;

   ----------------------
   -- Move_String_Down --
   ----------------------

   procedure Move_String_Down (Editor : access Gtk_Widget_Record'Class) is
      Ed          : constant File_Attribute_Editor :=
                      File_Attribute_Editor (Editor);
      M           : Gtk_Tree_Model;
      Iter, Iter2 : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Ed.View), M, Iter);
      if Iter /= Null_Iter then
         declare
            Value    : constant String := Get_String (Ed.Model, Iter, 0);
            Recurse  : constant Boolean := Get_Boolean (Ed.Model, Iter, 1);
            Relative : constant String := Get_String (Ed.Model, Iter, 2);
         begin
            Iter2 := Iter;
            Next (Ed.Model, Iter);

            if Iter /= Null_Iter then
               Remove (Ed.Model, Iter2);
               Insert_After
                 (Ed.Model, Iter2, Parent => Parent (Ed.Model, Iter),
                  Sibling                 => Iter);
               Select_Iter (Get_Selection (Ed.View), Iter2);
               Set (Ed.Model, Iter2, 0, Value);
               Set (Ed.Model, Iter2, 1, Recurse);
               Set (Ed.Model, Iter2, 2, Relative);
            end if;
         end;
      end if;
   end Move_String_Down;

   ---------------------------------
   -- Recursive_Directory_Changed --
   ---------------------------------

   procedure Recursive_Directory_Changed
     (Editor  : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues)
   is
      Ed       : constant File_Attribute_Editor :=
                   File_Attribute_Editor (Editor);
      Path     : constant String := Get_String (Nth (Params, 1));
      Iter     : constant Gtk_Tree_Iter :=
                   Get_Iter_From_String (Ed.Model, Path);
      Selected : constant Boolean := not Get_Boolean (Ed.Model, Iter, 1);
   begin
      Set (Ed.Model, Iter, 1, Selected);
   end Recursive_Directory_Changed;

   --------------------------
   -- Project_Path_Changed --
   --------------------------

   procedure Project_Path_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed        : constant File_Attribute_Editor :=
                    File_Attribute_Editor (Editor);
      Iter      : Gtk_Tree_Iter;
      Directory : constant String := Get_Safe_Text (Ed.Path_Widget);
   begin
      Iter := Get_Iter_First (Ed.Model);
      while Iter /= Null_Iter loop
         Set (Ed.Model, Iter, 0,
           Normalize_Pathname
             (Get_String (Ed.Model, Iter, 2),
              Directory     => Directory,
              Resolve_Links => False));
         Next (Ed.Model, Iter);
      end loop;
   end Project_Path_Changed;

   ------------------
   -- History_Name --
   ------------------

   function History_Name
     (Description : access Attribute_Description'Class) return History_Key
   is
   begin
      return "pp_"
        & History_Key (Description.Pkg.all & '_' & Description.Name.all);
   end History_Name;

   ----------------------------------
   -- Create_File_Attribute_Editor --
   ----------------------------------

   function Create_File_Attribute_Editor
     (Kernel          : access Kernel_Handle_Record'Class;
      Wiz             : Wizard;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Path_Widget     : Gtk_Entry;
      Is_List         : Boolean) return File_Attribute_Editor
   is
      Editor     : File_Attribute_Editor;
      Button     : Gtk_Button;
      Scrolled   : Gtk_Scrolled_Window;
      Box        : Gtk_Box;
      Text       : Gtk_Cell_Renderer_Text;
      Toggle     : Gtk_Cell_Renderer_Toggle;
      Col        : Gtk_Tree_View_Column;
      Arrow      : Gtk_Arrow;
      Ignore : Gint;
      pragma Unreferenced (Ignore);

      Attr       : constant Attribute_Type :=
                     Get_Attribute_Type_From_Description
                       (Description, Attribute_Index);
   begin
      Editor := new File_Attribute_Editor_Record;
      Initialize_Hbox (Editor, Homogeneous => False);
      Editor.Kernel       := Kernel_Handle (Kernel);
      Editor.As_Directory := Attr.Typ = Attribute_As_Directory;
      Editor.Filter       := Attr.Filter;
      Editor.Attribute    := Description;
      Editor.Path_Widget  := Path_Widget;
      Editor.Project      := Project;
      Editor.Use_History  :=
        not Is_List
        and then Attr.Typ = Attribute_As_Filename;

      Assert (Me, Editor.Path_Widget /= null, "No path widget given");

      if Is_List then
         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Editor, Scrolled, Expand => True, Fill => True);

         Gtk_New_Vbox (Box, Homogeneous => False);
         Pack_Start (Editor, Box, Expand => False);

         Gtk_New_From_Stock (Button, Stock_Add);
         Pack_Start (Box, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, Gtk.Button.Signal_Clicked, Add_String_In_List'Access,
            Slot_Object => Editor);

         Gtk_New_From_Stock (Button, Stock_Remove);
         Pack_Start (Box, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, Gtk.Button.Signal_Clicked, Remove_String_From_List'Access,
            Slot_Object => Editor);

         Gtk_New (Editor.Model, (0 => GType_String,   --  display name
                                 1 => GType_Boolean,  --  recursive ?
                                 2 => GType_String)); --  relative name

         Gtk_New (Editor.View, Editor.Model);
         Set_Headers_Visible (Editor.View, Editor.As_Directory);
         Add (Scrolled, Editor.View);

         Gtk_New (Text);
         Gtk_New (Col);
         Set_Resizable (Col, True);
         Set_Title (Col, -"Directory");
         Ignore := Append_Column (Editor.View, Col);
         Pack_Start (Col, Text, True);
         Add_Attribute (Col, Text, "text", 0);

         if Description.Ordered_List then
            Gtk_New (Button);
            Pack_Start (Box, Button, Expand => False);
            Gtk_New (Arrow, Arrow_Up, Shadow_None);
            Add (Button, Arrow);
            Widget_Callback.Object_Connect
              (Button, Gtk.Button.Signal_Clicked, Move_String_Up'Access,
               Slot_Object => Editor);

            Gtk_New (Button);
            Pack_Start (Box, Button, Expand => False);
            Gtk_New (Arrow, Arrow_Down, Shadow_None);
            Add (Button, Arrow);
            Widget_Callback.Object_Connect
              (Button, Gtk.Button.Signal_Clicked, Move_String_Down'Access,
               Slot_Object => Editor);
         else
            Set_Sort_Column_Id (Col, 0);
            Clicked (Col);
         end if;

         if Editor.As_Directory then
            Gtk_New (Toggle);
            Widget_Callback.Object_Connect
              (Toggle, Signal_Toggled,
               Recursive_Directory_Changed'Access, Editor);
            Gtk_New (Col);
            Set_Resizable (Col, True);
            Set_Title (Col, -"Include subdirectories");
            Ignore := Append_Column (Editor.View, Col);
            Pack_Start (Col, Toggle, False);
            Add_Attribute (Col, Toggle, "active", 1);

            Widget_Callback.Object_Connect
              (Path_Widget, Gtk.Editable.Signal_Changed,
               Project_Path_Changed'Access, Slot_Object => Editor);
         end if;

         declare
            Iter  : Gtk_Tree_Iter;
            Sort  : constant Gint := Freeze_Sort (Editor.Model);
            Value : String_List_Access :=
              Get_Current_Value
                (Kernel  => Kernel,
                 Project => Project,
                 Attr    => Description,
                 Index   => Attribute_Index);

         begin
            if Value /= null then
               for V in Value'Range loop
                  Append (Editor.Model, Iter, Null_Iter);

                  declare
                     Val : String renames Value (V).all;
                  begin
                     if Val'Length > 3
                       and then
                         (Val (Val'Last - 2 .. Val'Last) = "/**"
                          or else Val (Val'Last - 2 .. Val'Last) = "\**")
                     then
                        Set (Editor.Model, Iter, 0,
                          Normalize_Pathname
                            (Val (Val'First .. Val'Last - 3),
                             Directory     => Get_Safe_Text (Path_Widget),
                             Resolve_Links => False));
                        Set (Editor.Model, Iter, 1, True);

                     elsif Attr.Typ = Attribute_As_String
                       or else Attr.Typ = Attribute_As_Unit
                     then
                        Set (Editor.Model, Iter, 0, Val);
                        Set (Editor.Model, Iter, 1, False);

                     elsif Description.Base_Name_Only then
                        Set (Editor.Model, Iter, 0, Base_Name (Val));
                        Set (Editor.Model, Iter, 1, False);

                     else
                        Set
                          (Editor.Model, Iter, 0,
                           Normalize_Pathname
                             (Val,
                              Directory     => Get_Safe_Text (Path_Widget),
                              Resolve_Links => False));
                        Set (Editor.Model, Iter, 1, False);
                     end if;

                     Set (Editor.Model, Iter, 2, Val);
                  end;
               end loop;
               Free (Value);
            end if;

            Thaw_Sort (Editor.Model, Sort);
         end;

      else
         Gtk_New (Editor.Ent);

         if Wiz /= null then
            Widget_Callback.Object_Connect
              (Editor.Ent, Gtk.Editable.Signal_Changed,
               Update_Buttons_Sensitivity'Access, Wiz);
         end if;

         Set_Activates_Default (Editor.Ent, True);

         declare
            Current : constant String := Get_Current_Value
              (Project => Project,
               Attr    => Description,
               Index   => Attribute_Index);
         begin
            if Attr.Typ = Attribute_As_String
              or else Attr.Typ = Attribute_As_Unit
              or else Current = ""
            then
               Set_Text (Editor.Ent, Current);

            elsif Description.Base_Name_Only then
               Set_Text (Editor.Ent, Base_Name (Current));

            elsif Current (Current'First) = '(' then
               Set_Text (Editor.Ent, Current);

            else
               --  Normalization will take place when the project is generated.
               --  In particular, the final project path is not known till then
               Set_Text (Editor.Ent, Current);
            end if;
         end;

         Pack_Start (Editor, Editor.Ent, Expand => True);

         if Attr.Typ /= Attribute_As_String then
            Gtk_New (Button, -"Browse");
            Widget_Callback.Object_Connect
              (Button, Gtk.Button.Signal_Clicked, Select_File'Access,
               Slot_Object => Editor);
            Pack_Start (Editor, Button, Expand => False);
         end if;
      end if;

      return Editor;
   end Create_File_Attribute_Editor;

   -----------------------------
   -- Create_Attribute_Dialog --
   -----------------------------

   function Create_Attribute_Dialog
     (Kernel          : access Kernel_Handle_Record'Class;
      Toplevel        : access Gtk.Window.Gtk_Window_Record'Class;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Project_Path    : Filesystem_String) return GNAT.Strings.String_List
   is
      Unit_Name : aliased String := -"Unit name";

      Attr   : constant Attribute_Type :=
                 Get_Attribute_Type_From_Description
                   (Description, Attribute_Index);
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Ignore : Gtk_Widget;
      pragma Unreferenced (Ignore);
      Ent    : Gtk_Entry;
      Model  : Gtk_Tree_Store;
      View   : Gtk_Tree_View;
      Scrolled : Gtk_Scrolled_Window;
      Selection : Gtk_Tree_Selection;
      W      : List_Attribute_Editor;
      Iter   : Gtk_Tree_Iter;

   begin
      case Attr.Typ is
         when Attribute_As_String =>
            Gtk_New (Dialog,
                     Title  => -"Enter new value",
                     Parent => Gtk_Window (Toplevel),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Ent);
            Set_Text (Ent, Get_Current_Value
                      (Project, Description, Index => Attribute_Index));
            Set_Activates_Default (Ent, True);
            Pack_Start
              (Get_Content_Area (Dialog), Ent, Expand => True, Fill => True);

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            case Run (Dialog) is
               when Gtk_Response_OK =>
                  declare
                     S : constant String := Get_Safe_Text (Ent);
                  begin
                     Destroy (Dialog);
                     return (1 => new String'(S));
                  end;
               when others =>
                  Destroy (Dialog);
                  return (1 .. 0 => null);
            end case;

         when Attribute_As_Unit =>
            Gtk_New (Dialog,
                     Title  => -"Select units to add",
                     Parent => Gtk_Window (Toplevel),
                     Flags  => Modal or Destroy_With_Parent);
            Dialog.Set_Size_Request (600, 400);

            Gtk_New (Scrolled);
            Dialog.Get_Content_Area.Pack_Start (Scrolled, True, True);

            View := Create_Tree_View
              (Column_Types    => (1 => GType_String),
               Column_Names    => (1 => Unit_Name'Unchecked_Access),
               Selection_Mode  => Gtk.Enums.Selection_Multiple,
               Initial_Sort_On => 1);
            Scrolled.Add (View);
            Model := -View.Get_Model;
            Selection := View.Get_Selection;

            declare
               Sort  : constant Gint := Model.Freeze_Sort;
               Tree  : constant Project_Tree_Access :=
                 Get_Registry (Kernel).Tree;
               Current : GNAT.Strings.String_List_Access :=
                 Get_Current_Value
                   (Kernel, Project, Description, Index => Attribute_Index);
               Files : File_Array_Access :=
                 Project.Source_Files (Recursive => False);
               Info  : File_Info_Set;
               Found : Boolean;
            begin
               for F in Files'Range loop
                  Info := Tree.Info_Set (Files (F));
                  declare
                     F_Info : constant File_Info'Class :=
                       File_Info'Class (Info.First_Element);
                  begin
                     if F_Info.Unit_Part = Unit_Spec then
                        --  This is the <<add>> dialog, so we only want to show
                        --  those units that are not already in the list.
                        --  ??? Not very efficient

                        Found := False;

                        for C in Current'Range loop
                           if Current (C).all = F_Info.Unit_Name then
                              Found := True;
                              exit;
                           end if;
                        end loop;

                        if not Found then
                           Model.Append (Iter, Null_Iter);
                           Model.Set (Iter, 0, F_Info.Unit_Name);
                        end if;
                     end if;
                  end;
               end loop;

               Unchecked_Free (Files);
               Free (Current);
               Model.Thaw_Sort (Sort);
            end;

            Button := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Ignore := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
            Show_All (Dialog);

            case Run (Dialog) is
               when Gtk_Response_OK =>
                  declare
                     Result : GNAT.Strings.String_List
                       (1 .. Integer (Selection.Count_Selected_Rows));
                     Index : Integer := Result'First;
                  begin
                     Iter := Model.Get_Iter_First;
                     while Iter /= Null_Iter loop
                        if Selection.Iter_Is_Selected (Iter) then
                           Result (Index) := new String'
                             (Model.Get_String (Iter, 0));
                           Index := Index + 1;
                        end if;

                        Model.Next (Iter);
                     end loop;

                     Destroy (Dialog);
                     return Result;
                  end;

               when others =>
                  Destroy (Dialog);
                  return (1 .. 0 => null);
            end case;

         when Attribute_As_Filename | Attribute_As_Directory =>
            declare
               Current : constant Filesystem_String := +Get_Current_Value
                 (Project, Description, Index => Attribute_Index);
               Files   : constant File_Array := Select_Files_Or_Directories
                 (Toplevel          => Toplevel,
                  Project           => Project,
                  Default           => Current,
                  Project_Path      => Project_Path,
                  As_Directory      => Attr.Typ = Attribute_As_Directory,
                  Filter            => Attr.Filter,
                  Allow_Multiple    => False);
               Result  : GNAT.Strings.String_List (Files'Range);
            begin
               for F in Files'Range loop
                  Result (F) :=
                    new String'(String (Full_Name (Files (F)).all));
               end loop;
               return Result;
            end;

         when Attribute_As_Static_List | Attribute_As_Dynamic_List =>
            Gtk_New (Dialog,
                     Title  => -"Select new value",
                     Parent => Gtk_Window (Toplevel),
                     Flags  => Modal or Destroy_With_Parent);
            W := Create_List_Attribute_Editor
              (Kernel, Project, Description, Attribute_Index,
               Is_List => False);
            Set_Active_Text
              (W.Combo,
               Get_Current_Value
                 (Project, Description, Index => Attribute_Index),
               Case_Sensitive => Description.Case_Sensitive_Index);
            Pack_Start
              (Get_Content_Area (Dialog), W, Expand => True, Fill => True);

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            case Run (Dialog) is
               when Gtk_Response_OK =>
                  declare
                     S : constant String := Get_Active_Text (W.Combo);
                  begin
                     Destroy (Dialog);
                     return (1 .. 1 => new String'(S));
                  end;
               when others =>
                  Destroy (Dialog);
                  return (1 .. 0 => null);
            end case;
      end case;
   end Create_Attribute_Dialog;

   -----------------------------
   -- Create_Widget_Attribute --
   -----------------------------

   function Create_Widget_Attribute
     (Kernel          : access Kernel_Handle_Record'Class;
      Wiz             : Wizard;
      Project         : Project_Type;
      Description     : Editable_Attribute_Description_Access;
      Attribute_Index : String;
      Path_Widget     : Gtk_Entry;
      Is_List         : Boolean) return Attribute_Editor
   is
      Attr : constant Attribute_Type :=
               Get_Attribute_Type_From_Description
                 (Description, Attribute_Index);
   begin
      case Attr.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Unit
            | Attribute_As_Directory =>
            return Attribute_Editor
              (Create_File_Attribute_Editor
                 (Kernel, Wiz, Project, Description, Attribute_Index,
                  Path_Widget, Is_List));

         when Attribute_As_Static_List
            | Attribute_As_Dynamic_List =>
            return Attribute_Editor
              (Create_List_Attribute_Editor
                 (Kernel, Project, Description, Attribute_Index, Is_List));
      end case;
   end Create_Widget_Attribute;

   -------------------------
   -- Get_Value_As_String --
   -------------------------

   overriding function Get_Value_As_String
     (Editor          : access File_Attribute_Editor_Record;
      Attribute_Index : String := "") return String
   is
      pragma Unreferenced (Attribute_Index);
      M    : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;
   begin
      if Editor.Ent /= null then
         return Get_Safe_Text (Editor.Ent);
      else
         Get_Selected (Get_Selection (Editor.View), M, Iter);
         if Iter = Null_Iter then
            Iter := Get_Iter_First (Editor.Model);
         end if;

         if Iter /= Null_Iter then
            return Get_String (Editor.Model, Iter, 0);

         else
            return "";
         end if;
      end if;
   end Get_Value_As_String;

   -------------------------
   -- Get_Value_As_String --
   -------------------------

   overriding function Get_Value_As_String
     (Editor          : access List_Attribute_Editor_Record;
      Attribute_Index : String := "") return String
   is
      pragma Unreferenced (Attribute_Index);
   begin
      return Get_Active_Text (Editor.Combo);
   end Get_Value_As_String;

   -------------------------
   -- Get_Value_As_String --
   -------------------------

   overriding function Get_Value_As_String
     (Editor          : access Indexed_Attribute_Editor_Record;
      Attribute_Index : String := "") return String
   is
      Iter : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
   begin
      while Iter /= Null_Iter loop
         if Equal (Get_String (Editor.Model, Iter, 0),
                   Attribute_Index,
                   Case_Sensitive => Editor.Attribute.Case_Sensitive_Index)
         then
            return Get_String (Editor.Model, Iter, 1);
         end if;
         Next (Editor.Model, Iter);
      end loop;

      return "";
   end Get_Value_As_String;

   -----------------------
   -- Get_Value_As_List --
   -----------------------

   overriding function Get_Value_As_List
     (Editor          : access File_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.Strings.String_List
   is
      pragma Unreferenced (Attribute_Index);
      Count  : constant Integer := Integer (N_Children (Editor.Model));
      Result : GNAT.Strings.String_List (1 .. Count);
      Iter   : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
      Index  : Natural := Result'First;
   begin
      while Iter /= Null_Iter loop
         Result (Index) := new String'(Get_String (Editor.Model, Iter, 0));
         Index := Index + 1;
         Next (Editor.Model, Iter);
      end loop;

      return Result;
   end Get_Value_As_List;

   -----------------------
   -- Get_Value_As_List --
   -----------------------

   overriding function Get_Value_As_List
     (Editor          : access List_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.Strings.String_List
   is
      pragma Unreferenced (Attribute_Index);
      Count  : constant Integer := Integer (N_Children (Editor.Model));
      Result : GNAT.Strings.String_List (1 .. Count);
      Iter   : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
      Index  : Natural := Result'First;
   begin
      while Iter /= Null_Iter loop
         if Get_Boolean (Editor.Model, Iter, 1) then
            Result (Index) := new String'(Get_String (Editor.Model, Iter, 0));
            Index := Index + 1;
         end if;
         Next (Editor.Model, Iter);
      end loop;

      return Result (1 .. Index - 1);
   end Get_Value_As_List;

   -----------------------
   -- Get_Value_As_List --
   -----------------------

   overriding function Get_Value_As_List
     (Editor          : access Indexed_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.Strings.String_List
   is
   begin
      if Editor.Current_Values /= null then
         for C in Editor.Current_Values'Range loop
            if Equal (Editor.Current_Values (C).Index.all,
                      Attribute_Index,
                      Case_Sensitive => Editor.Attribute.Case_Sensitive_Index)
            then
               declare
                  V : GNAT.Strings.String_List
                    (Editor.Current_Values (C).Values'Range);
               begin
                  for Val in Editor.Current_Values (C).Values'Range loop
                     V (Val) := new String'
                       (Editor.Current_Values (C).Values (Val).all);
                  end loop;
                  return V;
               end;
            end if;
         end loop;
      end if;
      return GNAT.Strings.String_List'(1 .. 0 => null);
   end Get_Value_As_List;

   ----------------------------------
   -- Get_Attribute_Type_From_Name --
   ----------------------------------

   function Get_Attribute_Type_From_Name
     (Pkg : String; Name : String)
      return Editable_Attribute_Description_Access
   is
      Result : constant Attribute_Description_Access :=
        Properties_Module_ID.Get_Attribute_Type_From_Name (Pkg, Name);
   begin
      return Editable_Attribute_Description_Access (Result);
   end Get_Attribute_Type_From_Name;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Project       : Project_Type;
      Attr          : Editable_Attribute_Description_Access;
      Index         : String) return String
   is
      Lower_Attribute_Index : String := Index;
   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      --  First choice: if the editor is being edited, use that value
      if Attr.Editor /= null then
         return Get_Value_As_String (Attr.Editor, Lower_Attribute_Index);
      end if;

      --  Otherwise, we'll have to look in the project, or use the default
      --  value if the attribute hasn't been specified otherwise.
      return Get_Value_From_Project (Project, Attr, Index);
   end Get_Current_Value;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Kernel : access Kernel_Handle_Record'Class;
      Pkg    : String;
      Name   : String;
      Index  : String := "") return GNAT.Strings.String_List_Access
   is
      Attr : constant Editable_Attribute_Description_Access :=
        Get_Attribute_Type_From_Name (Pkg, Name);
   begin
      return Get_Current_Value
        (Kernel  => Kernel,
         Project => GNATCOLL.Projects.No_Project,
         Attr    => Attr,
         Index   => Index);
   end Get_Current_Value;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Kernel        : access Core_Kernel_Record'Class;
      Project       : Project_Type;
      Attr          : Editable_Attribute_Description_Access;
      Index         : String := "") return GNAT.Strings.String_List_Access
   is
      Lower_Attribute_Index : String := Index;
   begin
      if not Attr.Case_Sensitive_Index then
         To_Lower (Lower_Attribute_Index);
      end if;

      --  First choice: if the attribute is being edited, use that value
      if Attr.Editor /= null then
         return new GNAT.Strings.String_List'
           (Get_Value_As_List (Attr.Editor, Lower_Attribute_Index));
      else
         return Get_Value_From_Project (Kernel, Project, Attr, Index);
      end if;
   end Get_Current_Value;

   ----------------------------
   -- Edit_Indexed_Attribute --
   ----------------------------

   function Edit_Indexed_Attribute
     (Editor : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      Ed             : constant Indexed_Attribute_Editor :=
                         Indexed_Attribute_Editor (Editor);
      Path           : Gtk_Tree_Path;
      Column         : Gtk_Tree_View_Column;
      Cell_X, Cell_Y : Gint;
      Row_Found      : Boolean;
      Iter           : Gtk_Tree_Iter;
      Value_Ed       : Attribute_Editor;
      Dialog         : Gtk_Dialog;
      Button         : Gtk_Widget;
      Ignore         : Gtk_Widget;
      pragma Unreferenced (Ignore);
      Typ            : Attribute_Type;
      X, Y           : Gdouble;
   begin
      Get_Coords (Event, X, Y);

      Get_Path_At_Pos
        (Ed.View, Gint (X), Gint (Y),
         Path, Column, Cell_X, Cell_Y, Row_Found);

      if Row_Found
        and then Get_Button (Event) = 1
        and then (Get_Event_Type (Event) = Gdk_2button_Press
                  or else
                    (Get_Event_Type (Event) = Button_Press
                     and then Path_Is_Selected
                       (Get_Selection (Ed.View), Path)))
      then
         Iter := Get_Iter (Ed.Model, Path);

         declare
            Attribute_Index : constant String :=
              Get_String (Ed.Model, Iter, 0);
         begin
            Typ := Get_Attribute_Type_From_Description
              (Ed.Attribute, Index => Attribute_Index);

            if Ed.Attribute.Is_List then
               Gtk_New (Dialog,
                        Title  => -"Enter new value",
                        Parent => Gtk_Window (Get_Toplevel (Editor)),
                        Flags  => Modal or Destroy_With_Parent);
               Value_Ed := Create_Widget_Attribute
                 (Kernel          => Ed.Kernel,
                  Wiz             => Ed.Wiz,
                  Project         => Ed.Project,
                  Description     => Ed.Attribute,
                  Attribute_Index => Attribute_Index,
                  Path_Widget     => Ed.Path_Widget,
                  Is_List         => True);
               Pack_Start (Get_Content_Area (Dialog), Value_Ed,
                           Expand => True, Fill => True);
               Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
               Grab_Default (Button);
               Ignore := Add_Button
                 (Dialog, Stock_Cancel, Gtk_Response_Cancel);

               Show_All (Dialog);

               case Run (Dialog) is
                  when Gtk_Response_OK =>
                     for C in Ed.Current_Values'Range loop
                        if Ed.Current_Values (C).Index.all =
                          Attribute_Index
                        then
                           Free (Ed.Current_Values (C).Values);
                           Ed.Current_Values (C).Values :=
                             new GNAT.Strings.String_List'
                               (Get_Value_As_List (Value_Ed, ""));
                           Set (Ed.Model, Iter, 1,
                                To_String
                                  (Ed.Current_Values (C).Values.all));
                        end if;
                     end loop;
                     Destroy (Dialog);
                  when others =>
                     Destroy (Dialog);
               end case;

            else
               --  No need to open a dialog to edit simple string, this is
               --  done in-line
               if Typ.Typ /= Attribute_As_String then
                  declare
                     Value : GNAT.Strings.String_List :=
                       Create_Attribute_Dialog
                         (Kernel          => Ed.Kernel,
                          Toplevel        =>
                            Gtk_Window (Get_Toplevel (Ed)),
                          Project         => Ed.Project,
                          Description     => Ed.Attribute,
                          Attribute_Index => Attribute_Index,
                          Project_Path    =>
                          +Get_Safe_Text (Ed.Path_Widget));
                     --  ??? What if the filesystem path is non-UTF8?
                  begin
                     if Value'Length /= 0 then
                        Set (Ed.Model, Iter, 1, Value (Value'First).all);
                        Free (Value);
                     end if;
                  end;
               else
                  Column := Get_Column (Ed.View, 1);
                  Set_Cursor (Ed.View, Path, Column, Start_Editing => True);
               end if;
            end if;
         end;

         Path_Free (Path);
         return True;
      end if;
      return False;
   end Edit_Indexed_Attribute;

   ---------------
   -- To_String --
   ---------------

   function To_String (List : GNAT.Strings.String_List) return String is
      Str : Unbounded_String;
   begin
      for C in List'Range loop
         Str := Str & List (C).all;
         if C /= List'Last then
            Str := Str & ',';
         end if;
      end loop;

      return To_String (Str);
   end To_String;

   -------------------------------------
   -- Create_Indexed_Attribute_Editor --
   -------------------------------------

   function Create_Indexed_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Attr        : Editable_Attribute_Description_Access;
      Path_Widget : Gtk_Entry) return Indexed_Attribute_Editor
   is
      Index_Col     : constant := 0;
      Attribute_Col : constant := 1;
      Editable_Col  : constant := 2;

      use Gtk.Enums.String_List;
      Ed            : Indexed_Attribute_Editor;
      Text          : Gtk_Cell_Renderer_Text;
      Col           : Gtk_Tree_View_Column;
      Scrolled      : Gtk_Scrolled_Window;
      Ignore        : Gint;
      pragma Unreferenced (Ignore);
      Index         : constant Editable_Attribute_Description_Access :=
        Get_Attribute_Type_From_Name
                          (Pkg  => Attr.Index_Package.all,
                           Name => Attr.Index_Attribute.all);
      Current_Index : String_List_Access;

      procedure Value_Cb (Value : String; Selected : Boolean);
      --  Called for each possible value of the attribute

      --------------
      -- Value_Cb --
      --------------

      procedure Value_Cb (Value : String; Selected : Boolean) is
         pragma Unreferenced (Selected);
         Iter    : Gtk_Tree_Iter;
         Matched : Boolean;
      begin
         for C in Current_Index'Range loop
            Matched := Equal (Current_Index (C).all, Value,
                              Case_Sensitive => Attr.Case_Sensitive_Index);
            if Matched then
               Append (Ed.Model, Iter, Null_Iter);
               Set (Ed.Model, Iter, Index_Col, Value);
               Set (Ed.Model, Iter, Editable_Col,
                    not Attr.Is_List and then Is_Any_String (Attr, Value));

               if Attr.Is_List then
                  declare
                     Current : String_List_Access :=
                       Get_Current_Value
                         (Kernel   => Kernel,
                          Project  => Project,
                          Attr     => Attr,
                          Index    => Value);
                     Tmp     : Indexed_Values_Array_Access;
                  begin
                     if Current = null then
                        Current := new GNAT.OS_Lib.String_List'
                          (1 .. 0 => null);
                     end if;

                     Set
                       (Ed.Model, Iter, Attribute_Col,
                        To_String (Current.all));

                     Tmp := Ed.Current_Values;
                     if Tmp /= null then
                        Ed.Current_Values := new Indexed_Values_Array
                          (1 .. Ed.Current_Values'Length + 1);
                        Ed.Current_Values (Tmp'Range) := Tmp.all;
                        Unchecked_Free (Tmp);
                     else
                        Ed.Current_Values := new Indexed_Values_Array (1 .. 1);
                     end if;

                     Ed.Current_Values (Ed.Current_Values'Last) :=
                       (Index  => new String'(Value),
                        Values => Current);
                  end;
               else
                  Set (Ed.Model, Iter, Attribute_Col,
                       Get_Current_Value
                         (Project => Project, Attr  => Attr, Index => Value));
               end if;
               exit;
            end if;
         end loop;
      end Value_Cb;

   begin
      if Index = null then
         return null;
      end if;

      Ed := new Indexed_Attribute_Editor_Record;
      Initialize_Vbox (Ed, Homogeneous => True);

      Widget_Callback.Connect
        (Ed, Signal_Destroy, On_Indexed_Editor_Destroy'Access);

      Gtk_New (Scrolled);
      Pack_Start (Ed, Scrolled, Expand => True, Fill => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Ed.Kernel      := Kernel_Handle (Kernel);
      Ed.Attribute   := Attr;
      Ed.Project     := Project;
      Ed.Path_Widget := Path_Widget;

      Gtk_New (Ed.Model,
        (Index_Col      => GType_String,
         Attribute_Col  => GType_String,
         Editable_Col   => GType_Boolean));

      declare
         Current_Value : String_List_Access :=
           Get_Current_Value (Kernel, Project, Index);
      begin
         Current_Index := Current_Value;

         case Index.Non_Index_Type.Typ is
            when Attribute_As_Static_List | Attribute_As_Dynamic_List =>
               For_Each_Item_In_List
                 (Kernel, Index.Non_Index_Type, Value_Cb'Unrestricted_Access);
            when others =>
               if not Index.Is_List then
                  Insert (Kernel,
                          -"Index for project attribute """
                          & Attribute_Name (Attr.all)
                          & """ must be a list",
                          Mode => Error);
               else
                  for C in Current_Value'Range loop
                     Value_Cb (Current_Value (C).all, True);
                  end loop;
               end if;
         end case;
         Free (Current_Value);
      end;

      Gtk_New (Ed.View, Ed.Model);
      Add (Scrolled, Ed.View);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Ed.View, Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Edit_Indexed_Attribute'Access),
         Slot_Object => Ed);

      Gtk_New (Text);

      Gtk_New (Col);
      Set_Resizable (Col, True);

      if Index.Label = null then
         Set_Title (Col, Index.Name.all);
      else
         Set_Title (Col, Index.Label.all);
      end if;

      Ignore := Append_Column (Ed.View, Col);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "text", Index_Col);

      if Attr.Ordered_List then
         Set_Reorderable (Col, False);

      else
         Set_Sort_Column_Id (Col, Index_Col);
         Clicked (Col);
         Set_Sort_Column_Id (Col, -1);
      end if;

      Gtk_New (Col);
      Set_Resizable (Col, True);

      if Attr.Label /= null then
         Set_Title (Col, Attr.Label.all & (-" (Click to edit)"));
      else
         Set_Title (Col, -"(Click to edit)");
      end if;

      Ignore := Append_Column (Ed.View, Col);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "text", Attribute_Col);
      Add_Attribute (Col, Text, "editable", Editable_Col);

      Set_Editable_And_Callback (Ed.Model, Text, Attribute_Col);

      return Ed;
   end Create_Indexed_Attribute_Editor;

   -----------------------------
   -- Create_Widget_Attribute --
   -----------------------------

   procedure Create_Widget_Attribute
     (Kernel      : access Kernel_Handle_Record'Class;
      Wiz         : Wizard;
      Project     : Project_Type;
      Attr        : Editable_Attribute_Description_Access;
      Size_Group  : in out Gtk_Size_Group;
      Read_Only   : Boolean;
      Path_Widget : Gtk_Entry;
      Widget      : out Gtk_Widget;
      Expandable  : out Boolean;
      Context     : String)
   is
      Label       : Gtk_Label;
      Box         : Gtk_Box;
      Vbox        : Gtk_Box;
      Align       : Gtk_Alignment;
      Event       : Gtk_Event_Box;
      Check       : Gtk_Check_Button;
      First, Last : Natural;
      Exists      : Boolean := True;
   begin
      --  Should the page be displayed ?
      if Attr.Hide_In /= null then
         First := Attr.Hide_In'First;

         loop
            while First <= Attr.Hide_In'Last
              and then Attr.Hide_In (First) = ' '
            loop
               First := First + 1;
            end loop;

            Last := First + 1;
            while Last <= Attr.Hide_In'Last
              and then Attr.Hide_In (Last) /= ' '
            loop
               Last := Last + 1;
            end loop;

            if First <= Attr.Hide_In'Last
              and then
                (Attr.Hide_In (First .. Last - 1) = Context
                   or else Attr.Hide_In (First .. Last - 1) = "all")
            then
               Widget := null;
               Expandable := False;
               return;
            end if;

            First := Last + 1;
            exit when First > Attr.Hide_In'Last;
         end loop;
      end if;

      --  We also have an implicit filter for extended projects

      if not Attr.Indexed
        and then Attr.Non_Index_Type.Typ = Attribute_As_Filename
        and then Attr.Non_Index_Type.Filter = Filter_From_Extended
        and then
          (Project = No_Project
           or else Extended_Project (Project) = GNATCOLL.Projects.No_Project)
      then
         Widget := null;
         Expandable := False;
         return;
      end if;

      --  Prepare the display of the page

      Gtk_New_Hbox (Box, Homogeneous => False);

      if Attr.Label /= null or else Attr.Disable_If_Not_Set then
         Gtk_New_Hbox (Vbox, Homogeneous => False);
         Pack_Start (Box, Vbox, Expand => False, Fill => False);
      end if;

      if Attr.Disable_If_Not_Set then
         Exists := Attribute_Exists (Attr, Project, Attribute_Index => "");

         Gtk_New (Align, Xalign => 0.0, Yalign => 0.5,
                  Xscale        => 0.0, Yscale => 0.0);
         Set_Border_Width (Align, 0);
         Pack_Start (Vbox, Align, Expand => False, Fill => False);
         Gtk_New (Check, "");
         Check.Set_Sensitive (not Read_Only);
         Add (Align, Check);
         Set_Active (Check, Exists);
         Attribute_Handler.Connect
           (Check, Signal_Toggled, Toggle_Sensitive'Access, Attr);
      end if;

      if Attr.Label /= null then
         --  Put the label inside an event box, so that we can associate it
         --  with a tooltip. Put the event box inside the box, so that the
         --  size of the event box is just that of the label, and the tooltip
         --  doesn't appear far from the label

         Gtk_New (Align, Xalign => 0.0, Yalign => 0.5,
                  Xscale        => 0.0, Yscale => 0.0);
         Set_Border_Width (Align, 0);
         Pack_Start (Vbox, Align, Expand => False, Fill => False);
         Gtk_New (Event);
         Add (Align, Event);
         Gtk_New (Label, Attr.Label.all & ':');

         if Size_Group = null then
            Gtk_New (Size_Group);
         end if;

         Add_Widget (Size_Group, Align);
         Add (Event, Label);

         if Attr.Description /= null
           and then Attr.Description.all /= ""
         then
            Set_Tooltip_Text (Event, Attr.Description.all);
         end if;
      end if;

      if Attr.Indexed then
         Attr.Editor := Attribute_Editor
           (Create_Indexed_Attribute_Editor
              (Kernel, Project, Attr, Path_Widget => Path_Widget));
      else
         Attr.Editor := Create_Widget_Attribute
           (Kernel,
            Wiz,
            Project,
            Attr,
            Attribute_Index => "",
            Path_Widget     => Path_Widget,
            Is_List         => Attr.Is_List);
      end if;

      Attr.Editor.Wiz := Wiz;

      Attribute_Handler.Connect
        (Attr.Editor, Signal_Destroy, Editor_Destroyed'Access, Attr);

      Pack_Start (Box, Attr.Editor, Expand => True, Fill => True);
      if Attr.Description /= null
        and then Attr.Description.all /= ""
      then
         Set_Tooltip_Text (Attr.Editor, Attr.Description.all);
      end if;

      Attr.Editor.Active_Check := Check;
      Attr.Editor.Set_Sensitive (Exists and then not Read_Only);

      Widget := Gtk_Widget (Box);
      Expandable := Attr.Indexed or else Attr.Is_List;
   end Create_Widget_Attribute;

   ----------------------
   -- Toggle_Sensitive --
   ----------------------

   procedure Toggle_Sensitive
     (Check : access Gtk_Widget_Record'Class;
      Attr  : Editable_Attribute_Description_Access)
   is
      Active     : constant Boolean :=
        Get_Active
          (Gtk_Check_Button (Check));
      Attr2      : Editable_Attribute_Description_Access;
      Page       : Attribute_Page;
      Pkg_Start  : Natural;
      Pkg_End    : Natural;
      Name_Start : Natural;
      Index      : Natural;
   begin
      if Attr.Editor /= null
        and then Active /= Is_Sensitive (Attr.Editor)
      then
         Set_Sensitive (Attr.Editor, Active);

         if Active and then Attr.Disable /= null then
            Index := Attr.Disable'First;
            while Index <= Attr.Disable'Last loop
               Pkg_Start := Index;
               while Index <= Attr.Disable'Last
                 and then Attr.Disable (Index) /= ' '
                 and then Attr.Disable (Index) /= '.'
               loop
                  Index := Index + 1;
               end loop;

               if Index <= Attr.Disable'Last
                 and then Attr.Disable (Index) = '.'
               then
                  Name_Start := Index + 1;
                  Pkg_End    := Index - 1;

                  while Index <= Attr.Disable'Last
                    and then Attr.Disable (Index) /= ' '
                  loop
                     Index := Index + 1;
                  end loop;

               else
                  Name_Start := Pkg_Start;
                  Pkg_End    := Pkg_Start - 1;
               end if;

               for P in Properties_Module_ID.Pages'Range loop
                  Page := Properties_Module_ID.Pages (P);
                  for S in Page.Sections'Range loop
                     for A in Page.Sections (S).Attributes'Range loop
                        Attr2 := Editable_Attribute_Description_Access
                          (Page.Sections (S).Attributes (A));

                        if Attr2.Name.all =
                          Attr.Disable (Name_Start .. Index - 1)
                          and then Attr2.Pkg.all =
                            Attr.Disable (Pkg_Start  .. Pkg_End)
                        then
                           if Attr2.Editor /= null
                             and then Attr2.Editor.Active_Check /= null
                           then
                              Set_Active (Attr2.Editor.Active_Check, False);
                           end if;
                           exit;
                        end if;
                     end loop;
                  end loop;
               end loop;

               Index := Index + 1;
            end loop;
         end if;
      end if;
   end Toggle_Sensitive;

   -------------------------
   -- Find_Or_Create_Page --
   -------------------------

   procedure Find_Or_Create_Page
     (Editor  : not null access Properties_Editor_Record'Class;
      Name    : String;
      Page    : not null access Project_Editor_Page_Record'Class)
   is
      function Find (Parent : Gtk_Tree_Iter; N : String) return Gtk_Tree_Iter;
      function Find
        (Parent : Gtk_Tree_Iter; N : String) return Gtk_Tree_Iter
      is
         P : constant String := Parent_Menu_Name (N);
         Base : constant String := Base_Menu_Name (N);
         Iter, Child : Gtk_Tree_Iter;
      begin
         if P = "/" then
            --  look amongst the children of Parent for the base menu item
            Iter := Parent;
         else
            Iter := Find (Parent, P (P'First .. P'Last - 1));
         end if;

         if Iter = Null_Iter then
            Child := Editor.List_Of_Pages.Get_Iter_First;
         else
            Child := Editor.List_Of_Pages.Children (Iter);
         end if;

         while Child /= Null_Iter loop
            if Editor.List_Of_Pages.Get_String (Child, Column_Page_Name) =
              Base
            then
               --  nothing to do, already exists
               return Child;
            end if;
            Editor.List_Of_Pages.Next (Child);
         end loop;

         Editor.List_Of_Pages.Append (Child, Iter);
         Editor.List_Of_Pages.Set (Child, Column_Page_Name, Base);
         Editor.List_Of_Pages.Set (Child, Column_Visible, True);
         return Child;
      end Find;

      Child : Gtk_Tree_Iter;
   begin
      Child := Find (Null_Iter, Name);
      Editor.List_Of_Pages.Set (Child, Column_Path, Name);
      Editor.List_Of_Pages.Set (Child, Column_Page_Contents, GObject (Page));
      Page.Ref;
   end Find_Or_Create_Page;

   ----------------------------------
   -- For_Each_Project_Editor_Page --
   ----------------------------------

   procedure For_Each_Project_Editor_Page
     (Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Path      : not null access Gtk_Entry_Record'Class;
      Context   : String := "properties";
      Read_Only : Boolean;
      Callback  : not null access procedure
        (Title : String;
         Page  : not null access Project_Editor_Page_Record'Class))
   is
      XML_Page : XML_Page_Access;
   begin
      if Properties_Module_ID.Pages /= null then
         for P in Properties_Module_ID.Pages'Range loop
            declare
               N : constant String := Properties_Module_ID.Pages (P).Name.all;
            begin
               if N /= "" then
                  XML_Page := new XML_Page_Record;
                  XML_Page.Descr := Properties_Module_ID.Pages (P);
                  XML_Page.Path := Gtk_GEntry (Path);
                  XML_Page.Context := new String'(Context);
                  XML_Page.Initialize (Kernel, Read_Only, Project);

                  if not XML_Page.Has_Contents then
                     --  Not needed after all, no attribute is displayed
                     Destroy (XML_Page.all);
                     XML_Page.Ref_Sink;
                     XML_Page.Unref;
                  else
                     --  Do not add custom attributes to the hard-coded pages
                     if N = -"General" then
                        Callback (-"General/Attributes", XML_Page);
                     elsif N = -"Languages" then
                        Callback (-"Languages/Attributes", XML_Page);
                     else
                        Callback (N, XML_Page);
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end if;
   end For_Each_Project_Editor_Page;

   --------------------
   -- Row_Is_Visible --
   --------------------

   function Row_Is_Visible
     (Self : Gtk_Tree_Model; Iter : Gtk_Tree_Iter) return Boolean is
   begin
      return Get_Boolean (Self, Iter, Column_Visible);
   end Row_Is_Visible;

   -------------------
   -- For_All_Pages --
   -------------------

   procedure For_All_Pages
     (Data : access GObject_Record'Class;
      Callback : Page_Iterator_Callback)
   is
      Editor : constant Properties_Editor := Properties_Editor (Data);
   begin
      Editor.For_Each_Page (Callback, Visible_Only => False);
   end For_All_Pages;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class;
      Read_Only : Boolean)
   is
      Label            : Gtk_Label;
      Button           : Gtk_Widget;
      Box              : Gtk_Box;
      Scrolled         : Gtk_Scrolled_Window;
      Col              : Gtk_Tree_View_Column;
      Num              : Gint;
      Render           : Gtk_Cell_Renderer_Text;
      Table            : Gtk_Table;
      P                : access Project_Editor_Page_Record'Class;
      pragma Unreferenced (Num);

   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for " & Project.Name,
         Parent => Get_Current_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Editor.Set_Default_Size (1000, 700);
      Editor.Set_Name ("Project Properties"); --  For testsuite

      Gtk_New (Editor.Errors);
      Editor.Get_Content_Area.Pack_Start (Editor.Errors, Expand => False);
      Set_No_Show_All (Editor.Errors, No_Show_All => True);

      Gtk_New (Table, Rows => 1, Columns => 3, Homogeneous => False);

      Editor.Get_Content_Area.Pack_Start
        (Table, Expand => True, Fill => True);

      --  The list of pages, as a tree
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Table.Attach (Scrolled, 0, 1, 0, 1, Xoptions => Fill);

      Gtk_New (Editor.List_Of_Pages,
               (Column_Page_Name     => GType_String,
                Column_Page_Contents => GType_Object,
                Column_Visible       => GType_Boolean,
                Column_Path          => GType_String));

      Gtk_New (Editor.Filter, +Editor.List_Of_Pages);
      Editor.Filter.Set_Visible_Func (Row_Is_Visible'Access);

      Gtk_New (Editor.Tree, Editor.Filter);
      Scrolled.Add (Editor.Tree);
      Unref (Editor.Filter);  --  now owned by the tree
      Editor.Tree.Set_Name ("Project Properties Tree");  --  Testsuite
      Editor.Tree.Set_Headers_Visible (False);
      Editor.Tree.Get_Selection.On_Changed
        (On_Selection_Changed'Access, Editor);
      Gtk_New (Col);
      Num := Editor.Tree.Append_Column (Col);
      Gtk_New (Render);
      Col.Pack_Start (Render, Expand => True);
      Col.Add_Attribute (Render, "text", Column_Page_Name);

      --  The contents of the page
      Gtk_New_Vbox (Editor.Current_Page);
      Table.Attach (Editor.Current_Page, 1, 2, 0, 1);

      if not Read_Only then
         Gtk_New_Vbox (Box, Homogeneous => False);
         Table.Attach (Box, 2, 3, 0, 1, Xoptions => Fill);

         Gtk_New (Label, -"Apply changes to:");
         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start (Box, Label, Expand => False);

         Gtk_New (Editor.Prj_Selector, Kernel, Project);
         Pack_Start (Box, Editor.Prj_Selector, Expand => True, Fill => True);

         Gtk_New (Editor.Selector, Kernel);
         Pack_Start (Box, Editor.Selector, Expand => True, Fill => True);
      end if;

      Editor.Project := Project;
      Editor.Kernel  := Kernel_Handle (Kernel);

      if Read_Only then
         Gtk_New
           (Label,
            -"Cannot edit graphically");
         Label.Set_Tooltip_Text
           (-"Statements such as ""Var := ..."" prevent graphical editing."
            & ASCII.LF
            & (-"Aggregate projects also cannot be edited graphically"));
         Editor.Get_Action_Area.Pack_Start
           (Label, Expand => False, Fill => False);
      end if;

      Button := Editor.Add_Button (-"Edit Source", Response_Edit);
      Button.Set_Tooltip_Text
        ("Close this dialog and edit the source file for this project");

      Button := Editor.Add_Button (Stock_Ok, Gtk_Response_OK);
      Button.Set_Sensitive (not Read_Only);
      Button := Editor.Add_Button (Stock_Cancel, Gtk_Response_Cancel);

      Editor.General_Page := new General_Page_Record;
      Editor.General_Page.Initialize (Kernel, Read_Only, Project);
      Editor.Find_Or_Create_Page (-"General", Editor.General_Page);

      Editor.Languages_Editor := new Languages_Page_Record;
      Editor.Languages_Editor.Initialize (Kernel, Read_Only, Project);
      Editor.Languages_Editor.When_Languages_Change
        (Editor, On_Languages_Change'Access);
      Editor.Find_Or_Create_Page
        (-"Sources/Languages", Editor.Languages_Editor);

      P := new Toolchain_Page_Record;
      P.Initialize (Kernel, Read_Only, Project);
      Editor.Find_Or_Create_Page (-"Build/Toolchain", P);

      declare
         procedure Callback
           (Title : String;
            Page  : not null access Project_Editor_Page_Record'Class);
         procedure Callback
           (Title : String;
            Page  : not null access Project_Editor_Page_Record'Class) is
         begin
            Editor.Find_Or_Create_Page (Title, Page);
         end Callback;
      begin
         For_Each_Project_Editor_Page
           (Kernel,
            Project   => Project,
            Path      => Editor.General_Page.Path,
            Read_Only => Read_Only,
            Callback  => Callback'Access);
      end;

      --  Add the switches and naming pages

      declare
         Tools : constant Tool_Properties_Array := Get_All_Tools (Kernel);
         Page  : Project_Editor_Page;
         Tool_From_Name : constant Tool_From_Name_Getter :=
           (Data     => Editor,
            Iterator => For_All_Pages'Access);
      begin
         for T in Tools'Range loop
            Trace (Me, "MANU Add page for tool="
               & To_String (Tools (T).Tool_Name));
            Page := Switches_Editor_For_Tool_Factory
              (Tool           => Tools (T),
               Tool_From_Name => Tool_From_Name);
            if Page /= null then
               Page.Initialize (Kernel, Read_Only, Project);
               Editor.Find_Or_Create_Page
                 ("Build/Switches/" & To_String (Tools (T).Tool_Name), Page);
            end if;
         end loop;
      end;

      declare
         Languages : GNAT.Strings.String_List :=
           Known_Languages (Get_Language_Handler (Kernel));
         Page      : Project_Editor_Page;
      begin
         for L in Languages'Range loop
            Page := Get_Naming_Scheme_Page (Kernel, Languages (L).all);
            if Page /= null then
               Page.Initialize (Kernel, Read_Only, Project);
               Editor.Find_Or_Create_Page
                 ("Sources/Naming/" & Languages (L).all, Page);
            end if;
         end loop;
         Free (Languages);
      end;

      Editor.Show_All;
      Editor.Tree.Expand_All;

      On_Languages_Change (Editor, "");

      --  Select and display the first page
      Editor.Tree.Get_Selection.Select_Iter (Editor.Filter.Get_Iter_First);
   end Initialize;

   -------------------
   -- For_Each_Page --
   -------------------

   procedure For_Each_Page
     (Self         : not null access Properties_Editor_Record'Class;
      Callback     : Page_Iterator_Callback;
      Visible_Only : Boolean := True)
   is
      procedure For_Node (Node : Gtk_Tree_Iter);
      --  Iterate recursively for Node and its siblings

      procedure For_Node (Node : Gtk_Tree_Iter) is
         Iter : Gtk_Tree_Iter := Node;
         Page : Project_Editor_Page;
      begin
         while Iter /= Null_Iter loop
            Page := Project_Editor_Page
              (Self.List_Of_Pages.Get_Object (Iter, Column_Page_Contents));
            if Page /= null
              and then
                (not Visible_Only
                 or else Self.List_Of_Pages.Get_Boolean (Iter, Column_Visible))
            then
               Callback (Page);
            end if;

            For_Node (Self.List_Of_Pages.Children (Iter));
            Self.List_Of_Pages.Next (Iter);
         end loop;
      end For_Node;

   begin
      For_Node (Self.List_Of_Pages.Get_Iter_First);
   end For_Each_Page;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access XML_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Kernel, Languages);
      Attr : Editable_Attribute_Description_Access;
      Changed : Boolean := False;
   begin
      for S in Self.Descr.Sections'Range loop
         for A in Self.Descr.Sections (S).Attributes'Range loop
            Attr := Editable_Attribute_Description_Access
              (Self.Descr.Sections (S).Attributes (A));

            if Attr.Editor = null then
               Trace (Me, "No editor created for "
                      & Attr.Pkg.all & "'" & Attr.Name.all);

            elsif Is_Sensitive (Attr.Editor) then
               Generate_Project
                 (Attr.Editor, Project, Scenario_Variables, Changed);

            else
               --  The editor is insensitive, which means the user doesn't
               --  want to use that attribute
               Delete_Attribute_Value
                 (Project            => Project,
                  Attr               => Attr,
                  Attribute_Index    => "",
                  Scenario_Variables => Scenario_Variables,
                  Project_Changed    => Changed);
            end if;
         end loop;
      end loop;
      return Changed;
   end Edit_Project;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access XML_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project)
   is
      Attr         : Editable_Attribute_Description_Access;
      Box          : Gtk_Box;
      Frame        : Gtk_Frame;
      Size         : Gtk_Size_Group;
      Expandable   : Boolean;
      W_Expandable : Boolean;
      W            : Gtk_Widget;
   begin
      Initialize_Vbox (Self, Homogeneous => False);

      for S in Self.Descr.Sections'Range loop
         Box   := null;
         Frame := null;
         Expandable := False;

         for A in Self.Descr.Sections (S).Attributes'Range loop
            Attr := Editable_Attribute_Description_Access
              (Self.Descr.Sections (S).Attributes (A));

            Create_Widget_Attribute
              (Kernel,
               null,  --  wizard
               Project,
               Attr,
               Size,
               Read_Only   => Read_Only,
               Path_Widget => Self.Path,
               Widget      => W,
               Expandable  => W_Expandable,
               Context     => Self.Context.all);

            if W /= null then
               if Box = null then
                  Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 2);

                  if Self.Descr.Sections (S).Name.all /= "" then
                     Gtk_New (Frame, Self.Descr.Sections (S).Name.all);
                     Frame.Set_Border_Width (5);
                     Frame.Add (Box);
                  end if;
               end if;

               Expandable := Expandable or W_Expandable;
               Box.Pack_Start (W, Expand => W_Expandable, Fill => True);
               Self.Has_Contents := True;
            end if;
         end loop;

         if Box /= null then
            if Self.Descr.Sections (S).Name.all /= "" then
               Self.Pack_Start (Frame, Expand => Expandable, Fill => True);
            else
               Self.Pack_Start (Box, Expand => Expandable, Fill => True);
            end if;
         end if;
      end loop;
   end Initialize;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages
     (Editor : Properties_Editor) return String_List_Access is
   begin
      if Editor.Languages_Editor /= null then
         return Editor.Languages_Editor.Get_Languages;
      else
         return Get_Current_Value
           (Kernel  => Editor.Kernel,
            Project => GNATCOLL.Projects.No_Project,
            Attr    => Get_Attribute_Type_From_Name
              (Pkg => "", Name => "languages"));
      end if;
   end Get_Languages;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
     (Self : not null access Properties_Editor_Record'Class)
      return Project_Editor_Page
   is
      Children : Gtk.Widget.Widget_List.Glist :=
        Self.Current_Page.Get_Children;
      Result   : Gtk_Widget;
   begin
      if Children = Null_List then
         return null;
      else
         Result := Get_Data (Children);
         Free (Children);
         return Project_Editor_Page (Result);
      end if;
   end Get_Current_Page;

   -------------------------
   -- On_Languages_Change --
   -------------------------

   procedure On_Languages_Change
     (Editor : access GObject_Record'Class;
      Path   : String)
   is
      pragma Unreferenced (Path);
      Self : constant Properties_Editor := Properties_Editor (Editor);
      Languages : String_List_Access := Get_Languages (Self);
   begin
      Trace (Me, "MANU On_Languages_Change");
      for L in Languages'Range loop
         Trace (Me, "MANU   L =" & Languages (L).all);
      end loop;
      Set_Visible_Pages (Self, Languages.all);
      Free (Languages);
   end On_Languages_Change;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Self      : not null access Properties_Editor_Record'Class;
      Languages : GNAT.Strings.String_List;
      Parent    : Gtk_Tree_Iter := Null_Iter)
   is
      Iter, Child : Gtk_Tree_Iter;
      Page : Project_Editor_Page;
      Tmp  : Boolean;
   begin
      if Parent = Null_Iter then
         Iter := Self.List_Of_Pages.Get_Iter_First;
      else
         Iter := Self.List_Of_Pages.Children (Parent);
      end if;

      while Iter /= Null_Iter loop
         Set_Visible_Pages (Self, Languages, Iter);

         Page := Project_Editor_Page
           (Self.List_Of_Pages.Get_Object (Iter, Column_Page_Contents));
         if Page /= null then
            Tmp := Page.Is_Visible (Languages);
         else
            --  Check whether any of the children row is visible
            Tmp := False;
            Child := Self.List_Of_Pages.Children (Iter);
            while not Tmp and Child /= Null_Iter loop
               Tmp := Self.List_Of_Pages.Get_Boolean (Child, Column_Visible);
               Self.List_Of_Pages.Next (Child);
            end loop;
         end if;

         Self.List_Of_Pages.Set (Iter, Column_Visible, Tmp);
         Self.List_Of_Pages.Next (Iter);
      end loop;
   end Set_Visible_Pages;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (Editor : access GObject_Record'Class) is
      Self  : constant Properties_Editor := Properties_Editor (Editor);
      Current : constant Project_Editor_Page := Get_Current_Page (Self);
      Filter_Iter  : Gtk_Tree_Iter;
      Iter  : Gtk_Tree_Iter;
      M     : Gtk_Tree_Model;
      Page  : Project_Editor_Page;
   begin
      --  Check whether it is valid to change page
      --  ??? We should not prevent the change here, just highlight the name of
      --  the page in red or something
      if Current /= null then
         declare
            Error : constant String := Current.Is_Valid;
         begin
            Self.Errors.Set_Text (Error);
            if Error /= "" then
               Emit_Stop_By_Name (Self.Tree.Get_Selection, "changed");
               return;
            end if;
         end;
      end if;

      Self.Tree.Get_Selection.Get_Selected (M, Filter_Iter);
      if Filter_Iter /= Null_Iter then
         Self.Filter.Convert_Iter_To_Child_Iter (Iter, Filter_Iter);

         --  Remove the page currently displayed. It is not destroyed.
         Remove_All_Children (Self.Current_Page);

         --  Change the page that is displayed
         Page := Project_Editor_Page
           (Self.List_Of_Pages.Get_Object (Iter, Column_Page_Contents));
         if Page /= null then
            Self.Current_Page.Pack_Start (Page, Expand => True, Fill => True);
            Page.Show_All;

            if Self.Prj_Selector /= null then
               Set_Sensitive
                 (Self.Prj_Selector, (Page.Flags and Multiple_Projects) /= 0);
               Set_Sensitive
                 (Self.Selector, (Page.Flags and Multiple_Scenarios) /= 0);
            end if;
         end if;
      end if;
   end On_Selection_Changed;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Self : not null access General_Page_Record) return String
   is
      New_Name : constant String := Get_Safe_Text (Self.Name);
      New_Path : constant Virtual_File :=
        Create_From_UTF8 (Get_Safe_Text (Self.Path));
   begin
      if not Is_Valid_Project_Name (New_Name) then
         return (-"Invalid name for the project ") &
            (-"(only letters, digits and underscores ") &
            (-"and cannot be an Ada reserved word)");

      elsif not Is_Directory (New_Path) then
         return New_Path.Display_Full_Name & (-" is not a valid directory");
      end if;

      return "";
   end Is_Valid;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access General_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Kernel, Languages, Scenario_Variables);
      Relative : constant Boolean := Get_Active (Self.Use_Relative_Paths);
   begin
      --  Memorize the setup for relative paths. We do not do the path
      --  conversion ourselves, since each attribute editor will take into
      --  account this setting when it saves the project.

      if Relative /= Paths_Are_Relative (Project) then
         Trace (Me, "Process_General_Page: Paths will now be "
                & Boolean'Image (Relative));
         if Relative then
            Set_Paths_Type (Project, Projects.Relative);
         else
            Set_Paths_Type (Project, Absolute);
         end if;
         return True;
      end if;
      return False;
   end Edit_Project;

   ---------------------
   -- Edit_Properties --
   ---------------------

   procedure Edit_Properties
     (Project : Project_Type;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel), Sorted => False);

      procedure Report_Error (Msg : String);
      --  Report an error to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg);
      end Report_Error;

      Editor   : Properties_Editor;
      Changed  : Boolean := False;
      Ignore   : Message_Dialog_Buttons;
      Response : Gtk_Response_Type;
      pragma Unreferenced (Ignore);

      Read_Only : constant Boolean :=
        Project.Is_Aggregate_Project
        or else not Project.Is_Editable;
   begin
      Project_Editor_Hook.Run (Kernel);
      Gtk_New (Editor, Project, Kernel, Read_Only => Read_Only);

      loop
         Response := Editor.Run;
         case Response is
            when Response_Edit =>
               Open_File_Action_Hook.Run
                  (Kernel, File => Project.Project_Path,
                   Project => Project);
               exit;
            when Gtk_Response_OK =>
               declare
                  Failed : Boolean := False;

                  procedure Check_Page
                    (Page : not null access Project_Editor_Page_Record'Class);
                  procedure Check_Page
                    (Page : not null access Project_Editor_Page_Record'Class)
                  is
                     Error : constant String := Page.Is_Valid;
                  begin
                     if not Failed and then Error /= "" then
                        Ignore := Message_Dialog
                          (Msg         => Error,
                           Buttons     => Button_OK,
                           Dialog_Type => Gtkada.Dialogs.Error,
                           Title       => -"Error",
                           Parent      => Get_Current_Window (Kernel));
                        Failed := True;
                     end if;
                  end Check_Page;

               begin
                  Editor.For_Each_Page
                    (Check_Page'Unrestricted_Access, Visible_Only => True);

                  if Failed then
                     null;
                  else
                     declare
                        New_Name : constant String :=
                          Get_Safe_Text (Editor.General_Page.Name);
                        New_Base : constant Filesystem_String :=
                          To_File_Name (+New_Name);
                        New_Path : constant Virtual_File := Create_From_UTF8
                          (Get_Safe_Text (Editor.General_Page.Path));
                        New_File : constant Virtual_File :=
                          Create_From_Dir
                            (New_Path,
                             New_Base
                             & GNATCOLL.Projects.Project_File_Extension);

                     begin
                        if (New_Name /= Project.Name
                            or else New_Path /= Project_Directory (Project))
                          and then Is_Regular_File (New_File)
                        then
                           exit when Message_Dialog
                             (New_File.Display_Full_Name &
                              (-" already exists. Do you want to overwrite ?"),
                              Buttons     => Button_Yes or Button_No,
                              Dialog_Type => Gtkada.Dialogs.Error,
                              Title       => -"Error",
                              Parent      =>
                                Get_Current_Window (Kernel)) = Button_Yes;
                        else
                           exit;
                        end if;
                     end;
                  end if;
               end;

            when others =>
               exit;
         end case;
      end loop;

      if not Read_Only and then Response = Gtk_Response_OK then
         declare
            Prj_Iter    : Scenario_Selectors.Project_Iterator :=
                            Start (Editor.Prj_Selector);
            Languages   : String_List_Access := Get_Languages (Editor);
         begin
            while Current (Prj_Iter) /= GNATCOLL.Projects.No_Project loop
               declare
                  Scenar_Iter : Scenario_Iterator := Start (Editor.Selector);

                  procedure Edit_Page
                    (Page : not null access Project_Editor_Page_Record'Class);
                  procedure Edit_Page
                    (Page : not null access Project_Editor_Page_Record'Class)
                  is
                  begin
                     Changed := Changed
                       or Page.Edit_Project
                         (Current (Prj_Iter),
                          Kernel             => Kernel,
                          Languages          => Languages.all,
                          Scenario_Variables => Current (Scenar_Iter));
                  end Edit_Page;

               begin
                  while not At_End (Scenar_Iter) loop
                     Editor.For_Each_Page
                       (Edit_Page'Unrestricted_Access, Visible_Only => True);
                     Next (Scenar_Iter);
                  end loop;

                  Free (Languages);
               end;

               Next (Prj_Iter);
            end loop;
         end;

         --  Rename the project last, since we need to recompute the view
         --  immediately afterward before anything else can be done with the
         --  project.

         declare
            New_Name : constant String :=
              Get_Safe_Text (Editor.General_Page.Name);
            New_Path : constant Virtual_File :=
              Create_From_UTF8 (Get_Safe_Text (Editor.General_Page.Path));
            --  ??? Should we specify the build server's name as host ?
         begin
            if New_Name /= Project.Name
              or else New_Path /= Project_Directory (Project)
            then
               Project.Rename_And_Move
                 (New_Name   => New_Name,
                  Directory  => New_Path,
                  Errors     => Report_Error'Unrestricted_Access);

               --  Since we actually changed the project hierarchy (all modules
               --  that stored the name of the projects are now obsolete), we
               --  act as if a new project had been loaded.

               Project_Changed_Hook.Run (Kernel);

               Changed := True;
               Trace (Me, "Project was renamed or moved");
            end if;
         end;

         if Changed then
            Project.Set_Modified (True);
            Recompute_View (Kernel);
         end if;
      end if;

      declare
         procedure Destroy_Page
           (Page : not null access Project_Editor_Page_Record'Class);
         procedure Destroy_Page
           (Page : not null access Project_Editor_Page_Record'Class) is
         begin
            Project_Viewers.Destroy (Page.all);
            Unref (Page);
         end Destroy_Page;
      begin
         Editor.For_Each_Page
           (Destroy_Page'Unrestricted_Access, Visible_Only => False);
      end;

      Destroy (Editor);
      Free (Languages);
   end Edit_Properties;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Project_Properties_Editor_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      Project : Project_Type;
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      if Has_Project_Information (Context.Context) then
         Project := Project_Information (Context.Context);
      else
         Project := Get_Project (Kernel);
         Trace (Me, "MANU Editing kernel's project");
      end if;

      Edit_Properties (Project, Kernel);
      return Commands.Success;
   end Execute;

   -------------------------------
   -- New_Attribute_Description --
   -------------------------------

   overriding function New_Attribute_Description
     (Module  : access Properties_Module_ID_Record;
      Indexed : Boolean)
      return Attribute_Description_Access
   is
      pragma Unreferenced (Module);
   begin
      return new Editable_Attribute_Description (Indexed);
   end New_Attribute_Description;

end Project_Properties;
