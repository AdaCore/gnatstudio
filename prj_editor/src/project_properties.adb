-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2002-2004                       --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.IO; use GNAT.IO;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gdk.Event;                 use Gdk.Event;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Contexts;     use Glide_Kernel.Contexts;
with Glide_Kernel.Hooks;        use Glide_Kernel.Hooks;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Button;                use Gtk.Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with File_Utils;                use File_Utils;
with Basic_Types;               use Basic_Types;
with Language_Handlers;         use Language_Handlers;
with Language;                  use Language;
with Ada.Unchecked_Deallocation;
with Scenario_Selectors;        use Scenario_Selectors;
with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with Project_Viewers;           use Project_Viewers;
with Languages_Lists;           use Languages_Lists;
with Gtk.Event_Box;             use Gtk.Event_Box;
with VFS;                       use VFS;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with String_Utils;              use String_Utils;

package body Project_Properties is
   use Widget_List;

   Me : constant Debug_Handle := Create ("Project_Properties");


   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_List is access Boolean_Array;

   type Attribute_Editor_Record;
   type Attribute_Editor is access all Attribute_Editor_Record'Class;

   ----------------------------
   -- Attribute descriptions --
   ----------------------------

   type Attribute_As is (Attribute_As_String,
                         Attribute_As_Filename,
                         Attribute_As_Directory,
                         Attribute_As_Static_List,
                         Attribute_As_Dynamic_List);

   type Attribute_Type (Typ : Attribute_As := Attribute_As_String) is
   record
      case Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Directory   =>
            Default           : GNAT.OS_Lib.String_Access;
         when Attribute_As_Static_List =>
            Static_Allows_Any_String : Boolean := False;
            Static_List       : GNAT.OS_Lib.String_List_Access;
            Static_Default    : Boolean_List;
         when Attribute_As_Dynamic_List =>
            Dynamic_Allows_Any_String : Boolean := False;
            Dynamic_List_Lang : GNAT.OS_Lib.String_Access;
            Dynamic_List_Cmd  : GNAT.OS_Lib.String_Access;
            Dynamic_Default   : GNAT.OS_Lib.String_Access;
      end case;
   end record;

   type Indexed_Attribute_Type is record
      Typ         : Attribute_Type;
      Index_Value : GNAT.OS_Lib.String_Access;  --  null for the general case
   end record;
   type Indexed_Attribute_Type_Array
     is array (Natural range <>) of Indexed_Attribute_Type;
   type Indexed_Attribute_Type_List is access Indexed_Attribute_Type_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Indexed_Attribute_Type_Array, Indexed_Attribute_Type_List);

   type Attribute_Description (Indexed : Boolean := False) is record
      Name              : GNAT.OS_Lib.String_Access;
      Pkg               : GNAT.OS_Lib.String_Access;
      Description       : GNAT.OS_Lib.String_Access;
      Label             : GNAT.OS_Lib.String_Access;
      Is_List           : Boolean := False;
      Ordered_List      : Boolean := False;
      Omit_If_Default   : Boolean := True;
      Base_Name_Only    : Boolean := False;
      Editor            : Attribute_Editor;
      case Indexed is
         when True =>
            Index_Attribute : GNAT.OS_Lib.String_Access;
            Index_Package   : GNAT.OS_Lib.String_Access;
            Index_Types     : Indexed_Attribute_Type_List;
         when False =>
            Non_Index_Type  : Attribute_Type;
      end case;
   end record;
   type Attribute_Description_Access is access all Attribute_Description;
   type Attribute_Description_Array
     is array (Natural range <>) of Attribute_Description_Access;
   type Attribute_Description_List is access Attribute_Description_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Description_Array, Attribute_Description_List);

   type Attribute_Page_Section is record
      Name       : GNAT.OS_Lib.String_Access;  --  "" for unnamed sections
      Attributes : Attribute_Description_List;
   end record;
   type Attribute_Page_Section_Array
     is array (Natural range <>) of Attribute_Page_Section;
   type Attribute_Page_Section_List is access Attribute_Page_Section_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Page_Section_Array, Attribute_Page_Section_List);

   type Attribute_Page is record
      Name     : GNAT.OS_Lib.String_Access;
      Sections : Attribute_Page_Section_List;
   end record;
   type Attribute_Page_Array is array (Natural range <>) of Attribute_Page;
   type Attribute_Page_List is access Attribute_Page_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Page_Array, Attribute_Page_List);

   -----------------------
   -- Properties module --
   -----------------------

   type Properties_Module_ID_Record is new Module_ID_Record with record
      Pages : Attribute_Page_List;
   end record;
   type Properties_Module_ID_Access
     is access all Properties_Module_ID_Record'Class;
   Properties_Module_ID : Properties_Module_ID_Access;

   -----------------------
   -- Attribute editors --
   -----------------------

   type Attribute_Editor_Record is abstract new Root_Attribute_Editor_Record
   with record
      Kernel : Kernel_Handle;
      Project : Project_Type;
      Attribute : Attribute_Description_Access;
      --  Description of the attribute
   end record;

   procedure Create_Widget_Attribute
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Attr        : Attribute_Description_Access;
      Size_Group  : Gtk_Size_Group;
      Path_Widget : Gtk_Entry;
      Widget      : out Gtk_Widget;
      Expandable  : out Boolean);
   --  Create the widget used to edit Attr.
   --  Size_Group, if specified, is used to properly align all the labels.
   --  Expandable indicates whether the resulting widget should be expandable
   --  in its container box when the latter is resized, or whether it should
   --  keep its current size.
   --  Path_Widget should contain the full directory to the project file's
   --  location, and is used to resolve relative names.

   function Create_Widget_Attribute
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Path_Widget : Gtk_Entry;
      Is_List     : Boolean) return Attribute_Editor;
   --  Create the widget used to edit the attribute. This edits a specific
   --  attribute type, associated with Description (since for all indexes in
   --  the attribute the type can be different).
   --  See above for the description of Path_Widget

   function Create_Attribute_Dialog
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Index       : String := "") return String;
   --  Ask the user (through a dialog) for a new value for the attribute.
   --  If the attribute is a list, this queries the value for one of the
   --  elements in the list.

   -----------------------------------------------
   -- Attribute editors (files and directories) --
   -----------------------------------------------

   type File_Attribute_Editor_Record is new Attribute_Editor_Record with record
      As_Directory : Boolean := False;
      Ent          : Gtk_Entry;
      Model        : Gtk_Tree_Store;
      View         : Gtk_Tree_View;
      Path_Widget  : Gtk_Entry;
   end record;
   type File_Attribute_Editor is access all File_Attribute_Editor_Record'Class;

   function Create_File_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Path_Widget : Gtk_Entry;
      Is_List     : Boolean) return File_Attribute_Editor;
   --  Create a new attribute editor for a string attribute (any string, file
   --  or directory).
   --  Is_List should be true if the attribute is a list of simple strings
   --  Path_Widget is the widget that contains the full path to the project
   --  file's location, and is used to resolve relative names.

   procedure Generate_Project
     (Editor             : access File_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array);
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
     (Editor  : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues);
   --  Called when a directory is made recursive

   procedure Project_Path_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the location of the project is changed, to update the
   --  relative paths' display

   -------------------------------
   -- Attribute editors (lists) --
   -------------------------------

   type List_Attribute_Editor_Record is new Attribute_Editor_Record with record
      Model   : Gtk_Tree_Store;
      Combo   : Gtk_Combo;
   end record;
   type List_Attribute_Editor is access all List_Attribute_Editor_Record'Class;

   procedure Generate_Project
     (Editor             : access List_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array);
   --  See doc from inherited subprogram

   function Create_List_Attribute_Editor
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type;
      Description : Attribute_Description_Access;
      Attr    : Attribute_Type;
      Is_List : Boolean) return List_Attribute_Editor;
   --  Create the widget used to edit an attribute with a dynamic or static
   --  list type.
   --  Is_List indicates whether the value of the attribute is a list of
   --  values from this list, as opposed to a single value from the list.

   procedure Attribute_List_Changed
     (Editor  : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues);
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

   type List_Attribute_Callback is access procedure
     (Value : String; Is_Default : Boolean);
   --  Is_Default is set to true if Value is set as a default value for the
   --  attribute in the XML file

   procedure For_Each_Item_In_List
     (Kernel   : access Kernel_Handle_Record'Class;
      Attr     : Attribute_Type;
      Callback : List_Attribute_Callback);
   --  Calls Callback for each possible value of the attribute

   ---------------------------------
   -- Attribute editors (indexed) --
   ---------------------------------

   type Indexed_Attribute_Editor_Record is new Attribute_Editor_Record with
      record
         View        : Gtk_Tree_View;
         Model       : Gtk_Tree_Store;
         Path_Widget : Gtk_Entry;
      end record;
   type Indexed_Attribute_Editor is
     access all Indexed_Attribute_Editor_Record'Class;

   procedure Generate_Project
     (Editor             : access Indexed_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array);
   --  See doc from inherited subprogram

   function Create_Indexed_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Attr        : Attribute_Description_Access)
      return Indexed_Attribute_Editor;
   --  Create the widget used to edit an indexed attribute.

   function Edit_Indexed_Attribute
     (Editor  : access Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Boolean;
   --  Called when double-clicking on the value of an indexed attribute, and
   --  open a dialog to edit its value (that dialog contains one the standard
   --  widgets like combo boxes,... depending on the type of the attribute)




   type Widget_Array is array (Natural range <>) of Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   type Lang_Widget_Info is record
      Language  : GNAT.OS_Lib.String_Access;
      Widget    : Gtk_Widget;
      Attribute : Project_Field;
   end record;
   type Lang_Widget_Array is array (Natural range <>) of Lang_Widget_Info;
   type Lang_Widget_Array_Access is access Lang_Widget_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Lang_Widget_Array, Lang_Widget_Array_Access);

   type Properties_Editor_Record is new Gtk.Dialog.Gtk_Dialog_Record with
   record
      Name               : Gtk.GEntry.Gtk_Entry;
      Path               : Gtk.GEntry.Gtk_Entry;
      Debugger           : Gtk.GEntry.Gtk_Entry;
      Global_Pragmas     : Gtk.GEntry.Gtk_Entry;
      Local_Pragmas      : Gtk.GEntry.Gtk_Entry;
      Compilers          : Lang_Widget_Array_Access;
      Languages          : Languages_Lists.Languages_List;
      Use_Relative_Paths : Gtk.Check_Button.Gtk_Check_Button;
      Tools_Host         : Gtk.GEntry.Gtk_Entry;
      Program_Host       : Gtk.GEntry.Gtk_Entry;
      Protocol           : Gtk.GEntry.Gtk_Entry;

      Selector     : Scenario_Selector;
      Prj_Selector : Project_Selector;

      Pages              : Widget_Array_Access;
      --  The pages that have been registered.

      Project            : Project_Type;
      Kernel             : Kernel_Handle;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Gtk_New
     (Editor  : out Properties_Editor;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Create a new properties editor

   procedure Initialize
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Internal initialization function

   procedure Selected_Languages_Changed
     (Editor : access Gtk_Widget_Record'Class);
   --  Refresh the list of sensitive fields for compiler,... when the list of
   --  languages has changed.

   procedure Destroyed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the editor is destroyed

   function Create_General_Page
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Create the "General" page for the project properties

   procedure Switch_Page
     (Notebook : access GObject_Record'Class; Editor : GObject);
   --  Called when a new page is selected in the notebook

   function Paths_Are_Relative
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Type)
      return Boolean;
   --  Return True if the paths in the project should be relative paths

   type Project_Edition_Type is (Do_Not_Edit,
                                 Edit_File,
                                 Edit_Properties);

   function Warning_On_View_Incomplete
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type)
      return Project_Edition_Type;
   --  Display a warning dialog to indicate that the current view is
   --  incomplete, and it might be dangereous to edit the properties.

   procedure Add_Widget
     (List      : in out Lang_Widget_Array_Access;
      Lang      : String;
      Widget    : access Gtk_Widget_Record'Class;
      Attribute : Project_Field);
   --  Add a new entry in List. Resize as needed

   function Get_Value (Project : Project_Type; Attribute : Project_Field)
      return String;
   --  Return the value of Attribute in Project.

   function Get_Current_Value
     (Project : Project_Type;
      Attr    : Attribute_Description_Access;
      Index   : String) return String;
   function Get_Current_Value
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type;
      Attr    : Attribute_Description_Access)
      return GNAT.OS_Lib.String_List;
   --  Get the current value for the given attribute. If the attribute has no
   --  current value, the default value is returned.
   --  Returned value must be freed by the caller


   procedure Customize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level);
   --  Create a new user-specific attribute in the project. This also indicates
   --  how this attribute should be edited graphically.
   --  Node should be an <project_attribute> node

   function Find_Editor_Page_By_Name (Name : String) return Natural;
   --  Find the index in Properties_Module_ID.Pages of the page Name.
   --  If this page doesn't exist yet, it is created as appropriate

   function Find_Editor_Section_By_Name
     (Page : Natural; Name : String) return Natural;
   --  Find the index of a specific attribute section in the given page

   function Find_Attribute_By_Name
     (Page      : Natural;
      Section   : Natural;
      Name, Pkg : String;
      Indexed   : Boolean) return Attribute_Description_Access;
   --  Find the index of a specific attribute in a section. The attribute is
   --  created if necessary.

   procedure Parse_Attribute_Description
     (Kernel : access Kernel_Handle_Record'Class;
      N      : Node_Ptr;
      A      : Attribute_Description_Access);
   --  Parse the attribute description from an XML node

   procedure Parse_Attribute_Type
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Node_Ptr;
      Name   : String;
      A      : in out Attribute_Type);
   --  Parse an attribute type from an XML node

   function Get_Attribute_Type_From_Name
     (Pkg : String; Name : String) return Attribute_Description_Access;
   --  Find the description of an attribute given its package and name

   function Is_Any_String
     (Attr  : Attribute_Description_Access;
      Index : String) return Boolean;
   --  Whether, for the index Index, Attr behaves like a free-form string.
   --  False is returned for special types like lists, filenames,...

   function Get_Index_In_Index_Types
     (Attr : Attribute_Description_Access; Value : String) return Integer;
   --  Return the index, for an indexed attribute, of the
   --  Indexed_Attribute_Type that applies for the index Value.

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Editor             : access File_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array)
   is
      pragma Unreferenced (Project, Scenario_Variables);
      Iter : Gtk_Tree_Iter;
      First : Boolean := True;
   begin
      if Editor.Ent /= null then
         Put_Line ("For "
                   & Editor.Attribute.Name.all
                   & " use """
                   & Get_Text (Editor.Ent) & """;");
      else
         Put ("For "
              & Editor.Attribute.Name.all
              & " use (");

         Iter := Get_Iter_First (Editor.Model);
         while Iter /= Null_Iter loop
            if First then
               Put ("""");
               First := False;
            else
               Put (", """);
            end if;

            Put (Name_As_Directory (Get_String (Editor.Model, Iter, 0)));
            if Get_Boolean (Editor.Model, Iter, 1) then
               Put ("/**");
            end if;

            Put ("""");

            Next (Editor.Model, Iter);
         end loop;

         Put_Line (")");
      end if;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Editor             : access List_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array)
   is
      pragma Unreferenced (Project, Scenario_Variables);
      Iter  : Gtk_Tree_Iter;
      First : Boolean := True;
   begin
      if Editor.Combo /= null then
         Put_Line ("For "
                   & Editor.Attribute.Name.all
                   & " use """
                   & Get_Text (Get_Entry (Editor.Combo)) & """;");
      else
         Put ("For "
              & Editor.Attribute.Name.all
              & " use (");

         Iter := Get_Iter_First (Editor.Model);
         while Iter /= Null_Iter loop
            if Get_Boolean (Editor.Model, Iter, 1) then
               if not First then
                  Put (",");
               end if;
               Put (""""
                    & Get_String (Editor.Model, Iter, 0) & """");
               First := False;
            end if;
            Next (Editor.Model, Iter);
         end loop;
         Put_Line (");");
      end if;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Editor             : access Indexed_Attribute_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array)
   is
      pragma Unreferenced (Project, Scenario_Variables);
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Editor.Model);
   begin
      while Iter /= Null_Iter loop
         Put_Line ("for " & Editor.Attribute.Name.all
                   & " ("""
                   & Get_String (Editor.Model, Iter, 0)
                   & """) use """
                   & Get_String (Editor.Model, Iter, 1) & """;");
         Next (Editor.Model, Iter);
      end loop;
   end Generate_Project;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Properties_Module_ID := new Properties_Module_ID_Record;
      Register_Module
        (Module                  => Module_ID (Properties_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => "Project_Properties",
         Customization_Handler   => Customize'Access);
   end Register_Module;

   ------------------------------
   -- Find_Editor_Page_By_Name --
   ------------------------------

   function Find_Editor_Page_By_Name (Name : String) return Natural is
      Tmp   : Attribute_Page_List;
   begin
      if Properties_Module_ID.Pages /= null then
         for P in Properties_Module_ID.Pages'Range loop
            if Properties_Module_ID.Pages (P).Name.all = Name then
               return P;
            end if;
         end loop;

         Tmp := Properties_Module_ID.Pages;
         Properties_Module_ID.Pages :=
           new Attribute_Page_Array (1 .. Tmp'Length + 1);
         Properties_Module_ID.Pages (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);
      else
         Properties_Module_ID.Pages := new Attribute_Page_Array (1 .. 1);
      end if;

      Properties_Module_ID.Pages (Properties_Module_ID.Pages'Last) :=
        (Name     => new String'(Name),
         Sections => null);

      return Properties_Module_ID.Pages'Last;
   end Find_Editor_Page_By_Name;

   ---------------------------------
   -- Find_Editor_Section_By_Name --
   ---------------------------------

   function Find_Editor_Section_By_Name
     (Page : Natural; Name : String) return Natural
   is
      P   : Attribute_Page renames Properties_Module_ID.Pages (Page);
      Tmp : Attribute_Page_Section_List;
   begin
      if P.Sections /= null then
         for S in P.Sections'Range loop
            if P.Sections (S).Name.all = Name then
               return S;
            end if;
         end loop;

         Tmp := P.Sections;
         P.Sections := new Attribute_Page_Section_Array (1 .. Tmp'Length + 1);
         P.Sections (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);

      else
         P.Sections := new Attribute_Page_Section_Array (1 .. 1);
      end if;

      P.Sections (P.Sections'Last) :=
        (Name       => new String'(Name),
         Attributes => null);
      return P.Sections'Last;
   end Find_Editor_Section_By_Name;

   ----------------------------
   -- Find_Attribute_By_Name --
   ----------------------------

   function Find_Attribute_By_Name
     (Page      : Natural;
      Section   : Natural;
      Name, Pkg : String;
      Indexed   : Boolean) return Attribute_Description_Access
   is
      S   : Attribute_Page_Section renames
        Properties_Module_ID.Pages (Page).Sections (Section);
      Tmp : Attribute_Description_List;
   begin
      if S.Attributes /= null then
         for A in S.Attributes'Range loop
            if S.Attributes (A).Name.all = Name
              and then S.Attributes (A).Pkg.all = Pkg
            then
               return S.Attributes (A);
            end if;
         end loop;

         Tmp := S.Attributes;
         S.Attributes := new Attribute_Description_Array (1 .. Tmp'Length + 1);
         S.Attributes (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);
      else
         S.Attributes := new Attribute_Description_Array (1 .. 1);
      end if;

      S.Attributes (S.Attributes'Last) := new Attribute_Description (Indexed);
      S.Attributes (S.Attributes'Last).Name := new String'(Name);
      S.Attributes (S.Attributes'Last).Pkg  := new String'(Pkg);

      return S.Attributes (S.Attributes'Last);
   end Find_Attribute_By_Name;

   --------------------------
   -- Parse_Attribute_Type --
   --------------------------

   procedure Parse_Attribute_Type
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Node_Ptr;
      Name   : String;
      A      : in out Attribute_Type)
   is
      Child2 : Node_Ptr;
      Choice_Count : Natural;
   begin
      if Child = null then
         A := (Typ     => Attribute_As_String,
               Default => null);

      elsif Child.Tag.all = "string" then
         declare
            Typ     : constant String := Get_Attribute (Child, "type");
            Default : constant String := Get_Attribute (Child, "default");
         begin
            if Typ = "file" then
               A := (Typ     => Attribute_As_Filename,
                     Default => new String'(Default));
            elsif Typ = "directory" then
               A := (Typ     => Attribute_As_Directory,
                     Default => new String'(Default));
            else
               if Typ /= "" then
                  Insert (Kernel,
                          -("Invalid value for ""type"" attribute"
                            & " for a <string> node"),
                          Mode => Error);
               end if;

               A := (Typ     => Attribute_As_String,
                     Default => new String'(Default));
            end if;
         end;

         if Child.Next /= null then
            Insert (Kernel,
                    -("<string> node must always appear only once,"
                      & " and after all other type descriptions"),
                    Mode => Error);
         end if;

      elsif Child.Tag.all = "choice" then
         Choice_Count := 1;
         Child2 := Child.Next;
         while Child2 /= null
           and then Child2.Tag.all = "choice"
         loop
            Choice_Count := Choice_Count + 1;
            Child2 := Child2.Next;
         end loop;

         if Child2 /= null
           and then (Child2.Tag.all /= "string"
                     or else Child2.Next /= null)
         then
            Insert (Kernel,
                    -("Only <string> can be specified in addition"
                      & " to <choice> for the type of attributes"),
                    Mode => Error);
         end if;

         A :=
           (Typ            => Attribute_As_Static_List,
            Static_Allows_Any_String => Child2 /= null,
            Static_List    => new GNAT.OS_Lib.String_List (1 .. Choice_Count),
            Static_Default => new Boolean_Array (1 .. Choice_Count));

         Child2 := Child;
         Choice_Count := 1;
         while Child2 /= null
           and then Child2.Tag.all = "choice"
         loop
            A.Static_List (Choice_Count) := new String'(Child2.Value.all);
            A.Static_Default (Choice_Count) :=
              Get_Attribute (Child2, "default") = "true"
              or else Get_Attribute (Child2, "default") = "1";
            Choice_Count := Choice_Count + 1;
            Child2 := Child2.Next;
         end loop;

      elsif Child.Tag.all = "shell" then
         A :=
           (Typ                       => Attribute_As_Dynamic_List,
            Dynamic_Allows_Any_String => Child.Next /= null
               and then Child.Next.Tag.all = "string",
            Dynamic_Default   => new String'(Get_Attribute (Child, "default")),
            Dynamic_List_Lang => new String'
              (Get_Attribute (Child, "lang", "shell")),
            Dynamic_List_Cmd  => new String'(Child.Value.all));

         if Child.Next /= null
           and then (Child.Next.Tag.all /= "string"
                     or else Child.Next.Next /= null)
         then
            Insert (Kernel,
                    -("Only <string> can be specified in addition"
                      & " to <shell> for the type of attributes"),
                    Mode => Error);
         end if;

      else
         Insert
           (Kernel,
            -"Invalid type description for the attribute" & Name,
            Mode => Error);
      end if;
   end Parse_Attribute_Type;

   ---------------------------------
   -- Parse_Attribute_Description --
   ---------------------------------

   procedure Parse_Attribute_Description
     (Kernel : access Kernel_Handle_Record'Class;
      N      : Node_Ptr;
      A      : Attribute_Description_Access)
   is
      Descr   : constant String := Get_Attribute (N, "description");
      Label   : constant String := Get_Attribute (N, "label", A.Name.all);
      Is_List : constant String := Get_Attribute (N, "list", "false");
      Ordered : constant String := Get_Attribute (N, "ordered", "false");
      Omit    : constant String := Get_Attribute
        (N, "omit_if_default", "true");
      Base    : constant String := Get_Attribute
        (N, "base_name_only", "false");
      Indexed : constant Boolean := N.Child /= null
        and then (N.Child.Tag.all = "index"
                  or else N.Child.Tag.all = "specialized_index");
      Child : Node_Ptr;

      procedure Parse_Indexed_Type (Value : String);
      --  Parse the type definition for an <index> or <specialized_index> node

      ------------------------
      -- Parse_Indexed_Type --
      ------------------------

      procedure Parse_Indexed_Type (Value : String) is
         Found : Boolean := False;
         Tmp   : Indexed_Attribute_Type_List;
      begin
         if A.Index_Types /= null then
            for T in A.Index_Types'Range loop
               if Value = "" and then A.Index_Types (T).Index_Value = null then
                  Insert (Kernel,
                          -("General indexed type already defined for"
                            & " attribute " & A.Name.all),
                          Mode => Error);
                  Found := True;
                  exit;

               elsif Value /= ""
                 and then A.Index_Types (T).Index_Value /= null
                 and then A.Index_Types (T).Index_Value.all = Value
               then
                  Insert (Kernel,
                          -("Attribute type already defined for"
                            & " attribute " & A.Name.all & " indexed by """
                            & Value & """"),
                          Mode => Error);
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Tmp := A.Index_Types;
               A.Index_Types := new Indexed_Attribute_Type_Array
                 (1 .. Tmp'Length + 1);
               A.Index_Types (1 .. Tmp'Length) := Tmp.all;
               Unchecked_Free (Tmp);
            end if;
         else
            A.Index_Types := new Indexed_Attribute_Type_Array (1 .. 1);
         end if;

         if Value /= "" then
            A.Index_Types (A.Index_Types'Last).Index_Value :=
              new String'(Value);
         end if;

         Parse_Attribute_Type
           (Kernel, Child.Child, A.Name.all,
            A.Index_Types (A.Index_Types'Last).Typ);
      end Parse_Indexed_Type;

   begin
      if Descr /= "" and then A.Description = null then
         A.Description := new String'(Descr);
      end if;

      if Label /= "" and then A.Label = null then
         A.Label := new String'(Label);
      end if;

      A.Is_List := Is_List = "true" or else Is_List = "1";
      A.Ordered_List := Ordered = "true" or else Ordered = "1";
      A.Omit_If_Default := Omit = "true" or else Omit = "1";
      A.Base_Name_Only := Base = "true" or else Omit = "1";

      if Indexed then
         Child := N.Child;
         while Child /= null loop
            if Child.Tag.all = "index" then
               A.Index_Attribute := new String'
                 (Get_Attribute (Child, "attribute"));
               A.Index_Package := new String'
                 (Get_Attribute (Child, "package"));
               Parse_Indexed_Type ("");

            elsif Child.Tag.all = "specialized_index" then
               Parse_Indexed_Type (Get_Attribute (Child, "value"));
            end if;

            Child := Child.Next;
         end loop;

      else
         Parse_Attribute_Type (Kernel, N.Child, A.Name.all, A.Non_Index_Type);
      end if;
   end Parse_Attribute_Description;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      N     : Node_Ptr := Node;
   begin
      while N /= null loop
         if N.Tag.all = "project_attribute" then
            declare
               Editor_Page : constant Natural := Find_Editor_Page_By_Name
                 (Get_Attribute (N, "editor_page", Default => "General"));
               Editor_Section : constant Natural := Find_Editor_Section_By_Name
                 (Editor_Page, Get_Attribute (N, "editor_section"));
               Name    : constant String := Get_Attribute (N, "name");
               Pkg     : constant String := Get_Attribute (N, "package");
               Indexed : constant Boolean := N.Child /= null
                 and then (N.Child.Tag.all = "index"
                           or else N.Child.Tag.all = "specialized_index");
               Attribute : Attribute_Description_Access;
            begin
               if Name = "" then
                  Insert
                    (Kernel,
                     -"<project_attribute> must specify a ""name"" attribute",
                     Mode => Error);
               end if;

               Attribute := Find_Attribute_By_Name
                 (Editor_Page, Editor_Section, Name, Pkg, Indexed);
               Parse_Attribute_Description (Kernel, N, Attribute);
            end;
         end if;

         N := N.Next;
      end loop;
   end Customize;

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
     (List      : in out Lang_Widget_Array_Access;
      Lang      : String;
      Widget    : access Gtk_Widget_Record'Class;
      Attribute : Project_Field)
   is
      Tmp : Lang_Widget_Array_Access := List;
   begin
      if List = null then
         List := new Lang_Widget_Array (1 .. 1);
      else
         List := new Lang_Widget_Array (Tmp'First .. Tmp'Last + 1);
         List (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      List (List'Last) :=
        (Language  => new String'(Lang),
         Widget    => Gtk_Widget (Widget),
         Attribute => Attribute);
   end Add_Widget;

   ------------------------
   -- Paths_Are_Relative --
   ------------------------

   function Paths_Are_Relative
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Type)
      return Boolean
   is
   begin
      case Get_Paths_Type (Project) is
         when Relative  => return True;
         when Absolute  => return False;
         when From_Pref => return Get_Pref (Kernel, Generate_Relative_Paths);
      end case;
   end Paths_Are_Relative;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor  : out Properties_Editor;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class) is
   begin
      Editor := new Properties_Editor_Record;
      Initialize (Editor, Project, Kernel);
   end Gtk_New;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Editor : access Gtk_Widget_Record'Class) is
      E : constant Properties_Editor := Properties_Editor (Editor);
   begin
      if E.Compilers /= null then
         for C in E.Compilers'Range loop
            Free (E.Compilers (C).Language);
         end loop;
         Unchecked_Free (E.Compilers);
      end if;

      Unchecked_Free (E.Pages);
   end Destroyed;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Project : Project_Type; Attribute : Project_Field)
      return String is
   begin
      if Attribute.Attribute_Index = null then
         if Attribute.Values /= null then
            return Get_Attribute_Value
              (Project,
               Build (Ide_Package, Attribute.Attribute_Name.all),
               Default => Attribute.Values (Attribute.Values'First).all);
         else
            return Get_Attribute_Value
              (Project, Build (Ide_Package, Attribute.Attribute_Name.all));
         end if;

      else
         if Attribute.Values /= null then
            return Get_Attribute_Value
              (Project,
               Build (Ide_Package, Attribute.Attribute_Name.all),
               Default => Attribute.Values (Attribute.Values'First).all,
               Index   => Attribute.Attribute_Index.all);
         else
            return Get_Attribute_Value
              (Project,
               Build (Ide_Package, Attribute.Attribute_Name.all),
               Index   => Attribute.Attribute_Index.all);
         end if;
      end if;
   end Get_Value;

   --------------------------------
   -- Selected_Languages_Changed --
   --------------------------------

   procedure Selected_Languages_Changed
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed   : constant Properties_Editor := Properties_Editor (Editor);
      Lang : Argument_List := Get_Languages (Ed.Languages);
   begin
      if Ed.Compilers /= null then
         for F in Ed.Compilers'Range loop
            Set_Sensitive
              (Ed.Compilers (F).Widget,
               Contains (Lang, Ed.Compilers (F).Language.all,
                         Case_Sensitive => False));
         end loop;
      end if;
      Free (Lang);
   end Selected_Languages_Changed;

   -------------------------
   -- Create_General_Page --
   -------------------------

   function Create_General_Page
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Button2      : Gtk_Button;
      Label        : Gtk_Label;
      Ent     : Gtk_GEntry;
      Combo        : Gtk_Combo;
      Items        : Gtk.Enums.String_List.Glist;
      Frame        : Gtk_Frame;
      Group        : Gtk_Size_Group;
      Vbox, Box, Hbox : Gtk_Box;
      Event        : Gtk_Event_Box;
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel), Sorted => False);
      Project_Languages : Argument_List :=  Get_Languages (Project);

      use Gtk.Enums.String_List;

   begin
      Gtk_New_Vbox (Vbox, Homogeneous => False);

      Gtk_New (Group, Both);

      --  Name and location frame

      Gtk_New (Frame, -"Name & Location");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

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
      Set_Tip (Get_Tooltips (Kernel), Event,
               (-"Name of the project. ") &
               (-"Only applies to the project you selected initially"));

      Gtk_New (Editor.Name);
      Set_Width_Chars (Editor.Name, 0);
      Set_Text (Editor.Name, Project_Name (Project));
      Pack_Start (Hbox, Editor.Name, Expand => True);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      --  Path

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Path:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Add_Widget (Group, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Directory containing the project file. Changing this field"
                 & " will move the project file. This field only applies to"
                 & " the project you selected initially"));

      Gtk_New (Editor.Path);
      Set_Width_Chars (Editor.Path, 0);
      Set_Text (Editor.Path, Project_Directory (Project));
      Pack_Start (Hbox, Editor.Path, Expand => True);

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor.Path);

      Gtk_New (Editor.Use_Relative_Paths, -"Paths should be relative paths");
      Set_Active
        (Editor.Use_Relative_Paths, Paths_Are_Relative (Kernel, Project));
      Pack_Start (Box, Editor.Use_Relative_Paths);
      Set_Tip (Get_Tooltips (Kernel), Editor.Use_Relative_Paths,
               -("If this field is activated, then all the path information in"
                 & " the project (source and build directories, dependencies"
                 & " between projects,...) will be stored as paths relative"
                 & " to the location of the project file. It will thus be"
                 & " easier to move the project file to another directory"));

      --  Languages frame

      Gtk_New (Editor.Languages, Kernel, Project);
      Pack_Start (Vbox, Editor.Languages, Expand => True);
      Widget_Callback.Object_Connect
        (Editor.Languages, "changed",
         Widget_Callback.To_Marshaller (Selected_Languages_Changed'Access),
         Editor);

      --  Tools frame

      Gtk_New (Frame, -"Tools");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      for L in Languages'Range loop
         declare
            Fields : constant Project_Field_Array := Get_Project_Fields
              (Get_Language_By_Name
               (Get_Language_Handler (Kernel), Languages (L).all));
         begin
            for F in Fields'Range loop
               Gtk_New_Hbox (Hbox, Homogeneous => False);
               Pack_Start (Box, Hbox);

               Gtk_New (Event);
               Pack_Start (Hbox, Event, Expand => False);

               Gtk_New (Label, Fields (F).Description.all);
               Set_Alignment (Label, 0.0, 0.5);
               Add (Event, Label);
               Add_Widget (Group, Label);
               --  Set_Tip (Get_Tooltips (Kernel), Event,
               --           Fields (F).Description.all);
               --  ??? Should use real tooltip

               if Fields (F).Values /= null then
                  Gtk_New (Combo);
                  Set_Width_Chars (Get_Entry (Combo), 0);
                  Pack_Start (Hbox, Combo);

                  for V in Fields (F).Values'Range loop
                     Append (Items, Fields (F).Values (V).all);
                  end loop;

                  Set_Popdown_Strings (Combo, Items);
                  Free_String_List (Items);

                  Ent := Get_Entry (Combo);
                  Add_Widget
                    (Editor.Compilers, Languages (L).all, Combo, Fields (F));
               else
                  Gtk_New (Ent);
                  Pack_Start (Hbox, Ent);
                  Add_Widget
                    (Editor.Compilers, Languages (L).all, Ent, Fields (F));
               end if;

               Set_Editable (Ent, Fields (F).Editable);
               Set_Text (Ent, Get_Value (Project, Fields (F)));
            end loop;
         end;
      end loop;

      --  Debugger

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Debugger:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -"Name and location of the debugger to use");

      Gtk_New (Combo);
      Set_Width_Chars (Get_Entry (Combo), 0);
      Pack_Start (Hbox, Combo, Expand => True);

      Append (Items, "gdb");
      Append (Items, "powerpc-wrs-vxworks-gdb");
      Append (Items, "powerpc-wrs-vxworksae-gdb");
      Append (Items, "powerpc-elf-gdb");
      Append (Items, "i386-wrs-vxworks-gdb");
      Append (Items, "m68k-wrs-vxworks-gdb");
      Append (Items, "mips-wrs-vxworks-gdb");
      Append (Items, "sparc-wrs-vxworks-gdb");
      Append (Items, "sparc64-wrs-vxworks-gdb");
      Append (Items, "xscale-wrs-vxworks-gdb");
      Append (Items, "powerpc-elf-lynxos-gdb");
      Append (Items, "powerpc-xcoff-lynxos-gdb");
      Set_Popdown_Strings (Combo, Items);
      Free_String_List (Items);
      Editor.Debugger := Get_Entry (Combo);
      Set_Text
        (Editor.Debugger,
         Get_Attribute_Value
         (Project, Debugger_Command_Attribute, Default => "gdb"));

      --  Configuration frame

      Gtk_New (Frame, -"External configuration");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Global pragmas:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("External file that contains the configuration pragmas to use"
                 & " for Ada sources. This file will be used both for this"
                 & " project and all its imported projects"));

      Gtk_New (Editor.Global_Pragmas);
      Set_Width_Chars (Editor.Global_Pragmas, 0);
      Pack_Start (Hbox, Editor.Global_Pragmas, Expand => True);
      Set_Text
        (Editor.Global_Pragmas,
         Get_Attribute_Value
         (Project, Global_Pragmas_Attribute,
          Default => ""));

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor.Global_Pragmas);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Local pragmas:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("External file that contains the configuration pragmas to use"
                 & " for Ada sources of this project. This is combined with"
                 & " the pragmas found in the Global Pragmas section of the"
                 & " root project." & ASCII.NUL
                 & "This file isn't used for imported projects"));

      Gtk_New (Editor.Local_Pragmas);
      Set_Width_Chars (Editor.Local_Pragmas, 0);
      Pack_Start (Hbox, Editor.Local_Pragmas, Expand => True);
      Set_Text
        (Editor.Local_Pragmas,
         Get_Attribute_Value
         (Project, Local_Pragmas_Attribute,
          Default => ""));

      Gtk_New (Button2, -"Browse");
      Pack_Start (Hbox, Button2, Expand => False);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Editor.Local_Pragmas);

      --  Cross environment frame

      Gtk_New (Frame, -"Cross environment");
      Set_Border_Width (Frame, 5);
      Pack_Start (Vbox, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      --  Tools host

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Tools host:");
      Add_Widget (Group, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Name or IP address of the machine on which the application"
                 & " should be compiled, debugged and run. It is recommended"
                 & " that you always start GPS locally, and work remotely"
                 & " on your application. Leave this field blank when working"
                 & " locally"));


      Gtk_New (Editor.Tools_Host);
      Set_Width_Chars (Editor.Tools_Host, 0);
      Pack_Start (Hbox, Editor.Tools_Host, Expand => True);
      Set_Text
        (Editor.Tools_Host,
         Get_Attribute_Value
           (Project, Remote_Host_Attribute, Default => ""));

      --  Program host

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Program host:");
      Add_Widget (Group, Label);
      Add (Event, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Name or IP address of the embedded target. This field"
                 & " should be left blank if you are not working on an"
                 & " embedded application"));

      Gtk_New (Editor.Program_Host);
      Set_Width_Chars (Editor.Tools_Host, 0);
      Pack_Start (Hbox, Editor.Program_Host, Expand => True);
      Set_Text
        (Editor.Program_Host,
         Get_Attribute_Value
         (Project, Program_Host_Attribute, Default => ""));

      --  Protocol

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox);

      Gtk_New (Event);
      Pack_Start (Hbox, Event, Expand => False);
      Gtk_New (Label, -"Protocol:");
      Add_Widget (Group, Label);
      Add (Event, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Protocol used to connect to the embedded target. This"
                 & " field should be left blank if you are not working"
                 & " on an embedded application"));

      Gtk_New (Combo);
      Set_Width_Chars (Get_Entry (Combo), 0);
      Pack_Start (Hbox, Combo, Expand => True);
      Append (Items, "wtx");
      Append (Items, "vxworks");
      Append (Items, "remote");
      Set_Popdown_Strings (Combo, Items);
      Free_String_List (Items);
      Editor.Protocol := Get_Entry (Combo);
      Set_Text
        (Editor.Protocol,
         Get_Attribute_Value
         (Project, Protocol_Attribute, Default => ""));

      Free (Languages);
      Free (Project_Languages);

      --  Force a refresh of the sensitive fields
      Changed (Editor.Languages);

      return Gtk_Widget (Vbox);
   end Create_General_Page;

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
      New_Iter  : Gtk_Tree_Iter;
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
                    and then Sect.Attributes (A).Index_Package.all =
                       Index_Pkg
                    and then Sect.Attributes (A).Index_Attribute.all =
                       Index_Name
                  then
                     declare
                        Att  : constant Attribute_Description_Access :=
                          Sect.Attributes (A);
                        Ed   : constant Indexed_Attribute_Editor :=
                          Indexed_Attribute_Editor (Att.Editor);
                     begin
                        if Ed.Model /= null and then Is_Selected then
                           Append (Ed.Model, New_Iter, Null_Iter);
                           Set (Ed.Model, New_Iter, 0, Index_Value);
                           Set (Ed.Model, New_Iter, 1,
                                Get_Current_Value
                                  (Project => Project,
                                   Attr    => Att,
                                   Index   => Index_Value));
                           Set (Ed.Model, New_Iter, 2,
                                Is_Any_String (Att, Index_Value));

                        elsif Ed.Model /= null then  --  Remove
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
      Ed   : constant List_Attribute_Editor := List_Attribute_Editor (Editor);
      Path : constant String := Get_String (Nth (Params, 1));
      Iter : constant Gtk_Tree_Iter :=
        Get_Iter_From_String (Ed.Model, Path);
      Selected : constant Boolean := not Get_Boolean (Ed.Model, Iter, 1);
   begin
      Set (Ed.Model, Iter, 1, Selected);
      Select_Attribute_In_List (Index_Pkg   => Ed.Attribute.Pkg.all,
                                Index_Name  => Ed.Attribute.Name.all,
                                Project     => Ed.Project,
                                Index_Value => Get_String (Ed.Model, Iter, 0),
                                Is_Selected => Selected);
   end Attribute_List_Changed;

   ---------------------------
   -- For_Each_Item_In_List --
   ---------------------------

   procedure For_Each_Item_In_List
     (Kernel   : access Kernel_Handle_Record'Class;
      Attr     : Attribute_Type;
      Callback : List_Attribute_Callback)
   is
      Errors : aliased Boolean;
      Script : Scripting_Language;
      Start, Index : Natural;
   begin
      if Attr.Typ = Attribute_As_Dynamic_List then
         Script := Lookup_Scripting_Language
           (Kernel, Attr.Dynamic_List_Lang.all);
         if Script = null then
            Insert (Kernel,
                    -"Unknown scripting language "
                    & Attr.Dynamic_List_Lang.all
                    & " used when defining a project attribute",
                    Mode => Error);
            return;
         end if;

         declare
            List : constant String := Execute_Command
              (Script,
               Command     => Attr.Dynamic_List_Cmd.all,
               Hide_Output => True,
               Errors      => Errors'Access);
         begin
            if Errors then
               Insert (Kernel,
                       -"Couldn't execute the command """
                       & Attr.Dynamic_List_Cmd.all & """ when computing the"
                       & " valid values for a project attribute",
                       Mode => Error);
            end if;

            Start := List'First;
            while Start <= List'Last loop
               Index := Start + 1;
               while Index <= List'Last
                 and then List (Index) /= ASCII.LF
               loop
                  Index := Index + 1;
               end loop;

               Callback (List (Start .. Index - 1),
                         List (Start .. Index - 1) = Attr.Dynamic_Default.all);

               Start := Index + 1;
            end loop;
         end;

      elsif Attr.Typ = Attribute_As_Static_List then
         if Attr.Static_List /= null then
            for V in Attr.Static_List'Range loop
               Callback (Attr.Static_List (V).all,
                         Attr.Static_Default (V));
            end loop;
         end if;
      end if;
   end For_Each_Item_In_List;

   ----------------------------------
   -- Create_List_Attribute_Editor --
   ----------------------------------

   function Create_List_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Is_List     : Boolean) return List_Attribute_Editor
   is
      use Gtk.Enums.String_List;
      Editor : List_Attribute_Editor;
      Items  : Gtk.Enums.String_List.Glist;
      Scrolled : Gtk_Scrolled_Window;
      View     : Gtk_Tree_View;
      Toggle   : Gtk_Cell_Renderer_Toggle;
      Text     : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Col_Number : Gint;
      pragma Unreferenced (Col_Number);

      Current_Value : GNAT.OS_Lib.String_List_Access;

      procedure Value_Cb (Value : String; Is_Default : Boolean);
      --  Called for each possible value of the attribute

      procedure Value_Cb (Value : String; Is_Default : Boolean) is
         pragma Unreferenced (Is_Default);
         Iter : Gtk_Tree_Iter;
         Selected : Boolean := False;
      begin
         if Is_List then
            Append (Editor.Model, Iter, Null_Iter);
            Set (Editor.Model, Iter, 0, Value);

            for C in Current_Value'Range loop
               if Case_Insensitive_Equal (Value, Current_Value (C).all) then
                  Selected := True;
                  exit;
               end if;
            end loop;

            Set (Editor.Model, Iter, 1, Selected);
         else
            Append (Items, Value);
         end if;
      end Value_Cb;

   begin
      Editor := new List_Attribute_Editor_Record;
      Initialize_Vbox (Editor, Homogeneous => False);

      Editor.Attribute := Description;
      Editor.Kernel    := Kernel_Handle (Kernel);
      Editor.Project   := Project;

      if Is_List then
         Gtk_New (Editor.Model,
                  (0  => GType_String,    --  Attribute value
                   1  => GType_Boolean)); --  Selected ?
      else
         Gtk_New (Editor.Combo);
         Set_Activates_Default (Get_Entry (Editor.Combo), True);
      end if;

      if Is_List then
         declare
            Current : aliased GNAT.OS_Lib.String_List := Get_Current_Value
              (Kernel  => Kernel,
               Project => Project,
               Attr    => Description);
         begin
            Current_Value := Current'Unchecked_Access;
            For_Each_Item_In_List (Kernel, Attr, Value_Cb'Unrestricted_Access);
            Free (Current);
         end;

         Gtk_New (Scrolled);
         Pack_Start (Editor, Scrolled, Expand => True, Fill => True);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

         Gtk_New (View, Editor.Model);
         Set_Headers_Visible (View, False);
         Add (Scrolled, View);

         Gtk_New (Text);
         Gtk_New (Toggle);
         Widget_Callback.Object_Connect
           (Toggle, "toggled", Attribute_List_Changed'Access, Editor);

         Gtk_New (Col);
         Col_Number := Append_Column (View, Col);
         Pack_Start (Col, Toggle, False);
         Add_Attribute (Col, Toggle, "active", 1);

         Pack_Start (Col, Text, True);
         Add_Attribute (Col, Text, "text", 0);
         Set_Sort_Column_Id (Col, 0);
         Clicked (Col);
      else
         For_Each_Item_In_List (Kernel, Attr, Value_Cb'Unrestricted_Access);

         Pack_Start (Editor, Editor.Combo, Expand => True, Fill => True);
         Set_Width_Chars (Get_Entry (Editor.Combo), 0);
         Set_Popdown_Strings (Editor.Combo, Items);
         Free_String_List (Items);

         Set_Text (Get_Entry (Editor.Combo),
                   Get_Current_Value
                     (Project => Project,
                      Attr    => Description,
                      Index   => ""));

         if Attr.Typ = Attribute_As_Static_List then
            Set_Editable
              (Get_Entry (Editor.Combo), Attr.Static_Allows_Any_String);
         else
            Set_Editable
              (Get_Entry (Editor.Combo), Attr.Dynamic_Allows_Any_String);
         end if;
      end if;

      return Editor;
   end Create_List_Attribute_Editor;

   -----------------
   -- Select_File --
   -----------------

   procedure Select_File
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed   : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      File : Virtual_File;
   begin
      if Ed.As_Directory then
         declare
            Dir : constant String := Select_Directory
              (Parent => Get_Current_Window (Ed.Kernel),
               Use_Native_Dialog => Get_Pref (Ed.Kernel, Use_Native_Dialogs));
         begin
            if Dir /= "" then
               Set_Text (Gtk_Entry (Ed.Ent), Dir);
            end if;
         end;
      else
         File := Select_File
           (Parent => Get_Current_Window (Ed.Kernel),
            Use_Native_Dialog => Get_Pref (Ed.Kernel, Use_Native_Dialogs));
         if File /= VFS.No_File then
            Set_Text (Gtk_Entry (Ed.Ent), Full_Name (File).all);
         end if;
      end if;
   end Select_File;

   ------------------------
   -- Add_String_In_List --
   ------------------------

   procedure Add_String_In_List (Editor : access Gtk_Widget_Record'Class) is
      Ed    : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      Value : constant String := Create_Attribute_Dialog
        (Ed.Kernel, Ed.Project, Ed.Attribute, Ed.Attribute.Non_Index_Type);
      Iter  : Gtk_Tree_Iter;
   begin
      if Value /= "" then
         Append (Ed.Model, Iter, Null_Iter);
         if Ed.As_Directory then
            Set (Ed.Model, Iter, 0, Normalize_Pathname
                   (Value,
                    Directory     => Get_Text (Ed.Path_Widget),
                    Resolve_Links => False));
         else
            Set (Ed.Model, Iter, 0, Base_Name (Value));
         end if;
         Set (Ed.Model, Iter, 1, False);
         Set (Ed.Model, Iter, 2, Value);

         if Ed.Attribute.Base_Name_Only then
            Select_Attribute_In_List
              (Project     => Ed.Project,
               Index_Pkg   => Ed.Attribute.Pkg.all,
               Index_Name  => Ed.Attribute.Name.all,
               Index_Value => Base_Name (Value),
               Is_Selected => True);
         else
            Select_Attribute_In_List
              (Project     => Ed.Project,
               Index_Pkg   => Ed.Attribute.Pkg.all,
               Index_Name  => Ed.Attribute.Name.all,
               Index_Value => Value,
               Is_Selected => True);
         end if;
      end if;
   end Add_String_In_List;

   -----------------------------
   -- Remove_String_From_List --
   -----------------------------

   procedure Remove_String_From_List
     (Editor : access Gtk_Widget_Record'Class)
   is
      Ed    : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      M     : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Ed.View), M, Iter);
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
   end Remove_String_From_List;

   --------------------
   -- Move_String_Up --
   --------------------

   procedure Move_String_Up (Editor : access Gtk_Widget_Record'Class) is
      Ed    : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      M     : Gtk_Tree_Model;
      Iter, Iter2  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
   begin
      Get_Selected (Get_Selection (Ed.View), M, Iter);
      if Iter /= Null_Iter then
         Path := Get_Path (Ed.Model, Iter);

         declare
            Value : constant String := Get_String (Ed.Model, Iter, 0);
            Recurse : constant Boolean := Get_Boolean (Ed.Model, Iter, 1);
            Relative : constant String := Get_String (Ed.Model, Iter, 2);
         begin
            if Prev (Path) then
               Remove (Ed.Model, Iter);
               Iter := Get_Iter (Ed.Model, Path);
               Insert_Before
                 (Ed.Model, Iter2, Parent => Parent (Ed.Model, Iter),
                  Sibling => Iter);
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
      Ed    : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      M     : Gtk_Tree_Model;
      Iter, Iter2  : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Ed.View), M, Iter);
      if Iter /= Null_Iter then
         declare
            Value : constant String := Get_String (Ed.Model, Iter, 0);
            Recurse : constant Boolean := Get_Boolean (Ed.Model, Iter, 1);
            Relative : constant String := Get_String (Ed.Model, Iter, 2);
         begin
            Iter_Copy (Iter, Dest => Iter2);
            Next (Ed.Model, Iter);
            if Iter /= Null_Iter then
               Remove (Ed.Model, Iter2);
               Insert_After
                 (Ed.Model, Iter2, Parent => Parent (Ed.Model, Iter),
                  Sibling => Iter);
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
      Ed   : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      Path : constant String := Get_String (Nth (Params, 1));
      Iter : constant Gtk_Tree_Iter :=
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
      Ed   : constant File_Attribute_Editor := File_Attribute_Editor (Editor);
      Iter : Gtk_Tree_Iter;
      Directory : constant String := Get_Text (Ed.Path_Widget);
   begin
      Iter := Get_Iter_First (Ed.Model);
      while Iter /= Null_Iter loop
         Set (Ed.Model, Iter, 0,
              Normalize_Pathname
                (Get_String (Ed.Model, Iter, 2),
                 Directory => Directory,
                 Resolve_Links => False));
         Next (Ed.Model, Iter);
      end loop;
   end Project_Path_Changed;

   ----------------------------------
   -- Create_File_Attribute_Editor --
   ----------------------------------

   function Create_File_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Path_Widget : Gtk_Entry;
      Is_List     : Boolean) return File_Attribute_Editor
   is
      Editor   : File_Attribute_Editor;
      Button   : Gtk_Button;
      Scrolled : Gtk_Scrolled_Window;
      Box      : Gtk_Box;
      Text     : Gtk_Cell_Renderer_Text;
      Toggle   : Gtk_Cell_Renderer_Toggle;
      Col      : Gtk_Tree_View_Column;
      Iter     : Gtk_Tree_Iter;
      Arrow    : Gtk_Arrow;
      Col_Number : Gint;
      pragma Unreferenced (Col_Number);

   begin
      Editor := new File_Attribute_Editor_Record;
      Initialize_Hbox (Editor, Homogeneous => False);
      Editor.Kernel       := Kernel_Handle (Kernel);
      Editor.As_Directory := Attr.Typ = Attribute_As_Directory;
      Editor.Attribute    := Description;
      Editor.Path_Widget  := Path_Widget;
      Editor.Project      := Project;

      if Is_List then
         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Editor, Scrolled, Expand => True, Fill => True);

         Gtk_New_Vbox (Box, Homogeneous => False);
         Pack_Start (Editor, Box, Expand => False);

         Gtk_New_From_Stock (Button, Stock_Add);
         Pack_Start (Box, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Add_String_In_List'Access),
            Slot_Object => Editor);

         Gtk_New_From_Stock (Button, Stock_Remove);
         Pack_Start (Box, Button, Expand => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Remove_String_From_List'Access),
            Slot_Object => Editor);

         if Description.Ordered_List then
            Gtk_New (Button);
            Pack_Start (Box, Button, Expand => False);
            Gtk_New (Arrow, Arrow_Up, Shadow_None);
            Add (Button, Arrow);
            Widget_Callback.Object_Connect
              (Button, "clicked",
               Widget_Callback.To_Marshaller (Move_String_Up'Access),
               Slot_Object => Editor);

            Gtk_New (Button);
            Pack_Start (Box, Button, Expand => False);
            Gtk_New (Arrow, Arrow_Down, Shadow_None);
            Add (Button, Arrow);
            Widget_Callback.Object_Connect
              (Button, "clicked",
               Widget_Callback.To_Marshaller (Move_String_Down'Access),
               Slot_Object => Editor);
         end if;

         Gtk_New (Editor.Model, (0  => GType_String,   --  display name
                                 1  => GType_Boolean,  --  recursive ?
                                 2  => GType_String)); --  relative name

         Gtk_New (Editor.View, Editor.Model);
         Set_Headers_Visible (Editor.View, Editor.As_Directory);
         Add (Scrolled, Editor.View);

         Gtk_New (Text);
         Gtk_New (Col);
         Set_Resizable (Col, True);
         Set_Title (Col, -"Directory");
         Col_Number := Append_Column (Editor.View, Col);
         Pack_Start (Col, Text, True);
         Add_Attribute (Col, Text, "text", 0);

         if Editor.As_Directory then
            Gtk_New (Toggle);
            Widget_Callback.Object_Connect
              (Toggle, "toggled", Recursive_Directory_Changed'Access, Editor);
            Gtk_New (Col);
            Set_Resizable (Col, True);
            Set_Title (Col, -"Include subdirectories");
            Col_Number := Append_Column (Editor.View, Col);
            Pack_Start (Col, Toggle, False);
            Add_Attribute (Col, Toggle, "active", 1);

            Widget_Callback.Object_Connect
              (Path_Widget, "changed",
               Widget_Callback.To_Marshaller (Project_Path_Changed'Access),
               Slot_Object => Editor);
         end if;

         declare
            Value : GNAT.OS_Lib.String_List := Get_Current_Value
              (Kernel  => Kernel,
               Project => Project,
               Attr    => Description);
         begin
            for V in Value'Range loop
               Append (Editor.Model, Iter, Null_Iter);
               if Value (V) (Value (V)'Last - 2 .. Value (V)'Last) = "/**" then
                  Set (Editor.Model, Iter, 0,
                       Normalize_Pathname
                         (Value (V) (Value (V)'First .. Value (V)'Last - 3),
                          Directory => Get_Text (Path_Widget),
                          Resolve_Links => False));
                  Set (Editor.Model, Iter, 1, True);
               elsif Description.Omit_If_Default
                 and then Value (V).all = Attr.Default.all
               then
                  Set (Editor.Model, Iter, 0, Value (V).all);
                  Set (Editor.Model, Iter, 1, False);
               elsif Description.Base_Name_Only then
                  Set (Editor.Model, Iter, 0, Base_Name (Value (V).all));
                  Set (Editor.Model, Iter, 1, False);
               else
                  Set (Editor.Model, Iter, 0,
                       Normalize_Pathname
                         (Value (V).all,
                          Directory => Get_Text (Path_Widget),
                          Resolve_Links => False));
                  Set (Editor.Model, Iter, 1, False);
               end if;
               Set (Editor.Model, Iter, 2, Value (V).all);
            end loop;
            Free (Value);
         end;

      else
         Gtk_New (Editor.Ent);
         Set_Activates_Default (Editor.Ent, True);

         declare
            Current : constant String := Get_Current_Value
              (Project => Project, Attr => Description, Index   => "");
         begin
            if Description.Omit_If_Default
              and then Current = Attr.Default.all
            then
               Set_Text (Editor.Ent, Current);
            elsif Description.Base_Name_Only then
               Set_Text (Editor.Ent, Base_Name (Current));
            else
               Set_Text (Editor.Ent,
                         Normalize_Pathname
                           (Current,
                            Directory => Get_Text (Path_Widget),
                            Resolve_Links => False));
            end if;
         end;

         Pack_Start (Editor, Editor.Ent, Expand => True);

         if Attr.Typ /= Attribute_As_String then
            Gtk_New (Button, -"Browse");
            Widget_Callback.Object_Connect
              (Button, "clicked",
               Widget_Callback.To_Marshaller (Select_File'Access),
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
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Index       : String := "") return String
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Ent    : Gtk_Entry;
      File   : Virtual_File;
      W      : List_Attribute_Editor;
   begin
      case Attr.Typ is
         when Attribute_As_String =>
            Gtk_New (Dialog,
                     Title  => -"Enter new value",
                     Parent => Get_Current_Window (Kernel),
                     Flags  => Modal or Destroy_With_Parent);
            Gtk_New (Ent);
            Set_Text (Ent, Get_Current_Value
                        (Project, Description, Index => Index));
            Set_Activates_Default (Ent, True);
            Pack_Start (Get_Vbox (Dialog), Ent, Expand => True, Fill => True);

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            case Run (Dialog) is
               when Gtk_Response_OK =>
                  declare
                     S : constant String := Get_Text (Ent);
                  begin
                     Destroy (Dialog);
                     return S;
                  end;
               when others =>
                  Destroy (Dialog);
                  return "";
            end case;

         when Attribute_As_Filename =>
            declare
               Current : constant String := Get_Current_Value
                 (Project, Description, Index => Index);
            begin
               File := Select_File
                 (Parent => Get_Current_Window (Kernel),
                  Base_Directory => Dir_Name (Current),
                  Default_Name   => Base_Name (Current),
                  Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs));
               if File /= VFS.No_File then
                  return Full_Name (File).all;
               else
                  return "";
               end if;
            end;

         when Attribute_As_Directory =>
            return Select_Directory
              (Parent => Get_Current_Window (Kernel),
               Base_Directory => Get_Current_Value
                 (Project, Description, Index => Index),
               Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs));

         when Attribute_As_Static_List | Attribute_As_Dynamic_List =>
            Gtk_New (Dialog,
                     Title  => -"Select new value",
                     Parent => Get_Current_Window (Kernel),
                     Flags  => Modal or Destroy_With_Parent);
            W := Create_List_Attribute_Editor
              (Kernel, Project, Description, Attr, Is_List => False);
            Set_Text (Get_Entry (W.Combo),
                      Get_Current_Value
                        (Project, Description, Index => Index));
            Pack_Start (Get_Vbox (Dialog), W, Expand => True, Fill => True);

            Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
            Grab_Default (Button);
            Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

            Show_All (Dialog);

            case Run (Dialog) is
               when Gtk_Response_OK =>
                  declare
                     S : constant String := Get_Text (Get_Entry (W.Combo));
                  begin
                     Destroy (Dialog);
                     return S;
                  end;
               when others =>
                  Destroy (Dialog);
                  return "";
            end case;
      end case;
   end Create_Attribute_Dialog;

   -----------------------------
   -- Create_Widget_Attribute --
   -----------------------------

   function Create_Widget_Attribute
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Description : Attribute_Description_Access;
      Attr        : Attribute_Type;
      Path_Widget : Gtk_Entry;
      Is_List     : Boolean) return Attribute_Editor is
   begin
      case Attr.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Directory =>
            return Attribute_Editor
              (Create_File_Attribute_Editor
                 (Kernel, Project, Description, Attr, Path_Widget, Is_List));

         when Attribute_As_Static_List
              | Attribute_As_Dynamic_List =>
            return Attribute_Editor
              (Create_List_Attribute_Editor
                 (Kernel, Project, Description, Attr, Is_List));
      end case;
   end Create_Widget_Attribute;

   ----------------------------------
   -- Get_Attribute_Type_From_Name --
   ----------------------------------

   function Get_Attribute_Type_From_Name
     (Pkg : String; Name : String) return Attribute_Description_Access is
   begin
      for P in Properties_Module_ID.Pages'Range loop
         for S in Properties_Module_ID.Pages (P).Sections'Range loop
            declare
               Sect : Attribute_Page_Section renames
                 Properties_Module_ID.Pages (P).Sections (S);
            begin
               for A in Sect.Attributes'Range loop
                  if Sect.Attributes (A).Pkg.all = Pkg
                    and then Sect.Attributes (A).Name.all = Name
                  then
                     return Sect.Attributes (A);
                  end if;
               end loop;
            end;
         end loop;
      end loop;

      return null;
   end Get_Attribute_Type_From_Name;

   ------------------------------
   -- Get_Index_In_Index_Types --
   ------------------------------

   function Get_Index_In_Index_Types
     (Attr : Attribute_Description_Access; Value : String) return Integer
   is
      Default : Integer := -1;
   begin
      if Attr.Index_Types /= null then
         for T in Attr.Index_Types'Range loop
            if Attr.Index_Types (T).Index_Value = null then
               if Default = -1 then
                  Default := T;
               end if;
            elsif Attr.Index_Types (T).Index_Value.all = Value then
               Default := T;
               exit;
            end if;
         end loop;
      end if;
      return Default;
   end Get_Index_In_Index_Types;

   -------------------
   -- Is_Any_String --
   -------------------

   function Is_Any_String
     (Attr  : Attribute_Description_Access;
      Index : String) return Boolean
   is
      Tmp : constant Integer := Get_Index_In_Index_Types (Attr, Index);
   begin
      if Tmp = -1 then
         return False;
      else
         case Attr.Index_Types (Tmp).Typ.Typ is
            when Attribute_As_String    => return True;
            when Attribute_As_Filename
               | Attribute_As_Directory => return False;
            when Attribute_As_Static_List =>
               return Attr.Index_Types (Tmp).Typ.Static_List = null
                 and then Attr.Index_Types (Tmp).Typ.Static_Allows_Any_String;
            when Attribute_As_Dynamic_List =>
               return False;
         end case;
      end if;
   end Is_Any_String;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Project : Project_Type;
      Attr    : Attribute_Description_Access;
      Index   : String) return String
   is
      Empty_String  : aliased String := "";
      Default       : Integer;
      Default_Value : GNAT.OS_Lib.String_Access :=
        Empty_String'Unchecked_Access;
   begin
      --  Compute the default value
      if Attr.Indexed then
         Default := Get_Index_In_Index_Types (Attr, Index);
         if Default /= -1 then
            declare
               T : Attribute_Type renames Attr.Index_Types (Default).Typ;
            begin
               case T.Typ is
                  when Attribute_As_String
                     | Attribute_As_Filename
                     | Attribute_As_Directory =>
                     Default_Value := T.Default;

                  when Attribute_As_Static_List =>
                     for S in T.Static_Default'Range loop
                        if T.Static_Default (S) then
                           Default_Value := T.Static_List (S);
                           exit;
                        end if;
                     end loop;

                  when Attribute_As_Dynamic_List =>
                     Default_Value := T.Dynamic_Default;
               end case;
            end;
         end if;

      else
         case Attr.Non_Index_Type.Typ is
            when Attribute_As_String
               | Attribute_As_Filename
               | Attribute_As_Directory =>
               Default_Value := Attr.Non_Index_Type.Default;

            when Attribute_As_Static_List =>
               for S in Attr.Non_Index_Type.Static_Default'Range loop
                  if Attr.Non_Index_Type.Static_Default (S) then
                     Default_Value := Attr.Non_Index_Type.Static_List (S);
                     exit;
                  end if;
               end loop;

            when Attribute_As_Dynamic_List =>
               Default_Value := Attr.Non_Index_Type.Dynamic_Default;
         end case;
      end if;

      --  Get the value of the attribute from the project

      return Get_Attribute_Value
        (Project   => Project,
         Attribute => Build (Package_Name   => Attr.Pkg.all,
                             Attribute_Name => Attr.Name.all),
         Default   => Default_Value.all,
         Index     => Index);
   end Get_Current_Value;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type;
      Attr    : Attribute_Description_Access)
      return GNAT.OS_Lib.String_List
   is
      Result : String_List_Access;

      procedure Save_Value (Value : String; Is_Default : Boolean);
      --  Store the list of active values for Attr

      procedure Save_Value (Value : String; Is_Default : Boolean) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (GNAT.OS_Lib.String_List, String_List_Access);
         Tmp : String_List_Access := Result;
      begin
         if Is_Default then
            if Result /= null then
               Result := new GNAT.OS_Lib.String_List (1 .. Tmp'Length + 1);
               Result (Tmp'Range) := Tmp.all;
               Unchecked_Free (Tmp);
            else
               Result := new GNAT.OS_Lib.String_List (1 .. 1);
            end if;
            Result (Result'Last) := new String'(Value);
         end if;
      end Save_Value;

      Current : GNAT.OS_Lib.Argument_List := Get_Attribute_Value
        (Project   => Project,
         Attribute => Build (Package_Name   => Attr.Pkg.all,
                             Attribute_Name => Attr.Name.all),
         Index     => "");
   begin
      if Current'Length /= 0 then
         return Current;
      end if;

      Free (Current);

      --  Else get the default value
      case Attr.Non_Index_Type.Typ is
         when Attribute_As_String
            | Attribute_As_Filename
            | Attribute_As_Directory =>
            return GNAT.OS_Lib.String_List'(1 .. 0 => null);

         when Attribute_As_Static_List
              | Attribute_As_Dynamic_List =>
            For_Each_Item_In_List
              (Kernel, Attr.Non_Index_Type, Save_Value'Unrestricted_Access);

            declare
               R : constant GNAT.OS_Lib.String_List := Result.all;
            begin
               Unchecked_Free (Result);
               return R;
            end;
      end case;
   end Get_Current_Value;

   ----------------------------
   -- Edit_Indexed_Attribute --
   ----------------------------

   function Edit_Indexed_Attribute
     (Editor  : access Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Boolean
   is
      Ed        : constant Indexed_Attribute_Editor :=
        Indexed_Attribute_Editor (Editor);
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Cell_X, Cell_Y : Gint;
      Row_Found : Boolean;
      Iter      : Gtk_Tree_Iter;
      Tmp      : Integer;
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press
        and then Get_Button (Event) = 1
      then
         Get_Path_At_Pos
           (Ed.View, Gint (Get_X (Event)), Gint (Get_Y (Event)),
            Path, Column, Cell_X, Cell_Y, Row_Found);

         if Row_Found then
            Iter := Get_Iter (Ed.Model, Path);

            Tmp := Get_Index_In_Index_Types
              (Ed.Attribute, Value => Get_String (Ed.Model, Iter, 0));

            --  No need to open a dialog to edit simple string, this is done
            --  in-line
            if Ed.Attribute.Index_Types (Tmp).Typ.Typ /=
              Attribute_As_String
            then
               declare
                  Value : constant String := Create_Attribute_Dialog
                    (Ed.Kernel,
                     Project     => Ed.Project,
                     Description => Ed.Attribute,
                     Attr        => Ed.Attribute.Index_Types (Tmp).Typ,
                     Index       => Get_String (Ed.Model, Iter, 0));
               begin
                  if Value /= "" then
                     Set (Ed.Model, Iter, 1, Value);
                  end if;
               end;
            else
               Column := Get_Column (Ed.View, 1);
               Set_Cursor (Ed.View, Path, Column, Start_Editing => True);
            end if;
         end if;

         Path_Free (Path);
         return True;
      end if;
      return False;
   end Edit_Indexed_Attribute;

   -------------------------------------
   -- Create_Indexed_Attribute_Editor --
   -------------------------------------

   function Create_Indexed_Attribute_Editor
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Attr        : Attribute_Description_Access)
      return Indexed_Attribute_Editor
   is
      Language_Col  : constant := 0;
      Attribute_Col : constant := 1;
      Editable_Col  : constant := 2;

      use Gtk.Enums.String_List;
      Ed       : Indexed_Attribute_Editor;
      Text     : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Scrolled : Gtk_Scrolled_Window;
      Col_Number : Gint;
      pragma Unreferenced (Col_Number);
      Index    : constant Attribute_Description_Access :=
        Get_Attribute_Type_From_Name
          (Pkg  => Attr.Index_Package.all,
           Name => Attr.Index_Attribute.all);
      Current_Index : String_List_Access;

      procedure Value_Cb (Value : String; Selected : Boolean);
      --  Called for each possible value of the attribute

      procedure Value_Cb (Value : String; Selected : Boolean) is
         pragma Unreferenced (Selected);
         Iter : Gtk_Tree_Iter;
      begin
         for C in Current_Index'Range loop
            if Case_Insensitive_Equal (Current_Index (C).all, Value) then
               Append (Ed.Model, Iter, Null_Iter);
               Set (Ed.Model, Iter, Language_Col, Value);
               Set (Ed.Model, Iter, Attribute_Col,
                    Get_Current_Value
                      (Project => Project, Attr  => Attr, Index => Value));
               Set (Ed.Model, Iter, Editable_Col,
                    Is_Any_String (Attr, Value));
            end if;
         end loop;
      end Value_Cb;

   begin
      if Index = null then
         return null;
      end if;

      Ed := new Indexed_Attribute_Editor_Record;
      Initialize_Vbox (Ed, Homogeneous => True);

      Gtk_New (Scrolled);
      Pack_Start (Ed, Scrolled, Expand => True, Fill => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Ed.Kernel      := Kernel_Handle (Kernel);
      Ed.Attribute   := Attr;
      Ed.Project     := Project;

      Gtk_New (Ed.Model,
               (Language_Col  => GType_String,
                Attribute_Col => GType_String,
                Editable_Col  => GType_Boolean));

      declare
         Current_Value : aliased GNAT.OS_Lib.String_List :=
           Get_Current_Value (Kernel, Project, Index);
         Iter : Gtk_Tree_Iter;
      begin
         case Index.Non_Index_Type.Typ is
            when Attribute_As_Static_List | Attribute_As_Dynamic_List =>
               Current_Index := Current_Value'Unchecked_Access;
               For_Each_Item_In_List
                 (Kernel, Index.Non_Index_Type, Value_Cb'Unrestricted_Access);
            when others =>
               if not Index.Is_List then
                  Insert (Kernel,
                          -"Index for project attribute """
                          & Attr.Pkg.all & "'" & Attr.Name.all
                          & """ must be a list",
                          Mode => Error);
               else
                  for C in Current_Value'Range loop
                     Append (Ed.Model, Iter, Null_Iter);
                     Set (Ed.Model, Iter, Language_Col, Current_Value (C).all);
                     Set (Ed.Model, Iter, Attribute_Col,
                          Get_Current_Value
                            (Project => Project, Attr  => Attr,
                             Index => Current_Value (C).all));
                     Set (Ed.Model, Iter, Editable_Col,
                          Is_Any_String (Attr, Current_Value (C).all));
                  end loop;
               end if;
         end case;
         Free (Current_Value);
      end;

      Gtk_New (Ed.View, Ed.Model);
      Add (Scrolled, Ed.View);

      Return_Callback.Object_Connect
        (Ed.View, "button_press_event",
         Return_Callback.To_Marshaller (Edit_Indexed_Attribute'Access),
         Slot_Object => Ed);

      Gtk_New (Text);

      Gtk_New (Col);
      if Index.Label = null then
         Set_Title (Col, Index.Name.all);
      else
         Set_Title (Col, Index.Label.all);
      end if;
      Col_Number := Append_Column (Ed.View, Col);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "text", Language_Col);
      Set_Sort_Column_Id (Col, Language_Col);
      Clicked (Col);

      Gtk_New (Col);
      Set_Title (Col, Attr.Label.all & " (Click to edit)");
      Col_Number := Append_Column (Ed.View, Col);
      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "text", Attribute_Col);
      Add_Attribute (Col, Text, "editable", Editable_Col);
      Set_Sort_Column_Id (Col, Attribute_Col);

      return Ed;
   end Create_Indexed_Attribute_Editor;

   -----------------------------
   -- Create_Widget_Attribute --
   -----------------------------

   procedure Create_Widget_Attribute
     (Kernel      : access Kernel_Handle_Record'Class;
      Project     : Project_Type;
      Attr        : Attribute_Description_Access;
      Size_Group  : Gtk_Size_Group;
      Path_Widget : Gtk_Entry;
      Widget      : out Gtk_Widget;
      Expandable  : out Boolean)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Event  : Gtk_Event_Box;
   begin
      Gtk_New_Hbox (Box, Homogeneous => False);

      if Attr.Label /= null then
         Gtk_New (Event);
         Pack_Start (Box, Event, Expand => False);
         Gtk_New (Label, Attr.Label.all & ':');
         Set_Alignment (Label, 0.0, 0.5);

         if Size_Group /= null then
            Add_Widget (Size_Group, Label);
         end if;

         Add (Event, Label);

         if Attr.Description /= null
           and then Attr.Description.all /= ""
         then
            Set_Tip (Get_Tooltips (Kernel), Event, Attr.Description.all);
         end if;
      end if;

      if Attr.Indexed then
         Attr.Editor := Attribute_Editor
           (Create_Indexed_Attribute_Editor (Kernel, Project, Attr));
      else
         Attr.Editor := Create_Widget_Attribute
           (Kernel, Project, Attr, Attr.Non_Index_Type,
            Path_Widget, Attr.Is_List);
      end if;

      if Attr.Editor /= null then
         Pack_Start (Box, Attr.Editor, Expand => True, Fill => True);
         if Attr.Description /= null
           and then Attr.Description.all /= ""
         then
            Set_Tip (Get_Tooltips (Kernel), Attr.Editor, Attr.Description.all);
         end if;
      end if;

      Widget := Gtk_Widget (Box);
      Expandable := Attr.Indexed or else Attr.Is_List;
   end Create_Widget_Attribute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Properties_Editor_Record'Class;
      Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class)
  is
      Label        : Gtk_Label;
      Main_Note    : Gtk_Notebook;
      Button       : Gtk_Widget;
      Page         : Project_Editor_Page;
      Box          : Gtk_Box;
      Page_Box     : Gtk_Box;
      Main_Box     : Gtk_Box;
      Event        : Gtk_Event_Box;
      Frame        : Gtk_Frame;
      Size         : Gtk_Size_Group;
      Expandable   : Boolean;
      W_Expandable : Boolean;
      W            : Gtk_Widget;

   begin
      Gtk.Dialog.Initialize
        (Dialog => Editor,
         Title  => -"Properties for " & Project_Name (Project),
         Parent => Get_Current_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Policy
        (Editor,
         Allow_Shrink => False,
         Allow_Grow   => True,
         Auto_Shrink  => True);
      Realize (Editor);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (Destroyed'Access));

      Gtk_New_Hbox (Main_Box);
      Pack_Start (Get_Vbox (Editor), Main_Box, Expand => True, Fill => True);

      Gtk_New (Main_Note);
      Set_Tab_Pos (Main_Note, Pos_Left);
      Pack_Start (Main_Box, Main_Note, Expand => True, Fill => True);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_Start (Main_Box, Box, Expand => True, Fill => True);

      Gtk_New (Label, -"Apply changes to:");
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Editor.Prj_Selector, Kernel, Project);
      Pack_Start (Box, Editor.Prj_Selector, Expand => True, Fill => True);

      Gtk_New (Editor.Selector, Kernel);
      Pack_Start (Box, Editor.Selector, Expand => True, Fill => True);

      Editor.Project := Project;
      Editor.Kernel  := Kernel_Handle (Kernel);
      Editor.Pages   := new Widget_Array
        (1 .. Project_Editor_Pages_Count (Kernel));

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Show (Button);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
      Show (Button);

      if Properties_Module_ID.Pages = null then
         return;
      end if;

      Gtk_New (Label, -"General");
      Append_Page
        (Main_Note, Create_General_Page (Editor, Project, Kernel), Label);

      for P in Properties_Module_ID.Pages'Range loop

         --  We need to put the pages in an event box to workaround a gtk+ bug:
         --  since a notebook is a NO_WINDOW widget, button_press events are
         --  sent to the parent of the notebook. In case of nested notebooks,
         --  this means the event is sent to the parent's of the enclosing
         --  notebook, and thus is improperly handled by the nested notebooks.
         Gtk_New (Event);
         Gtk_New_Vbox (Page_Box, Homogeneous => False);
         Add (Event, Page_Box);

         Gtk_New (Size);

         for S in Properties_Module_ID.Pages (P).Sections'Range loop
            Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 2);

            Expandable := False;

            if Properties_Module_ID.Pages (P).Sections (S).Name.all /= "" then
               Gtk_New (Frame,
                        Properties_Module_ID.Pages (P).Sections (S).Name.all);
               Set_Border_Width (Frame, 5);
               Add (Frame, Box);
            end if;

            for A in Properties_Module_ID.Pages (P).Sections (S).
              Attributes'Range
            loop
               Create_Widget_Attribute
                 (Kernel,
                  Project,
                  Properties_Module_ID.Pages (P).Sections (S).
                    Attributes (A),
                  Size,
                  Path_Widget => Editor.Path,
                  Widget      => W,
                  Expandable  => W_Expandable);
               Expandable := Expandable or W_Expandable;
               Pack_Start (Box, W, Expand => W_Expandable, Fill => True);
            end loop;

            if Properties_Module_ID.Pages (P).Sections (S).Name.all /= "" then
               Pack_Start
                 (Page_Box, Frame, Expand => Expandable, Fill => True);
            else
               Pack_Start (Page_Box, Box, Expand => Expandable, Fill => True);
            end if;
         end loop;

         Gtk_New (Label, Properties_Module_ID.Pages (P).Name.all);
         Show (Event);
         Append_Page (Main_Note, Event, Label);
      end loop;

      Show_All (Editor);

      --  We used to create the pages dynamically, in Switch_Page. However,
      --  this means that the pages that haven't been visited by the user will
      --  not generate a project on exit, which is a problem when copying a
      --  scenario to another one for instance
      for E in Editor.Pages'Range loop
         Page := Get_Nth_Project_Editor_Page (Kernel, E);

         --  We need to put the pages in an event box to workaround a gtk+ bug:
         --  since a notebook is a NO_WINDOW widget, button_press events are
         --  sent to the parent of the notebook. In case of nested notebooks,
         --  this means the event is sent to the parent's of the enclosing
         --  notebook, and thus is improperly handled by the nested notebooks.
         Gtk_New (Event);

         Gtk_New (Label, Get_Label (Page));
         Editor.Pages (E) := Widget_Factory
           (Page, Project, Project_Path (Project), Editor.Kernel);

         Add (Event, Editor.Pages (E));
         Show (Event);
         Append_Page (Main_Note, Event, Label);
      end loop;

      --  Connect this only once we have created the pages
      Object_User_Callback.Connect
        (Main_Note, "switch_page",
         Object_User_Callback.To_Marshaller (Switch_Page'Access),
         User_Data => GObject (Editor),
         After => True);
   end Initialize;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Notebook : access GObject_Record'Class;
      Editor   : GObject)
   is
      Note  : constant Gtk_Notebook := Gtk_Notebook (Notebook);
      Ed    : constant Properties_Editor := Properties_Editor (Editor);
      Page  : constant Integer := Integer (Get_Current_Page (Note));
      Flags : Selector_Flags;

      pragma Warnings (Off);
   begin
      return;

      if Page >= 1
        and then not Gtk.Object.In_Destruction_Is_Set (Ed)
      then
         declare
            Languages : Argument_List := Get_Languages (Ed.Languages);
         begin
            Refresh
              (Page         => Get_Nth_Project_Editor_Page (Ed.Kernel, Page),
               Widget       => Ed.Pages (Page),
               Project      => Ed.Project,
               Languages    => Languages);
            Free (Languages);
         end;

         Flags := Get_Flags (Get_Nth_Project_Editor_Page (Ed.Kernel, Page));

         Set_Sensitive
           (Ed.Prj_Selector, (Flags and Multiple_Projects) /= 0);
         Set_Sensitive
           (Ed.Selector, (Flags and Multiple_Scenarios) /= 0);

      elsif Page = 0 then
         Set_Sensitive (Ed.Prj_Selector, True);
         Set_Sensitive (Ed.Selector, True);
      end if;
   end Switch_Page;

   --------------------------------
   -- Warning_On_View_Incomplete --
   --------------------------------

   function Warning_On_View_Incomplete
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type)
      return Project_Edition_Type
   is
      D : Gtk_Dialog;
      B : Gtk_Widget;
      L : Gtk_Label;
      C : Gtk_Check_Button;
      pragma Unreferenced (B);
   begin
      Gtk_New (D,
               Title  => -"Project had errors",
               Parent => Get_Main_Window (Kernel),
               Flags  => Modal);

      Gtk_New
        (L,
         -"The project """
         & Project_Name (Project)
         & (-(""" contained errors, and was incorrectly"
              & ASCII.LF
              & "loaded by GPS. Editing it through the project properties"
              & ASCII.LF
              & "dialog might result in a loss of data.")));
      Set_Alignment (L, 0.0, 0.5);
      Pack_Start (Get_Vbox (D), L, Expand => True, Fill => True);

      Gtk_New (C, -"Edit the project file");
      Set_Active (C, True);
      Pack_End (Get_Vbox (D), C, Expand => False);

      B := Add_Button (D, Stock_Open,   Gtk_Response_OK);
      B := Add_Button (D, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (D);

      case Run (D) is
         when Gtk_Response_OK =>
            if Get_Active (C) then
               Destroy (D);
               return Edit_File;
            else
               Destroy (D);
               return Edit_Properties;
            end if;

         when others =>
            Destroy (D);
            return Do_Not_Edit;
      end case;
   end Warning_On_View_Incomplete;

   ---------------------
   -- Edit_Properties --
   ---------------------

   procedure Edit_Properties
     (Project : Project_Type;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel), Sorted => False);

      procedure Report_Error (Msg : String);
      --  Report an error to the console

      function Process_General_Page
        (Editor : Properties_Editor;
         Project : Project_Type;
         Scenario_Variables : Scenario_Variable_Array;
         Project_Renamed_Or_Moved : Boolean)
         return Boolean;
      --  Modify the attributes set on the general page

      procedure Process_XML_Attributes
        (Project : Project_Type;
         Scenario_Variables : Scenario_Variable_Array);
      --  Generate the project for the attributes coming from
      --  the XML files

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg);
      end Report_Error;

      ----------------------------
      -- Process_XML_Attributes --
      ----------------------------

      procedure Process_XML_Attributes
        (Project            : Project_Type;
         Scenario_Variables : Scenario_Variable_Array)
      is
         Attr : Attribute_Description_Access;
      begin
         for P in Properties_Module_ID.Pages'Range loop
            for S in Properties_Module_ID.Pages (P).Sections'Range loop
               for A in Properties_Module_ID.Pages (P).Sections (S).
                 Attributes'Range
               loop
                  Attr := Properties_Module_ID.Pages (P).Sections (S).
                    Attributes (A);
                  Generate_Project (Attr.Editor, Project, Scenario_Variables);
               end loop;
            end loop;
         end loop;
      end Process_XML_Attributes;


      --------------------------
      -- Process_General_Page --
      --------------------------

      function Process_General_Page
        (Editor : Properties_Editor;
         Project : Project_Type;
         Scenario_Variables : Scenario_Variable_Array;
         Project_Renamed_Or_Moved : Boolean)
         return Boolean
      is
         Changed  : Boolean := False;
         Relative : Boolean := Get_Active (Editor.Use_Relative_Paths);
         New_Languages : Argument_List := Get_Languages (Editor.Languages);
         Project_Languages : Argument_List := Get_Languages (Project);
      begin
         --  If we are moving the project through the GUI, then we need to
         --  convert the paths to absolute or the semantics changes.

         if Project_Renamed_Or_Moved then
            Relative := False;
         end if;

         --  Convert the paths if necessary
         if Relative /= Paths_Are_Relative (Kernel, Project) then
            if Relative then
               Set_Paths_Type (Project, Projects.Relative);
            else
               Set_Paths_Type (Project, Absolute);
            end if;

            Changed := Changed
              or Convert_Paths (Project                => Project,
                                Use_Relative_Paths     => Relative,
                                Update_With_Statements => True);

            if Changed then
               Trace (Me, "Paths have changed relative/absolute");
            end if;
         end if;

         --  List of languages has changed

         if Project = No_Project
           or else not Is_Equal
           (New_Languages, Project_Languages, Case_Sensitive => False)
         then
            Changed := True;
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Languages_Attribute,
               Values             => New_Languages);
            Trace (Me, "List of languages changed");
         end if;

         --  Process the tools

         for C in Editor.Compilers'Range loop
            if Contains (New_Languages,
                         Editor.Compilers (C).Language.all,
                         Case_Sensitive => False)
            then
               declare
                  F   : Lang_Widget_Info renames Editor.Compilers (C);
                  Att : Project_Field renames Editor.Compilers (C).Attribute;
                  Ent : Gtk_Entry;
               begin
                  if F.Widget.all in Gtk_Entry_Record'Class then
                     Ent := Gtk_Entry (F.Widget);
                  elsif F.Widget.all in Gtk_Combo_Record'Class then
                     Ent := Get_Entry (Gtk_Combo (F.Widget));
                  end if;

                  if Ent /= null
                    and then
                      (Project = No_Project
                       or else Get_Text (Ent) /= Get_Value (Project, Att))
                  then
                     Changed := True;

                     if Att.Attribute_Index /= null then
                        Trace (Me, Att.Attribute_Name.all
                               & ' ' & Att.Attribute_Index.all
                               & " changed");
                        Update_Attribute_Value_In_Scenario
                          (Project          => Project,
                           Scenario_Variables => Scenario_Variables,
                           Attribute        => Build
                             (Ide_Package, Att.Attribute_Name.all),
                           Value            => Get_Text (Ent),
                           Attribute_Index  => Att.Attribute_Index.all);
                     else
                        Trace (Me, Att.Attribute_Name.all & " changed");
                        Update_Attribute_Value_In_Scenario
                          (Project         => Project,
                           Scenario_Variables => Scenario_Variables,
                           Attribute       => Build
                           (Ide_Package, Att.Attribute_Name.all),
                           Value           => Get_Text (Ent));
                     end if;
                  end if;
               end;
            end if;
         end loop;

         if Project = No_Project
           or else Get_Text (Editor.Debugger) /= Get_Attribute_Value
           (Project, Debugger_Command_Attribute, Default => "gdb")
         then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Debugger_Command_Attribute,
               Value              => Get_Text (Editor.Debugger));
            Changed := True;
            Trace (Me, "debugger command changed");
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Tools_Host) /= Get_Attribute_Value
           (Project, Remote_Host_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "Remote_Host changed");

            if Get_Text (Editor.Tools_Host) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Remote_Host_Attribute,
                  Value              => Get_Text (Editor.Tools_Host));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Remote_Host_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Program_Host) /= Get_Attribute_Value
           (Project, Program_Host_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "Program_Host changed");

            if Get_Text (Editor.Program_Host) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Program_Host_Attribute,
                  Value              => Get_Text (Editor.Program_Host));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Program_Host_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Protocol) /= Get_Attribute_Value
           (Project, Protocol_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "Protocol changed");

            if Get_Text (Editor.Protocol) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Protocol_Attribute,
                  Value              => Get_Text (Editor.Protocol));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Protocol_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Global_Pragmas) /= Get_Attribute_Value
           (Project, Global_Pragmas_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "global pragmas changed");

            if Get_Text (Editor.Global_Pragmas) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Global_Pragmas_Attribute,
                  Value              => Get_Text (Editor.Global_Pragmas));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Global_Pragmas_Attribute);
            end if;
         end if;

         if Project = No_Project
           or else Get_Text (Editor.Local_Pragmas) /= Get_Attribute_Value
           (Project, Local_Pragmas_Attribute, Default => "")
         then
            Changed := True;
            Trace (Me, "local pragmas changed");

            if Get_Text (Editor.Local_Pragmas) /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Local_Pragmas_Attribute,
                  Value              => Get_Text (Editor.Local_Pragmas));
            else
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Local_Pragmas_Attribute);
            end if;
         end if;

         Free (New_Languages);
         Free (Project_Languages);

         return Changed;
      end Process_General_Page;


      Editor  : Properties_Editor;
      Changed : Boolean := False;
      Response : Gtk_Response_Type;
      Response2 : Message_Dialog_Buttons;
      Project_Renamed_Or_Moved : Boolean := False;

   begin
      if not View_Is_Complete (Project) then
         case Warning_On_View_Incomplete (Kernel, Project) is
            when Do_Not_Edit =>
               return;

            when Edit_File =>
               Open_File_Editor
                 (Kernel,
                  Create (Full_Filename => Project_Path (Project)));
               return;

            when Edit_Properties =>
               null;
         end case;
      end if;

      Gtk_New (Editor, Project, Kernel);

      loop
         Response := Run (Editor);

         exit when Response /= Gtk_Response_OK;

         if not Is_Valid_Project_Name (Get_Text (Editor.Name)) then
            Response2 := Message_Dialog
              (Msg     => (-"Invalid name for the project ") &
                          (-"(only letters, digits and underscores)"),
               Buttons => Button_OK,
               Dialog_Type => Error,
               Title   => -"Error",
               Parent  => Get_Current_Window (Kernel));

         elsif not Is_Directory
           (Name_As_Directory (Get_Text (Editor.Path)))
         then
            Response2 := Message_Dialog
              (Msg     => Name_As_Directory (Get_Text (Editor.Path))
               & (-" is not a valid directory"),
               Buttons => Button_OK,
               Dialog_Type => Error,
               Title   => -"Error",
               Parent  => Get_Current_Window (Kernel));

         else
            declare
               New_Name : constant String := Get_Text (Editor.Name);
               New_File : constant String := To_File_Name (New_Name);
               New_Path : constant String :=
                 Name_As_Directory (Get_Text (Editor.Path));
            begin
               if (New_Name /= Project_Name (Project)
                   or else New_Path /= Project_Directory (Project))
                 and then Is_Regular_File
                 (New_Path & New_File & Project_File_Extension)
               then
                  Response2 := Message_Dialog
                    (Msg => New_Path & New_File & Project_File_Extension
                     & (-" already exists. Do you want to overwrite ?"),
                     Buttons => Button_Yes or Button_No,
                     Dialog_Type => Error,
                     Title   => -"Error",
                     Parent  => Get_Current_Window (Kernel));

                  if Response2 = Button_Yes then
                     exit;
                  end if;
               else
                  exit;
               end if;
            end;
         end if;
      end loop;

      if Response = Gtk_Response_OK then
         declare
            Vars         : constant Scenario_Variable_Array :=
              Scenario_Variables (Kernel);
            Saved_Values : Argument_List := Get_Current_Scenario (Vars);
            Prj_Iter     : Project_Iterator := Start (Editor.Prj_Selector);
            Ed           : Project_Editor_Page;
         begin
            while Current (Prj_Iter) /= No_Project loop
               declare
                  Scenar_Iter : Scenario_Iterator := Start (Editor.Selector);
               begin
                  while not At_End (Scenar_Iter) loop
                     --  Set the scenario
                     declare
                        Curr   : Argument_List := Current (Scenar_Iter);
                        Is_Env : Boolean := True;
                     begin
                        Set_Environment (Vars, Curr);

                        for V in Vars'Range loop
                           Is_Env := Is_Env and then Curr (V).all =
                             Saved_Values (V).all;
                        end loop;

                        Free (Curr);
                     end;

                     Process_XML_Attributes (Current (Prj_Iter), Vars);

                     if Process_General_Page
                       (Editor, Current (Prj_Iter), Vars,
                        Project_Renamed_Or_Moved)
                     then
                        Trace (Me, "General page modified the project");
                        Changed := True;
                     end if;

                     --  Modify each projects

                     for P in Editor.Pages'Range loop
                        Ed := Get_Nth_Project_Editor_Page (Kernel, P);

                        --  If the project is either the one the user clicked
                        --  on or the page might apply to multiple projects
                        if (Get_Flags (Ed) and Multiple_Projects) /= 0
                          or else Current (Prj_Iter) = Project
                        then
                           if Project_Editor
                             (Ed, Current (Prj_Iter),
                              Kernel, Editor.Pages (P),
                              Languages,
                              Vars,
                              Ref_Project => Project)
                           then
                              Trace (Me, "Project modified on page " & P'Img
                                     & "/" & Editor.Pages'Length'Img);
                              Changed := True;
                           end if;
                        end if;
                     end loop;

                     Next (Scenar_Iter);
                  end loop;
               end;

               Next (Prj_Iter);
            end loop;

            --  Restore the scenario
            Set_Environment (Vars, Saved_Values);
            Free (Saved_Values);
         end;

         --  Rename the project last, since we need to recompute the view
         --  immediately afterward before anything else can be done with the
         --  project.

         declare
            New_Name : constant String := Get_Text (Editor.Name);
            New_Path : constant String :=
              Name_As_Directory (Get_Text (Editor.Path));
         begin
            if New_Name /= Project_Name (Project)
              or else New_Path /= Project_Directory (Project)
            then
               Project_Renamed_Or_Moved := True;

               Rename_And_Move
                 (Root_Project  => Get_Project (Kernel),
                  Project       => Project,
                  New_Name      => New_Name,
                  New_Path      => New_Path,
                  Report_Errors => Report_Error'Unrestricted_Access);

               --  Since we actually changed the project hierarchy (all modules
               --  that stored the name of the projects are now obsolete), we
               --  act as if a new project had been loaded.

               Run_Hook (Kernel, Project_Changed_Hook);

               Changed := True;
               Trace (Me, "Project was renamed or moved");
            end if;
         end;

         if Changed then
            Recompute_View (Kernel);
         end if;
      end if;

      Destroy (Editor);
      Free (Languages);
   end Edit_Properties;

   -----------------------------
   -- Edit_Project_Properties --
   -----------------------------

   procedure Edit_Project_Properties
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      Edit_Properties (Project_Information (C), Get_Kernel (Context));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Edit_Project_Properties;

end Project_Properties;
