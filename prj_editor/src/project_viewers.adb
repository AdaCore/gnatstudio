-----------------------------------------------------------------------
--                              G P S                                --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Color;                    use Gdk.Color;
with Gdk.Event;                    use Gdk.Event;
with Glib;                         use Glib;
with Glib.Convert;                 use Glib.Convert;
with Glib.Object;                  use Glib.Object;
with Glib.Xml_Int;                 use Glib.Xml_Int;
with Gtk.Alignment;                use Gtk.Alignment;
with Gtk.Arguments;                use Gtk.Arguments;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Event_Box;                use Gtk.Event_Box;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Size_Group;               use Gtk.Size_Group;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Table;                    use Gtk.Table;
with Gtk.Tooltips;                 use Gtk.Tooltips;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_Store;               use Gtk.Tree_Store;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Vbutton_Box;              use Gtk.Vbutton_Box;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Window;                   use Gtk.Window;
with Gtkada.Dialogs;               use Gtkada.Dialogs;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada.File_Selector;         use Gtkada.File_Selector;

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with System;

with Basic_Types;              use Basic_Types;
with File_Utils;               use File_Utils;
with Prj;
with Projects.Editor;          use Projects, Projects.Editor;
with Projects.Registry;        use Projects.Registry;
with Creation_Wizard;          use Creation_Wizard;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Intl;               use Glide_Intl;
with Switches_Editors;         use Switches_Editors;
with Naming_Editors;           use Naming_Editors;
with Language_Handlers;        use Language_Handlers;
with Language_Handlers.Glide;  use Language_Handlers.Glide;
with Directory_Tree;           use Directory_Tree;
with Switches_Editors;         use Switches_Editors;
with Traces;                   use Traces;
with Variable_Editors;         use Variable_Editors;
with Project_Properties;       use Project_Properties;
with Histories;                use Histories;
with GUI_Utils;                use GUI_Utils;
with String_Utils;             use String_Utils;
with VFS;                      use VFS;

with Types;         use Types;

package body Project_Viewers is

   Me : constant Debug_Handle := Create ("Project_Viewers");

   type Project_Editor_Page_Array is array (Natural range <>)
     of Project_Editor_Page;
   type Project_Editor_Page_Array_Access is access Project_Editor_Page_Array;

   type Switches_Page_Creator_Data is record
      Creator  : Switches_Page_Creator;
      Page     : Switches_Editors.Switches_Editor_Page;
   end record;
   --  Page is the cached version of the page. It is created lazily, and never
   --  destroyed afterwards.

   type XML_Switches_Record is new Switches_Page_Creator_Record with record
      XML_Node  : Node_Ptr;   --  The <switches> node
      Tool_Name : GNAT.OS_Lib.String_Access;
      Languages : GNAT.OS_Lib.Argument_List_Access;
   end record;
   type XML_Switches is access all XML_Switches_Record'Class;

   function Create
     (Creator : access XML_Switches_Record;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Switches_Editors.Switches_Editor_Page;
   procedure Destroy (Creator : in out XML_Switches_Record);
   --  See inherited subprograms


   type Pages_Array is array (Natural range <>) of Switches_Page_Creator_Data;
   type Page_Array_Access is access Pages_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pages_Array, Page_Array_Access);

   type Naming_Page is record
      Language : Name_Id;
      Creator  : Naming_Scheme_Editor_Creator;
   end record;

   type Naming_Pages_Array is array (Natural range <>) of Naming_Page;
   type Naming_Pages_Array_Access is access Naming_Pages_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Naming_Pages_Array, Naming_Pages_Array_Access);

   type Prj_Editor_Module_Id_Record is new Module_ID_Record with record
      Project_Editor_Pages : Project_Editor_Page_Array_Access;
      --  The pages to be added in the project properties editor and the
      --  project creation wizard.

      Switches_Pages       : Page_Array_Access;
      Naming_Pages         : Naming_Pages_Array_Access;
   end record;
   type Prj_Editor_Module_Id_Access is access all
     Prj_Editor_Module_Id_Record'Class;

   Prj_Editor_Module_ID : Prj_Editor_Module_Id_Access;
   --  Id for the project editor module

   Project_Switches_Name : constant String := "Project Switches";

   Full_Path_Cst : aliased constant String := "full_path";
   Recursive_Cst : aliased constant String := "recursive";
   Directory_Cst : aliased constant String := "directory";
   Imported_Cst  : aliased constant String := "imported";
   Sources_Cmd_Parameters : constant Glide_Kernel.Scripts.Cst_Argument_List :=
     (1 => Full_Path_Cst'Access,
      2 => Recursive_Cst'Access);
   Source_Dirs_Cmd_Parameters : constant Glide_Kernel.Scripts.Cst_Argument_List
     := (1 => Recursive_Cst'Access);
   Add_Source_Dir_Cmd_Parameters :
     constant Glide_Kernel.Scripts.Cst_Argument_List :=
     (1 => Directory_Cst'Access);
   Remove_Dep_Cmd_Parameters : constant Glide_Kernel.Scripts.Cst_Argument_List
     := (1 => Imported_Cst'Access);

   File_Name_Column         : constant := 0;
   Compiler_Switches_Column : constant := 1;
   Compiler_Color_Column    : constant := 2;

   type Project_Viewer_Record is new Gtk.Box.Gtk_Hbox_Record with record
      Tree  : Gtk.Tree_View.Gtk_Tree_View;
      Model : Gtk.Tree_Store.Gtk_Tree_Store;
      --  The actual contents of the viewer

      Default_Switches_Color : Gdk.Color.Gdk_Color;
      --  Color to use when displaying switches that are set at the project
      --  level, rather than file specific

      Kernel  : Glide_Kernel.Kernel_Handle;

      View_Changed_Id : Gtk.Handlers.Handler_Id;
      --  Id for the "project_view_changed" handler

      Current_Project : Projects.Project_Type;
      --  The project to which the files currently in the viewer belong. This
      --  indicates which project file should be normalized when a modification
      --  takes place.
   end record;
   type Project_Viewer is access all Project_Viewer_Record'Class;

   procedure Gtk_New
     (Viewer  : out Project_Viewer;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new project viewer.
   --  Every time the selection in Explorer changes, the info displayed in
   --  the viewer is changed.

   procedure Initialize
     (Viewer : access Project_Viewer_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal subprogram for creating widgets

   procedure Show_Project
     (Viewer              : access Project_Viewer_Record'Class;
      Project_Filter      : Projects.Project_Type;
      Directory_Filter    : String := "");
   --  Shows all the direct source files of Project_Filter (ie not including
   --  imported projects, but including all source directories).
   --  This doesn't clear the list first!
   --  Directory_Filter should be used to limit the search path for the files.
   --  Only the files in Directory_Filter will be displayed.
   --
   --  Project_Filter mustn't be No_Project.

   procedure Destroy (Module : in out Prj_Editor_Module_Id_Record);
   --  Free the memory associated with the module

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Editor_Page_Array, Project_Editor_Page_Array_Access);
   --  Free the memory used by Pages

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      File_Name        : Name_Id;
      Directory_Filter : String := "");
   --  Append a new line in the current page of Viewer, for File_Name.
   --  The exact contents inserted depends on the current view.
   --  The file is automatically searched in all the source directories of
   --  Project_View.

   function Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Event : Gdk_Event)
     return Boolean;
   --  Callback when a row/column has been selected in the clist

   procedure Explorer_Selection_Changed
     (Viewer  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args);
   --  Called every time the selection has changed in the tree

   procedure Explorer_Selection_Changed
     (Viewer  : access Project_Viewer_Record'Class;
      Context : Selection_Context_Access);
   --  Same as above, but work directly on a context.

   function Project_Editor_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Return the current context for the contextual menu

   procedure Project_Editor_Contextual
     (Object    : access GObject_Record'Class;
      Context   : access Selection_Context'Class;
      Menu      : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add new entries, when needed, to the contextual menus from other
   --  modules.

   procedure On_New_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->New menu

   procedure On_Edit_Switches
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Edit Project Switches menu

   procedure On_Project_Properties
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Edit Project Properties menu

   procedure On_Project_Recompute
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Recompute Project menu

   procedure On_Add_Dependency_From_Wizard
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Add a dependency to the project described in Context. The dependency is
   --  created from the wizard.

   procedure On_Add_Dependency_From_Existing
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Add a dependency to a default empty project.

   procedure Remove_Project_Dependency
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Remove the project dependency between the two projects in Context.

   procedure Save_All_Projects
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Save the project associated with the kernel, and all its imported
   --  projects.

   procedure Save_Specific_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Save the project described in the context.

   procedure Edit_Project_File
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Callback for a menu item. Edits the project source file.

   procedure Project_View_Changed (Viewer  : access Gtk_Widget_Record'Class);
   --  Called when the project view has changed.

   procedure Read_Project_Name
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Type);
   --  Open a popup dialog to select a new name for Project.

   procedure Preferences_Changed
     (Viewer : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

   procedure Edit_Multiple_Switches
     (Viewer : access Gtk_Widget_Record'Class);
   --  Edit the switches for all the files selected in Viewer.

   procedure Set_Sensitive_Cb
     (Check : access Glib.Object.GObject_Record'Class;
      User  : GObject);
   --  Change the sensitivity of User according to the state of the check
   --  button Check.

   procedure Add_Dependency_Internal
     (Kernel                : access Kernel_Handle_Record'Class;
      Importing_Project     : Project_Type;
      Imported_Project_Path : String);
   --  Internal function that creates a dependency between two projects. If
   --  properly handle the case where a project with the same name as
   --  Imported_Project_Path already exists in the project hierarchy

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle the interactive commands related to the project editor

   procedure Update_Contents
     (Viewer    : access Project_Viewer_Record'Class;
      Project   : Project_Type;
      Directory : String := "";
      File      : String := "");
   --  Update the contents of the viewer.
   --  Directory and File act as filters for the information that is displayed.

   procedure Project_Viewers_Set
     (Viewer            : access Project_Viewer_Record'Class;
      Iter              : Gtk_Tree_Iter);
   --  Set the contents of the line Iter in the model. It is assumed the file
   --  name has already been set on that line

   procedure On_Project_Changed
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the project has just changed

   procedure On_File_Edited
     (Object : access GObject_Record'Class;
      Args   : Gtk_Args;
      Kernel : Kernel_Handle);
   pragma Unreferenced (On_File_Edited);
   --  See comments in the body of this function explaining the use of this
   --  pragma.
   --  Called when a new file is edited

   type On_Reopen is new Menu_Callback_Record with record
      Kernel : Kernel_Handle;
   end record;
   procedure Activate (Callback : access On_Reopen; Item : String);

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Called when a new customization in parsed

   procedure Parsing_Switches_XML
     (Kernel : access Kernel_Handle_Record'Class;
      Page   : Switches_Editor_Page;
      Table  : access Gtk_Table_Record'Class;
      Lines  : Natural;
      Cols   : Natural;
      Node   : Node_Ptr);
   --  Subprogram for Parse_Switches_Page, this is used to handle both the
   --  <switches> and the <popup> tags

   --------------------------
   -- Project editor pages --
   --------------------------

   type Main_Editor_Record is new Project_Editor_Page_Record with null record;
   function Widget_Factory
     (Page         : access Main_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Main_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean;

   type Source_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Source_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget;
   function Project_Editor
     (Page         : access Source_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type) return Boolean;

   type Object_Editor_Widget_Record is new Gtk_Box_Record with record
      Obj_Dir  : Gtk_Entry;
      Exec_Dir : Gtk_Entry;
      Same     : Gtk_Check_Button;
   end record;
   type Object_Editor_Widget is access all Object_Editor_Widget_Record'Class;

   type Object_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Object_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Object_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean;

   type Switches_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Switches_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Switches_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean;
   procedure Refresh
     (Page         : access Switches_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project      : Project_Type := No_Project;
      Languages    : GNAT.OS_Lib.Argument_List);

   type Naming_Editor_Record is new Project_Editor_Page_Record
     with record
        Kernel : Kernel_Handle;
     end record;
   function Widget_Factory
     (Page         : access Naming_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Naming_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type) return Boolean;
   procedure Refresh
     (Page         : access Naming_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project      : Project_Type := No_Project;
      Languages    : GNAT.OS_Lib.Argument_List);

   ------------------------
   -- Main files editors --
   ------------------------

   type Executables_Editor_Record is new Gtk_Box_Record with record
      Executables  : Gtk_Tree_Store;
      Tree_View    : Gtk_Tree_View;
      Project      : Project_Type;
      Kernel       : Kernel_Handle;
   end record;
   type Executables_Editor is access all Executables_Editor_Record'Class;
   --  An widget to edit the list of main files.

   procedure Add_Main_File
     (Editor : access Executables_Editor_Record'Class; File : String);
   --  Add a new file entry in the list of main units.

   procedure Add_Main_Unit (Editor : access Gtk_Widget_Record'Class);
   --  Add a main unit to the list of main units for the edited project

   procedure Remove_Main_Unit (Editor : access Gtk_Widget_Record'Class);
   --  Remove the selected main units.

   function Get_Languages_From_Tool_Node
     (N : Node_Ptr) return String_List_Access;
   --  Return the list of languages for that <tool> node

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Prj_Editor_Module_Id_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Editor_Page_Record'Class, Project_Editor_Page);
   begin
      if Module.Project_Editor_Pages /= null then
         for P in Module.Project_Editor_Pages'Range loop
            Destroy (Module.Project_Editor_Pages (P).all);
            Unchecked_Free (Module.Project_Editor_Pages (P));
         end loop;

         Unchecked_Free (Module.Project_Editor_Pages);
      end if;

      Unchecked_Free (Module.Switches_Pages);
      Unchecked_Free (Module.Naming_Pages);

      Destroy (Module_ID_Record (Module));
   end Destroy;

   --------------
   -- Activate --
   --------------

   procedure Activate (Callback : access On_Reopen; Item : String) is
   begin
      Load_Project (Callback.Kernel, Item);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Activate;

   -------------------------
   -- Project_Viewers_Set --
   -------------------------

   procedure Project_Viewers_Set
     (Viewer : access Project_Viewer_Record'Class;
      Iter   : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col2  : Gint; Value2 : String;
         Col3  : Gint; Value3 : Gdk_Color;
         Final : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");

      File_Name  : constant Virtual_File :=
        Create
          (Get_String (Viewer.Model, Iter, File_Name_Column),
           Viewer.Current_Project);
      Color      : Gdk_Color;
      Value      : Prj.Variable_Value;
      Is_Default : Boolean;
      Language   : constant Name_Id := Get_String
        (Get_Language_From_File
         (Glide_Language_Handler (Get_Language_Handler (Viewer.Kernel)),
          File_Name));
   begin
      Get_Switches
        (Viewer.Current_Project, Compiler_Package, File_Name,
         Language, Value, Is_Default);

      if Is_Default then
         Color := Viewer.Default_Switches_Color;
      else
         Color := Black (Get_Default_Colormap);
      end if;

      Internal (Get_Object (Viewer.Model), Iter'Address,
                Compiler_Switches_Column,
                Locale_To_UTF8 (To_String (Value)) & ASCII.NUL,
                Compiler_Color_Column, Color);
   end Project_Viewers_Set;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      File_Name        : Name_Id;
      Directory_Filter : String := "")
   is
      File_N     : constant String := Get_String (File_Name);
      Iter       : Gtk_Tree_Iter;

   begin
      if Directory_Filter = ""
        or else Is_Regular_File
          (Name_As_Directory (Directory_Filter) & File_N)
      then
         Append (Viewer.Model, Iter, Null_Iter);
         Set (Viewer.Model, Iter, File_Name_Column, Locale_To_UTF8 (File_N));
         Project_Viewers_Set (Viewer, Iter);
      end if;
   end Append_Line;

   ----------------
   -- Select_Row --
   ----------------

   function Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean
   is
      V     : constant Project_Viewer := Project_Viewer (Viewer);
      Iter  : Gtk_Tree_Iter;
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press
        and then Get_Button (Event) = 1
      then
         Iter := Find_Iter_For_Event (V.Tree, V.Model, Event);
         if Iter /= Null_Iter then
            if not Iter_Is_Selected (Get_Selection (V.Tree), Iter) then
               Unselect_All (Get_Selection (V.Tree));
               Select_Iter (Get_Selection (V.Tree), Iter);
            end if;

            Edit_Multiple_Switches (V);
            return True;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Select_Row;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed (Viewer : access Gtk_Widget_Record'Class) is
      V : Project_Viewer := Project_Viewer (Viewer);
   begin
      Clear (V.Model);  --  ??? Should delete selectively

      if V.Current_Project /= No_Project then
         V.Current_Project := Get_Project (V.Kernel);
         Show_Project (V, V.Current_Project);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Project_View_Changed;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
     (Viewer    : access Project_Viewer_Record'Class;
      Project   : Project_Type;
      Directory : String := "";
      File      : String := "")
   is
      Child : MDI_Child;
      Iter  : Gtk_Tree_Iter;

   begin
      Child := Find_MDI_Child (Get_MDI (Viewer.Kernel), Viewer);
      Assert (Me, Child /= null, "No MDI window visiting the project viewer");

      --  If the context is invalid, keep the currently displayed lines, so
      --  that when a new MDI child is selected, the contents of the viewer is
      --  not necessarily reset.

      if Directory = "" then
         Set_Title (Child,
                    Title => -"Editing switches for project "
                    & Project_Name (Project),
                    Short_Title => Project_Switches_Name);
      else
         Set_Title (Child,
                    Title => -"Editing switches for directory " & Directory,
                    Short_Title => Project_Switches_Name);
      end if;

      Viewer.Current_Project := Project;
      Clear (Viewer.Model);  --  ??? Should delete selectively

      if Viewer.Current_Project /= No_Project then
         Show_Project (Viewer, Viewer.Current_Project, Directory);
      end if;

      if File /= "" then
         Iter := Get_Iter_First (Viewer.Model);
         while Iter /= Null_Iter loop
            if Get_String (Viewer.Model, Iter, File_Name_Column) = File then
               Unselect_All (Get_Selection (Viewer.Tree));
               Select_Iter (Get_Selection (Viewer.Tree), Iter);
               exit;
            end if;
            Next (Viewer.Model, Iter);
         end loop;
      end if;
   end Update_Contents;

   --------------------------------
   -- Explorer_Selection_Changed --
   --------------------------------

   procedure Explorer_Selection_Changed
     (Viewer  : access Project_Viewer_Record'Class;
      Context : Selection_Context_Access)
   is
      File : File_Selection_Context_Access;
   begin
      --  If the context is invalid, keep the currently displayed lines, so
      --  that when a new MDI child is selected, the contents of the viewer is
      --  not necessarily reset.

      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);
         Update_Contents (Viewer,
                          Project_Information (File),
                          Dir_Name (File_Information (File)).all,
                          Base_Name (File_Information (File)));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Explorer_Selection_Changed;

   --------------------------------
   -- Explorer_Selection_Changed --
   --------------------------------

   procedure Explorer_Selection_Changed
     (Viewer  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      V     : constant Project_Viewer := Project_Viewer (Viewer);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (V.Kernel));
   begin
      --  Do nothing if we forced the selection change ourselves. For instance,
      --  when a new switch editor is created in On_Edit_Switches, to avoid
      --  doing extra work.
      if Child = null
        or else Get_Widget (Child) /= Gtk_Widget (Viewer)
      then
         Explorer_Selection_Changed
           (V, To_Selection_Context_Access (To_Address (Args, 1)));
      end if;
   end Explorer_Selection_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Viewer   : out Project_Viewer;
      Kernel   : access Kernel_Handle_Record'Class) is
   begin
      Viewer := new Project_Viewer_Record;
      Project_Viewers.Initialize (Viewer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Viewer   : access Project_Viewer_Record'Class;
      Kernel   : access Kernel_Handle_Record'Class)
   is
      Column_Types : constant GType_Array :=
        (File_Name_Column         => GType_String,
         Compiler_Switches_Column => GType_String,
         Compiler_Color_Column    => Gdk_Color_Type);

      Scrolled   : Gtk_Scrolled_Window;
      Col        : Gtk_Tree_View_Column;
      Render     : Gtk_Cell_Renderer_Text;
      Col_Number : Gint;
      pragma Unreferenced (Col_Number);
   begin
      Gtk.Box.Initialize_Hbox (Viewer);
      Viewer.Kernel := Kernel_Handle (Kernel);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Viewer, Scrolled);

      Gtk_New (Viewer.Model, Column_Types);
      Gtk_New (Viewer.Tree,  Viewer.Model);
      Set_Mode (Get_Selection (Viewer.Tree), Selection_Multiple);
      Add (Scrolled, Viewer.Tree);

      Gtk_New (Col);
      Col_Number := Append_Column (Viewer.Tree, Col);
      Set_Title (Col, -"File");
      Set_Resizable (Col, True);
      Set_Reorderable (Col, True);
      Gtk_New (Render);
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", File_Name_Column);

      Gtk_New (Col);
      Col_Number := Append_Column (Viewer.Tree, Col);
      Set_Title (Col, -"Compiler switches");
      Set_Resizable (Col, True);
      Set_Reorderable (Col, True);
      Gtk_New (Render);
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", Compiler_Switches_Column);
      Add_Attribute (Col, Render, "foreground_gdk", Compiler_Color_Column);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Viewer.Tree,
         Object          => Viewer,
         ID              => Module_ID (Prj_Editor_Module_ID),
         Context_Func    => Project_Editor_Context_Factory'Access);

      Return_Callback.Object_Connect
        (Viewer.Tree, "button_press_event",
         Return_Callback.To_Marshaller (Select_Row'Access), Viewer);

      Widget_Callback.Object_Connect
        (Kernel, Context_Changed_Signal,
         Explorer_Selection_Changed'Access,
         Viewer);
      Viewer.View_Changed_Id := Widget_Callback.Object_Connect
        (Kernel, Project_View_Changed_Signal,
         Widget_Callback.To_Marshaller (Project_View_Changed'Access),
         Viewer);

      Preferences_Changed (Viewer, Kernel_Handle (Kernel));

      Kernel_Callback.Object_Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         Slot_Object => Viewer,
         User_Data   => Kernel_Handle (Kernel));
      Show_All (Viewer);
   end Initialize;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Viewer : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      V     : constant Project_Viewer := Project_Viewer (Viewer);
   begin
      V.Default_Switches_Color := Get_Pref (Kernel, Default_Switches_Color);
      --  ??? Do we need to change the model to reflect this change
   end Preferences_Changed;

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record'Class;
      Project_Filter   : Project_Type;
      Directory_Filter : String := "")
   is
      Files : constant Name_Id_Array := Get_Source_Files
        (Project_Filter, Recursive => False);
   begin
      Viewer.Current_Project := Project_Filter;

      for F in Files'Range loop
         Append_Line (Viewer, Files (F), Directory_Filter);
      end loop;
   end Show_Project;

   --------------------
   -- On_New_Project --
   --------------------

   procedure On_New_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Wiz               : Creation_Wizard.Prj_Wizard;
      Load_Project_Page : Gtk_Alignment;
      Frame             : Gtk_Frame;
      Load_Project      : Gtk_Check_Button;
      Box               : Gtk_Vbox;
      Label             : Gtk_Label;

   begin
      Gtk_New (Load_Project_Page, 0.0, 0.5, 1.0, 0.0);
      Set_Border_Width (Load_Project_Page, 5);

      Gtk_New (Frame);
      Set_Border_Width (Frame, 5);
      Add (Load_Project_Page, Frame);

      Gtk_New_Vbox (Box, Spacing => 10);
      Add (Frame, Box);
      Gtk_New (Label, (-"Clicking on apply will generate the project") &
                 ASCII.LF &
                 (-"Warning: this operation may take a long time"));
      Set_Alignment (Label, 0.0, 0.5);
      Add (Box, Label);
      Gtk_New (Load_Project, -"Automatically load the project");
      Set_Active (Load_Project, True);
      Add (Box, Load_Project);

      Gtk_New (Wiz, Kernel);
      Add_Page
        (Wiz, Load_Project_Page, -"Loading the project", -"Load project");

      Show_All (Load_Project_Page);

      declare
         Name : constant String := Run (Wiz);
      begin

         --  If we have a child project, report limitations.

         for J in reverse Name'Range loop
            if Name (J) = '.' then
               Insert
                 (Kernel,
                  -("Child projects must import or extend their parent"
                    & " project. This is not done automatically by GPS, and"
                    & " you will have to edit the project by hand to make it"
                    & " valid."));
               Set_Active (Load_Project, False);
               exit;
            end if;
         end loop;

         --  Load the project if needed

         if Name /= ""
           and then Get_Active (Load_Project)
         then
            Glide_Kernel.Project.Load_Project (Kernel, Name);
         end if;
      end;

      Destroy (Wiz);

   exception
      when E : others =>
         Destroy (Wiz);
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_New_Project;

   ------------------------
   -- On_Editor_Switches --
   ------------------------

   procedure On_Edit_Switches
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Child   : MDI_Child;
      Viewer  : Project_Viewer;
      Context : Selection_Context_Access := Get_Current_Context (Kernel);

   begin
      Ref (Context);

      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Viewer_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
         Viewer := Project_Viewer (Get_Widget (Child));
      else
         Gtk_New (Viewer, Kernel);
         Child := Put
           (Kernel, Viewer,
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height),
            Module => Prj_Editor_Module_ID);
         Set_Title (Child, -"Switches editor");
      end if;

      --  The initial contents of the viewer should be read immediately from
      --  the explorer, without forcing the user to do a new selection.

      if Context /= null then
         Explorer_Selection_Changed (Viewer, Context);
      else
         Update_Contents (Viewer, Get_Project (Kernel));
      end if;

      Set_Focus_Child (Child);

      Unref (Context);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         Unref (Context);
   end On_Edit_Switches;

   --------------------------
   -- On_Project_Recompute --
   --------------------------

   procedure On_Project_Recompute
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Recompute_View (Kernel);
   end On_Project_Recompute;

   ---------------------------
   -- On_Project_Properties --
   ---------------------------

   procedure On_Project_Properties
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      Project : Project_Type;

   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_Project_Information
          (File_Selection_Context_Access (Context))
      then
         Project := Project_Information
           (File_Selection_Context_Access (Context));
      else
         Project := Get_Project (Kernel);
      end if;

      Edit_Properties (Project, Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_Project_Properties;

   -----------------------
   -- Read_Project_Name --
   -----------------------

   procedure Read_Project_Name
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Type)
   is
      procedure Report_Error (Msg : String);
      --  Report an error to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Console.Insert (Kernel, Msg, Mode => Console.Error);
         Parse_File_Locations (Kernel, Msg, -"Project read");
      end Report_Error;

      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Text   : Gtk_Entry;
      Widget : Gtk_Widget;
      pragma Unreferenced (Widget);

   begin
      Gtk_New
        (Dialog,
         Title  => -"Select name for project",
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Widget := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Widget := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Gtk_New (Label, -"Enter name of project:");
      Pack_Start (Get_Vbox (Dialog), Label);

      Gtk_New (Text, 40);
      Set_Width_Chars (Text, 20);
      Set_Text (Text, Project_Name (Project));
      Pack_Start (Get_Vbox (Dialog), Text);

      Show_All (Dialog);
      if Run (Dialog) = Gtk_Response_OK then
         Rename_And_Move
           (Root_Project  => Project,
            Project       => Project,
            New_Name      => Get_Text (Text),
            New_Path      => Project_Directory (Project),
            Report_Errors => Report_Error'Unrestricted_Access);
         Project_Changed (Kernel);
         Recompute_View (Kernel);
      end if;

      Destroy (Dialog);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Read_Project_Name;

   -----------------------
   -- Save_All_Projects --
   -----------------------

   procedure Save_All_Projects
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      if Status (Get_Project (Kernel)) /= From_File then
         Read_Project_Name (Kernel, Get_Project (Kernel));
      end if;

      Save_Project (Kernel, Get_Project (Kernel), Recursive => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Save_All_Projects;

   ---------------------------
   -- Save_Specific_Project --
   ---------------------------

   procedure Save_Specific_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      File    : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      Project : constant Project_Type := Project_Information (File);
   begin
      if Status (Project) /= From_File then
         Read_Project_Name (Kernel, Project);
      end if;

      Save_Project (Kernel, Project);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Save_Specific_Project;

   -----------------------
   -- Edit_Project_File --
   -----------------------

   procedure Edit_Project_File
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      File_C   : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File_C := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_C) then
            Open_File_Editor
              (Kernel,
               Create
                 (Full_Filename =>
                    Project_Path (Project_Information (File_C))));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Edit_Project_File;

   -----------------------------------
   -- On_Add_Dependency_From_Wizard --
   -----------------------------------

   procedure On_Add_Dependency_From_Wizard
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Wiz  : Creation_Wizard.Prj_Wizard;

   begin
      if Has_Project_Information (File) then
         Gtk_New (Wiz, Get_Kernel (Context));

         declare
            Name : constant String := Run (Wiz);
         begin
            if Name /= "" then
               Add_Dependency_Internal
                 (Get_Kernel (File),
                  Project_Information (File),
                  Name);
            end if;
         end;

         Destroy (Wiz);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_Add_Dependency_From_Wizard;

   -------------------------------
   -- Remove_Project_Dependency --
   -------------------------------

   procedure Remove_Project_Dependency
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Prj : constant Project_Type :=
        Importing_Project_Information (File);
   begin
      Trace (Me, "Removing project dependency "
             & Project_Name (Prj) & " -> "
             & Project_Name (Project_Information (File)));
      Remove_Imported_Project (Prj, Project_Information (File));
      Recompute_View (Get_Kernel (Context));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Remove_Project_Dependency;

   -----------------------------
   -- Add_Dependency_Internal --
   -----------------------------

   procedure Add_Dependency_Internal
     (Kernel                : access Kernel_Handle_Record'Class;
      Importing_Project     : Project_Type;
      Imported_Project_Path : String)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the glide console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Console.Insert
           (Kernel, S & ASCII.LF,
            Mode => Console.Error,
            Add_LF => False);
         Parse_File_Locations (Kernel, S, -"Project add dependency");
      end Report_Error;

      Base : constant String := Project_Directory (Importing_Project);
      Use_Relative_Path : constant Boolean :=
        Get_Paths_Type (Importing_Project) = Projects.Relative;
      Changed : Import_Project_Error;
      Result : Message_Dialog_Buttons;
      Must_Recompute : Boolean := False;
      Imported_Project : Project_Type;

   begin
      loop
         Changed := Add_Imported_Project
           (Importing_Project,
            Normalize_Pathname (Imported_Project_Path, Base),
            Report_Error'Unrestricted_Access,
            Use_Relative_Path => Use_Relative_Path);

         exit when Changed /= Project_Already_Exists;

         --  If there is already a project by that name in the tree,
         --  confirm whether we should rename it everywhere

         Result := Message_Dialog
           (Msg => -("A project with this name already exists in the"
                     & ASCII.LF
                     & "project graph. Do you want to replace all"
                     & ASCII.LF
                     & "occurences with the new project, or"
                     & ASCII.LF
                     & "cancel the new dependency ?"),
            Dialog_Type => Gtkada.Dialogs.Error,
            Buttons     => Button_OK or Button_Cancel,
            Title       => -"Project already exists",
            Parent      => Get_Main_Window (Kernel));

         exit when Result = Button_Cancel;

         Imported_Project := Load_Or_Find
           (Get_Registry (Kernel), Imported_Project_Path);

         Replace_Project_Occurrences
           (Root_Project      => Get_Project (Kernel),
            Project           => Imported_Project,
            Use_Relative_Path => Use_Relative_Path);
         Must_Recompute := True;
      end loop;

      if Changed = Success or else Must_Recompute then
         Recompute_View (Kernel);
      end if;
   end Add_Dependency_Internal;

   -------------------------------------
   -- On_Add_Dependency_From_Existing --
   -------------------------------------

   procedure On_Add_Dependency_From_Existing
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Prj : constant Project_Type := Project_Information (File);
      Dir : constant String := Project_Directory (Prj);

   begin
      if Has_Project_Information (File) then
         declare
            Name : constant Virtual_File :=
              Select_File
                (-"Select Project",
                 Dir,
                 File_Pattern      => "*.gpr",
                 Pattern_Name      => "Project files",
                 Parent            => Get_Main_Window (Get_Kernel (File)),
                 Use_Native_Dialog =>
                   Get_Pref (Get_Kernel (File), Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Get_Kernel (File)));

         begin
            if Name /= VFS.No_File then
               Add_Dependency_Internal
                 (Get_Kernel (File), Prj, Full_Name (Name).all);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_Add_Dependency_From_Existing;

   -------------------------------
   -- Project_Editor_Contextual --
   -------------------------------

   procedure Project_Editor_Contextual
     (Object    : access GObject_Record'Class;
      Context   : access Selection_Context'Class;
      Menu      : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      Item         : Gtk_Menu_Item;
      Submenu      : Gtk_Menu;
      File_Context : File_Selection_Context_Access;

   begin
      --  Very important: all callbacks that actually modify the project must
      --  set the project modified property (see Project_Hash), so that the
      --  user gets asked whether to save on exit.

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context)
           and then not Has_Directory_Information (File_Context)
           and then not Has_File_Information (File_Context)
         then
            Gtk_New (Item, Label => -"Edit project properties");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Project_Properties'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Save the project "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Save_Specific_Project'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Edit project source file");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (Edit_Project_File'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, -"Add dependency");
            Add (Menu, Item);

            Gtk_New (Submenu);
            Set_Submenu (Item, Submenu);

            Gtk_New (Item, -"From wizard...");
            Add (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Add_Dependency_From_Wizard'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, -"From project file...");
            Add (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Add_Dependency_From_Existing'Access),
               Selection_Context_Access (Context));

            if Has_Importing_Project_Information (File_Context)
              and then Project_Information (File_Context) /=
                Importing_Project_Information (File_Context)
            then
               Gtk_New
                 (Item, Label => -"Remove dependency "
                  & Project_Name (Importing_Project_Information (File_Context))
                  & " -> "
                  & Project_Name (Project_Information (File_Context)));
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Remove_Project_Dependency'Access),
                  Selection_Context_Access (Context));
            end if;
         end if;

         if Module_Name (Get_Creator (Context)) = Explorer_Module_Name then
            Gtk_New (Item, Label => "");
            Append (Menu, Item);

            Gtk_New (Item, Label => -"Add configuration variable");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Add_Variable'Access),
               Selection_Context_Access (Context));
         end if;

         if Has_Project_Information (File_Context)
           and then Has_File_Information (File_Context)
         then
            Gtk_New (Item, Label => "");
            Append (Menu, Item);

            Gtk_New (Item, Label => -"Edit switches for "
                     & Base_Name (File_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (Edit_Switches'Access),
               Selection_Context_Access (Context));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Project_Editor_Contextual;

   ------------------------------------
   -- Project_Editor_Context_Factory --
   ------------------------------------

   function Project_Editor_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event_Widget);

      Context     : constant File_Selection_Context_Access :=
        new File_Selection_Context;
      V           : constant Project_Viewer := Project_Viewer (Object);
      Item        : Gtk_Menu_Item;
      Iter        : Gtk_Tree_Iter;

   begin
      --  ??? Should call Project_Editor_Contextual
      Iter := Find_Iter_For_Event (V.Tree, V.Model, Event);

      if Iter = Null_Iter then
         Set_File_Information
           (Context, Project => V.Current_Project);
      else
         if not Iter_Is_Selected (Get_Selection (V.Tree), Iter) then
            Unselect_All (Get_Selection (V.Tree));
            Select_Iter (Get_Selection (V.Tree), Iter);
         end if;

         declare
            File_Name   : constant String := Get_String
              (V.Model, Iter, File_Name_Column);
         begin
            Set_File_Information
              (Context,
               File    => Create (File_Name, Kernel, Use_Object_Path => False),
               Project      => V.Current_Project);
         end;
      end if;

      if Has_File_Information (Context) then
         Gtk_New (Item, -"Edit switches for all selected files");
         Add (Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Edit_Multiple_Switches'Access),
            Slot_Object => V);
      end if;

      return Selection_Context_Access (Context);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return Selection_Context_Access (Context);
   end Project_Editor_Context_Factory;

   ----------------------------
   -- Edit_Multiple_Switches --
   ----------------------------

   procedure Edit_Multiple_Switches
     (Viewer : access Gtk_Widget_Record'Class)
   is
      V         : constant Project_Viewer := Project_Viewer (Viewer);
      Selection : constant Gtk_Tree_Selection := Get_Selection (V.Tree);
      Length    : Natural := 0;
      Iter      : Gtk_Tree_Iter := Get_Iter_First (V.Model);

   begin
      while Iter /= Null_Iter loop
         if Iter_Is_Selected (Selection, Iter) then
            Length := Length + 1;
         end if;

         Next (V.Model, Iter);
      end loop;

      declare
         Names : File_Array (1 .. Length);
         N     : Natural := Names'First;
      begin
         Iter := Get_Iter_First (V.Model);

         while Iter /= Null_Iter loop
            if Iter_Is_Selected (Selection, Iter) then
               --  ??? This isn't really a full file name
               Names (N) := Create_From_Base
                 (Base_Name =>
                    Get_String (V.Model, Iter, File_Name_Column));
               N := N + 1;
            end if;

            Next (V.Model, Iter);
         end loop;

         if Edit_Switches_For_Files (V.Kernel, V.Current_Project, Names) then
            --  Temporarily block the handlers so that the the editor is not
            --  cleared, or we would lose the selection
            Gtk.Handlers.Handler_Block (V.Kernel, V.View_Changed_Id);
            Recompute_View (V.Kernel);
            Gtk.Handlers.Handler_Unblock (V.Kernel, V.View_Changed_Id);

            Iter := Get_Iter_First (V.Model);

            while Iter /= Null_Iter loop
               if Iter_Is_Selected (Selection, Iter) then
                  Project_Viewers_Set (V, Iter);
               end if;

               Next (V.Model, Iter);
            end loop;
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Edit_Multiple_Switches;

   -------------------
   -- Add_Main_File --
   -------------------

   procedure Add_Main_File
     (Editor : access Executables_Editor_Record'Class; File : String)
   is
      Iter : Gtk_Tree_Iter;
   begin
      Append (Editor.Executables, Iter, Null_Iter);
      Set
        (Editor.Executables,
         Iter   => Iter,
         Column => 0,
         Value  => File);

      Set
        (Editor.Executables,
         Iter   => Iter,
         Column => 1,
         Value  => Get_Executable_Name (Editor.Project, File));

      Set
        (Editor.Executables,
         Iter   => Iter,
         Column => 2,
         Value  => True);
   end Add_Main_File;

   -------------------
   -- Add_Main_Unit --
   -------------------

   procedure Add_Main_Unit (Editor : access Gtk_Widget_Record'Class) is
      Ed : constant Executables_Editor := Executables_Editor (Editor);

      procedure Add_File (Base_Dir : String := "");
      --  Open a file selection dialog in Base_Dir (or current directory if
      --  null) and if a file is selected, add this file to the list of main
      --  units. If Base_Dir is null, change the current directory to
      --  Dir_Name (File).

      --------------
      -- Add_File --
      --------------

      procedure Add_File (Base_Dir : String := "") is
         File : constant Virtual_File := Select_File
           (Title             => -"Select the main file to add",
            Base_Directory    => Base_Dir,
            Parent            => Gtk_Window (Get_Toplevel (Editor)),
            Use_Native_Dialog => Get_Pref (Ed.Kernel, Use_Native_Dialogs),
            Kind              => Unspecified,
            History           => Get_History (Ed.Kernel));

      begin
         if File /= VFS.No_File then
            Add_Main_File (Ed, Base_Name (File));
         end if;
      end Add_File;

   begin
      if Ed.Project /= No_Project then
         declare
            Dirs : constant Name_Id_Array := Source_Dirs (Ed.Project);
         begin
            if Dirs'Length = 0 then
               Add_File (Dir_Name (Project_Path (Ed.Project)));
            else
               Add_File (Get_String (Dirs (Dirs'First)));
            end if;
         end;
      else
         Add_File;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Add_Main_Unit;

   ----------------------
   -- Remove_Main_Unit --
   ----------------------

   procedure Remove_Main_Unit (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Executables_Editor := Executables_Editor (Editor);
      Iter, Tmp : Gtk_Tree_Iter;
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.Tree_View);

   begin
      Iter := Get_Iter_First (Ed.Executables);

      while Iter /= Null_Iter loop
         Tmp := Iter;
         Next (Ed.Executables, Iter);

         if Iter_Is_Selected (Selection, Tmp) then
            Remove (Ed.Executables, Tmp);
         end if;
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Remove_Main_Unit;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access Main_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      Box      : Executables_Editor;
      Hbox     : Gtk_Box;
      Label    : Gtk_Label;
      Column   : Gtk_Tree_View_Column;
      Renderer : Gtk_Cell_Renderer_Text;
      Col      : Gint;
      pragma Unreferenced (Page, Full_Project, Col);

      Scrolled : Gtk_Scrolled_Window;
      Bbox     : Gtk_Vbutton_Box;
      Button2  : Gtk_Button;

      Has_Support_For_Executables : constant Boolean :=
        GNAT_Version (Kernel) >= "3.16";

   begin
      Box := new Executables_Editor_Record;
      Box.Project := Project;
      Box.Kernel := Kernel_Handle (Kernel);
      Initialize_Vbox (Box, Homogeneous => False);

      Gtk_New
        (Label,
         -"Main files are the targets for the builder and the debugger");
      Set_Padding (Label, Xpad => 0, Ypad => 5);
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Box, Label, Expand => False, Fill => True);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => True, Fill => True);

      Gtk_New (Box.Executables,
               (0 => GType_String, 1 => GType_String, 2 => GType_Boolean));
      Gtk_New (Box.Tree_View, Model => Box.Executables);
      Set_Mode (Get_Selection (Box.Tree_View), Selection_Multiple);

      Gtk_New (Renderer);
      Gtk_New (Column);
      Set_Clickable (Column, True);
      Set_Sort_Column_Id (Column, 0);
      Set_Title (Column, -"Source file name");
      Pack_Start (Column, Renderer, Expand => True);
      Add_Attribute (Column, Renderer, "text", 0);
      Col := Append_Column (Box.Tree_View, Column);

      Gtk_New (Column);
      Set_Clickable (Column, True);
      Set_Sort_Column_Id (Column, 1);
      Set_Title (Column, -"Executable name");
      Pack_Start (Column, Renderer, Expand => True);
      Add_Attribute (Column, Renderer, "text", 1);

      if Has_Support_For_Executables then
         Add_Attribute (Column, Renderer, "editable", 2);
      end if;

      Col := Append_Column (Box.Tree_View, Column);

      Set_Editable_And_Callback (Box.Executables, Renderer, 1);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Add (Scrolled, Box.Tree_View);
      Pack_Start (Hbox, Scrolled, Expand => True, Fill => True);
      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Start);
      Pack_Start (Hbox, Bbox, Expand => False, Fill => False);

      Gtk_New_From_Stock (Button2, Stock_Add);
      Pack_Start (Bbox, Button2);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Add_Main_Unit'Access),
         Slot_Object => Box);

      Gtk_New_From_Stock (Button2, Stock_Remove);
      Pack_Start (Bbox, Button2);
      Widget_Callback.Object_Connect
        (Button2, "clicked",
         Widget_Callback.To_Marshaller (Remove_Main_Unit'Access),
         Slot_Object => Box);

      if Project /= No_Project then
         declare
            Mains : Argument_List := Get_Attribute_Value
              (Project, Attribute => Main_Attribute);
         begin
            for M in Mains'Range loop
               Add_Main_File (Box, Mains (M).all);
            end loop;

            Free (Mains);
         end;
      end if;

      Show_All (Box);
      return Gtk_Widget (Box);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Main_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean
   is
      pragma Unreferenced (Page);
      Editor       : constant Executables_Editor :=
        Executables_Editor (Widget);
      Num_Children : constant Gint := N_Children (Editor.Executables);
      New_Mains    : Argument_List (1 .. Integer (Num_Children));
      Iter         : Gtk_Tree_Iter := Get_Iter_First (Editor.Executables);
      N            : Natural := New_Mains'First;
      Changed      : Boolean := False;

      Has_Support_For_Executables : constant Boolean :=
        GNAT_Version (Kernel) >= "3.16";

   begin
      Assert (Me, Project = Ref_Project,
              "Invalid project when modifying main files");

      --  First get the list of main files
      while Iter /= Null_Iter loop
         New_Mains (N) := new String'
           (Get_String (Editor.Executables, Iter, 0));

         if Has_Support_For_Executables then
            Changed := Changed or else
              Get_Executable_Name (Project, New_Mains (N).all) /=
              Get_String (Editor.Executables, Iter, 1);
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Executable_Attribute,
               Attribute_Index    => New_Mains (N).all,
               Value              => Get_String (Editor.Executables, Iter, 1));
         end if;

         N := N + 1;
         Next (Editor.Executables, Iter);
      end loop;

      --  ??? We only know how to get the value of an attribute in the current
      --  scenario, so the project might be reported as modified too often when
      --  changing multiple scenarios
      if Project /= No_Project then
         declare
            Old_Mains    : Argument_List := Get_Attribute_Value
              (Project, Attribute => Main_Attribute);
            Found : Boolean;
         begin
            Changed := Changed or else not Is_Equal (Old_Mains, New_Mains);

            if Changed and then Has_Support_For_Executables then
               for M in Old_Mains'Range loop
                  Found := False;
                  for Nm in New_Mains'Range loop
                     if New_Mains (Nm).all = Old_Mains (M).all then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Delete_Attribute
                       (Project            => Project,
                        Scenario_Variables => Scenario_Variables,
                        Attribute          => Executable_Attribute,
                        Attribute_Index    => Old_Mains (M).all);
                  end if;
               end loop;
            end if;

            Free (Old_Mains);
         end;
      else
         Changed := True;
      end if;

      if Changed then
         if New_Mains'Length /= 0 then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Main_Attribute,
               Values             => New_Mains);
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Main_Attribute);
         end if;
      end if;

      Free (New_Mains);

      return Changed;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access Source_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Page);
      Src_Dir_Selection : Directory_Tree.Directory_Selector;
   begin
      if Project /= No_Project then
         declare
            Initial_Dirs_Id : constant Name_Id_Array := Source_Dirs
              (Project);
            Initial_Dirs : Argument_List (Initial_Dirs_Id'Range);
         begin
            for J in Initial_Dirs_Id'Range loop
               Initial_Dirs (J) :=
                 new String'(Get_String (Initial_Dirs_Id (J)));
            end loop;

            Gtk_New
              (Src_Dir_Selection,
               Initial_Directory => Project_Directory (Project),
               Multiple_Directories => True,
               Busy_Cursor_On => Get_Window (Get_Main_Window (Kernel)),
               Initial_Selection => Initial_Dirs);
            Free (Initial_Dirs);
         end;
      else
         declare
            Initial_Dirs : Argument_List (1 .. 1);
            Dir          : constant String := Dir_Name (Full_Project);
         begin
            Initial_Dirs (1) := new String'(Dir);
            Gtk_New
              (Src_Dir_Selection,
               Initial_Directory    => Dir,
               Multiple_Directories => True,
               Busy_Cursor_On       => Get_Window (Get_Main_Window (Kernel)),
               Initial_Selection    => Initial_Dirs);
            Free (Initial_Dirs);
         end;
      end if;

      Show_All (Src_Dir_Selection);
      return Gtk_Widget (Src_Dir_Selection);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page               : access Source_Editor_Record;
      Project            : Project_Type;
      Kernel             : access Kernel_Handle_Record'Class;
      Widget             : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project        : Project_Type) return Boolean
   is
      pragma Unreferenced (Page);
      Dirs     : Argument_List := Get_Multiple_Selection
        (Directory_Selector (Widget));
      Equal    : Boolean := False;
      Prj_Dir  : constant String := Project_Directory (Project);
      Tmp      : GNAT.OS_Lib.String_Access;
      Relative : constant Boolean :=
        Get_Paths_Type (Project) = Projects.Relative
        or else (Get_Paths_Type (Project) = From_Pref
                 and then Get_Pref (Kernel, Generate_Relative_Paths));
      Initial_Dirs_Id : constant Name_Id_Array := Source_Dirs (Project);
      Initial_Dirs : Argument_List (Initial_Dirs_Id'Range);

   begin
      Assert (Me, Project = Ref_Project,
              "Invalid project when modifying main files");

      if Relative then
         for J in Dirs'Range loop
            Tmp := Dirs (J);
            Dirs (J) := new String'(Relative_Path_Name (Tmp.all, Prj_Dir));
            Free (Tmp);
         end loop;
      end if;

      for J in Initial_Dirs_Id'Range loop
         declare
            Str : constant String := Get_String (Initial_Dirs_Id (J));
         begin
            --  Initial_Dirs will always contain an unnormalized,
            --  absolute path, therefore we need to trim it first before
            --  comparing the old and new values.

            if Relative then
               Initial_Dirs (J) := new String'
                 (Relative_Path_Name (Str, Prj_Dir));
            else
               Initial_Dirs (J) := new String'(Str);
            end if;
         end;
      end loop;

      Equal := Is_Equal (Dirs, Initial_Dirs);
      Free (Initial_Dirs);

      if not Equal then
         Trace (Me, "Source dirs modified");
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Source_Dirs_Attribute,
            Values             => Dirs,
            Attribute_Index    => "",
            Prepend            => False);
      end if;

      Free (Dirs);

      return not Equal;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   ----------------------
   -- Set_Sensitive_Cb --
   ----------------------

   procedure Set_Sensitive_Cb
     (Check : access Glib.Object.GObject_Record'Class;
      User  : GObject) is
   begin
      Set_Sensitive
        (Gtk_Widget (User), not Get_Active (Gtk_Check_Button (Check)));
   end Set_Sensitive_Cb;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access Object_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Page);
      Obj_Dir : Object_Editor_Widget;
      Label   : Gtk_Label;
      Box     : Gtk_Box;
      Event   : Gtk_Event_Box;
      Button  : Gtk_Button;

   begin
      Obj_Dir := new Object_Editor_Widget_Record;
      Initialize_Vbox (Obj_Dir, Homogeneous => False);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Obj_Dir, Box, Expand => False, Padding => 10);

      Gtk_New (Event);
      Pack_Start (Box, Event, Expand => False);
      Gtk_New (Label, -"Build directory:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Directory that will contain all the object files generated"
                 & " by the compiler (.o files, .ali files for Ada, ...)"));

      Gtk_New (Obj_Dir.Obj_Dir);
      Set_Width_Chars (Obj_Dir.Obj_Dir, 0);
      Pack_Start (Box, Obj_Dir.Obj_Dir, Expand => True);

      if Project /= No_Project then
         Set_Text (Obj_Dir.Obj_Dir,
                   Name_As_Directory (GNAT.OS_Lib.Normalize_Pathname
                    (Object_Path (Project, False))));
      else
         Set_Text (Obj_Dir.Obj_Dir, Dir_Name (Full_Project));
      end if;

      Gtk_New (Button, -"Browse");
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Obj_Dir.Obj_Dir);

      Gtk_New (Obj_Dir.Same, -"Exec directory is the same as build directory");
      Pack_Start (Obj_Dir, Obj_Dir.Same, Expand => False);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Obj_Dir, Box, Expand => False);

      Gtk_New (Event);
      Pack_Start (Box, Event, Expand => False);
      Gtk_New (Label, -"Exec directory:");
      Set_Alignment (Label, 0.0, 0.5);
      Add (Event, Label);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Directory that will contain the executables resulting from"
                 & " the compilation"));

      Gtk_New (Obj_Dir.Exec_Dir);
      Set_Width_Chars (Obj_Dir.Exec_Dir, 0);
      Pack_Start (Box, Obj_Dir.Exec_Dir, Expand => True);

      if Project /= No_Project then
         Set_Text
           (Obj_Dir.Exec_Dir,
            Name_As_Directory (GNAT.OS_Lib.Normalize_Pathname
                                 (Executables_Directory (Project))));
      else
         Set_Text (Obj_Dir.Exec_Dir, Dir_Name (Full_Project));
      end if;

      Gtk_New (Button, -"Browse");
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Browse_Location'Access),
         Slot_Object => Obj_Dir.Exec_Dir);

      Object_User_Callback.Connect
        (Obj_Dir.Same, "toggled",
         Object_User_Callback.To_Marshaller (Set_Sensitive_Cb'Access),
         User_Data => GObject (Obj_Dir.Exec_Dir));
      Object_User_Callback.Connect
        (Obj_Dir.Same, "toggled",
         Object_User_Callback.To_Marshaller (Set_Sensitive_Cb'Access),
         User_Data => GObject (Button));

      Set_Active (Obj_Dir.Same,
                  Project = No_Project
                  or else Get_Attribute_Value
                    (Project => Project,
                     Attribute => Exec_Dir_Attribute,
                     Default => "@@@@") = "@@@@");

      Show_All (Obj_Dir);
      return Gtk_Widget (Obj_Dir);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Object_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean
   is
      pragma Unreferenced (Page);
      Obj_Dir : constant Object_Editor_Widget := Object_Editor_Widget (Widget);
      New_Dir, Exec_Dir : GNAT.OS_Lib.String_Access;
      Changed : Boolean := False;
      Project_Uses_Relative_Paths : constant Boolean :=
        Get_Paths_Type (Project) = Relative
        or else (Get_Paths_Type (Project) = From_Pref
                 and then Get_Pref (Kernel, Generate_Relative_Paths));
      Exec : constant String := Name_As_Directory
        (Normalize_Pathname (Executables_Directory (Project)));

   begin
      Assert (Me, Project = Ref_Project,
              "Invalid project when modifying main files");

      if Project_Uses_Relative_Paths then
         New_Dir := new String'(Relative_Path_Name
            (Get_Text (Obj_Dir.Obj_Dir), Project_Directory (Project)));
      else
         New_Dir := new String'(Name_As_Directory
           (Normalize_Pathname (Get_Text (Obj_Dir.Obj_Dir))));
      end if;

      if Get_Active (Obj_Dir.Same) then
         Exec_Dir := new String'(New_Dir.all);
      elsif Project_Uses_Relative_Paths then
         Exec_Dir := new String'(Relative_Path_Name
           (Get_Text (Obj_Dir.Exec_Dir), Project_Directory (Project)));
      else
         Exec_Dir := new String'(Name_As_Directory
           (Normalize_Pathname (Get_Text (Obj_Dir.Exec_Dir))));
      end if;

      if Get_Text (Obj_Dir.Obj_Dir) /= Name_As_Directory
        (Normalize_Pathname (Object_Path (Project, False)))
      then
         if New_Dir.all = "" then
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Obj_Dir_Attribute);

         else
            if not Is_Directory (New_Dir.all) then
               if Message_Dialog
                 (Msg => New_Dir.all
                  & (-" is not a directory, would you like to create it ?"),
                  Title => -"Directory not found",
                  Dialog_Type => Information,
                  Buttons => Button_Yes or Button_No) = Button_Yes
               then
                  begin
                     Make_Dir (New_Dir.all);
                  exception
                     when Directory_Error =>
                        null;
                  end;
               end if;
            end if;

            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Obj_Dir_Attribute,
               Value              => New_Dir.all);
         end if;

         Trace (Me, "Object directory was modified");
         Changed := True;
      end if;

      if (Get_Active (Obj_Dir.Same)
          and then Get_Text (Obj_Dir.Obj_Dir) /= Exec)
        or else (not Get_Active (Obj_Dir.Same)
                 and then Get_Text (Obj_Dir.Exec_Dir) /= Exec)
      then
         Trace (Me, "Exec directory was modified");
         Changed := True;

         if Exec_Dir.all = ""
           or else Get_Active (Obj_Dir.Same)
         then
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Exec_Dir_Attribute);

         else
            if not Is_Directory (Exec_Dir.all) then
               if Message_Dialog
                 (Msg => Exec_Dir.all
                  & (-" is not a directory, would you like to create it ?"),
                  Title => -"Directory not found",
                  Dialog_Type => Information,
                  Buttons => Button_Yes or Button_No) = Button_Yes
               then
                  begin
                     Make_Dir (Exec_Dir.all);
                  exception
                     when Directory_Error =>
                        null;
                  end;
               end if;
            end if;

            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Exec_Dir_Attribute,
               Value              => Exec_Dir.all);
         end if;
      end if;

      Free (New_Dir);
      Free (Exec_Dir);

      return Changed;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page : access Switches_Editor_Record;
      Project : Project_Type;
      Full_Project : String;
      Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      pragma Unreferenced (Page, Full_Project);
      Switches : Switches_Editors.Switches_Edit;
   begin
      Gtk_New (Switches, Kernel);
      Show_All (Switches);
      Set_Switches (Switches, Project);

      return Gtk_Widget (Switches);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Switches_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean
   is
      pragma Unreferenced (Kernel, Page, Ref_Project);
   begin
      return Generate_Project
        (Switches           => Switches_Edit (Widget),
         Project            => Project,
         Scenario_Variables => Scenario_Variables,
         Files              => (1 .. 0 => VFS.No_File));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page         : access Switches_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project      : Project_Type := No_Project;
      Languages    : GNAT.OS_Lib.Argument_List)
   is
      pragma Unreferenced (Page, Project);
   begin
      Set_Visible_Pages (Switches_Edit (Widget), Languages);
   end Refresh;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access Naming_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Full_Project);
      Editor : Naming_Editor;
   begin
      if Project /= No_Project then
         Gtk_New (Editor, Kernel, Project);
         Show (Editor);
      else
         Gtk_New (Editor, Kernel,
                  Known_Languages (Get_Language_Handler (Kernel)));
         Show (Editor);
      end if;

      Page.Kernel := Kernel_Handle (Kernel);
      return Gtk_Widget (Editor);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Naming_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean
   is
      pragma Unreferenced (Page, Kernel, Ref_Project);
   begin
      return Create_Project_Entry
        (Naming_Editor (Widget),
         Project            => Project,
         Scenario_Variables => Scenario_Variables);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page         : access Naming_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project      : Project_Type := No_Project;
      Languages    : GNAT.OS_Lib.Argument_List) is
   begin
      Set_Visible_Pages
        (Naming_Editor (Widget), Page.Kernel, Languages, Project);
   end Refresh;

   -----------------------------
   -- Project_Command_Handler --
   -----------------------------

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Instance   : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_Project_Class (Kernel));
      Project    : constant Project_Type := Get_Data (Instance);
   begin
      if Command = "add_main_unit" then
         declare
            Args : Argument_List (1 .. Number_Of_Arguments (Data) - 1);
         begin
            for Index in 2 .. Number_Of_Arguments (Data) loop
               Args (Index - 1) := new String'(Nth_Arg (Data, Index));
            end loop;

            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables (Kernel),
               Attribute          => Main_Attribute,
               Values             => Args,
               Prepend            => True);
            Recompute_View (Kernel);
            Free (Args);
         end;

      elsif Command = "remove_dependency" then
         Name_Parameters (Data, Remove_Dep_Cmd_Parameters);
         declare
            Instance2 : constant Class_Instance :=
              Nth_Arg (Data, 2, Get_Project_Class (Kernel));
            Project2  : constant Project_Type := Get_Data (Instance2);
         begin
            Remove_Imported_Project (Project, Project2);
            Recompute_View (Kernel);
         end;

      elsif Command = "sources" then
         Name_Parameters (Data, Sources_Cmd_Parameters);
         declare
            Full_Path : constant Boolean := Nth_Arg (Data, 2, False);
            Recursive : constant Boolean := Nth_Arg (Data, 3, False);
            Sources   : File_Array_Access := Get_Source_Files
              (Project    => Project,
               Recursive  => Recursive,
               Full_Path  => Full_Path);
         begin
            Set_Return_Value_As_List (Data);
            for S in Sources'Range loop
               Set_Return_Value
                 (Data, Create_File (Get_Script (Data), Sources (S)));
            end loop;
            Unchecked_Free (Sources);
         end;

      elsif Command = "source_dirs" then
         Name_Parameters (Data, Source_Dirs_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Dirs      : String_Array_Access := Source_Dirs
              (Project, Recursive => Recursive);
         begin
            Set_Return_Value_As_List (Data);

            for D in Dirs'Range loop
               Set_Return_Value (Data, Dirs (D).all);
            end loop;

            Free (Dirs);
         end;

      elsif Command = "object_dirs" then
         Name_Parameters (Data, Source_Dirs_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Object    : constant String := Object_Path (Project, Recursive);
            Iter      : Path_Iterator := Start (Object);
         begin
            Set_Return_Value_As_List (Data);

            while not At_End (Object, Iter) loop
               Set_Return_Value (Data, Current (Object, Iter));
               Iter := Next (Object, Iter);
            end loop;
         end;

      elsif Command = "add_source_dir" then
         Name_Parameters (Data, Add_Source_Dir_Cmd_Parameters);
         declare
            Dir : constant String := Nth_Arg (Data, 2);
            Dirs : Argument_List := (1 => new String'(Dir));
         begin
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables (Get_Kernel (Data)),
               Attribute          => Source_Dirs_Attribute,
               Values             => Dirs,
               Attribute_Index    => "",
               Prepend            => True);
            Free (Dirs);
         end;

      elsif Command = "remove_source_dir" then
         Name_Parameters (Data, Add_Source_Dir_Cmd_Parameters);
         declare
            Dir : constant String := Nth_Arg (Data, 2);
            Dirs : Argument_List := Get_Attribute_Value
              (Project, Source_Dirs_Attribute);
            Index : Natural := Dirs'Last;
         begin
            for D in Dirs'Range loop
               if File_Equal (Dirs (D).all, Dir) then
                  Free (Dirs (D));
                  Dirs (D .. Dirs'Last - 1) := Dirs (D + 1 .. Dirs'Last);
                  Index := Index - 1;
               end if;
            end loop;

            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables (Get_Kernel (Data)),
               Attribute          => Source_Dirs_Attribute,
               Values             => Dirs (Dirs'First .. Index),
               Attribute_Index    => "");
            Free (Dirs);
         end;

      end if;
   end Project_Command_Handler;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed
     (Object : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
      Filename : constant String := Normalize_Pathname
        (Project_Path (Get_Project (Kernel)), Resolve_Links => False);
   begin
      if Filename /= "" then
         Add_To_History (Kernel, Project_History_Key, Filename);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end On_Project_Changed;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Page : in out Project_Editor_Page_Record) is
   begin
      Free (Page.Label);
      Free (Page.Toc);
      Free (Page.Title);
   end Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page      : access Project_Editor_Page_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project   : Project_Type := No_Project;
      Languages : Argument_List)
   is
      pragma Unreferenced (Page, Widget, Project, Languages);
   begin
      null;
   end Refresh;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Page : access Project_Editor_Page_Record'Class)
      return String is
   begin
      return Page.Label.all;
   end Get_Label;

   -------------
   -- Get_Toc --
   -------------

   function Get_Toc (Page : access Project_Editor_Page_Record'Class)
      return String is
   begin
      return Page.Toc.all;
   end Get_Toc;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Page : access Project_Editor_Page_Record'Class) return String is
   begin
      return Page.Title.all;
   end Get_Title;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Page : access Project_Editor_Page_Record'Class) return Selector_Flags is
   begin
      return Page.Flags;
   end Get_Flags;

   ----------------------------------
   -- Register_Project_Editor_Page --
   ----------------------------------

   procedure Register_Project_Editor_Page
     (Kernel : access Kernel_Handle_Record'Class;
      Page   : Project_Editor_Page;
      Label  : String;
      Toc    : String;
      Title  : String;
      Flags     : Selector_Flags := Multiple_Projects or Multiple_Scenarios;
      Ref_Page  : String := "";
      Add_After : Boolean := True)
   is
      pragma Unreferenced (Kernel);
      Tmp : Project_Editor_Page_Array_Access;
      Pos : Natural;
   begin
      if Prj_Editor_Module_ID = null then
         Trace (Me, "Register_Project_Editor_Page: module not registered");
         return;
      end if;

      Tmp := Prj_Editor_Module_ID.Project_Editor_Pages;

      if Tmp = null then
         Prj_Editor_Module_ID.Project_Editor_Pages :=
           new Project_Editor_Page_Array (1 .. 1);
      else
         Prj_Editor_Module_ID.Project_Editor_Pages :=
           new Project_Editor_Page_Array (Tmp'First .. Tmp'Last + 1);
         Prj_Editor_Module_ID.Project_Editor_Pages (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Page.Flags := Flags;
      Page.Label := new String'(Label);
      Page.Toc   := new String'(Toc);
      Page.Title := new String'(Title);

      Tmp := Prj_Editor_Module_ID.Project_Editor_Pages;

      Pos := Tmp'Last;

      if Ref_Page /= "" then
         for J in Tmp'First .. Tmp'Last - 1 loop
            if Tmp (J).Label.all = Ref_Page then
               if Add_After then
                  Pos := J + 1;
               elsif J > Tmp'First then
                  Pos := J - 1;
               else
                  Pos := J;
               end if;
               exit;
            end if;
         end loop;
      end if;

      Tmp (Pos + 1 .. Tmp'Last) := Tmp (Pos .. Tmp'Last - 1);
      Tmp (Pos) := Page;
   end Register_Project_Editor_Page;

   --------------------------------
   -- Project_Editor_Pages_Count --
   --------------------------------

   function Project_Editor_Pages_Count
     (Kernel : access Kernel_Handle_Record'Class) return Natural
   is
      pragma Unreferenced (Kernel);
   begin
      if Prj_Editor_Module_ID.Project_Editor_Pages = null then
         return 0;
      else
         return Prj_Editor_Module_ID.Project_Editor_Pages'Length;
      end if;
   end Project_Editor_Pages_Count;

   ---------------------------------
   -- Get_Nth_Project_Editor_Page --
   ---------------------------------

   function Get_Nth_Project_Editor_Page
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return Project_Editor_Page
   is
      pragma Unreferenced (Kernel);
   begin
      if Prj_Editor_Module_ID.Project_Editor_Pages /= null
        and then Num <= Prj_Editor_Module_ID.Project_Editor_Pages'Length
      then
         return Prj_Editor_Module_ID.Project_Editor_Pages
           (Prj_Editor_Module_ID.Project_Editor_Pages'First + Num - 1);
      end if;
      return null;
   end Get_Nth_Project_Editor_Page;

   ----------------------------
   -- Register_Switches_Page --
   ----------------------------

   procedure Register_Switches_Page
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Creator : access Switches_Page_Creator_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Tmp : Page_Array_Access;
   begin
      if Prj_Editor_Module_ID /= null then
         if Prj_Editor_Module_ID.Switches_Pages = null then
            Prj_Editor_Module_ID.Switches_Pages :=
              new Pages_Array'(1 => (Switches_Page_Creator (Creator), null));
         else
            Tmp := Prj_Editor_Module_ID.Switches_Pages;
            Prj_Editor_Module_ID.Switches_Pages := new Pages_Array'
              (Tmp.all & Switches_Page_Creator_Data'
                 (Switches_Page_Creator (Creator), null));
            Unchecked_Free (Tmp);
         end if;
      else
         Trace (Me, "Register_Switches_Page: module not registered");
      end if;
   end Register_Switches_Page;

   ---------------------------
   -- Get_Nth_Switches_Page --
   ---------------------------

   function Get_Nth_Switches_Page
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Num    : Positive)
      return Switches_Editors.Switches_Editor_Page
   is
      Pages  : Page_Array_Access := Prj_Editor_Module_ID.Switches_Pages;
   begin
      if Pages = null or else Num not in Pages'Range then
         return null;
      end if;

      if Pages (Num).Page = null and then Pages (Num).Creator /= null then
         Pages (Num).Page := Create (Pages (Num).Creator, Kernel);
         Destroy (Pages (Num).Creator);
      end if;

      if Pages (Num).Page /= null then
         Ref (Pages (Num).Page);
      end if;

      return Pages (Num).Page;
   end Get_Nth_Switches_Page;

   -------------------------
   -- Switches_Page_Count --
   -------------------------

   function Switches_Page_Count
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) return Natural
   is
      pragma Unreferenced (Kernel);
   begin
      if Prj_Editor_Module_ID.Switches_Pages = null then
         return 0;
      else
         return Prj_Editor_Module_ID.Switches_Pages'Length;
      end if;
   end Switches_Page_Count;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Creator : in out Switches_Page_Creator_Record) is
      pragma Unreferenced (Creator);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Creator : in out Switches_Page_Creator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Switches_Page_Creator_Record'Class, Switches_Page_Creator);
   begin
      if Creator /= null then
         Destroy (Creator.all);
         Unchecked_Free (Creator);
      end if;
   end Destroy;

   -----------------------------------
   -- Register_Naming_Scheme_Editor --
   -----------------------------------

   procedure Register_Naming_Scheme_Editor
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Language : String;
      Creator  : Naming_Scheme_Editor_Creator)
   is
      pragma Unreferenced (Kernel);
      Tmp : Naming_Pages_Array_Access;
      Lang : constant Name_Id := Get_String (To_Lower (Language));
   begin
      if Prj_Editor_Module_ID /= null then
         if Prj_Editor_Module_ID.Naming_Pages = null then
            Prj_Editor_Module_ID.Naming_Pages :=
              new Naming_Pages_Array'(1 => (Lang, Creator));
         else
            Tmp := Prj_Editor_Module_ID.Naming_Pages;
            Prj_Editor_Module_ID.Naming_Pages := new Naming_Pages_Array'
              (Tmp.all & Naming_Page'(Lang, Creator));
            Unchecked_Free (Tmp);
         end if;
      else
         Trace (Me, "Register_Naming_Scheme_Editor: module not registered");
      end if;
   end Register_Naming_Scheme_Editor;

   ----------------------------
   -- Get_Naming_Scheme_Page --
   ----------------------------

   function Get_Naming_Scheme_Page
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Language : String) return Naming_Editors.Language_Naming_Editor
   is
      Lang : constant Name_Id := Get_String (To_Lower (Language));
   begin
      if Prj_Editor_Module_ID.Naming_Pages /= null then
         for Num in Prj_Editor_Module_ID.Naming_Pages'Range loop
            if Prj_Editor_Module_ID.Naming_Pages (Num).Language = Lang then
               return Prj_Editor_Module_ID.Naming_Pages (Num).Creator (Kernel);
            end if;
         end loop;
      end if;
      return null;
   end Get_Naming_Scheme_Page;

   --------------------
   -- On_File_Edited --
   --------------------

   procedure On_File_Edited
     (Object : access GObject_Record'Class;
      Args   : Gtk_Args;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      if Status (Get_Project (Kernel)) = Default then
         declare
            File         : constant String := To_String (Args, 1);
            Dir          : aliased String := Dir_Name (File);
            Current_Dirs : String_Array_Access := Source_Dirs
              (Get_Project (Kernel), Recursive => False);

         begin
            for C in Current_Dirs'Range loop
               if Current_Dirs (C).all = Dir then
                  Free (Current_Dirs);
                  return;
               end if;
            end loop;

            Free (Current_Dirs);

            if Message_Dialog
              (Msg            => -"Add directory " & ASCII.LF
                 & Dir & ASCII.LF
                 & (-" to the default project ?"),
               Dialog_Type    => Confirmation,
               Buttons        => Button_Yes or Button_No,
               Default_Button => Button_Yes,
               Title          => -"Adding directory to project",
               Justification  => Justify_Left,
               Parent         => Get_Main_Window (Kernel)) = Button_Yes
            then
               Update_Attribute_Value_In_Scenario
                 (Get_Project (Kernel),
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute          => Source_Dirs_Attribute,
                  Values             => (1 => Dir'Unchecked_Access),
                  Prepend            => True);
               Recompute_View (Kernel);
            end if;
         end;
      end if;
   end On_File_Edited;

   --------------------------
   -- Parsing_Switches_XML --
   --------------------------

   procedure Parsing_Switches_XML
     (Kernel : access Kernel_Handle_Record'Class;
      Page   : Switches_Editor_Page;
      Table  : access Gtk_Table_Record'Class;
      Lines  : Natural;
      Cols   : Natural;
      Node   : Node_Ptr)
   is
      type Frame_Array is array (1 .. Lines, 1 .. Cols) of Gtk_Frame;
      type Box_Array   is array (1 .. Lines, 1 .. Cols) of Gtk_Box;
      type Size_Group_Array is array (1 .. Lines, 1 .. Cols) of Gtk_Size_Group;

      Frames : Frame_Array;
      Boxes  : Box_Array;
      Sizes  : Size_Group_Array;

      procedure Coordinates_From_Node
        (N : Node_Ptr; Line, Col : out Natural);
      --  Get the line and column from N

      procedure Process_Title_Node      (N : Node_Ptr);
      procedure Process_Check_Node      (N : Node_Ptr);
      procedure Process_Spin_Node       (N : Node_Ptr);
      procedure Process_Radio_Node      (N : Node_Ptr);
      procedure Process_Combo_Node      (N : Node_Ptr);
      procedure Process_Popup_Node      (N : Node_Ptr);
      procedure Process_Dependency_Node (N : Node_Ptr);
      procedure Process_Expansion_Node  (N : Node_Ptr);
      --  Process a child node (resp. <title>, <check>, <spin>, <radio>,
      --  <combo>, <popup>, <dependency>, <expansion>)

      function Process_Radio_Entry_Nodes
        (Parent : Node_Ptr) return Radio_Switch_Array;
      function Process_Combo_Entry_Nodes
        (Parent : Node_Ptr) return Combo_Switch_Array;
      --  Return the contents of all the <radio-entry> and
      --  <combo-entry> nodes of Parent

      ---------------------------
      -- Coordinates_From_Node --
      ---------------------------

      procedure Coordinates_From_Node
        (N : Node_Ptr; Line, Col : out Natural)
      is
      begin
         Line :=
           Natural'Min (Safe_Value (Get_Attribute (N, "line", "1")), Lines);
         Col :=
           Natural'Min (Safe_Value (Get_Attribute (N, "column", "1")), Cols);
      end Coordinates_From_Node;

      ------------------------
      -- Process_Title_Node --
      ------------------------

      procedure Process_Title_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Line_Span : constant Integer := Safe_Value
           (Get_Attribute (N, "line-span", "1"));
         Col_Span : constant Integer := Safe_Value
           (Get_Attribute (N, "column-span", "1"));
      begin
         Coordinates_From_Node (N, Line, Col);
         Set_Label (Frames (Line, Col), N.Value.all);

         if Line_Span = 0 or else Col_Span = 0 then
            Set_Child_Visible (Frames (Line, Col), False);

         elsif Line_Span /= 1 or else Col_Span /= 1 then
            Ref (Frames (Line, Col));
            Remove (Table, Frames (Line, Col));
            Attach (Table,
                    Frames (Line, Col),
                    Left_Attach   => Guint (Col - 1),
                    Right_Attach  => Guint (Col + Col_Span - 1),
                    Top_Attach    => Guint (Line - 1),
                    Bottom_Attach => Guint (Line + Line_Span - 1));
            Unref (Frames (Line, Col));
         end if;
      end Process_Title_Node;

      -----------------------------
      -- Process_Dependency_Node --
      -----------------------------

      procedure Process_Dependency_Node (N : Node_Ptr) is
         Master_Page   : constant String := Get_Attribute (N, "master-page");
         Master_Switch : constant String := Get_Attribute (N, "master-switch");
         Slave_Page    : constant String := Get_Attribute (N, "slave-page");
         Slave_Switch  : constant String := Get_Attribute (N, "slave-switch");
         Master_Status : constant String :=
           Get_Attribute (N, "master-status", "true");
         Slave_Status  : constant String :=
           Get_Attribute (N, "slave-status", "true");
      begin
         if Master_Page = ""
           or else Master_Switch = ""
           or else Slave_Page = ""
           or else Slave_Switch = ""
         then
            Insert
              (Kernel,
                 -("Invalid <dependency> node in custom file,"
                   & " all attributes must be specified"),
               Mode => Glide_Kernel.Console.Error);
            return;
         end if;

         Add_Dependency
           (Page,
            Master_Page,
            Master_Switch,
            Master_Status = "true" or else Master_Status = "on",
            Slave_Page,
            Slave_Switch,
            Slave_Status = "true" or else Slave_Status = "on");
      end Process_Dependency_Node;

      -------------------------------
      -- Process_Radio_Entry_Nodes --
      -------------------------------

      function Process_Radio_Entry_Nodes
        (Parent : Node_Ptr) return Radio_Switch_Array
      is
         N : Node_Ptr := Parent.Child;
         Num_Children : Natural := 0;
      begin
         while N /= null loop
            if N.Tag.all = "radio-entry" then
               Num_Children := Num_Children + 1;
            end if;
            N := N.Next;
         end loop;

         declare
            Buttons : Radio_Switch_Array (1 .. Num_Children);
         begin
            N := Parent.Child;
            for B in Buttons'Range loop
               while  N.Tag.all /= "radio-entry" loop
                  N := N.Next;
               end loop;

               declare
                  Label   : constant String := Get_Attribute (N, "label");
                  Switch  : constant String := Get_Attribute (N, "switch");
                  Tip     : constant String := Get_Attribute (N, "tip");
               begin
                  if Label = "" or else Switch = "" then
                     Insert
                       (Kernel,
                          -("Invalid <radio-entry> node in custom file,"
                            & " requires a label and a switch attributes"),
                        Mode => Glide_Kernel.Console.Error);
                     return Buttons (1 .. 0);
                  end if;

                  Buttons (B) :=
                    (Label  => new String'(Label),
                     Switch => new String'(Switch),
                     Tip    => new String'(Tip));
               end;

               N := N.Next;
            end loop;

            return Buttons;
         end;
      end Process_Radio_Entry_Nodes;

      -------------------------------
      -- Process_Combo_Entry_Nodes --
      -------------------------------

      function Process_Combo_Entry_Nodes
        (Parent : Node_Ptr) return Combo_Switch_Array
      is
         N : Node_Ptr := Parent.Child;
         Num_Children : Natural := 0;
      begin
         while N /= null loop
            if N.Tag.all = "combo-entry" then
               Num_Children := Num_Children + 1;
            end if;
            N := N.Next;
         end loop;

         declare
            Buttons : Combo_Switch_Array (1 .. Num_Children);
         begin
            N := Parent.Child;
            for B in Buttons'Range loop
               while N.Tag.all /= "combo-entry" loop
                  N := N.Next;
               end loop;

               declare
                  Label : constant String := Get_Attribute (N, "label");
                  Value : constant String := Get_Attribute (N, "value");
               begin
                  if Label = "" or else Value = "" then
                     Insert
                       (Kernel,
                          -("Invalid <combo-entry> node in custom file,"
                            & " requires a label and a switch attributes"),
                        Mode => Glide_Kernel.Console.Error);
                     return Buttons (1 .. 0);
                  end if;

                  Buttons (B) :=
                    (Label => new String'(Label),
                     Value => new String'(Value));
               end;

               N := N.Next;
            end loop;

            return Buttons;
         end;
      end Process_Combo_Entry_Nodes;

      ------------------------
      -- Process_Radio_Node --
      ------------------------

      procedure Process_Radio_Node (N : Node_Ptr) is
         Line, Col : Natural;
      begin
         Coordinates_From_Node (N, Line, Col);
         Create_Radio (Page, Boxes (Line, Col), Process_Radio_Entry_Nodes (N));
      end Process_Radio_Node;

      ------------------------
      -- Process_Popup_Node --
      ------------------------

      procedure Process_Popup_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Table     : Gtk_Table;
         Lines : constant Integer :=
           Safe_Value (Get_Attribute (N, "lines", "1"));
         Cols  : constant Integer :=
           Safe_Value (Get_Attribute (N, "columns", "1"));
      begin
         Coordinates_From_Node (N, Line, Col);
         if Label = "" then
            Insert
              (Kernel,
                 -("Invalid <popup> node in custom file,"
                   & " requires a label attributes"),
               Mode => Glide_Kernel.Console.Error);
            return;
         end if;

         Gtk_New (Table, Guint (Lines), Guint (Cols), Homogeneous => False);
         Parsing_Switches_XML (Kernel, Page, Table, Lines, Cols, N);

         Pack_Start
           (Boxes (Line, Col), Create_Popup (Label, Table), False, False);
      end Process_Popup_Node;

      ------------------------
      -- Process_Combo_Node --
      ------------------------

      procedure Process_Combo_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
         Tip       : constant String := Get_Attribute (N, "tip");
         No_Switch : constant String := Get_Attribute (N, "noswitch");
         No_Digit  : constant String := Get_Attribute (N, "nodigit");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Switch = "" then
            Insert (Kernel,
                      -("Invalid <combo> node in custom file, requires"
                        & " a switch attributes"),
                    Mode => Glide_Kernel.Console.Error);
            return;
         end if;

         Pack_Start
           (Boxes (Line, Col), Create_Combo
              (Page, Label,
               Switch            => Switch,
               Default_No_Switch => No_Switch,
               Default_No_Digit  => No_Digit,
               Buttons           => Process_Combo_Entry_Nodes (N),
               Tip               => Tip,
               Label_Size_Group  => Sizes (Line, Col)),
            False, False);
      end Process_Combo_Node;

      -----------------------
      -- Process_Spin_Node --
      -----------------------

      procedure Process_Spin_Node  (N : Node_Ptr) is
         Line, Col : Natural;
         Label   : constant String := Get_Attribute (N, "label");
         Switch  : constant String := Get_Attribute (N, "switch");
         Tip     : constant String := Get_Attribute (N, "tip");
         Min     : constant Integer := Safe_Value
           (Get_Attribute (N, "min", "1"));
         Max     : constant Integer := Safe_Value
           (Get_Attribute (N, "max", "1"));
         Default     : constant Integer := Safe_Value
           (Get_Attribute (N, "default", "1"));
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <spin> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => Glide_Kernel.Console.Error);
            return;
         end if;

         Create_Spin
           (Page, Boxes (Line, Col), Label, Switch, Min, Max, Default, Tip,
            Sizes (Line, Col));
      end Process_Spin_Node;

      ------------------------
      -- Process_Check_Node --
      ------------------------

      procedure Process_Check_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Label   : constant String := Get_Attribute (N, "label");
         Switch  : constant String := Get_Attribute (N, "switch");
         Tip     : constant String := Get_Attribute (N, "tip");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <check> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => Glide_Kernel.Console.Error);
            return;
         end if;

         Create_Check (Page, Boxes (Line, Col), Label, Switch, Tip);
      end Process_Check_Node;

      ----------------------------
      -- Process_Expansion_Node --
      ----------------------------

      procedure Process_Expansion_Node (N : Node_Ptr) is
         Switch : constant String := Get_Attribute (N, "switch");
         Alias  : constant String := Get_Attribute (N, "alias");
         N2 : Node_Ptr := N.Child;
         Num_Children : Natural := 0;
      begin
         if Switch = "" then
            Insert (Kernel,
                      -("Invalid <expansion> node in custom file, requires"
                        & " a switch attributes"),
                    Mode => Glide_Kernel.Console.Error);
            return;
         end if;

         if Alias /= "" then
            Add_Coalesce_Switch (Page, Switch, Alias);
         end if;

         while N2 /= null loop
            if N2.Tag.all = "entry" then
               Num_Children := Num_Children + 1;
            end if;
            N2 := N2.Next;
         end loop;

         if Num_Children /= 0 then
            declare
               Switches : Switches_Editors.Cst_Argument_List
                 (1 .. Num_Children);
            begin
               N2 := N.Child;
               for S in Switches'Range loop
                  while N2.Tag.all /= "entry" loop
                     N2 := N2.Next;
                  end loop;

                  Switches (S) := new String'
                    (Get_Attribute (N2, "switch"));
                  N2 := N2.Next;
               end loop;

               Add_Custom_Expansion (Page, Switch, Switches);
            end;
         end if;
      end Process_Expansion_Node;


      N      : Node_Ptr;

   begin
      --  Create all the frames

      for L in Frame_Array'Range (1) loop
         for C in Frame_Array'Range (2) loop
            Gtk_New (Frames (L, C));
            Set_Border_Width (Frames (L, C), 5);
            Attach (Table, Frames (L, C), Guint (C - 1), Guint (C),
                    Guint (L - 1), Guint (L));
            Gtk_New_Vbox (Boxes (L, C), False, 0);
            Add (Frames (L, C), Boxes (L, C));

            Gtk_New (Sizes (L, C));
         end loop;
      end loop;

      --  Parse the contents of the XML tree

      N := Node.Child;
      while N /= null loop
         begin
            if N.Tag.all = "title" then
               Process_Title_Node (N);
            elsif N.Tag.all = "check" then
               Process_Check_Node (N);
            elsif N.Tag.all = "spin" then
               Process_Spin_Node (N);
            elsif N.Tag.all = "radio" then
               Process_Radio_Node (N);
            elsif N.Tag.all = "combo" then
               Process_Combo_Node (N);
            elsif N.Tag.all = "popup" then
               Process_Popup_Node (N);
            elsif N.Tag.all = "dependency" then
               Process_Dependency_Node (N);
            elsif N.Tag.all = "expansion" then
               Process_Expansion_Node (N);
            else
               Insert (Kernel,
                       -"Invalid xml tag child for <switches>: "
                       & N.Tag.all);
            end if;

         exception
            when E : others =>
               Trace (Me, "Unexpected exception " & Exception_Information (E));
         end;

         N := N.Next;
      end loop;
   end Parsing_Switches_XML;

   ------------
   -- Create --
   ------------

   function Create
     (Creator : access XML_Switches_Record;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Switches_Editors.Switches_Editor_Page
   is
      Lines : constant Integer :=
        Safe_Value (Get_Attribute (Creator.XML_Node, "lines", "1"));
      Cols : constant Integer :=
        Safe_Value (Get_Attribute (Creator.XML_Node, "columns", "1"));
      Page : Switches_Editor_Page;
      Tool : constant Tool_Properties_Record :=
        Get_Tool_Properties (Kernel, Creator.Tool_Name.all);
   begin
      Gtk_New (Page, Creator.Tool_Name.all,
               Tool.Project_Package.all,
               Tool.Project_Index.all,
               Guint (Lines), Guint (Cols), Get_Tooltips (Kernel));

      if Creator.Languages /= null then
         for L in Creator.Languages'Range loop
            Add_Language (Page, Creator.Languages (L).all);
         end loop;
      end if;

      Parsing_Switches_XML (Kernel, Page, Page, Lines, Cols, Creator.XML_Node);
      return Page;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Creator : in out XML_Switches_Record) is
   begin
      Free (Creator.XML_Node);
      Free (Creator.Tool_Name);
      Free (Creator.Languages);
   end Destroy;

   ----------------------------------
   -- Get_Languages_From_Tool_Node --
   ----------------------------------

   function Get_Languages_From_Tool_Node
     (N : Node_Ptr) return Argument_List_Access
   is
      Result : Argument_List_Access;
      Child : Node_Ptr := N.Child;
   begin
      while Child /= null loop
         if Child.Tag.all = "language" then
            Append (Result, (1 => new String'(Child.Value.all)));
            To_Lower (Result (Result'Last).all);
         end if;
         Child := Child.Next;
      end loop;

      return Result;
   end Get_Languages_From_Tool_Node;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);
      N : Node_Ptr := Node;
      N2 : Node_Ptr;
      Creator : XML_Switches;
   begin
      while N /= null loop
         if N.Tag.all = "tool" then
            declare
               Tool_Name : constant String := Get_Attribute (N, "name");
            begin
               if Tool_Name = "" then
                  Glide_Kernel.Console.Insert
                    (Kernel, -"Invalid <tool> node, no name specified");
               else
                  N2 := N.Child;
                  while N2 /= null loop
                     if N2.Tag.all = "switches" then
                        Creator := new XML_Switches_Record'
                          (Switches_Page_Creator_Record with
                           XML_Node  => Deep_Copy (N2),
                           Tool_Name => new String'(Tool_Name),
                           Languages => Get_Languages_From_Tool_Node (N));
                        Register_Switches_Page (Kernel, Creator);
                     end if;

                     N2 := N2.Next;
                  end loop;
               end if;
            end;
         end if;

         N := N.Next;
      end loop;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
      Reopen_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Prj_Editor_Module_ID := new Prj_Editor_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Prj_Editor_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Project_Editor_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Project_Editor_Contextual'Access,
         Customization_Handler   => Customize'Access);

      Register_Menu (Kernel, Project, null, Ref_Item => -"Edit",
                     Add_Before => False);

      Register_Menu (Kernel, Project, -"_New...", "", On_New_Project'Access,
                     Ref_Item => -"Open...", Add_Before => True);


      Reopen_Menu := Register_Menu
        (Kernel, Project, -"_Recent", "",
         null, Ref_Item => -"Open...", Add_Before => False);
      Associate (Get_History (Kernel).all,
                 Project_History_Key,
                 Reopen_Menu,
                 new On_Reopen'(Menu_Callback_Record with
                                Kernel => Kernel_Handle (Kernel)));

      Kernel_Callback.Connect
        (Kernel, Project_Changed_Signal,
         Kernel_Callback.To_Marshaller (On_Project_Changed'Access),
         Kernel_Handle (Kernel));

      --  ??? Disabled for now, pending resolution of related problems
      --  encountered during testing

      --  Kernel_Callback.Connect
      --    (Kernel, File_Edited_Signal,
      --     On_File_Edited'Access,
      --     Kernel_Handle (Kernel));

      Register_Menu
        (Kernel, Project, -"Edit File _Switches", "",
         On_Edit_Switches'Access, Ref_Item => -"Recent", Add_Before => False);
      Register_Menu
        (Kernel, Project, -"Edit Project _Properties", "",
         On_Project_Properties'Access, Ref_Item => -"Recent",
         Add_Before => False);
      Register_Menu
        (Kernel, Project, -"Save _All", "",
         Save_All_Projects'Access, Ref_Item => -"Edit Project Properties",
         Add_Before => False);
      Register_Menu
        (Kernel, Project, -"R_ecompute Project", "",
         On_Project_Recompute'Access, Ref_Item => -"Edit File Switches",
         Add_Before => False);

      Register_Project_Editor_Page
        (Kernel,
         Page  => new Source_Editor_Record,
         Label => -"Sources",
         Toc   => -"Selecting sources",
         Title => -"Please select the source directories for this project",
         Flags => Multiple_Scenarios);
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Object_Editor_Record,
         Label => -"Objects",
         Toc   => -"Build directory",
         Title => -"Please select the build directory for this project",
         Flags => Multiple_Scenarios);
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Naming_Editor_Record,
         Label => -"Naming",
         Toc   => -"Naming scheme",
         Title => -"Please select the naming scheme to use");
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Main_Editor_Record,
         Label => -"Main files",
         Toc   => -"Selecting main units",
         Title => -"Please select the main units for this project",
         Flags => Multiple_Scenarios);
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Switches_Editor_Record,
         Label => -"Switches",
         Toc   => -"Switches",
         Title => -"Please select the switches to build the project");

      Register_Command
        (Kernel,
         Command      => "add_main_unit",
         Params       => "(main1, [main2 ...])",
         Description  =>
           -("Add some main units to the current project, and for the"
             & " current scenario. The project is not saved automatically."),
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "remove_dependency",
         Params       => Parameter_Names_To_Usage (Remove_Dep_Cmd_Parameters),
         Description  => -("Remove a dependency between two projects."),
         Minimum_Args => Remove_Dep_Cmd_Parameters'Length,
         Maximum_Args => Remove_Dep_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "sources",
         Params       =>
           Parameter_Names_To_Usage (Sources_Cmd_Parameters, 2),
         Return_Value => "list",
         Description  =>
           -("Return the list of source files for this project."
             & " If recursive is true, then all sources from imported projects"
             & " are also returned. Otherwise, only the direct sources are"
             & " returned. If full_path is true, then the full path of the"
             & " files is returned, otherwise only the base names are"
             & " returned. The basenames of the returned files are alwayse"
             & " unique: not two files with the same basenames are returned,"
             & " and the one returned is the first one see while traversing"
             & " the project hierarchy."),
         Minimum_Args => 0,
         Maximum_Args => Sources_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "source_dirs",
         Params       =>
           Parameter_Names_To_Usage (Source_Dirs_Cmd_Parameters, 1),
         Return_Value => "list",
         Description  =>
           -("Return the list of source directories for this project."
             & " If Recursive is True, the source directories of imported"
             & " projects is also returned. There might be duplicate"
             & " directories in the returned list"),
         Minimum_Args => Source_Dirs_Cmd_Parameters'Length - 1,
         Maximum_Args => Source_Dirs_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "object_dirs",
         Params       =>
           Parameter_Names_To_Usage (Source_Dirs_Cmd_Parameters, 1),
         Return_Value => "list",
         Description  =>
           -("Return the list of object directories for this project."
             & " If Recursive is True, the source directories of imported"
             & " projects is also returned. There might be duplicate"
             & " directories in the returned list"),
         Minimum_Args => Source_Dirs_Cmd_Parameters'Length - 1,
         Maximum_Args => Source_Dirs_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Params       =>
           Parameter_Names_To_Usage (Add_Source_Dir_Cmd_Parameters),
         Command      => "add_source_dir",
         Description  =>
            -("Add a new source directory to the project. The new directory"
              & " is added in front of the source path. You should call"
              & " recompute() after calling this method, to recompute the list"
              & " of source files. The directory is added for the current"
              & " value of the scenario variables only"),
         Minimum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Maximum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel,
         Params       =>
           Parameter_Names_To_Usage (Add_Source_Dir_Cmd_Parameters),
         Command      => "remove_source_dir",
         Description  =>
            -("Remove a source directory from the project. You should call"
              & " recompute() after calling this method, to recompute the list"
              & " of source files. The directory is added for the current"
              & " value of the scenario variables only"),
         Minimum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Maximum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);

   end Register_Module;

end Project_Viewers;
