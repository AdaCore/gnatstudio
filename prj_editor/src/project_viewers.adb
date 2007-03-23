-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                             AdaCore                               --
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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Case_Util;               use GNAT.Case_Util;
with GNAT.Strings;                 use GNAT.Strings;
with GNAT.OS_Lib;

with Gdk.Color;                    use Gdk.Color;
with Gdk.Event;                    use Gdk.Event;

with Glib;                         use Glib;
with Glib.Convert;                 use Glib.Convert;
with Glib.Object;                  use Glib.Object;
with Glib.Xml_Int;                 use Glib.Xml_Int;

with Gtk.Box;                      use Gtk.Box;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Size_Group;               use Gtk.Size_Group;
with Gtk.Table;                    use Gtk.Table;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_Store;               use Gtk.Tree_Store;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Widget;                   use Gtk.Widget;

with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;

with Basic_Types;
with Commands.Interactive;         use Commands, Commands.Interactive;
with Creation_Wizard.Dependencies; use Creation_Wizard.Dependencies;
with Creation_Wizard.Extending;    use Creation_Wizard.Extending;
with Creation_Wizard.Selector;     use Creation_Wizard.Selector;
with File_Utils;                   use File_Utils;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Console;           use GPS.Kernel.Console;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Scripts;           use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;    use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                   use GPS.Kernel;
with GUI_Utils;                    use GUI_Utils;
with Language_Handlers;            use Language_Handlers;
with Naming_Editors;               use Naming_Editors;
with Namet;                        use Namet;
with Prj;
with Project_Properties;           use Project_Properties;
with Projects.Editor;              use Projects, Projects.Editor;
with Projects.Registry;            use Projects.Registry;
with Remote;                       use Remote;
with String_Utils;                 use String_Utils;
with Switches_Editors;             use Switches_Editors;
with System;
with Traces;                       use Traces;
with VFS;                          use VFS;
with Variable_Editors;             use Variable_Editors;

package body Project_Viewers is

   Me : constant Debug_Handle := Create ("Project_Viewers");

   type Project_Editor_Page_Array is array (Natural range <>)
     of Project_Editor_Page;
   type Project_Editor_Page_Array_Access is access Project_Editor_Page_Array;

   type Switches_Page_Creator_Data is record
      Creator : Switches_Page_Creator;
      Page    : Switches_Editors.Switches_Editor_Page;
   end record;
   --  Page is the cached version of the page. It is created lazily, and never
   --  destroyed afterwards.

   type XML_Switches_Record is new Switches_Page_Creator_Record with record
      XML_Node  : Node_Ptr;   --  The <switches> node
      Tool_Name : GNAT.Strings.String_Access;
      Languages : GNAT.Strings.String_List_Access;
   end record;
   type XML_Switches is access all XML_Switches_Record'Class;

   function Create
     (Creator : access XML_Switches_Record;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
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

   procedure Customize
     (Module : access Prj_Editor_Module_Id_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   Prj_Editor_Module_ID : Prj_Editor_Module_Id_Access;
   --  Id for the project editor module

   Project_Switches_Name : constant String := "Project Switches";

   Recursive_Cst : aliased constant String := "recursive";
   Directory_Cst : aliased constant String := "directory";
   Imported_Cst  : aliased constant String := "imported";
   Src_Path_Cst  : aliased constant String := "sources";
   Obj_Path_Cst  : aliased constant String := "objects";
   Name_Cst      : aliased constant String := "name";
   Path_Cst      : aliased constant String := "path";
   Sources_Cmd_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List :=
                              (1 => Recursive_Cst'Access);
   Source_Dirs_Cmd_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List
     := (1 => Recursive_Cst'Access);
   Languages_Cmd_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List
     := (1 => Recursive_Cst'Access);
   Add_Source_Dir_Cmd_Parameters :
     constant GPS.Kernel.Scripts.Cst_Argument_List :=
     (1 => Directory_Cst'Access);
   Remove_Dep_Cmd_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List
     := (1 => Imported_Cst'Access);
   Add_Predefined_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List
     := (1 => Src_Path_Cst'Access, 2 => Obj_Path_Cst'Access);
   Rename_Cmd_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Path_Cst'Access);
   Add_Dep_Cmd_Parameters : constant GPS.Kernel.Scripts.Cst_Argument_List :=
                              (1 => Path_Cst'Access);

   Base_File_Name_Column     : constant := 0;
   Absolute_File_Name_Column : constant := 1;
   Compiler_Switches_Column  : constant := 2;
   Compiler_Color_Column     : constant := 3;

   type Project_Viewer_Record is new Gtk.Box.Gtk_Hbox_Record with record
      Tree  : Gtk.Tree_View.Gtk_Tree_View;
      Model : Gtk.Tree_Store.Gtk_Tree_Store;
      --  The actual contents of the viewer

      Default_Switches_Color : Gdk.Color.Gdk_Color;
      --  Color to use when displaying switches that are set at the project
      --  level, rather than file specific

      Kernel : GPS.Kernel.Kernel_Handle;

      View_Changed_Blocked : Boolean := False;
      --  True if the hook for "project_view_changed" should be ignored

      Current_Project : Projects.Project_Type;
      --  The project to which the files currently in the viewer belong. This
      --  indicates which project file should be normalized when a modification
      --  takes place.
   end record;
   type Project_Viewer is access all Project_Viewer_Record'Class;

   procedure Gtk_New
     (Viewer : out Project_Viewer;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new project viewer.
   --  Every time the selection in Explorer changes, the info displayed in
   --  the viewer is changed.

   procedure Initialize
     (Viewer : access Project_Viewer_Record'Class;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
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
      File_Name        : VFS.Virtual_File;
      Directory_Filter : String := "");
   --  Append a new line in the current page of Viewer, for File_Name.
   --  The exact contents inserted depends on the current view.
   --  The file is automatically searched in all the source directories of
   --  Project_View.

   function Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Event : Gdk_Event)
     return Boolean;
   --  Callback when a row/column has been selected in the clist

   type Context_Hook_Record is new Function_With_Args with record
      Viewer : Project_Viewer;
   end record;
   type Context_Hook is access all Context_Hook_Record'Class;
   procedure Execute
     (Hook   : Context_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called every time the selection has changed in the tree

   procedure Explorer_Selection_Changed
     (Viewer  : access Project_Viewer_Record'Class;
      Context : Selection_Context);
   --  Same as above, but work directly on a context.

   procedure Project_Editor_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Return the current context for the contextual menu

   procedure On_Edit_Switches
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Edit Project Switches menu

   procedure On_Project_Properties
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Edit Project Properties menu

   procedure Save_All_Projects
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Save the project associated with the kernel, and all its imported
   --  projects.

   procedure Edit_Multiple_Switches
     (Viewer : access Gtk_Widget_Record'Class);
   --  Edit the switches for all the files selected in Viewer.

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle the interactive commands related to the project editor

   procedure Project_Static_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle static methods for the GPS.Project class

   procedure Update_Contents
     (Viewer    : access Project_Viewer_Record'Class;
      Project   : Project_Type;
      Directory : String := "";
      File      : Virtual_File := VFS.No_File);
   --  Update the contents of the viewer.
   --  Directory and File act as filters for the information that is displayed.

   procedure Project_Viewers_Set
     (Viewer : access Project_Viewer_Record'Class;
      Iter   : Gtk_Tree_Iter);
   --  Set the contents of the line Iter in the model. It is assumed the file
   --  name has already been set on that line

   procedure Parsing_Switches_XML
     (Kernel            : access Kernel_Handle_Record'Class;
      Page              : Switches_Editor_Page;
      Table             : access Gtk_Table_Record'Class;
      Lines             : Natural;
      Cols              : Natural;
      Node              : Node_Ptr;
      Default_Separator : String);
   --  Subprogram for Parse_Switches_Page, this is used to handle both the
   --  <switches> and the <popup> tags

   type Preferences_Hook_Record is new Function_No_Args with record
      Viewer : Project_Viewer;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   procedure Execute
     (Hook : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Hook called when the preferences change

   type Project_View_Hook_Record is new Function_No_Args with record
      Viewer : Project_Viewer;
   end record;
   type Project_View_Hook is access all Project_View_Hook_Record'Class;
   procedure Execute
     (Hook : Project_View_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Hook called when the project view changes

   --------------------------
   -- Project editor pages --
   --------------------------

   type Switches_Editor_Record is
     new Project_Editor_Page_Record with null record;
--     function Create_Content
--       (Page : access Switches_Editor_Record;
--        Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
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
      Languages    : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean;
   procedure Refresh
     (Page         : access Switches_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project      : Project_Type := No_Project;
      Languages    : GNAT.Strings.String_List);

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
      Languages    : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type) return Boolean;
   procedure Refresh
     (Page         : access Naming_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project      : Project_Type := No_Project;
      Languages    : GNAT.Strings.String_List);

   ----------------------
   -- Contextual menus --
   ----------------------

   type Save_Project_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Save_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Edit_Project_Source_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Edit_Project_Source_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ------------------------
   -- Main files editors --
   ------------------------

   function Get_Languages_From_Tool_Node
     (N : Node_Ptr) return String_List_Access;
   --  Return the list of languages for that <tool> node

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Prj_Editor_Module_Id_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Editor_Page_Record'Class, Project_Editor_Page);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Switches_Page_Creator_Record'Class, Switches_Page_Creator);
   begin
      if Module.Project_Editor_Pages /= null then
         for P in Module.Project_Editor_Pages'Range loop
            Destroy (Module.Project_Editor_Pages (P).all);
            Unchecked_Free (Module.Project_Editor_Pages (P));
         end loop;

         Unchecked_Free (Module.Project_Editor_Pages);
      end if;

      if Module.Switches_Pages /= null then
         for P in Module.Switches_Pages'Range loop
            Destroy (Module.Switches_Pages (P).Creator);
            Unchecked_Free (Module.Switches_Pages (P).Creator);
         end loop;
         Unchecked_Free (Module.Switches_Pages);
      end if;

      Unchecked_Free (Module.Switches_Pages);
      Unchecked_Free (Module.Naming_Pages);

      Destroy (Module_ID_Record (Module));
   end Destroy;

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
         Col3  : Gint; Value3 : Gdk_Color);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr_ptr");

      File_Name  : constant Virtual_File := Create
        (Get_String (Viewer.Model, Iter, Absolute_File_Name_Column));
      Language   : constant Name_Id := Get_String
        (Get_Language_From_File
         (Language_Handler (Get_Language_Handler (Viewer.Kernel)),
          File_Name));
      Color      : Gdk_Color;
      Value      : Prj.Variable_Value;
      Is_Default : Boolean;
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
                Locale_To_UTF8
                  (To_String (Get_Tree (Viewer.Current_Project), Value))
                  & ASCII.NUL,
                Compiler_Color_Column, Color);
   end Project_Viewers_Set;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      File_Name        : VFS.Virtual_File;
      Directory_Filter : String := "")
   is
      Iter : Gtk_Tree_Iter;
   begin
      if Directory_Filter = ""
        or else Dir_Name (File_Name).all = Name_As_Directory (Directory_Filter)
      then
         Append (Viewer.Model, Iter, Null_Iter);
         Set (Viewer.Model,
              Iter,
              Base_File_Name_Column,
              Base_Name (File_Name));
         Set (Viewer.Model,
              Iter,
              Absolute_File_Name_Column,
              Full_Name (File_Name).all);
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
      V    : constant Project_Viewer := Project_Viewer (Viewer);
      Iter : Gtk_Tree_Iter;
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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Select_Row;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook : Project_View_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      if not Hook.Viewer.View_Changed_Blocked then
         Clear (Hook.Viewer.Model);  --  ??? Should delete selectively

         if Hook.Viewer.Current_Project /= No_Project then
            Hook.Viewer.Current_Project := Get_Project (Kernel);
            Show_Project (Hook.Viewer, Hook.Viewer.Current_Project);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Execute;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
     (Viewer    : access Project_Viewer_Record'Class;
      Project   : Project_Type;
      Directory : String := "";
      File      : Virtual_File := VFS.No_File)
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

      if File /= VFS.No_File then
         Iter := Get_Iter_First (Viewer.Model);
         while Iter /= Null_Iter loop
            if Create
              (Get_String
                 (Viewer.Model, Iter, Absolute_File_Name_Column)) = File
            then
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
      Context : Selection_Context) is
   begin
      --  If the context is invalid, keep the currently displayed lines, so
      --  that when a new MDI child is selected, the contents of the viewer is
      --  not necessarily reset.

      if Has_File_Information (Context) then
         Update_Contents
           (Viewer,
            Project_Information (Context),
            Dir_Name (File_Information (Context)).all,
            File_Information (Context));
      else
         Update_Contents (Viewer, Get_Project (Viewer.Kernel));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Explorer_Selection_Changed;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Context_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D     : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      --  Do nothing if we forced the selection change ourselves. For instance,
      --  when a new switch editor is created in On_Edit_Switches, to avoid
      --  doing extra work.
      if Child = null
        or else Get_Widget (Child) /= Gtk_Widget (Hook.Viewer)
      then
         Explorer_Selection_Changed (Hook.Viewer, D.Context);
      end if;
   end Execute;

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
     (Viewer : access Project_Viewer_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Column_Types : constant GType_Array :=
                       (Base_File_Name_Column     => GType_String,
                        Absolute_File_Name_Column => GType_String,
                        Compiler_Switches_Column  => GType_String,
                        Compiler_Color_Column     => Gdk_Color_Type);

      Scrolled     : Gtk_Scrolled_Window;
      Col          : Gtk_Tree_View_Column;
      Render       : Gtk_Cell_Renderer_Text;
      Col_Number   : Gint;
      Hook         : Preferences_Hook;
      Hook2        : Project_View_Hook;
      Hook3        : Context_Hook;
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
      Set_Sort_Column_Id (Col, Base_File_Name_Column);
      Add_Attribute (Col, Render, "text", Base_File_Name_Column);

      Clicked (Col);

      Gtk_New (Col);
      Col_Number := Append_Column (Viewer.Tree, Col);
      Set_Title (Col, -"Compiler switches");
      Set_Resizable (Col, True);
      Set_Reorderable (Col, True);
      Gtk_New (Render);
      Pack_Start (Col, Render, False);
      Set_Sort_Column_Id (Col, Compiler_Switches_Column);
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

      Hook3 := new Context_Hook_Record'
        (Function_With_Args with Viewer => Project_Viewer (Viewer));
      Add_Hook
        (Kernel, Context_Changed_Hook, Hook3,
         Name => "project_viewer.context_changed", Watch => GObject (Viewer));

      Hook2 := new Project_View_Hook_Record'
        (Function_No_Args with Viewer => Project_Viewer (Viewer));
      Add_Hook
        (Kernel, Project_View_Changed_Hook, Hook2,
         Name => "project_viewer.project_view_changed",
         Watch => GObject (Viewer));

      Hook := new Preferences_Hook_Record'
        (Function_No_Args with Viewer => Project_Viewer (Viewer));
      Execute (Hook.all, Kernel);
      Add_Hook
        (Kernel, Preferences_Changed_Hook,
         Hook,
         Name => "project_viewer.preferences_changed",
         Watch => GObject (Viewer));

      Show_All (Viewer);
   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Hook.Viewer.Default_Switches_Color :=
        Get_Pref (Default_Switches_Color);
      --  ??? Do we need to change the model to reflect this change
   end Execute;

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record'Class;
      Project_Filter   : Project_Type;
      Directory_Filter : String := "")
   is
      Files : File_Array_Access :=
                Get_Source_Files (Project_Filter, Recursive => False);
   begin
      Viewer.Current_Project := Project_Filter;

      for F in Files'Range loop
         Append_Line (Viewer, Files (F), Directory_Filter);
      end loop;

      Unchecked_Free (Files);
   end Show_Project;

   ------------------------
   -- On_Editor_Switches --
   ------------------------

   procedure On_Edit_Switches
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Child   : GPS_MDI_Child;
      Viewer  : Project_Viewer;

   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Viewer_Record'Tag));

      if Child /= null then
         Raise_Child (Child);
         Viewer := Project_Viewer (Get_Widget (Child));
      else
         Gtk_New (Viewer, Kernel);
         Gtk_New (Child, Viewer,
                  Default_Width  => Get_Pref (Default_Widget_Width),
                  Default_Height => Get_Pref (Default_Widget_Height),
                  Group          => Group_Default,
                  Module         => Prj_Editor_Module_ID);
         Set_Title (Child, -"Switches editor");
         Put (Get_MDI (Kernel), Child);
      end if;

      --  The initial contents of the viewer should be read immediately from
      --  the explorer, without forcing the user to do a new selection.

      if Context /= No_Context then
         Explorer_Selection_Changed (Viewer, Context);
      else
         Update_Contents (Viewer, Get_Project (Kernel));
      end if;

      Set_Focus_Child (Child);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Edit_Switches;

   ---------------------------
   -- On_Project_Properties --
   ---------------------------

   procedure On_Project_Properties
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Project : Project_Type;

   begin
      if Has_Project_Information (Context) then
         Project := Project_Information (Context);
      else
         Project := Get_Project (Kernel);
      end if;

      Edit_Properties (Project, Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Project_Properties;

   -----------------------
   -- Save_All_Projects --
   -----------------------

   procedure Save_All_Projects
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Tmp : Boolean;
      pragma Unreferenced (Widget, Tmp);
   begin
      Tmp := Save_Project (Kernel, Get_Project (Kernel), Recursive => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Save_All_Projects;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Save_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project : constant Project_Type := Project_Information (Context.Context);

   begin
      if Save_Project (Kernel, Project) then
         return Success;
      else
         return Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_Project_Source_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Open_File_Editor
        (Kernel, Project_Path (Project_Information (Context.Context)));
      return Success;
   end Execute;

   ------------------------------------
   -- Project_Editor_Context_Factory --
   ------------------------------------

   procedure Project_Editor_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget, Kernel);
      V    : constant Project_Viewer := Project_Viewer (Object);
      Item : Gtk_Menu_Item;
      Iter : Gtk_Tree_Iter;

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
              (V.Model, Iter, Absolute_File_Name_Column);
         begin
            Set_File_Information
              (Context,
               File    => Create (File_Name),
               Project => V.Current_Project);
         end;
      end if;

      if Has_File_Information (Context) then
         Gtk_New (Item, -"Edit switches for all selected files");
         Add (Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate", Edit_Multiple_Switches'Access,
            Slot_Object => V);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
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
               Names (N) := Create
                 (Get_String (V.Model, Iter, Absolute_File_Name_Column));
               N := N + 1;
            end if;

            Next (V.Model, Iter);
         end loop;

         if Edit_Switches_For_Files (V.Kernel, V.Current_Project, Names) then
            --  Temporarily block the handlers so that the the editor is not
            --  cleared, or we would lose the selection
            V.View_Changed_Blocked := True;
            Recompute_View (V.Kernel);
            V.View_Changed_Blocked := False;

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
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Edit_Multiple_Switches;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access Switches_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget
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
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page               : access Switches_Editor_Record;
      Project            : Project_Type;
      Kernel             : access Kernel_Handle_Record'Class;
      Widget             : access Gtk_Widget_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project        : Project_Type) return Boolean
   is
      pragma Unreferenced (Kernel, Page, Ref_Project);
   begin
      return Generate_Project
        (Switches           => Switches_Edit (Widget),
         Project            => Project,
         Languages          => Languages,
         Scenario_Variables => Scenario_Variables,
         Files              => (1 .. 0 => VFS.No_File));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page      : access Switches_Editor_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project   : Project_Type := No_Project;
      Languages : GNAT.Strings.String_List)
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
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page               : access Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : access Kernel_Handle_Record'Class;
      Widget             : access Gtk_Widget_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project        : Project_Type) return Boolean
   is
      pragma Unreferenced (Page, Kernel, Ref_Project);
   begin
      return Create_Project_Entry
        (Naming_Editor (Widget),
         Project            => Project,
         Languages          => Languages,
         Scenario_Variables => Scenario_Variables);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return False;
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page      : access Naming_Editor_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project   : Project_Type := No_Project;
      Languages : GNAT.Strings.String_List) is
   begin
      Set_Visible_Pages
        (Naming_Editor (Widget), Page.Kernel, Languages, Project);
   end Refresh;

   ------------------------------------
   -- Project_Static_Command_Handler --
   ------------------------------------

   procedure Project_Static_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);

      function Remove_Redundant_Directories
        (Old_Path, New_Path : String) return String;
      --  Return New_Path after having removed the directories that have been
      --  found on Old_Path.

      ----------------------------------
      -- Remove_Redundant_Directories --
      ----------------------------------

      function Remove_Redundant_Directories
        (Old_Path, New_Path : String) return String
      is
         Returned_Path        : String (1 .. New_Path'Length);
         Returned_Path_Length : Integer := 0;
         New_It               : Path_Iterator := Start (New_Path);
         Old_It               : Path_Iterator;
         Found                : Boolean;
      begin
         while not At_End (New_Path, New_It) loop
            Old_It := Start (Old_Path);

            Found := False;

            declare
               New_Path_Item : constant String := Current (New_Path, New_It);
            begin
               while not At_End (Old_Path, Old_It) loop
                  if Current (Old_Path, Old_It) = New_Path_Item then
                     Found := True;
                     exit;
                  end if;

                  Old_It := Next (Old_Path, Old_It);
               end loop;

               if not Found then
                  if Returned_Path_Length /= 0 then
                     Returned_Path (Returned_Path_Length + 1) :=
                       GNAT.OS_Lib.Path_Separator;

                     Returned_Path_Length := Returned_Path_Length + 1;
                  end if;

                  Returned_Path
                    (Returned_Path_Length + 1
                     .. Returned_Path_Length + New_Path_Item'Length) :=
                    New_Path_Item;

                  Returned_Path_Length := Returned_Path_Length
                    + New_Path_Item'Length;
               end if;
            end;

            New_It := Next (New_Path, New_It);
         end loop;

         return Returned_Path (1 .. Returned_Path_Length);
      end Remove_Redundant_Directories;

   begin
      if Command = "add_predefined_paths" then
         Name_Parameters (Data, Add_Predefined_Parameters);
         declare
            Old_Src : constant String :=
              Get_Predefined_Source_Path (Get_Registry (Kernel).all);
            Old_Obj : constant String :=
              Get_Predefined_Object_Path (Get_Registry (Kernel).all);
            New_Src : constant String :=
              Remove_Redundant_Directories (Old_Src, Nth_Arg (Data, 1, ""));
            New_Obj : constant String :=
              Remove_Redundant_Directories (Old_Obj, Nth_Arg (Data, 2, ""));
         begin
            if New_Src /= "" then
               Set_Predefined_Source_Path
                 (Get_Registry (Kernel).all,
                  New_Src & GNAT.OS_Lib.Path_Separator & Old_Src);
            end if;

            if Old_Obj /= "" then
               Set_Predefined_Object_Path
                 (Get_Registry (Kernel).all,
                  New_Obj & GNAT.OS_Lib.Path_Separator & Old_Obj);
            end if;
         end;
      end if;
   end Project_Static_Command_Handler;

   -----------------------------
   -- Project_Command_Handler --
   -----------------------------

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Project : constant Project_Type := Get_Data (Data, 1);

      procedure Set_Error_Tmp (Str : String);
      --  Set an error

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error_Tmp (Str : String) is
      begin
         Set_Error_Msg (Data, Str);
      end Set_Error_Tmp;

   begin
      if Command = "add_main_unit" then
         declare
            Args : GNAT.Strings.String_List
              (1 .. Number_Of_Arguments (Data) - 1);
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
            Basic_Types.Free (Args);
         end;

      elsif Command = "rename" then
         Name_Parameters (Data, Rename_Cmd_Parameters);
         declare
            Name : constant String := Nth_Arg (Data, 2);
            Path : constant String :=
              Nth_Arg (Data, 3, Full_Name (Project_Directory (Project)).all);
         begin
            Rename_And_Move
              (Root_Project  => Get_Project (Kernel),
               Project       => Project,
               New_Name      => Name,
               New_Path      => Create (Path),
               Report_Errors => Set_Error_Tmp'Unrestricted_Access);
            Run_Hook (Kernel, Project_Changed_Hook);
         end;

      elsif Command = "remove_dependency" then
         Name_Parameters (Data, Remove_Dep_Cmd_Parameters);
         declare
            Project2  : constant Project_Type := Get_Data (Data, 2);
         begin
            Remove_Imported_Project (Project, Project2);
         end;

      elsif Command = "add_dependency" then
         Name_Parameters (Data, Add_Dep_Cmd_Parameters);
         declare
            Project2 : constant String  := GNAT.OS_Lib.Normalize_Pathname
              (Name => Nth_Arg (Data, 2));
            Relative : constant Boolean :=
              Get_Paths_Type (Project) = Projects.Relative
              or else (Get_Paths_Type (Project) = From_Pref
                       and then Get_Pref (Generate_Relative_Paths));
            Error    : Import_Project_Error;
            pragma Unreferenced (Error);
         begin
            Error := Add_Imported_Project
              (Root_Project              => Get_Project (Kernel),
               Project                   => Project,
               Imported_Project_Location => Create (Project2),
               Report_Errors             => Set_Error_Tmp'Unrestricted_Access,
               Use_Relative_Path         => Relative);
         end;

      elsif Command = "sources" then
         Name_Parameters (Data, Sources_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Sources   : File_Array_Access := Get_Source_Files
              (Project    => Project,
               Recursive  => Recursive);
         begin
            Set_Return_Value_As_List (Data);
            for S in Sources'Range loop
               Set_Return_Value
                 (Data, Create_File (Get_Script (Data), Sources (S)));
            end loop;
            Unchecked_Free (Sources);
         end;

      elsif Command = "languages" then
         Name_Parameters (Data, Languages_Cmd_Parameters);
         declare
            Langs : GNAT.Strings.String_List := Get_Languages
              (Project => Project, Recursive => Nth_Arg (Data, 2, False));
         begin
            Set_Return_Value_As_List (Data);
            for L in Langs'Range loop
               Set_Return_Value (Data, Langs (L).all);
            end loop;
            Basic_Types.Free (Langs);
         end;

      elsif Command = "source_dirs" then
         Name_Parameters (Data, Source_Dirs_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Dirs      : String_List_Access := Source_Dirs
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
            Dir     : constant String := Name_As_Directory
              (GNAT.OS_Lib.Normalize_Pathname
                 (Nth_Arg (Data, 2),
                  Directory => Full_Name (Project_Directory (Project)).all,
                  Resolve_Links => False));
            Dirs    : GNAT.Strings.String_List := (1 => new String'(Dir));
            Sources : String_List_Access :=
              Source_Dirs (Project, Recursive => False);
            Found   : Boolean := False;

         begin
            for S in Sources'Range loop
               if Sources (S).all = Dir then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables (Get_Kernel (Data)),
                  Attribute          => Source_Dirs_Attribute,
                  Values             => Dirs,
                  Attribute_Index    => "",
                  Prepend            => True);
            end if;

            Basic_Types.Free (Dirs);
            Free (Sources);
         end;

      elsif Command = "remove_source_dir" then
         Name_Parameters (Data, Add_Source_Dir_Cmd_Parameters);
         declare
            Dir : constant String := Nth_Arg (Data, 2);
            Dirs : GNAT.Strings.String_List := Get_Attribute_Value
              (Project, Source_Dirs_Attribute);
            Index : Natural := Dirs'Last;
         begin
            for D in Dirs'Range loop
               if File_Equal (Dirs (D).all, Dir, Build_Server) then
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
            Basic_Types.Free (Dirs);
         end;

      end if;
   end Project_Command_Handler;

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
      Languages : GNAT.Strings.String_List)
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
     (Kernel    : access Kernel_Handle_Record'Class;
      Page      : Project_Editor_Page;
      Label     : String;
      Toc       : String;
      Title     : String;
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
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Num    : Positive)
      return Switches_Editors.Switches_Editor_Page
   is
      Pages : constant Page_Array_Access :=
                Prj_Editor_Module_ID.Switches_Pages;
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Natural
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
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String;
      Creator  : Naming_Scheme_Editor_Creator)
   is
      pragma Unreferenced (Kernel);
      Tmp  : Naming_Pages_Array_Access;
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
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String) return Naming_Editors.Language_Naming_Editor
   is
      Lang : constant Name_Id := Get_String (To_Lower (Language));
   begin
      if Prj_Editor_Module_ID.Naming_Pages /= null then
         for Num in Prj_Editor_Module_ID.Naming_Pages'Range loop
            if Prj_Editor_Module_ID.Naming_Pages (Num).Language = Lang then
               return Prj_Editor_Module_ID.Naming_Pages (Num).Creator
                 (Kernel, Language);
            end if;
         end loop;
      end if;
      return null;
   end Get_Naming_Scheme_Page;

   --------------------------
   -- Parsing_Switches_XML --
   --------------------------

   procedure Parsing_Switches_XML
     (Kernel            : access Kernel_Handle_Record'Class;
      Page              : Switches_Editor_Page;
      Table             : access Gtk_Table_Record'Class;
      Lines             : Natural;
      Cols              : Natural;
      Node              : Node_Ptr;
      Default_Separator : String)
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

      function Check_Space_In_Switch (Switch : String) return Boolean;
      --  Return True if Switch contains a space

      procedure Process_Title_Node      (N : Node_Ptr);
      procedure Process_Check_Node      (N : Node_Ptr);
      procedure Process_Spin_Node       (N : Node_Ptr);
      procedure Process_Field_Node      (N : Node_Ptr);
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
      -- Check_Space_In_Switch --
      ---------------------------

      function Check_Space_In_Switch (Switch : String) return Boolean is
      begin
         for S in Switch'Range loop
            if Is_Blank (Switch (S)) then
               Insert
                 (Kernel,
                  -("Attribute switch cannot contain spaces. Use the separator"
                    & " attribute if you need to separate the switch and its"
                    & " argument"),
                  Mode => GPS.Kernel.Console.Error);
               return True;
            end if;
         end loop;
         return False;
      end Check_Space_In_Switch;

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
         Line_Span : constant Integer :=
                       Safe_Value (Get_Attribute (N, "line-span", "1"));
         Col_Span  : constant Integer :=
                       Safe_Value (Get_Attribute (N, "column-span", "1"));
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
               Mode => GPS.Kernel.Console.Error);
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
         N            : Node_Ptr := Parent.Child;
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
                  Label  : constant String := Get_Attribute (N, "label");
                  Switch : constant String := Get_Attribute (N, "switch");
                  Tip    : constant String := Get_Attribute (N, "tip");
               begin
                  if Label = "" then
                     Insert
                       (Kernel,
                          -("Invalid <radio-entry> node in custom file,"
                            & " requires a label and a switch attributes"),
                        Mode => GPS.Kernel.Console.Error);
                     return Buttons (1 .. 0);
                  end if;

                  if Check_Space_In_Switch (Switch) then
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
         N            : Node_Ptr := Parent.Child;
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
                        Mode => GPS.Kernel.Console.Error);
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
         Create_Radio
           (Page, Boxes (Line, Col), Process_Radio_Entry_Nodes (N));
      end Process_Radio_Node;

      ------------------------
      -- Process_Popup_Node --
      ------------------------

      procedure Process_Popup_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Table     : Gtk_Table;
         Lines     : constant Integer :=
                       Safe_Value (Get_Attribute (N, "lines", "1"));
         Cols      : constant Integer :=
                       Safe_Value (Get_Attribute (N, "columns", "1"));
      begin
         Coordinates_From_Node (N, Line, Col);
         if Label = "" then
            Insert
              (Kernel,
                 -("Invalid <popup> node in custom file,"
                   & " requires a label attributes"),
               Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         Gtk_New (Table, Guint (Lines), Guint (Cols), Homogeneous => False);
         Parsing_Switches_XML
           (Kernel, Page, Table, Lines, Cols, N, Default_Separator);

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
         Sep       : constant String :=
                       Get_Attribute (N, "separator", Default_Separator);
      begin
         Coordinates_From_Node (N, Line, Col);

         if Switch = "" then
            Insert (Kernel,
                      -("Invalid <combo> node in custom file, requires"
                        & " a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
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
               Label_Size_Group  => Sizes (Line, Col),
               Separator         => Sep),
            False, False);
      end Process_Combo_Node;

      ------------------------
      -- Process_Field_Node --
      ------------------------

      procedure Process_Field_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Label   : constant String := Get_Attribute (N, "label");
         Switch  : constant String := Get_Attribute (N, "switch");
         Tip     : constant String := Get_Attribute (N, "tip");
         As_Dir  : constant Boolean :=
                       Get_Attribute (N, "as-directory", "false") = "true";
         As_File : constant Boolean :=
                       Get_Attribute (N, "as-file", "false") = "true";
         Sep     : constant String :=
                       Get_Attribute (N, "separator", Default_Separator);
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <field> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Create_Field
           (Page, Boxes (Line, Col), Label, Switch, Tip,
            As_Directory     => As_Dir and not As_File,
            As_File          => As_File,
            Label_Size_Group => Sizes (Line, Col),
            Separator        => Sep);
      end Process_Field_Node;

      -----------------------
      -- Process_Spin_Node --
      -----------------------

      procedure Process_Spin_Node  (N : Node_Ptr) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
         Tip       : constant String := Get_Attribute (N, "tip");
         Min       : constant Integer :=
                      Safe_Value (Get_Attribute (N, "min", "1"));
         Max       : constant Integer :=
                       Safe_Value (Get_Attribute (N, "max", "1"));
         Default   : constant Integer :=
                       Safe_Value (Get_Attribute (N, "default", "1"));
         Sep       : constant String :=
                       Get_Attribute (N, "separator", Default_Separator);
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <spin> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Create_Spin
           (Page, Boxes (Line, Col), Label, Switch, Min, Max, Default, Tip,
            Sizes (Line, Col), Sep);
      end Process_Spin_Node;

      ------------------------
      -- Process_Check_Node --
      ------------------------

      procedure Process_Check_Node (N : Node_Ptr) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
         Tip       : constant String := Get_Attribute (N, "tip");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Insert (Kernel,
                      -("Invalid <check> node in custom file, requires"
                        & " a label and a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Create_Check (Page, Boxes (Line, Col), Label, Switch, Tip);
      end Process_Check_Node;

      ----------------------------
      -- Process_Expansion_Node --
      ----------------------------

      procedure Process_Expansion_Node (N : Node_Ptr) is
         Switch       : constant String := Get_Attribute (N, "switch");
         Alias        : constant String := Get_Attribute (N, "alias");
         N2           : Node_Ptr := N.Child;
         Num_Children : Natural := 0;
      begin
         if Switch = "" then
            Insert (Kernel,
                      -("Invalid <expansion> node in custom file, requires"
                        & " a switch attributes"),
                    Mode => GPS.Kernel.Console.Error);
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Coalesce_Switch (Page, Switch, Alias);

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
            elsif N.Tag.all = "field" then
               Process_Field_Node (N);
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
               Trace (Exception_Handle,
                      "Unexpected exception " & Exception_Information (E));
         end;

         N := N.Next;
      end loop;
   end Parsing_Switches_XML;

   ------------
   -- Create --
   ------------

   function Create
     (Creator : access XML_Switches_Record;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Switches_Editors.Switches_Editor_Page
   is
      Lines             : constant Integer := Safe_Value
        (Get_Attribute (Creator.XML_Node, "lines", "1"));
      Cols              : constant Integer := Safe_Value
        (Get_Attribute (Creator.XML_Node, "columns", "1"));
      Default_Separator : constant String :=
                            Get_Attribute (Creator.XML_Node, "separator", "");
      Page              : Switches_Editor_Page;
      Tool              : constant Tool_Properties_Record :=
                            Get_Tool_Properties
                              (Kernel, Creator.Tool_Name.all);
   begin
      Gtk_New (Page, Kernel, Creator.Tool_Name.all,
               Tool.Project_Package.all,
               Tool.Project_Index.all,
               Guint (Lines), Guint (Cols), Get_Tooltips (Kernel));

      if Creator.Languages /= null then
         for L in Creator.Languages'Range loop
            Add_Language (Page, Creator.Languages (L).all);
         end loop;
      end if;

      Parsing_Switches_XML
        (Kernel, Page, Page, Lines, Cols, Creator.XML_Node, Default_Separator);
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
     (N : Node_Ptr) return String_List_Access
   is
      Result : String_List_Access;
      Child  : Node_Ptr := N.Child;
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
     (Module : access Prj_Editor_Module_Id_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);
      N2, Child : Node_Ptr;
      Creator   : XML_Switches;
      Valid     : Boolean;
   begin
      if Node.Tag.all = "tool" then
         declare
            Tool_Name : constant String := Get_Attribute (Node, "name");
         begin
            if Tool_Name = "" then
               GPS.Kernel.Console.Insert
                 (Get_Kernel (Module.all),
                  -"Invalid <tool> node, no name specified");
            else
               N2 := Node.Child;
               while N2 /= null loop
                  if N2.Tag.all = "switches" then
                     Valid := True;

                     Child := N2.Child;
                     while Child /= null loop
                        if Child.Tag.all /= "title"
                          and then Child.Tag.all /= "check"
                          and then Child.Tag.all /= "spin"
                          and then Child.Tag.all /= "radio"
                          and then Child.Tag.all /= "combo"
                          and then Child.Tag.all /= "field"
                          and then Child.Tag.all /= "popup"
                          and then Child.Tag.all /= "dependency"
                          and then Child.Tag.all /= "expansion"
                        then
                           Insert (Get_Kernel (Module.all),
                                   -("Invalid child tag for <switches>"
                                     & " in customization files: <")
                                   & Child.Tag.all & '>',
                                   Mode => GPS.Kernel.Console.Error);
                           Valid := False;
                        end if;

                        Child := Child.Next;
                     end loop;

                     if Valid then
                        Creator := new XML_Switches_Record'
                          (Switches_Page_Creator_Record with
                           XML_Node  => Deep_Copy (N2),
                           Tool_Name => new String'(Tool_Name),
                           Languages => Get_Languages_From_Tool_Node (Node));
                        Register_Switches_Page
                          (Get_Kernel (Module.all), Creator);
                     end if;
                  end if;

                  N2 := N2.Next;
               end loop;
            end if;
         end;
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
      Tools   : constant String := '/' & (-"Tools") & '/' & (-"Views");
      Filter  : Action_Filter;
      Command : Interactive_Command_Access;
      Mitem   : Gtk_Menu_Item;

   begin
      Prj_Editor_Module_ID := new Prj_Editor_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Prj_Editor_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Project_Editor_Module_Name,
         Priority                => Default_Priority);

      Register_Menu (Kernel, Project, null, Ref_Item => -"Edit",
                     Add_Before => False);
      Register_Menu (Kernel, Project, -"_New...", "",
                     Creation_Wizard.Selector.On_New_Project'Access,
                     Ref_Item => -"Open From Host...", Add_Before => False);

      Register_Menu
        (Kernel, Project, -"Edit File _Switches", "",
         On_Edit_Switches'Access, Ref_Item => -"Recent", Add_Before => False);
      Register_Menu
        (Kernel, Tools, -"File Sw_itches", "", On_Edit_Switches'Access,
         Ref_Item => -"Remote");

      Register_Menu
        (Kernel, Project, -"Edit Project _Properties", "",
         On_Project_Properties'Access, Ref_Item => -"Recent",
         Add_Before => False);

      Register_Menu
        (Kernel, Project, -"Save _All", "",
         Save_All_Projects'Access, Ref_Item => -"Edit Project Properties",
         Add_Before => False);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Project, Mitem, Ref_Item => "Recent",
                     Add_Before => False);

      --  ??? Disabled for now, pending resolution of related problems
      --  encountered during testing

      --  Kernel_Callback.Connect
      --    (Kernel, File_Edited_Signal,
      --     On_File_Edited'Access,
      --     Kernel_Handle (Kernel));

      Register_Project_Editor_Page
        (Kernel,
         Page  => new Naming_Editor_Record,
         Label => -"Naming",
         Toc   => -"Naming scheme",
         Title => -"Please select the naming scheme to use");
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Switches_Editor_Record,
         Label => -"Switches",
         Toc   => -"Switches",
         Title => -"Please select the switches to build the project");

      Filter  := Lookup_Filter (Kernel, "Project only");
      Command := new Project_Properties_Editor_Command;
      Register_Contextual_Menu
        (Kernel, "Edit project properties",
         Action => Command,
         Label  => "Project/Properties",
         Filter => Filter);

      Command := new Save_Project_Command;
      Register_Contextual_Menu
        (Kernel, "Save project",
         Action => Command,
         Filter => Filter,
         Label  => "Project/Save project %p");

      Command := new Edit_Project_Source_Command;
      Register_Contextual_Menu
        (Kernel, "Project/Edit source file",
         Action => Command,
         Filter => Filter);

      Command := new Project_Dependency_Wizard_Command;
      Register_Contextual_Menu
        (Kernel, "Project dependencies",
         Action => Command,
         Filter => Filter,
         Label  => "Project/Dependencies");

      Command := new Add_Variable_Command;
      Register_Contextual_Menu
        (Kernel, "Add configuration variable",
         Action => Command,
         Label  => "Project/Add configuration variable",
         Filter => Action_Filter
           ((Create (Module => "Explorer") and Filter)
            or Create (Module => "Scenario_View")));

      Filter := Lookup_Filter (Kernel, "Project and file");
      Register_Contextual_Menu
        (Kernel, "", Filter => Filter);

      Command := new Edit_Switches_Command;
      Register_Contextual_Menu
        (Kernel, "Edit file switches",
         Action => Command,
         Filter => Filter,
         Label  => "Edit switches for %f");

      Creation_Wizard.Extending.Register_Contextual_Menus (Kernel);

      Register_Command
        (Kernel, "add_main_unit",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_dependency",
         Minimum_Args => Remove_Dep_Cmd_Parameters'Length,
         Maximum_Args => Remove_Dep_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "add_dependency",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "sources",
         Maximum_Args => Sources_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "source_dirs",
         Minimum_Args => Source_Dirs_Cmd_Parameters'Length - 1,
         Maximum_Args => Source_Dirs_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "languages",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "rename",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "add_predefined_paths",
         Maximum_Args => 2,
         Class        => Get_Project_Class (Kernel),
         Static_Method => True,
         Handler      => Project_Static_Command_Handler'Access);
      Register_Command
        (Kernel, "object_dirs",
         Minimum_Args => Source_Dirs_Cmd_Parameters'Length - 1,
         Maximum_Args => Source_Dirs_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "add_source_dir",
         Minimum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Maximum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_source_dir",
         Minimum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Maximum_Args => Add_Source_Dir_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Kernel),
         Handler      => Project_Command_Handler'Access);
   end Register_Module;

end Project_Viewers;
