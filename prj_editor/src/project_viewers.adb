------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Generic_Views;
with GNAT.Strings;                 use GNAT.Strings;
with GNATCOLL.Arg_Lists;           use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;             use GNATCOLL.Scripts;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Utils;               use GNATCOLL.Utils;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;          use GNATCOLL.VFS.GtkAda;
with GNATCOLL.VFS_Utils;           use GNATCOLL.VFS_Utils;

with Gdk.RGBA;                     use Gdk.RGBA;
with Gdk.Event;                    use Gdk.Event;

with Glib;                         use Glib;
with Glib.Convert;                 use Glib.Convert;
with Glib.Object;                  use Glib.Object;

with Gtk.Box;                      use Gtk.Box;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_Store;               use Gtk.Tree_Store;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Widget;                   use Gtk.Widget;

with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;

with Commands.Interactive;         use Commands, Commands.Interactive;
with Creation_Wizard.Dependencies; use Creation_Wizard.Dependencies;
with Creation_Wizard.Extending;    use Creation_Wizard.Extending;
with Creation_Wizard.Selector;     use Creation_Wizard.Selector;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Scripts;           use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;    use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                   use GPS.Kernel;
with GUI_Utils;                    use GUI_Utils;
with Language_Handlers;            use Language_Handlers;
with Naming_Editors;               use Naming_Editors;
with Project_Properties;           use Project_Properties;
with Projects;                     use Projects;
with Remote;                       use Remote;
with Switches_Editors;             use Switches_Editors;
with System;
with Variable_Editors;             use Variable_Editors;

package body Project_Viewers is

   Me : constant Trace_Handle := Create ("Project_Viewers");

   type Project_Editor_Page_Array is array (Natural range <>)
     of Project_Editor_Page;
   type Project_Editor_Page_Array_Access is access Project_Editor_Page_Array;

   type Naming_Page is record
      Language : String_Access;
      Creator  : Naming_Scheme_Editor_Creator;
   end record;

   type Naming_Pages_Array is array (Natural range <>) of Naming_Page;
   type Naming_Pages_Array_Access is access Naming_Pages_Array;
   procedure Free (Arr : in out Naming_Pages_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Naming_Pages_Array, Naming_Pages_Array_Access);

   type Prj_Editor_Module_Id_Record is new Module_ID_Record with record
      Project_Editor_Pages : Project_Editor_Page_Array_Access;
      --  The pages to be added in the project properties editor and the
      --  project creation wizard.

      Naming_Pages         : Naming_Pages_Array_Access;
   end record;
   type Prj_Editor_Module_Id_Access is access all
     Prj_Editor_Module_Id_Record'Class;

   Prj_Editor_Module_ID : Prj_Editor_Module_Id_Access;
   --  Id for the project editor module

   Project_Switches_Name : constant String := "Project Switches";

   Directory_Cst : aliased constant String := "directory";
   Imported_Cst  : aliased constant String := "imported";
   Src_Path_Cst  : aliased constant String := "sources";
   Obj_Path_Cst  : aliased constant String := "objects";
   Name_Cst      : aliased constant String := "name";
   Path_Cst      : aliased constant String := "path";
   Add_Source_Dir_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Directory_Cst'Access);
   Remove_Dep_Cmd_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List :=
     (1 => Imported_Cst'Access);
   Add_Predefined_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List
     := (1 => Src_Path_Cst'Access, 2 => Obj_Path_Cst'Access);
   Rename_Cmd_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Path_Cst'Access);
   Add_Dep_Cmd_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List :=
     (1 => Path_Cst'Access);

   Display_File_Name_Column  : constant := 0;
   --  This columns contains the UTF8 representation of the file name
   File_Column               : constant := 1;
   --  This column contains the file itself
   Compiler_Switches_Column  : constant := 2;
   Compiler_Color_Column     : constant := 3;

   type Project_Viewer_Record is new Generic_Views.View_Record with record
      Tree  : Gtk.Tree_View.Gtk_Tree_View;
      Model : Gtk.Tree_Store.Gtk_Tree_Store;
      --  The actual contents of the viewer

      Default_Switches_Color : Gdk.RGBA.Gdk_RGBA;
      --  Color to use when displaying switches that are set at the project
      --  level, rather than file specific

      View_Changed_Blocked : Boolean := False;
      --  True if the hook for "project_view_changed" should be ignored

      Current_Project : Project_Type;
      --  The project to which the files currently in the viewer belong. This
      --  indicates which project file should be normalized when a modification
      --  takes place.

      Current_Dir : Virtual_File := No_File;
      --  The directory currently being shown
   end record;

   function Initialize
     (Viewer : access Project_Viewer_Record'Class)
     return Gtk_Widget;
   --  Create a new project viewer, and return the focus widget.
   --  Every time the selection in Explorer changes, the info displayed in
   --  the viewer is changed.

   type Files_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Files_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   package File_Views is new Generic_Views.Simple_Views
     (Formal_View_Record => Project_Viewer_Record,
      Module_Name        => "File_Switches",
      View_Name          => -"Switches editor",
      Formal_MDI_Child   => Files_Child_Record,
      Initialize         => Initialize,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Areas              => Gtkada.MDI.Central_Only,
      Group              => Group_Default,
      Position           => Position_Automatic);
   subtype Project_Viewer is File_Views.View_Access;
   use File_Views;

   procedure Show_Project
     (Viewer              : access Project_Viewer_Record'Class;
      Project_Filter      : Project_Type;
      Directory_Filter    : Virtual_File := GNATCOLL.VFS.No_File);
   --  Shows all the direct source files of Project_Filter (ie not including
   --  imported projects, but including all source directories).
   --  This clears the list first.
   --  Directory_Filter should be used to limit the search path for the files.
   --  Only the files in Directory_Filter will be displayed.
   --
   --  Project_Filter mustn't be No_Project.

   overriding procedure Destroy (Module : in out Prj_Editor_Module_Id_Record);
   --  Free the memory associated with the module

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Editor_Page_Array, Project_Editor_Page_Array_Access);
   --  Free the memory used by Pages

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      File_Name        : Virtual_File;
      Directory_Filter : Virtual_File := GNATCOLL.VFS.No_File);
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
   overriding procedure Execute
     (Hook   : Context_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called every time the selection has changed in the tree

   procedure Explorer_Selection_Changed
     (Viewer  : access Project_Viewer_Record'Class;
      Context : Selection_Context);
   --  Same as above, but work directly on a context

   type Edit_Project_Properties_Command
      is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_Project_Properties_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Save_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Save_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Save the project associated with the kernel, and all its imported
   --  projects.

   type Edit_File_Switches is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_File_Switches;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit the switches for all the files selected in Viewer

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle the interactive commands related to the project editor

   procedure Project_Static_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle static methods for the GPS.Project class

   procedure Update_Contents
     (Viewer    : access Project_Viewer_Record'Class;
      Project   : Project_Type;
      Directory : Virtual_File := GNATCOLL.VFS.No_File;
      File      : Virtual_File := GNATCOLL.VFS.No_File);
   --  Update the contents of the viewer.
   --  Directory and File act as filters for the information that is displayed.

   procedure Project_Viewers_Set
     (Viewer : access Project_Viewer_Record'Class;
      Iter   : Gtk_Tree_Iter);
   --  Set the contents of the line Iter in the model. It is assumed the file
   --  name has already been set on that line

   type Preferences_Hook_Record is new Function_With_Args with record
      Viewer : Project_Viewer;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Hook called when the preferences change

   type Project_View_Hook_Record is new Function_No_Args with record
      Viewer : Project_Viewer;
   end record;
   type Project_View_Hook is access all Project_View_Hook_Record'Class;
   overriding procedure Execute
     (Hook : Project_View_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Hook called when the project view changes

   --------------------------
   -- Project editor pages --
   --------------------------

   type Switches_Editor_Record is
     new Project_Editor_Page_Record with null record;
   overriding function Widget_Factory
     (Page         : access Switches_Editor_Record;
      Project      : Project_Type;
      Full_Project : GNATCOLL.VFS.Virtual_File;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   overriding function Project_Editor
     (Page               : access Switches_Editor_Record;
      Project            : Project_Type;
      Kernel             : access Kernel_Handle_Record'Class;
      Widget             : access Gtk_Widget_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project        : Project_Type) return Boolean;
   overriding procedure Refresh
     (Page      : access Switches_Editor_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project   : Project_Type := No_Project;
      Languages : GNAT.Strings.String_List);

   type Naming_Editor_Record is new Project_Editor_Page_Record with record
      Kernel : Kernel_Handle;
   end record;
   overriding function Widget_Factory
     (Page         : access Naming_Editor_Record;
      Project      : Project_Type;
      Full_Project : GNATCOLL.VFS.Virtual_File;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   overriding function Project_Editor
     (Page               : access Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : access Kernel_Handle_Record'Class;
      Widget             : access Gtk_Widget_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project        : Project_Type) return Boolean;
   overriding procedure Refresh
     (Page      : access Naming_Editor_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project   : Project_Type := No_Project;
      Languages : GNAT.Strings.String_List);

   ----------------------
   -- Contextual menus --
   ----------------------

   type Save_Project_Command
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Save_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Edit_Project_Source_Command
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_Project_Source_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ----------
   -- Free --
   ----------

   procedure Free (Arr : in out Naming_Pages_Array_Access) is
   begin
      if Arr /= null then
         for A in Arr'Range loop
            Free (Arr (A).Language);
         end loop;

         Unchecked_Free (Arr);
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Prj_Editor_Module_Id_Record)
   is
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

      Free (Module.Naming_Pages);

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
         Col3  : Gint; Value3 : Gdk_RGBA);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr_ptr");

      File_Name  : constant Virtual_File :=
                     Get_File (Viewer.Model, Iter, File_Column);
      Language   : constant String :=
                     Get_Language_From_File
                       (Get_Language_Handler (Viewer.Kernel), File_Name);
      Color      : Gdk_RGBA;
      Value      : String_List_Access;
      Is_Default : Boolean;
      Success    : Boolean;
   begin
      Viewer.Current_Project.Switches
        (Compiler_Package, File_Name,
         Language, Value, Is_Default);

      if Is_Default then
         Color := Viewer.Default_Switches_Color;
      else
         Parse (Color, "#000000", Success);
      end if;

      Internal (Get_Object (Viewer.Model), Iter'Address,
                Compiler_Switches_Column,
                Locale_To_UTF8
                  (Argument_List_To_String (Value.all)) & ASCII.NUL,
                Compiler_Color_Column, Color);
      Free (Value);
   end Project_Viewers_Set;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      File_Name        : Virtual_File;
      Directory_Filter : Virtual_File := GNATCOLL.VFS.No_File)
   is
      Iter : Gtk_Tree_Iter;
   begin
      if Directory_Filter = GNATCOLL.VFS.No_File
        or else Dir (File_Name) = Directory_Filter
      then
         Append (Viewer.Model, Iter, Null_Iter);
         Set (Viewer.Model,
              Iter,
              Display_File_Name_Column,
              File_Name.Display_Base_Name);
         Set_File
           (Viewer.Model,
            Iter,
            File_Column,
            File_Name);
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
         Iter := Find_Iter_For_Event (V.Tree, Event);
         if Iter /= Null_Iter then
            if not Iter_Is_Selected (Get_Selection (V.Tree), Iter) then
               Unselect_All (Get_Selection (V.Tree));
               Select_Iter (Get_Selection (V.Tree), Iter);
            end if;

            return Execute_In_Background
              (V.Kernel, Action => "edit switches for file");
         end if;
      end if;

      return False;
   end Select_Row;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook : Project_View_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      if not Hook.Viewer.View_Changed_Blocked then
         Hook.Viewer.Current_Project := Get_Project (Kernel);
         Show_Project (Hook.Viewer, Hook.Viewer.Current_Project);
      end if;
   end Execute;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
     (Viewer    : access Project_Viewer_Record'Class;
      Project   : Project_Type;
      Directory : Virtual_File := GNATCOLL.VFS.No_File;
      File      : Virtual_File := GNATCOLL.VFS.No_File)
   is
      Child : constant MDI_Child := File_Views.Child_From_View (Viewer);
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;

   begin
      --  If the context is invalid, keep the currently displayed lines, so
      --  that when a new MDI child is selected, the contents of the viewer is
      --  not necessarily reset.

      if Child /= null then
         if Directory = GNATCOLL.VFS.No_File then
            Set_Title (Child,
                       Title => -"Editing switches for project "
                       & Project.Name,
                       Short_Title => Project_Switches_Name);
         else
            Set_Title (Child,
                       Title => -"Editing switches for directory " &
                         Directory.Display_Full_Name,
                       Short_Title => Project_Switches_Name);
         end if;
      end if;

      Viewer.Current_Project := Project;

      if Viewer.Current_Project /= No_Project then
         Show_Project (Viewer, Viewer.Current_Project, Directory);
      end if;

      if File /= GNATCOLL.VFS.No_File then
         Iter := Get_Iter_First (Viewer.Model);
         while Iter /= Null_Iter loop
            if Get_File (Viewer.Model, Iter, File_Column) = File then
               Unselect_All (Get_Selection (Viewer.Tree));
               Select_Iter (Get_Selection (Viewer.Tree), Iter);

               Path := Get_Path (Viewer.Model, Iter);
               Scroll_To_Cell (Viewer.Tree, Path, null, True, 0.5, 0.5);
               Path_Free (Path);
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
            Directory_Information (Context),
            File_Information (Context));
      else
         Update_Contents (Viewer, Get_Project (Viewer.Kernel));
      end if;
   end Explorer_Selection_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Context_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      type Context_Hooks_Args_Access is access all Context_Hooks_Args'Class;
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
   begin
      --  Do nothing if we forced the selection change ourselves. For instance,
      --  when a new switch editor is created in On_Edit_Switches, to avoid
      --  doing extra work.
      if Child = null
        or else Get_Widget (Child) /= Gtk_Widget (Hook.Viewer)
      then
         Explorer_Selection_Changed
           (Hook.Viewer, Context_Hooks_Args_Access (Data).Context);
      end if;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Viewer : access Project_Viewer_Record'Class)
      return Gtk_Widget
   is
      Context : constant Selection_Context :=
        Get_Current_Context (Viewer.Kernel);

      Column_Types : constant GType_Array :=
                       (Display_File_Name_Column => GType_String,
                        File_Column              => Get_Virtual_File_Type,
                        Compiler_Switches_Column => GType_String,
                        Compiler_Color_Column    => Gdk.RGBA.Get_Type);

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
      Set_Sort_Column_Id (Col, Display_File_Name_Column);
      Add_Attribute (Col, Render, "text", Display_File_Name_Column);

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
      Add_Attribute (Col, Render, "foreground_rgba", Compiler_Color_Column);

      Setup_Contextual_Menu
        (Kernel          => Viewer.Kernel,
         Event_On_Widget => Viewer.Tree);

      Return_Callback.Object_Connect
        (Viewer.Tree, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Select_Row'Access), Viewer);

      Hook3 := new Context_Hook_Record'
        (Function_With_Args with Viewer => Project_Viewer (Viewer));
      Add_Hook
        (Viewer.Kernel, Context_Changed_Hook, Hook3,
         Name => "project_viewer.context_changed", Watch => GObject (Viewer));

      Hook2 := new Project_View_Hook_Record'
        (Function_No_Args with Viewer => Project_Viewer (Viewer));
      Add_Hook
        (Viewer.Kernel, Project_View_Changed_Hook, Hook2,
         Name => "project_viewer.project_view_changed",
         Watch => GObject (Viewer));

      Hook := new Preferences_Hook_Record'
        (Function_With_Args with Viewer => Project_Viewer (Viewer));
      Execute (Hook.all, Viewer.Kernel, Data => null);
      Add_Hook
        (Viewer.Kernel, Preference_Changed_Hook,
         Hook,
         Name => "project_viewer.preferences_changed",
         Watch => GObject (Viewer));

      Show_All (Viewer);

      --  The initial contents of the viewer should be read immediately from
      --  the explorer, without forcing the user to do a new selection.

      if Context /= No_Context then
         Explorer_Selection_Changed (Viewer, Context);
      else
         Update_Contents (Viewer, Get_Project (Viewer.Kernel));
      end if;

      return Gtk_Widget (Viewer.Tree);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      View : constant Project_Viewer := File_Views.Retrieve_View (Kernel);
   begin
      Hook.Viewer.Default_Switches_Color :=
        Default_Switches_Color.Get_Pref;
      --  ??? Do we need to change the model to reflect this change

      if View /= null then
         Set_Font_And_Colors
           (View, Fixed_Font => True, Pref => Get_Pref (Data));
      end if;
   end Execute;

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record'Class;
      Project_Filter   : Project_Type;
      Directory_Filter : Virtual_File := GNATCOLL.VFS.No_File)
   is
      Files                : File_Array_Access :=
                               Project_Filter.Source_Files
                                 (Recursive => False);
      Same_Dir_And_Project : constant Boolean :=
                               (Viewer.Current_Project = Project_Filter
                                and then Directory_Filter /= No_File
                                and then
                                  Viewer.Current_Dir = Directory_Filter);
      Sorted : Gint;
   begin
      Viewer.Current_Project := Project_Filter;

      if Same_Dir_And_Project then
         return;
      end if;

      if Directory_Filter /= No_File then
         Viewer.Current_Dir := Directory_Filter;
      end if;

      Clear (Viewer.Model);

      Sorted := Freeze_Sort (Viewer.Model);
      Ref (Viewer.Model);

      --  Do not refresh while adding files
      Set_Model (Viewer.Tree, Null_Gtk_Tree_Model);

      for F in Files'Range loop
         Append_Line (Viewer, Files (F), Directory_Filter);
      end loop;

      Thaw_Sort (Viewer.Model, Sorted);
      Set_Model (Viewer.Tree, +Viewer.Model);
      Unref (Viewer.Model);

      Unchecked_Free (Files);
   end Show_Project;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Project_Properties_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Project : Project_Type;
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      if Has_Project_Information (Context.Context) then
         Project := Project_Information (Context.Context);
      else
         Project := Get_Project (Kernel);
      end if;

      Edit_Properties (Project, Kernel);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Save_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Tmp : Boolean;
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      pragma Unreferenced (Command, Tmp);
   begin
      Tmp := Save_Project (Kernel, Get_Project (Kernel), Recursive => True);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
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

   overriding function Execute
     (Command : access Edit_Project_Source_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project : Project_Type;
   begin
      if Has_Project_Information (Context.Context) then
         Project := Project_Information (Context.Context);
      else
         Project := Kernel.Registry.Tree.Root_Project;
      end if;
      Open_File_Editor (Kernel, Project.Project_Path, Project);
      return Success;
   end Execute;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Files_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      V    : constant Project_Viewer :=
        Project_Viewer (GPS_MDI_Child (Self).Get_Actual_Widget);
      Iter : Gtk_Tree_Iter;
      Context : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);

   begin
      Iter := Find_Iter_For_Event (V.Tree, Event);

      if Iter = Null_Iter then
         Set_File_Information (Context, Project => V.Current_Project);

      else
         if not Iter_Is_Selected (Get_Selection (V.Tree), Iter) then
            Unselect_All (Get_Selection (V.Tree));
            Select_Iter (Get_Selection (V.Tree), Iter);
         end if;

         declare
            File_Name : constant Virtual_File :=
                          Get_File (V.Model, Iter, File_Column);
         begin
            Set_File_Information
              (Context,
               Files   => (1 => File_Name),
               Project => V.Current_Project);
         end;
      end if;
      return Context;
   end Build_Context;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_File_Switches;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      V         : constant Project_Viewer :=
        File_Views.Retrieve_View (Get_Kernel (Context.Context));
      Selection : Gtk_Tree_Selection;
      Length    : Natural := 0;
      Iter      : Gtk_Tree_Iter;

   begin
      if V = null then
         return Commands.Failure;
      end if;

      Selection := V.Tree.Get_Selection;
      Iter := V.Model.Get_Iter_First;

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
               Names (N) := Get_File (V.Model, Iter, File_Column);
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
      return Commands.Success;
   end Execute;

   --------------------
   -- Widget_Factory --
   --------------------

   overriding function Widget_Factory
     (Page         : access Switches_Editor_Record;
      Project      : Project_Type;
      Full_Project : GNATCOLL.VFS.Virtual_File;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Page, Full_Project);
      Switches : Switches_Editors.Switches_Edit;
   begin
      Run_Hook (Kernel, Project_Editor_Hook);
      Gtk_New (Switches, Kernel);
      Show_All (Switches);
      Set_Switches (Switches, Project);

      return Gtk_Widget (Switches);

   exception
      when E : others =>
         Trace (Me, E);
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   overriding function Project_Editor
     (Page               : access Switches_Editor_Record;
      Project            : Project_Type;
      Kernel             : access Kernel_Handle_Record'Class;
      Widget             : access Gtk_Widget_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project        : Project_Type) return Boolean
   is
      pragma Unreferenced (Kernel, Page, Ref_Project);
      Result : Boolean;
   begin
      Result := Generate_Project
        (Switches           => Switches_Edit (Widget),
         Project            => Project,
         Languages          => Languages,
         Scenario_Variables => Scenario_Variables,
         Files              => (1 .. 0 => GNATCOLL.VFS.No_File));
      if Result then
         Trace (Me, "Switches editor: project was modified");
      end if;

      return Result;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh
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

   overriding function Widget_Factory
     (Page         : access Naming_Editor_Record;
      Project      : Project_Type;
      Full_Project : GNATCOLL.VFS.Virtual_File;
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
         Trace (Me, E);
         return null;
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   overriding function Project_Editor
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
         Trace (Me, E);
         return False;
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh
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
      function Remove_Redundant_Directories
        (Old_Path, New_Path : File_Array) return File_Array;
      --  Return New_Path after having removed the directories that have been
      --  found on Old_Path.

      ----------------------------------
      -- Remove_Redundant_Directories --
      ----------------------------------

      function Remove_Redundant_Directories
        (Old_Path, New_Path : File_Array) return File_Array
      is
         Returned_Path        : File_Array (1 .. New_Path'Length);
         Returned_Path_Length : Integer := 0;
         Found                : Boolean;
      begin
         for J in New_Path'Range loop
            Found := False;

            for K in Old_Path'Range loop
               if New_Path (J) = Old_Path (K) then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Returned_Path_Length := Returned_Path_Length + 1;
               Returned_Path (Returned_Path_Length) := New_Path (J);
            end if;
         end loop;

         return Returned_Path (1 .. Returned_Path_Length);
      end Remove_Redundant_Directories;

      Kernel : constant Kernel_Handle := Get_Kernel (Data);

   begin
      if Command = "add_predefined_paths" then
         Name_Parameters (Data, Add_Predefined_Parameters);
         declare
            Old_Src : constant File_Array :=
              Get_Registry (Kernel).Environment.Predefined_Source_Path;
            Old_Obj : constant File_Array :=
              Get_Registry (Kernel).Environment.Predefined_Object_Path;
            New_Src : constant File_Array :=
                        Remove_Redundant_Directories
                          (Old_Src,
                           From_Path (Nth_Arg (Data, 1, "")));
            New_Obj : constant File_Array :=
                        Remove_Redundant_Directories
                          (Old_Obj,
                           From_Path (+Nth_Arg (Data, 2, "")));
         begin
            if New_Src'Length /= 0 then
               Get_Registry (Kernel).Environment.Set_Predefined_Source_Path
                 (New_Src & Old_Src);
            end if;

            if Old_Obj'Length /= 0 then
               Get_Registry (Kernel).Environment.Set_Predefined_Object_Path
                 (New_Obj & Old_Obj);
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

      procedure Set_Error_Tmp (Str : String);
      --  Set an error

      ---------------
      -- Set_Error --
      ---------------

      procedure Set_Error_Tmp (Str : String) is
      begin
         Set_Error_Msg (Data, Str);
      end Set_Error_Tmp;

      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      Project : constant Project_Type := Get_Data (Data, 1);

   begin
      if Command = "add_main_unit" then
         declare
            Args : GNAT.Strings.String_List
              (1 .. Number_Of_Arguments (Data) - 1);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               for Index in 2 .. Number_Of_Arguments (Data) loop
                  Args (Index - 1) := new String'(Nth_Arg (Data, Index));
               end loop;

               Project.Set_Attribute
                 (Scenario  => Scenario_Variables (Kernel),
                  Attribute => Main_Attribute,
                  Values    => Args,
                  Prepend   => True);
               Recompute_View (Kernel);
               Free (Args);
            end if;
         end;

      elsif Command = "rename" then
         Name_Parameters (Data, Rename_Cmd_Parameters);
         declare
            Name : constant String := Nth_Arg (Data, 2);
            Path : constant Filesystem_String :=
              Nth_Arg (Data, 3, Project_Directory (Project).Full_Name);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Project.Rename_And_Move
                 (New_Name  => Name,
                  Directory => Create (Path),
                  Errors    => Set_Error_Tmp'Unrestricted_Access);
               Run_Hook (Kernel, Project_Changed_Hook);
            end if;
         end;

      elsif Command = "remove_dependency" then
         Name_Parameters (Data, Remove_Dep_Cmd_Parameters);
         declare
            Project2  : constant Project_Type := Get_Data (Data, 2);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Remove_Imported_Project (Project, Project2);
            end if;
         end;

      elsif Command = "add_dependency" then
         Name_Parameters (Data, Add_Dep_Cmd_Parameters);
         declare
            Project2 : constant Filesystem_String  := Normalize_Pathname
              (Name => Nth_Arg (Data, 2));
            Relative : constant Boolean :=
              Get_Paths_Type (Project) = Projects.Relative
              or else (Get_Paths_Type (Project) = From_Pref
                       and then Generate_Relative_Paths.Get_Pref);
            Error    : Import_Project_Error;
            pragma Unreferenced (Error);
         begin
            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               Error := Get_Registry (Kernel).Tree.Add_Imported_Project
                 (Project            => Project,
                  Imported_Project_Location => Create (Project2),
                  Errors             => Set_Error_Tmp'Unrestricted_Access,
                  Use_Base_Name      => False,
                  Use_Relative_Path  => Relative);
            end if;
         end;

      elsif Command = "add_source_dir" then
         Name_Parameters (Data, Add_Source_Dir_Cmd_Parameters);
         declare
            Dir     : constant Virtual_File :=
              Create
                (Normalize_Pathname
                     (Nth_Arg (Data, 2),
                      Directory =>
                        Full_Name (Project_Directory (Project)),
                      Resolve_Links => False));
            Dirs    : GNAT.Strings.String_List (1 .. 1);
            Sources : constant File_Array :=
              Project.Source_Dirs (Recursive => False);
            Found   : Boolean := False;

         begin
            Ensure_Directory (Dir);
            Dirs (1) := new String'(+Dir.Full_Name);

            if not Is_Editable (Project) then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               for S in Sources'Range loop
                  if Sources (S) = Dir then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Project.Set_Attribute
                    (Scenario  => Scenario_Variables (Get_Kernel (Data)),
                     Attribute => Source_Dirs_Attribute,
                     Values    => Dirs,
                     Index     => "",
                     Prepend   => True);
               end if;

               Free (Dirs);
            end if;
         end;

      elsif Command = "remove_source_dir" then
         Name_Parameters (Data, Add_Source_Dir_Cmd_Parameters);
         declare
            Dir : Virtual_File :=
              Create (Nth_Arg (Data, 2), Get_Nickname (Build_Server));
            Dirs : String_List_Access := Project.Attribute_Value
              (Source_Dirs_Attribute);
            Index : Natural := Dirs'Last;
         begin
            if not Is_Editable (Project) or else Dirs = null then
               Set_Error_Msg (Data, -"Project is not editable");
            else
               if not Is_Absolute_Path (Dir) then
                  Dir := Create_From_Base
                    (Nth_Arg (Data, 2),
                     Get_Current_Dir
                       (Get_Nickname (Build_Server)).Full_Name.all);
               end if;

               for D in Dirs'Range loop
                  declare
                     Tested_Dir : Virtual_File :=
                       Create (+Dirs (D).all, Get_Nickname (Build_Server));
                  begin
                     if not Is_Absolute_Path (Tested_Dir) then
                        Tested_Dir := Create_From_Base
                          (+Dirs (D).all,
                           Get_Current_Dir
                             (Get_Nickname (Build_Server)).Full_Name.all);
                     end if;

                     if Dir = Tested_Dir then
                        Free (Dirs (D));
                        Dirs (D .. Dirs'Last - 1) := Dirs (D + 1 .. Dirs'Last);
                        Index := Index - 1;
                     end if;
                  end;
               end loop;

               Project.Set_Attribute
                 (Scenario  => Scenario_Variables (Get_Kernel (Data)),
                  Attribute => Source_Dirs_Attribute,
                  Values    => Dirs (Dirs'First .. Index),
                  Index     => "");
            end if;

            Free (Dirs);
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

   function Get_Label
     (Page : access Project_Editor_Page_Record'Class) return String is
   begin
      return Page.Label.all;
   end Get_Label;

   -------------
   -- Get_Toc --
   -------------

   function Get_Toc
     (Page : access Project_Editor_Page_Record'Class) return String is
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
     (Kernel : access Kernel_Handle_Record'Class; Num : Positive)
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
      Lang : constant String := To_Lower (Language);
   begin
      if Prj_Editor_Module_ID /= null then
         if Prj_Editor_Module_ID.Naming_Pages = null then
            Prj_Editor_Module_ID.Naming_Pages :=
              new Naming_Pages_Array'(1 => (new String'(Lang), Creator));

         else
            Tmp := Prj_Editor_Module_ID.Naming_Pages;
            Prj_Editor_Module_ID.Naming_Pages := new Naming_Pages_Array'
              (Tmp.all & Naming_Page'(new String'(Lang), Creator));
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
      Lang : constant String := To_Lower (Language);
   begin
      if Prj_Editor_Module_ID.Naming_Pages /= null then
         for Num in Prj_Editor_Module_ID.Naming_Pages'Range loop
            if Prj_Editor_Module_ID.Naming_Pages (Num).Language.all = Lang then
               return Prj_Editor_Module_ID.Naming_Pages (Num).Creator
                 (Kernel, Language);
            end if;
         end loop;
      end if;
      return null;
   end Get_Naming_Scheme_Page;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter  : Action_Filter;
      Filter2 : Action_Filter;
      Command : Interactive_Command_Access;

   begin
      Prj_Editor_Module_ID := new Prj_Editor_Module_Id_Record;
      Register_Module
        (Module      => Module_ID (Prj_Editor_Module_ID),
         Kernel      => Kernel,
         Module_Name => Project_Editor_Module_Name,
         Priority    => Default_Priority);

      File_Views.Register_Module (Kernel);

      Register_Action
        (Kernel, "new project", new New_Project_Command,
         Description => -"Interactively create a new project");

      Register_Action
        (Kernel, "open Project Properties",
         new Edit_Project_Properties_Command,
         "Open the project properties editor",
         Stock_Id => Stock_Edit,
         Category => -"Views");

      Register_Action
        (Kernel, "save all projects", new Save_All_Command,
         Description => -"Save all modified projects to disk");

      Register_Action
        (Kernel, "edit switches for file", new Edit_File_Switches,
         -"Edit the switches for the files selected in the switches editor",
         Stock_Id => Stock_Edit);

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
      Filter2  := Lookup_Filter (Kernel, "Project only")
        and Lookup_Filter (Kernel, "Editable Project");
      Command := new Project_Properties_Editor_Command;
      Register_Contextual_Menu
        (Kernel, "Edit project properties",
         Action => Command,
         Label  => "Project/Properties",
         Filter => Filter2);

      Command := new Save_Project_Command;
      Register_Contextual_Menu
        (Kernel, "Save project",
         Action => Command,
         Filter => Filter2,
         Label  => "Project/Save project %p");

      Register_Action
        (Kernel, "Edit project source file", new Edit_Project_Source_Command,
         "Open an editor for the .gpr file of the current project",
         null, -"Projects");
      Register_Contextual_Menu
        (Kernel, "Project/Edit source file",
         Action => new Edit_Project_Source_Command,
         Filter => Filter);

      Command := new Project_Dependency_Wizard_Command;
      Register_Contextual_Menu
        (Kernel, "Project dependencies",
         Action => Command,
         Filter => Filter2,
         Label  => "Project/Dependencies");

      Command := new Add_Variable_Command;
      Register_Contextual_Menu
        (Kernel, "Add scenario variable",
         Action => Command,
         Label  => "Project/Add scenario variable",
         Filter => (Create (Module => Explorer_Module_Name) and Filter2));
      Register_Action
        (Kernel, Action_Add_Scenario_Variable,
         Command     => new Add_Variable_Command,
         Stock_Id    => Stock_Add,
         Description => -"Add a new scenario variable to the selected project",
         Category => -"Projects");

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
