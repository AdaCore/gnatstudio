-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with Glib.Object;                  use Glib.Object;
with Glib.Values;                  use Glib.Values;
with Gtk.Alignment;                use Gtk.Alignment;
with Gtk.Arguments;                use Gtk.Arguments;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Clist;                    use Gtk.Clist;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Event_Box;                use Gtk.Event_Box;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;
with Gtk.List_Store;               use Gtk.List_Store;
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Style;                    use Gtk.Style;
with Gtk.Tooltips;                 use Gtk.Tooltips;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Vbutton_Box;              use Gtk.Vbutton_Box;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Window;                   use Gtk.Window;
with Gtkada.Dialogs;               use Gtkada.Dialogs;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtkada.Types;                 use Gtkada.Types;

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;

with Basic_Types;              use Basic_Types;
with String_Utils;
with Prj;
with Projects.Editor;          use Projects, Projects.Editor;
with Projects.Registry;        use Projects.Registry;
with Creation_Wizard;          use Creation_Wizard;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
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
with String_Utils;             use String_Utils;
with Project_Properties;       use Project_Properties;
with Histories;                use Histories;
with GUI_Utils;                use GUI_Utils;

with Stringt;       use Stringt;
with Types;         use Types;
with Namet;         use Namet;
with Snames;        use Snames;

package body Project_Viewers is

   Me : constant Debug_Handle := Create ("Project_Viewers");

   type Prj_Editor_Module_Id_Record is new Module_ID_Record with record
      Reopen_Menu : Gtk.Menu_Item.Gtk_Menu_Item;
   end record;
   type Prj_Editor_Module_Id_Access is access all
     Prj_Editor_Module_Id_Record'Class;

   Prj_Editor_Module_ID : Prj_Editor_Module_Id_Access;
   --  Id for the project editor module

   Project_Switches_Name : constant String := "Project Switches";

   type View_Display is access procedure
     (Viewer    : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style);
   --  Procedure used to return the contents of one of the columns.
   --  The returned string (Line) will be freed by the caller.
   --  Style is the style to apply to the matching cell in the clist.

   type View_Callback is access procedure
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      Context   : File_Selection_Context_Access);
   --  Callback called every time the user selects a column in one of the
   --  view. The view is not passed as a parameter, but can be obtained
   --  directly from the Viewer, since this is the current view displayed in
   --  the viewer

   type View_Display_Array is array (Interfaces.C.size_t range <>)
     of View_Display;

   type View_Callback_Array is array (Interfaces.C.size_t range <>)
     of View_Callback;

   type View_Description (Num_Columns : Interfaces.C.size_t) is record
      Titles : Interfaces.C.Strings.chars_ptr_array (1 .. Num_Columns);
      --  The titles for all the columns

      Display : View_Display_Array (1 .. Num_Columns);
      --  The functions to display each of the columns. null can be provided
      --  if the columns doesn't contain any information.

      Callbacks : View_Callback_Array (1 .. Num_Columns);
      --  The callbacks to call when a column is clicked. If null, no callback
      --  is called.
   end record;

   procedure Name_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style);
   --  Return the name of the file

   procedure Compiler_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style);
   --  Return the switches used for the compiler

   procedure Edit_Switches_Callback
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      Context   : File_Selection_Context_Access);
   --  Called every time the user wants to edit some specific switches

   View_Switches : aliased constant View_Description :=
     (Num_Columns => 2,
      Titles      => (-"File Name") + (-"Compiler"),
      Display     => (Name_Display'Access,
                      Compiler_Switches_Display'Access),
      Callbacks   => (null,
                      Edit_Switches_Callback'Access));

   type User_Data is record
      File_Name : String_Id;
      Directory : String_Id;
   end record;
   package Project_User_Data is new Row_Data (User_Data);

   procedure Refresh_Reopen_Menu (Kernel : access Kernel_Handle_Record'Class);
   --  Fill the reopen menu.

   procedure On_Reopen
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Project->Reopen menu

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      Project          : Project_Type;
      File_Name        : String_Id;
      Directory_Filter : String := "");
   --  Append a new line in the current page of Viewer, for File_Name.
   --  The exact contents inserted depends on the current view.
   --  The file is automatically searched in all the source directories of
   --  Project_View.

   function Append_Line_With_Full_Name
     (Viewer         : access Project_Viewer_Record'Class;
      File_Name      : String;
      Directory_Name : String) return Gint;
   --  Same as above, except we have already found the proper location for
   --  the file.
   --  Return the number of the newly inserted row

   function Find_In_Source_Dirs
     (Project : Project_Type; File : String) return String_Id;
   --  Return the location of File in the source dirs of Project_View.
   --  null is returned if the file wasn't found.

   procedure Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Args : Gtk_Args);
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
   --  Callback for the Project->Edit Switches menu

   procedure On_Project_Properties
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Edit properties menu

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
     (Viewer : access GObject_Record'Class;
      Context : Selection_Context_Access);
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

   function Project_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Handle the interactive commands related to the project editor

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
      Executables  : Gtk_List_Store;
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

   ---------------
   -- On_Reopen --
   ---------------

   procedure On_Reopen
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Mitem     : constant Full_Path_Menu_Item
        := Full_Path_Menu_Item (Widget);
      Filename : constant String := Get_Path (Mitem);
      Dir      : constant String := Dir_Name (Filename);

   begin
      Change_Dir (Dir);
      Load_Project (Kernel, Filename);

   exception
      when Directory_Error =>
         Console.Insert (Kernel, -"Invalid directory " & Dir,
                         Mode => Console.Error);

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Reopen;

   -------------------------
   -- Refresh_Reopen_Menu --
   -------------------------

   procedure Refresh_Reopen_Menu
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Value       : constant String_List_Access := Get_History
        (Get_History (Kernel).all, Project_History_Key);
      Reopen_Menu : Gtk_Menu;
   begin
      if Get_Submenu (Prj_Editor_Module_ID.Reopen_Menu) /= null then
         Remove_Submenu (Prj_Editor_Module_ID.Reopen_Menu);
      end if;

      Gtk_New (Reopen_Menu);
      Set_Submenu (Prj_Editor_Module_ID.Reopen_Menu, Gtk_Widget (Reopen_Menu));

      if Value /= null then
         for V in Value'Range loop
            declare
               Path  : constant String := Value (V).all;
               Mitem : Full_Path_Menu_Item;
            begin
               Gtk_New (Mitem, Shorten (Path), Path);
               Append (Reopen_Menu, Mitem);

               Kernel_Callback.Connect
                 (Mitem,
                  "activate",
                  Kernel_Callback.To_Marshaller (On_Reopen'Access),
                  Kernel_Handle (Kernel));

            end;
         end loop;

         Show_All (Reopen_Menu);
      end if;
   end Refresh_Reopen_Menu;

   -------------------
   -- Add_To_Reopen --
   -------------------

   procedure Add_To_Reopen
     (Kernel : access Kernel_Handle_Record'Class;
      Filename : String) is
   begin
      Add_To_History (Kernel, Project_History_Key, Filename);
      Refresh_Reopen_Menu (Kernel);
   end Add_To_Reopen;

   -------------------------
   -- Find_In_Source_Dirs --
   -------------------------

   function Find_In_Source_Dirs
     (Project : Project_Type; File : String) return String_Id
   is
      Dirs : constant String_Id_Array := Source_Dirs (Project);
   begin
      --  We do not use Ada_Include_Path to locate the source file,
      --  since this would include directories from imported project
      --  files, and thus slow down the search. Instead, we search
      --  in all the directories directly belong to the project.

      for D in Dirs'Range loop
         String_To_Name_Buffer (Dirs (D));

         if Is_Regular_File
           (Name_As_Directory (Name_Buffer (1 .. Name_Len)) & File)
         then
            return Dirs (D);
         end if;
      end loop;

      return No_String;
   end Find_In_Source_Dirs;

   -------------------------------
   -- Compiler_Switches_Display --
   -------------------------------

   procedure Compiler_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style)
   is
      pragma Unreferenced (Directory, Fd);
      Value      : Prj.Variable_Value;
      Is_Default : Boolean;
      Language   : constant String := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Viewer.Kernel)),
         File_Name);
   begin
      Name_Len := Language'Length;
      Name_Buffer (1 .. Name_Len) := Language;

      Get_Switches
        (Viewer.Project_Filter, "compiler", File_Name,
         Name_Find, Value, Is_Default);
      Line := New_String (To_String (Value));

      if Is_Default then
         Style := Viewer.Default_Switches_Style;
      end if;
   end Compiler_Switches_Display;

   ------------------
   -- Name_Display --
   ------------------

   procedure Name_Display
     (Viewer    : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style)
   is
      pragma Unreferenced (Viewer, Directory, Fd);
   begin
      Style := null;
      Line  := New_String (File_Name);
   end Name_Display;

   ----------------------------
   -- Edit_Switches_Callback --
   ----------------------------

   procedure Edit_Switches_Callback
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      Context   : File_Selection_Context_Access)
   is
      pragma Unreferenced (Column, Viewer);
   begin
      Edit_Switches_For_Context
        (Selection_Context_Access (Context), Force_Default => False);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Edit_Switches_Callback;

   --------------------------------
   -- Append_Line_With_Full_Name --
   --------------------------------

   function Append_Line_With_Full_Name
     (Viewer         : access Project_Viewer_Record'Class;
      File_Name      : String;
      Directory_Name : String) return Gint
   is
      Line      : Gtkada.Types.Chars_Ptr_Array
        (1 .. View_Switches.Num_Columns);
      Row       : Gint;
      File_Desc : File_Descriptor;
      Style     : array (1 .. View_Switches.Num_Columns) of Gtk_Style;

   begin
      if Is_Absolute_Path (Directory_Name) then
         File_Desc := Open_Read
           (String_Utils.Name_As_Directory (Directory_Name) & File_Name, Text);
      else
         File_Desc := Open_Read
           (Get_Current_Dir & String_Utils.Name_As_Directory (Directory_Name)
            & File_Name, Text);
      end if;

      for Column in Line'Range loop
         if View_Switches.Display (Column) /= null then
            View_Switches.Display (Column)
              (Viewer, File_Name, Directory_Name, File_Desc,
               Line (Column), Style (Column));
         else
            Line (Column) := New_String ("");
            Style (Column) := null;
         end if;
      end loop;

      Close (File_Desc);

      Row := Append (Viewer.List, Line);

      for S in Style'Range loop
         Set_Cell_Style
           (Viewer.List, Row, Gint (S - Style'First), Style (S));
      end loop;

      Free (Line);
      return Row;
   end Append_Line_With_Full_Name;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      Project          : Project_Type;
      File_Name        : String_Id;
      Directory_Filter : String := "")
   is
      File_N   : String (1 .. Integer (String_Length (File_Name)));
      Dir_Name : String_Id;

   begin
      String_To_Name_Buffer (File_Name);
      File_N := Name_Buffer (1 .. Name_Len);

      if Directory_Filter /= ""
        and then not Is_Regular_File
          (String_Utils.Name_As_Directory (Directory_Filter) & File_N)
      then
         return;
      end if;

      Dir_Name := Find_In_Source_Dirs (Project, File_N);
      pragma Assert (Dir_Name /= No_String);

      String_To_Name_Buffer (Dir_Name);
      Project_User_Data.Set
        (Viewer.List,
         Append_Line_With_Full_Name
           (Viewer, File_N, Name_Buffer (1 .. Name_Len)),
         (File_Name => File_Name, Directory => Dir_Name));
   end Append_Line;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      V            : constant Project_Viewer := Project_Viewer (Viewer);
      Row          : constant Gint := To_Gint (Args, 1);
      Column       : constant Gint := To_Gint (Args, 2);
      Event        : constant Gdk_Event := To_Event (Args, 3);
      User         : User_Data;
      Callback     : View_Callback;
      File         : File_Selection_Context_Access;

   begin
      --  Unless we selected the full row
      if Column /= -1 then
         Callback :=
           View_Switches.Callbacks (Interfaces.C.size_t (Column + 1));

         --  Event could be null when the row was selected programmatically
         if Event /= null
           and then Get_Event_Type (Event) = Gdk_2button_Press
           and then Callback /= null
         then
            User := Project_User_Data.Get (V.List, Row);

            File := new File_Selection_Context;
            Set_Context_Information
              (File, Kernel => V.Kernel,
               Creator => Module_ID (Prj_Editor_Module_ID));
            Set_File_Information
              (File,
               Directory    => Get_String (User.Directory),
               File_Name    => Get_String (User.File_Name),
               Project      => V.Project_Filter);

            Callback (V, Column, File);
            Free (Selection_Context_Access (File));
         end if;
      end if;
   end Select_Row;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed (Viewer  : access Gtk_Widget_Record'Class) is
      V : Project_Viewer := Project_Viewer (Viewer);
   begin
      Clear (V);  --  ??? Should delete selectively
      if V.Project_Filter /= No_Project then
         V.Current_Project := Get_Project (V.Kernel);
         Show_Project (V, V.Project_Filter);
      end if;
   end Project_View_Changed;

   --------------------------------
   -- Explorer_Selection_Changed --
   --------------------------------

   procedure Explorer_Selection_Changed
     (Viewer  : access Project_Viewer_Record'Class;
      Context : Selection_Context_Access)
   is
      User : User_Data;
      Rows : Gint;
      File : File_Selection_Context_Access;
      Child : MDI_Child;

   begin
      Child := Find_MDI_Child (Get_MDI (Viewer.Kernel), Viewer);
      if Child = null then
         Trace (Me, "No MDI window visiting the project viewer");
         return;
      end if;

      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         if File.all in File_Selection_Context'Class
           and then not Has_Directory_Information (File)
         then
            Set_Title (Child,
                       Title => -"Editing switches for project "
                         & Project_Name (Project_Information (File)),
                       Short_Title => Project_Switches_Name);
         else
            Set_Title (Child,
                       Title => -"Editing switches for directory "
                         & Directory_Information (File),
                       Short_Title => Project_Switches_Name);
         end if;

         Viewer.Current_Project := Project_Information (File);
         Clear (Viewer);  --  ??? Should delete selectively

         if Viewer.Current_Project /= No_Project then
            Show_Project (Viewer, Viewer.Current_Project,
                          Directory_Information (File));
         end if;

         if Has_File_Information (File) then
            Rows := Get_Rows (Viewer.List);

            for J in 0 .. Rows - 1 loop
               User := Project_User_Data.Get (Viewer.List, J);

               if Get_String (User.File_Name) = File_Information (File) then
                  Select_Row (Viewer.List, J, 0);
                  return;
               end if;
            end loop;
         end if;

      else
         Clear (Viewer);

         Viewer.Current_Project := Get_Project (Viewer.Kernel);
         Set_Title (Child,
                    Title => -"Editing switches for project "
                    & Project_Name (Viewer.Current_Project),
                    Short_Title => Project_Switches_Name);
         Show_Project (Viewer, Viewer.Current_Project, "");
      end if;
   end Explorer_Selection_Changed;

   --------------------------------
   -- Explorer_Selection_Changed --
   --------------------------------

   procedure Explorer_Selection_Changed
     (Viewer  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      Context      : constant Selection_Context_Access :=
        To_Selection_Context_Access (To_Address (Args, 1));
   begin
      Explorer_Selection_Changed (Project_Viewer (Viewer), Context);
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
      Scrolled : Gtk_Scrolled_Window;

   begin
      Gtk.Box.Initialize_Hbox (Viewer);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Viewer,
         Object          => Viewer,
         ID              => Module_ID (Prj_Editor_Module_ID),
         Context_Func    => Project_Editor_Context_Factory'Access);

      Viewer.Kernel := Kernel_Handle (Kernel);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Viewer, Scrolled);

      Gtk_New (Viewer.List,
               Columns => Gint (View_Switches.Num_Columns),
               Titles  => View_Switches.Titles);
      Set_Selection_Mode (Viewer.List, Selection_Multiple);
      Add (Scrolled, Viewer.List);
      Set_Column_Auto_Resize (Viewer.List, 0, True);

      Widget_Callback.Object_Connect
        (Viewer.List, "select_row",  Select_Row'Access, Viewer);

      Widget_Callback.Object_Connect
        (Kernel, Context_Changed_Signal,
         Explorer_Selection_Changed'Access,
         Viewer);
      Widget_Callback.Object_Connect
        (Kernel, Project_View_Changed_Signal,
         Widget_Callback.To_Marshaller (Project_View_Changed'Access),
         Viewer);

      Viewer.Default_Switches_Style := Copy (Get_Style (Viewer));
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
      Color : Gdk_Color;
   begin
      Color := Get_Pref (Kernel, Default_Switches_Color);
      Set_Foreground (V.Default_Switches_Style, State_Normal, Color);
   end Preferences_Changed;

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record;
      Project_Filter   : Project_Type;
      Directory_Filter : String := "")
   is
      Files : constant String_Id_Array := Get_Source_Files
        (Project_Filter, Recursive => False);
   begin
      Viewer.Project_Filter := Project_Filter;
      Freeze (Viewer.List);

      for F in Files'Range loop
         Append_Line (Viewer, Project_Filter, Files (F), Directory_Filter);
      end loop;

      Thaw (Viewer.List);
   end Show_Project;

   -----------
   -- Clear --
   -----------

   procedure Clear (Viewer : access Project_Viewer_Record) is
   begin
      Freeze (Viewer.List);
      Clear (Viewer.List);
      Thaw (Viewer.List);
   end Clear;

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
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Viewer_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Gtk_New (Viewer, Kernel);

         Set_Size_Request
           (Viewer,
            Get_Pref (Kernel, Default_Widget_Width),
            Get_Pref (Kernel, Default_Widget_Height));
         Child := Put (Get_MDI (Kernel), Viewer);
         Set_Focus_Child (Child);
         Set_Title (Child, Project_Switches_Name);

         --  The initial contents of the viewer should be read immediately from
         --  the explorer, without forcing the user to do a new selection.

         Explorer_Selection_Changed (Viewer, Context);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception "
                & Exception_Information (E));
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
      if Is_Default (Get_Project (Kernel)) then
         Read_Project_Name (Kernel, Get_Project (Kernel));
      end if;

      Save_Project (Kernel, Get_Project (Kernel), Recursive => True);
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
      if Is_Default (Project) then
         Read_Project_Name (Kernel, Project);
      end if;

      Save_Project (Kernel, Project);
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
               Project_Path (Project_Information (File_C)),
               From_Path => False);
         end if;
      end if;
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
            Name : constant String :=
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
            if Name /= "" then
               Add_Dependency_Internal (Get_Kernel (File), Prj, Name);
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
            Gtk_New (Item, Label => -"Save the project "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Save_Specific_Project'Access),
               Selection_Context_Access (Context));

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

            if Has_Importing_Project_Information (File_Context) then
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
      pragma Unreferenced (Kernel, Event_Widget);

      Context     : constant File_Selection_Context_Access :=
        new File_Selection_Context;
      V           : constant Project_Viewer := Project_Viewer (Object);
      Item        : Gtk_Menu_Item;
      Row, Column : Gint;
      Is_Valid    : Boolean;
      User        : User_Data;

   begin
      --  ??? Should call Project_Editor_Contextual

      Get_Selection_Info
        (V.List, Gint (Get_X (Event)), Gint (Get_Y (Event)),
         Row, Column, Is_Valid);

      if not Is_Valid then
         return null;
      end if;

      User := Project_User_Data.Get (V.List, Row);

      if User.File_Name /= No_String then
         Set_File_Information
           (Context,
            Directory    => Get_String (User.Directory),
            File_Name    => Get_String (User.File_Name),
            Project      => V.Project_Filter);
      else
         Set_File_Information
           (Context, Project => V.Project_Filter);
      end if;

      if Has_File_Information (Context) then
         Gtk_New (Item, -"Edit switches for all selected files");
         Add (Menu, Item);
         Context_Callback.Object_Connect
           (Item, "activate",
            Context_Callback.To_Marshaller (Edit_Multiple_Switches'Access),
            Slot_Object => V,
            User_Data => Selection_Context_Access (Context));
      end if;

      return Selection_Context_Access (Context);
   end Project_Editor_Context_Factory;

   ----------------------------
   -- Edit_Multiple_Switches --
   ----------------------------

   procedure Edit_Multiple_Switches
     (Viewer : access GObject_Record'Class; Context : Selection_Context_Access)
   is
      use Gtk.Enums.Gint_List;
      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      V : constant Project_Viewer := Project_Viewer (Viewer);
      Selection : Glist := Get_Selection (V.List);
      Names : Argument_List (1 .. Integer (Length (Selection)));
      User        : User_Data;
   begin
      for N in Names'Range loop
         User := Project_User_Data.Get (V.List, Get_Data (Selection));
         Names (N) := new String'(Get_String (User.File_Name));
         Selection := Next (Selection);
      end loop;

      Edit_Switches_For_Files
        (Get_Kernel (File),
         Project_Information (File),
         Names);
      Free (Names);

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
      Val  : GValue;
      Iter : Gtk_Tree_Iter;
   begin
      Init (Val, GType_String);
      Set_String (Val, File);
      Append (Editor.Executables, Iter);
      Set_Value
        (Editor.Executables,
         Iter   => Iter,
         Column => 0,
         Value  => Val);
      Unset (Val);
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
         File : constant String := Select_File
           (Title             => -"Select the main file to add",
            Base_Directory    => Base_Dir,
            File_Pattern      => "*.ad*",
            Pattern_Name      => -"Ada source files",
            Parent            => Gtk_Window (Get_Toplevel (Editor)),
            Use_Native_Dialog => Get_Pref (Ed.Kernel, Use_Native_Dialogs),
            Kind              => Unspecified,
            History           => Get_History (Ed.Kernel));

      begin
         if File /= "" then
            if Base_Dir = "" then
               Change_Dir (Dir_Name (File));
            end if;

            Add_Main_File (Ed, Base_Name (File));
         end if;
      end Add_File;

   begin
      if Ed.Project /= No_Project then
         declare
            Dirs : constant String_Id_Array := Source_Dirs (Ed.Project);
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

      Gtk_New (Box.Executables, (1 => GType_String));
      Gtk_New (Box.Tree_View, Model => Box.Executables);
      Set_Mode (Get_Selection (Box.Tree_View), Selection_Multiple);
      Set_Headers_Visible (Box.Tree_View, False);
      Gtk_New (Renderer);
      Gtk_New (Column);
      Pack_Start (Column, Renderer, Expand => True);
      Add_Attribute (Column, Renderer, "text", 0);
      Col := Append_Column (Box.Tree_View, Column);

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
              (Project, Attribute_Name => Main_Attribute);
         begin
            for M in Mains'Range loop
               Add_Main_File (Box, Mains (M).all);
            end loop;

            Free (Mains);
         end;
      end if;

      Show_All (Box);
      return Gtk_Widget (Box);
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
      pragma Unreferenced (Page, Kernel);
      Editor       : constant Executables_Editor :=
        Executables_Editor (Widget);
      Num_Children : constant Gint := N_Children (Editor.Executables);
      New_Mains    : Argument_List (1 .. Integer (Num_Children));
      Iter         : Gtk_Tree_Iter := Get_Iter_First (Editor.Executables);
      N            : Natural := New_Mains'First;
      Changed      : Boolean := False;
   begin
      Assert (Me, Project = Ref_Project,
              "Invalid project when modifying main files");

      --  First get the list of main files
      while Iter /= Null_Iter loop
         New_Mains (N) := new String'
           (Get_String (Editor.Executables, Iter, 0));
         N := N + 1;
         Next (Editor.Executables, Iter);
      end loop;

      --  ??? We only know how to get the value of an attribute in the current
      --  scenario, so the project might be reported as modified too often when
      --  changing multiple scenarios
      if Project /= No_Project then
         declare
            Old_Mains    : Argument_List := Get_Attribute_Value
              (Project, Attribute_Name => Main_Attribute);
         begin
            Changed := not Is_Equal (Old_Mains, New_Mains);
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
               Attribute_Name     => Main_Attribute,
               Values             => New_Mains);
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute_Name     => Main_Attribute);
         end if;
      end if;

      Free (New_Mains);

      return Changed;
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
            Initial_Dirs_Id : constant String_Id_Array := Source_Dirs
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
      pragma Unreferenced (Kernel, Page);
      Dirs     : Argument_List := Get_Multiple_Selection
        (Directory_Selector (Widget));
      Equal    : Boolean := False;
      Prj_Dir  : constant String := Project_Directory (Project);
      Tmp      : GNAT.OS_Lib.String_Access;
      Relative : constant Boolean :=
        Get_Paths_Type (Project) = Projects.Relative;
      Initial_Dirs_Id : constant String_Id_Array := Source_Dirs (Project);
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
            Pkg_Name           => "",
            Scenario_Variables => Scenario_Variables,
            Attribute_Name     => Get_String (Name_Source_Dirs),
            Values             => Dirs,
            Attribute_Index    => "",
            Prepend            => False);
      end if;

      Free (Dirs);

      return not Equal;
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
     (Page : access Object_Editor_Record;
      Project : Project_Type;
      Full_Project : String;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Page);
      Obj_Dir : Object_Editor_Widget;
      Label : Gtk_Label;
      Box : Gtk_Box;
      Event : Gtk_Event_Box;
      Button : Gtk_Button;
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
             (Get_Attribute_Value
              (Project => Project,
               Attribute_Name => Exec_Dir_Attribute,
               Default => Get_Current_Dir))));
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
                     Attribute_Name => Exec_Dir_Attribute,
                     Default => "@@@@") = "@@@@");

      Show_All (Obj_Dir);
      return Gtk_Widget (Obj_Dir);
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
      pragma Unreferenced (Kernel, Page);
      Obj_Dir : constant Object_Editor_Widget := Object_Editor_Widget (Widget);
      New_Dir, Exec_Dir : GNAT.OS_Lib.String_Access;
      Changed : Boolean := False;
      Project_Uses_Relative_Paths : constant Boolean :=
        Get_Paths_Type (Project) = Relative;

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
               Attribute_Name     => Get_String (Name_Object_Dir));

         else
            if not Is_Directory (New_Dir.all) then
               if Message_Dialog
                 (Msg => New_Dir.all
                  & (-" is not a directory, would you like to create it ?"),
                  Title => -"Directory not found",
                  Dialog_Type => Information,
                  Buttons => Button_Yes or Button_No) = Button_Yes
               then
                  Make_Dir (New_Dir.all);
               end if;
            end if;

            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute_Name     => Get_String (Name_Object_Dir),
               Value              => New_Dir.all);
         end if;

         Trace (Me, "Object directory was modified");
         Changed := True;
      end if;

      if (Get_Active (Obj_Dir.Same)
          and then Get_Text (Obj_Dir.Obj_Dir) /= Name_As_Directory
            (Normalize_Pathname (Executables_Directory (Project))))
        or else (not Get_Active (Obj_Dir.Same)
                 and then Get_Text (Obj_Dir.Exec_Dir) /= Name_As_Directory
                   (Normalize_Pathname (Executables_Directory (Project))))
      then
         if Exec_Dir.all = "" then
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute_Name     => Exec_Dir_Attribute);

         else
            if not Is_Directory (Exec_Dir.all) then
               if Message_Dialog
                 (Msg => Exec_Dir.all
                  & (-" is not a directory, would you like to create it ?"),
                  Title => -"Directory not found",
                  Dialog_Type => Information,
                  Buttons => Button_Yes or Button_No) = Button_Yes
               then
                  Make_Dir (Exec_Dir.all);
               end if;
            end if;

            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute_Name     => Exec_Dir_Attribute,
               Value              => Exec_Dir.all);
         end if;

         Trace (Me, "Exec directory was modified");
         Changed := True;
      end if;

      Free (New_Dir);
      Free (Exec_Dir);

      return Changed;
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
      pragma Unreferenced (Page, Kernel, Full_Project);
      Switches : Switches_Editors.Switches_Edit;
   begin
      Gtk_New (Switches);
      Show_All (Switches);
      Set_Switches (Switches, Project);

      return Gtk_Widget (Switches);
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
         Files              => (1 .. 0 => null));
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
         Show_Project_Settings (Editor, Kernel, Project);
      else
         Gtk_New (Editor, Kernel,
                  Known_Languages (Get_Language_Handler (Kernel)));
         Show (Editor);
      end if;

      Page.Kernel := Kernel_Handle (Kernel);
      return Gtk_Widget (Editor);
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

   function Project_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String is
   begin
      if Command = "prj_add_main_unit" then
         Update_Attribute_Value_In_Scenario
           (Project            => Get_Project (Kernel),
            Scenario_Variables => Scenario_Variables (Kernel),
            Attribute_Name     => Main_Attribute,
            Values             => Args,
            Prepend            => True);
         Recompute_View (Kernel);
      end if;

      return "";
   end Project_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
   begin
      Prj_Editor_Module_ID := new Prj_Editor_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Prj_Editor_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => Project_Editor_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Project_Editor_Contextual'Access);

      Register_Menu (Kernel, Project, null, Ref_Item => -"Edit",
                     Add_Before => False);

      Register_Menu (Kernel, Project, -"New...", "", On_New_Project'Access,
                     Ref_Item => -"Open...", Add_Before => True);

      Prj_Editor_Module_ID.Reopen_Menu := Register_Menu
        (Kernel, Project, -"Recent", "",
         null, Ref_Item => -"Open...", Add_Before => False);
      Refresh_Reopen_Menu (Kernel);

      Register_Menu
        (Kernel, Project, -"Edit Switches", "",
         On_Edit_Switches'Access, Ref_Item => -"Recent", Add_Before => False);
      Register_Menu
        (Kernel, Project, -"Edit Properties", "",
         On_Project_Properties'Access, Ref_Item => -"Recent",
         Add_Before => False);
      Register_Menu
        (Kernel, Project, -"Save All", "",
         Save_All_Projects'Access, Ref_Item => -"Edit Properties",
         Add_Before => False);
      Set_Tip
        (Get_Tooltips (Kernel),
         Register_Menu
         (Kernel, Project, -"Recompute Project", "",
          On_Project_Recompute'Access, Ref_Item => -"Edit Switches",
          Add_Before => False),
         Tip_Text => -("Recompute the contents of the project after"
                       & " modifications outside of GPS. This isn't needed"
                       & " for modifications through GPS. Note also that"
                       & " this doesn't reparse the physical project file"
                       & " on disk. Re-open the project if you have done"
                       & " manual modifications to it."));

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
         Command      => "prj_add_main_unit",
         Usage        => "prj_add_main_unit main1 [main2 ...]",
         Description  =>
           -("Add some main units to the current project, and for the"
             & " current scenario. The project is not saved automatically."),
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Project_Command_Handler'Access);
   end Register_Module;

end Project_Viewers;
