-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;
with Gtk.List_Store;               use Gtk.List_Store;
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Style;                    use Gtk.Style;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Vbutton_Box;              use Gtk.Vbutton_Box;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Window;                   use Gtk.Window;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtkada.File_Selector.Filters; use Gtkada.File_Selector.Filters;
with Gtkada.Types;                 use Gtkada.Types;

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;

with Basic_Types;              use Basic_Types;
with String_Utils;
with Prj_API;                  use Prj_API;
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
with Directory_Tree;           use Directory_Tree;
with Switches_Editors;         use Switches_Editors;
with Traces;                   use Traces;
with Variable_Editors;         use Variable_Editors;
with String_Utils;             use String_Utils;
with Project_Properties;       use Project_Properties;

with Prj;           use Prj;
with Prj.Tree;      use Prj.Tree;
with Stringt;       use Stringt;
with Types;         use Types;
with Namet;         use Namet;
with Snames;        use Snames;

package body Project_Viewers is

   Me : constant Debug_Handle := Create ("Project_Viewers");

   Prj_Editor_Module_ID : Module_ID;
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

   procedure Append_Line
     (Viewer           : access Project_Viewer_Record'Class;
      Project_View     : Project_Id;
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
     (Project_View : Project_Id; File : String) return String_Id;
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

   procedure Project_View_Changed (Viewer  : access Gtk_Widget_Record'Class);
   --  Called when the project view has changed.

   procedure Read_Project_Name
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Node_Id);
   --  Open a popup dialog to select a new name for Project.

   procedure Preferences_Changed
     (Viewer : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

   procedure Edit_Multiple_Switches
     (Viewer : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Edit the switches for all the files selected in Viewer.

   --------------------------
   -- Project editor pages --
   --------------------------

   type Main_Editor_Record is new Project_Editor_Page_Record with null record;
   function Widget_Factory
     (Page         : access Main_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Main_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean;

   type Source_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Source_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Source_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean;

   type Object_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Object_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Object_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean;

   type Switches_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Switches_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Switches_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean;
   procedure Refresh
     (Page         : access Switches_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project_View : Prj.Project_Id := Prj.No_Project;
      Languages    : GNAT.OS_Lib.Argument_List);

   type Naming_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access Naming_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access Naming_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean;
   procedure Refresh
     (Page         : access Naming_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project_View : Prj.Project_Id := Prj.No_Project;
      Languages    : GNAT.OS_Lib.Argument_List);

   ------------------------
   -- Main files editors --
   ------------------------

   type Executables_Editor_Record is new Gtk_Box_Record with record
      Executables  : Gtk_List_Store;
      Tree_View    : Gtk_Tree_View;
      Project_View : Project_Id;
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

   -------------------------
   -- Find_In_Source_Dirs --
   -------------------------

   function Find_In_Source_Dirs
     (Project_View : Project_Id; File : String) return String_Id
   is
      Dirs   : String_List_Id := Projects.Table (Project_View).Source_Dirs;
      File_A : GNAT.OS_Lib.String_Access;

   begin
      --  We do not use Ada_Include_Path to locate the source file,
      --  since this would include directories from imported project
      --  files, and thus slow down the search. Instead, we search
      --  in all the directories directly belong to the project.

      while Dirs /= Nil_String loop
         String_To_Name_Buffer (String_Elements.Table (Dirs).Value);
         File_A := Locate_Regular_File (File, Name_Buffer (1 .. Name_Len));

         if File_A /= null then
            Free (File_A);
            return String_Elements.Table (Dirs).Value;
         end if;

         Dirs := String_Elements.Table (Dirs).Next;
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
      Value      : Variable_Value;
      Is_Default : Boolean;

   begin
      --  ??? Should show the switches for the specific language of the file
      Get_Switches
        (Viewer.Project_Filter, "compiler", File_Name,
         Snames.Name_Ada, Value, Is_Default);
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
      Project_View     : Project_Id;
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

      Dir_Name := Find_In_Source_Dirs (Project_View, File_N);
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
              (File, Kernel => V.Kernel, Creator => Prj_Editor_Module_ID);
            Set_File_Information
              (File,
               Directory    => Get_String (User.Directory),
               File_Name    => Get_String (User.File_Name),
               Project_View => V.Project_Filter);

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
         V.Current_Project := Get_Project_View (V.Kernel);
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
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         Child := Find_MDI_Child (Get_MDI (Viewer.Kernel), Viewer);
         if Child = null then
            Trace (Me, "No MDI window visiting the project viewer");
            return;
         end if;

         if File.all in File_Selection_Context'Class
           and then not Has_Directory_Information (File)
         then
            Set_Title (Child,
                       Title => -"Editing switches for project "
                         & Project_Name (Project_Information (File)),
                       Short_Title => Project_Switches_Name);
            Viewer.Current_Project := Project_Information (File);
         else
            Set_Title (Child,
                       Title => -"Editing switches for directory "
                         & Directory_Information (File),
                       Short_Title => Project_Switches_Name);
            Viewer.Current_Project := Get_Project_From_Directory
              (Get_Project_View (Viewer.Kernel), Directory_Information (File));
         end if;

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
         ID              => Prj_Editor_Module_ID,
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
      V : Project_Viewer := Project_Viewer (Viewer);
      Color    : Gdk_Color;
   begin
      Color := Get_Pref (Kernel, Default_Switches_Color);
      Set_Foreground (V.Default_Switches_Style, State_Normal, Color);
   end Preferences_Changed;

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record;
      Project_Filter   : Prj.Project_Id;
      Directory_Filter : String := "")
   is
      Src : String_List_Id := Projects.Table (Project_Filter).Sources;
   begin
      Viewer.Project_Filter := Project_Filter;
      Freeze (Viewer.List);

      while Src /= Nil_String loop
         Append_Line
           (Viewer, Project_Filter,
            String_Elements.Table (Src).Value,
            Directory_Filter);
         Src := String_Elements.Table (Src).Next;
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

   begin
      Gtk_New (Load_Project_Page, 0.0, 0.5, 1.0, 0.0);
      Set_Border_Width (Load_Project_Page, 5);

      Gtk_New (Frame);
      Set_Border_Width (Frame, 5);
      Add (Load_Project_Page, Frame);

      Gtk_New (Load_Project, -"Automatically load the project");
      Set_Active (Load_Project, True);
      Add (Frame, Load_Project);

      Gtk_New (Wiz, Kernel);
      Add_Page
        (Wiz, Load_Project_Page, -"Loading the project", -"Load project");

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
         Trace (Me, "Unexpected exception " & Exception_Message (E));
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
         Set_Title (Child, Project_Switches_Name);

         --  The initial contents of the viewer should be read immediately from
         --  the explorer, without forcing the user to do a new selection.

         Explorer_Selection_Changed (Viewer, Context);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Message (E));
   end On_Edit_Switches;

   -----------------------
   -- Read_Project_Name --
   -----------------------

   procedure Read_Project_Name
     (Kernel : access Kernel_Handle_Record'Class; Project : Project_Node_Id)
   is
      procedure Report_Error (Msg : String);
      --  Report an error to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Console.Insert (Kernel, Msg);
      end Report_Error;


      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Text   : Gtk_Entry;
      Widget : Gtk_Widget;
      View   : constant Project_Id := Get_Project_View_From_Project (Project);
   begin
      Gtk_New (Dialog,
               Title  => -"Select name for project",
               Parent => Get_Main_Window (Kernel),
               Flags  => Modal or Destroy_With_Parent);
      Widget := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

      Gtk_New (Label, -"Enter name of project:");
      Pack_Start (Get_Vbox (Dialog), Label);

      Gtk_New (Text, 40);
      Set_Width_Chars (Text, 20);
      Set_Text (Text, Project_Name (View));
      Pack_Start (Get_Vbox (Dialog), Text);

      Show_All (Dialog);
      if Run (Dialog) = Gtk_Response_OK then
         Rename_And_Move
           (Root_Project  => Project,
            Project       => Project,
            New_Name      => Get_Text (Text),
            New_Path      => Dir_Name (Project_Path (View)),
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
      --  Are we still using the default project ?
      if Get_Project_File_Name (Kernel) = "" then
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
      Project : Project_Node_Id;
   begin
      Project := Get_Project_From_View (Project_Information (File));

      --  Are we still using the default project ?
      if Get_Project_File_Name (Kernel) = ""
        and then Project_Information (File) = Get_Project_View (Kernel)
      then
         Read_Project_Name (Kernel, Project);
      end if;

      Save_Project (Kernel, Project);
   end Save_Specific_Project;

   -----------------------------------
   -- On_Add_Dependency_From_Wizard --
   -----------------------------------

   procedure On_Add_Dependency_From_Wizard
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      procedure Report_Error (Msg : String);
      --  Report an error to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Console.Insert (Get_Kernel (Context), Msg);
      end Report_Error;

      Prj : Project_Node_Id;
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
               Prj := Get_Project_From_View (Project_Information (File));
               if Add_Imported_Project
                 (Prj, Name, Report_Error'Unrestricted_Access)
               then
                  Set_Project_Modified (Get_Kernel (Context), Prj, True);
                  Recompute_View (Get_Kernel (Context));
               end if;
            end if;
         end;

         Destroy (Wiz);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Message (E));
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
      Prj : constant Project_Node_Id :=
        Get_Project_From_View (Importing_Project_Information (File));
   begin
      Remove_Imported_Project
        (Prj, Project_Name (Project_Information (File)));
      Trace (Me, "Removing project dependency");
      Trace_Pretty_Print (Me, Prj);
      Set_Project_Modified (Get_Kernel (Context), Prj, True);
      Recompute_View (Get_Kernel (Context));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Message (E));
   end Remove_Project_Dependency;

   -------------------------------------
   -- On_Add_Dependency_From_Existing --
   -------------------------------------

   procedure On_Add_Dependency_From_Existing
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the glide console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Console.Insert
           (Get_Kernel (Context), S, Mode => Console.Error, Add_LF => False);
      end Report_Error;

      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Selector : File_Selector_Window_Access;
      Prj : constant Project_Node_Id :=
        Get_Project_From_View (Project_Information (File));

      Dir : constant String := Get_Name_String (Path_Name_Of (Prj));
   begin
      if Has_Project_Information (File) then
         Gtk_New (Selector,
                  Root => (1 => Directory_Separator),
                  Initial_Directory => Dir_Name (Dir),
                  Dialog_Title => -"Select project");
         Register_Filter (Selector, Prj_File_Filter);

         declare
            Name : constant String := Select_File (Selector);
            Base : constant String := Get_Name_String (Directory_Of (Prj));
            Changed : Boolean;
         begin
            if Name /= "" then
               if Project_Uses_Relative_Paths (Get_Kernel (File), Prj) then
                  Changed := Add_Imported_Project
                    (Prj,
                     Relative_Path_Name (Name, Base),
                     Report_Error'Unrestricted_Access);
               else
                  Changed := Add_Imported_Project
                    (Prj, Normalize_Pathname (Name, Base),
                     Report_Error'Unrestricted_Access);
               end if;

               if Changed then
                  Set_Project_Modified (Get_Kernel (Context), Prj, True);
                  Recompute_View (Get_Kernel (Context));
               end if;
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

            Gtk_New (Item, -"From file...");
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

         if Has_File_Information (File_Context) then
            Gtk_New (Item, Label => "");
            Append (Menu, Item);

            Gtk_New (Item, Label => -"Edit switches for "
                     & Base_Name (File_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Switches'Access),
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
            Project_View => V.Project_Filter);
      else
         Set_File_Information
           (Context, Project_View => V.Project_Filter);
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
         Names (N) := new String' (Get_String (User.File_Name));
         Selection := Next (Selection);
      end loop;

      Edit_Switches_For_Files
        (Get_Kernel (File),
         Get_Project_From_View (Project_Information (File)),
         Project_Information (File),
         Names);
      Free (Names);
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
      Ed : Executables_Editor := Executables_Editor (Editor);
      Dirs : constant String_Id_Array := Source_Dirs (Ed.Project_View);
   begin
      if Dirs'Length = 0 then
         declare
            File : constant String := Select_File
              (Title          => -"Select the main unit to add",
               Base_Directory => Dir_Name (Project_Path (Ed.Project_View)));
         begin
            if File /= "" then
               Add_Main_File (Ed, Base_Name (File));
            end if;
         end;

      else
         declare
            File : constant String := Select_File
              (Title          => -"Select the main unit to add",
               Base_Directory => Get_String (Dirs (Dirs'First)));
         begin
            if File /= "" then
               Add_Main_File (Ed, Base_Name (File));
            end if;
         end;
      end if;
   end Add_Main_Unit;

   ----------------------
   -- Remove_Main_Unit --
   ----------------------

   procedure Remove_Main_Unit (Editor : access Gtk_Widget_Record'Class) is
      Ed : Executables_Editor := Executables_Editor (Editor);
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
      Project_View : Project_Id; Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      pragma Unreferenced (Page, Kernel);
      Box      : Executables_Editor;
      Hbox     : Gtk_Box;
      Label    : Gtk_Label;
      Column   : Gtk_Tree_View_Column;
      Renderer : Gtk_Cell_Renderer_Text;
      Col      : Gint;
      Scrolled : Gtk_Scrolled_Window;
      Bbox     : Gtk_Vbutton_Box;
      Button2  : Gtk_Button;

   begin
      Box := new Executables_Editor_Record;
      Box.Project_View := Project_View;
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

      if Project_View /= No_Project then
         declare
            Mains : Argument_List := Get_Attribute_Value
              (Project_View,
               Attribute_Name => Main_Attribute);
         begin
            for M in Mains'Range loop
               Add_Main_File (Box, Mains (M).all);
            end loop;
            Free (Mains);
         end;
      end if;

      return Gtk_Widget (Box);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Main_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Page);
      Editor       : Executables_Editor := Executables_Editor (Widget);
      Num_Children : constant Gint := N_Children (Editor.Executables);
      New_Mains    : Argument_List (1 .. Integer (Num_Children));
      Iter         : Gtk_Tree_Iter := Get_Iter_First (Editor.Executables);
      N            : Natural := New_Mains'First;
      Changed      : Boolean := False;

   begin
      --  First get the list of main files
      while Iter /= Null_Iter loop
         New_Mains (N) := new String'
           (Get_String (Editor.Executables, Iter, 0));
         N := N + 1;
         Next (Editor.Executables, Iter);
      end loop;

      if Project_View /= No_Project then
         declare
            Old_Mains    : Argument_List := Get_Attribute_Value
              (Project_View,
               Attribute_Name => Main_Attribute);
         begin
            Changed := not Is_Equal (Old_Mains, New_Mains);
            Free (Old_Mains);
         end;
      end if;

      if Changed then
         if New_Mains'Length /= 0 then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Pkg_Name           => "",
               Scenario_Variables => Scenario_Variables (Kernel),
               Attribute_Name     => Main_Attribute,
               Values             => New_Mains);
         else
            Delete_Attribute
              (Project            => Project,
               Pkg_Name           => "",
               Scenario_Variables => Scenario_Variables (Kernel),
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
     (Page : access Source_Editor_Record;
      Project_View : Project_Id; Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      pragma Unreferenced (Page);
      Src_Dir_Selection : Directory_Tree.Directory_Selector;
   begin
      if Project_View /= No_Project then
         declare
            Initial_Dirs_Id : constant String_Id_Array := Source_Dirs
              (Project_View);
            Initial_Dirs : Argument_List (Initial_Dirs_Id'Range);
         begin
            for J in Initial_Dirs_Id'Range loop
               Initial_Dirs (J) :=
                 new String' (Get_String (Initial_Dirs_Id (J)));
            end loop;

            Gtk_New
              (Src_Dir_Selection,
               Initial_Directory => Dir_Name (Project_Path (Project_View)),
               Multiple_Directories => True,
               Busy_Cursor_On => Get_Window (Get_Main_Window (Kernel)),
               Initial_Selection => Initial_Dirs);
            Free (Initial_Dirs);
         end;
      else
         declare
            Initial_Dirs : Argument_List (1 .. 1);
         begin
            Initial_Dirs (1) := new String' (Get_Current_Dir);
            Gtk_New
              (Src_Dir_Selection,
               Initial_Directory    => Get_Current_Dir,
               Multiple_Directories => True,
               Busy_Cursor_On       => Get_Window (Get_Main_Window (Kernel)),
               Initial_Selection    => Initial_Dirs);
            Free (Initial_Dirs);
         end;
      end if;

      return Gtk_Widget (Src_Dir_Selection);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Source_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Page);
      Dirs     : Argument_List := Get_Multiple_Selection
        (Directory_Selector (Widget));
      Equal    : Boolean := False;
      Prj_Dir  : constant String := Project_Path (Project);
      Tmp      : GNAT.OS_Lib.String_Access;
      Relative : constant Boolean :=
        Project_Uses_Relative_Paths (Kernel, Project);
   begin
      if Relative then
         for J in Dirs'Range loop
            Tmp := Dirs (J);
            Dirs (J) := new String' (Relative_Path_Name (Tmp.all, Prj_Dir));
            Free (Tmp);
         end loop;
      end if;

      if Project_View /= No_Project then
         declare
            Initial_Dirs_Id : constant String_Id_Array := Source_Dirs
              (Project_View);
            Initial_Dirs : Argument_List (Initial_Dirs_Id'Range);
         begin
            for J in Initial_Dirs_Id'Range loop
               declare
                  Str : constant String := Get_String (Initial_Dirs_Id (J));
               begin
                  --  Initial_Dirs will always contained an unnormalized,
                  --  absolute path, therefore we need to trim it first before
                  --  comparing the old and new values.
                  if Relative then
                     if Str'Length > Prj_Dir'Length
                       and then Str
                       (Str'First .. Str'First + Prj_Dir'Length - 1) = Prj_Dir
                     then
                        Initial_Dirs (J) := new String'
                          (Str (Str'First + Prj_Dir'Length .. Str'Last));
                     else
                        Initial_Dirs (J) := new String' (Str);
                     end if;
                  else
                     Initial_Dirs (J) := new String' (Str);
                  end if;
               end;
            end loop;

            Equal := Is_Equal (Dirs, Initial_Dirs);
            Free (Initial_Dirs);
         end;

         if not Equal then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Pkg_Name           => "",
               Scenario_Variables => Scenario_Variables (Kernel),
               Attribute_Name     => Get_Name_String (Name_Source_Dirs),
               Values             => Dirs,
               Attribute_Index    => "",
               Prepend            => False);
         end if;

      else
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Pkg_Name           => "",
            Scenario_Variables => (1 .. 0 => Empty_Node),
            Attribute_Name     => Get_Name_String (Name_Source_Dirs),
            Values             => Dirs,
            Attribute_Index    => "",
            Prepend            => False);
      end if;

      Free (Dirs);

      return not Equal;
   end Project_Editor;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page : access Object_Editor_Record;
      Project_View : Project_Id; Kernel : access Kernel_Handle_Record'Class)
     return Gtk_Widget
   is
      pragma Unreferenced (Page);
      Obj_Dir_Selection : Directory_Tree.Directory_Selector;
   begin
      if Project_View /= No_Project then
         Gtk_New
           (Obj_Dir_Selection,
            Initial_Directory => Name_As_Directory
              (GNAT.OS_Lib.Normalize_Pathname (Get_Name_String
                 (Prj.Projects.Table (Project_View).Object_Directory))),
            Multiple_Directories => False,
            Busy_Cursor_On => Get_Window (Get_Main_Window (Kernel)));

      else
         Gtk_New
           (Obj_Dir_Selection,
            Initial_Directory => Get_Current_Dir,
            Multiple_Directories => False,
            Busy_Cursor_On => Get_Window (Get_Main_Window (Kernel)));
      end if;

      return Gtk_Widget (Obj_Dir_Selection);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Object_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Page);
      New_Dir : constant String := Get_Single_Selection
        (Directory_Selector (Widget));

   begin
      if Project_View /= No_Project then
         if New_Dir /= Name_As_Directory
           (Normalize_Pathname (Get_Name_String
             (Prj.Projects.Table (Project_View).Object_Directory)))
         then
            if New_Dir = "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => "",
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Get_Name_String (Name_Object_Dir),
                  Value              => ".",
                  Attribute_Index    => "");

            elsif Project_Uses_Relative_Paths (Kernel, Project) then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => "",
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Get_Name_String (Name_Object_Dir),
                  Value              => Relative_Path_Name
                  (New_Dir, Project_Path (Project)),
                  Attribute_Index    => "");

            else
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => "",
                  Scenario_Variables => Scenario_Variables (Kernel),
                  Attribute_Name     => Get_Name_String (Name_Object_Dir),
                  Value              => New_Dir,
                  Attribute_Index    => "");
            end if;

            return True;
         end if;

      else
         if New_Dir = "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Pkg_Name           => "",
               Scenario_Variables => (1 .. 0 => Empty_Node),
               Attribute_Name     => Get_Name_String (Name_Object_Dir),
               Value              => ".",
               Attribute_Index    => "");

         elsif Project_Uses_Relative_Paths (Kernel, Project) then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Pkg_Name           => "",
               Scenario_Variables => (1 .. 0 => Empty_Node),
               Attribute_Name     => Get_Name_String (Name_Object_Dir),
               Value              => Relative_Path_Name
               (New_Dir, Project_Path (Project)),
               Attribute_Index    => "");

         else
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Pkg_Name           => "",
               Scenario_Variables => (1 .. 0 => Empty_Node),
               Attribute_Name     => Get_Name_String (Name_Object_Dir),
               Value              => New_Dir,
               Attribute_Index    => "");
         end if;

         return True;
      end if;

      return False;
   end Project_Editor;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page : access Switches_Editor_Record;
      Project_View : Project_Id; Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      pragma Unreferenced (Page, Kernel);
      Switches : Switches_Editors.Switches_Edit;
   begin
      Gtk_New (Switches);

      if Project_View /= No_Project then
         declare
            Languages : Argument_List := Get_Languages (Project_View);
         begin
            Set_Visible_Pages (Switches, Languages);
            Free (Languages);
         end;

         Set_Switches (Switches, Project_View);
      end if;

      return Get_Window (Switches);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Switches_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Page);
   begin
      return Generate_Project
        (Switches     => From_Window (Widget),
         Kernel       => Kernel,
         Project      => Project,
         Project_View => Project_View,
         Files        => (1 .. 0 => null));
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page         : access Switches_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project_View : Prj.Project_Id := Prj.No_Project;
      Languages    : GNAT.OS_Lib.Argument_List)
   is
      pragma Unreferenced (Page, Project_View);
   begin
      Set_Visible_Pages (From_Window (Widget), Languages);
   end Refresh;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access Naming_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Page);
      Editor : Naming_Editor;
   begin
      if Project_View /= No_Project then
         Gtk_New (Editor, Project_View);
         Show_Project_Settings (Editor, Project_View);
      else
         Gtk_New (Editor, Known_Languages (Get_Language_Handler (Kernel)));
      end if;

      return Gtk_Widget (Editor);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access Naming_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Page);
   begin
      return Create_Project_Entry
        (Naming_Editor (Widget), Kernel, Project, Project_View,
         Ignore_Scenario => Project_View = No_Project);
   end Project_Editor;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page         : access Naming_Editor_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project_View : Prj.Project_Id := Prj.No_Project;
      Languages    : GNAT.OS_Lib.Argument_List)
   is
      pragma Unreferenced (Page);
   begin
      Set_Visible_Pages (Naming_Editor (Widget), Languages, Project_View);
   end Refresh;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
   begin
      Register_Module
        (Module                  => Prj_Editor_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Project_Editor_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Project_Editor_Contextual'Access);

      Register_Menu
        (Kernel, Project, -"New...", "",
         On_New_Project'Access, Ref_Item => -"Open...");
      Register_Menu
        (Kernel, Project, -"Edit Switches", "",
         On_Edit_Switches'Access, Ref_Item => -"Open...", Add_Before => False);
      Register_Menu
        (Kernel, Project, -"Save All", "",
         Save_All_Projects'Access, Ref_Item => -"Edit Switches",
         Add_Before => False);

      Register_Project_Editor_Page
        (Kernel,
         Page  => new Source_Editor_Record,
         Label => -"Sources",
         Toc   => -"Selecting sources",
         Title => -"Please select the source directories for this project");
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Object_Editor_Record,
         Label => -"Objects",
         Toc   => -"Build directory",
         Title => -"Please select the build directory for this project");
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Main_Editor_Record,
         Label => -"Main files",
         Toc   => -"Selecting main units",
         Title => -"Please select the main units for this project");
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Switches_Editor_Record,
         Label => -"Switches",
         Toc   => -"Switches",
         Title => -"Please select the switches to build the project");
      Register_Project_Editor_Page
        (Kernel,
         Page  => new Naming_Editor_Record,
         Label => -"Naming scheme",
         Toc   => -"Naming scheme",
         Title => -"Please select the naming scheme to use");
   end Register_Module;

end Project_Viewers;
