-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Gtk.Alignment;                use Gtk.Alignment;
with Gtk.Arguments;                use Gtk.Arguments;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Clist;                    use Gtk.Clist;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.Menu;                     use Gtk.Menu;
with Gtk.Menu_Item;                use Gtk.Menu_Item;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Style;                    use Gtk.Style;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Window;                   use Gtk.Window;
with Gtkada.Handlers;              use Gtkada.Handlers;
with Gtkada.MDI;                   use Gtkada.MDI;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtkada.File_Selector.Filters; use Gtkada.File_Selector.Filters;
with Gtkada.Types;                 use Gtkada.Types;

with Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Calendar;             use GNAT.Calendar;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Interfaces.C;              use Interfaces.C;

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
with Directory_Tree;           use Directory_Tree;
with Switches_Editors;         use Switches_Editors;
with Traces;                   use Traces;
with Variable_Editors;         use Variable_Editors;

with Prj;           use Prj;
with Prj.Tree;      use Prj.Tree;
with Stringt;       use Stringt;
with Types;         use Types;
with Namet;         use Namet;
with Snames;        use Snames;

package body Project_Viewers is

   Me : Debug_Handle := Create ("Project_Viewers");

   Prj_Editor_Module_ID : Module_ID;
   --  Id for the project editor module

   Default_Project_Width  : constant := 400;
   Default_Project_Height : constant := 400;
   --  <preferences>

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
   type View_Description_Access is access constant View_Description;

   procedure Name_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style);
   --  Return the name of the file

   procedure Size_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style);
   --  Return the size of the file

   procedure Timestamp_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style);
   --  Return the timestamp for the file

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
      Project_View   : Project_Id;
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

   procedure Edit_Source_Dirs_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the contextual menu item to add some source directories

   procedure Change_Obj_Directory_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Change the object directory associated with a specific project

   procedure On_New_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->New menu

   procedure On_Edit_Switches
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Callback for the Project->Edit Switches menu

   procedure On_Edit_Naming_Scheme
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   procedure On_Edit_Naming_Scheme
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callbacks for Project->Edit Naming Scheme, or the contextual menu

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

   -------------------------
   -- Find_In_Source_Dirs --
   -------------------------

   function Find_In_Source_Dirs
     (Project_View : Project_Id; File : String) return String_Id
   is
      Dirs   : String_List_Id := Projects.Table (Project_View).Source_Dirs;
      File_A : String_Access;

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
      pragma Warnings (Off, Viewer);
      pragma Warnings (Off, Directory);
      pragma Warnings (Off, Fd);
   begin
      Style := null;
      Line  := New_String (File_Name);
   end Name_Display;

   ------------------
   -- Size_Display --
   ------------------

   procedure Size_Display
     (Viewer    : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style)
   is
      pragma Warnings (Off, Viewer);
      pragma Warnings (Off, Directory);
      pragma Warnings (Off, File_Name);
   begin
      Style := null;
      Line := New_String (Long_Integer'Image (File_Length (Fd)));
   end Size_Display;

   -----------------------
   -- Timestamp_Display --
   -----------------------

   procedure Timestamp_Display
     (Viewer    : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor;
      Line      : out Interfaces.C.Strings.chars_ptr;
      Style     : out Gtk_Style)
   is
      pragma Warnings (Off, Viewer);
      pragma Warnings (Off, Directory);
      pragma Warnings (Off, File_Name);

      type Char_Pointer is access Character;

      type tm is record
         tm_sec    : Integer;
         tm_min    : Integer;
         tm_hour   : Integer;
         tm_mday   : Integer;
         tm_mon    : Integer;
         tm_year   : Integer;
         tm_wday   : Integer;
         tm_yday   : Integer;
         tm_isdst  : Integer;
         tm_gmtoff : Long_Integer;
         tm_zone   : Char_Pointer;
      end record;

      procedure localtime_r
        (C : in out OS_Time; res : out tm);
      pragma Import (C, localtime_r, "__gnat_localtime_r");

      T      : tm;
      A_Time : Ada.Calendar.Time;
      O_Time : OS_Time;

   begin
      O_Time := File_Time_Stamp (Fd);
      localtime_r (O_Time, T);

      --  Make sure the values returned by localtime are in the
      --  appropriate range

      T.tm_mon := T.tm_mon + 1;
      A_Time := Time_Of (1900 + T.tm_year, T.tm_mon, T.tm_mday,
                         T.tm_hour, T.tm_min, T.tm_sec);
      Line := New_String
        (Image (A_Time,
                Picture_String (Get_Pref (Viewer.Kernel, Timestamp_Picture))));
      Style := null;
   end Timestamp_Display;

   ----------------------------
   -- Edit_Switches_Callback --
   ----------------------------

   procedure Edit_Switches_Callback
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      Context   : File_Selection_Context_Access)
   is
      pragma Warnings (Off, Column);
   begin
      Edit_Switches_For_Context
        (Selection_Context_Access (Context), Force_Default => False);
   end Edit_Switches_Callback;

   --------------------------------
   -- Append_Line_With_Full_Name --
   --------------------------------

   function Append_Line_With_Full_Name
     (Viewer         : access Project_Viewer_Record'Class;
      Project_View   : Project_Id;
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
         File_Desc := Open_Read (Directory_Name & Directory_Separator
                                 & File_Name & ASCII.Nul, Text);
      else
         File_Desc := Open_Read
           (Get_Current_Dir & Directory_Name & Directory_Separator
            & File_Name & ASCII.Nul, Text);
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
        (Directory_Filter & Directory_Separator & File_N)
      then
         return;
      end if;

      Dir_Name := Find_In_Source_Dirs (Project_View, File_N);
      pragma Assert (Dir_Name /= No_String);

      String_To_Name_Buffer (Dir_Name);
      Project_User_Data.Set
        (Viewer.List,
         Append_Line_With_Full_Name
           (Viewer, Project_View, File_N, Name_Buffer (1 .. Name_Len)),
         (File_Name => File_Name, Directory => Dir_Name));
   end Append_Line;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      V            : Project_Viewer := Project_Viewer (Viewer);
      Row          : Gint := To_Gint (Args, 1);
      Column       : Gint := To_Gint (Args, 2);
      Event        : Gdk_Event := To_Event (Args, 3);
      User         : User_Data;
      Callback     : View_Callback;
      File         : File_Selection_Context_Access;

   begin
      Callback := View_Switches.Callbacks (Interfaces.C.size_t (Column + 1));

      --  Event could be null when the row was selected programmatically
      if Event /= null
        and then Get_Event_Type (Event) = Gdk_2button_Press
        and then Callback /= null
      then
         User := Project_User_Data.Get (V.List, Row);

         File := new File_Selection_Context;
         Set_Context_Information
           (File, Kernel => V.Kernel, Creator => Prj_Editor_Module_ID);
         Set_File_Name_Information
           (File,
            Directory    => Get_String (User.Directory),
            File_Name    => Get_String (User.File_Name));
         Set_File_Information (File, Project_View => V.Project_Filter);

         Callback (V, Column, File);
         Free (Selection_Context_Access (File));
      end if;
   end Select_Row;

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

   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);
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
      end if;
   end Explorer_Selection_Changed;

   --------------------------------
   -- Explorer_Selection_Changed --
   --------------------------------

   procedure Explorer_Selection_Changed
     (Viewer  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      Context      : Selection_Context_Access :=
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
      Color    : Gdk_Color;
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
      Add (Scrolled, Viewer.List);
      Set_Column_Auto_Resize (Viewer.List, 0, True);

      Widget_Callback.Object_Connect
        (Viewer.List, "select_row",  Select_Row'Access, Viewer);

      Widget_Callback.Object_Connect
        (Kernel, Context_Changed_Signal,
         Explorer_Selection_Changed'Access,
         Viewer);

      Color := Get_Pref (Kernel, Default_Switches_Color);
      Viewer.Default_Switches_Style := Copy (Get_Style (Viewer));
      Set_Foreground (Viewer.Default_Switches_Style, State_Normal, Color);

      Show_All (Viewer);
   end Initialize;

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

   --------------------------------------
   -- Edit_Source_Dirs_From_Contextual --
   --------------------------------------

   procedure Edit_Source_Dirs_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File_Context : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Initial_Dirs_Id : String_Id_Array := Source_Dirs
        (Project_Information (File_Context));
      Initial_Dirs : Argument_List (Initial_Dirs_Id'Range);
      Selector : Directory_Selector;
   begin
      for J in Initial_Dirs_Id'Range loop
         Initial_Dirs (J) := new String'
           (Get_String (Initial_Dirs_Id (J)));
      end loop;

      Gtk_New
        (Selector,
         Initial_Directory => Get_Current_Dir,
         Multiple_Directories => True,
         Busy_Cursor_On => Get_Window (Get_Main_Window (Get_Kernel (Context))),
         Initial_Selection => Initial_Dirs);

      if Run (Selector,
              -"Select source directories",
              Get_Main_Window (Get_Kernel (Context))) =
        Gtk_Response_OK
      then
         declare
            Dirs : Argument_List := Get_Multiple_Selection (Selector);
         begin
            Update_Attribute_Value_In_Scenario
              (Project            => Get_Project_From_View
                 (Project_Information (File_Context)),
               Pkg_Name           => "",
               Scenario_Variables => Scenario_Variables (Get_Kernel (Context)),
               Attribute_Name     => Get_Name_String (Name_Source_Dirs),
               Values             => Dirs,
               Attribute_Index    => "",
               Prepend            => False);
            Free (Dirs);
            Recompute_View (Get_Kernel (Context));
         end;
      end if;

      Destroy (Selector);

      Free (Initial_Dirs);
   end Edit_Source_Dirs_From_Contextual;

   ------------------------------------------
   -- Change_Obj_Directory_From_Contextual --
   ------------------------------------------

   procedure Change_Obj_Directory_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Selector : Directory_Selector;
      Project : Project_Id :=
        Project_Information (File_Selection_Context_Access (Context));
   begin
      Gtk_New
        (Selector,
         Initial_Directory => GNAT.OS_Lib.Normalize_Pathname
           (Get_Name_String (Prj.Projects.Table (Project).Object_Directory))
           & Directory_Separator,
         Busy_Cursor_On    =>
           Get_Window (Get_Main_Window (Get_Kernel (Context))));

      if Run (Selector, -"Select object directory",
              Get_Main_Window (Get_Kernel (Context))) =
        Gtk_Response_OK
      then
         declare
            Dir : constant String := Get_Single_Selection (Selector);
         begin
            if Dir /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Get_Project_From_View (Project),
                  Pkg_Name           => "",
                  Scenario_Variables =>
                    Scenario_Variables (Get_Kernel (Context)),
                  Attribute_Name     => Get_Name_String (Name_Object_Dir),
                  Value              => Dir,
                  Attribute_Index    => "");
               Recompute_View (Get_Kernel (Context));
            end if;
         end;
      end if;
      Destroy (Selector);
   end Change_Obj_Directory_From_Contextual;

   --------------------
   -- On_New_Project --
   --------------------

   procedure On_New_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Wiz : Creation_Wizard.Prj_Wizard;
      Load_Project_Page : Gtk_Alignment;
      Frame : Gtk_Frame;
      Load_Project : Gtk_Check_Button;
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
      Add_Page (Wiz, Load_Project_Page, "Loading the project", "Load project");

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
   end On_New_Project;

   ------------------------
   -- On_Editor_Switches --
   ------------------------

   procedure On_Edit_Switches
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Child  : MDI_Child;
      Viewer : Project_Viewer;
   begin
      Child := Find_MDI_Child_By_Tag (Get_MDI (Kernel),
                                      Project_Viewer_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Gtk_New (Viewer, Kernel);

         --  The initial contents of the viewer should be read immediately from
         --  the explorer, without forcing the user to do a new selection.

         Explorer_Selection_Changed
           (Viewer, Get_Current_Explorer_Context (Kernel));

         Set_Size_Request
           (Viewer, Default_Project_Width, Default_Project_Height);
         Child := Put (Get_MDI (Kernel), Viewer);
         Set_Title (Child, Project_Switches_Name);
      end if;
   end On_Edit_Switches;

   -----------------------
   -- Save_All_Projects --
   -----------------------

   procedure Save_All_Projects
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) is
   begin
      Save_Project (Get_Project (Kernel), Recursive => True);
   end Save_All_Projects;

   ---------------------------
   -- Save_Specific_Project --
   ---------------------------

   procedure Save_Specific_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Save_Project
        (Get_Project_From_View
         (Project_Information (File_Selection_Context_Access (Context))));
   end Save_Specific_Project;

   ---------------------------
   -- On_Edit_Naming_Scheme --
   ---------------------------

   procedure On_Edit_Naming_Scheme
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) is
   begin
      On_Edit_Naming_Scheme
        (Gtk_Widget (Widget), Get_Current_Explorer_Context (Kernel));
   end On_Edit_Naming_Scheme;

   ---------------------------
   -- On_Edit_Naming_Scheme --
   ---------------------------

   procedure On_Edit_Naming_Scheme
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      if Has_Project_Information (File)
        and then Edit_Naming_Scheme
        (Get_Main_Window (Get_Kernel (Context)),
         Get_Project_From_View (Project_Information (File)),
         Project_Information (File))
      then
         Recompute_View (Get_Kernel (Context));
      end if;
   end On_Edit_Naming_Scheme;

   -----------------------------------
   -- On_Add_Dependency_From_Wizard --
   -----------------------------------

   procedure On_Add_Dependency_From_Wizard
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Wiz  : Creation_Wizard.Prj_Wizard;
   begin
      if Has_Project_Information (File) then
         Gtk_New (Wiz, Get_Kernel (Context));
         declare
            Name : constant String := Run (Wiz);
         begin
            if Name /= "" then
               Add_Imported_Project
                 (Get_Project_From_View (Project_Information (File)), Name);
            end if;
         exception
            when E : Project_Warning | Project_Error =>
               Insert (Get_Kernel (Context), Exception_Message (E));
         end;
         Destroy (Wiz);
         Recompute_View (Get_Kernel (Context));
      end if;
   end On_Add_Dependency_From_Wizard;

   -------------------------------
   -- Remove_Project_Dependency --
   -------------------------------

   procedure Remove_Project_Dependency
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
   begin
      Remove_Imported_Project
        (Get_Project_From_View (Importing_Project_Information (File)),
         Project_Name (Project_Information (File)));
      Trace (Me, "Removing project dependency");
      Trace_Pretty_Print
        (Me, Get_Project_From_View (Importing_Project_Information (File)));
      Recompute_View (Get_Kernel (Context));
   end Remove_Project_Dependency;

   -------------------------------------
   -- On_Add_Dependency_From_Existing --
   -------------------------------------

   procedure On_Add_Dependency_From_Existing
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Selector : File_Selector_Window_Access;

      Dir : constant String := Get_Name_String
        (Path_Name_Of (Get_Project_From_View (Project_Information (File))));
   begin
      if Has_Project_Information (File) then
         Gtk_New (Selector,
                  Root => "/",
                  Initial_Directory => Dir_Name (Dir),
                  Dialog_Title => -"Select project");
         Register_Filter (Selector, Prj_File_Filter);

         declare
            Name : constant String := Select_File (Selector);
         begin
            if Name /= "" then
               Add_Imported_Project
                 (Get_Project_From_View (Project_Information (File)), Name);
            end if;
         end;
         Recompute_View (Get_Kernel (Context));
      end if;

   exception
      when E : Project_Warning | Project_Error =>
         Insert (Get_Kernel (Context), Exception_Message (E));
   end On_Add_Dependency_From_Existing;

   -------------------------------
   -- Project_Editor_Contextual --
   -------------------------------

   procedure Project_Editor_Contextual
     (Object    : access GObject_Record'Class;
      Context   : access Selection_Context'Class;
      Menu      : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Item : Gtk_Menu_Item;
      Submenu : Gtk_Menu;
      File_Context : File_Selection_Context_Access;
   begin
      --  We insert entries whatever the sender_id is, as long as the context
      --  knows something about project or files

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

            Gtk_New (Item, Label => -"Edit Default Switches for "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Default_Switches'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, -"Edit naming scheme for "
                     & Project_Name (Project_Information (File_Context)));
            Add (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Edit_Naming_Scheme'Access),
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

         if Has_Project_Information (File_Context)
           and then not Has_File_Information (File_Context)
         then
            Gtk_New (Item, Label => -"Edit Source Directories for "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Source_Dirs_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Edit Object Directory for "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Change_Obj_Directory_From_Contextual'Access),
               Selection_Context_Access (Context));
         end if;

         if Module_Name (Get_Creator (Context)) = Explorer_Module_Name then
            Gtk_New (Item, Label => "");
            Append (Menu, Item);

            Gtk_New (Item, Label => -"Add Variable");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (On_Add_Variable'Access),
               Selection_Context_Access (Context));
         end if;

         if Has_File_Information (File_Context) then
            Gtk_New (Item, Label => "");
            Append (Menu, Item);

            Gtk_New (Item, Label => -"Edit Switches for "
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

   ----------------------------
   -- Viewer_Contextual_Menu --
   ----------------------------

   function Project_Editor_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      Context : File_Selection_Context_Access :=
        new File_Selection_Context;
      V : Project_Viewer := Project_Viewer (Object);
      Item : Gtk_Menu_Item;
      Row, Column : Gint;
      Is_Valid : Boolean;
      User : User_Data;

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
         Set_File_Name_Information
           (Context,
            Directory    => Get_String (User.Directory),
            File_Name    => Get_String (User.File_Name));
      end if;
      Set_File_Information (Context, Project_View => V.Project_Filter);

      if V.Project_Filter /= No_Project then
         Gtk_New (Item, -"Edit default switches");
         Add (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (Edit_Default_Switches'Access),
            Selection_Context_Access (Context));
      end if;

      if Has_File_Information (Context) then
         Gtk_New (Item, -"Edit switches for "
                  & File_Information (Context));
         Add (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (Edit_Switches'Access),
            Selection_Context_Access (Context));
      end if;

      return Selection_Context_Access (Context);
   end Project_Editor_Context_Factory;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
   begin
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
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Prj_Editor_Module_ID := Register_Module
        (Module_Name             => Project_Editor_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => Project_Editor_Contextual'Access);
   end Register_Module;

end Project_Viewers;
