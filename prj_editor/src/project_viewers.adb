
with Glib;            use Glib;
with Gtk.Notebook;    use Gtk.Notebook;
with Gtk.Clist;       use Gtk.Clist;
with Gtkada.Types;    use Gtkada.Types;
with Gtk.Label;       use Gtk.Label;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Arguments;   use Gtk.Arguments;
with Gdk.Event;       use Gdk.Event;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Calendar;             use GNAT.Calendar;
with Ada.Calendar;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Prj;          use Prj;
with Prj_API;      use Prj_API;
with Stringt;      use Stringt;
with Types;        use Types;
with Namet;        use Namet;

with Switches_Editors; use Switches_Editors;

package body Project_Viewers is

   Timestamp_Picture : constant Picture_String := "%Y/%m/%d %H:%M:%S";
   --  <preference>Format used to display timestamps

   Show_Default_Switches : constant Boolean := False;
   --  <preference>Whether we should display the switches for a file when
   --  they are in fact the default switches defined in the project file.

   type View_Display is access function
     (Viewer    : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Function used to return the contents of one of the columns.
   --  The returned string will be freed by the caller.

   type View_Callback is access procedure
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      File_Name : String_Id;
      Directory : String_Id);
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

      Tab_Title : String_Access;
      --  The label for the notebook page that contains the view

      Display : View_Display_Array (1 .. Num_Columns);
      --  The functions to display each of the columns. null can be provided
      --  if the columns doesn't contain any information.

      Callbacks : View_Callback_Array (1 .. Num_Columns);
      --  The callbacks to call when a column is clicked. If null, no callback
      --  is called.
   end record;
   type View_Description_Access is access constant View_Description;


   function Name_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the name of the file

   function Size_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the size of the file

   function Timestamp_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the timestamp for the file

   function Make_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the switches used for the gnatmake command

   function Compiler_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the switches used for the compiler

   function Binder_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the switches used for the binder

   function Linker_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr;
   --  Return the switches used for the linker

   procedure Edit_Switches_Callback
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      File_Name : String_Id;
      Directory : String_Id);
   --  Called every time the user wans to edit some specific switches

   View_System : aliased constant View_Description :=
     (Num_Columns => 3,
      Titles      => "File Name" + "Size" + "Last_Modified",
      Tab_Title   => new String' ("System"),
      Display     => (Name_Display'Access,
                      Size_Display'Access,
                      Timestamp_Display'Access),
      Callbacks   => (null, null, null));
   View_Version_Control : aliased constant View_Description :=
     (Num_Columns => 3,
      Titles      => "File Name" + "Revision" + "Head Revision",
      Tab_Title   => new String' ("VC"),
      Display     => (Name_Display'Access, null, null),
      Callbacks   => (null, null, null));
   View_Switches : aliased constant View_Description :=
     (Num_Columns => 5,
      Titles      => "File Name" + "Make" + "Compiler" + "Binder" + "Linker",
      Tab_Title   => new String' ("Switches"),
      Display     => (Name_Display'Access,
                      Make_Switches_Display'Access,
                      Compiler_Switches_Display'Access,
                      Binder_Switches_Display'Access,
                      Linker_Switches_Display'Access),
      Callbacks   => (null,
                      Edit_Switches_Callback'Access,
                      Edit_Switches_Callback'Access,
                      Edit_Switches_Callback'Access,
                      Edit_Switches_Callback'Access));

   Views : array (View_Type) of View_Description_Access :=
     (View_System'Access, View_Version_Control'Access, View_Switches'Access);

   type User_Data is record
      File_Name : String_Id;
      Directory : String_Id;
   end record;
   package Project_User_Data is new Row_Data (User_Data);

   function Current_Page (Viewer : access Project_Viewer_Record'Class)
      return View_Type;
   pragma Inline (Current_Page);
   --  Return the view associated with the current page of Viewer.

   procedure Append_Line
     (Viewer : access Project_Viewer_Record'Class;
      Project_View : Project_Id;
      File_Name : String_Id;
      Directory_Filter : Filter := No_Filter);
   --  Append a new line in the current page of Viewer, for File_Name.
   --  The exact contents inserted depends on the current view.
   --  The file is automatically searched in all the source directories of
   --  Project_View.

   function Append_Line_With_Full_Name
     (Viewer : access Project_Viewer_Record'Class;
      Current_View : View_Type;
      Project_View : Project_Id;
      File_Name : String;
      Directory_Name : String) return Gint;
   --  Same as above, except we have already found the proper location for
   --  the file.
   --  Return the number of the newly inserted row

   function Find_In_Source_Dirs
     (Project_View : Project_Id; File : String) return String_Id;
   --  Return the location of File in the source dirs of Project_View.
   --  null is returned if the file wasn't found.

   procedure Switch_Page
     (Viewer : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Callback when a new page is selected in Viewer.
   --  If the page is not up-to-date, we refresh its contents

   procedure Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Callback when a row/column has been selected in the clist

   function To_String (Value : Variable_Value) return String;
   --  Convert a variable value to a string suitable for insertion in the list.

   function To_Argument_List (Value : Variable_Value) return Argument_List;
   --  Convert a variable value to a list of arguments.

   -------------------------
   -- Find_In_Source_Dirs --
   -------------------------

   function Find_In_Source_Dirs
     (Project_View : Project_Id; File : String) return String_Id
   is
      Dirs : String_List_Id := Projects.Table (Project_View).Source_Dirs;
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

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Variable_Value) return String is
      Buffer : String (1 .. 1024);
      Current    : Prj.String_List_Id;
      The_String : String_Element;
      Index : Natural := Buffer'First;
   begin
      case Value.Kind is
         when Prj.Undefined =>
            return "";

         when Prj.Single =>
            String_To_Name_Buffer (Value.Value);
            return Name_Buffer (1 .. Name_Len);

         when Prj.List =>
            Current := Value.Values;
            while Current /= Prj.Nil_String loop
               The_String := String_Elements.Table (Current);
               String_To_Name_Buffer (The_String.Value);

               if Index /= Buffer'First then
                  Buffer (Index) := ' ';
                  Index := Index + 1;
               end if;

               Buffer (Index .. Index + Name_Len - 1) :=
                 Name_Buffer (1 .. Name_Len);
               Index := Index + Name_Len;

               Current := The_String.Next;
            end loop;
            return Buffer (Buffer'First .. Index - 1);
      end case;
   end To_String;

   ----------------------
   -- To_Argument_List --
   ----------------------

   function To_Argument_List (Value : Variable_Value) return Argument_List is
      S : Argument_List (1 .. Length (Value)) := (others => null);
      V : String_List_Id;
   begin
      case Value.Kind is
         when Undefined =>
            null;

         when Single =>
            String_To_Name_Buffer (Value.Value);
            S (1) := new String' (Name_Buffer (1 .. Name_Len));

         when List =>
            V := Value.Values;
            for J in S'Range loop
               String_To_Name_Buffer (String_Elements.Table (V).Value);
               S (J) := new String' (Name_Buffer (1 .. Name_Len));
               V := String_Elements.Table (V).Next;
            end loop;
      end case;
      return S;
   end To_Argument_List;

   ---------------------------
   -- Make_Switches_Display --
   ---------------------------

   function Make_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
   is
      File     : Name_Id;
      Value    : Variable_Value;
      Is_Default : Boolean;
   begin
      Name_Len := File_Name'Length;
      Name_Buffer (1 .. Name_Len) := File_Name;
      File := Name_Find;

      Get_Switches (Viewer.Project_View, "gnatmake", File, Value, Is_Default);
      if Show_Default_Switches or else not Is_Default then
         return New_String (To_String (Value));
      else
         return New_String ("");
      end if;
   end Make_Switches_Display;

   -------------------------------
   -- Compiler_Switches_Display --
   -------------------------------

   function Compiler_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
   is
      File     : Name_Id;
      Value    : Variable_Value;
      Is_Default : Boolean;
   begin
      Name_Len := File_Name'Length;
      Name_Buffer (1 .. Name_Len) := File_Name;
      File := Name_Find;

      Get_Switches (Viewer.Project_View, "compiler", File, Value, Is_Default);
      if Show_Default_Switches or else not Is_Default then
         return New_String (To_String (Value));
      else
         return New_String ("");
      end if;
   end Compiler_Switches_Display;

   -----------------------------
   -- Binder_Switches_Display --
   -----------------------------

   function Binder_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
   is
      File     : Name_Id;
      Value    : Variable_Value;
      Is_Default : Boolean;
   begin
      Name_Len := File_Name'Length;
      Name_Buffer (1 .. Name_Len) := File_Name;
      File := Name_Find;

      Get_Switches (Viewer.Project_View, "gnatbind", File, Value, Is_Default);
      if Show_Default_Switches or else not Is_Default then
         return New_String (To_String (Value));
      else
         return New_String ("");
      end if;
   end Binder_Switches_Display;

   -----------------------------
   -- Linker_Switches_Display --
   -----------------------------

   function Linker_Switches_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
   is
      File     : Name_Id;
      Value    : Variable_Value;
      Is_Default : Boolean;
   begin
      Name_Len := File_Name'Length;
      Name_Buffer (1 .. Name_Len) := File_Name;
      File := Name_Find;

      Get_Switches (Viewer.Project_View, "gnatlink", File, Value, Is_Default);
      if Show_Default_Switches or else not Is_Default then
         return New_String (To_String (Value));
      else
         return New_String ("");
      end if;
   end Linker_Switches_Display;

   ------------------
   -- Name_Display --
   ------------------

   function Name_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
   is
      pragma Warnings (Off, Viewer);
      pragma Warnings (Off, Directory);
      pragma Warnings (Off, Fd);
   begin
      return New_String (File_Name);
   end Name_Display;

   ------------------
   -- Size_Display --
   ------------------

   function Size_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
   is
      pragma Warnings (Off, Viewer);
      pragma Warnings (Off, Directory);
      pragma Warnings (Off, File_Name);
   begin
      return New_String (Long_Integer'Image (File_Length (Fd)));
   end Size_Display;

   -----------------------
   -- Timestamp_Display --
   -----------------------

   function Timestamp_Display
     (Viewer : access Project_Viewer_Record'Class;
      File_Name : String;
      Directory : String;
      Fd        : File_Descriptor) return Interfaces.C.Strings.chars_ptr
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
      T : tm;
      A_Time : Ada.Calendar.Time;
      O_Time : OS_Time;
   begin
      O_Time := File_Time_Stamp (Fd);
      localtime_r (O_Time, T);
      A_Time := Time_Of (1900 + T.tm_year, T.tm_mon, T.tm_mday,
                         T.tm_hour, T.tm_min, T.tm_sec);
      return New_String (Image (A_Time, Timestamp_Picture));
   end Timestamp_Display;

   ----------------------------
   -- Edit_Switches_Callback --
   ----------------------------

   procedure Edit_Switches_Callback
     (Viewer    : access Project_Viewer_Record'Class;
      Column    : Gint;
      File_Name : String_Id;
      Directory : String_Id)
   is
      procedure Set (Tool : Tool_Names; Pkg : String);
      --  Set the switches for one of the tools

      File : Name_Id;
      Tool : Tool_Names;
      Switches : Switches_Edit;

      ---------
      -- Set --
      ---------

      procedure Set (Tool : Tool_Names; Pkg : String) is
         Value : Variable_Value;
         Is_Default : Boolean;
      begin
         Get_Switches (Viewer.Project_View, Pkg, File, Value, Is_Default);
         declare
            List : Argument_List := To_Argument_List (Value);
         begin
            Set_Switches (Switches, Tool, List);
            Free (List);
         end;
      end Set;

   begin
      String_To_Name_Buffer (File_Name);
      File := Name_Find;

      case Column is
         when 1      => Tool := Gnatmake;
         when 2      => Tool := Compiler;
         when 3      => Tool := Binder;
         when 4      => Tool := Linker;
         when others => return;
      end case;

      Gtk_New (Switches);
      String_To_Name_Buffer (File_Name);
      Set_Title
        (Switches, "Editing switches for " & Name_Buffer (1 .. Name_Len));

      --  Set the switches for all the pages
      Set (Gnatmake, "gnatmake");
      Set (Compiler, "compiler");
      Set (Binder,   "gnatbind");
      Set (Linker,   "gnatlink");

      Show_All (Switches);
      Set_Page (Switches, Tool);
   end Edit_Switches_Callback;

   --------------------------------
   -- Append_Line_With_Full_Name --
   --------------------------------

   function Append_Line_With_Full_Name
     (Viewer : access Project_Viewer_Record'Class;
      Current_View : View_Type;
      Project_View : Project_Id;
      File_Name : String;
      Directory_Name : String) return Gint
   is
      Line : Gtkada.Types.Chars_Ptr_Array
        (1 .. Views (Current_View).Num_Columns);
      Row : Gint;
      File_Desc : File_Descriptor;
   begin
      if Is_Absolute_Path (Directory_Name) then
         File_Desc := Open_Read (Directory_Name & Directory_Separator
                                 & File_Name & ASCII.Nul, Text);
      else
         File_Desc := Open_Read
           (Get_Current_Dir & Directory_Name & Directory_Separator
            & File_Name & ASCII.Nul, Text);
      end if;

      for Column in 1 .. Views (Current_View).Num_Columns loop
         if Views (Current_View).Display (Column) /= null then
            Line (Column) := Views (Current_View).Display (Column)
              (Viewer, File_Name, Directory_Name, File_Desc);
         else
            Line (Column) := New_String ("");
         end if;
      end loop;

      Close (File_Desc);

      Row := Append (Viewer.Pages (Current_View), Line);
      Free (Line);
      return Row;
   end Append_Line_With_Full_Name;

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Viewer : access Project_Viewer_Record'Class;
      Project_View : Project_Id;
      File_Name : String_Id;
      Directory_Filter : Filter := No_Filter)
   is
      Current_View : constant View_Type := Current_Page (Viewer);
      File_N : String (1 .. Integer (String_Length (File_Name)));
      Dir_Name : String_Id;
   begin
      String_To_Name_Buffer (File_Name);
      File_N := Name_Buffer (1 .. Name_Len);
      Dir_Name := Find_In_Source_Dirs (Project_View, File_N);
      pragma Assert (Dir_Name /= No_String);

      String_To_Name_Buffer (Dir_Name);
      declare
         Dir : constant String := Name_Buffer (1 .. Name_Len);
      begin
         if Directory_Filter /= No_Filter
           and then Dir /= Get_Name_String (Directory_Filter)
         then
            return;
         end if;

         Project_User_Data.Set
           (Viewer.Pages (Current_View),
            Append_Line_With_Full_Name
               (Viewer, Current_View, Project_View, File_N, Dir),
            (File_Name => File_Name, Directory => Dir_Name));
      end;
   end Append_Line;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page
     (Viewer : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      V : Project_Viewer := Project_Viewer (Viewer);
      use type Row_List.Glist;
      Page_Num : constant Guint := To_Guint (Args, 2);
      Current_View : View_Type := View_Type'Val (Page_Num);
      Up_To_Date : View_Type;
      List : Row_List.Glist;
      User : User_Data;
   begin
      --  No current page (happens only while V is not realized)
      if Get_Current_Page (V) = -1 then
         return;
      end if;

      --  Nothing to do if the page is already up-to-date
      if V.Page_Is_Up_To_Date (Current_View) then
         return;
      end if;

      --  Otherwise, we update the list of files based on the contents of
      --  one of the up-to-date pages
      Up_To_Date := View_Type'First;
      loop
         exit when V.Page_Is_Up_To_Date (Up_To_Date);

         --  We haven't found an up-to-date page. This can happen for instance
         --  when Viewer is empty and has never been associated with a
         --  directory before.
         if Up_To_Date = View_Type'Last then
            return;
         end if;
         Up_To_Date := View_Type'Succ (Up_To_Date);
      end loop;

      Freeze (V.Pages (Current_View));
      Clear (V.Pages (Current_View));

      List := Get_Row_List (V.Pages (Up_To_Date));
      while List /= Row_List.Null_List loop
         User := Project_User_Data.Get
           (V.Pages (Up_To_Date), Row_List.Get_Data (List));

         declare
            N : String (1 .. Integer (String_Length (User.File_Name)));
            D : String (1 .. Integer (String_Length (User.Directory)));
         begin
            String_To_Name_Buffer (User.File_Name);
            N := Name_Buffer (1 .. Name_Len);

            String_To_Name_Buffer (User.Directory);
            D := Name_Buffer (1 .. Name_Len);

            Project_User_Data.Set
              (V.Pages (Current_View),
               Append_Line_With_Full_Name
                  (V, Current_View, V.Project_View, N, D),
               User);
         end;

         List := Row_List.Next (List);
      end loop;

      Thaw (V.Pages (Current_View));
      V.Page_Is_Up_To_Date (Current_View) := True;
   end Switch_Page;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row
     (Viewer : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      V      : Project_Viewer := Project_Viewer (Viewer);
      Current_View : constant View_Type := Current_Page (V);
      Row    : Gint := To_Gint (Args, 1);
      Column : Gint := To_Gint (Args, 2);
      Event  : Gdk_Event := To_Event (Args, 3);
      User   : User_Data;
      Callback : View_Callback;

   begin
      Callback := Views (Current_View).Callbacks
        (Interfaces.C.size_t (Column + 1));
      if Get_Event_Type (Event) = Gdk_2button_Press
        and then Callback /= null
      then
         User := Project_User_Data.Get (V.Pages (Current_View), Row);
         Callback (V, Column, User.File_Name, User.Directory);
      end if;
   end Select_Row;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Viewer : out Project_Viewer) is
   begin
      Viewer := new Project_Viewer_Record;
      Project_Viewers.Initialize (Viewer);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Viewer : access Project_Viewer_Record'Class) is
      Label : Gtk_Label;
   begin
      Gtk.Notebook.Initialize (Viewer);

      for View in View_Type'Range loop
         Gtk_New (Viewer.Pages (View),
                  Columns => Gint (Views (View).Num_Columns),
                  Titles  => Views (View).Titles);
         Set_Column_Auto_Resize (Viewer.Pages (View), 0, True);
         Gtk_New (Label, Views (View).Tab_Title.all);

         Widget_Callback.Object_Connect
           (Viewer.Pages (View), "select_row",  Select_Row'Access, Viewer);

         Append_Page (Viewer, Viewer.Pages (View), Label);
      end loop;

      Widget_Callback.Connect (Viewer, "switch_page", Switch_Page'Access);

      Show_All (Viewer);
   end Initialize;

   ------------------
   -- Current_Page --
   ------------------

   function Current_Page (Viewer : access Project_Viewer_Record'Class)
      return View_Type is
   begin
      return View_Type'Val (Get_Current_Page (Viewer));
   end Current_Page;

   ------------------
   -- Show_Project --
   ------------------

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record;
      Project_View     : Prj.Project_Id;
      Directory_Filter : Filter := No_Filter)
   is
      Src : String_List_Id := Projects.Table (Project_View).Sources;
      Current_View : constant View_Type := Current_Page (Viewer);

   begin
      Viewer.Page_Is_Up_To_Date := (others => False);
      Viewer.Page_Is_Up_To_Date (Current_View) := True;
      Viewer.Project_View := Project_View;

      Freeze (Viewer.Pages (Current_View));

      while Src /= Nil_String loop
         Append_Line (Viewer, Project_View,
                      String_Elements.Table (Src).Value,
                      Directory_Filter);
         Src := String_Elements.Table (Src).Next;
      end loop;

      Thaw (Viewer.Pages (Current_View));
   end Show_Project;

   -----------
   -- Clear --
   -----------

   procedure Clear (Viewer : access Project_Viewer_Record) is
      Current_View : constant View_Type := Current_Page (Viewer);
   begin
      Viewer.Page_Is_Up_To_Date := (others => False);
      Viewer.Page_Is_Up_To_Date (Current_View) := True;

      Freeze (Viewer.Pages (Current_View));
      Clear (Viewer.Pages (Current_View));
      Thaw (Viewer.Pages (Current_View));
   end Clear;

end Project_Viewers;
