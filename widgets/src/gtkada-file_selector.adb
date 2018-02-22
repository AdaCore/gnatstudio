------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Regexp;               use GNAT.Regexp;
with GNAT.Strings;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;   use Gtk.File_Chooser_Dialog;
with Gtk.File_Filter;           use Gtk.File_Filter;
with Interfaces.C.Strings;
with System;

with Gdk;                       use Gdk;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;
with Glib.Unicode;              use Glib.Unicode;

with Gtk;                       use Gtk;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Intl;               use Gtkada.Intl;

with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Remote;                    use Remote;
with Gexpect.Db;                use Gexpect, Gexpect.Db;
with GNATCOLL.Traces;                    use GNATCOLL.Traces;
with Unchecked_Deallocation;

package body Gtkada.File_Selector is

   Me : constant Trace_Handle := Create ("GPS.OTHERS.GTKADA_FILE_SELECTOR");

   Use_Gtk_Selector : constant Trace_Handle :=
     Create ("GPS.INTERNAL.GTK_FILE_SELECTOR", GNATCOLL.Traces.On);
   --  Use Gtk_File_Chooser_Dialog from Gtk instead Dialog from GPS

   Directories_Hist_Key : constant Histories.History_Key := "directories";
   --  Key used in the history

   Base_Name_Column  : constant := 0;
   Comment_Column    : constant := 1;
   Text_Color_Column : constant := 2;
   Icon_Column       : constant := 3;
   File_Column       : constant := 4;

   Last_Directory : Virtual_File := GNATCOLL.VFS.No_File;
   Last_Remote_Directory : Virtual_File := GNATCOLL.VFS.Get_Current_Dir;
   --  It would be nice to use a user data instead of this global variable,
   --  but this is in any case better than changing the current directory
   --  as we did before.

   --------------------------
   -- Extern C subprograms --
   --------------------------

   function NativeFileSelectionSupported return Integer;
   pragma Import
     (C, NativeFileSelectionSupported, "NativeFileSelectionSupported");

   function NativeFileSelection
     (Title       : String;
      Basedir     : String;
      Filepattern : String;
      Patternname : String;
      Defaultname : String;
      Style       : Integer;
      Kind        : Integer) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, NativeFileSelection, "NativeFileSelection");

   function NativeDirSelection
     (Title   : String;
      Basedir : String) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, NativeDirSelection, "NativeDirSelection");

   procedure c_free (S : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, c_free, "free");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Basename_Less_Than (F1, F2 : Virtual_File) return Boolean
     is (F1.Base_Name < F2.Base_Name);
   package By_Basename is new File_List.Generic_Sorting (Basename_Less_Than);

   procedure Set_Location
     (Location_Combo : Gtk_Combo_Box_Text;
      Dir            : Virtual_File);
   --  Sets the location in the combo

   function Get_Location
     (Location_Combo : Gtk_Combo_Box_Text) return Virtual_File;
   --  Gets the location from the combo

   function Columns_Types return GType_Array;
   --  Return the types of the columns in the list

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view

   procedure Change_Directory
     (Win : access File_Selector_Window_Record'Class;
      Dir : GNATCOLL.VFS.Virtual_File);
   --  Called every time that the contents of a new directory should be
   --  displayed in the File_Explorer. Dir is the absolute pathname to
   --  that directory.

   procedure Refresh_Files
     (Win : access File_Selector_Window_Record'Class);

   function Display_File
     (Win : File_Selector_Window_Access) return Boolean;
   --  This function gets one entry from Win.Remaining_Files, applies
   --  a filter to it, and displays the corresponding information in the
   --  file list.

   type Regexp_Filter_Record is new File_Filter_Record with record
      Pattern : Regexp;
   end record;

   type Regexp_Filter is access all Regexp_Filter_Record'Class;

   function Regexp_File_Filter
     (Pattern : String;
      Name    : String) return Regexp_Filter;
   --  Return a new filter that only shows files matching pattern.
   --  New memory is allocated, that will be freed automatically by the file
   --  selector where the filter is registered.
   --  If Name is not null, use it instead of Pattern for the name of the
   --  filter.

   overriding procedure Use_File_Filter
     (Filter : access Regexp_Filter_Record;
      Win    : access File_Selector_Window_Record'Class;
      File   : Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.Strings.String_Access);
   --  See spec for more details on this dispatching routine

   procedure Set_Busy (W : Gtk_Window; Busy : Boolean);
   --  Set/Reset the busy cursor on the specified window

   function Get_Selected (Combo : Gtk_Combo_Box_Text) return String;

   function Regexp_File_Filter
     (Filter_Info : Gtk.File_Filter.Gtk_File_Filter_Info;
      Exp         : Regexp)
      return Boolean;
   --  Validate whether file match expression

   package File_Filter_Regexp is
     new Gtk.File_Filter.Add_Custom_User_Data (Regexp);
   --  Set Regexp as User_Data in to GtkFileFilter

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Back_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Forward_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure Host_Selected (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure Directory_Selected (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure Filter_Selected (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Location_Combo_Entry_Activate
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Explorer_Tree_Select_Row
     (Object : access GObject_Record'Class;
      Params : Gtk_Args);
   --  ???

   procedure On_File_List_End_Selection
     (Object : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  ???

   procedure On_Destroy
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   function On_File_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  ???

   function On_Selection_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  ???

   function On_Location_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  ???

   procedure On_Display_Idle_Destroy
     (Win : in out File_Selector_Window_Access);
   --  Callback used to destroy the display idle loop

   procedure Name_Selected (File : access Gtk_Widget_Record'Class);
   --  Called when a new file has been selected

   function Get_Selected (Combo : Gtk_Combo_Box_Text) return String is
      Iter : constant Gtk_Tree_Iter := Combo.Get_Active_Iter;
   begin
      if Iter = Null_Iter then
         return "";
      else
         return Get_String (Combo.Get_Model, Iter, 0);
      end if;
   end Get_Selected;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (Location_Combo : Gtk_Combo_Box_Text;
      Dir            : Virtual_File) is
   begin
      if Is_Local (Dir) then
         Add_Unique_Combo_Entry (Location_Combo,
                                 Display_Full_Name (Dir, True),
                                 True);
      else
         Add_Unique_Combo_Entry
           (Location_Combo,
            Get_Host (Dir) & ":|" & Display_Full_Name (Dir, True),
            Select_Text => True);
      end if;
   end Set_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Location_Combo : Gtk_Combo_Box_Text) return Virtual_File
   is
      Str : constant String := Get_Active_Text (Location_Combo);

   begin
      for J in Str'First .. Str'Last - 1 loop
         if Str (J .. J + 1) = ":|" then
            return Create_From_UTF8
              (Str (J + 2 .. Str'Last),
               Host => Str (Str'First .. J - 1));
         end if;
      end loop;

      return Create_From_UTF8 (Str, Normalize => True);
   end Get_Location;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy (W : Gtk_Window; Busy : Boolean) is
   begin
      if W /= null then
         Set_Busy_Cursor (Get_Window (W), Busy, Busy);
      end if;
   end Set_Busy;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Text_Color_Column => Gdk.RGBA.Get_Type,
         Base_Name_Column  => GType_String,
         Comment_Column    => GType_String,
         Icon_Column       => Gdk.Pixbuf.Get_Type,
         File_Column       => Get_Virtual_File_Type);
   end Columns_Types;

   ------------------------
   -- Regexp_File_Filter --
   ------------------------

   function Regexp_File_Filter
     (Pattern : String;
      Name    : String) return Regexp_Filter
   is
      Filter : constant Regexp_Filter := new Regexp_Filter_Record;
   begin
      if Name = "" then
         --  Removes the { } in the pattern if any

         if Pattern (Pattern'First) = '{'
           and then Pattern (Pattern'Last) = '}'
         then
            Filter.Label :=
              new String'(Pattern (Pattern'First + 1 .. Pattern'Last - 1));
         else
            Filter.Label := new String'(Pattern);
         end if;

      else
         Filter.Label := new String'(Name);
      end if;

      Filter.Pattern :=
        Compile
          (Pattern        => Pattern,
           Glob           => True,
           Case_Sensitive => Local_Host_Is_Case_Sensitive);
      --  ??? At this place, we don't know what's the selected server. If we
      --  would, we could use the server's case sensitivity instead of the
      --  local one.
      return Filter;
   end Regexp_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   overriding procedure Use_File_Filter
     (Filter : access Regexp_Filter_Record;
      Win    : access File_Selector_Window_Record'Class;
      File   : Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (Win);
   begin
      Text   := null;
      Pixbuf := null;

      if Match (+Base_Name (File), Filter.Pattern) then
         State := Normal;
      else
         State := Invisible;
      end if;
   end Use_File_Filter;

   -----------------------------
   -- On_Display_Idle_Destroy --
   -----------------------------

   procedure On_Display_Idle_Destroy
     (Win : in out File_Selector_Window_Access)
   is
      pragma Warnings (Off, Win);
   begin
      Win.Display_Idle_Handler := 0;
   end On_Display_Idle_Destroy;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Dialog : access File_Selector_Window_Record) return Virtual_File is
   begin
      if Dialog.Selection_Entry = null
        or else Get_Text (Dialog.Selection_Entry) = ""
      then
         return GNATCOLL.VFS.No_File;

      else
         declare
            Filename : constant String :=
                         Get_Text (Dialog.Selection_Entry);
            File     : Virtual_File;

         begin
            File := Create_From_UTF8
              (Filename, Get_Host (Dialog.Current_Directory));

            if Is_Absolute_Path (File) then
               return File;
            else
               return Create_From_Base
                 (File.Full_Name (Normalize => True),
                  Dialog.Current_Directory.Full_Name (Normalize => True),
                  Host => Get_Host (Dialog.Current_Directory));
            end if;
         end;
      end if;
   end Get_Selection;

   -----------------
   -- Select_File --
   -----------------

   function Select_File
     (Title             : String  := "Select a file";
      Base_Directory    : Virtual_File := No_File;
      File_Pattern      : Filesystem_String  := "";
      Pattern_Name      : String  := "";
      Default_Name      : Filesystem_String  := "";
      Parent            : Gtk_Window := null;
      Remote_Browsing   : Boolean := False;
      Use_Native_Dialog : Boolean := False;
      Kind              : File_Selector_Kind := Unspecified;
      History           : Histories.History := null) return Virtual_File
   is
      Pos_Mouse     : constant := 2;
      File_Selector : File_Selector_Window_Access;
      S             : Interfaces.C.Strings.chars_ptr;
      Working_Dir   : Virtual_File;
      Initial_Dir   : Virtual_File;
      Default_File  : Virtual_File;
      Dialog        : Gtk_File_Chooser_Dialog;

      type Pattern_Callback is
        access procedure (Pattern : String; Name : String);

      procedure Parse_Pattern (Callback : Pattern_Callback);
      --  Parse File_Pattern and Pattern_Name, separates them on parts
      --  and call Callback for each part

      procedure Add_File_Selector_Filter (Pattern : String; Name : String);
      --  Add filter to File_Selector

      procedure Add_File_Chooser_Filter (Pattern : String; Name : String);
      --  Add filter to File_Chooser

      function Make_Result (Name : String) return Virtual_File;
      --  Create Virtual_File for file represented by Name and make
      --  store/restore actions before exit

      -----------------------------
      -- Add_File_Chooser_Filter --
      -----------------------------

      procedure Add_File_Chooser_Filter (Pattern : String; Name : String)
      is
         Filter : Gtk.File_Filter.Gtk_File_Filter;
      begin
         Gtk.File_Filter.Gtk_New (Filter);

         if Name = "" then
            if Pattern (Pattern'First) = '{'
              and then Pattern (Pattern'Last) = '}'
            then
               Filter.Set_Name
                 (Pattern (Pattern'First + 1 .. Pattern'Last - 1));
            else
               Filter.Set_Name (Pattern);
            end if;
         else
            Filter.Set_Name (Name);
         end if;

         if Pattern (Pattern'First) = '{' then
            File_Filter_Regexp.Add_Custom
              (Filter, File_Filter_Filename, Regexp_File_Filter'Access,
               (Compile (Pattern        => Pattern,
                         Glob           => True,
                         Case_Sensitive => Local_Host_Is_Case_Sensitive)));

         else
            Filter.Add_Pattern (Pattern);
         end if;

         Dialog.Add_Filter (Filter);
      end Add_File_Chooser_Filter;

      ------------------------------
      -- Add_File_Selector_Filter --
      ------------------------------

      procedure Add_File_Selector_Filter (Pattern : String; Name : String) is
      begin
         Register_Filter (File_Selector, Regexp_File_Filter (Pattern, Name));
      end Add_File_Selector_Filter;

      -----------------
      -- Make_Result --
      -----------------

      function Make_Result (Name : String) return Virtual_File
      is
         F : Virtual_File;
      begin
         --  Change back to working directory
         Change_Dir (Working_Dir);

         if Name = "" then
            return GNATCOLL.VFS.No_File;
         else
            F := Create (+Name);
            Last_Directory := Dir (F);

            return F;
         end if;
      end Make_Result;

      -------------------
      -- Parse_Pattern --
      -------------------

      procedure Parse_Pattern (Callback : Pattern_Callback)
      is
         Fl     : Natural := File_Pattern'First; -- Last indexes
         Nl     : Natural := Pattern_Name'First;
         Ff, Nf : Natural;                       -- First indexes
         Fo, No : Natural := 0;                  -- Indexes offset

      begin
         while Fl < File_Pattern'Last loop
            Ff := Fl;
            Nf := Nl;

            while Fl < File_Pattern'Last
              and then File_Pattern (Fl) /= ';'
            loop
               Fl := Fl + 1;
            end loop;

            while Nl < Pattern_Name'Last
              and then Pattern_Name (Nl) /= ';'
            loop
               Nl := Nl + 1;
            end loop;

            if File_Pattern (Fl) = ';' then
               Fo := 1;
            else
               Fo := 0;
            end if;

            if Pattern_Name (Nl) = ';' then
               No := 1;
            else
               No := 0;
            end if;

            if Nf > Pattern_Name'Last then
               Callback (+File_Pattern (Ff .. Fl - Fo), "");
            else
               Callback
                 (+File_Pattern (Ff .. Fl - Fo),
                  Pattern_Name (Nf .. Nl - No));
            end if;

            Fl := Fl + 1;
            Nl := Nl + 1;
         end loop;
      end Parse_Pattern;

   begin
      if not Remote_Browsing
        and then Is_Local (Base_Directory)
      then
         --  Save working directory
         Working_Dir := Get_Current_Dir;

         if Base_Directory = No_File then
            if Last_Directory = No_File then
               Last_Directory := Working_Dir;
            end if;
            Initial_Dir := Last_Directory;

         else
            Initial_Dir := Base_Directory;
         end if;

         if Use_Native_Dialog
           and then NativeFileSelectionSupported /= 0
         then
            if Kind = Open_Directory then
               S := NativeDirSelection
                 (Title & ASCII.NUL,
                  +Full_Name (Initial_Dir).all & ASCII.NUL);
            else
               S := NativeFileSelection
                 (Title & ASCII.NUL,
                  +Full_Name (Initial_Dir).all & ASCII.NUL,
                  +File_Pattern & ASCII.NUL,
                  Pattern_Name & ASCII.NUL,
                  +Default_Name & ASCII.NUL,
                  Pos_Mouse,
                  File_Selector_Kind'Pos (Kind));
            end if;

            declare
               Val : constant String := Interfaces.C.Strings.Value (S);
            begin
               c_free (S);
               return Make_Result (Val);
            end;

         elsif Kind /= Unspecified
           and then Active (Use_Gtk_Selector)
         then
            declare
               Ignore : Boolean;
               Button : Gtk.Widget.Gtk_Widget;

               To_Action : constant array (Open_File .. Open_Directory) of
                 Gtk.File_Chooser.Gtk_File_Chooser_Action :=
                   (Open_File      => Gtk.File_Chooser.Action_Open,
                    Save_File      => Gtk.File_Chooser.Action_Save,
                    Open_Directory => Gtk.File_Chooser.Action_Select_Folder);
            begin
               Dialog := Gtk_File_Chooser_Dialog_New
                 (Title  => Title,
                  Parent => Parent,
                  Action => To_Action (Kind));

               Button := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
               Button.Set_Name ("gtk_file_chooser_dialog.ok_button");

               Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);

               Ignore := Dialog.Set_Current_Folder (+Full_Name (Initial_Dir));

               if Kind = Open_File then
                  Dialog.Set_Create_Folders (False);

               else
                  Dialog.Set_Do_Overwrite_Confirmation (True);
                  if Default_Name /= "" then
                     Dialog.Set_Current_Name (+Default_Name);
                  end if;
               end if;

               if File_Pattern /= "" then
                  Parse_Pattern (Add_File_Chooser_Filter'Access);
               end if;

               return F : constant Virtual_File := Make_Result
                 (if Dialog.Run = Gtk_Response_OK
                  then Dialog.Get_Filename
                  else "")
               do
                  Dialog.Destroy;
               end return;
            end;
         end if;
      end if;

      Set_Busy (Parent, True);

      if Remote_Browsing then
         Initial_Dir := Last_Remote_Directory;
      else
         if Last_Directory = No_File then
            Last_Directory := Get_Current_Dir;
         end if;
         Initial_Dir := Last_Directory;
      end if;

      if Base_Directory = No_File then
         Gtk_New
           (File_Selector_Window => File_Selector,
            Root                 => Get_Root (Initial_Dir),
            Initial_Directory    => Initial_Dir,
            Dialog_Title         => Title,
            Show_Files           => Kind /= Open_Directory,
            History              => History,
            Remote_Browsing      => Remote_Browsing);
      else
         Gtk_New
           (File_Selector_Window => File_Selector,
            Root                 => Get_Root (Base_Directory),
            Initial_Directory    => Base_Directory,
            Dialog_Title         => Title,
            Show_Files           => Kind /= Open_Directory,
            History              => History,
            Remote_Browsing      => Remote_Browsing);
      end if;

      Set_Position (File_Selector, Win_Pos_Mouse);

      if Default_Name /= "" then
         Default_File := Create_From_Dir (Initial_Dir, Default_Name);
         Set_Text
           (File_Selector.Selection_Entry, Display_Base_Name (Default_File));
      end if;

      if File_Pattern /= "" then
         Parse_Pattern (Add_File_Selector_Filter'Access);
      end if;

      Set_Busy (Parent, False);

      if Kind = Open_Directory then
         return Select_Directory (File_Selector, Parent);
      else
         return Select_File (File_Selector, Parent);
      end if;
   end Select_File;

   -----------------
   -- Select_File --
   -----------------

   function Select_File
     (File_Selector : File_Selector_Window_Access;
      Parent        : Gtk_Window := null) return GNATCOLL.VFS.Virtual_File
   is
      Filter_A : constant Filter_Show_All_Access := new Filter_Show_All;
      Resp     : Gtk_Response_Type;
      Ret      : Virtual_File;

   begin
      pragma Assert (File_Selector /= null);

      Filter_A.Label := new String'(-"All files");

      Register_Filter (File_Selector, Filter_A);
      Set_Modal (File_Selector, True);

      if Parent /= null then
         Set_Transient_For (File_Selector, Parent);
      end if;

      Show_All (File_Selector);

      Resp := File_Selector.Run;

      if Resp = Gtk_Response_OK then
         Ret := Get_Selection (File_Selector);
      else
         Ret := No_File;
      end if;

      Destroy (File_Selector);

      return Ret;
   end Select_File;

   ----------------------
   -- Select_Directory --
   ----------------------

   function Select_Directory
     (Title             : String := "Select a directory";
      Base_Directory    : Virtual_File := No_File;
      Parent            : Gtk_Window := null;
      Use_Native_Dialog : Boolean := False;
      History           : Histories.History := null) return Virtual_File
   is
     (Select_File
        (Title             => Title,
         Base_Directory    => Base_Directory,
         Parent            => Parent,
         Remote_Browsing   => not Is_Local (Base_Directory),
         Use_Native_Dialog => Use_Native_Dialog,
         Kind              => Open_Directory,
         History           => History));

   ----------------------
   -- Select_Directory --
   ----------------------

   function Select_Directory
     (File_Selector : File_Selector_Window_Access;
      Parent        : Gtk_Window := null) return Virtual_File
   is
      Filter_A      : constant Filter_Show_All_Access := new Filter_Show_All;
      Resp          : Gtk_Response_Type;
      Ret           : Virtual_File;

   begin
      pragma Assert (File_Selector /= null);

      Filter_A.Label := new String'(-"All files");

      Register_Filter (File_Selector, Filter_A);
      Set_Modal (File_Selector, True);
      Set_Transient_For (File_Selector, Parent);

      Show_All (File_Selector);
      Resp := File_Selector.Run;

      if Resp = Gtk_Response_OK then
         Ret := Get_Selection (File_Selector);
      else
         Ret := No_File;
      end if;

      Destroy (File_Selector);

      return Ret;
   end Select_Directory;

   ------------------
   -- Display_File --
   ------------------

   function Display_File (Win : File_Selector_Window_Access) return Boolean is
      Max_Idle_Duration : constant Duration := 0.05;
      --  Maximum time spent in the idle callback (in seconds)

      Text   : String_Access;
      State  : File_State;
      Pixbuf : Gdk_Pixbuf;
      Iter   : Gtk_Tree_Iter := Null_Iter;
      Color  : Gdk_RGBA := Null_RGBA;
      Start  : constant Time := Clock;

      procedure Internal
        (Tree, Iter : System.Address;
         Col        : Gint;
         Value      : Gdk_RGBA);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

      Has_Info : Boolean := False;
      F        : Virtual_File;

      Values  : Glib.Values.GValue_Array (1 .. 4);
      Columns : Columns_Array (Values'Range);
      Last    : Gint;

   begin
      if Win.Current_Directory = No_File then
         return False;
      end if;

      while Has_Element (Win.Remaining_Files)
        and then (Clock - Start <= Max_Idle_Duration)
      loop
         F := Element (Win.Remaining_Files);

         Use_File_Filter
           (Win.Current_Filter,
            Win,
            F,
            State,
            Pixbuf,
            Text);

         if State /= Invisible
           and then Text = null
           and then F.Is_Local
         then
            Text := new String'
              (GNAT.Calendar.Time_IO.Image
               (F.File_Time_Stamp,
                GNAT.Calendar.Time_IO.ISO_Date
                & " %H:%M:%S"));
         end if;

         --  ??? The selectable state should be set here, if possible

         Iter := Null_Iter;
         Color := Null_RGBA;

         case State is
            when Invisible =>
               Free (Text);

            when Normal =>
               Win.File_Model.Append (Iter, Null_Iter);

            when Highlighted =>
               Win.File_Model.Append (Iter, Null_Iter);
               Color := Win.Highlighted_Color;

            when Insensitive =>
               Win.File_Model.Append (Iter, Null_Iter);
               Color := Win.Insensitive_Color;
         end case;

         if Iter /= Null_Iter then
            Columns (1 .. 2) := (Base_Name_Column, File_Column);
            Values  (1 .. 2) := (As_String (F.Display_Base_Name), As_File (F));
            Last := 2;

            if Text /= null then
               Last := Last + 1;
               Columns (Last) := Comment_Column;
               Values  (Last) := As_String (Locale_To_UTF8 (Text.all));

               Free (Text);
               Has_Info := True;
            end if;

            if Pixbuf /= Null_Pixbuf then
               Last := Last + 1;
               Columns (Last) := Icon_Column;
               Values  (Last) := As_Object (GObject (Pixbuf));
            end if;

            Set_And_Clear
              (Win.File_Model, Iter, Columns (1 .. Last), Values (1 .. Last));

            if Color /= Null_RGBA then
               Internal
                 (Get_Object (Win.File_Model), Iter'Address,
                  Text_Color_Column, Color);
            end if;
         end if;

         Win.Remaining_Files := Next (Win.Remaining_Files);
      end loop;

      --  If there is nothing in the info column, we just hide it (to do so, we
      --  simply hide all column headers)
      Win.File_Tree.Set_Headers_Visible (Has_Info);

      return Has_Element (Win.Remaining_Files);
   end Display_File;

   ------------------------
   -- Regexp_File_Filter --
   ------------------------

   function Regexp_File_Filter
     (Filter_Info : Gtk.File_Filter.Gtk_File_Filter_Info;
      Exp         : Regexp)
      return Boolean is
   begin
      return Match
        (Interfaces.C.Strings.Value (Filter_Info.Display_Name), Exp);
   end Regexp_File_Filter;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Win    : access File_Selector_Window_Record;
      Filter : access File_Filter_Record'Class) is
   begin
      Append (Win.Filters, File_Filter (Filter));
      Add_Unique_Combo_Entry (Win.Filter_Combo, Filter.Label.all);
      if Get_Active (Win.Filter_Combo) = -1 then
         Set_Active (Win.Filter_Combo, 0);
      end if;
      Refresh_Files (Win);
   end Register_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   overriding procedure Use_File_Filter
     (Filter : access Filter_Show_All;
      Win    : access File_Selector_Window_Record'Class;
      File   : Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out String_Access)
   is
      pragma Unreferenced (File, Win, Filter);
   begin
      State  := Normal;
      Pixbuf := null;
      Text   := null;
   end Use_File_Filter;

   -------------------
   -- Refresh_Files --
   -------------------

   procedure Refresh_Files (Win : access File_Selector_Window_Record'Class) is
      Filter   : File_Filter := null;
      Iter     : Gtk_Tree_Iter;
      Files    : File_Array_Access;
      Alloc    : Gtk_Allocation;

   begin
      if Win.Current_Directory = No_File
        or else not Is_Directory (Win.Current_Directory)
        or else Win.File_Tree = null
      then
         return;
      end if;

      Win.File_Model.Clear;
      Win.Files.Clear;
      Win.Remaining_Files := No_Element;

      --  Find out which filter to use

      declare
         S : constant String :=
               Locale_From_UTF8 (Get_Active_Text (Win.Filter_Combo));
      begin
         for Item of Win.Filters loop
            if Item.Label.all = S then
               Filter := Item;
               exit;
            end if;
         end loop;
      end;

      if Filter = null then
         return;
      end if;

      Win.Current_Filter := Filter;

      begin
         Files := Read_Dir (Win.Current_Directory, Files_Only);

         for F in Files'Range loop
            if not Files (F).Is_Directory then
               Win.Files.Append (Files (F));
            end if;
         end loop;

         Unchecked_Free (Files);

         By_Basename.Sort (Win.Files);

         --  Register the function that will fill the list in the background

         Win.Remaining_Files := First (Win.Files);

         --  Do at least one pass to insert the files immediately

         if Display_File (File_Selector_Window_Access (Win))
           and then Win.Display_Idle_Handler = 0
         then
            Win.Display_Idle_Handler :=
              Idle_Add (Display_File'Access,
                        Win,
                        Notify => On_Display_Idle_Destroy'Access);
         end if;

      exception
         when VFS_Directory_Error =>
            Win.File_Model.Clear;
            Win.File_Model.Append (Iter, Null_Iter);
            Win.File_Model.Set
              (Iter, Base_Name_Column,
               -"Could not open " & Win.Current_Directory.Display_Full_Name);
      end;

      --  A workaround for a gtk3 issue: if you select a directory with no
      --  file, the tree view no longer uses the whole vertical space.
      --  Switching back to a directory that contains files still doesn't show
      --  them properly until the paned is resized.

      Win.File_Tree.Get_Allocation (Alloc);
      Win.File_Tree.Size_Allocate (Alloc);
   end Refresh_Files;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Win : access File_Selector_Window_Record'Class;
      Dir : GNATCOLL.VFS.Virtual_File)
   is
   begin
      --  If the new directory is not the one currently shown in the File_List,
      --  then update the File_List.

      if Dir /= No_File
        and then Win.Current_Directory /= Dir
        and then Is_Directory (Dir)
      then
         if +Base_Dir_Name (Dir) = -"Drives" then
            Win.Current_Directory := Create_From_Base ("");
         else
            Ensure_Directory (Dir);
            Normalize_Path (Dir);
            Win.Current_Directory := Dir;
         end if;

         --  If we are currently moving through the history,
         --  do not append items to the Location_Combo.

         if Win.Moving_Through_History then
            Win.Moving_Through_History := False;
         else
            Push (Win.Past_History, Get_Location (Win.Location_Combo));
            Clear (Win.Future_History);
            Set_Sensitive (Win.Back_Button);
            Set_Sensitive (Win.Forward_Button, False);

            if Win.History /= null and then Is_Local (Dir) then
               Add_To_History (Win.History.all, "directories",
                               +Full_Name (Dir, True));
            end if;
         end if;

         Set_Location (Win.Location_Combo, Dir);

         --  If the new directory is not the one currently shown
         --  in the Explorer_Tree, then update the Explorer_Tree.

         if Dir /= Get_Selection (Win.Explorer_Tree) then
            Show_Directory (Win.Explorer_Tree, Dir, Get_Window (Win));
         end if;

         if Win.File_Tree = null then
            Ensure_Directory (Dir);
            Set_Text (Win.Selection_Entry, Display_Full_Name (Dir));
            Set_Position
              (Win.Selection_Entry,
               Gint (UTF8_Strlen (Display_Full_Name (Dir))));
         else
            Refresh_Files (Win);
         end if;
      end if;
   end Change_Directory;

   ----------------------------
   -- On_Back_Button_Clicked --
   ----------------------------

   procedure On_Back_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
      S   : Virtual_File;

   begin
      Pop (Win.Past_History, S);

      if Is_Empty (Win.Past_History) then
         Set_Sensitive (Win.Back_Button, False);
      end if;

      if Is_Empty (Win.Future_History) then
         Set_Sensitive (Win.Forward_Button);
      end if;

      Push (Win.Future_History,
            Get_Location (Win.Location_Combo));

      Win.Moving_Through_History := True;
      Set_Location (Win.Location_Combo, S);
      Show_Directory (Win.Explorer_Tree, S);
   end On_Back_Button_Clicked;

   ----------------------------
   -- On_Home_Button_Clicked --
   ----------------------------

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      Host : constant String := Get_Host (Win.Current_Directory);
      H   : constant Virtual_File := Get_Home_Directory (Host);

   begin
      if H /= No_File then
         Change_Directory (Win, H);
      else
         Change_Directory (Win, Win.Home_Directory);
      end if;
   end On_Home_Button_Clicked;

   --------------------------
   -- On_Up_Button_Clicked --
   --------------------------

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Show_Parent (Win.Explorer_Tree);
      Change_Directory (Win, Get_Selection (Win.Explorer_Tree));
   end On_Up_Button_Clicked;

   -------------------------------
   -- On_Refresh_Button_Clicked --
   -------------------------------

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Refresh_Files (Win);
   end On_Refresh_Button_Clicked;

   -------------------------------
   -- On_Forward_Button_Clicked --
   -------------------------------

   procedure On_Forward_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
      S   : Virtual_File;

   begin
      Pop (Win.Future_History, S);

      if Is_Empty (Win.Future_History) then
         Set_Sensitive (Win.Forward_Button, False);
      end if;

      if Is_Empty (Win.Past_History) then
         Set_Sensitive (Win.Back_Button, True);
      end if;

      Push (Win.Past_History, Win.Current_Directory);

      Win.Moving_Through_History := True;
      Set_Location (Win.Location_Combo, S);
      Show_Directory (Win.Explorer_Tree, S);

   exception
      when Stack_Empty =>
         null;
   end On_Forward_Button_Clicked;

   -------------------
   -- Host_Selected --
   -------------------

   procedure Host_Selected (Object : access Gtk_Widget_Record'Class)
   is
      Win  : constant File_Selector_Window_Access :=
               File_Selector_Window_Access (Object);
      Host : constant String := Get_Selected (Win.Hosts_Combo);
      Dir  : Virtual_File;
      Dead : Message_Dialog_Buttons;
      List : Gtk_Tree_Store;
      Iter : Gtk_Tree_Iter;
      pragma Unreferenced (Dead);
   begin
      if Host /= Display_Local_Nickname then
         Dir := Get_Current_Dir (Host);
      else
         Dir := Get_Current_Dir;
      end if;

      if Is_Directory (Dir) then
         Change_Directory (Win, Dir);
         Set_Location (Win.Location_Combo, Dir);
      else
         raise Process_Died;
      end if;

   exception
      when Process_Died | Invalid_Process |
           VFS_Invalid_File_Error | VFS_Directory_Error =>
         Dead := Message_Dialog
           ("Problem while connecting to " & Host & ASCII.LF &
            "There might be a problem with Host's configuration",
            Dialog_Type => Error,
            Buttons     => Button_OK,
            Parent      => Gtk_Window (Win));
         if Is_Local (Win.Current_Directory) then
            --  local host is always the first iter
            Win.Hosts_Combo.Set_Active_Iter
              (Get_Iter_First (Win.Hosts_Combo.Get_Model));
         else
            List := -Win.Hosts_Combo.Get_Model;
            Iter := List.Get_Iter_First;
            while Iter /= Null_Iter loop
               if List.Get_String (Iter, 0) =
                 Get_Host (Win.Current_Directory)
               then
                  Win.Hosts_Combo.Set_Active_Iter (Iter);
                  exit;
               end if;
               List.Next (Iter);
            end loop;
         end if;
         Host_Selected (Object);
   end Host_Selected;

   ------------------------
   -- Directory_Selected --
   ------------------------

   procedure Directory_Selected
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Change_Directory
        (Win, Get_Location (Win.Location_Combo));
   end Directory_Selected;

   ---------------------
   -- Filter_Selected --
   ---------------------

   procedure Filter_Selected
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Refresh_Files (Win);
   end Filter_Selected;

   --------------------------------------
   -- On_Location_Combo_Entry_Activate --
   --------------------------------------

   procedure On_Location_Combo_Entry_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
      S   : constant Virtual_File := Get_Location (Win.Location_Combo);
   begin
      if Is_Directory (S) then
         Change_Directory (Win, S);
      end if;
   end On_Location_Combo_Entry_Activate;

   ---------------------------------
   -- On_Explorer_Tree_Select_Row --
   ---------------------------------

   procedure On_Explorer_Tree_Select_Row
     (Object : access GObject_Record'Class;
      Params : Gtk_Args)
   is
      pragma Unreferenced (Params);
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Object);
      Dir : constant Virtual_File := Win.Explorer_Tree.Get_Selection;

   begin
      if Win.In_Destruction then
         return;
      end if;

      if Dir /= No_File then
         Change_Directory (Win, Dir);
      end if;
   end On_Explorer_Tree_Select_Row;

   -------------------
   -- Name_Selected --
   -------------------

   procedure Name_Selected (File : access Gtk_Widget_Record'Class) is
      Win   : constant File_Selector_Window_Access :=
                File_Selector_Window_Access (File);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Win.File_Tree), Model, Iter);

      if Iter /= Null_Iter then
         Set_Text
           (Win.Selection_Entry,
            Get_String (Win.File_Model, Iter, Base_Name_Column));
      end if;
   end Name_Selected;

   --------------------------------
   -- On_File_List_End_Selection --
   --------------------------------

   procedure On_File_List_End_Selection
     (Object : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      pragma Unreferenced (Args);
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Name_Selected (Win);

      if Get_Text (Win.Selection_Entry) /= "" then
         if Win.Display_Remote then
            Last_Remote_Directory := Win.Current_Directory;
         else
            Last_Directory := Win.Current_Directory;
         end if;

         Win.OK_Button.Clicked;
      end if;
   end On_File_List_End_Selection;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
              File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Clear (Win.Past_History);
      Clear (Win.Future_History);

      Win.Files.Clear;
      Win.Filters.Clear;

      if Win.Display_Idle_Handler /= 0 then
         Remove (Win.Display_Idle_Handler);
      end if;
   end On_Destroy;

   ----------------------------------
   -- On_File_List_Key_Press_Event --
   ----------------------------------

   function On_File_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Win   : constant File_Selector_Window_Access :=
                File_Selector_Window_Access (Get_Toplevel (Object));
      Event : constant Gdk_Event := To_Event (Params, 1);
      Iter  : Gtk_Tree_Iter;

   begin
      declare
         S     : constant String := Interfaces.C.Strings.Value
           (Event.Key.String);
      begin
         if S'Length /= 0
           and then (Is_Alphanumeric (S (S'First))
                     or else Is_Special (S (S'First)))
         then
            Iter := Get_Iter_First (Win.File_Model);

            while Iter /= Null_Iter loop
               declare
                  T    : constant String :=
                           Get_String (Win.File_Model, Iter, Base_Name_Column);
                  Path : Gtk_Tree_Path;
               begin
                  if T'Length /= 0
                    and then T (T'First) = S (S'First)
                  then
                     Select_Iter (Get_Selection (Win.File_Tree), Iter);

                     Path := Get_Path (Win.File_Model, Iter);
                     Scroll_To_Cell
                       (Win.File_Tree, Path, null, True, 0.1, 0.1);
                     Path_Free (Path);

                     exit;
                  end if;
               end;

               Next (Win.File_Model, Iter);
            end loop;

            return True;
         else
            return False;
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_File_List_Key_Press_Event;

   ---------------------------------------
   -- On_Location_Entry_Key_Press_Event --
   ---------------------------------------

   function On_Location_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Win   : constant File_Selector_Window_Access :=
                File_Selector_Window_Access (Get_Toplevel (Object));
      Event : constant Gdk_Event := To_Event (Params, 1);

      use Gdk.Types;
   begin
      if Get_Key_Val (Event) = GDK_Return then
         declare
            S : Virtual_File renames Get_Location (Win.Location_Combo);
         begin
            if Is_Directory (S) then
               Change_Directory (Win, S);

               if Win.File_Tree /= null then
                  Set_Text (Win.Selection_Entry, "");
               end if;
            end if;
         end;

         return True;
      end if;

      return False;
   end On_Location_Entry_Key_Press_Event;

   ----------------------------------------
   -- On_Selection_Entry_Key_Press_Event --
   ----------------------------------------

   function On_Selection_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Win             : constant File_Selector_Window_Access :=
                          File_Selector_Window_Access (Get_Toplevel (Object));
      Event           : constant Gdk_Event := To_Event (Params, 1);
      S               : constant UTF8_String := Get_Text (Win.Selection_Entry);
      G               : constant String :=
        Interfaces.C.Strings.Value (Event.Key.String);

      First_Match     : Gtk_Tree_Iter := Null_Iter;
      --  The first column that completely matches S

      Suffix_Length   : Integer := -1;
      --  The length of the biggest common matching prefix

      Best_Match      : String (1 .. 1024);
      Iter            : Gtk_Tree_Iter;
      Path            : Gtk_Tree_Path;

      procedure Matcher
        (Base     : String;
         T        : String;
         Position : Gtk_Tree_Iter := Null_Iter);
      --  ??? Should replace Matcher by
      --  Project_Explorers_Files.Greatest_Common_Path

      -------------
      -- Matcher --
      -------------

      procedure Matcher
        (Base     : String;
         T        : String;
         Position : Gtk_Tree_Iter := Null_Iter)
      is
         K : Natural := 0;
      begin
         while K < T'Length
           and then K < Base'Length
           and then T (T'First + K) = Base (Base'First + K)
         loop
            K := K + 1;
         end loop;

         --  Does the prefix match S ?

         if K = Base'Length then
            if Suffix_Length = -1 then
               First_Match := Position;
               Best_Match (1 .. T'Length) := T;
               Suffix_Length := T'Length;

            else
               --  If there is already a biggest match, try to
               --  get it.

               while K < Suffix_Length
                 and then K < T'Length
                 and then T (T'First + K) = Best_Match (K + 1)
               loop
                  K := K + 1;
               end loop;

               Suffix_Length := K;
            end if;
         end if;
      end Matcher;

      use Gdk.Types;

   begin
      if Get_Key_Val (Event) = GDK_Tab then
         declare
            File     : Virtual_File;
            Dir      : Virtual_File;
         begin
            --  Handle "Tab completion".
            --  The current implementation will fail if there are file names
            --  longer than 1024 characters.

            --  Handle the easy part: change to the longest directory available

            if S /= "" then
               File := Create_From_UTF8
                 (S, Get_Host (Win.Current_Directory));
            else
               File := GNATCOLL.VFS.No_File;
            end if;

            if not Is_Absolute_Path (File) then
               File :=
                 Create_From_Dir (Win.Current_Directory, File.Full_Name.all);
            end if;

            if Is_Directory (File) then
               Change_Directory (Win, File);

            elsif Is_Directory (Get_Parent (File)) then
               Change_Directory (Win, Get_Parent (File));

            else
               --  File's dir is a non-existing directory: exit now

               return True;
            end if;

            if Win.File_Tree /= null then
               Set_Text (Win.Selection_Entry, Display_Base_Name (File));
               Set_Position
                 (Win.Selection_Entry,
                  Glib.Gint (UTF8_Strlen (Display_Base_Name (File))));
            end if;

            --  Simple case: Base is a complete valid directory

            if Is_Directory (File) then
               return True;
            end if;

            --  Base may be the start of a longer name: start the match and
            --  find out whether Base is the start of a unique directory, in
            --  which case open it, or the start of a file.

            if Win.File_Tree /= null then
               Iter := Get_Iter_First (Win.File_Model);

               while Iter /= Null_Iter loop
                  Matcher
                    (Display_Base_Name (File),
                     Display_Base_Name
                       (Get_File (Win.File_Model, Iter, File_Column)),
                     Iter);
                  Next (Win.File_Model, Iter);
               end loop;
            end if;

            declare
               Files : File_Array_Access :=
                         Read_Dir (Win.Current_Directory, Dirs_Only);
               --  ??? It would be nice to get those from the directory tree.
               --  This would remove a duplicated filesystem access.
            begin
               for F in Files'Range loop
                  Matcher (Display_Base_Name (File),
                           Display_Base_Dir_Name (Files (F)));
               end loop;
               Unchecked_Free (Files);
            end;

            if First_Match /= Null_Iter then
               --  The best match is a file

               if Suffix_Length > 0 then
                  Select_Iter (Get_Selection (Win.File_Tree), First_Match);
                  Path := Get_Path (Win.File_Model, First_Match);
                  Scroll_To_Cell (Win.File_Tree, Path, null, True, 0.1, 0.1);
                  Path_Free (Path);

                  Set_Text
                    (Win.Selection_Entry,
                     Best_Match (1 .. Suffix_Length));
                  Set_Position
                    (Win.Selection_Entry,
                     Gint (UTF8_Strlen (Best_Match (1 .. Suffix_Length))));
               end if;

            else
               --  The best match is a directory, or no match at all

               if Suffix_Length > 0 then
                  Set_Text
                    (Win.Selection_Entry,
                     Display_Base_Name
                       (Create (+Best_Match (1 .. Suffix_Length))));
                  Set_Position (Win.Selection_Entry, Gint (Suffix_Length));

                  Dir := GNATCOLL.VFS.Create_From_UTF8
                    (Win.Current_Directory.Display_Full_Name &
                     Best_Match (1 .. Suffix_Length),
                     Get_Host (Win.Current_Directory));

                  if Is_Directory (Dir)
                    and then Win.Current_Directory /= Dir
                  then
                     Set_Text (Win.Selection_Entry, "");
                     Change_Directory (Win, Dir);
                  end if;
               end if;
            end if;

            return True;
         end;
      end if;

      if Win.File_Tree /= null then
         Iter := Get_Iter_First (Win.File_Model);

         while Iter /= Null_Iter loop
            declare
               T : constant String :=
                     Get_String (Win.File_Model, Iter, Base_Name_Column);
               S : constant String :=
                     Get_Text (Win.Selection_Entry) & G;
            begin
               if T'Length >= S'Length
                 and then T (T'First .. T'First + S'Length - 1)
                 = S (S'First .. S'First + S'Length - 1)
               then
                  Path := Get_Path (Win.File_Model, Iter);
                  Scroll_To_Cell (Win.File_Tree, Path, null, True, 0.1, 0.1);
                  Path_Free (Path);

                  exit;
               end if;
            end;

            Next (Win.File_Model, Iter);
         end loop;
      end if;

      return False;

   exception
      --  ??? should catch VFS_Directory_Error
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Selection_Entry_Key_Press_Event;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (File_Selector_Window : out File_Selector_Window_Access;
      Root                 : Virtual_File;
      Initial_Directory    : Virtual_File;
      Dialog_Title         : String;
      Show_Files           : Boolean := True;
      History              : Histories.History;
      Remote_Browsing      : Boolean := False) is
   begin
      File_Selector_Window := new File_Selector_Window_Record;

      if Is_Absolute_Path (Root)
        and then Is_Directory (Root)
      then
         Initialize
           (File_Selector_Window, Root, Initial_Directory,
            Dialog_Title, Show_Files, History, Remote_Browsing);
      else
         Initialize
           (File_Selector_Window, Get_Current_Dir, Initial_Directory,
            Dialog_Title, Show_Files, History, Remote_Browsing);
      end if;
   end Gtk_New;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View) is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Set_Title (Col, -"Name");
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Comment_Column);
      Set_Title (Col, -"Info");
      Dummy := Append_Column (Tree, Col);

      Tree.Set_Headers_Visible (False);
   end Set_Column_Types;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                 : access File_Selector_Window_Record'Class;
      Root                 : Virtual_File;
      Initial_Directory    : Virtual_File;
      Dialog_Title         : String;
      Show_Files           : Boolean := True;
      History              : Histories.History;
      Remote_Browsing      : Boolean := False)
   is
      pragma Suppress (All_Checks);

      Toolbar1 : Gtk_Toolbar;
      Label1   : Gtk_Label;
      Button   : Gtk_Widget;
      Success  : Boolean;
      pragma Unreferenced (Button, Success);

      Hpaned1  : Gtk_Hpaned;

      Hbox2    : Gtk_Hbox;
      Hbox3    : Gtk_Hbox;
      Hbox4    : Gtk_Hbox;
      Hbox5    : Gtk_Hbox;
      Hbox6    : Gtk_Hbox;
      Hbox7    : Gtk_Hbox;

   begin
      Gtk.Dialog.Initialize
         (Self, Dialog_Title,
          Flags => Use_Header_Bar_From_Settings);
      Self.Set_Position (Win_Pos_Mouse);
      Self.Set_Modal (False);

      Self.History := History;
      Self.Home_Directory := Root;

      Parse (Self.Highlighted_Color, "#FF0000", Success);
      Parse (Self.Insensitive_Color, "#808080", Success);

      Gtk_New (Self.Explorer_Tree, Initial_Directory);
      Object_Callback.Object_Connect
        (Self.Explorer_Tree.Get_Tree_Selection,
         Gtk.Tree_Selection.Signal_Changed,
         On_Explorer_Tree_Select_Row'Access,
         Slot_Object => Self,
         After => True);

      if Show_Files then
         Self.Set_Default_Size (800, 650);
      else
         Self.Set_Default_Size (600, 650);
      end if;

      Gtk_New (Toolbar1);
      Toolbar1.Set_Orientation (Orientation_Horizontal);
      Toolbar1.Set_Style (Toolbar_Icons);
      Toolbar1.Set_Icon_Size (Icon_Size_Small_Toolbar);
      Self.Get_Content_Area.Pack_Start (Toolbar1, False, False);

      Gtk_New_From_Stock (Self.Back_Button, Stock_Go_Back);
      Self.Back_Button.Set_Tooltip_Text (-"Go To Previous Location");
      Toolbar1.Insert (Self.Back_Button);
      Widget_Callback.Connect
        (Self.Back_Button, Gtk.Tool_Button.Signal_Clicked,
         On_Back_Button_Clicked'Access);

      Gtk_New_From_Stock (Self.Forward_Button, Stock_Go_Forward);
      Self.Forward_Button.Set_Tooltip_Text (-"Go To Next Location");
      Toolbar1.Insert (Self.Forward_Button);
      Widget_Callback.Connect
        (Self.Forward_Button, Gtk.Tool_Button.Signal_Clicked,
         On_Forward_Button_Clicked'Access);

      Gtk_New_From_Stock (Self.Up_Button, Stock_Go_Up);
      Self.Up_Button.Set_Tooltip_Text (-"Go To Parent Directory");
      Toolbar1.Insert (Self.Up_Button);
      Widget_Callback.Connect
        (Self.Up_Button, Gtk.Tool_Button.Signal_Clicked,
         On_Up_Button_Clicked'Access);

      Gtk_New_From_Stock (Self.Refresh_Button, Stock_Refresh);
      Self.Refresh_Button.Set_Tooltip_Text (-"Refresh");
      Toolbar1.Insert (Self.Refresh_Button);
      Widget_Callback.Connect
        (Self.Refresh_Button, Gtk.Tool_Button.Signal_Clicked,
         On_Refresh_Button_Clicked'Access);

      Gtk_New_From_Stock (Self.Home_Button, Stock_Home);
      Self.Home_Button.Set_Tooltip_Text (-"Go To Home Directory");
      Toolbar1.Insert (Self.Home_Button);
      Widget_Callback.Connect
        (Self.Home_Button, Gtk.Tool_Button.Signal_Clicked,
         On_Home_Button_Clicked'Access);

      Self.Display_Remote :=
        Remote_Browsing or not Is_Local (Initial_Directory);

      if Remote_Browsing then
         Gtk_New_Hbox (Hbox2, False, 0);
         Self.Get_Content_Area.Pack_Start (Hbox2, False, False);

         Gtk_New (Label1, -("Host:"));
         Hbox2.Pack_Start (Label1, False, False, 3);

         Gtk_New_With_Entry (Self.Hosts_Combo);
         Self.Hosts_Combo.Append_Text (Display_Local_Nickname);

         declare
            Machines : constant GNAT.Strings.String_List := Get_Servers;
         begin
            for J in Machines'Range loop
               Trace (Me, "Adding " & Machines (J).all &
                      " in servers list");
               Self.Hosts_Combo.Append_Text (Machines (J).all);

               if Initial_Directory /= No_File
                 and then not Is_Local (Initial_Directory)
                 and then Get_Host (Initial_Directory) = Machines (J).all
               then
                  Self.Hosts_Combo.Set_Active (Gint (J));
               end if;
            end loop;
         end;

         Hbox2.Pack_Start (Self.Hosts_Combo, True, True, 3);

         --  Connect to Gtkada-combo's "changed" signal, that is raised when
         --  the list disapears. This prevents eventual dialogs appearing on
         --  host selection to be hidden by the drop down list.
         Widget_Callback.Object_Connect
           (Self.Hosts_Combo, Gtk.Combo_Box.Signal_Changed,
            Host_Selected'Access, Self, After => True);
      end if;

      Gtk_New_Hbox (Hbox3, False, 0);
      Self.Get_Content_Area.Pack_Start (Hbox3, False, False, 3);

      Gtk_New (Label1, -("Exploring :"));
      Hbox3.Pack_Start (Label1, False, False, 3);

      Gtk_New_With_Entry (Self.Location_Combo);
      Hbox3.Pack_Start (Self.Location_Combo, True, True, 3);
      Widget_Callback.Object_Connect
        (Self.Location_Combo,
         Gtk.Combo_Box.Signal_Changed,
         Directory_Selected'Access, Self.Location_Combo);

      Widget_Callback.Connect
        (Self.Location_Combo.Get_Child,
         Gtk.GEntry.Signal_Activate, On_Location_Combo_Entry_Activate'Access);
      Return_Callback.Connect
        (Self.Location_Combo.Get_Child,
         Signal_Key_Press_Event, On_Location_Entry_Key_Press_Event'Access,
         After => False);

      if Show_Files then
         Gtk_New_Hpaned (Hpaned1);
         Hpaned1.Set_Position (200);

         Gtk_New_Hbox (Hbox7, False, 0);
         Self.Get_Content_Area.Pack_Start (Hbox7, True, True, 3);

         Hbox7.Pack_Start (Hpaned1, True, True, 3);

         Hpaned1.Pack1 (Self.Explorer_Tree, True, True);

         Gtk_New (Self.Files_Scrolledwindow);
         Self.Files_Scrolledwindow.Set_Policy
           (Policy_Automatic, Policy_Always);
         Hpaned1.Pack2 (Self.Files_Scrolledwindow, True, True);

         Gtk_New (Self.File_Model, Columns_Types);
         Gtk_New (Self.File_Tree, Self.File_Model);
         Self.File_Model.Unref;  --  owned by the file_tree
         Self.Files_Scrolledwindow.Add (Self.File_Tree);
         Self.File_Tree.Set_Name ("file_selector_window.file_tree");
         Self.File_Tree.Set_Headers_Visible (True);
         Set_Column_Types (Self.File_Tree);

         Self.File_Tree.Get_Selection.Set_Mode (Selection_Single);
         Return_Callback.Connect
           (Self.File_Tree, Signal_Key_Press_Event,
            On_File_List_Key_Press_Event'Access);

         Widget_Callback.Object_Connect
           (Self.File_Tree.Get_Selection,
            Gtk.Tree_Selection.Signal_Changed,
            Name_Selected'Access, Self);

         Widget_Callback.Connect
           (Self.File_Tree, Signal_Row_Activated,
            On_File_List_End_Selection'Access);

      else
         Self.Get_Content_Area.Pack_Start
           (Self.Explorer_Tree, True, True, 3);
      end if;

      Gtk_New_Hbox (Hbox4, False, 0);
      Self.Get_Content_Area.Pack_Start (Hbox4, False, False, 3);

      Gtk_New (Self.Filter_Combo);
      Hbox4.Pack_Start (Self.Filter_Combo, True, True, 3);
      Widget_Callback.Connect
        (Self.Filter_Combo,
         Gtk.Combo_Box.Signal_Changed,
         Filter_Selected'Access);

      Gtk_New_Hbox (Hbox5, False, 0);
      Self.Get_Content_Area.Pack_Start (Hbox5, False, False, 3);

      Gtk_New (Self.Selection_Entry);
      Self.Selection_Entry.Set_Name ("file_selector_window.selection_entry");
      Self.Selection_Entry.Set_Editable (True);
      Self.Selection_Entry.Set_Max_Length (0);
      Self.Selection_Entry.Set_Visibility (True);
      Hbox5.Pack_Start (Self.Selection_Entry, True, True, 3);
      Self.Selection_Entry.Set_Activates_Default (True);

      Return_Callback.Connect
        (Self.Selection_Entry,
         Signal_Key_Press_Event, On_Selection_Entry_Key_Press_Event'Access);

      --  Get_History may trigger a call to Directory_Selected which in turn
      --  may need to access the Selection_Entry field, so need to move this
      --  call after Selection_Entry is created

      if History /= null then
         Get_History (History.all, Directories_Hist_Key, Self.Location_Combo);
      end if;

      Gtk_New_Hbox (Hbox6, False, 0);
      Self.Get_Content_Area.Pack_Start (Hbox6, False, False, 3);

      Self.OK_Button :=
        Gtk_Button (Add_Button (Self, Stock_Ok, Gtk_Response_OK));

      Self.OK_Button.Set_Name ("file_selector_window.ok_button");

      Button := Add_Button (Self, Stock_Cancel, Gtk_Response_Cancel);
      Self.Set_Default_Response (Gtk_Response_OK);

      if Initial_Directory /= No_File then
         Show_Directory
           (Self.Explorer_Tree, Initial_Directory, Self.Get_Window);
      else
         Show_Directory
           (Self.Explorer_Tree, GNATCOLL.VFS.Get_Current_Dir, Self.Get_Window);
      end if;

      Widget_Callback.Connect
        (Self, Signal_Destroy, On_Destroy'Access);

      Self.Selection_Entry.Grab_Focus;

      if Initial_Directory = No_File then
         Change_Directory (Self, GNATCOLL.VFS.Get_Current_Dir);
      else
         Change_Directory (Self, Initial_Directory);
      end if;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Filter : access File_Filter_Record) is
   begin
      Free (Filter.Label);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (Filter : in out File_Filter) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (File_Filter_Record'Class, File_Filter);
   begin
      Destroy (Filter);
      Unchecked_Free (Filter);
   end Free;

   ---------------------
   -- Browse_Location --
   ---------------------

   procedure Browse_Location (Ent : access Gtk_Widget_Record'Class) is
      Result : constant Gtk_Entry := Gtk_Entry (Ent);
      Name   : Virtual_File;
   begin
      if Get_Text (Result) = "" then
         Name := Select_Directory
           (-"Select directory",
            Parent            => Gtk_Window (Get_Toplevel (Ent)),
            Use_Native_Dialog => True,
            Base_Directory    => Get_Current_Dir);
      else
         Name := Select_Directory
           (-"Select directory",
            Parent            => Gtk_Window (Get_Toplevel (Ent)),
            Use_Native_Dialog => True,
            Base_Directory    => Create_From_UTF8 (Get_Text (Result)));
      end if;

      if Name /= No_File then
         Ensure_Directory (Name);
         Set_Text (Result, Display_Full_Name (Name));
      end if;
   end Browse_Location;

end Gtkada.File_Selector;
