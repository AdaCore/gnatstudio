-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001-2006 AdaCore                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.Regexp;               use GNAT.Regexp;
with Interfaces.C.Strings;
with System;

with Gdk;                       use Gdk;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;

with Gtk;                       use Gtk;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Ctree;                 use Gtk.Ctree;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.List;                  use Gtk.List;
with Gtk.List_Item;             use Gtk.List_Item;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Intl;               use Gtkada.Intl;
with Gtkada.Types;              use Gtkada.Types;

with File_Utils;                use File_Utils;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Remote;                    use Remote;
with Traces;                    use Traces;
with Unchecked_Deallocation;
with VFS;                       use VFS;

package body Gtkada.File_Selector is

   Me : constant Debug_Handle := Create ("Gtkada.File_Selector");

   Directories_Hist_Key : constant Histories.History_Key := "directories";
   --  Key used in the history

   Base_Name_Column  : constant := 0;
   Comment_Column    : constant := 1;
   Text_Color_Column : constant := 2;
   Icon_Column       : constant := 3;

   Last_Directory : Virtual_File := VFS.Get_Current_Dir;
   Last_Remote_Directory : Virtual_File := VFS.Get_Current_Dir;
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
      Kind        : Integer) return Chars_Ptr;
   pragma Import (C, NativeFileSelection, "NativeFileSelection");

   function NativeDirSelection
     (Title       : String;
      Basedir     : String) return Chars_Ptr;
   pragma Import (C, NativeDirSelection, "NativeDirSelection");

   procedure c_free (S : Chars_Ptr);
   pragma Import (C, c_free, "free");

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Location
     (Location_Combo : Gtk_Combo;
      Dir            : Virtual_File);
   --  Sets the location in the combo

   function Get_Location (Location_Combo : Gtk_Combo) return Virtual_File;
   --  Gets the location from the combo

   function Columns_Types return GType_Array;
   --  Return the types of the columns in the list.

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Change_Directory
     (Win : access File_Selector_Window_Record'Class;
      Dir : VFS.Virtual_File);
   --  Called every time that the contents of a new directory should be
   --  displayed in the File_Explorer. Dir is the absolute pathname to
   --  that directory.

   procedure Refresh_Files
     (Win : access File_Selector_Window_Record'Class);

   function Read_File
     (Win : File_Selector_Window_Access) return Boolean;
   --  Read one file from the current directory and insert it in Files.

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

   procedure Use_File_Filter
     (Filter    : access Regexp_Filter_Record;
      Win       : access File_Selector_Window_Record'Class;
      File      : Virtual_File;
      State     : out File_State;
      Pixbuf    : out Gdk_Pixbuf;
      Text      : out GNAT.OS_Lib.String_Access);
   --  See spec for more details on this dispatching routine.

   procedure Set_Busy (W : Gtk_Window; Busy : Boolean);
   --  Set/Reset the busy cursor on the specified window.

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

   procedure Filter_Selected
     (Object : access Gtk_Widget_Record'Class);
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

   procedure On_Selection_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  ???

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);
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
   --  Callback used to destroy the display idle loop.

   procedure On_Read_Idle_Destroy
     (Win : in out File_Selector_Window_Access);
   --  Callback used to destroy the read idle loop.

   procedure Name_Selected (File : access Gtk_Widget_Record'Class);
   --  Called when a new file has been selected.

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (Location_Combo : Gtk_Combo;
      Dir            : Virtual_File) is
   begin
      if Is_Local (Dir) then
         Add_Unique_Combo_Entry (Location_Combo,
                                 Full_Name (Dir, True).all);
         Set_Text (Get_Entry (Location_Combo), Full_Name (Dir, True).all);
      else
         Add_Unique_Combo_Entry
           (Location_Combo,
            Get_Host (Dir) & ":|" & Full_Name (Dir, True).all);
         Set_Text (Get_Entry (Location_Combo),
                   Get_Host (Dir) & ":|" & Full_Name (Dir, True).all);
      end if;
   end Set_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Location_Combo : Gtk_Combo) return Virtual_File is
      Str : constant String := Get_Text (Get_Entry (Location_Combo));
   begin
      for J in Str'First .. Str'Last - 1 loop
         if Str (J .. J + 1) = ":|" then
            return Create (Str (Str'First .. J - 1), Str (J + 2 .. Str'Last));
         end if;
      end loop;

      return Create (Str);
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
        (Text_Color_Column => Gdk_Color_Type,
         Base_Name_Column  => GType_String,
         Comment_Column    => GType_String,
         Icon_Column       => Gdk.Pixbuf.Get_Type);
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
           Case_Sensitive => Is_Case_Sensitive (GPS_Server));
      --  ??? At this place, we don't know what's the selected server. If we
      --  would, we could use the server's case sensitivity instead of the
      --  local one.
      return Filter;
   end Regexp_File_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter : access Regexp_Filter_Record;
      Win    : access File_Selector_Window_Record'Class;
      File   : Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access)
   is
      pragma Unreferenced (Win);
   begin
      Text   := null;
      Pixbuf := null;

      if Match (Base_Name (File), Filter.Pattern) then
         State := Normal;
      else
         State := Invisible;
      end if;
   end Use_File_Filter;

   -----------------------------
   -- On_Display_Idle_Destroy --
   -----------------------------

   procedure On_Display_Idle_Destroy
     (Win : in out File_Selector_Window_Access) is
   begin
      Win.Display_Idle_Handler := 0;
   end On_Display_Idle_Destroy;

   ---------------------
   -- On_Idle_Destroy --
   ---------------------

   procedure On_Read_Idle_Destroy (Win : in out File_Selector_Window_Access) is
   begin
      Win.Read_Idle_Handler := 0;
   end On_Read_Idle_Destroy;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Dialog : access File_Selector_Window_Record) return VFS.Virtual_File is
   begin
      if Dialog.Selection_Entry = null
        or else Get_Text (Dialog.Selection_Entry) = ""
      then
         return VFS.No_File;
      else
         declare
            Filename : constant String := Get_Text (Dialog.Selection_Entry);
            File     : Virtual_File;
         begin
            File := Create (Get_Host (Dialog.Current_Directory), Filename);
            if Is_Absolute_Path (File) then
               return File;
            else
               File := VFS.Create_From_Dir
                 (Dialog.Current_Directory, Filename);
               return File;
            end if;
         end;
      end if;
   end Get_Selection;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button
     (File_Selection : access File_Selector_Window_Record)
      return Gtk.Button.Gtk_Button is
   begin
      return File_Selection.Ok_Button;
   end Get_Ok_Button;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
     (File_Selection : access File_Selector_Window_Record)
      return Gtk.Button.Gtk_Button is
   begin
      return File_Selection.Cancel_Button;
   end Get_Cancel_Button;

   -----------------
   -- Select_File --
   -----------------

   function Select_File
     (Title             : String  := "Select a file";
      Base_Directory    : Virtual_File := No_File;
      File_Pattern      : String  := "";
      Pattern_Name      : String  := "";
      Default_Name      : String  := "";
      Parent            : Gtk_Window := null;
      Remote_Browsing   : Boolean := False;
      Use_Native_Dialog : Boolean := False;
      Kind              : File_Selector_Kind := Unspecified;
      History           : Histories.History := null) return VFS.Virtual_File
   is
      Pos_Mouse     : constant := 2;
      File_Selector : File_Selector_Window_Access;
      S             : Chars_Ptr;
      Working_Dir   : Virtual_File;
      Initial_Dir   : Virtual_File;

   begin
      if Use_Native_Dialog
        and then NativeFileSelectionSupported /= 0
        and then not Remote_Browsing
        and then Is_Local (Base_Directory)
      then
         --  Save working directory
         Working_Dir := Get_Current_Dir;

         if Base_Directory = No_File then
            if Last_Directory = No_File then
               Last_Directory := Working_Dir;
            end if;

            S := NativeFileSelection
              (Title & ASCII.NUL,
               Locale_Full_Name (Last_Directory) & ASCII.NUL,
               File_Pattern & ASCII.NUL,
               Pattern_Name & ASCII.NUL,
               Default_Name & ASCII.NUL,
               Pos_Mouse,
               File_Selector_Kind'Pos (Kind));

         else
            S := NativeFileSelection
              (Title & ASCII.NUL,
               Locale_Full_Name (Base_Directory) & ASCII.NUL,
               File_Pattern & ASCII.NUL,
               Pattern_Name & ASCII.NUL,
               Default_Name & ASCII.NUL,
               Pos_Mouse,
               File_Selector_Kind'Pos (Kind));
         end if;

         --  Change back to working directory
         Change_Dir (Working_Dir);

         declare
            Val : constant String := Interfaces.C.Strings.Value (S);
         begin
            c_free (S);

            if Val = "" then
               return VFS.No_File;
            else
               Last_Directory := Create (Locale_To_UTF8 (Dir_Name (Val)));

               return Create (Full_Filename => Locale_To_UTF8 (Val));
            end if;
         end;
      end if;

      Set_Busy (Parent, True);

      if Remote_Browsing then
         Initial_Dir := Last_Remote_Directory;
      else
         Initial_Dir := Last_Directory;
      end if;

      if Base_Directory = No_File then
         Gtk_New
           (File_Selector, Get_Root (Initial_Dir), Initial_Dir,
            Title, History => History, Remote_Browsing => Remote_Browsing);
      else
         Gtk_New
           (File_Selector, Get_Root (Base_Directory), Base_Directory,
            Title, History => History, Remote_Browsing => Remote_Browsing);
      end if;

      Set_Position (File_Selector, Win_Pos_Mouse);
      Set_Text
        (File_Selector.Selection_Entry, Locale_To_UTF8 (Default_Name));

      if File_Pattern /= "" then
         declare
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

               if Nl < Pattern_Name'Last and then Pattern_Name (Nl) = ';' then
                  No := 1;
               else
                  No := 0;
               end if;

               if Nf > Pattern_Name'Last then
                  Register_Filter
                    (File_Selector,
                     Regexp_File_Filter
                       (File_Pattern (Ff .. Fl - Fo), ""));
               else
                  Register_Filter
                    (File_Selector,
                     Regexp_File_Filter
                       (File_Pattern (Ff .. Fl - Fo),
                        Pattern_Name (Nf .. Nl - No)));
               end if;

               Fl := Fl + 1;
               Nl := Nl + 1;
            end loop;
         end;
      end if;

      Set_Busy (Parent, False);

      return Select_File (File_Selector, Parent);
   end Select_File;

   function Select_File
     (File_Selector : File_Selector_Window_Access;
      Parent        : Gtk_Window := null) return VFS.Virtual_File
   is
      Filter_A      : constant Filter_Show_All_Access := new Filter_Show_All;
      Selected_File : aliased Virtual_File := No_File;
   begin
      pragma Assert (File_Selector /= null);

      Filter_A.Label := new String'(-"All files");

      Register_Filter (File_Selector, Filter_A);
      Set_Modal (File_Selector, True);

      if Parent /= null then
         Set_Transient_For (File_Selector, Parent);
      end if;

      Widget_Callback.Object_Connect
        (File_Selector.Ok_Button, "clicked",
         On_Ok_Button_Clicked'Access, File_Selector);
      Widget_Callback.Object_Connect
        (File_Selector.Cancel_Button, "clicked",
         On_Cancel_Button_Clicked'Access, File_Selector);

      Show_All (File_Selector);
      Grab_Default (File_Selector.Ok_Button);

      File_Selector.Own_Main_Loop := True;

      File_Selector.Selected_File := Selected_File'Unchecked_Access;

      Gtk.Main.Main;

      return Selected_File;
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
      File_Selector_Window : File_Selector_Window_Access;
      S                    : Chars_Ptr;
      Working_Dir          : Virtual_File;

   begin
      if Use_Native_Dialog
        and then NativeFileSelectionSupported /= 0
        and then Is_Local (Base_Directory)
      then
         --  Save working directory
         Working_Dir := Get_Current_Dir;

         if Base_Directory = No_File then
            if Last_Directory = No_File then
               Last_Directory := Working_Dir;
            end if;

            S := NativeDirSelection
              (Title & ASCII.NUL,
               Locale_Full_Name (Last_Directory) & ASCII.NUL);

         else
            S := NativeDirSelection
              (Title & ASCII.NUL,
               Locale_Full_Name (Base_Directory) & ASCII.NUL);
         end if;

         --  Change back to working directory
         Change_Dir (Working_Dir);

         declare
            Val : constant String := Interfaces.C.Strings.Value (S);
         begin
            c_free (S);

            if Val = "" then
               return VFS.No_File;
            else
               Last_Directory := Create (Locale_To_UTF8 (Val));

               return Last_Directory;
            end if;
         end;
      end if;

      Set_Busy (Parent, True);

      if Base_Directory = No_File then
         if Last_Directory = No_File then
            Last_Directory := VFS.Get_Current_Dir;
         end if;

         Gtk_New
           (File_Selector_Window, Get_Root (Last_Directory), Last_Directory,
            Title, False, History);
      else
         Gtk_New
           (File_Selector_Window, Get_Root (Base_Directory), Base_Directory,
            Title, False, History);
      end if;

      Set_Position (File_Selector_Window, Win_Pos_Mouse);
      Set_Busy (Parent, False);

      return Select_Directory (File_Selector_Window, Parent);
   end Select_Directory;

   function Select_Directory
     (File_Selector : File_Selector_Window_Access;
      Parent        : Gtk_Window := null) return Virtual_File
   is
      Filter_A      : constant Filter_Show_All_Access := new Filter_Show_All;
      Selected_File : aliased Virtual_File := No_File;
   begin
      pragma Assert (File_Selector /= null);

      Filter_A.Label := new String'(-"All files");

      Register_Filter (File_Selector, Filter_A);
      Set_Modal (File_Selector, True);
      Set_Transient_For (File_Selector, Parent);

      Widget_Callback.Object_Connect
        (File_Selector.Ok_Button, "clicked",
         On_Ok_Button_Clicked'Access, File_Selector);
      Widget_Callback.Object_Connect
        (File_Selector.Cancel_Button, "clicked",
         On_Cancel_Button_Clicked'Access, File_Selector);

      Show_All (File_Selector);
      File_Selector.Own_Main_Loop := True;

      File_Selector.Selected_File := Selected_File'Unchecked_Access;

      Gtk.Main.Main;

      return Selected_File;
   end Select_Directory;

   ------------------
   -- Display_File --
   ------------------

   function Display_File (Win : File_Selector_Window_Access) return Boolean is
      Text        : String_Access;
      State       : File_State;
      Pixbuf      : Gdk_Pixbuf;
      Iter        : Gtk_Tree_Iter := Null_Iter;
      Color       : Gdk_Color := Null_Color;

      procedure Internal
        (Tree, Iter : System.Address;
         Col   : Gint;
         Value : Gdk_Color);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

   begin
      if Win.Current_Directory = No_File then
         return False;
      end if;

      for J in 1 .. 100 loop
         exit when Win.Remaining_Files = File_List.Null_Node;

         Use_File_Filter
           (Win.Current_Filter,
            Win,
            Data (Win.Remaining_Files),
            State,
            Pixbuf,
            Text);

         --  ??? The selectable state should be set here, if possible.

         Iter := Null_Iter;
         Color := Null_Color;

         case State is
            when Invisible =>
               null;

            when Normal =>
               Append (Win.File_Model, Iter, Null_Iter);

            when Highlighted =>
               Append (Win.File_Model, Iter, Null_Iter);
               Color := Win.Highlighted_Color;

            when Insensitive =>
               Append (Win.File_Model, Iter, Null_Iter);
               Color := Win.Insensitive_Color;

         end case;

         if Iter /= Null_Iter then
            Set (Win.File_Model, Iter, Base_Name_Column,
                 Locale_Base_Name (Data (Win.Remaining_Files)));

            if Text /= null then
               Set (Win.File_Model, Iter, Comment_Column,
                    Locale_To_UTF8 (Text.all));
               Free (Text);
            end if;

            if Color /= Null_Color then
               Internal
                 (Get_Object (Win.File_Model), Iter'Address,
                  Text_Color_Column, Color);
            end if;

            if Pixbuf /= Null_Pixbuf then
               Set (Win.File_Model, Iter, Icon_Column,
                    Gdk.C_Proxy (Pixbuf));
            end if;
         end if;

         Win.Remaining_Files := Next (Win.Remaining_Files);
      end loop;

      return Win.Remaining_Files /= File_List.Null_Node;
   end Display_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Win : File_Selector_Window_Access) return Boolean is
      Prev   : File_List.List_Node;
      Node   : File_List.List_Node;

   begin
      if Win.Current_Directory = No_File
        or else not Is_Directory (Win.Current_Directory)
      then
         return False;
      end if;

      declare
         Files : File_Array_Access
           := Read_Dir (Win.Current_Directory, Files_Only);
         Inserted : Boolean;
      begin
         for F in Files'Range loop
            if Is_Directory (Files (F)) then
               null;
               --  ??? or should we display directories in the File_List ?

            else
               Node := First (Win.Files);
               Prev := File_List.Null_Node;
               Inserted := False;

               Insert_Loop :
               while Node /= File_List.Null_Node loop
                  if Base_Name (Files (F)) < Base_Name (Data (Node)) then
                     Append (Win.Files, Prev, Files (F));
                     Inserted := True;
                     exit Insert_Loop;
                  end if;

                  Prev := Node;
                  Node := Next (Node);
               end loop Insert_Loop;

               if not Inserted then
                  Append (Win.Files, Files (F));
               end if;
            end if;
         end loop;

         Unchecked_Free (Files);
         Clear (Win.File_Model);

         --  Register the function that will fill the list in the background.

         Win.Remaining_Files := First (Win.Files);

         if Win.Display_Idle_Handler = 0 then
            Win.Display_Idle_Handler :=
              Add (Display_File'Access,
                   Win,
                   Destroy => On_Display_Idle_Destroy'Access);
         end if;

         return False;

      end;

   exception
      --  ??? should catch VFS_Directory_Error
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Read_File;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Win    : access File_Selector_Window_Record;
      Filter : access File_Filter_Record'Class) is
   begin
      Append (Win.Filters, File_Filter (Filter));
      Add_Unique_Combo_Entry (Win.Filter_Combo, Filter.Label.all);
      Refresh_Files (Win);
   end Register_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
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
      Text   := new String'("");
   end Use_File_Filter;

   -------------------
   -- Refresh_Files --
   -------------------

   procedure Refresh_Files (Win : access File_Selector_Window_Record'Class) is
      Dir    : constant Virtual_File := Win.Current_Directory;
      Filter : File_Filter := null;
      Iter   : Gtk_Tree_Iter;

   begin
      if Get_Window (Win) = null
        or else Win.File_Tree = null
        or else Dir = No_File
      then
         return;
      end if;

      Set_Busy (Gtk_Window (Win), True);
      Clear (Win.File_Model);
      Free (Win.Files);
      Win.Remaining_Files := File_List.Null_Node;

      --  Find out which filter to use.

      declare
         S : constant String :=
               Locale_From_UTF8 (Get_Text (Win.Filter_Combo_Entry));
         C : Filter_List.List_Node := First (Win.Filters);

      begin
         while C /= Filter_List.Null_Node loop
            if Data (C).Label.all = S then
               Filter := Data (C);
               exit;
            else
               C := Next (C);
            end if;
         end loop;
      end;

      if Filter = null then
         Set_Busy (Gtk_Window (Win), False);
         return;
      end if;

      Append (Win.File_Model, Iter, Null_Iter);
      Set (Win.File_Model, Iter, Base_Name_Column, -"Opening ... ");

      Win.Current_Filter := Filter;

      --  Fill the File_List.
      begin
         if Win.Read_Idle_Handler = 0 then
            Win.Read_Idle_Handler :=
              Add (Read_File'Access,
                   File_Selector_Window_Access (Win),
                   Destroy => On_Read_Idle_Destroy'Access);
         end if;

      exception
         when VFS_Directory_Error =>
            Clear (Win.File_Model);
            Append (Win.File_Model, Iter, Null_Iter);
            Set (Win.File_Model, Iter, Base_Name_Column,
                   -"Could not open " & Full_Name (Dir).all);
      end;

      Set_Busy (Gtk_Window (Win), False);
   end Refresh_Files;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Win : access File_Selector_Window_Record'Class;
      Dir : VFS.Virtual_File)
   is
   begin
      --  If the new directory is not the one currently shown in the File_List,
      --  then update the File_List.

      if Dir /= No_File
        and then Win.Current_Directory /= Dir
        and then Is_Directory (Dir)
      then
         if Base_Dir_Name (Dir) = -"Drives" then
            Win.Current_Directory := Create_From_Base ("");
         else
            Ensure_Directory (Dir);
            Win.Current_Directory := Dir;
         end if;

         --  If we are currently moving through the history,
         --  do not append items to the Location_Combo.

         if Win.Moving_Through_History then
            Win.Moving_Through_History := False;
         else
            Push (Win.Past_History,
                  Get_Location (Win.Location_Combo));
            Clear (Win.Future_History);
            Set_Sensitive (Win.Back_Button);
            Set_Sensitive (Win.Forward_Button, False);

            if Win.History /= null and then Is_Local (Dir) then
               Add_To_History (Win.History.all, "directories",
                               Full_Name (Dir, True).all);
            end if;
         end if;

         Set_Location (Win.Location_Combo, Dir);

         --  If the new directory is not the one currently shown
         --  in the Explorer_Tree, then update the Explorer_Tree.

         if Dir /= Get_Selection (Win.Explorer_Tree) then
            Show_Directory (Win.Explorer_Tree, Dir,
                            Get_Window (Win));
         end if;

         if Win.File_Tree = null then
            Ensure_Directory (Dir);
            Set_Text (Win.Selection_Entry, Full_Name (Dir, True).all);
            Set_Position (Win.Selection_Entry, Full_Name (Dir, True)'Length);
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

      Set_Location (Win.Location_Combo, S);
      Win.Moving_Through_History := True;
      Show_Directory (Win.Explorer_Tree, S);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Free (S);
   end On_Back_Button_Clicked;

   ----------------------------
   -- On_Home_Button_Clicked --
   ----------------------------

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

      H   : Virtual_File := Get_Home_Dir (Get_Host (Win.Current_Directory));
   begin
      if H /= No_File then
         Change_Directory
           (Win, H);
      else
         Change_Directory (Win, Win.Home_Directory);
      end if;

      Free (H);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Home_Button_Clicked;

   --------------------------
   -- On_Up_Button_Clicked --
   --------------------------

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      use type Node_List.Glist;

      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Show_Parent (Win.Explorer_Tree);
      Change_Directory (Win, Get_Selection (Win.Explorer_Tree));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Up_Button_Clicked;

   -------------------------------
   -- On_Refresh_Button_Clicked --
   -------------------------------

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      use type Node_List.Glist;

      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Refresh_Files (Win);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

      Push (Win.Past_History,
            Win.Current_Directory);

      Set_Location (Win.Location_Combo, S);
      Win.Moving_Through_History := True;
      Show_Directory (Win.Explorer_Tree, S);

   exception
      when Stack_Empty =>
         null;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Forward_Button_Clicked;

   -------------------
   -- Host_Selected --
   -------------------

   procedure Host_Selected (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Object);
      Host : constant String := Get_Text (Get_Entry (Win.Hosts_Combo));
      Dir  : Virtual_File;
      Dead : Message_Dialog_Buttons;
      pragma Unreferenced (Dead);
   begin
      if Host /= Local_Nickname then
         Dir := Get_Root (Create (Host, ""));
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
      when Process_Died | Invalid_Process | Invalid_Nickname =>
         Dead := Message_Dialog
           ("Problem while connecting to " & Host & ASCII.LF &
            "There might be a problem with Host's configuration",
            Dialog_Type => Error,
            Buttons     => Button_OK,
            Parent      => Gtk_Window (Win));
         if Is_Local (Win.Current_Directory) then
            Set_Text (Get_Entry (Win.Hosts_Combo),
                      Local_Nickname);
         else
            Set_Text (Get_Entry (Win.Hosts_Combo),
                      Get_Host (Win.Current_Directory));
         end if;
         Host_Selected (Object);
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

      Dir : constant Virtual_File := Get_Selection (Win.Explorer_Tree);
   begin
      if Dir /= No_File then
         Change_Directory (Win, Dir);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Explorer_Tree_Select_Row;

   -------------------
   -- Name_Selected --
   -------------------

   procedure Name_Selected (File : access Gtk_Widget_Record'Class) is
      Win : constant File_Selector_Window_Access :=
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
      Win   : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Name_Selected (Win);
      if Get_Text (Win.Selection_Entry) /= ""
        and then Win.Own_Main_Loop
      then
         if Win.Selected_File /= null then
            Win.Selected_File.all := Get_Selection (Win);
         end if;
         if Win.Display_Remote then
            Last_Remote_Directory := Win.Current_Directory;
         else
            Last_Directory := Win.Current_Directory;
         end if;

         Main_Quit;
         Win.Own_Main_Loop := False;
         Destroy (Win);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_File_List_End_Selection;

   --------------------------------
   -- On_Selection_Entry_Changed --
   --------------------------------

   procedure On_Selection_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Selection_Entry_Changed;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Object);
   begin
      if Win.Selected_File /= null then
         Win.Selected_File.all := Get_Selection (Win);
      end if;
      if Win.Display_Remote then
         Last_Remote_Directory := Win.Current_Directory;
      else
         Last_Directory := Win.Current_Directory;
      end if;

      Main_Quit;
      Win.Own_Main_Loop := False;
      Destroy (Win);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Ok_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Object);
   begin
      if Win /= null
        and then Win.Selection_Entry /= null
      then
         Set_Text (Win.Selection_Entry, "");
      end if;

      Main_Quit;
      Win.Own_Main_Loop := False;
      Destroy (Win);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Cancel_Button_Clicked;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Free (Win.Files);
      Free (Win.Filters);

      if Win.Display_Idle_Handler /= 0 then
         Idle_Remove (Win.Display_Idle_Handler);
      end if;

      if Win.Read_Idle_Handler /= 0 then
         Idle_Remove (Win.Read_Idle_Handler);
      end if;

      if Win.Own_Main_Loop then
         Main_Quit;
         Win.Own_Main_Loop := False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         S     : constant String := Locale_From_UTF8 (Get_String (Event));
         Found : Boolean := False;
      begin
         if S'Length /= 0
           and then (Is_Alphanumeric (S (S'First))
                     or else Is_Special (S (S'First)))
         then
            Iter := Get_Iter_First (Win.File_Model);

            while Iter /= Null_Iter loop
               exit when Found;

               declare
                  T : constant String :=
                    Locale_From_UTF8
                      (Get_String (Win.File_Model, Iter, Base_Name_Column));
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

                     Found := True;
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
            S : constant Virtual_File := Get_Location (Win.Location_Combo);
         begin
            if Is_Directory (S) then
               Change_Directory (Win, S);
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

      Found           : Boolean := False;
      Event           : constant Gdk_Event := To_Event (Params, 1);
      S               : constant UTF8_String := Get_Text (Win.Selection_Entry);
      G               : constant String :=
        Locale_From_UTF8 (Get_String (Event));

      First_Match     : Gtk_Tree_Iter := Null_Iter;
      --  The first column that completely matches S.

      Suffix_Length   : Integer := -1;
      --  The length of the biggest common matching prefix.

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
            Sub_File : Virtual_File;
            Dir      : Virtual_File;
         begin
            --  Handle "Tab completion".
            --  The current implementation will fail if there are file names
            --  longer than 1024 characters.

            --  Handle the easy part: change to the longest directory available

            File     := Create (Get_Host (Win.Current_Directory), S);
            Sub_File := Create_From_Dir (Win.Current_Directory, S);

            if Is_Absolute_Path (File)
              and then Is_Directory (VFS.Dir (File))
            then
               Change_Directory (Win, VFS.Dir (File));
            elsif Is_Directory (VFS.Dir (Sub_File)) then
               File := Sub_File;
               Change_Directory (Win, VFS.Dir (File));
               Set_Text (Win.Selection_Entry, Base_Dir_Name (File));
               Set_Position (Win.Selection_Entry, Base_Dir_Name (File)'Length);
            else
               --  Dir is a non-existing directory: exit now

               return True;
            end if;

            --  Simple case: Base is a complete valid directory.

            if Is_Directory (File) then
               Ensure_Directory (File);
               Change_Directory (Win, File);
               Set_Text (Win.Selection_Entry, "");
               return True;
            end if;

            --  Base may be the start of a longer name: start the match and
            --  find out whether Base is the start of a unique directory, in
            --  which case open it, or the start of a file.

            if Win.File_Tree /= null then
               Iter := Get_Iter_First (Win.File_Model);

               while Iter /= Null_Iter loop
                  Matcher
                    (VFS.Base_Dir_Name (File),
                     Locale_From_UTF8
                       (Get_String (Win.File_Model, Iter, Base_Name_Column)),
                     Iter);
                  Next (Win.File_Model, Iter);
               end loop;
            end if;

            declare
               Files : File_Array_Access :=
                         Read_Dir (Win.Current_Directory, Dirs_Only);
            begin
               for F in Files'Range loop
                  Matcher (Base_Name (S), Base_Dir_Name (Files (F)));
               end loop;
               Unchecked_Free (Files);
            end;

            if First_Match /= Null_Iter then
               --  The best match is a file.

               if Suffix_Length > 0 then
                  Select_Iter (Get_Selection (Win.File_Tree), First_Match);
                  Path := Get_Path (Win.File_Model, First_Match);
                  Scroll_To_Cell (Win.File_Tree, Path, null, True, 0.1, 0.1);
                  Path_Free (Path);

                  Set_Text (Win.Selection_Entry,
                            Locale_To_UTF8 (Best_Match (1 .. Suffix_Length)));
                  Set_Position (Win.Selection_Entry, Gint (Suffix_Length));
               end if;

            else
               --  The best match is a directory, or no match at all.

               if Suffix_Length > 0 then
                  Set_Text (Win.Selection_Entry,
                            Locale_To_UTF8 (Best_Match (1 .. Suffix_Length)));
                  Set_Position (Win.Selection_Entry, Gint (Suffix_Length));

                  Dir := VFS.Sub_Dir (Win.Current_Directory,
                                      Best_Match (1 .. Suffix_Length));
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
            exit when Found;

            declare
               T : constant String :=
                 Locale_From_UTF8
                   (Get_String (Win.File_Model, Iter, Base_Name_Column));
               S : constant String :=
                 Locale_From_UTF8 (Get_Text (Win.Selection_Entry)) & G;
            begin
               if T'Length >= S'Length
                 and then T (T'First .. T'First + S'Length - 1)
                 = S (S'First .. S'First + S'Length - 1)
               then
                  Found := True;
                  Path := Get_Path (Win.File_Model, Iter);
                  Scroll_To_Cell (Win.File_Tree, Path, null, True, 0.1, 0.1);
                  Path_Free (Path);
               end if;
            end;

            Next (Win.File_Model, Iter);
         end loop;
      end if;

      return False;

   exception
      --  ??? should catch VFS_Directory_Error
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
           (File_Selector_Window, VFS.Get_Current_Dir, Initial_Directory,
            Dialog_Title, Show_Files, History, Remote_Browsing);
      end if;
   end Gtk_New;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Dummy         : Gint;
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
   end Set_Column_Types;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (File_Selector_Window : access File_Selector_Window_Record'Class;
      Root                 : Virtual_File;
      Initial_Directory    : Virtual_File;
      Dialog_Title         : String;
      Show_Files           : Boolean := True;
      History              : Histories.History;
      Remote_Browsing      : Boolean := False)
   is
      pragma Suppress (All_Checks);

      Toolbar1    : Gtk_Toolbar;

      Label1      : Gtk_Label;
      Item        : Gtk_List_Item;

      Hpaned1     : Gtk_Hpaned;

      Hbox1       : Gtk_Hbox;
      Hbox2       : Gtk_Hbox;
      Hbox3       : Gtk_Hbox;
      Hbox4       : Gtk_Hbox;
      Hbox5       : Gtk_Hbox;
      Hbox6       : Gtk_Hbox;
      Hbox7       : Gtk_Hbox;

      Success     : Boolean;

   begin
      Gtk.Dialog.Initialize (File_Selector_Window);

      Set_Has_Separator (File_Selector_Window, False);

      File_Selector_Window.History := History;

      File_Selector_Window.Highlighted_Color := Parse ("#FF0000");
      Alloc_Color
        (Get_Default_Colormap,
         File_Selector_Window.Highlighted_Color,
         False, True, Success);

      File_Selector_Window.Insensitive_Color := Parse ("#808080");
      Alloc_Color
        (Get_Default_Colormap,
         File_Selector_Window.Insensitive_Color,
         False, True, Success);

      File_Selector_Window.Home_Directory := Root;

      Gtk_New
        (File_Selector_Window.Explorer_Tree,
         File_Selector_Window.Home_Directory,
         Initial_Directory);

      Set_Title (File_Selector_Window, Dialog_Title);

      if Show_Files then
         Set_Default_Size (File_Selector_Window, 600, 500);
      else
         Set_Default_Size (File_Selector_Window, 400, 647);
      end if;

      Set_Policy (File_Selector_Window, False, True, False);
      Set_Position (File_Selector_Window, Win_Pos_Mouse);
      Set_Modal (File_Selector_Window, False);

      Gtk_New_Hbox (Hbox1, False, 0);
      Pack_Start (Get_Vbox (File_Selector_Window),
                  Hbox1, False, False, 3);

      Gtk_New
        (Toolbar1,
         Orientation_Horizontal, Toolbar_Both);

      Set_Icon_Size (Toolbar1, Icon_Size_Button);
      Set_Style (Toolbar1,  Toolbar_Icons);
      File_Selector_Window.Back_Button := Insert_Stock
        (Toolbar1,
         Stock_Go_Back,
         -"Go To Previous Location",
         Position => -1);
      Set_Sensitive (File_Selector_Window.Back_Button, False);
      Widget_Callback.Connect
        (File_Selector_Window.Back_Button, "clicked",
         On_Back_Button_Clicked'Access);

      File_Selector_Window.Forward_Button := Insert_Stock
        (Toolbar1,
         Stock_Go_Forward,
         -"Go To Next Location",
         Position => -1);
      Set_Sensitive (File_Selector_Window.Forward_Button, False);
      Widget_Callback.Connect
        (File_Selector_Window.Forward_Button, "clicked",
         On_Forward_Button_Clicked'Access);

      Gtk_New (File_Selector_Window.Up_Icon, Stock_Go_Up, Icon_Size_Button);
      File_Selector_Window.Up_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Go To Parent Directory",
         Icon => Gtk_Widget (File_Selector_Window.Up_Icon));

      Widget_Callback.Connect
        (File_Selector_Window.Up_Button, "clicked",
         On_Up_Button_Clicked'Access);

      Gtk_New
        (File_Selector_Window.Refresh_Icon, Stock_Refresh, Icon_Size_Button);
      File_Selector_Window.Refresh_Button := Append_Element
        (Toolbar => Toolbar1,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Refresh",
         Icon => Gtk_Widget (File_Selector_Window.Refresh_Icon));
      Widget_Callback.Connect
        (File_Selector_Window.Refresh_Button, "clicked",
         On_Refresh_Button_Clicked'Access);

      File_Selector_Window.Home_Button := Insert_Stock
        (Toolbar1,
         Stock_Home,
         -"Go To Home Directory",
         Position => -1);
      Set_Sensitive (File_Selector_Window.Home_Button, True);
      Widget_Callback.Connect
        (File_Selector_Window.Home_Button, "clicked",
         On_Home_Button_Clicked'Access);

      Pack_Start (Hbox1, Toolbar1, True, True, 3);

      File_Selector_Window.Display_Remote :=
        Remote_Browsing or not Is_Local (Initial_Directory);

      if Remote_Browsing then
         Gtk_New_Hbox (Hbox2, False, 0);
         Pack_Start (Get_Vbox (File_Selector_Window), Hbox2, True, True, 0);

         Gtk_New (Label1, -("Host:"));
         Pack_Start (Hbox2, Label1, False, False, 3);

         Gtk_New (File_Selector_Window.Hosts_Combo);
         Set_Editable (Get_Entry (File_Selector_Window.Hosts_Combo), False);
         Gtk_New (Item, Local_Nickname);
         Add (Get_List (File_Selector_Window.Hosts_Combo), Item);

         for J in 1 .. Get_Nb_Machine_Descriptor loop
            Gtk_New (Item, Locale_To_UTF8 (Get_Nickname (J)));
            Add (Get_List (File_Selector_Window.Hosts_Combo), Item);
         end loop;
         if Initial_Directory /= No_File
           and then not Is_Local (Initial_Directory)
         then
            Set_Text (Get_Entry (File_Selector_Window.Hosts_Combo),
                      Get_Host (Initial_Directory));
         end if;

         Show_All (Get_List (File_Selector_Window.Hosts_Combo));
         Pack_Start (Hbox2, File_Selector_Window.Hosts_Combo, True, True, 3);

         --  Connect to Gtkada-combo's "changed" signal, that is raised when
         --  the list disapears. This prevents eventual dialogs appearing on
         --  host selection to be hidden by the drop down list.
         Widget_Callback.Object_Connect
           (File_Selector_Window.Hosts_Combo, "changed",
            Host_Selected'Access, File_Selector_Window, After => True);
      end if;

      Gtk_New_Hbox (Hbox3, False, 0);
      Pack_Start (Get_Vbox (File_Selector_Window), Hbox3, False, False, 3);

      Gtk_New (Label1, -("Exploring :"));
      Pack_Start (Hbox3, Label1, False, False, 3);

      Gtk_New (File_Selector_Window.Location_Combo);
      Set_Case_Sensitive (File_Selector_Window.Location_Combo, True);
      Pack_Start (Hbox3,
                  File_Selector_Window.Location_Combo, True, True, 3);
      Widget_Callback.Object_Connect
        (Get_Popup_Window (File_Selector_Window.Location_Combo),
         "hide",
         Directory_Selected'Access, File_Selector_Window.Location_Combo);

      if History /= null then
         Get_History (History.all, Directories_Hist_Key,
                      File_Selector_Window.Location_Combo);
      end if;

      File_Selector_Window.Location_Combo_Entry :=
        Get_Entry (File_Selector_Window.Location_Combo);
      Set_Editable (File_Selector_Window.Location_Combo_Entry, True);
      Set_Max_Length (File_Selector_Window.Location_Combo_Entry, 0);
      Set_Visibility (File_Selector_Window.Location_Combo_Entry, True);
      Widget_Callback.Connect
        (File_Selector_Window.Location_Combo_Entry, "activate",
         On_Location_Combo_Entry_Activate'Access);
      Return_Callback.Connect
        (File_Selector_Window.Location_Combo_Entry,
         "key_press_event", On_Location_Entry_Key_Press_Event'Access,
         After => False);

      Object_Callback.Object_Connect
        (Get_Tree_Selection (File_Selector_Window.Explorer_Tree),
         "changed",
         On_Explorer_Tree_Select_Row'Access,
         Slot_Object => File_Selector_Window,
         After => True);

      if Show_Files then
         Gtk_New_Hpaned (Hpaned1);
         Set_Position (Hpaned1, 200);
         Set_Handle_Size (Hpaned1, 10);
         Set_Gutter_Size (Hpaned1, 6);

         Gtk_New_Hbox (Hbox7, False, 0);
         Pack_Start
           (Get_Vbox (File_Selector_Window),
            Hbox7, True, True, 3);

         Pack_Start (Hbox7, Hpaned1, True, True, 3);

         Add (Hpaned1,
           File_Selector_Window.Explorer_Tree);

         Gtk_New (File_Selector_Window.Files_Scrolledwindow);
         Set_Policy
           (File_Selector_Window.Files_Scrolledwindow,
            Policy_Automatic, Policy_Always);
         Add (Hpaned1, File_Selector_Window.Files_Scrolledwindow);

         Gtk_New (File_Selector_Window.File_Model, Columns_Types);
         Gtk_New
           (File_Selector_Window.File_Tree,
            File_Selector_Window.File_Model);
         --  ??? File_Model should be Unref when File_Selector is destroyed

         Set_Headers_Visible (File_Selector_Window.File_Tree, True);
         Set_Column_Types (File_Selector_Window.File_Tree);

         Set_Mode
           (Get_Selection (File_Selector_Window.File_Tree), Selection_Single);
         Return_Callback.Connect
           (File_Selector_Window.File_Tree, "key_press_event",
            On_File_List_Key_Press_Event'Access);

         Widget_Callback.Object_Connect
           (Get_Selection (File_Selector_Window.File_Tree), "changed",
            Name_Selected'Access, File_Selector_Window);

         Widget_Callback.Connect
           (File_Selector_Window.File_Tree, "row_activated",
            On_File_List_End_Selection'Access);
         Add (File_Selector_Window.Files_Scrolledwindow,
              File_Selector_Window.File_Tree);

      else
         Pack_Start
           (Get_Vbox (File_Selector_Window),
            File_Selector_Window.Explorer_Tree, True, True, 3);
      end if;

      Gtk_New_Hbox (Hbox4, False, 0);
      Pack_Start
        (Get_Vbox (File_Selector_Window),
         Hbox4, False, False, 3);

      Gtk_New (File_Selector_Window.Filter_Combo);
      Set_Case_Sensitive (File_Selector_Window.Filter_Combo, False);

      Widget_Callback.Object_Connect
        (Get_Popup_Window (File_Selector_Window.Filter_Combo),
         "hide", Filter_Selected'Access, File_Selector_Window.Filter_Combo);

      Pack_Start (Hbox4,
                  File_Selector_Window.Filter_Combo, True, True, 3);

      File_Selector_Window.Filter_Combo_Entry :=
        Get_Entry (File_Selector_Window.Filter_Combo);
      Set_Editable (File_Selector_Window.Filter_Combo_Entry, False);
      Set_Max_Length (File_Selector_Window.Filter_Combo_Entry, 0);
      Set_Visibility (File_Selector_Window.Filter_Combo_Entry, True);

      Gtk_New_Hbox (Hbox5, False, 0);
      Pack_Start
        (Get_Vbox (File_Selector_Window),
         Hbox5, False, False, 3);

      Gtk_New (File_Selector_Window.Selection_Entry);
      Set_Editable (File_Selector_Window.Selection_Entry, True);
      Set_Max_Length (File_Selector_Window.Selection_Entry, 0);
      Set_Visibility (File_Selector_Window.Selection_Entry, True);
      Pack_Start
        (Hbox5, File_Selector_Window.Selection_Entry, True, True, 3);
      Set_Activates_Default (File_Selector_Window.Selection_Entry, True);

      Return_Callback.Connect
        (File_Selector_Window.Selection_Entry,
         "key_press_event", On_Selection_Entry_Key_Press_Event'Access);

      Widget_Callback.Connect
        (File_Selector_Window.Selection_Entry, "changed",
         On_Selection_Entry_Changed'Access);

      Gtk_New_Hbox (Hbox6, False, 0);
      Pack_Start
        (Get_Vbox (File_Selector_Window),
         Hbox6, False, False, 3);

      File_Selector_Window.Ok_Button :=
        Gtk_Button (Add_Button (File_Selector_Window,
                                Stock_Ok,
                                Gtk_Response_OK));

      Set_Relief (File_Selector_Window.Ok_Button, Relief_Normal);
      Set_Flags (File_Selector_Window.Ok_Button, Can_Default);

      File_Selector_Window.Cancel_Button :=
        Gtk_Button (Add_Button (File_Selector_Window,
                                Stock_Close,
                                Gtk_Response_Cancel));

      Set_Relief (File_Selector_Window.Cancel_Button, Relief_Normal);
      Set_Flags (File_Selector_Window.Cancel_Button, Can_Default);

      Realize (File_Selector_Window);

      if Initial_Directory /= No_File then
         Show_Directory
           (File_Selector_Window.Explorer_Tree,
            Initial_Directory,
            Get_Window (File_Selector_Window));
      else
         Show_Directory
           (File_Selector_Window.Explorer_Tree,
            VFS.Get_Current_Dir,
            Get_Window (File_Selector_Window));
      end if;

      Widget_Callback.Connect
        (File_Selector_Window, "destroy", On_Destroy'Access);

      Grab_Default (File_Selector_Window.Ok_Button);
      Grab_Focus (File_Selector_Window.Selection_Entry);

      if Initial_Directory = No_File then
         Change_Directory (File_Selector_Window, VFS.Get_Current_Dir);
      else
         Change_Directory (File_Selector_Window, Initial_Directory);
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

   procedure Free (F : in out Virtual_File) is
      pragma Unreferenced (F);
   begin
      null;
   end Free;

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
            Base_Directory    => Create (Get_Text (Result)));
      end if;

      if Name /= No_File then
         Set_Text (Result, Full_Name (Name).all);
      end if;
   end Browse_Location;

end Gtkada.File_Selector;
