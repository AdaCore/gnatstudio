-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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

--  <description>
--  This widget provides an advanced file selection dialog.
--
--  This widget provides a directory explorer as well as a file explorer.
--  The user decides which files should be viewed in the file explorer
--  by selecting a filter.
--
--  User-defined filters can be created by inheriting from
--  File_Filter_Record and overriding Use_File_Filter.
--
--  Before this widget can be used, any filters must be registered
--  using Register_Filter.
--
--  For a basic usage, the typical code would be :
--
--  procedure Run_File_Selector is
--     File_Selector_Window : File_Selector_Window_Access;
--     Filter_A : Filter_Show_All_Access := new Filter_Show_All;
--  begin
--     Gtk.Main.Init;
--     Gtk_New (File_Selector_Window, "/");
--
--     Register_Filter (File_Selector_Window, Filter_A);
--
--     Show_All (File_Selector_Window);
--     Gtk.Main.Main;
--     Free (Filter_A);
--  end Run_Test_File_Selector;
--  </description>

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Label;           use Gtk.Label;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist;           use Gtk.Clist;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Button;          use Gtk.Button;
with Gtk.Image;           use Gtk.Image;
with Gtk.Style;           use Gtk.Style;
with Gtk.Main;            use Gtk.Main;
with Gtk.Widget;

with Gdk.Pixmap;
with Gdk.Bitmap;

with Directory_Tree; use Directory_Tree;
with Generic_Stack;
with Generic_List;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Unchecked_Deallocation;

package Gtkada.File_Selector is

   function Select_File
     (Title          : String := "Select a file";
      Base_Directory : String := "") return String;
   --  Create a file selection dialog, display it, and return the absolute file
   --  name that was selected, if any, or return an empty string.
   --  Base_Directory is the directory on which the dialog starts. If the
   --  directory is invalid, then the dialog will point to the current
   --  directory.

   function Select_Directory
     (Title          : String := "Select a directory";
      Base_Directory : String := "") return String;
   --  Create a directory selection dialog, display it, and return the absolute
   --  name of the selected directory, if any, or return an empty string.
   --  Base_Directory is the directory on which the dialog starts. If the
   --  directory is invalid, then the dialog will point to the current
   --  directory.

   type File_State is (Normal, Highlighted, Insensitive, Invisible);
   --  The state of a file :
   --    Normal means the file is shown and selectable.
   --    Highlighted means the file is shown with a different color and
   --      selectable
   --    Insensitive means the file is shown but cannot be selected.
   --    Invisible means the file is not shown.

   type File_Selector_Window_Record is new
     Gtk.Widget.Gtk_Widget_Record with private;
   type File_Selector_Window_Access is
     access all File_Selector_Window_Record'Class;
   --  A file selector window.

   function Select_File (File_Selector : File_Selector_Window_Access)
      return String;
   --  Display File_Selector on the screen, and wait until the user selects a
   --  file. The absolute file name is returned, or the empty string if the
   --  user cancelled the dialog.
   --  As opposed to the first version of Select_File above, this one gives
   --  the opportunity to register filters before displaying the dialog.

   function Select_Directory (File_Selector : File_Selector_Window_Access)
      return String;
   --  Display File_Selector on the screen, and wait until the user selects a
   --  file. The absolute dir name is returned, or the empty string if the
   --  user cancelled the dialog.
   --  As opposed to the first version of Select_Directory above, this one
   --  gives the opportunity to register filters before displaying the dialog.
   --
   --  ??? The filters do not have any effect on directories yet.

   procedure Browse_Location (Ent : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  This subprogram should be used as a callback for a "browse" button.
   --  It will get its initial directory from the text in the Gtk_Entry, and
   --  put the result there.

   function Get_Selection (Dialog : access File_Selector_Window_Record)
      return String;
   --  Return the selected file.
   --  Return an empty string if the entry does not exist.

   function Get_Ok_Button
     (File_Selection : access File_Selector_Window_Record)
     return Gtk.Button.Gtk_Button;
   --  Return the OK button.
   --  The callbacks on this button should close the dialog and do something
   --  with the file selected by the user.

   function Get_Cancel_Button
     (File_Selection : access File_Selector_Window_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the Cancel button.
   --  To remove this button from the dialog, call Hide on the return value.
   --  The callbacks on this button should simply close the dialog, but should
   --  ignore the file selected by the user.

   -------------
   -- Filters --
   -------------

   type File_Filter_Record is abstract tagged record
      Label : String_Access;
   end record;
   type File_Filter is access all File_Filter_Record'Class;
   --  The basic type for file filters.

   procedure Destroy (Filter : access File_Filter_Record);
   --  Free memory associated with Filter.
   --  You should always call the parent's Destroy operation from this
   --  procedure.

   procedure Use_File_Filter
     (Filter    : access File_Filter_Record;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : String;
      File      : String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access) is abstract;
   --  This is the function that is called every time that a file could
   --  be shown in the file explorer.
   --  Dir is the directory the file is in.
   --  File is the file name.
   --  State is the state the file should be displayed in.
   --  Pixmap is an icon that can be associated to a file,
   --  with the associated Mask.
   --  Text represents additional info that can be displayed

   procedure Register_Filter
     (Win    : access File_Selector_Window_Record;
      Filter : access File_Filter_Record'Class);
   --  Register a file filter in the file selector.
   --  The first filter registered will be the default one in the file
   --  selector.
   --  The filter is automatically freed when the file selector is destroyed.

   ---------------------
   -- Show_All filter --
   ---------------------

   type Filter_Show_All is new File_Filter_Record with null record;
   type Filter_Show_All_Access is access all Filter_Show_All;
   --  This provides a basic filter that shows all files.

   -------------
   -- Dialogs --
   -------------

   procedure Gtk_New
     (File_Selector_Window : out File_Selector_Window_Access;
      Root                 : String;
      Initial_Directory    : String;
      Dialog_Title         : String;
      Show_Files           : Boolean := True);
   --  Create a new file selector.
   --  Root is the directory shown in the root node of the tree. The user will
   --  not be able to select directories higher up in the hierarchy.
   --  Initial_Directory is the name of the directory that is selected
   --  initially, or the empty string for the current directory.
   --  Root must be an absolute path, and end with a directory separator.
   --  If Show_Files is False, then the widget will not display the
   --  list of files.

   procedure Initialize
     (File_Selector_Window : access File_Selector_Window_Record'Class;
      Root                 : String;
      Initial_Directory    : String;
      Dialog_Title         : String;
      Show_Files           : Boolean := True);
   --  Internal initialization function.

private

   procedure Free (S : in out String);
   package String_List is new Generic_List (String);
   use String_List;

   procedure Free (Filter : in out File_Filter);

   package Filter_List is new Generic_List (File_Filter, Free);
   use Filter_List;

   package File_Selector_Idle is new Idle (File_Selector_Window_Access);
   use File_Selector_Idle;

   package String_Stack is new Generic_Stack (String_Access);
   use String_Stack;

   type Dir_Type_Access is access Dir_Type;
   procedure Free is new Unchecked_Deallocation
     (Dir_Type, Dir_Type_Access);

   procedure Use_File_Filter
     (Filter    : access Filter_Show_All;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : String;
      File      : String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access);
   --  Implementation of the Use_File_Filter procedure for
   --  the Filter_Show_All filter.

   type File_Selector_Window_Record is new Gtk_Window_Record with record
      Current_Directory    : String_Access := new String'("");
      Current_Directory_Id : Dir_Type_Access := new Dir_Type;
      --  The directory that is currently being explored.
      --  Current_Directory must always be a Normalized path, ending with
      --  a directory separator.

      Current_Directory_Is_Open : Boolean := False;
      --  Tells whether the current directory is being read.

      Files : String_List.List;
      --  The list of files in the current directory.

      Remaining_Files : String_List.List_Node;
      --  The list of files that are in the current directory but not yet
      --  filtered nor shown in the file list.
      --  This list should never be allocated any memory explicitly, but
      --  should be a subset of Files.

      Current_Filter : File_Filter;
      --  The filter that is currently used for displaying files.

      Filters : Filter_List.List;
      --  A list of all registered filters.

      Normal_Style : Gtk_Style;
      --  The style for displaying normal state;

      Highlighted_Style : Gtk_Style;
      --  How the highlighted file names should be displayed.

      Insensitive_Style : Gtk_Style;
      --  How the inactive file names should be displayed.

      Moving_Through_History : Boolean := True;
      --  Set to true in case we are navigating using the back/forward buttons.

      Own_Main_Loop : Boolean := False;
      --  Whether the file selector is running in its own main loop or not.

      Read_Idle_Handler   : Idle_Handler_Id;
      --  Identifier for read idle loops.

      Display_Idle_Handler   : Idle_Handler_Id;
      --  Identifier for display idle loops.

      Home_Directory : String_Access := new String'("");

      Past_History : Simple_Stack;
      Future_History : Simple_Stack;

      File_Selector_Vbox : Gtk_Vbox;

      Back_Button : Gtk_Button;
      Forward_Button : Gtk_Button;
      Home_Button : Gtk_Button;

      Up_Button : Gtk_Widget;
      Up_Icon   : Gtk_Image;

      Refresh_Button : Gtk_Widget;
      Refresh_Icon   : Gtk_Image;

      Location_Combo : Gtk_Combo;
      Location_Combo_Entry : Gtk_Entry;

      Explorer_Tree_Scrolledwindow : Gtk_Scrolled_Window;

      Explorer_Tree : Dir_Tree;

      Files_Scrolledwindow : Gtk_Scrolled_Window;
      File_List : Gtk_Clist;
      File_Icon_Label : Gtk_Label;
      File_Name_Label : Gtk_Label;
      File_Text_Label : Gtk_Label;

      Filter_Combo : Gtk_Combo;
      Filter_Combo_Entry : Gtk_Entry;
      Selection_Entry : Gtk_Entry;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;

end Gtkada.File_Selector;
