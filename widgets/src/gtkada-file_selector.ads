------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
--  For a basic usage, the typical code would be:
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
--
--  To handle the most common cases, a function Select_File is provided that
--  installs automatically default filters, and runs the main loop for you.
--
--  </description>

with Ada.Containers.Doubly_Linked_Lists;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Glib.Main;           use Glib.Main;

with Gdk.RGBA;            use Gdk.RGBA;
with Gdk.Pixbuf;          use Gdk.Pixbuf;

with Gtk.Button;          use Gtk.Button;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tool_Button;     use Gtk.Tool_Button;
with Gtk.Tree_Store;      use Gtk.Tree_Store;
with Gtk.Tree_View;       use Gtk.Tree_View;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;

with Directory_Tree;      use Directory_Tree;
with Generic_Stack;
with Histories;
with GNATCOLL.VFS;

private with GPS_Vectors;

package Gtkada.File_Selector is

   type File_Selector_Kind is
     (Open_File, Save_File, Open_Directory, Unspecified);

   function Select_File
     (Title             : String  := "Select a file";
      Base_Directory    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      File_Pattern      : GNATCOLL.VFS.Filesystem_String  := "";
      Pattern_Name      : String  := "";
      Default_Name      : GNATCOLL.VFS.Filesystem_String  := "";
      Parent            : Gtk_Window := null;
      Remote_Browsing   : Boolean := False;
      Use_Native_Dialog : Boolean := False;
      Kind              : File_Selector_Kind := Unspecified;
      History           : Histories.History  := null)
      return GNATCOLL.VFS.Virtual_File;
   --  Create a file selection dialog, display it, and return the selected file
   --  if any, or return a VFS.No_File if the user cancelled the dialog.
   --  Base_Directory is the directory on which the dialog starts. If the
   --  directory is invalid, then the dialog will point to the current
   --  directory.
   --  Remote_Browsing tells if browsing a remote host is allowed
   --  Parent is the parent window, used for setting the transient attribute.
   --  File_Pattern is a globbing pattern, as described in GNAT.Regexp (e.g.
   --  "*.htm"), or null for no filter. It is possible to pass multiple
   --  patterns by separating them with a semicolon.
   --  If Pattern_Name is not null, use this parameter instead of File_Pattern
   --  as the filter name. As for File_Pattern it is possible to pass multiple
   --  names (corresponding to each pattern) by separating them with a
   --  semicolon.
   --  Default_Name is the default value entered in the text entry.
   --  If Use_Native_Dialog is True, this function will use the native file
   --  selection widget instead of one based on GtkAda.
   --  ??? What if the filesystem path is non-UTF8?

   function Select_Directory
     (Title             : String  := "Select a directory";
      Base_Directory    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Parent            : Gtk_Window := null;
      Use_Native_Dialog : Boolean := False;
      History           : Histories.History := null)
      return GNATCOLL.VFS.Virtual_File;
   --  Create a directory selection dialog, display it, and return the absolute
   --  name of the selected directory, if any, or return an empty string.
   --  Base_Directory is the directory on which the dialog starts. If the
   --  directory is invalid, then the dialog will point to the current
   --  directory.
   --  See Select_File above for a description of the Use_Native_Dialog
   --  parameter.

   type File_State is (Normal, Highlighted, Insensitive, Invisible);
   --  The state of a file :
   --    Normal means the file is shown and selectable.
   --    Highlighted means the file is shown with a different color and
   --      selectable
   --    Insensitive means the file is shown but cannot be selected.
   --    Invisible means the file is not shown.

   type File_Selector_Window_Record is new Gtk_Dialog_Record with private;
   type File_Selector_Window_Access is
     access all File_Selector_Window_Record'Class;
   --  A file selector window

   function Select_File
     (File_Selector : File_Selector_Window_Access;
      Parent        : Gtk_Window := null) return GNATCOLL.VFS.Virtual_File;
   --  Display File_Selector on the screen, and wait until the user selects a
   --  file. VFS.No_File is returned if the user cancelled the dialog.
   --  As opposed to the first version of Select_File above, this one gives
   --  the opportunity to register filters before displaying the dialog.

   function Select_Directory
     (File_Selector : File_Selector_Window_Access;
      Parent        : Gtk_Window := null) return GNATCOLL.VFS.Virtual_File;
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
   --  ??? Add a Host parameter ?

   function Get_Selection
     (Dialog : access File_Selector_Window_Record)
      return GNATCOLL.VFS.Virtual_File;
   --  Return the selected file.
   --  Return VFS.No_File if the entry does not exist.

   -------------
   -- Filters --
   -------------

   type File_Filter_Record is abstract tagged record
      Label : String_Access;
   end record;
   type File_Filter is access all File_Filter_Record'Class;
   --  The basic type for file filters

   procedure Destroy (Filter : access File_Filter_Record);
   --  Free memory associated with Filter.
   --  You should always call the parent's Destroy operation from this
   --  procedure.

   procedure Use_File_Filter
     (Filter : access File_Filter_Record;
      Win    : access File_Selector_Window_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out String_Access) is abstract;
   --  This is the function that is called every time that a file could
   --  be shown in the file explorer.
   --  File is the considered file.
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
   --  This provides a basic filter that shows all files

   -------------
   -- Dialogs --
   -------------

   procedure Gtk_New
     (File_Selector_Window : out File_Selector_Window_Access;
      Root                 : GNATCOLL.VFS.Virtual_File;
      Initial_Directory    : GNATCOLL.VFS.Virtual_File;
      Dialog_Title         : String;
      Show_Files           : Boolean := True;
      History              : Histories.History;
      Remote_Browsing      : Boolean := False);
   --  Create a new file selector.
   --  Root is the directory shown in the root node of the tree. The user will
   --  not be able to select directories higher up in the hierarchy.
   --  Initial_Directory is the name of the directory that is selected
   --  initially, or the empty string for the current directory.
   --  Root must be an absolute path, and end with a directory separator.
   --  If Show_Files is False, then the widget will not display the
   --  list of files.

   procedure Initialize
     (Self                 : access File_Selector_Window_Record'Class;
      Root                 : GNATCOLL.VFS.Virtual_File;
      Initial_Directory    : GNATCOLL.VFS.Virtual_File;
      Dialog_Title         : String;
      Show_Files           : Boolean := True;
      History              : Histories.History;
      Remote_Browsing      : Boolean := False);
   --  Internal initialization function

private

   package File_List is new Ada.Containers.Doubly_Linked_Lists
     (GNATCOLL.VFS.Virtual_File, GNATCOLL.VFS."=");
   use File_List;

   procedure Free (Filter : in out File_Filter);

   package Filter_List is new GPS_Vectors (File_Filter);
   use Filter_List;

   package File_Selector_Idle is new Glib.Main.Generic_Sources
     (File_Selector_Window_Access);
   use File_Selector_Idle;

   package Dir_Stack is new Generic_Stack (GNATCOLL.VFS.Virtual_File);
   use Dir_Stack;

   overriding procedure Use_File_Filter
     (Filter : access Filter_Show_All;
      Win    : access File_Selector_Window_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out String_Access);
   --  Implementation of the Use_File_Filter procedure for
   --  the Filter_Show_All filter.

   type File_Selector_Window_Record is new Gtk_Dialog_Record with record
      Current_Directory      : GNATCOLL.VFS.Virtual_File :=
                                 GNATCOLL.VFS.No_File;
      --  The directory that is currently being explored.
      --  Current_Directory must always be a Normalized path, ending with
      --  a directory separator.

      Current_Host           : String_Access;

      Files                  : File_List.List;
      --  The list of files in the current directory

      Remaining_Files        : File_List.Cursor;
      --  The list of files that are in the current directory but not yet
      --  filtered nor shown in the file list.
      --  This list should never be allocated any memory explicitly, but
      --  should be a subset of Files.

      Current_Filter         : File_Filter;
      --  The filter that is currently used for displaying files

      Filters                : Filter_List.Vector;
      --  A list of all registered filters

      Highlighted_Color      : Gdk_RGBA := Null_RGBA;
      Insensitive_Color      : Gdk_RGBA := Null_RGBA;

      Moving_Through_History : Boolean := True;
      --  Set to true in case we are navigating using the back/forward buttons

      Display_Idle_Handler   : G_Source_Id := 0;
      --  Identifier for display idle loops

      Home_Directory         : GNATCOLL.VFS.Virtual_File :=
                                 GNATCOLL.VFS.Get_Current_Dir;

      Past_History           : Simple_Stack;
      Future_History         : Simple_Stack;
      Back_Button            : Gtk_Tool_Button;
      Forward_Button         : Gtk_Tool_Button;
      Home_Button            : Gtk_Tool_Button;
      Up_Button              : Gtk_Tool_Button;
      Refresh_Button         : Gtk_Tool_Button;

      Hosts_Combo            : Gtk_Combo_Box_Text;

      Location_Combo         : Gtk_Combo_Box_Text;

      Explorer_Tree          : Dir_Tree;

      Files_Scrolledwindow   : Gtk_Scrolled_Window;

      File_Tree              : Gtk_Tree_View;
      File_Model             : Gtk_Tree_Store;

      File_Icon_Label        : Gtk_Label;
      File_Name_Label        : Gtk_Label;
      File_Text_Label        : Gtk_Label;

      Filter_Combo           : Gtk_Combo_Box_Text;
      Selection_Entry        : Gtk_Entry;

      OK_Button              : Gtk_Button;

      History                : Histories.History;

      Display_Remote         : Boolean;
   end record;

end Gtkada.File_Selector;
