-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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
--  end Run_Test_File_Selector;
--  </description>

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Toolbar;         use Gtk.Toolbar;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Label;           use Gtk.Label;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist;           use Gtk.Clist;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Image;           use Gtk.Image;

with Gdk.Pixmap;
with Gdk.Bitmap;

with Directory_Tree; use Directory_Tree;
with Generic_Stack;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Gtkada.File_Selector is

   type File_State is (Normal, Highlighted, Inactive, Invisible);
   --  The state of a file :
   --    Normal means the file is shown and selectable.
   --    Highlighted means the file is shown with a different color and
   --      selectable
   --    Inactive means the file is shown but cannot be selected.
   --    Invisible means the file is not shown.

   type File_Selector_Window_Record is new
     Gtk.Widget.Gtk_Widget_Record with private;
   type File_Selector_Window_Access is
     access all File_Selector_Window_Record'Class;
   --  A file selector window.

   type File_Filter_Record
     (Label : String_Access)
      is abstract tagged null record;
   type File_Filter is access all File_Filter_Record'Class;
   --  The basic type for file filters.

   procedure Use_File_Filter
     (Filter    : access File_Filter_Record;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
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
     (Win    : File_Selector_Window_Access;
      Filter : access File_Filter_Record'Class);
   --  Register a file filter in the file selector.

   type Filter_Show_All is new File_Filter_Record (new String'("All files"))
     with null record;
   type Filter_Show_All_Access is access all Filter_Show_All'Class;
   --  This provides a basic filter that shows all files.

   procedure Gtk_New
     (File_Selector_Window : out File_Selector_Window_Access;
      Directory            : in String);
   --  Create a new file selector. Directory is the directory that should
   --  be opened when the file selector is first shown.

   procedure Initialize
     (File_Selector_Window : access File_Selector_Window_Record'Class;
      Directory            : in String);
   --  Internal initialization function.

private

   package String_Stack is new Generic_Stack (String_Access);
   use String_Stack;

   procedure Use_File_Filter
     (Filter    : access Filter_Show_All;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access);
   --  ???

   -----------------
   -- Filter_List --
   -----------------

   type Filter_List_Node;
   type Filter_List is access Filter_List_Node;

   type Filter_List_Node is record
      Element : File_Filter;
      Next    : Filter_List;
   end record;

   type File_Selector_Window_Record is new Gtk_Window_Record with record
      --  ??? Fields that do not need to be referenced directly should not
      --  be in this record, but only declared as local variables in Initialize

      Filters : Filter_List;

      Moving_Through_History : Boolean := True;
      Current_Directory : String_Access := new String'("");
      Home_Directory : String_Access := new String'("");

      Past_History : Simple_Stack;
      Future_History : Simple_Stack;

      File_Selector_Vbox : Gtk_Vbox;
      Hbox1 : Gtk_Hbox;
      Hbox3 : Gtk_Hbox;
      Toolbar1 : Gtk_Toolbar;
      Back_Button : Gtk_Button;
      Forward_Button : Gtk_Button;
      Home_Button : Gtk_Button;

      Up_Button : Gtk_Widget;
      Up_Icon   : Gtk_Image;

      Refresh_Button : Gtk_Widget;
      Refresh_Icon   : Gtk_Image;

      Hbox2 : Gtk_Hbox;
      Label10 : Gtk_Label;
      Location_Combo : Gtk_Combo;
      Location_Combo_Entry : Gtk_Entry;
      Hpaned1 : Gtk_Hpaned;
      Explorer_Tree_Scrolledwindow : Gtk_Scrolled_Window;

      Explorer_Tree : Dir_Tree;

      Label7 : Gtk_Label;
      Label8 : Gtk_Label;
      Label9 : Gtk_Label;
      Files_Scrolledwindow : Gtk_Scrolled_Window;
      File_List : Gtk_Clist;
      File_Icon_Label : Gtk_Label;
      File_Name_Label : Gtk_Label;
      File_Text_Label : Gtk_Label;
      Hbox4 : Gtk_Hbox;
      Filter_Combo : Gtk_Combo;
      Filter_Combo_Entry : Gtk_Entry;
      Hbox5 : Gtk_Hbox;
      Selection_Entry : Gtk_Entry;
      Hbox6 : Gtk_Hbox;
      Hbuttonbox2 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;

end Gtkada.File_Selector;
