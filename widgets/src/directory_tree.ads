-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
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

--  This package implements a special version of a Gtk_Ctree that allows
--  browsing a complete directory hierarchy.
--  Initially, no node is expanded, so that the creation of the tree is as
--  fast as possible.
--  The directories on the disk are read only when a directory is expanded.
--
--  All directories passed as parameter to this file should use either '/'
--  or GNAT.Os_Lib.Directory_Separator as a separator.
--  They must end with a directory separator.

with Gdk.Pixmap;
with Gdk.Bitmap;
with Gdk.Window;
with Gtk.Box;
with Gtk.Clist;
with Gtk.Ctree;
with Gtk.Menu;
with Gtk.Main;
with GNAT.OS_Lib;

package Directory_Tree is

   ----------------------------------
   -- Low-level directory selector --
   ----------------------------------
   --  The following widget provides a simple directory selector. The
   --  directories can be expanded dynamically by the user, and the disk is
   --  read on-demand for efficiency reasons.
   --  However, this widget only allows the selection of a single
   --  directory. See below for another widget that provides multiple directory
   --  selection.

   type Dir_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Dir_Tree is access all Dir_Tree_Record'Class;

   procedure Gtk_New (Tree : out Dir_Tree; Root : String);
   --  Create a new tree, whose root node points to the directory Root.

   procedure Initialize (Tree : access Dir_Tree_Record'Class; Root : String);
   --  Internal function used to create the tree.

   procedure Show_Parent (Tree : access Dir_Tree_Record);
   --  Shows the parent of the selected node.

   procedure Show_Directory
     (Tree           : access Dir_Tree_Record;
      Dir            : String;
      Busy_Cursor_On : Gdk.Window.Gdk_Window := null);
   --  Expand the tree so that the directory Dir is visible.
   --  Dir must end with a directory separator.
   --  If Busy_Cursor_On is not null, then the cursor is that window is set to
   --  the busy cursor. It is recommended to use this parameter instead of
   --  setting this parameter yourself, since all this handling is done in an
   --  idle loop, and thus you would restore the cursor too early.

   function Get_Selection (Tree : access Dir_Tree_Record) return String;
   --  Return the absolute directory for the selected node.
   --  An empty string "" is returned if there is no selection currently.

   -----------------------------------
   -- High-level directory selector --
   -----------------------------------

   type Directory_Selector_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Directory_Selector is access all Directory_Selector_Record'Class;

   No_Selection : GNAT.OS_Lib.Argument_List (1 .. 0) := (others => null);

   procedure Gtk_New
     (Selector             : out Directory_Selector;
      Initial_Directory    : String;
      Root_Directory       : String := "/";
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection);
   --  Create a directory selector.
   --  Multiple_Directories should be True if multiple directories can be
   --  selected by the user.
   --  Root_Directory is the directory associated with the root node in the
   --  tree.

   procedure Initialize
     (Selector             : access Directory_Selector_Record'Class;
      Initial_Directory    : String;
      Root_Directory       : String := "/";
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection);
   --  Internal function for the creation of new widgets.

   function Get_Single_Selection
     (Selector  : access Directory_Selector_Record'Class) return String;
   --  Return the directory selected by the user.
   --  If Selector allowed multiple directories, only the first one is
   --  returned.
   --  The empty string is returned if there is no selection.

   function Get_Multiple_Selection
     (Selector : access Directory_Selector_Record'Class)
      return GNAT.OS_Lib.Argument_List;
   --  Return the list of all selected directories in Selector.
   --  If Selector only allowed the selection of a single directory, then an
   --  array of size 1 is returned.
   --  The return list must be freed by the caller

   function Single_Directory_Selector_Dialog (Initial_Directory : String)
      return String;
   --  Open a dialog in which the user can select a directory.
   --  Initial_Directory is the directory that is selected initially.

   function Multiple_Directories_Selector_Dialog
     (Initial_Directory : String;
      Initial_Selection : GNAT.OS_Lib.Argument_List := No_Selection)
      return GNAT.OS_Lib.Argument_List;
   --  Open a dialog for the selection of multiple directories by the user.
   --  Return the list of directories selected by the user.
   --  It is the responsability of the caller to free the array.


private
   type Dir_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with record
      Folder_Pix, Ofolder_Pix : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Mask, Ofolder_Mask : Gdk.Bitmap.Gdk_Bitmap;
      Idle      : Gtk.Main.Idle_Handler_Id;
   end record;

   type Directory_Selector_Record is new Gtk.Box.Gtk_Box_Record with
      record
         Directory : Dir_Tree;
         List      : Gtk.Clist.Gtk_Clist;
         Tree_Contextual_Menu : Gtk.Menu.Gtk_Menu;
         List_Contextual_Menu : Gtk.Menu.Gtk_Menu;
      end record;

end Directory_Tree;
