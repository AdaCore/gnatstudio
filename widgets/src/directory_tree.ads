-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements a special version of a Gtk_Ctree that allows
--  browsing a complete directory hierarchy.
--  Initially, no node is expanded, so that the creation of the tree is as
--  fast as possible.
--  The directories on the disk are read only when a directory is expanded.
--
--  All directories passed as parameter to this file should use either '/'
--  or GNAT.OS_Lib.Directory_Separator as a separator.
--  They must end with a directory separator.

with Gdk.Window;
with Gtk.Menu;
with Gtk.Main;
with Gtk.Paned;
with GNAT.OS_Lib;

with Gtk.Main;
with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Tree_Store;
with Gtk.Scrolled_Window;

with Generic_List;
with Gtk.Tree_Selection;

with VFS;

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

   type Dir_Tree_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Dir_Tree is access all Dir_Tree_Record'Class;

   procedure Gtk_New
     (Tree    : out Dir_Tree;
      Root    : VFS.Virtual_File;
      Initial : VFS.Virtual_File := VFS.No_File);
   --  Create a new tree, whose root node points to the directory Root,
   --  pointing at the Initial directory, if it is a subdirectory of Root.

   procedure Initialize
     (Tree    : access Dir_Tree_Record'Class;
      Root    : VFS.Virtual_File;
      Initial : VFS.Virtual_File);
   --  Internal function used to create the tree.

   procedure Show_Parent (Tree : access Dir_Tree_Record);
   --  Shows the parent of the selected node.

   procedure Show_Directory
     (Tree           : access Dir_Tree_Record;
      Dir            : VFS.Virtual_File;
      Busy_Cursor_On : Gdk.Window.Gdk_Window := null);
   --  Expand the tree so that the directory Dir is visible.
   --  If Busy_Cursor_On is not null, then the cursor is that window is set to
   --  the busy cursor. It is recommended to use this parameter instead of
   --  setting this parameter yourself, since all this handling is done in an
   --  idle loop, and thus you would restore the cursor too early.
   --
   --  If the directory doesn't exist, the closest possible parent is selected.

   function Get_Selection (Tree : access Dir_Tree_Record)
                           return VFS.Virtual_File;
   --  Return the absolute directory for the selected node.
   --  An empty string "" is returned if there is no selection currently.

   function Get_Tree_Selection
     (Tree : access Dir_Tree_Record)
      return Gtk.Tree_Selection.Gtk_Tree_Selection;
   --  Return the selection associated with the internal tree.

   -----------------------------------
   -- High-level directory selector --
   -----------------------------------

   type Directory_Selector_Record is new Gtk.Paned.Gtk_Paned_Record
     with private;
   type Directory_Selector is access all Directory_Selector_Record'Class;

   No_Selection : constant GNAT.OS_Lib.Argument_List (1 .. 0) :=
     (others => null);

   procedure Gtk_New
     (Selector             : out Directory_Selector;
      Initial_Directory    : VFS.Virtual_File;
      Root_Directory       : VFS.Virtual_File := VFS.Local_Root_Dir;
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection);
   --  Create a directory selector.
   --  Multiple_Directories should be True if multiple directories can be
   --  selected by the user.
   --  Root_Directory is the directory associated with the root node in the
   --  tree.
   --  You are responsible for freeing Initialize_Selection.

   procedure Initialize
     (Selector             : access Directory_Selector_Record'Class;
      Initial_Directory    : VFS.Virtual_File;
      Root_Directory       : VFS.Virtual_File := VFS.Local_Root_Dir;
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection);
   --  Internal function for the creation of new widgets.

   function Get_Single_Selection
     (Selector  : access Directory_Selector_Record'Class)
      return VFS.Virtual_File;
   --  Return the directory selected by the user.
   --  If Selector allowed multiple directories, only the first one is
   --  returned.
   --  The empty string is returned if there is no selection.

   function Get_Multiple_Selection
     (Selector : access Directory_Selector_Record'Class)
      return VFS.File_Array;
   --  Return the list of all selected directories in Selector.
   --  If Selector only allowed the selection of a single directory, then an
   --  array of size 1 is returned.
   --  The return list must be freed by the caller

private

   type Append_Directory_Idle_Data;
   type Append_Directory_Idle_Data_Access is access Append_Directory_Idle_Data;
   --  Custom data for the asynchronous fill function.

   package File_Append_Directory_Timeout is
      new Gtk.Main.Timeout (Append_Directory_Idle_Data_Access);

   procedure Free (D : in out Gtk.Main.Timeout_Handler_Id);

   package Timeout_Id_List is new Generic_List (Gtk.Main.Timeout_Handler_Id);

   type Dir_Tree_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record
   with record
      File_Tree  : Gtk.Tree_View.Gtk_Tree_View;
      File_Model : Gtk.Tree_Store.Gtk_Tree_Store;
      Expanding  : Boolean := False;

      Scroll_To_Directory : Boolean := False;
      Path                : Gtk.Tree_Model.Gtk_Tree_Path;
      Realize_Cb_Id       : Gtk.Handlers.Handler_Id;

      Current_Dir         : VFS.Virtual_File;

      Fill_Timeout_Ids : Timeout_Id_List.List;
   end record;

   type Directory_Selector_Record is new Gtk.Paned.Gtk_Paned_Record with record
      Directory : Dir_Tree;

      List_Tree  : Gtk.Tree_View.Gtk_Tree_View;
      List_Model : Gtk.Tree_Store.Gtk_Tree_Store;
      Tree_Contextual_Menu : Gtk.Menu.Gtk_Menu;
   end record;

end Directory_Tree;
