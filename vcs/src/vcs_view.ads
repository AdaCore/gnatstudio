------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

--  This package contains the code shared by the VCS Explorer and
--  VCS Activities.

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;

with Gdk.Event;            use Gdk.Event;

with Gtk.Box;              use Gtk.Box;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Selection;   use Gtk.Tree_Selection;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;           use Gtk.Widget;

with GPS.Kernel;           use GPS.Kernel;
with VCS_Status;           use VCS_Status;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with GNATCOLL.VFS;         use GNATCOLL.VFS;
with Tooltips;

package VCS_View is

   type VCS_View_Record is abstract new Gtk_Hbox_Record with private;
   type VCS_View_Access is access all VCS_View_Record'Class;

   procedure Do_Delete (Explorer : VCS_View_Record) is abstract;
   --  Explorer specific action for the On_Delete event

   procedure Do_Refresh (Explorer : access VCS_View_Record) is abstract;
   --  Explorer specific for the Refresh action

   procedure Do_Fill_Info
     (Explorer  : VCS_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean) is abstract;
   --  Explorer specific for the Fill_Info action

   procedure Do_Initialize
     (Explorer : access VCS_View_Record;
      Kernel   : Kernel_Handle) is abstract;
   --  Explorer specific initialization

   function Build_View_Context
     (Explorer : not null access VCS_View_Record;
      Event : Gdk.Event.Gdk_Event)
      return Selection_Context is abstract;
   --  Describe the current selection

   function Build_View_Context
     (Child : not null access GPS_MDI_Child_Record'Class;
      Event : Gdk.Event.Gdk_Event)
      return Selection_Context
   is (Build_View_Context (VCS_View_Access (Child.Get_Actual_Widget), Event));

   function Columns_Types
     (Explorer : access VCS_View_Record) return GType_Array is abstract;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   procedure Create_Model (Explorer : access VCS_View_Record'Class);
   --  Creates the underlying tree model for VCS_View

   procedure Fill_Info
     (Explorer  : access VCS_View_Record'Class;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean);
   --  Fills the tree info at the given Iter with values from
   --  Status_Record.
   --  Success tells whether the information has been filled or not.

   function Get_Kernel
     (Explorer : access VCS_View_Record) return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated with VCS_View

   function Get_Selected_Files
     (Explorer : VCS_View_Access) return File_Array_Access;
   --  Return the list of files that are selected

   procedure Clear (Explorer : access VCS_View_Record'Class);
   --  Clear all the files in the model

   procedure On_Destroy (Self : in out VCS_View_Record) is null;
   --  Called when Self is destroyed.

   function Get_Iter_From_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File) return Gtk_Tree_Iter;
   --  Return the Iter associated with the given File.
   --  Return Null_Iter if no such iter was found.

   function Get_Iter_From_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Parent   : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Return the Iter associated with the given File under Parent.
   --  Return Null_Iter if no such iter was found.

   function Get_Iter_For_Root_Node
     (Explorer : access VCS_View_Record'Class;
      Column   : Gint;
      Value    : String) return Gtk_Tree_Iter;
   --  Look into the root nodes and return the iterator for the node with the
   --  given Value for the specified column.

   procedure Remove_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File);
   --  Remove the given file from the explorer

   procedure Refresh (Explorer : access VCS_View_Record'Class);
   --  Redraw the files in the VCS Explorer

   procedure Refresh_File
     (Explorer : access VCS_View_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Log      : Boolean := False);
   --  Refresh the file status and the "Log" column if Log set to true

   procedure Initialize
     (Explorer : access VCS_View_Record'Class;
      Kernel   : access Kernel_Handle_Record'Class);
   --  Initialize the explorer

   procedure Collapse_All (Explorer : access VCS_View_Record'Class);
   --  Collapse all nodes

   procedure Expand_All (Explorer : access VCS_View_Record'Class);
   --  Expand all nodes

   procedure Select_Files_Same_Status
     (Explorer : access VCS_View_Record'Class);
   --  Select all files having the same status as the current selection

   procedure On_Selected (View : access Gtk_Widget_Record'Class);
   --  Give the focus to the current page tree

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "delete_event" signal

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event

   generic
      with procedure Action
        (Iter : in out Gtk_Tree_Iter; -- Tree iterator
         Root : Boolean;              -- True if node is a root one
         Quit : in out Boolean);      -- Can be set to True to stop iterator
   procedure For_Every_Nodes
     (Explorer  : access VCS_View_Record'Class;
      Root_Only : Boolean);
   --  Iterates through the explorer nodes and call Action for each one. If
   --  Root_Only if set to True, Action is called only for the root nodes.

   procedure On_Menu_Expand_All
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Expand all nodes in the VCS Explorer view

   procedure On_Menu_Collapse_All
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Collapse all nodes in the VCS Explorer view

   procedure On_Menu_Select_Files_Same_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Select all files having the same status as the current selection

   function File_Key (File : GNATCOLL.VFS.Virtual_File) return String;
   --  Returns the key used for the file or directory in the view

private

   Base_Name_Column          : constant := 0;
   Name_Column               : constant := 1;
   Local_Rev_Column          : constant := 2;
   Rep_Rev_Column            : constant := 3;
   Status_Description_Column : constant := 4;
   Status_Icon_Name_Column   : constant := 5;
   Has_Log_Column            : constant := 6;
   Activity_Column           : constant := 7;
   Control_Column            : constant := 8;
   Key_Column                : constant := 9;
   File_Column               : constant := 10;
   --  Used to control the Activity "editable" attribute for the Activities
   --  Explorer or the Log "visible" attribute of the VCS Explorer.

   type VCS_View_Record is abstract new Gtk_Hbox_Record with record
      Kernel          : Kernel_Handle;
      --  Reference to the kernel that launched the explorer, if any

      Context         : Selection_Context;
      --  The current context being shown / selected in the explorer

      Tree            : Gtk_Tree_View;
      Model           : Gtk_Tree_Store;
      Iter            : Gtk_Tree_Iter;

      File_Column     : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column containing the file names

      Status_Column   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column containing the file status

      Log_Column      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column corresponding to the log file indicator

      Activity_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  The column containing the activity name
   end record;

   type VCS_Tooltips is new Tooltips.Tooltips with record
      Explorer : VCS_View_Access;
   end record;
   type VCS_Tooltips_Access is access all VCS_Tooltips'Class;
   overriding function Create_Contents
     (Tooltip : access VCS_Tooltips;
      Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y    : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation

   package Explorer_Selection_Foreach is
     new Gtk.Tree_Selection.Selected_Foreach_User_Data (VCS_View_Access);

end VCS_View;
