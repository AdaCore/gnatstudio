-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package groups a tree (that shows projects, directories, files, and
--  entities in the files), and the display of the scenario variables that the
--  user can modify.
--  This widget also knows how to save its state to an Ada stream, and re-read
--  a previously saved configuration.

with Glide_Kernel;
with Scenario_Views;
with Gtk.Main;
with Gtk.Ctree;
with Gtk.Handlers;
with Gdk.Pixmap;
with Gdk.Bitmap;
with Gdk.Pixbuf;

with Gtk.Box;
with Gtk.Notebook;
with Gtk.Tree_View;
with Gtk.Tree_Store;

with Generic_List;

package Project_Explorers is

   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Project_Explorer is access all Project_Explorer_Record'Class;

   procedure Gtk_New
     (Explorer : out Project_Explorer;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new explorer.
   --  On each update, and since the list of withed projects can not changed,
   --  the open/close status of all the project nodes is kept.

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  You should connect to the "context_changed" signal in the kernel to get
   --  report on selection changes.
   --  </signals>

private
   type Node_Types is
     (Project_Node,
      Extends_Project_Node,
      Directory_Node,
      Obj_Directory_Node,
      File_Node,
      Category_Node,
      Entity_Node,
      Modified_Project_Node);
   --  The kind of nodes one might find in the tree

   subtype Real_Node_Types is Node_Types range Project_Node .. Entity_Node;

   type Pixmap_Array is array (Node_Types) of Gdk.Pixmap.Gdk_Pixmap;
   type Mask_Array   is array (Node_Types) of Gdk.Bitmap.Gdk_Bitmap;
   type Pixbuf_Array is array (Node_Types) of Gdk.Pixbuf.Gdk_Pixbuf;

   type Append_Directory_Idle_Data;
   type Append_Directory_Idle_Data_Access is access Append_Directory_Idle_Data;
   --  Custom data for the asynchronous fill function.

   package File_Append_Directory_Timeout is
      new Gtk.Main.Timeout (Append_Directory_Idle_Data_Access);

   procedure Free (D : in out Gtk.Main.Timeout_Handler_Id);

   package Timeout_Id_List is new Generic_List (Gtk.Main.Timeout_Handler_Id);

   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with record
      Scenario      : Scenario_Views.Scenario_View;
      Tree          : Gtk.Ctree.Gtk_Ctree;

      Notebook      : Gtk.Notebook.Gtk_Notebook;
      Kernel        : Glide_Kernel.Kernel_Handle;
      Open_Pixmaps  : Pixmap_Array;
      Close_Pixmaps : Pixmap_Array;

      Open_Pixbufs  : Pixbuf_Array;
      Close_Pixbufs : Pixbuf_Array;
      --  ??? We need pixbufs for the Tree_View and pixmaps for the CTree...

      Open_Masks    : Mask_Array;
      Close_Masks   : Mask_Array;

      Expand_Id     : Gtk.Handlers.Handler_Id;
      --  The signal for the expansion of nodes in the project view

      Old_Selection : Gtk.Ctree.Gtk_Ctree_Node;
      --  Memorizes the node that was selected at the beginning of a
      --  refresh. It is used to restore the selection at the end of the
      --  refresh. It needs to be stored in this record, so that if this node
      --  is removed then we simply do not try to restore the selection

      --  The following fields are for the File view.
      File_Tree     : Gtk.Tree_View.Gtk_Tree_View;
      File_Model    : Gtk.Tree_Store.Gtk_Tree_Store;
      Expanding     : Boolean := False;

      Fill_Timeout_Ids : Timeout_Id_List.List;
      --  ??? This is implemented as a list of handlers instead of just one
      --  handler, in case the fill function should call itself recursively :
      --  to be investigated.
   end record;
end Project_Explorers;
