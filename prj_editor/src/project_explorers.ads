-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package groups a tre (that shows projects, directories, files, and
--  entities in the files), and the display of the scenario variables that the
--  user can modify.
--  This widget also knows how to save its state to an Ada stream, and re-read
--  a previously saved configuration.

with Glide_Kernel;
with Scenario_Views;
with Vsearch_Ext;
with Gtk.Ctree;
with Gdk.Pixmap;
with Gdk.Bitmap;

with Gtk.Box;

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
      Modified_Project_Node,
      Directory_Node,
      Obj_Directory_Node,
      File_Node,
      Category_Node,
      Entity_Node);
   --  The kind of nodes one might find in the tree

   type Pixmap_Array is array (Node_Types) of Gdk.Pixmap.Gdk_Pixmap;
   type Mask_Array   is array (Node_Types) of Gdk.Bitmap.Gdk_Bitmap;

   type Project_Explorer_Record is new Gtk.Box.Gtk_Box_Record with record
      Scenario      : Scenario_Views.Scenario_View;
      Tree          : Gtk.Ctree.Gtk_Ctree;
      Search        : Vsearch_Ext.Vsearch_Extended;
      Kernel        : Glide_Kernel.Kernel_Handle;
      Open_Pixmaps  : Pixmap_Array;
      Close_Pixmaps : Pixmap_Array;
      Open_Masks    : Mask_Array;
      Close_Masks   : Mask_Array;

      Old_Selection : Gtk.Ctree.Gtk_Ctree_Node;
      --  Memorizes the node that was selected at the beginning of a
      --  refresh. It is used to restore the selection at the end of the
      --  refresh. It needs to be stored in this record, so that if this node
      --  is removed then we simply do not try to restore the selection
   end record;
end Project_Explorers;
