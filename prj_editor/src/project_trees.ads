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

--  <description>
--
--  This package provides a tree widget that lists a project and all its
--  dependencies (and the dependencies thereof...). It also includes the
--  directories of the project.
--
--  </description>

with Gtk.Ctree;
with Gdk.Pixmap;
with Gdk.Bitmap;

with Types;
with Prj;

with Glide_Kernel;

package Project_Trees is

   type Project_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Project_Tree is access all Project_Tree_Record'Class;

   procedure Gtk_New
     (Tree        : out Project_Tree;
      Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new tree.
   --  This tree displays the current view of Manager, and is refreshed
   --  automatically every time this view is updated.
   --
   --  On each update, and since the list of withed projects can not changed,
   --  the open/close status of all the project nodes is kept.

   ---------------
   -- Selection --
   ---------------

   function Get_Selected_Project (Tree : access Project_Tree_Record)
      return Prj.Project_Id;
   --  Return the selected project.
   --  In case a directory is selected in the tree, this returns the project
   --  this directory belongs to.

   function Get_Selected_Directory
     (Tree    : access Project_Tree_Record;
      Project_Filter : Prj.Project_Id := Prj.No_Project)
      return Types.String_Id;
   --  Return the name of the selected directory for Project.
   --  It returns No_String if a project node is selected, and the name of the
   --  directory containing the file is a File_Node, Category_Node or
   --  Entity_Node is selected.
   --  No_Name is also returned if the directory is not in Project_Filter (and
   --  Project_Filter is not null).

   function Get_Selected_File
     (Tree    : access Project_Tree_Record) return Types.String_Id;
   --  Return the name of the selected file, or No_Name if no file is selected.
   --  When a project_node or directory_node is selected, No_Name is returned.
   --  When a category node or an entity node is selected, the name of the
   --  containing file is returned.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "tree_select_row" and "tree_unselect_row"
   --    These are the standard Gtk_Ctree signals. You should connect to these
   --    to get information when the selection has changed. Use the subprograms
   --    Get_Selected_Projects and Get_Selected_Dirs to get more information
   --    on the current state of the selection
   --
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


   type Project_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with record
      Kernel             : Glide_Kernel.Kernel_Handle;
      Open_Pixmaps       : Pixmap_Array;
      Close_Pixmaps      : Pixmap_Array;
      Open_Masks         : Mask_Array;
      Close_Masks        : Mask_Array;
   end record;
end Project_Trees;
