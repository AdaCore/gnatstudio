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

with Glib;
with Gtk.Ctree;
with Gdk.Pixmap;
with Gdk.Bitmap;

with Types;
with Prj;

with Prj_Manager;

package Project_Trees is

   type Project_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Project_Tree is access all Project_Tree_Record'Class;

   type Name_Id_Array is array (Natural range <>) of Types.Name_Id;

   procedure Gtk_New
     (Tree        : out Project_Tree;
      Manager     : access Prj_Manager.Project_Manager_Record'Class;
      Columns     : Glib.Gint;
      Tree_Column : Glib.Gint := 0);
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

   function Get_Selected_Directories
     (Tree    : access Project_Tree_Record;
      Project : Prj.Project_Id) return Name_Id_Array;
   --  Return the list of selected directories for Project. Note that this
   --  doesn't include any imported projects.
   --  This returns an empty array if no directory are selected.
   --  It is your responsability to provide a correct semantic in your
   --  application if Project wasn't selected.

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
      Manager            : Prj_Manager.Project_Manager;
      Open_Pixmaps       : Pixmap_Array;
      Close_Pixmaps      : Pixmap_Array;
      Open_Masks         : Mask_Array;
      Close_Masks        : Mask_Array;
   end record;
end Project_Trees;
