-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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

with Ada.Containers.Ordered_Sets;

with GNAT.Strings;               use GNAT.Strings;

with Glib;                       use Glib;
with Glib.Values;                use Glib.Values;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model; use Gtkada.Abstract_Tree_Model;

with Language;               use Language;
with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;

--  This package implements a sortable hierarchical outline model, based on the
--  construct trees.

private package Outline_View.Model is

   Pixbuf_Column       : constant := 0;
   Display_Name_Column : constant := 1;

   type Outline_Model_Record
     is new Gtk_Abstract_Tree_Model_Record with private;
   --  This model represents a structured tree. It contains instances of
   --  Entity_Persisent_Access. Using the update listener callback, we ensure
   --  that these entities are never referenced if they don't exist - so we
   --  don't have to manage their references, they'll get deleted
   --  automatically when irrelevant. We only do that for object that are
   --  explicitelly referenced in the graphical outline, as a defensive code
   --  for the case where they'd stay there longer because of a bug.

   type Outline_Model is access all Outline_Model_Record'Class;

   type Tree_Filter is record
      Hide_Types        : Boolean := False;
      Hide_Declarations : Boolean := False;
      Show_Profile      : Boolean := False;
   end record;

   procedure Init_Model
     (Model     : access Outline_Model_Record'Class;
      Key       : Construct_Annotations_Pckg.Annotation_Key;
      File      : Structured_File_Access;
      Filter    : Tree_Filter;
      Sort      : Boolean;
      Add_Roots : Boolean := False);

   function Get_File (Model : Outline_Model) return Structured_File_Access;
   --  Return the file modelized by this model

   type Sorted_Node is private;

   type Sorted_Node_Access is access all Sorted_Node;

   function Get_Entity
     (Node : Sorted_Node_Access) return Entity_Persistent_Access;
   --  Return the entity designed by this node

   -------------------------------
   --  GtkTreeModel subprograms --
   -------------------------------

   overriding function Get_N_Columns
     (Self : access Outline_Model_Record)
      return Glib.Gint;
   --  See inherited documentation

   overriding function Get_Column_Type
     (Self  : access Outline_Model_Record;
      Index : Glib.Gint) return Glib.GType;
   --  See inherited documentation

   overriding function Get_Iter
     (Self : access Outline_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  See inherited documentation

   overriding function Get_Path
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  See inherited documentation

   overriding procedure Get_Value
     (Self   : access Outline_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  See inherited documentation

   overriding procedure Next
     (Self : access Outline_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  See inherited documentation

   overriding function Children
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  See inherited documentation

   overriding function Has_Child
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  See inherited documentation

   overriding function N_Children
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;
   --  See inherited documentation

   overriding function Nth_Child
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  See inherited documentation

   overriding function Parent
     (Self  : access Outline_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  See inherited documentation

   function Get_Entity (Iter : Gtk_Tree_Iter) return Entity_Persistent_Access;
   --  Return the entity designed by this iterator

   procedure Free (Model : access Outline_Model_Record);
   --  Free the memory associated to this model content. The model won't be
   --  usable afterwards (this doesn't unchecked free the model itself).

   procedure File_Updated
     (Model    : access Outline_Model_Record;
      File     : Structured_File_Access;
      Old_Tree : Construct_Tree;
      Kind     : Update_Kind);
   --  In order to keep the model up to date with the tree, this function
   --  should be called every time the construct tree is changed.

   function Get_Path_Enclosing_Location
     (Model        : access Outline_Model_Record;
      Line, Column : Integer) return Gtk_Tree_Path;
   --  Return the closest path enclosing the {line, column} from the model

private

   function "<" (Left, Right : Sorted_Node_Access) return Boolean;

   package Sorted_Node_Set is new
     Ada.Containers.Ordered_Sets (Sorted_Node_Access);

   type Order_Kind_Type is (Alphabetical, Positional);

   type Sorted_Node is record
      Entity      : Entity_Persistent_Access;
      Prev, Next  : Sorted_Node_Access;
      First_Child : Sorted_Node_Access;
      Last_Child  : Sorted_Node_Access;
      Parent      : Sorted_Node_Access;

      Index_In_Siblings : Integer := -1;
      N_Children        : Integer := -1;

      Ordered_Index : Sorted_Node_Set.Set;
      Order_Kind    : Order_Kind_Type;

      --  We need to remove the following things in order to still be able
      --  to remove the node after it has been deleted from the construct
      --  tree.
      Category     : Language_Category;
      Name         : String_Access;
      Sloc         : Source_Location;
   end record;

   type Outline_Model_Record is new Gtk_Abstract_Tree_Model_Record with record
      Annotation_Key : Construct_Annotations_Pckg.Annotation_Key;
      File           : Structured_File_Access;
      Filter         : Tree_Filter;
      Sorted         : Boolean;

      Phantom_Root : aliased Sorted_Node;
      --  This is a 'dummy' root, not in the model. Actual roots are children
      --  of that node.
   end record;

end Outline_View.Model;
