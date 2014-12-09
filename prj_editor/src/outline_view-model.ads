------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;
with GPS.Search;

with Glib;                       use Glib;
with Glib.Values;                use Glib.Values;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model; use Gtkada.Abstract_Tree_Model;

with Language;               use Language;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

--  This package implements a sortable hierarchical outline model, based on the
--  construct trees.

private package Outline_View.Model is

   Spec_Pixbuf_Column  : constant := 0;
   Display_Name_Column : constant := 1;
   Body_Pixbuf_Column  : constant := 2;

   type Outline_Model_Record
     is new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
       with private;
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
      Hide_Objects      : Boolean := False;
      Hide_Declarations : Boolean := False;
      Hide_Tasks        : Boolean := False;
      Hide_Withes       : Boolean := False;
      Show_Profile      : Boolean := False;
      Sorted            : Boolean;
      Group_Spec_And_Body : Boolean := False;
      Flat_View           : Boolean := False;
   end record;
   --  Group_Spec_And_Body indicates whether to group the spec and body for
   --  a given entity on the same line. This doesn't force a refresh of the
   --  model.
   --  Flat_View indicates whether to display all entities in a flat view, or
   --  hierarchically

   procedure Set_Filter
     (Model   : not null access Outline_Model_Record'Class;
      Pattern : GPS.Search.Search_Pattern_Access);
   --  Filters the contents of the model. This does not refresh the model.

   procedure Set_Tree
     (Model    : not null access Outline_Model_Record'Class;
      Sem_Tree : Semantic_Tree'Class;
      Filter   : Tree_Filter);

   function Get_Tree (Model : Outline_Model) return Semantic_Tree'Class;
   --  Return the file modelized by this model.
   --  Setting the file forces a refresh of the model.

   type Sorted_Node is private;

   type Sorted_Node_Access is access all Sorted_Node;
   pragma No_Strict_Aliasing (Sorted_Node_Access);

   function Get_Info (S : Sorted_Node) return Semantic_Node_Info;

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

   function Get_Info
     (Self   : not null access Outline_Model_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Gint := Display_Name_Column) return Semantic_Node_Info;
   --  Return the entity designed by this iterator.
   --  If Column is set to Body_Pixbuf_Column, the "body" of the entity is
   --  returned.

   procedure Free (Model : access Outline_Model_Record);
   --  Free the memory associated to this model content. The model won't be
   --  usable afterwards (this doesn't unchecked free the model itself).

   procedure File_Updated
     (Model    : access Outline_Model_Record);
   --  In order to keep the model up to date with the tree, this function
   --  should be called every time the construct tree is changed.

   function Get_Path_Enclosing_Location
     (Model        : access Outline_Model_Record;
      Line, Column : Integer) return Gtk_Tree_Path;
   --  Return the closest path enclosing the {line, column} from the model

private

   function "<" (Left, Right : Sorted_Node_Access) return Boolean;

   package Sorted_Node_Vector is new
     Ada.Containers.Vectors (Positive, Sorted_Node_Access);

   package Sorted_Node_Set is new
     Ada.Containers.Ordered_Sets (Sorted_Node_Access, "<");

   type Sorted_Node is record
      Spec_Info, Body_Info : Semantic_Node_Info := No_Node_Info;
      --  A node might be associated to up to two construct entities (the
      --  spec and the body of an Ada entity). We keep pointers to these so
      --  that the node is only freed when both entities are freed, but kept
      --  if only one of them changes (for instance adding a parameter to a
      --  spec creates a new construct entity, and thus a new node, but we
      --  should still preserve the original node for the body).
      --
      --  Either of these might be null, but not both. When not grouping
      --  spec and body, the body will always be null.

      Prev, Next   : Sorted_Node_Access;
      Parent : Sorted_Node_Access;

      Index_In_Siblings : Integer := -1;
      --  This is used to build a Gtk_Tree_Path from a node

      Model : Outline_Model;

      Children : Sorted_Node_Set.Set;
   end record;

   function Get_Info (S : Sorted_Node) return Semantic_Node_Info is
      (if S.Spec_Info /= No_Node_Info then S.Spec_Info else S.Body_Info);

   function Hash (H : Hash_Type) return Hash_Type is (H);

   package Sem_To_Tree_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Sorted_Node_Access, Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Outline_Model_Record is new Gtk_Abstract_Tree_Model_Record with record
      Semantic_Tree     : Sem_Tree_Holders.Holder :=
        Sem_Tree_Holders.Empty_Holder;

      Filter            : Tree_Filter;
      Filter_Pattern    : GPS.Search.Search_Pattern_Access := null;

      Phantom_Root      : aliased Sorted_Node;
      --  This is a 'dummy' root, not in the model. Actual roots are children
      --  of that node.
      Root_With         : Sorted_Node_Access;
      --  Container for with clauses.

      Sem_To_Tree_Nodes : Sem_To_Tree_Maps.Map;
   end record;

end Outline_View.Model;
