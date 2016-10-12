------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

--  This widget provides various enhancements on top of the standard
--  Gtk_Tree_View widget.
--
--     * There is an optional filter model.
--       This can be used to dynamically filter which information is displayed
--       and is used to support the filter fields in the views local toolbars.
--
--       You can either manipulate the filter model manually (by settings its
--       visibilty_func callback yourself), or use the built-in support which
--       uses the Is_Visible primitive operation and properly takes care of
--       making parent rows visible when a child row should be visible.
--
--       Various subprograms are provided to ease the translation of iterators
--       from one model to the other.
--
--     * The view automatically remembers the expanded state of children when
--       the user opens and closes a parent node (gtk+ by default would reset
--       all children to a collapsed state).
--
--       Support is provided for saving that state in an external data
--       structure, and restoring it. This helps updating the contents of the
--       model since your code does not need to deal with expansion.
--
--     * Support for lazily adding nodes
--       Not all contents of the model is known at creation time (or might be
--       too expansive to add if the model is large). Instead, it is possible
--       to indicate that a node might have children (so that an expansion
--       arrow is visible), and then the children rows will be added lazily
--       when and if the user expands the node.
--
--     * Support for reorderable rows
--       The filter model we use is compatible with a Gtk_Tree_View that has
--       reorderable rows. For this to work, it properly forwards the events
--       to the child model, and temporarily unsets the filter during a drag
--       operation.

with Ada.Containers.Indefinite_Hashed_Sets;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Tree_Store;        use Gtk.Tree_Store;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Glib;                  use Glib;

package Gtkada.Tree_View is

   type Tree_View_Record is new Gtk_Tree_View_Record with private;
   type Tree_View is access all Tree_View_Record'Class;

   procedure Gtk_New
     (Widget       : out Tree_View;
      Column_Types : Glib.GType_Array;
      Filtered     : Boolean := False);
   procedure Initialize
     (Widget           : access Tree_View_Record'Class;
      Column_Types     : Glib.GType_Array;
      Filtered         : Boolean;
      Set_Visible_Func : Boolean := False);
   --  Create a new Tree_View with column types given by Column_Types.
   --  All callbacks set on the view (collapse_row, ...) will receive a
   --  Filter_Iter, which needs to be converted to an iterator on the user
   --  model via one of the functions below.
   --
   --  If Set_Visible_Func is true and a filter is set, then the visibility of
   --  a row is given by an extra column in the model. This column can be
   --  recomputed by calling Refilter below. This should be False if you
   --  intend to set your own more complex visibility function.
   --
   --  ??? This function creates both model and view, so can't easily create
   --  multiple views of the same model.

   function Model
     (Self : not null access Tree_View_Record) return Gtk_Tree_Store
     with Inline;
   --  The data model

   function Filter
     (Self : not null access Tree_View_Record) return Gtk_Tree_Model_Filter
     with Inline;
   --  Optional view filter

   ---------------
   -- Expansion --
   ---------------

   generic
      type Tree_Record is new Tree_View_Record with private;
      type Id (<>) is private;

      with function Get_Id
        (Self : not null access Tree_Record'Class;
         Row  : Gtk_Tree_Iter) return Id;
      --  Given a row in the tree, returns a unique id for it, which remains
      --  valid when the model is updated

      with function Hash (Element : Id) return Ada.Containers.Hash_Type;
      with function "=" (Left, Right : Id) return Boolean is <>;

   package Expansion_Support is

      type Expansion_Status is private;
      procedure Get_Expansion_Status
        (Self   : not null access Tree_Record'Class;
         Status : out Expansion_Status);
      procedure Set_Expansion_Status
        (Self   : not null access Tree_Record'Class;
         Status : Expansion_Status);
      --  Retrieve, in the opaque Expansion_Status structure, the list of
      --  expanded nodes, and apply it again later on.
      --  This is useful when you need to update the contents of a tree:
      --       - Get_Expansion_Status
      --       - clear model
      --       - add all rows
      --       - Set_Expansion_Status
      --
      --  This requires having a unique id for each row, which remains valid
      --  across refresh of the model.
      --  This also preserves the current scrolling position

   private
      package Id_Sets is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => Id,
         Hash                => Hash,
         Equivalent_Elements => "=",
         "="                 => "=");

      type Expansion_Status is record
         Expanded : Id_Sets.Set;

         Has_Scroll_Info : Boolean := False;
         Scroll_Y : Gtk_Tree_Path;
         --  Top visible row
      end record;
   end Expansion_Support;

   -------------
   -- Filters --
   -------------

   procedure Refilter
     (Self    : not null access Tree_View_Record'Class);
   --  For each row of the model, calls Self.Is_Visible to check whether it
   --  should be made visible. The parent rows are automatically made visible
   --  as well.
   --  This only has an effect if a filter has been set.
   --  This procedure needs to be called every time the model is repopulated if
   --  a filter pattern is currently applied to the view.

   procedure Set_Propagate_Filtered_Status
     (Self      : not null access Tree_View_Record;
      Propagate : Boolean := True);
   --  By default, filtering is done by calling Is_Visible on each child node,
   --  and if any of them is kept visible, then the parent is also kept
   --  visible. But this propagation to the parent might be slow depending on
   --  the depth of the tree.
   --  When you combine filtering with lazy contents, you will have to
   --  independently compute whether a node should be visible (since the tree
   --  view does not know which children will eventually be added). In such a
   --  case, propagating the visibility to the parent is just wasted time, and
   --  can be disabled via this procedure.
   --  After changing this, you might need to call Refilter.

   function Is_Visible
     (Self    : not null access Tree_View_Record;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is (True);
   --  Whether the given row in the model should be made visible (along with
   --  its parents)
   --  Iter applies to Self.Model

   -------------------
   -- Lazy contents --
   -------------------

   procedure Set_Might_Have_Children
     (Self    : not null access Tree_View_Record'Class;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Indicates that the row might have children, so can be expanded by the
   --  user. When it is actually expanded, Add_Children_Lazily will be called
   --  to insert the actual children.
   --  This has no effect if children were already inserted (and thus
   --  Add_Children_Lazily will not be called).

   procedure Add_Children
     (Self       : not null access Tree_View_Record;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is null;
   --  This procedure is called automatically when a row for which
   --  Set_Might_Have_Children was called is opened.

   ----------------
   -- Converters --
   ----------------
   --  These various subprograms convert between the user model and the filter
   --  model. It is valid (and even encouraged) to call them even if the tree
   --  view wasn't created with a filter. In this case, the subprograms do
   --  nothing.

   function Convert_To_Store_Iter
     (Self        : access Tree_View_Record'Class;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts model filter iter into model store iter

   function Convert_To_Filter_Iter
     (Self        : access Tree_View_Record'Class;
      Store_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts model store iter into filter store iter

   function Get_Store_Iter_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts model filter path into model store iter

   function Get_Store_Path_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Returned value must be freed by caller

   function Get_Filter_Path_For_Store_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Converts model store iter into model filter path
   --  Returned value must be freed by caller

private
   type Tree_View_Record is new Gtk_Tree_View_Record with record
      Model : Gtk_Tree_Store;
      --  The data model.

      Filter : Gtk_Tree_Model_Filter;
      --  Optional view filter

      Column_Extra : Glib.Gint := -1;
      --  The extra column added to the model, which stores information like
      --  whether a node was expanded or filtered by the model.

      Lock  : Boolean := False;
      --  Whether the expand callbacks should do anything.
      --  It's useful to set this lock to True when the user wants to
      --  control expansion himself.

      Filter_Disabled : Boolean := False;
      --  If true, filtering is temporarily disabled. This is necessary during
      --  a drag-and-drop operation.

      Propagate_Filtered_Status : Boolean := True;
      --  See Set_Propagate_Filtered_Status
   end record;

   function Model
     (Self : not null access Tree_View_Record) return Gtk_Tree_Store
     is (Self.Model);
   function Filter
     (Self : not null access Tree_View_Record) return Gtk_Tree_Model_Filter
     is (Self.Filter);

end Gtkada.Tree_View;
