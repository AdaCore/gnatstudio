------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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
with Ada.Finalization;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;  use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;    use Gtk.Tree_Model_Sort;
with Glib;                   use Glib;
with Glib.Main;              use Glib.Main;
private with Glib.Object;
private with System;

package Gtkada.Tree_View is

   type Tree_View_Record is new Gtk_Tree_View_Record with private;
   type Tree_View is access all Tree_View_Record'Class;

   type Tree_View_Capability_Type is
     (Sortable, Filtered, Filtered_And_Sortable);
   --  Type enumerating the possible capabilities for Gtkada.Tree_View
   --  widgets.
   --
   --  . Sortable: Simple tree view. Sorting on columns can be enabled via the
   --    Gtk.Tree_View_column.Sort_Column_Id procedure.
   --
   --  . Filtered: Filtered tree view. Sorting on columns is disabled on this
   --    kind of tree views.
   --
   --  . Filtered_And_Sortable: A tree view that can be filtered and also
   --    sorted on its columns. Sorting on columns can be enabled via the
   --    Gtk.Tree_View_column.Sort_Column_Id procedure.

   overriding procedure Expand_All (Self : not null access Tree_View_Record);

   procedure Gtk_New
     (Widget          : out Tree_View;
      Column_Types    : Glib.GType_Array;
      Capability_Type : Tree_View_Capability_Type := Sortable);
   procedure Initialize
     (Widget           : access Tree_View_Record'Class;
      Column_Types     : Glib.GType_Array;
      Capability_Type  : Tree_View_Capability_Type := Sortable;
      Set_Visible_Func : Boolean := False);
   --  Create a new Tree_View with column types given by Column_Types.
   --  All callbacks set on the view (collapse_row, ...) will receive a
   --  Filter_Iter, which needs to be converted to an iterator on the user
   --  model via one of the functions below.
   --
   --  Capability_Type is used to determine which capabilities the tree view
   --  will have (e.g: sorting, filtering).
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
   --  The data model.

   function Filter
     (Self : not null access Tree_View_Record) return Gtk_Tree_Model_Filter
     with Inline;
   --  Optional view filter

   function Sortable_Model
     (Self : not null access Tree_View_Record) return Gtk_Tree_Model_Sort
     with Inline;
   --  Optional sortable model

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
        (Self           : not null access Tree_Record'Class;
         Status         : out Expansion_Status;
         Save_Scrolling : Boolean := True);
      procedure Set_Expansion_Status
        (Self   : not null access Tree_Record'Class;
         Status : Expansion_Status;
         Collapse_All_First : Boolean := True);
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

      type Detached_Model is new Ada.Finalization.Limited_Controlled
         with private;
      type Detached_Model_Access is access all Detached_Model'Class;
      overriding procedure Finalize (Self : in out Detached_Model);

      function Tree (Self : Detached_Model) return access Tree_Record'Class
        with Inline;
      --  Return the tree associated with Self.
      --  It returns null if the tree has been destroyed in between

      function Detach_Model_From_View
         (Self           : not null access Tree_Record'Class;
          Freeze         : Boolean := True;
          Save_Expansion : Boolean := True;
          Save_Scrolling : Boolean := True)
         return Detached_Model;
      --  Temporarily detach the model from the view.
      --  This results in significant performance improvement when adding lots
      --  of rows. When the resulting object goes out of scope, the view is
      --  automatically reattached.
      --  It is safe to nest calls (nested calls will have no effect).
      --  Note that while detached, you cannot perform operations like
      --  expanding a node for instance.
      --
      --  If Freeze is true, sorting is also suspended and restored.
      --
      --  When the model is reattached, the expansion status is lost. If
      --  Save_Expansion is True, the nodes will be re-expanded as they were
      --  before.
      --
      --  This works even if the tree is destroyed while detached. In this case
      --  it will simply never be reattached.

      procedure Set_Expanded
        (Status    : in out Expansion_Status;
         Row       : Id;
         Expanded  : Boolean := True);
      procedure Set_Expanded
        (Status    : in out Detached_Model;
         Row       : Id;
         Expanded  : Boolean := True);
      --  Force a saved expansion status for a row.
      --  This doesn't impact the actual tree, only when Set_Expansion_Status
      --  is called.
      --  The row doesn't have to exist.

   private
      package Id_Sets is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => Id,
         Hash                => Hash,
         Equivalent_Elements => "=",
         "="                 => "=");

      type Expansion_Status is record
         Expanded      : Id_Sets.Set;
         Selection     : Id_Sets.Set;

         Has_Scroll_Info : Boolean := False;
         Scroll_Y : Gtk_Tree_Path;
         --  Top visible row
      end record;

      type Detached_Data is record
         Tree           : access Tree_Record'Class;
         Was_Detached   : Boolean := True;
         Sort_Col       : Gint := -1;

         Save_Expansion : Boolean := False;
         Expansion      : Expansion_Status;
      end record;
      type Detached_Data_Access is access Detached_Data;

      type Detached_Model is new Ada.Finalization.Limited_Controlled with
         record
            Data : Detached_Data_Access;
         end record
      with Warnings => Off;  --  avoid warnings on unused instances

      function Tree (Self : Detached_Model) return access Tree_Record'Class
        is (if Self.Data = null then null else Self.Data.Tree);

      procedure On_Tree_Destroyed
        (Data   : System.Address;
         Tree   : System.Address);
      pragma Convention (C, On_Tree_Destroyed);
      On_Tree_Destroyed_Access : constant Glib.Object.Weak_Notify :=
        On_Tree_Destroyed'Access;
      --  Called when the tree is destroyed while it is detached from the
      --  model.

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
   --  It will often be a good idea to use Detach_Model_From_View when
   --  adding lots of children. This isn't done automatically since this also
   --  prevents manually expanding rows.

   procedure Add_Row_Children
     (Self       : not null access Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter);
   --  Calls Add_Children on the row, if necessary (and if children are not
   --  already known). The view doesn't need to be attached to the model.

   procedure Remove_Dummy_Child
     (Self       : not null access Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter);
   --  Remove a dummy child node if there's one.
   --  Such nodes are inserted to make the parent node expandable.
   --  In general, you don't need to call this function, call Add_Row_Children
   --  instead (or let Add_Children be called automatically.

   -------------
   -- Editing --
   -------------
   --  Interactive editing

   type Edited_Column_Id is new Gint;

   procedure On_Edited
     (Self        : not null access Tree_View_Record;
      Store_Iter  : Gtk_Tree_Iter;
      View_Column : Edited_Column_Id;
      Text        : String) is null;
   --  Called when interactive editing finishes.
   --  The column is provided as a way to distinguish when multiple cells are
   --  editable in a given row.

   procedure Start_Editing
     (Self        : not null access Tree_View_Record'Class;
      Render      : not null access Gtk_Cell_Renderer_Text_Record'Class;
      Store_Iter  : Gtk_Tree_Iter := Null_Iter;
      View_Column : Edited_Column_Id := 0);
   --  Start editing the contents of the render.
   --  View_Column is not a column in the model, but a physical column in the
   --  view.
   --  Editing starts in an idle, in case this was started from a menu.
   --  You must override On_Edited to perform an actual change in the model.
   --
   --  If Store_Iter is unspecified, the current selection is used instead.
   --  If it is specified, the current selection is modified.

   ----------------
   -- Converters --
   ----------------
   --  These various subprograms convert between the user model, the filter
   --  model and the sortable model. It is valid (and even encouraged) to call
   --  them even if the tree view wasn't created with a filter or a sortable
   --  model. In this case, the subprograms do nothing.

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

   function Convert_To_Sortable_Model_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts a model store iter into sortable model iter

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

   function Get_Store_Path_For_Filter_Iter
     (Self        : access Tree_View_Record'Class;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
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

      Sortable_Model : Gtk_Tree_Model_Sort;
      --  Optional sort model

      Column_Extra : Glib.Gint := -1;
      --  The extra column added to the model, which stores information like
      --  whether a node was expanded or filtered by the model.

      Target_Path_For_Scroll : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Background_Scroll_Id   : Glib.Main.G_Source_Id := No_Source_Id;
      --  Ensure this path is visible, in an idle.

      Lock  : Boolean := False;
      --  Whether the expand callbacks should do anything.
      --  It's useful to set this lock to True when the user wants to
      --  control expansion himself.

      Filter_Disabled : Boolean := False;
      --  If true, filtering is temporarily disabled. This is necessary during
      --  a drag-and-drop operation.

      Propagate_Filtered_Status : Boolean := True;
      --  See Set_Propagate_Filtered_Status

      Row_Expanded_Callback_ID : Handler_Id;
   end record;

   function Model
     (Self : not null access Tree_View_Record) return Gtk_Tree_Store
   is
     (Self.Model);
   function Filter
     (Self : not null access Tree_View_Record) return Gtk_Tree_Model_Filter
   is
     (Self.Filter);
   function Sortable_Model
     (Self : not null access Tree_View_Record) return Gtk_Tree_Model_Sort
   is
     (Self.Sortable_Model);

end Gtkada.Tree_View;
