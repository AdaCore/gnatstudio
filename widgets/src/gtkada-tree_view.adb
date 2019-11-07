------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Gdk.Drag_Contexts;    use Gdk.Drag_Contexts;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtk.Cell_Renderer;    use Gtk.Cell_Renderer;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Selection_Data;   use Gtk.Selection_Data;
with Gtk.Tree_Drag_Dest;   use Gtk.Tree_Drag_Dest;
with Gtk.Tree_Row_Reference; use Gtk.Tree_Row_Reference;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Types;         use Gtkada.Types;
with System;               use System;
with GNATCOLL.Traces;      use GNATCOLL.Traces;

package body Gtkada.Tree_View is
   Me : constant Trace_Handle := Create ("GPS.WIDGETS.TREE_VIEW", Off);

   ------------------
   -- Filter_Model --
   ------------------

   Filter_Model_With_Dnd_Klass : aliased Ada_GObject_Class :=
     Uninitialized_Class;
   --  A special implementation of Gtk.Tree_Model_Filter, which also
   --  implements the drag-and-drop related interfaces to the underlying
   --  model.

   function Get_Filter_Model_Type return GType;
   --  Support for creating a new gtk+ class for Filter_Model_With_Dnd_Klass

   type Gtkada_Tree_Model_Filter_Record is new Gtk_Tree_Model_Filter_Record
     with null record;
   type Gtkada_Tree_Model_Filter is
     access all Gtkada_Tree_Model_Filter_Record'Class;

   procedure Init_Tree_Drag_IFace
     (IFace : Tree_Drag_Dest_Interface_Descr;
      Data  : System.Address)
     with Convention => C;
   --  Initialize the interfaces that the tree model implements

   function On_Drag_Data_Received_Proxy
     (Self           : Gtk_Tree_Drag_Dest;
      Dest           : System.Address;
      Selection_Data : System.Address) return Glib.Gboolean
     with Convention => C;
   function On_Row_Drop_Possible_Proxy
     (Self           : Gtk_Tree_Drag_Dest;
      Dest           : System.Address;
      Selection_Data : System.Address) return Glib.Gboolean
     with Convention => C;
   --  Forward events to the child model

   package Implements_Gtk_Tree_Drag_Dest is new Glib.Types.Implements
     (Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest,
      Gtkada_Tree_Model_Filter_Record,
      Gtkada_Tree_Model_Filter);

   package Tree_Sources is new Glib.Main.Generic_Sources (Tree_View);
   function On_Idle_Scroll (Self : Tree_View) return Boolean;
   --  Scroll the tree in a timeout, to make Self.Target_Path_For_Scroll
   --  visible.
   --  This cannot be done immediately in case we have detached and then
   --  reattached the view, since gtk+ cannot immediately compute the visible
   --  area and thus ends up scrolling too much.

   function On_User_Scroll (Self : Tree_View) return Boolean;
   --  Scroll the tree in a timeout using the scrolling data stored in
   --  Self.User_Scroll_Data.

   procedure Force_Expansion (Self : not null access Tree_View_Record'Class);
   --  Force the expansion of the nodes that are flagged as expanded.
   --  This is useful after refiltering, since Gtk seems to collapse
   --  automatically the nodes that are made invisible.

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the tree view is being destroyed

   procedure Row_Expanded_Callback
     (Widget      : access Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Callback for the "row_expanded" signal.

   procedure Row_Collapsed_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Callback for the "row_collapsed" signal.

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter);   --  relative to Widget.Model always
   --  Callback for the "row_inserted" signal.

   package Set_Visible_Funcs is new Set_Visible_Func_User_Data
     (User_Data_Type => Tree_View);
   function Is_Visible
     (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self        : Tree_View) return Boolean;
   --  Support for filtering the tree model

   -------------------
   -- Drag and drop --
   -------------------

   procedure On_Drag_Begin
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class);
   procedure On_Drag_End
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class);
   --  Called when a drag operation starts or end.
   --  When we use a filter, we must disable filtering during such an
   --  operation, since the selection_data manipulated by gtk+ internally
   --  manipulates paths from the child model which we cannot override.

   -------------
   -- Editing --
   -------------

   type Editing_Data_Record is record
      Tree                   : Tree_View;
      Render                 : Gtk_Cell_Renderer_Text;
      Filter_Path            : Gtk_Tree_Path;
      View_Column            : Edited_Column_Id;
      Edited_Cb, Canceled_Cb : Handler_Id;
   end record;
   type Editing_Data is access all Editing_Data_Record;
   package Rename_Idle is new Glib.Main.Generic_Sources (Editing_Data);
   package Editing_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk_Cell_Renderer_Text_Record, Editing_Data);

   function Start_Editing_Idle (Data : Editing_Data) return Boolean;
   --  Start interactive editing in an idle loop

   procedure On_Edited
     (V           : access Gtk_Cell_Renderer_Text_Record'Class;
      Params      : Glib.Values.GValues;
      Data        : Editing_Data);
   --  Called when a line is edited in the view

   procedure On_Editing_Canceled
     (V    : access Gtk_Cell_Renderer_Text_Record'Class;
      Data : Editing_Data);
   --  Called when interactive editing has finished.

   -----------
   -- Flags --
   -----------

   type Flags is mod 2 ** 8;

   Flag_Is_Expanded : constant Flags := 2 ** 1;
   --  Whether the corresponding row was expanded when its parent was expanded

   Flag_Is_Visible  : constant Flags := 2 ** 2;
   --  Whether the row has been filtered out.

   Flag_Is_Dummy    : constant Flags := 2 ** 3;
   --  Whether this row is a dummy row added so that the parent has an
   --  expansion arrow. Such a row is not meant to be visible to the user ever.

   function Get_Flags
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Flags
     is (Flags (Get_Int (Self.Model, Iter, Self.Column_Extra)))
     with Inline;
   --  Get the flags for the row

   function Get_Flag
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      F    : Flags) return Boolean
     is ((Flags (Get_Int (Self.Model, Iter, Self.Column_Extra)) and F) /= 0)
     with Inline;
   --  Get the value for a specific flag

   procedure Set_Flag
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      F    : Flags) with Inline;
   procedure Clear_Flag
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      F    : Flags) with Inline;
   --   Set or unset a flag on a specific row

   ----------------
   -- Expand_All --
   ----------------

   overriding procedure Expand_All (Self : not null access Tree_View_Record) is
   begin
      --  Block the Row_Expanded_Callback while expanding all the nodes: we
      --  don't want to scroll the view to the last node in this case.

      Handler_Block (Self, Self.Row_Expanded_Callback_ID);
      Gtk_Tree_View_Record (Self.all).Expand_All;
      Handler_Unblock (Self, Self.Row_Expanded_Callback_ID);
   end Expand_All;

   ---------------------
   -- Force_Expansion --
   ---------------------

   procedure Force_Expansion (Self : not null access Tree_View_Record'Class)
   is
      function Expand_Node
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter) return Boolean;

      -----------------
      -- Expand_Node --
      -----------------

      function Expand_Node
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter) return Boolean
      is
         pragma Unreferenced (Model, Path);

         Dummy         : Boolean;
         Sortable_Path : Gtk_Tree_Path;
      begin
         if Get_Flag (Self, Iter, Flag_Is_Expanded) then
            Sortable_Path := Self.Get_Sortable_Path_For_Store_Iter (Iter);
            Dummy := Self.Expand_Row (Sortable_Path, Open_All => False);
            Path_Free (Sortable_Path);
         end if;

         return False;  --  keep iterating
      end Expand_Node;
   begin
      Self.Model.Foreach (Expand_Node'Unrestricted_Access);
   end Force_Expansion;

   ---------------------------------
   -- On_Drag_Data_Received_Proxy --
   ---------------------------------

   function On_Drag_Data_Received_Proxy
     (Self           : Gtk_Tree_Drag_Dest;
      Dest           : System.Address;
      Selection_Data : System.Address) return Glib.Gboolean
   is
      Filter      : constant Gtkada_Tree_Model_Filter :=
        Implements_Gtk_Tree_Drag_Dest.To_Object (Self);
      Child_IFace : constant Gtk_Tree_Drag_Dest :=
        Gtk.Tree_Store.Implements_Gtk_Tree_Drag_Dest.To_Interface
          (Gtk.Tree_Store.Implements_Gtk_Tree_Model.To_Object
             (Filter.Get_Model));
      Path        : constant Gtk_Tree_Path :=
        Filter.Convert_Path_To_Child_Path (From_Object (Dest));
      Result      : Boolean;
   begin
      --  This only works if no filtering is active during this phase, since
      --  the selection_data will contains paths relative to the child model.
      --  This is why we connect to drag-begin to disable filtering

      Result := Gtk.Tree_Drag_Dest.Drag_Data_Received
        (Self           => Child_IFace,
         Dest           => Path,
         Selection_Data => From_Object (Selection_Data));
      Path_Free (Path);
      return (if Result then 1 else 0);
   end On_Drag_Data_Received_Proxy;

   --------------------------------
   -- On_Row_Drop_Possible_Proxy --
   --------------------------------

   function On_Row_Drop_Possible_Proxy
     (Self           : Gtk_Tree_Drag_Dest;
      Dest           : System.Address;
      Selection_Data : System.Address) return Glib.Gboolean
   is
      Filter      : constant Gtkada_Tree_Model_Filter :=
        Implements_Gtk_Tree_Drag_Dest.To_Object (Self);
      Child_IFace : constant Gtk_Tree_Drag_Dest :=
        Gtk.Tree_Store.Implements_Gtk_Tree_Drag_Dest.To_Interface
          (Gtk.Tree_Store.Implements_Gtk_Tree_Model.To_Object
             (Filter.Get_Model));
      Path        : constant Gtk_Tree_Path :=
        Filter.Convert_Path_To_Child_Path (From_Object (Dest));
      Result      : Boolean;
   begin
      --  This only works if no filtering is active during this phase, since
      --  the selection_data will contains paths relative to the child model.

      if Path = Null_Gtk_Tree_Path then
         return 0;
      else
         Result := Gtk.Tree_Drag_Dest.Row_Drop_Possible
           (Self           => Child_IFace,
            Dest_Path      => Path,
            Selection_Data => From_Object (Selection_Data));
         Path_Free (Path);
         return (if Result then 1 else 0);
      end if;
   end On_Row_Drop_Possible_Proxy;

   --------------------------
   -- Init_Tree_Drag_IFace --
   --------------------------

   procedure Init_Tree_Drag_IFace
     (IFace : Tree_Drag_Dest_Interface_Descr;
      Data  : System.Address)
   is
      pragma Unreferenced (Data);
   begin
      Set_Drag_Data_Received
        (Self    => IFace,
         Handler => On_Drag_Data_Received_Proxy'Access);
      Set_Row_Drop_Possible
        (Self    => IFace,
         Handler => On_Row_Drop_Possible_Proxy'Access);
   end Init_Tree_Drag_IFace;

   -------------------
   -- On_Drag_Begin --
   -------------------

   procedure On_Drag_Begin
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class)
   is
      pragma Unreferenced (Context);
      V : constant Tree_View := Tree_View (Self);
   begin
      --  Disable filtering during a drag-and-drop if we are using a filter,
      --  since the selection_data manipulates paths referencing the child
      --  model.
      if V.Filter /= null then
         V.Filter_Disabled := True;
         V.Refilter;
      end if;
   end On_Drag_Begin;

   -----------------
   -- On_Drag_End --
   -----------------

   procedure On_Drag_End
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class)
   is
      pragma Unreferenced (Context);
      V : constant Tree_View := Tree_View (Self);
   begin
      --  Disable filtering during a drag-and-drop if we are using a filter,
      --  since the selection_data manipulates paths referencing the child
      --  model.
      if V.Filter /= null then
         V.Filter_Disabled := False;
         V.Refilter;
      end if;
   end On_Drag_End;

   ---------------------------
   -- Get_Filter_Model_Type --
   ---------------------------

   function Get_Filter_Model_Type return GType is
      Info : GInterface_Info_Access;

   begin
      if Initialize_Class_Record
        (Ancestor     => Gtk.Tree_Model_Filter.Get_Type,
         Class_Record => Filter_Model_With_Dnd_Klass'Access,
         Type_Name    => "Gtkada_Filter_Model",
         Class_Init   => null)
      then
         Info := new GInterface_Info'
           (Interface_Init     => Init_Tree_Drag_IFace'Access,
            Interface_Finalize => null,
            Interface_Data     => System.Null_Address);
         Add_Interface (Filter_Model_With_Dnd_Klass,
                        Gtk.Tree_Drag_Dest.Get_Type,
                        Info);
      end if;
      return Filter_Model_With_Dnd_Klass.The_Type;
   end Get_Filter_Model_Type;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      F    : Flags) is
   begin
      Set (Self.Model, Iter, Self.Column_Extra,
           Gint (Get_Flags (Self, Iter) or F));
   end Set_Flag;

   ----------------
   -- Clear_Flag --
   ----------------

   procedure Clear_Flag
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      F    : Flags) is
   begin
      Set (Self.Model, Iter, Self.Column_Extra,
           Gint (Get_Flags (Self, Iter) and not F));
   end Clear_Flag;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Child_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Self        : Tree_View) return Boolean
   is
      pragma Unreferenced (Child_Model);
   begin
      return Get_Flag (Self, Iter, Flag_Is_Visible);
   end Is_Visible;

   ----------------------------
   -- Convert_To_Filter_Iter --
   ----------------------------

   function Convert_To_Filter_Iter
     (Self        : access Tree_View_Record'Class;
      Store_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk_Tree_Iter
   is
      Filter_Iter : Gtk_Tree_Iter;
   begin
      if Self.Filter /= null and then Store_Iter /= Null_Iter then
         Self.Filter.Convert_Child_Iter_To_Iter (Filter_Iter, Store_Iter);
         return Filter_Iter;
      else
         return Store_Iter;
      end if;
   end Convert_To_Filter_Iter;

   ---------------------------
   -- Convert_To_Store_Iter --
   ---------------------------

   function Convert_To_Store_Iter
     (Self : access Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk_Tree_Iter
   is
      Filter_Iter : Gtk_Tree_Iter;
      Store_Iter  : Gtk_Tree_Iter;
   begin
      --  If the tree view has a sortable model, convert the given iter to
      --  a child one.

      if Self.Sortable_Model /= null and then Iter /= Null_Iter then
         Self.Sortable_Model.Convert_Iter_To_Child_Iter
           (Child_Iter  => Filter_Iter,
            Sorted_Iter => Iter);
      else
         Filter_Iter := Iter;
      end if;

      --  If the tree view has a filtered model, convert the given iter to
      --  a child one.

      if Self.Filter /= null and then Filter_Iter /= Null_Iter then
         Self.Filter.Convert_Iter_To_Child_Iter
           (Child_Iter  => Store_Iter,
            Filter_Iter => Filter_Iter);
      else
         Store_Iter := Filter_Iter;
      end if;

      return Store_Iter;
   end Convert_To_Store_Iter;

   ------------------------------------
   -- Convert_To_Sortable_Model_Iter --
   ------------------------------------

   function Convert_To_Sortable_Model_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Filter_Iter         : Gtk_Tree_Iter;
      Sortable_Model_Iter : aliased Gtk_Tree_Iter;
      Result              : Boolean;
   begin
      if Self.Filter /= null and then Store_Iter /= Null_Iter then
         Self.Filter.Convert_Child_Iter_To_Iter
           (Filter_Iter, Store_Iter);

         if Self.Sortable_Model /= null and then Filter_Iter /= Null_Iter then
            Result := Self.Sortable_Model.Convert_Child_Iter_To_Iter
              (Sortable_Model_Iter'Access, Filter_Iter);
            return (if Result then Sortable_Model_Iter else Filter_Iter);
         else
            return Filter_Iter;
         end if;
      else
         return Store_Iter;
      end if;
   end Convert_To_Sortable_Model_Iter;

   ------------------------------------
   -- Get_Filter_Path_For_Store_Iter --
   ------------------------------------

   function Get_Filter_Path_For_Store_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Self.Filter /= null then
         Self.Filter.Convert_Child_Iter_To_Iter (Iter, Store_Iter);
         return Self.Filter.Get_Path (Iter);
      else
         return Self.Model.Get_Path (Store_Iter);
      end if;
   end Get_Filter_Path_For_Store_Iter;

   --------------------------------------
   -- Get_Sortable_Path_For_Store_Iter --
   --------------------------------------

   function Get_Sortable_Path_For_Store_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Self.Sortable_Model /= null then
         Iter := Self.Convert_To_Sortable_Model_Iter (Store_Iter);
         return Self.Sortable_Model.Get_Path (Iter);
      else
         return Self.Get_Filter_Path_For_Store_Iter (Store_Iter);
      end if;
   end Get_Sortable_Path_For_Store_Iter;

   ------------------------------------
   -- Get_Store_Path_For_Filter_Path --
   ------------------------------------

   function Get_Store_Path_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
   begin
      if Self.Filter /= null then
         return Self.Filter.Convert_Path_To_Child_Path (Filter_Path);
      else
         return Copy (Filter_Path);
      end if;
   end Get_Store_Path_For_Filter_Path;

   ------------------------------------
   -- Get_Store_Path_For_Filter_Iter --
   ------------------------------------

   function Get_Store_Path_For_Filter_Iter
     (Self        : access Tree_View_Record'Class;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Store_Iter : Gtk_Tree_Iter;
   begin
      if Self.Filter /= null then
         Self.Filter.Convert_Iter_To_Child_Iter (Store_Iter, Filter_Iter);
         return Self.Model.Get_Path (Store_Iter);
      else
         return Self.Model.Get_Path (Filter_Iter);
      end if;
   end Get_Store_Path_For_Filter_Iter;

   ------------------------------------
   -- Get_Store_Iter_For_Filter_Path --
   ------------------------------------

   function Get_Store_Iter_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Self.Filter /= null then
         Iter := Self.Filter.Get_Iter (Filter_Path);
         Self.Filter.Convert_Iter_To_Child_Iter (Iter, Iter);

         return Iter;

      else
         return Self.Model.Get_Iter (Filter_Path);
      end if;
   end Get_Store_Iter_For_Filter_Path;

   ------------------------
   -- Get_First_Selected --
   ------------------------

   procedure Get_First_Selected
     (Self   : not null access Tree_View_Record'Class;
      Model  : out Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      List : Gtk_Tree_Path_List.Glist;
      Path : Gtk_Tree_Path;
      use Gtk_Tree_Path_List;
   begin
      Self.Get_Selection.Get_Selected_Rows (Model, List);
      if List /= Null_List then
         Path := Gtk_Tree_Path
           (Gtk_Tree_Path_List.Get_Data (Gtk_Tree_Path_List.First (List)));
         Iter := Gtk.Tree_Model.Get_Iter (Model, Path);
      else
         Iter := Null_Iter;
      end if;
      Free (List);
   end Get_First_Selected;

   ------------------------
   -- Remove_Dummy_Child --
   ------------------------

   procedure Remove_Dummy_Child
     (Self       : not null access Tree_View_Record'Class;
      Store_Iter : Gtk_Tree_Iter)
   is
      Iter : Gtk_Tree_Iter;
   begin
      if Self.Model.Has_Child (Store_Iter) then
         Iter := Self.Model.Children (Store_Iter);
         if Get_Flag (Self, Iter, Flag_Is_Dummy) then
            Self.Model.Remove (Iter);   --  remove dummy node
         end if;
      end if;
   end Remove_Dummy_Child;

   ----------------------
   -- Add_Row_Children --
   ----------------------

   procedure Add_Row_Children
      (Self       : not null access Tree_View_Record'Class;
       Store_Iter : Gtk_Tree_Iter)
   is
      Iter : Gtk_Tree_Iter;
      Row  : Gtk_Tree_Row_Reference;
      Path : Gtk_Tree_Path;
   begin
      if Self.Model.Has_Child (Store_Iter) then
         Iter := Self.Model.Children (Store_Iter);
         if Get_Flag (Self, Iter, Flag_Is_Dummy) then
            Path := Self.Model.Get_Path (Iter);
            Gtk_New (Row, +Self.Model, Path);
            Path_Free (Path);

            Self.Add_Children (Store_Iter);

            --  Remove the dummy node last, so that the expanded status of the
            --  node doesn't change
            Path := Get_Path (Row);
            Iter := Self.Model.Get_Iter (Path);
            Path_Free (Path);
            Row.Free;
            Self.Model.Remove (Iter);
         end if;
      end if;
   end Add_Row_Children;

   --------------------
   -- On_Idle_Scroll --
   --------------------

   function On_Idle_Scroll (Self : Tree_View) return Boolean is
   begin
      if Self.Target_Path_For_Scroll /= Null_Gtk_Tree_Path then
         Gtk_Tree_View_Record (Self.all).Scroll_To_Cell
            (Self.Target_Path_For_Scroll, null, False, 0.0, 0.0);
         Path_Free (Self.Target_Path_For_Scroll);
         Self.Target_Path_For_Scroll := Null_Gtk_Tree_Path;
      end if;
      Self.Background_Scroll_Id := No_Source_Id;
      return False;   --  do not execute again
   end On_Idle_Scroll;

   --------------------
   -- On_User_Scroll --
   --------------------

   function On_User_Scroll (Self : Tree_View) return Boolean is
   begin
      if Self.User_Scroll_Data /= Null_Scroll_Data then
         Gtk_Tree_View_Record (Self.all).Scroll_To_Cell
           (Path      => Self.User_Scroll_Data.Path,
            Column    => Self.User_Scroll_Data.Column,
            Use_Align => Self.User_Scroll_Data.Use_Align,
            Row_Align => Self.User_Scroll_Data.Row_Align,
            Col_Align => Self.User_Scroll_Data.Col_Align);
         Path_Free (Self.User_Scroll_Data.Path);
         Self.User_Scroll_Data := Null_Scroll_Data;
      end if;

      Self.User_Scroll_Id := No_Source_Id;
      return False;
   end On_User_Scroll;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class) is
      Tree : constant Tree_View := Tree_View (Self);
   begin
      if Tree.Background_Scroll_Id /= No_Source_Id then
         Remove (Tree.Background_Scroll_Id);
         Tree.Background_Scroll_Id := No_Source_Id;
      end if;
   end On_Destroy;

   ---------------------------
   -- Row_Expanded_Callback --
   ---------------------------

   procedure Row_Expanded_Callback
     (Widget      : access Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Iter);

      Tree       : constant Tree_View := Tree_View (Widget);
      Iter       : Gtk_Tree_Iter;
      Store_Iter : Gtk_Tree_Iter;
      Path       : Gtk_Tree_Path;
      Dummy      : Boolean;
      L          : constant Boolean := Tree.Lock;

   begin
      if Tree.Lock then
         return;
      end if;

      Tree.Lock := True;

      Store_Iter := Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);

      --  Replace dummy child nodes if needed.
      --  We always assume the dummy child (if any) is the first child

      if Tree.Model.Has_Child (Store_Iter) then
         Set_Flag (Tree, Store_Iter, Flag_Is_Expanded);
         Add_Row_Children (Tree, Store_Iter);

         --  Re-expand existing child nodes as needed

         Iter := Children (Tree.Model, Store_Iter);
         while Iter /= Null_Iter loop
            if Get_Flag (Tree, Iter, Flag_Is_Expanded) then
               Path  := Tree.Get_Filter_Path_For_Store_Iter (Iter);
               Dummy := Expand_Row (Tree, Path, False);
               Path_Free (Path);
            end if;
            Next (Tree.Model, Iter);
         end loop;

         --  Make sure the first child is visible.
         --  We can't call Scroll_To_Cell directly after we detached and
         --  reattached the view (which is likely to have happened in
         --  Add_Children), because gtk+ can't compute the visible range.

         if Tree.Target_Path_For_Scroll /= Null_Gtk_Tree_Path then
            Path_Free (Tree.Target_Path_For_Scroll);
         end if;

         Path := Copy (Filter_Path);
         Down (Path);
         Tree.Target_Path_For_Scroll := Path;

         if Tree.Background_Scroll_Id = No_Source_Id then
            Tree.Background_Scroll_Id :=
               Tree_Sources.Idle_Add (On_Idle_Scroll'Access, Tree);
         end if;
      end if;

      Tree.Lock := L;
   end Row_Expanded_Callback;

   ----------------------------
   -- Row_Collapsed_Callback --
   ----------------------------

   procedure Row_Collapsed_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Iter);
      Tree       : constant Tree_View := Tree_View (Widget);
      Store_Iter : Gtk_Tree_Iter;
   begin
      Store_Iter := Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);
      Clear_Flag (Tree, Store_Iter, Flag_Is_Expanded);
   end Row_Collapsed_Callback;

   ---------------------------
   -- Row_Inserted_Callback --
   ---------------------------

   procedure Row_Inserted_Callback
     (Widget : access GObject_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter)   --  relative to Widget.Model always
   is
      pragma Unreferenced (Path);
      Tree : constant Tree_View := Tree_View (Widget);
   begin
      --  Make the newly inserted rows visible by default since their
      --  visibility needs to be computed from their parents too (and thus
      --  using the Refilter procedure).
      Set_Flag (Tree, Iter, Flag_Is_Visible);
   end Row_Inserted_Callback;

   -----------------------------
   -- Set_Might_Have_Children --
   -----------------------------

   procedure Set_Might_Have_Children
     (Self    : not null access Tree_View_Record'Class;
      Iter    : Gtk_Tree_Iter)
   is
      Dummy_Node : Gtk_Tree_Iter;
   begin
      if not Has_Child (Self.Model, Iter) then
         Self.Model.Append (Iter => Dummy_Node, Parent => Iter);
         Set_Flag (Self, Dummy_Node, Flag_Is_Dummy);
      end if;
   end Set_Might_Have_Children;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget          : out Tree_View;
      Column_Types    : Glib.GType_Array;
      Capability_Type : Tree_View_Capability_Type := Sortable) is
   begin
      Widget := new Tree_View_Record;
      Initialize
        (Widget,
         Column_Types    => Column_Types,
         Capability_Type => Capability_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget           : access Tree_View_Record'Class;
      Column_Types     : Glib.GType_Array;
      Capability_Type  : Tree_View_Capability_Type := Sortable;
      Set_Visible_Func : Boolean := False)
   is
      Params            : GParameter_Array (1 .. 1);
      Real_Column_Types : constant GType_Array := Column_Types & (GType_Int);
      --  Reserve space for extra information

   begin
      --  Tree columns in Gtk+ are numbered starting from 0
      Widget.Column_Extra := Gint (Real_Column_Types'Length - 1);

      Gtk_New (Widget.Model, Real_Column_Types);

      case Capability_Type is
      when Filtered | Filtered_And_Sortable =>
         Init (Params (1).Value, Gtk.Tree_Model.Get_Type);
         Set_Object (Params (1).Value, Widget.Model);
         Params (1).Name := New_String ("child-model");

         Widget.Filter := new Gtkada_Tree_Model_Filter_Record;
         G_New (Widget.Filter, Get_Filter_Model_Type, Params);

         Free (Params);

         Unref (Widget.Model);  --  owned by the filter

         --  Create a Tree_Model_Sort wrapper around the Tree_Model_Filter to
         --  when the tree view needs sorting capabilities.
         if Capability_Type = Filtered_And_Sortable then
            Gtk_New_With_Model (Widget.Sortable_Model, +Widget.Filter);
            Initialize (Gtk_Tree_View (Widget), +Widget.Sortable_Model);
            Unref (Widget.Sortable_Model); --  owned by the widget
         else
            Initialize (Gtk_Tree_View (Widget), +Widget.Filter);
         end if;

         Unref (Widget.Filter);  --  owned by the widget or the sortable model

         Widget.On_Drag_Begin (On_Drag_Begin'Access, Slot => Widget);
         Widget.On_Drag_End (On_Drag_End'Access, Slot => Widget);

         if Set_Visible_Func then
            Set_Visible_Funcs.Set_Visible_Func
              (Widget.Filter, Is_Visible'Access, Data => Widget);
         end if;

      when Sortable =>
         Initialize (Gtk_Tree_View (Widget), +Widget.Model);
         Unref (Widget.Model);  --  owned by the widget
      end case;

      --  We can't connect with After => True, because then the Gtk_Tree_Iter
      --  might have been modified, and in particular would no longer be
      --  relative to the filter model if we use one.

      Widget.Row_Expanded_Callback_ID := Widget_Callback.Connect
        (Widget,
         Gtk.Tree_View.Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (Row_Expanded_Callback'Access));
      Widget.On_Row_Collapsed (Row_Collapsed_Callback'Access, After => False);
      Widget.On_Destroy (On_Destroy'Access);

      --  Consider any newly inserted row as a collapsed row,
      --  set the flag accordingly and recompute its visibility.
      On_Row_Inserted
        (+Widget.Model, Row_Inserted_Callback'Access, Widget, After => False);
   end Initialize;

   -----------------------------------
   -- Set_Propagate_Filtered_Status --
   -----------------------------------

   procedure Set_Propagate_Filtered_Status
     (Self      : not null access Tree_View_Record;
      Propagate : Boolean := True) is
   begin
      Self.Propagate_Filtered_Status := Propagate;
   end Set_Propagate_Filtered_Status;

   --------------
   -- Refilter --
   --------------

   procedure Refilter
     (Self    : not null access Tree_View_Record'Class;
      Iter    : Gtk_Tree_Iter := Null_Iter)
   is
      function Check_Node
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter) return Boolean;
      function Check_Node
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter) return Boolean
      is
         pragma Unreferenced (Model, Path);
         Child : Gtk_Tree_Iter;
      begin
         if Self.Filter_Disabled then
            Set_Flag (Self, Iter, Flag_Is_Visible);
            return False;  --  keep traversing
         end if;

         --  A dummy node is always visible, so that its parent can be
         --  expanded even when filters are active

         if Get_Flag (Self, Iter, Flag_Is_Dummy) then
            Set_Flag (Self, Iter, Flag_Is_Visible);
            return False;  --  keep traversing
         end if;

         --  Since we are doing depth-first search, the children have already
         --  been computed. So we look at their status first

         if Self.Propagate_Filtered_Status then
            Child := Self.Model.Children (Iter);
            while Child /= Null_Iter loop
               if Get_Flag (Self, Child, Flag_Is_Visible) then
                  Set_Flag (Self, Iter, Flag_Is_Visible);
                  return False;  --  keep traversing
               end if;
               Self.Model.Next (Child);
            end loop;
         end if;

         --  None of the children is visible, but maybe the parent itself
         --  should be visible ?

         if Self.Is_Visible (Iter) then
            Set_Flag (Self, Iter, Flag_Is_Visible);
         else
            Clear_Flag (Self, Iter, Flag_Is_Visible);
         end if;
         return False;  --  keep traversing
      end Check_Node;

   begin
      if Self.Filter /= null then
         if Iter = Null_Iter then
            Self.Model.Foreach (Check_Node'Unrestricted_Access);
            Self.Filter.Refilter;
         else
            declare
               Path  : constant Gtk_Tree_Path := Self.Model.Get_Path (Iter);
               Dummy : Boolean;
            begin
               Dummy := Check_Node (+(Self.Model), Path, Iter);
               Self.Model.Row_Changed (Path, Iter);
               Path_Free (Path);
            end;
         end if;

         --  Force the re-expansion of the nodes that are flagged as expanded.
         --  This is needed because Gtk collapses automatically the nodes that
         --  are made invisible.
         Self.Force_Expansion;
      end if;
   end Refilter;

   -----------------------
   -- Expansion_Support --
   -----------------------

   package body Expansion_Support is

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Detached_Data_Access);

      ------------------
      -- Set_Expanded --
      ------------------

      procedure Set_Expanded
        (Status    : in out Expansion_Status;
         Row       : Id;
         Expanded  : Boolean := True) is
      begin
         if Expanded then
            Status.Expanded.Include (Row);
         else
            Status.Expanded.Delete (Row);
         end if;
      end Set_Expanded;

      ------------------
      -- Set_Expanded --
      ------------------

      procedure Set_Expanded
        (Status    : in out Detached_Model;
         Row       : Id;
         Expanded  : Boolean := True) is
      begin
         if Status.Data /= null then
            Set_Expanded (Status.Data.Expansion, Row, Expanded);
         end if;
      end Set_Expanded;

      --------------------------
      -- Get_Expansion_Status --
      --------------------------

      procedure Get_Expansion_Status
        (Self           : not null access Tree_Record'Class;
         Status         : out Expansion_Status;
         Save_Scrolling : Boolean := True)
      is
         procedure Do_Node
           (View : not null access Gtk_Tree_View_Record'Class;
            Path : Gtk_Tree_Path);
         --  Called for each expanded row

         procedure Do_Node
           (View : not null access Gtk_Tree_View_Record'Class;
            Path : Gtk_Tree_Path)
         is
            pragma Unreferenced (View);
         begin
            Status.Expanded.Include
              (Get_Id (Self, Self.Get_Store_Iter_For_Filter_Path (Path)));
         end Do_Node;

         procedure On_Selected
           (Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;
            Iter  : Gtk_Tree_Iter);
         --  Called for each selected row

         procedure On_Selected
           (Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;
            Iter  : Gtk_Tree_Iter)
         is
            pragma Unreferenced (Model, Iter);
         begin
            Status.Selection.Include
              (Get_Id (Self, Self.Get_Store_Iter_For_Filter_Path (Path)));
         end On_Selected;

         End_Path : Gtk_Tree_Path;
      begin
         Status.Expanded.Clear;
         Status.Selection.Clear;

         Self.Map_Expanded_Rows (Do_Node'Unrestricted_Access);

         Self.Get_Selection.Selected_Foreach (On_Selected'Unrestricted_Access);

         if Save_Scrolling then
            Self.Get_Visible_Range
              (Start_Path => Status.Scroll_Y,
               End_Path   => End_Path,
               Success    => Status.Has_Scroll_Info);
            if Status.Has_Scroll_Info then
               Path_Free (End_Path);
            end if;
         else
            Status.Has_Scroll_Info := False;
         end if;
      end Get_Expansion_Status;

      --------------------------
      -- Set_Expansion_Status --
      --------------------------

      procedure Set_Expansion_Status
        (Self   : not null access Tree_Record'Class;
         Status : Expansion_Status;
         Collapse_All_First : Boolean := True)
      is
         function Expand_Node
           (Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;
            Iter  : Gtk_Tree_Iter) return Boolean;

         -----------------
         -- Expand_Node --
         -----------------

         function Expand_Node
           (Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;
            Iter  : Gtk_Tree_Iter) return Boolean
         is
            pragma Unreferenced (Model, Path);

            Dummy         : Boolean;
            The_Id        : constant Id := Get_Id (Self, Iter);
            Sortable_Path : Gtk_Tree_Path;
         begin
            if Status.Expanded.Contains (The_Id) then
               Sortable_Path := Self.Get_Sortable_Path_For_Store_Iter (Iter);
               Dummy := Self.Expand_Row (Sortable_Path, Open_All => False);
               Path_Free (Sortable_Path);
            end if;

            if Status.Selection.Contains (The_Id) then
               Self.Get_Selection.Select_Iter
                 (Self.Convert_To_Sortable_Model_Iter (Iter));
            end if;

            return False;  --  keep iterating
         end Expand_Node;

      begin
         if Collapse_All_First then
            Self.Collapse_All;
         end if;

         Self.Model.Foreach (Expand_Node'Unrestricted_Access);

         if Status.Has_Scroll_Info then
            Gtk_Tree_View_Record (Self.all).Scroll_To_Cell
              (Path      => Status.Scroll_Y,
               Column    => null,
               Use_Align => False,
               Row_Align => 0.0,
               Col_Align => 0.0);
            Path_Free (Status.Scroll_Y);
         end if;
      end Set_Expansion_Status;

      -----------------------
      -- On_Tree_Destroyed --
      -----------------------

      procedure On_Tree_Destroyed
        (Data   : System.Address;
         Tree   : System.Address)
      is
         pragma Unreferenced (Tree);
         D : constant Detached_Data_Access := Convert (Data);
      begin
         if not D.Was_Detached then
            if D.Tree.Sortable_Model /= null then
               Unref (D.Tree.Sortable_Model);
            elsif D.Tree.Filter /= null then
               Unref (D.Tree.Filter);
            else
               Unref (D.Tree.Model);
            end if;
         end if;
         D.Tree := null;
      end On_Tree_Destroyed;

      ----------------------------
      -- Detach_Model_From_View --
      ----------------------------

      function Detach_Model_From_View
         (Self           : not null access Tree_Record'Class;
          Freeze         : Boolean := True;
          Save_Expansion : Boolean := True;
          Save_Scrolling : Boolean := True)
          return Detached_Model
      is
         Data : constant Detached_Data_Access := new Detached_Data;
      begin
         Data.Tree := Self;
         Data.Was_Detached := Self.Get_Model = Null_Gtk_Tree_Model;

         if not Data.Was_Detached then
            Increase_Indent (Me, "Detach model from view");

            --  In case the tree is destroyed before we have to reattach.
            --  Since we have a limited type, we can take its address, it
            --  will not change.
            Data.Tree.Weak_Ref (On_Tree_Destroyed_Access, Data.all'Address);

            if Freeze then
               Data.Sort_Col := Freeze_Sort (Self.Model);
            else
               Data.Sort_Col := -1;
            end if;

            Data.Save_Expansion := Save_Expansion;
            if Save_Expansion then
               Get_Expansion_Status
                 (Self, Data.Expansion, Save_Scrolling => Save_Scrolling);
            end if;

            if Self.Sortable_Model /= null then
               Ref (Self.Sortable_Model);
            elsif Self.Filter /= null then
               Ref (Self.Filter);
            else
               Ref (Self.Model);
            end if;
            Self.Set_Model (Null_Gtk_Tree_Model);
         end if;

         if Self.Filter /= null then
            Self.Filter.Clear_Cache;
         end if;

         return D : Detached_Model do
            D.Data := Data;
         end return;
      end Detach_Model_From_View;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Detached_Model) is
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Detached_Data, Detached_Data_Access);
         Data : Detached_Data_Access := Self.Data;
      begin
         Self.Data := null;   --  make finalize idempotent

         if Data /= null
           and then not Data.Was_Detached
           and then Data.Tree /= null
         then
            Decrease_Indent (Me, "Reattach model to view");

            Data.Tree.Weak_Unref (On_Tree_Destroyed_Access, Data.all'Address);

            if Data.Sort_Col /= -1 then
               Thaw_Sort (Data.Tree.Model, Data.Sort_Col);
            end if;

            if Data.Tree.Sortable_Model /= null then
               Data.Tree.Set_Model (+Data.Tree.Sortable_Model);
               Unref (Data.Tree.Sortable_Model);
            elsif Data.Tree.Filter /= null then
               Data.Tree.Set_Model (+Data.Tree.Filter);
               Unref (Data.Tree.Filter);
            else
               Data.Tree.Set_Model (+Data.Tree.Model);
               Unref (Data.Tree.Model);
            end if;

            if Data.Save_Expansion then
               Set_Expansion_Status (Data.Tree, Data.Expansion);
            end if;
         end if;

         Unchecked_Free (Data);
      end Finalize;
   end Expansion_Support;

   -------------------------
   -- On_Editing_Canceled --
   -------------------------

   procedure On_Editing_Canceled
     (V    : access Gtk_Cell_Renderer_Text_Record'Class;
      Data : Editing_Data)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Editing_Data_Record, Editing_Data);
      Id : Handler_Id;
      D  : Editing_Data;
   begin
      if Data.Tree.Being_Edited then
         --  Prevent interactive editing via single click
         Set_Property
           (Data.Render, Gtk.Cell_Renderer_Text.Editable_Property, False);
         Data.Tree.Being_Edited := False;

         Id := Data.Edited_Cb;
         Disconnect (V, Id);

         Id := Data.Canceled_Cb;
         Disconnect (V, Id);

         D := Data;
         Unchecked_Free (D);
      end if;
   end On_Editing_Canceled;

   ---------------
   -- On_Edited --
   ---------------

   procedure On_Edited
     (V           : access Gtk_Cell_Renderer_Text_Record'Class;
      Params      : Glib.Values.GValues;
      Data        : Editing_Data)
   is
      Filter_Path : constant UTF8_String := Get_String (Nth (Params, 1));
      Text        : constant UTF8_String := Get_String (Nth (Params, 2));
      Filter_Iter  : Gtk_Tree_Iter;
   begin
      if Data.Tree.Being_Edited then
         Filter_Iter := Data.Tree.Filter.Get_Iter_From_String (Filter_Path);
         Data.Tree.On_Edited
           (Store_Iter  => Data.Tree.Convert_To_Store_Iter (Filter_Iter),
            View_Column => Data.View_Column,
            Text        => Text);
         --  On_Editing_Canceled will reset Being_Edited
         On_Editing_Canceled (V, Data);
      end if;
   end On_Edited;

   ------------------------
   -- Start_Editing_Idle --
   ------------------------

   function Start_Editing_Idle (Data : Editing_Data) return Boolean is
   begin
      --  Make the rows editable temporarily
      Set_Property
        (Data.Render, Gtk.Cell_Renderer_Text.Editable_Property, True);
      Set_Cursor_On_Cell
        (Data.Tree,
         Path          => Data.Filter_Path,
         Focus_Column  => Data.Tree.Get_Column (Gint (Data.View_Column)),
         Focus_Cell    => Data.Render,
         Start_Editing => True);

      --  Data will be freed by one of the On_Edited or On_Editing_Canceled
      --  callbacks
      return False;
   end Start_Editing_Idle;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
     (Self        : not null access Tree_View_Record'Class;
      Render      : not null access Gtk_Cell_Renderer_Text_Record'Class;
      Store_Iter  : Gtk_Tree_Iter := Null_Iter;
      View_Column : Edited_Column_Id := 0)
   is
      Dummy       : G_Source_Id;
      Filter_Iter : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;
      Data        : Editing_Data;
   begin
      if Store_Iter = Null_Iter then
         if Self.Get_Selection.Get_Mode = Selection_Single then
            Self.Get_Selection.Get_Selected (Model, Filter_Iter);
         else
            Self.Get_First_Selected (Model, Filter_Iter);
         end if;
      else
         Model := (if Self.Filter /= null then +Self.Filter else +Self.Model);
         Filter_Iter := Self.Convert_To_Filter_Iter (Store_Iter);
         Self.Get_Selection.Unselect_All;
         Self.Get_Selection.Select_Iter (Filter_Iter);
      end if;

      if Filter_Iter /= Null_Iter and then not Self.Being_Edited then
         Self.Being_Edited := True;
         Data := new Editing_Data_Record;
         Data.all :=
           (Tree        => Tree_View (Self),
            Render      => Gtk_Cell_Renderer_Text (Render),
            Filter_Path => Get_Path (Model, Filter_Iter),
            View_Column => View_Column,
            Edited_Cb   => Editing_Callbacks.Connect
              (Render, Signal_Edited, On_Edited'Access, Data),
            Canceled_Cb => Editing_Callbacks.Connect
              (Render, Gtk.Cell_Renderer.Signal_Editing_Canceled,
               Editing_Callbacks.To_Marshaller (On_Editing_Canceled'Access),
               Data));

         --  Start the edition in idle mode, since otherwise the tree gains
         --  the focus when the menu is hidden, and stops the edition
         --  immediately.
         Dummy := Rename_Idle.Idle_Add
           (Start_Editing_Idle'Access, Data, Priority   => Priority_High_Idle);
      end if;
   end Start_Editing;

   --------------------
   -- Scroll_To_Cell --
   --------------------

   overriding procedure Scroll_To_Cell
     (Self      : not null access Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : access Gtk_Tree_View_Column_Record'Class;
      Use_Align : Boolean;
      Row_Align : Gfloat;
      Col_Align : Gfloat) is
   begin
      if Self.User_Scroll_Id = No_Source_Id then
         if Self.Background_Scroll_Id /= No_Source_Id then
            Self.User_Scroll_Data := User_Scroll_Data_Type'
              (Path      => Copy (Path),
               Column    => Column,
               Use_Align => Use_Align,
               Row_Align => Row_Align,
               Col_Align => Col_Align);
            Self.User_Scroll_Id :=
              Tree_Sources.Idle_Add (On_User_Scroll'Access, Self);
         else
            Gtk_Tree_View_Record (Self.all).Scroll_To_Cell
              (Path      => Path,
               Column    => Column,
               Use_Align => Use_Align,
               Row_Align => Row_Align,
               Col_Align => Col_Align);
         end if;
      end if;
   end Scroll_To_Cell;

end Gtkada.Tree_View;
