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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gdk.Drag_Contexts;    use Gdk.Drag_Contexts;
with Glib.Object;          use Glib.Object;
with Glib.Types;           use Glib.Types;
with Glib.Values;          use Glib.Values;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Selection_Data;   use Gtk.Selection_Data;
with Gtk.Tree_Drag_Dest;   use Gtk.Tree_Drag_Dest;
with Gtk.Widget;           use Gtk.Widget;
with System;               use System;

package body Gtkada.Tree_View is

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

   ---------------
   -- Callbacks --
   ---------------

   procedure Row_Expanded_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
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
      Info : access GInterface_Info;
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
     (Self        : access Tree_View_Record'Class;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk_Tree_Iter
   is
      Store_Iter : Gtk_Tree_Iter;
   begin
      if Self.Filter /= null and then Filter_Iter /= Null_Iter then
         Self.Filter.Convert_Iter_To_Child_Iter (Store_Iter, Filter_Iter);
         return Store_Iter;
      else
         return Filter_Iter;
      end if;
   end Convert_To_Store_Iter;

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

   ---------------------------
   -- Row_Expanded_Callback --
   ---------------------------

   procedure Row_Expanded_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      Tree       : constant Tree_View := Tree_View (Widget);
      Iter       : Gtk_Tree_Iter;
      Store_Iter : Gtk_Tree_Iter;
      Path       : Gtk_Tree_Path;
      Dummy      : Boolean;
      M          : constant Gtk_Tree_Model :=
        (if Tree.Filter /= null then +Tree.Filter else +Tree.Model);

   begin
      if Tree.Lock or else Filter_Iter = Null_Iter then
         return;
      end if;

      Store_Iter := Tree.Convert_To_Store_Iter (Filter_Iter);
      Set_Flag (Tree, Store_Iter, Flag_Is_Expanded);

      --  Replace dummy child nodes if needed.
      --  We always assume the dummy child (if any) is the first child

      if Tree.Model.Has_Child (Store_Iter) then
         Iter := Tree.Model.Children (Store_Iter);
         if Get_Flag (Tree, Iter, Flag_Is_Dummy) then
            Tree.Model.Remove (Iter);   --  remove dummy node
            Tree.Add_Children (Store_Iter);

            --  Make sure the parent is indeed expanded
            Dummy := Tree.Expand_Row (Filter_Path, Open_All => False);

         else
            --  Re-expand existing child nodes as needed

            Iter := Children (M, Filter_Iter);
            while Iter /= Null_Iter loop
               Store_Iter := Tree.Convert_To_Store_Iter (Iter);

               if Get_Flag (Tree, Store_Iter, Flag_Is_Expanded) then
                  Path := Get_Path (M, Iter);
                  Dummy := Expand_Row (Tree, Path, False);
                  Path_Free (Path);
               end if;

               Next (M, Iter);
            end loop;
         end if;
      end if;
   end Row_Expanded_Callback;

   ----------------------------
   -- Row_Collapsed_Callback --
   ----------------------------

   procedure Row_Collapsed_Callback
     (Widget      : access Gtk_Tree_View_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Path);
      Tree       : constant Tree_View := Tree_View (Widget);
      Store_Iter : Gtk_Tree_Iter;
   begin
      Store_Iter := Tree.Convert_To_Store_Iter (Filter_Iter);
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
     (Widget       : out Tree_View;
      Column_Types : GType_Array;
      Filtered     : Boolean := False) is
   begin
      Widget := new Tree_View_Record;
      Initialize (Widget, Column_Types, Filtered => Filtered);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget           : access Tree_View_Record'Class;
      Column_Types     : GType_Array;
      Filtered         : Boolean;
      Set_Visible_Func : Boolean := False)
   is
      Params            : GParameter_Array (1 .. 1);
      Real_Column_Types : constant GType_Array := Column_Types & (GType_Int);
      --  Reserve space for extra information

   begin
      --  Tree columns in Gtk+ are numbered starting from 0
      Widget.Column_Extra := Gint (Real_Column_Types'Length - 1);

      Gtk_New (Widget.Model, Real_Column_Types);

      if Filtered then
         Init (Params (1).Value, Gtk.Tree_Model.Get_Type);
         Set_Object (Params (1).Value, Widget.Model);
         Params (1).Name := New_String ("child-model");

         Widget.Filter := new Gtkada_Tree_Model_Filter_Record;
         G_New (Widget.Filter, Get_Filter_Model_Type, Params);

         Free (Params);

         Unref (Widget.Model);  --  owned by the filter

         Initialize (Gtk_Tree_View (Widget), +Widget.Filter);
         Unref (Widget.Filter);  --  owned by the widget

         Widget.On_Drag_Begin (On_Drag_Begin'Access, Slot => Widget);
         Widget.On_Drag_End (On_Drag_End'Access, Slot => Widget);

         if Set_Visible_Func then
            Set_Visible_Funcs.Set_Visible_Func
              (Widget.Filter, Is_Visible'Access, Data => Widget);
         end if;

      else
         Initialize (Gtk_Tree_View (Widget), +Widget.Model);
         Unref (Widget.Model);  --  owned by the widget
      end if;

      --  We can't connect with After => True, because then the Gtk_Tree_Iter
      --  might have been modified, and in particular would no longer be
      --  relative to the filter model if we use one.

      Widget.On_Row_Expanded (Row_Expanded_Callback'Access, After => False);
      Widget.On_Row_Collapsed (Row_Collapsed_Callback'Access, After => False);

      --  Consider any newly inserted row as a collapsed row,
      --  set the flag accordingly and recompute its visibility.
      On_Row_Inserted
        (+Widget.Model, Row_Inserted_Callback'Access, Widget, After => False);
   end Initialize;

   --------------
   -- Refilter --
   --------------

   procedure Refilter
     (Self    : not null access Tree_View_Record'Class)
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

         --  Since we are doing depth-first search, the children have already
         --  been computed. So we look at their status first

         Child := Self.Model.Children (Iter);
         while Child /= Null_Iter loop
            if Get_Flag (Self, Child, Flag_Is_Visible) then
               Set_Flag (Self, Iter, Flag_Is_Visible);
               return False;  --  keep traversing
            end if;
            Self.Model.Next (Child);
         end loop;

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
         Self.Model.Foreach (Check_Node'Unrestricted_Access);
         Self.Filter.Refilter;
      end if;
   end Refilter;

   -----------------------
   -- Expansion_Support --
   -----------------------

   package body Expansion_Support is

      --------------------------
      -- Get_Expansion_Status --
      --------------------------

      procedure Get_Expansion_Status
        (Self   : not null access Tree_Record'Class;
         Status : out Expansion_Status)
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

         End_Path : Gtk_Tree_Path;
      begin
         Status.Expanded.Clear;
         Self.Map_Expanded_Rows (Do_Node'Unrestricted_Access);

         Self.Get_Visible_Range
           (Start_Path => Status.Scroll_Y,
            End_Path   => End_Path,
            Success    => Status.Has_Scroll_Info);
         if Status.Has_Scroll_Info then
            Path_Free (End_Path);
         end if;
      end Get_Expansion_Status;

      --------------------------
      -- Set_Expansion_Status --
      --------------------------

      procedure Set_Expansion_Status
        (Self   : not null access Tree_Record'Class;
         Status : Expansion_Status)
      is
         function Expand_Node
           (Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;
            Iter  : Gtk_Tree_Iter) return Boolean;
         function Expand_Node
           (Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;
            Iter  : Gtk_Tree_Iter) return Boolean
         is
            pragma Unreferenced (Model);
            Dummy : Boolean;
         begin
            if Status.Expanded.Contains (Get_Id (Self, Iter)) then
               Dummy := Self.Expand_Row (Path, Open_All => False);
            end if;
            return False;  --  keep iterating
         end Expand_Node;

      begin
         Self.Collapse_All;
         Self.Model.Foreach (Expand_Node'Unrestricted_Access);

         if Status.Has_Scroll_Info then
            Self.Scroll_To_Cell
              (Path      => Status.Scroll_Y,
               Column    => null,
               Use_Align => False,
               Row_Align => 0.0,
               Col_Align => 0.0);
            Path_Free (Status.Scroll_Y);
         end if;
      end Set_Expansion_Status;

   end Expansion_Support;

end Gtkada.Tree_View;
