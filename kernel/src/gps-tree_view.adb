------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with System;

with Glib.Values;
with Gtk.Handlers;
with Gtk.Widget;

with Traces;

package body GPS.Tree_View is

   use Glib;
   use Glib.Values;
   use Gtk.Handlers;
   use Gtk.Tree_Model;
   use Gtk.Tree_View;
   use Traces;

   procedure Finalize (Node : in out Node_Record);
   --  Finalize and deallocate all children nodes recursively.

   procedure On_Lowerst_Model_Row_Inserted
     (Object : access Gtk_Tree_Model_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter;
      Self   : GPS_Tree_View);
   --  Allocates new node

   procedure On_Lowerst_Model_Row_Deleted
     (Object : access Gtk_Tree_Model_Record'Class;
      Path   : Gtk_Tree_Path;
      Self   : GPS_Tree_View);
   --  Frees corresponding node and its children

   procedure On_Lowerst_Model_Rows_Reordered
     (Object : access Gtk_Tree_Model_Record'Class;
      Params : Glib.Values.GValues;
      Self   : GPS_Tree_View);
   --  Reorders internal nodes

   procedure On_Source_Model_Row_Has_Child_Toggled
     (Object : access Gtk_Tree_Model_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter;
      Self   : GPS_Tree_View);
   --  Expands node when necessary

   procedure On_Row_Expanded
     (Object : access Gtk_Tree_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Self   : GPS_Tree_View);
   --  Save state of the node and restore expanded/collapsed state of node's
   --  children.

   procedure On_Row_Collapsed
     (Object : access Gtk_Tree_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Self   : GPS_Tree_View);
   --  Save state of the node

   procedure On_Destroy
     (Object : access Gtk_Tree_View_Record'Class;
      Self   : GPS_Tree_View);
   --  Frees all internal data

   function To_Lowerst_Model_Path
     (Self : not null access GPS_Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Returns lowerst's model path corresponding to given view' source model
   --  iterator.

   package Gtk_Tree_Model_Callbacks is
     new Gtk.Handlers.User_Callback (Gtk_Tree_Model_Record, GPS_Tree_View);

   package Gtk_Tree_View_Callbacks is
     new Gtk.Handlers.User_Callback (Gtk_Tree_View_Record, GPS_Tree_View);

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Record, Node_Access);

   function On_Idle (Self : GPS_Tree_View) return Boolean;
   --  Idle callback.

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Node : in out Node_Record) is
      Child : Node_Access;

   begin
      for J in Node.Children.First_Index .. Node.Children.Last_Index loop
         Child := Node.Children.Element (J);
         Finalize (Child.all);
         Free (Child);
      end loop;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self          : not null access GPS_Tree_View_Record'Class;
      Lowerst_Model : not null Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Create_Nodes
        (Parent_Path : Gtk_Tree_Path;
         Parent_Iter : Gtk_Tree_Iter;
         Parent_Node : Node_Access);
      --  Creates internal nodes for each child node

      ------------------
      -- Create_Nodes --
      ------------------

      procedure Create_Nodes
        (Parent_Path : Gtk_Tree_Path;
         Parent_Iter : Gtk_Tree_Iter;
         Parent_Node : Node_Access)
      is
         Iter : Gtk_Tree_Iter;
         Path : Gtk_Tree_Path;
         Node : Node_Access;

      begin
         Iter := Lowerst_Model.Children (Parent_Iter);
         Path := Copy (Parent_Path);
         Down (Path);

         while Iter /= Null_Iter loop
            Node :=
              new Node_Record'(Parent_Node, False, Node_Vectors.Empty_Vector);
            Parent_Node.Children.Append (Node);
            Self.On_Lowerst_Model_Row_Inserted (Path, Iter, Node);
            Create_Nodes (Path, Iter, Node);
            Lowerst_Model.Next (Iter);
            Next (Path);
         end loop;

         Path_Free (Path);
      end Create_Nodes;

      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
      Node : Node_Access;

   begin
      Gtk.Tree_View.Initialize (Self);
      Self.Root := new Node_Record'(null, False, Node_Vectors.Empty_Vector);
      Self.Lowerst_Model := Lowerst_Model;

      --  Connect to view

      Gtk_Tree_View_Callbacks.Connect
        (Self,
         Signal_Row_Collapsed,
         Gtk_Tree_View_Callbacks.To_Marshaller (On_Row_Collapsed'Access),
         GPS_Tree_View (Self),
         True);
      Gtk_Tree_View_Callbacks.Connect
        (Self,
         Signal_Row_Expanded,
         Gtk_Tree_View_Callbacks.To_Marshaller (On_Row_Expanded'Access),
         GPS_Tree_View (Self),
         True);
      Gtk_Tree_View_Callbacks.Connect
        (Self, Gtk.Widget.Signal_Destroy,
         On_Destroy'Access, GPS_Tree_View (Self), True);

      --  Connect to lowerst model

      Add_Watch
        (Gtk_Tree_Model_Callbacks.Connect
           (Lowerst_Model,
            Signal_Row_Inserted,
            Gtk_Tree_Model_Callbacks.To_Marshaller
              (On_Lowerst_Model_Row_Inserted'Access),
            GPS_Tree_View (Self),
            False),
         Self);
      Add_Watch
        (Gtk_Tree_Model_Callbacks.Connect
           (Lowerst_Model,
            Signal_Row_Deleted,
            Gtk_Tree_Model_Callbacks.To_Marshaller
              (On_Lowerst_Model_Row_Deleted'Access),
            GPS_Tree_View (Self),
            True),
         Self);
      Add_Watch
        (Gtk_Tree_Model_Callbacks.Connect
           (Lowerst_Model,
            Signal_Rows_Reordered,
            On_Lowerst_Model_Rows_Reordered'Access,
            GPS_Tree_View (Self),
            True),
         Self);

      --  Create internal structure for already present nodes in the lowerst
      --  model

      Iter := Lowerst_Model.Get_Iter_First;
      Path := Gtk_New_First;

      while Iter /= Null_Iter loop
         Node := new Node_Record'(Self.Root, False, Node_Vectors.Empty_Vector);
         Self.Root.Children.Append (Node);
         Self.On_Lowerst_Model_Row_Inserted (Path, Iter, Node);
         Create_Nodes (Path, Iter, Node);
         Lowerst_Model.Next (Iter);
         Next (Path);
      end loop;

      Path_Free (Path);
   end Initialize;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : GPS_Tree_View) return Boolean is
      C     : Path_List.Cursor;
      Path  : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if not Self.Paths_To_Be_Expanded.Is_Empty then
         C := Self.Paths_To_Be_Expanded.First;

         while Path_List.Has_Element (C) loop
            Path := Path_List.Element (C);
            Dummy := Self.Expand_Row (Path, False);
            Path_Free (Path);
            Path_List.Next (C);
         end loop;

         Self.Paths_To_Be_Expanded.Clear;
      end if;

      if Self.On_Idle /= 0 then
         Remove (Self.On_Idle);
         Self.On_Idle := 0;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Idle;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Object : access Gtk_Tree_View_Record'Class;
      Self   : GPS_Tree_View)
   is
      pragma Unreferenced (Object);

      C : Path_List.Cursor;
      Path : Gtk_Tree_Path;
   begin
      if not Self.Paths_To_Be_Expanded.Is_Empty then
         C := Self.Paths_To_Be_Expanded.First;
         while Path_List.Has_Element (C) loop
            Path := Path_List.Element (C);
            Path_Free (Path);
            Path_List.Next (C);
         end loop;
      end if;

      if Self.On_Idle /= 0 then
         Remove (Self.On_Idle);
         Self.On_Idle := 0;
      end if;

      Finalize (Self.Root.all);
      Free (Self.Root);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Destroy;

   ----------------------------------
   -- On_Lowerst_Model_Row_Deleted --
   ----------------------------------

   procedure On_Lowerst_Model_Row_Deleted
     (Object : access Gtk_Tree_Model_Record'Class;
      Path   : Gtk_Tree_Path;
      Self   : GPS_Tree_View)
   is
      pragma Unreferenced (Object);

      Indices : constant Glib.Gint_Array := Get_Indices (Path);
      Node    : Node_Access := Self.Root;

   begin
      for J in Indices'First .. Indices'Last loop
         Node := Node.Children.Element (Indices (J));
      end loop;

      Node.Parent.Children.Delete (Indices (Indices'Last));
      Finalize (Node.all);
      Free (Node);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Lowerst_Model_Row_Deleted;

   -----------------------------------
   -- On_Lowerst_Model_Row_Inserted --
   -----------------------------------

   procedure On_Lowerst_Model_Row_Inserted
     (Object : access Gtk_Tree_Model_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter;
      Self   : GPS_Tree_View)
   is
      pragma Unreferenced (Object);

      Indices : constant Glib.Gint_Array := Get_Indices (Path);
      Parent  : Node_Access := Self.Root;
      Node    : Node_Access;

   begin
      for J in Indices'First .. Indices'Last - 1 loop
         Parent := Parent.Children.Element (Indices (J));
      end loop;

      Node := new Node_Record'(Parent, False, Node_Vectors.Empty_Vector);
      Parent.Children.Insert (Indices (Indices'Last), Node);

      Self.On_Lowerst_Model_Row_Inserted (Path, Iter, Node);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Lowerst_Model_Row_Inserted;

   -------------------------------------
   -- On_Lowerst_Model_Rows_Reordered --
   -------------------------------------

   procedure On_Lowerst_Model_Rows_Reordered
     (Object : access Gtk_Tree_Model_Record'Class;
      Params : Glib.Values.GValues;
      Self   : GPS_Tree_View)
   is
      Path    : constant Gtk_Tree_Path   := Get_Tree_Path (Nth (Params, 1));
      Indices : constant Glib.Gint_Array := Get_Indices (Path);
      Iter    : constant Gtk_Tree_Iter   := Get_Tree_Iter (Nth (Params, 2));
      Address : constant System.Address  := Get_Address (Nth (Params, 3));
      Length  : constant Natural         := Natural (Object.N_Children (Iter));
      Node    : Node_Access := Self.Root;

      subtype New_Order_Array is Gint_Array (0 .. Length - 1);
      New_Order : New_Order_Array;
      for New_Order'Address use Address;

      Aux : Node_Vectors.Vector;

   begin
      for J in Indices'First .. Indices'Last loop
         Node := Node.Children.Element (Indices (J));
      end loop;

      Aux.Reserve_Capacity (Ada.Containers.Count_Type (Length));
      Aux.Set_Length (Ada.Containers.Count_Type (Length));

      for J in New_Order'Range loop
         Aux.Replace_Element (Gint (J), Node.Children.Element (New_Order (J)));
      end loop;

      Node.Children := Aux;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Lowerst_Model_Rows_Reordered;

   ----------------------
   -- On_Row_Collapsed --
   ----------------------

   procedure On_Row_Collapsed
     (Object : access Gtk_Tree_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Self   : GPS_Tree_View)
   is
      pragma Unreferenced (Object, Path);

      Lowerst_Path : constant Gtk_Tree_Path :=
                       Self.To_Lowerst_Model_Path (Iter);

   begin
      declare
         Indices : constant Glib.Gint_Array := Get_Indices (Lowerst_Path);
         Node    : Node_Access := Self.Root;

      begin
         for J in Indices'First .. Indices'Last loop
            Node := Node.Children.Element (Indices (J));
         end loop;

         Node.Expanded := False;
      end;

      Path_Free (Lowerst_Path);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Row_Collapsed;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (Object : access Gtk_Tree_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Self   : GPS_Tree_View)
   is
      pragma Unreferenced (Object);

      Child_Path : Gtk_Tree_Path;
      Child_Iter : Gtk_Tree_Iter;

   begin
      --  Update stored node's state

      declare
         Lowerst_Path : constant Gtk_Tree_Path :=
                          Self.To_Lowerst_Model_Path (Iter);
         Indices      : constant Glib.Gint_Array := Get_Indices (Lowerst_Path);
         Node         : Node_Access := Self.Root;

      begin
         for J in Indices'First .. Indices'Last loop
            Node := Node.Children.Element (Indices (J));
         end loop;

         Node.Expanded := True;
         Path_Free (Lowerst_Path);

         Self.On_Row_Expanded (Path, Iter, Node);
      end;

      --  Expand children nodes when necessary

      Child_Path := Copy (Path);
      Child_Iter := Self.Get_Model.Children (Iter);
      Down (Child_Path);

      while Child_Iter /= Null_Iter loop
         declare
            Lowerst_Path : constant Gtk_Tree_Path :=
                             Self.To_Lowerst_Model_Path (Child_Iter);
            Indices      : constant Glib.Gint_Array :=
                             Get_Indices (Lowerst_Path);
            Child_Node   : Node_Access := Self.Root;
            Dummy        : Boolean;
            pragma Unreferenced (Dummy);

         begin
            for J in Indices'First .. Indices'Last loop
               Child_Node := Child_Node.Children.Element (Indices (J));
            end loop;

            if Child_Node.Expanded then
               Dummy := Self.Expand_Row (Child_Path, False);
            end if;

            Path_Free (Lowerst_Path);

            Self.Get_Model.Next (Child_Iter);
            Next (Child_Path);
         end;
      end loop;

      Path_Free (Child_Path);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Row_Expanded;

   -------------------------------------------
   -- On_Source_Model_Row_Has_Child_Toggled --
   -------------------------------------------

   procedure On_Source_Model_Row_Has_Child_Toggled
     (Object : access Gtk_Tree_Model_Record'Class;
      Path   : Gtk_Tree_Path;
      Iter   : Gtk_Tree_Iter;
      Self   : GPS_Tree_View)
   is
      pragma Unreferenced (Object);

   begin
      --  Careful: it is not possible to modify the tree view (for instance
      --  by expanding rows or by scrolling) in reaction to
      --  Row_Has_Child_Toggled, since this confuses the validation of the
      --  rbtree nodes in Gtk+.
      --  Instead, we schedule these operations to occur in an idle callback.

      if Self.Get_Model.Has_Child (Iter)
        and then not Self.Row_Expanded (Path)
      then
         declare
            Lowerst_Path : constant Gtk_Tree_Path :=
                             Self.To_Lowerst_Model_Path (Iter);
            Indices      : constant Glib.Gint_Array :=
                             Get_Indices (Lowerst_Path);
            Node         : Node_Access := Self.Root;
            Dummy        : Boolean;
            pragma Unreferenced (Dummy);

         begin
            for J in Indices'First .. Indices'Last loop
               Node := Node.Children.Element (Indices (J));
            end loop;

            if Node.Expanded then
               Self.Paths_To_Be_Expanded.Prepend (Copy (Path));
            end if;

            Path_Free (Lowerst_Path);
         end;
      end if;

      --  If no idle callback is currently registered, and there are paths
      --  to expand, register an idle callback.
      if Self.On_Idle = 0
        and then not Self.Paths_To_Be_Expanded.Is_Empty
      then
         Self.On_Idle := Tree_View_Sources.Idle_Add
           (Func => On_Idle'Access,
            Data => Self);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Source_Model_Row_Has_Child_Toggled;

   ----------------------
   -- Set_Source_Model --
   ----------------------

   procedure Set_Source_Model
     (Self         : access GPS_Tree_View_Record;
      Source_Model : Gtk.Tree_Model.Gtk_Tree_Model) is
   begin
      Self.Set_Model (Source_Model);

      --  Connect to source model

      Gtk_Tree_Model_Callbacks.Connect
        (Source_Model,
         Signal_Row_Has_Child_Toggled,
         Gtk_Tree_Model_Callbacks.To_Marshaller
           (On_Source_Model_Row_Has_Child_Toggled'Access),
         GPS_Tree_View (Self),
         True);
   end Set_Source_Model;

   ---------------------------
   -- To_Lowerst_Model_Path --
   ---------------------------

   function To_Lowerst_Model_Path
     (Self : not null access GPS_Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path is
   begin
      return Self.Lowerst_Model.Get_Path (Self.To_Lowerst_Model_Iter (Iter));
   end To_Lowerst_Model_Path;

end GPS.Tree_View;
