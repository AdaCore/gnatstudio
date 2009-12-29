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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Glib.Values;
with Gtk.Object;
with Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;

package body GPS.Tree_View is

   use Glib;
   use Glib.Values;
   use Gtk.Handlers;
   use Gtk.Object;
   use Gtk.Tree_Model;
   use Gtk.Tree_Model_Filter;
   use Gtk.Tree_Model_Sort;
   use Gtk.Tree_View;

   procedure Finalize (Node : in out Node_Record);
   --  Finalize and deallocate all children nodes recursively.

   function To_Lowerst
     (Self        : not null access GPS_Tree_View_Record'Class;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Converts specified Gtk_Tree_Iter of the source model to corresponding
   --  Gtk_Tree_Path of the lowerst model.

   procedure Connect_To_View
     (Self : not null access GPS_Tree_View_Record'Class);
   --  Connects callbacks to the view

   procedure Connect_To_Model
     (Self : not null access GPS_Tree_View_Record'Class);
   --  Connects callbacks to the model

   procedure Disconnect_From_Model
     (Self : not null access GPS_Tree_View_Record'Class);
   --  Disconnects callbacks from the model

   function Get_Tree_Path
     (Value : Glib.Values.GValue) return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Helper function to get value of Gtk_Tree_Path type from the GValue.

   function Get_Tree_Iter
     (Value : Glib.Values.GValue) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Helper function to get value of Gtk_Tree_Iter type from the GValue.

   function Lowerst_Model
     (Self : not null access GPS_Tree_View_Record'Class)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns lowerst underling model by unwinding known model classes in the
   --  stack of models.

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
     (Self : access GPS_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path);
   --  Save state of the node and restore expanded/collapsed state of node's
   --  children.

   procedure On_Row_Collapsed
     (Self : access GPS_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path);
   --  Save state of the node

   procedure On_Destroy (Self : access GPS_Tree_View_Record'Class);
   --  Frees all internal data

   package Gtk_Tree_Model_Callbacks is
     new Gtk.Handlers.User_Callback (Gtk_Tree_Model_Record, GPS_Tree_View);

   package GPS_Tree_View_Callbacks is
      new Gtk.Handlers.Callback (GPS_Tree_View_Record);

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Record, Node_Access);

   ----------------------
   -- Connect_To_Model --
   ----------------------

   procedure Connect_To_Model
     (Self : not null access GPS_Tree_View_Record'Class)
   is
      Model : constant Gtk_Tree_Model := Self.Lowerst_Model;

   begin
      --  Connect to lowerst model

      Gtk_Tree_Model_Callbacks.Connect
        (Model,
         Signal_Row_Inserted,
         Gtk_Tree_Model_Callbacks.To_Marshaller
           (On_Lowerst_Model_Row_Inserted'Access),
         GPS_Tree_View (Self),
         False);
      Gtk_Tree_Model_Callbacks.Connect
        (Model,
         Signal_Row_Deleted,
         Gtk_Tree_Model_Callbacks.To_Marshaller
           (On_Lowerst_Model_Row_Deleted'Access),
         GPS_Tree_View (Self),
         True);
      Gtk_Tree_Model_Callbacks.Connect
        (Model,
         Signal_Rows_Reordered,
         On_Lowerst_Model_Rows_Reordered'Access,
         GPS_Tree_View (Self),
         True);

      --  Connect to source model

      Gtk_Tree_Model_Callbacks.Connect
        (Self.Get_Model,
         Signal_Row_Has_Child_Toggled,
         Gtk_Tree_Model_Callbacks.To_Marshaller
           (On_Source_Model_Row_Has_Child_Toggled'Access),
         GPS_Tree_View (Self),
         True);
   end Connect_To_Model;

   ---------------------
   -- Connect_To_View --
   ---------------------

   procedure Connect_To_View
     (Self : not null access GPS_Tree_View_Record'Class) is
   begin
      GPS_Tree_View_Callbacks.Connect
        (Self,
         Signal_Row_Collapsed,
         GPS_Tree_View_Callbacks.To_Marshaller (On_Row_Collapsed'Access),
         True);
      GPS_Tree_View_Callbacks.Connect
        (Self,
         Signal_Row_Expanded,
         GPS_Tree_View_Callbacks.To_Marshaller (On_Row_Expanded'Access),
         True);
      GPS_Tree_View_Callbacks.Connect
        (Self, Signal_Destroy, On_Destroy'Access, True);
   end Connect_To_View;

   ---------------------------
   -- Disconnect_From_Model --
   ---------------------------

   procedure Disconnect_From_Model
     (Self : not null access GPS_Tree_View_Record'Class)
   is
      Model : constant Gtk_Tree_Model := Self.Lowerst_Model;

   begin
      Disconnect (Model, Self.Row_Inserted_Handler);
      Disconnect (Model, Self.Row_Deleted_Handler);
      Disconnect (Model, Self.Row_Has_Child_Toggled_Handler);
      Disconnect (Model, Self.Rows_Reordered_Handler);
   end Disconnect_From_Model;

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

   -------------------
   -- Get_Tree_Iter --
   -------------------

   function Get_Tree_Iter
     (Value : Glib.Values.GValue) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter;

   begin
      Get_Tree_Iter (Value, Iter);

      return Iter;
   end Get_Tree_Iter;

   -------------------
   -- Get_Tree_Path --
   -------------------

   function Get_Tree_Path (Value : Glib.Values.GValue) return Gtk_Tree_Path is

      function To_Gtk_Tree_Path is
        new Ada.Unchecked_Conversion (System.Address, Gtk_Tree_Path);

   begin
      return To_Gtk_Tree_Path (Get_Address (Value));
   end Get_Tree_Path;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Object : out GPS_Tree_View) is
   begin
      Object := new GPS_Tree_View_Record;
      GPS.Tree_View.Initialize (Object);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Object : out GPS_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Object := new GPS_Tree_View_Record;
      GPS.Tree_View.Initialize (Object, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access GPS_Tree_View_Record'Class) is
   begin
      Gtk.Tree_View.Initialize (Self);
      Self.Root := new Node_Record'(null, False, Node_Vectors.Empty_Vector);
      Self.Connect_To_View;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access GPS_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Gtk.Tree_View.Initialize (Self, Model);
      Self.Root := new Node_Record'(null, False, Node_Vectors.Empty_Vector);
      Self.Connect_To_View;
      Self.Connect_To_Model;
   end Initialize;

   -------------------
   -- Lowerst_Model --
   -------------------

   function Lowerst_Model
     (Self : not null access GPS_Tree_View_Record'Class)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
      Model : Gtk_Tree_Model := Self.Get_Model;

   begin
      loop
         if Model.all in Gtk_Tree_Model_Sort_Record'Class then
            Model := Gtk_Tree_Model_Sort (Model).Get_Model;

         elsif Model.all in Gtk_Tree_Model_Filter_Record'Class then
            Model := Gtk_Tree_Model_Filter (Model).Get_Model;

         else
            exit;
         end if;
      end loop;

      return Model;
   end Lowerst_Model;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access GPS_Tree_View_Record'Class) is
   begin
      Finalize (Self.Root.all);
      Free (Self.Root);
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
   end On_Lowerst_Model_Rows_Reordered;

   ----------------------
   -- On_Row_Collapsed --
   ----------------------

   procedure On_Row_Collapsed
     (Self : access GPS_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);

      Lowerst_Path : constant Gtk_Tree_Path := Self.To_Lowerst (Iter);

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
   end On_Row_Collapsed;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (Self : access GPS_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path)
   is
      Child_Path : Gtk_Tree_Path;
      Child_Iter : Gtk_Tree_Iter;

   begin
      --  Update stored node's state

      declare
         Lowerst_Path : constant Gtk_Tree_Path := Self.To_Lowerst (Iter);
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
              Self.To_Lowerst (Child_Iter);
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
      if Self.Get_Model.Has_Child (Iter)
        and then not Self.Row_Expanded (Path)
      then
         declare
            Lowerst_Path : constant Gtk_Tree_Path := Self.To_Lowerst (Iter);
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
               Dummy := Self.Expand_Row (Path, False);
            end if;

            Path_Free (Lowerst_Path);
         end;
      end if;
   end On_Source_Model_Row_Has_Child_Toggled;

   ---------------
   -- Set_Model --
   ---------------

   overriding procedure Set_Model
     (Self  : access GPS_Tree_View_Record;
      Model : Gtk.Tree_Model.Gtk_Tree_Model) is
   begin
      Self.Disconnect_From_Model;
      Finalize (Self.Root.all);
      Free (Self.Root);
      Gtk_Tree_View_Record (Self.all).Set_Model (Model);
      Self.Root := new Node_Record'(null, False, Node_Vectors.Empty_Vector);
      Self.Connect_To_Model;
   end Set_Model;

   ----------------
   -- To_Lowerst --
   ----------------

   function To_Lowerst
     (Self        : not null access GPS_Tree_View_Record'Class;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Model : Gtk_Tree_Model := Self.Get_Model;
      Iter  : Gtk_Tree_Iter  := Source_Iter;
      Aux   : Gtk_Tree_Iter;

   begin
      loop
         if Model.all in Gtk_Tree_Model_Filter_Record'Class then
            Gtk_Tree_Model_Filter (Model).Convert_Iter_To_Child_Iter
              (Aux, Iter);
            Iter := Aux;
            Model := Gtk_Tree_Model_Filter (Model).Get_Model;

         elsif Model.all in Gtk_Tree_Model_Sort_Record'Class then
            Gtk_Tree_Model_Sort (Model).Convert_Iter_To_Child_Iter (Aux, Iter);
            Iter := Aux;
            Model := Gtk_Tree_Model_Sort (Model).Get_Model;

         else
            exit;
         end if;
      end loop;

      return Model.Get_Path (Iter);
   end To_Lowerst;

end GPS.Tree_View;
