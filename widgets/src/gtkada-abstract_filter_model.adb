------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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
with System.Address_To_Access_Conversions;

with Gtk.Handlers;
with Gtk.Tree_Model.Utils;

package body Gtkada.Abstract_Filter_Model is

   use type Glib.Gint;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use type Gtk.Tree_Model.Gtk_Tree_Model;

   function Create_Iter
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Create_Path
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path;

   function To_Node
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Node_Access;

   procedure Construct_Tree
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class);
   --  Constructs internal tree.

   function Is_Visible
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Boolean;
   --  Lookup whether specified node and all its parents are visible.

   procedure On_Row_Changed
     (Self        : access Gtk_Abstract_Filter_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Handles "row-changed" signal from source model.

   procedure On_Row_Inserted_Callback
     (Self        : access Gtk_Abstract_Filter_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Handles "row-inserted" signal of source model.

   procedure On_Row_Deleted_Callback
     (Self        : access Gtk_Abstract_Filter_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Handles "row-deleted" signal of source model.

   procedure On_Rows_Reordered
     (Self   : access Gtk_Abstract_Filter_Model_Record'Class;
      Params : Glib.Values.GValues);
   --  Handles reordering of the rows.

   procedure On_Destroy
     (Data   : System.Address;
      Object : System.Address);
   pragma Convention (C, On_Destroy);
   --  Frees all internal data

   function Visible_Child
     (Self   : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Parent : not null Node_Access;
      Index  : Glib.Gint) return Node_Access;
   --  Returns visible child at the specified position.

   function Has_Visible_Child
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Boolean;
   --  Returns True when at least one of children nodes is visible. It doesn't
   --  recompute visibility of nodes.

   procedure Hide
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access);
   --  Hides all visible nodes. Nodes' state is changed to Unknown. This
   --  subprogram emits "row-deleted" and "row-has-child-toggled" signals.

   procedure Show_Visible
     (Self        : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Parent_Node : not null Node_Access;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Shows all visible children of the specified node. Source_Path and
   --  Source_Iter are path and iter of the first child node of Parent_Node.

   procedure Deep_Free
     (Self : access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access);
   --  Deallocate node and all its children.

   package Conversions is
     new System.Address_To_Access_Conversions (Node_Record);

   package Gtk_Abstract_Filter_Model_Callbacks is
     new Gtk.Handlers.Callback (Gtk_Abstract_Filter_Model_Record);

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Record, Node_Access);

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Gtk_Abstract_Filter_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Parent_Node : constant Node_Access := Self.To_Node (Parent);
      Position    : Node_Vectors.Cursor := Parent_Node.Children.First;

   begin
      while Node_Vectors.Has_Element (Position) loop
         if Node_Vectors.Element (Position).Visibility = Visible then
            return Self.Create_Iter (Node_Vectors.Element (Position));
         end if;

         Node_Vectors.Next (Position);
      end loop;

      return Gtk.Tree_Model.Null_Iter;
   end Children;

   --------------------
   -- Construct_Tree --
   --------------------

   procedure Construct_Tree
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class)
   is
      procedure Construct
        (Parent_Node : not null Node_Access;
         Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
         Source_Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
      --  Construct internal nodes for children of the specified node.
      --  Source_Path and Source_Iter are point for the first child of the
      --  specified parent node.

      ---------------
      -- Construct --
      ---------------

      procedure Construct
        (Parent_Node : not null Node_Access;
         Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
         Source_Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
      is
         Node         : Node_Access;
         Source_Child : Gtk.Tree_Model.Gtk_Tree_Iter;
         Dummy        : Boolean;
         pragma Unreferenced (Dummy);

      begin
         while Source_Iter /= Gtk.Tree_Model.Null_Iter loop
            --  Create corresponding node and connect it to parent node.

            Node := new Node_Record;
            Node.Parent := Parent_Node;
            Parent_Node.Children.Append (Node);

            --  Determine node's visibility.

            if Self.Is_Visible (Parent_Node) then
               if Self.Is_Visible (Source_Path, Source_Iter) then
                  Node.Visibility := Visible;

               else
                  Node.Visibility := Invisible;
               end if;

            else
               Node.Visibility := Unknown;
            end if;

            --  Notify about added children.

            if Node.Visibility = Visible then
               declare
                  Parent_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Self.Create_Path (Node);
                  Parent_Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                    Self.Create_Iter (Node);
                  Indices     : constant Glib.Gint_Array :=
                    Gtk.Tree_Model.Get_Indices (Parent_Path);

               begin
                  Self.Row_Inserted (Parent_Path, Parent_Iter);

                  if Indices (Indices'Last) = 0 then
                     --  Notify about change of children status.

                     Self.Row_Has_Child_Toggled (Parent_Path, Parent_Iter);
                  end if;

                  Gtk.Tree_Model.Path_Free (Parent_Path);
               end;
            end if;

            --  Process children if any.

            if Self.Model.Has_Child (Source_Iter) then
               Source_Child := Self.Model.Children (Source_Iter);
               Gtk.Tree_Model.Down (Source_Path);
               Construct (Node, Source_Path, Source_Child);
               Dummy := Gtk.Tree_Model.Up (Source_Path);
            end if;

            Self.Model.Next (Source_Iter);
            Gtk.Tree_Model.Next (Source_Path);
         end loop;
      end Construct;

      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Self.Model.Get_Iter_First;
      Source_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
        Gtk.Tree_Model.Gtk_New_First;

   begin
      Construct (Self.Root, Source_Path, Source_Iter);
      Gtk.Tree_Model.Path_Free (Source_Path);
   end Construct_Tree;

   -----------------
   -- Create_Iter --
   -----------------

   function Create_Iter
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Current : Node_Access := Node;

   begin
      --  Return Null_Iter when node or one of its parents are invisible.

      while Current.Parent /= null loop
         if Current.Visibility /= Visible then
            return Gtk.Tree_Model.Null_Iter;
         end if;

         Current := Current.Parent;
      end loop;

      if Node.Parent = null then
         --  Return Null_Iter for root node.

         return Gtk.Tree_Model.Null_Iter;

      else
         return
           Gtk.Tree_Model.Utils.Init_Tree_Iter
             (Self.Stamp,
              Conversions.To_Address (Conversions.Object_Pointer (Node)));
      end if;
   end Create_Iter;

   -----------------
   -- Create_Path --
   -----------------

   function Create_Path
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      pragma Unreferenced (Self);

      Path     : Gtk.Tree_Model.Gtk_Tree_Path;
      Index    : Natural;
      Current  : Node_Access := Node;
      Position : Node_Vectors.Cursor;

   begin
      --  For root node returns invalid path. Also, returns invalid path when
      --  specified node is invisible.

      if Node.Parent = null or else Node.Visibility /= Visible then
         return null;
      end if;

      Path := Gtk.Tree_Model.Gtk_New;

      while Current.Parent /= null loop
         --  Return invalid path when parent node is invisible (visibility of
         --  current node was checked already before this loop or at previous
         --  iteration).

         if Current.Parent.Visibility /= Visible then
            Gtk.Tree_Model.Path_Free (Path);

            return null;
         end if;

         Index := 0;
         Position := Current.Parent.Children.First;

         Children : while Node_Vectors.Has_Element (Position) loop
            if Node_Vectors.Element (Position) = Current then
               Gtk.Tree_Model.Prepend_Index (Path, Glib.Gint (Index));

               exit Children;
            end if;

            if Node_Vectors.Element (Position).Visibility = Visible then
               Index := Index + 1;
            end if;

            Node_Vectors.Next (Position);
         end loop Children;

         Current := Current.Parent;
      end loop;

      return Path;
   end Create_Path;

   ---------------
   -- Deep_Free --
   ---------------

   procedure Deep_Free
     (Self : access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access)
   is
      Aux      : Node_Access         := Node;
      Position : Node_Vectors.Cursor := Node.Children.First;

   begin
      while Node_Vectors.Has_Element (Position) loop
         Self.Deep_Free (Node_Vectors.Element (Position));
         Node_Vectors.Next (Position);
      end loop;

      Node.Children.Clear;

      if Node /= Self.Root then
         Free (Aux);
      end if;
   end Deep_Free;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Gtk_Abstract_Filter_Model_Record;
      Index : Glib.Gint) return Glib.GType is
   begin
      if Self.Model = null then
         return Glib.GType_Invalid;

      else
         return Self.Model.Get_Column_Type (Index);
      end if;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
      Node    : Node_Access := Self.Root;

   begin
      for J in Indices'Range loop
         Node := Self.Visible_Child (Node, Indices (J));
      end loop;

      return Self.Create_Iter (Node);
   end Get_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Gtk_Abstract_Filter_Model_Record)
      return Glib.Gint is
   begin
      if Self.Model = null then
         return 0;

      else
         return Self.Model.Get_N_Columns;
      end if;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Gtk_Abstract_Filter_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue) is
   begin
      if Self.Model /= null then
         Self.Model.Get_Value (Self.Map_To_Source (Iter), Column, Value);
      end if;
   end Get_Value;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Node : constant Node_Access := Self.To_Node (Iter);

   begin
      return Self.Create_Path (Node);
   end Get_Path;

   ----------------------
   -- Get_Source_Model --
   ----------------------

   not overriding function Get_Source_Model
     (Self  : not null access Gtk_Abstract_Filter_Model_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model is
   begin
      return Self.Model;
   end Get_Source_Model;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Node     : constant Node_Access := Self.To_Node (Iter);
      Position : Node_Vectors.Cursor := Node.Children.First;

   begin
      while Node_Vectors.Has_Element (Position) loop
         if Node_Vectors.Element (Position).Visibility = Visible then
            return True;
         end if;

         Node_Vectors.Next (Position);
      end loop;

      return False;
   end Has_Child;

   -----------------------
   -- Has_Visible_Child --
   -----------------------

   function Has_Visible_Child
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Boolean
   is
      Position : Node_Vectors.Cursor := Node.Children.First;

   begin
      if not Self.Is_Visible (Node) then
         --  If parent node is not visible none of its children are visible.

         return False;
      end if;

      while Node_Vectors.Has_Element (Position) loop
         if Node_Vectors.Element (Position).Visibility = Visible then
            return True;
         end if;

         Node_Vectors.Next (Position);
      end loop;

      return False;
   end Has_Visible_Child;

   ----------
   -- Hide --
   ----------

   procedure Hide
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Path;

      Proxy_Path       : Gtk.Tree_Model.Gtk_Tree_Path := null;
      Position         : Node_Vectors.Cursor := Node.Children.First;
      Had_Visible_Rows : constant Boolean :=
        Node.Parent /= null and then Self.Has_Visible_Child (Node.Parent);
      Dummy            : Boolean;
      pragma Unreferenced (Dummy);

   begin
      if not Self.Is_Visible (Node) then
         --  Returns immidiately when node is not visible

         return;
      end if;

      --  Construct path for the first children of node when node is
      --  visible.

      Proxy_Path := Self.Create_Path (Node);

      if Proxy_Path = null then
         Proxy_Path := Gtk.Tree_Model.Gtk_New_First;

      else
         Gtk.Tree_Model.Down (Proxy_Path);
      end if;

      --  Hide all children.

      while Node_Vectors.Has_Element (Position) loop
         if Node_Vectors.Element (Position).Visibility = Visible then
            Self.Hide (Node_Vectors.Element (Position));
         end if;

         Node_Vectors.Next (Position);
      end loop;

      --  Mark node as invalidated (except root node, which is visible always).

      if Self.Root /= Node then
         Node.Visibility := Unknown;

         --  Notify remove of the row.

         Dummy := Gtk.Tree_Model.Up (Proxy_Path);
         Self.Row_Deleted (Proxy_Path);

         --  Notify change of status of Has_Children of parent node.

         if Had_Visible_Rows
           and then not Self.Has_Visible_Child (Node.Parent)
         then
            Dummy := Gtk.Tree_Model.Up (Proxy_Path);

            if Gtk.Tree_Model.Get_Depth (Proxy_Path) /= 0 then
               Self.Row_Has_Child_Toggled
                 (Proxy_Path, Self.Create_Iter (Node.Parent));
            end if;
         end if;
      end if;

      Gtk.Tree_Model.Path_Free (Proxy_Path);
   end Hide;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class)
   is
      function To_Address is
        new Ada.Unchecked_Conversion
              (Gtk_Abstract_Filter_Model, System.Address);

   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);

      Self.Model := null;
      Self.Stamp := 1;
      Self.Root  := new Node_Record;
      Self.Root.Visibility := Visible;

      Self.Weak_Ref
        (On_Destroy'Access, To_Address (Gtk_Abstract_Filter_Model (Self)));
   end Initialize;

   ----------------
   -- Invalidate --
   ----------------

   not overriding procedure Invalidate
     (Self : not null access Gtk_Abstract_Filter_Model_Record)
   is
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
        Gtk.Tree_Model.Gtk_New_First;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Self.Model.Get_Iter_First;

   begin
      Self.Hide (Self.Root);
      Self.Show_Visible (Self.Root, Path, Iter);
      Gtk.Tree_Model.Path_Free (Path);
   end Invalidate;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Self        : not null access Gtk_Abstract_Filter_Model_Record;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Source_Path);
      pragma Unreferenced (Source_Iter);

   begin
      return True;
   end Is_Visible;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Node : not null Node_Access) return Boolean
   is
      pragma Unreferenced (Self);

      Current : Node_Access := Node;

   begin
      while Current.Parent /= null loop
         if Current.Visibility /= Visible then
            return False;
         end if;

         Current := Current.Parent;
      end loop;

      return True;
   end Is_Visible;

   -------------------
   -- Map_To_Source --
   -------------------

   not overriding function Map_To_Source
     (Self : not null access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node   : Node_Access := Self.To_Node (Iter);
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Result : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      --  Root node is mapped to Null_Iter.

      if Node.Parent = null then
         return Gtk.Tree_Model.Null_Iter;
      end if;

      --  Construct path in source model space.

      Path := Gtk.Tree_Model.Gtk_New;

      while Node.Parent /= null loop
         Gtk.Tree_Model.Prepend_Index
           (Path, Glib.Gint (Node.Parent.Children.Find_Index (Node)));

         Node := Node.Parent;
      end loop;

      --  Obtain source model's iter for constructed path.

      Result := Self.Model.Get_Iter (Path);

      --  Cleanup.

      Gtk.Tree_Model.Path_Free (Path);

      return Result;
   end Map_To_Source;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint
   is
      Node     : constant Node_Access := Self.To_Node (Iter);
      Position : Node_Vectors.Cursor  := Node.Children.First;
      Count    : Natural              := 0;

   begin
      while Node_Vectors.Has_Element (Position) loop
         if Node_Vectors.Element (Position).Visibility = Visible then
            Count := Count + 1;
         end if;

         Node_Vectors.Next (Position);
      end loop;

      return Glib.Gint (Count);
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Node     : constant Node_Access := Self.To_Node (Iter);
      Position : Node_Vectors.Cursor;

   begin
      Iter := Gtk.Tree_Model.Null_Iter;

      --  GtkTreeView can request next iter for null iter, protect from this
      --  case.

      if Node.Parent /= null then
         Position := Node.Parent.Children.Find (Node);

         --  Looking for next visible node and return it.

         Node_Vectors.Next (Position);

         while Node_Vectors.Has_Element (Position) loop
            if Node_Vectors.Element (Position).Visibility = Visible then
               Iter := Self.Create_Iter (Node_Vectors.Element (Position));

               exit;
            end if;

            Node_Vectors.Next (Position);
         end loop;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Gtk_Abstract_Filter_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return Self.Create_Iter (Self.Visible_Child (Self.To_Node (Parent), N));
   end Nth_Child;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Data   : System.Address;
      Object : System.Address)
   is
      pragma Unreferenced (Object);

      function To_Object is
        new Ada.Unchecked_Conversion
                  (System.Address, Gtk_Abstract_Filter_Model);

      Self : constant Gtk_Abstract_Filter_Model := To_Object (Data);

   begin
      Self.Deep_Free (Self.Root);
      Free (Self.Root);
   end On_Destroy;

   --------------------
   -- On_Row_Changed --
   --------------------

   procedure On_Row_Changed
     (Self        : access Gtk_Abstract_Filter_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Indices    : constant Glib.Gint_Array :=
        Gtk.Tree_Model.Get_Indices (Source_Path);
      Node       : Node_Access := Self.Root;
      Is_Visible : Boolean;
      Has_Rows   : Boolean;

   begin
      Self.Stamp := Self.Stamp + 1;

      --  Lookup for node.

      for J in Indices'Range loop
         Node := Node.Children (Natural (Indices (J)));
      end loop;

      --  Recompute visibility when parent node is visible

      if Self.Is_Visible (Node.Parent) then
         Is_Visible := Self.Is_Visible (Source_Path, Source_Iter);

         if Node.Visibility = Visible xor Is_Visible then
            if Is_Visible then
               --  Row is shown.

               Has_Rows := Self.Has_Visible_Child (Node);

               Node.Visibility := Visible;

               --  Do notifications.

               declare
                  Node_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Self.Create_Path (Node);
                  Node_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
                    Self.Create_Iter (Node);
                  Dummy     : Boolean;
                  pragma Unreferenced (Dummy);

               begin
                  Self.Row_Inserted (Node_Path, Node_Iter);

                  if not Has_Rows and Node.Parent.Parent /= null then
                     --  Notify about change of children status.

                     Dummy := Gtk.Tree_Model.Up (Node_Path);
                     Node_Iter := Self.Parent (Node_Iter);
                     Self.Row_Has_Child_Toggled (Node_Path, Node_Iter);
                  end if;

                  Gtk.Tree_Model.Path_Free (Node_Path);
               end;

               --  Show children.

               if not Node.Children.Is_Empty then
                  declare
                     Child_Source_Path : constant
                       Gtk.Tree_Model.Gtk_Tree_Path :=
                         Gtk.Tree_Model.Copy (Source_Path);
                     Child_Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
                       Self.Model.Children (Source_Iter);

                  begin
                     Gtk.Tree_Model.Down (Child_Source_Path);

                     Self.Show_Visible
                       (Node, Child_Source_Path, Child_Source_Iter);

                     Gtk.Tree_Model.Path_Free (Child_Source_Path);
                  end;
               end if;

            else
               --  Row is hided

               Self.Hide (Node);
            end if;
         end if;
      end if;
   end On_Row_Changed;

   -----------------------------
   -- On_Row_Deleted_Callback --
   -----------------------------

   procedure On_Row_Deleted_Callback
     (Self        : access Gtk_Abstract_Filter_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      Indices : constant Glib.Gint_Array :=
        Gtk.Tree_Model.Get_Indices (Source_Path);
      Parent  : Node_Access := Self.Root;
      Child   : Node_Access;

   begin
      Self.Stamp := Self.Stamp + 1;

      --  Lookup for parent node.

      for J in Indices'First .. Indices'Last - 1 loop
         Parent := Parent.Children (Natural (Indices (J)));
      end loop;

      --  Obtain children node.

      Child := Parent.Children (Natural (Indices (Indices'Last)));

      --  Hide child node.

      Self.Hide (Child);

      --  Delete node from parent's list.

      Parent.Children.Delete (Natural (Indices (Indices'Last)));

      --  Deallocate node and its children recursively.

      Self.Deep_Free (Child);
   end On_Row_Deleted_Callback;

   ------------------------------
   -- On_Row_Inserted_Callback --
   ------------------------------

   procedure On_Row_Inserted_Callback
     (Self        : access Gtk_Abstract_Filter_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Parent  : Node_Access := Self.Root;
      Indices : constant Glib.Gint_Array :=
        Gtk.Tree_Model.Get_Indices (Source_Path);
      Node    : Node_Access;

   begin
      Self.Stamp := Self.Stamp + 1;

      for J in Indices'First .. Indices'Last - 1 loop
         Parent := Parent.Children.Element (Natural (Indices (J)));
      end loop;

      Node := new Node_Record;
      Node.Parent := Parent;
      Parent.Children.Insert (Natural (Indices (Indices'Last)), Node);

      --  Compute visibility of new node.

      if Self.Is_Visible (Parent) then
         if Self.Is_Visible (Source_Path, Source_Iter) then
            Node.Visibility := Visible;

         else
            Node.Visibility := Invisible;
         end if;

      else
         Node.Visibility := Unknown;
      end if;

      --  Do notifications when necessary.

      if Node.Visibility = Visible then
         declare
            Node_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
              Self.Create_Path (Node);
            Node_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
              Self.Create_Iter (Node);
            Dummy     : Boolean;
            pragma Unreferenced (Dummy);

         begin
            Self.Row_Inserted (Node_Path, Node_Iter);

            Dummy := Gtk.Tree_Model.Up (Node_Path);
            Node_Iter := Self.Parent (Node_Iter);

            if Self.N_Children (Node_Iter) = 1 then
               --  Notify about change of children status.

               if Node_Iter /= Gtk.Tree_Model.Null_Iter then
                  Self.Row_Has_Child_Toggled (Node_Path, Node_Iter);
               end if;
            end if;

            Gtk.Tree_Model.Path_Free (Node_Path);
         end;
      end if;
   end On_Row_Inserted_Callback;

   -----------------------
   -- On_Rows_Reordered --
   -----------------------

   procedure On_Rows_Reordered
     (Self   : access Gtk_Abstract_Filter_Model_Record'Class;
      Params : Glib.Values.GValues)
   is
      Source_Path    : constant Gtk.Tree_Model.Gtk_Tree_Path :=
        Gtk.Tree_Model.Get_Tree_Path (Glib.Values.Nth (Params, 1));
      Source_Iter    : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
        Gtk.Tree_Model.Get_Tree_Iter (Glib.Values.Nth (Params, 2));
      Address        : constant System.Address :=
        Glib.Values.Get_Address (Glib.Values.Nth (Params, 3));
      Length         : constant Natural :=
        Natural (Self.Get_Source_Model.N_Children (Source_Iter));

      subtype New_Order_Array is Glib.Gint_Array (0 .. Length - 1);
      New_Source_Order : New_Order_Array;
      for New_Source_Order'Address use Address;

      function Find_Visible_Index
        (Children : Node_Vectors.Vector;
         Node     : not null Node_Access) return Integer;

      ------------------------
      -- Find_Visible_Index --
      ------------------------

      function Find_Visible_Index
        (Children : Node_Vectors.Vector;
         Node     : not null Node_Access) return Integer
      is
         Position : Node_Vectors.Cursor := Children.First;
         Index    : Integer := 0;

      begin
         while Node_Vectors.Has_Element (Position) loop
            if Node_Vectors.Element (Position) = Node then
               if Node_Vectors.Element (Position).Visibility = Visible then
                  return Index;

               else
                  return -1;
               end if;

            elsif Node_Vectors.Element (Position).Visibility = Visible then
               Index := Index + 1;
            end if;

            Node_Vectors.Next (Position);
         end loop;

         return -1;
      end Find_Visible_Index;

      Indices    : constant Glib.Gint_Array :=
        Gtk.Tree_Model.Get_Indices (Source_Path);
      Node       : Node_Access := Self.Root;
      Child_Node : Node_Access;
      Aux        : Node_Vectors.Vector;
      Is_Visible : Boolean;

      New_Order : New_Order_Array;

   begin
      --  Lookup for node.

      for J in Indices'Range loop
         Node := Node.Children (Natural (Indices (J)));
      end loop;

      Is_Visible := Self.Is_Visible (Node);

      --  Reorder rows.

      Aux.Reserve_Capacity (Ada.Containers.Count_Type (Length));
      Aux.Set_Length (Ada.Containers.Count_Type (Length));

      for J in New_Source_Order'Range loop
         Child_Node := Node.Children.Element (Natural (New_Source_Order (J)));
         Aux.Replace_Element (J, Child_Node);
      end loop;

      --  Compute reordering map for children rows.

      if Is_Visible then
         declare
            Position : Node_Vectors.Cursor := Node.Children.First;

         begin
            while Node_Vectors.Has_Element (Position) loop
               Child_Node := Node_Vectors.Element (Position);

               if Child_Node.Visibility = Visible then
                  New_Order (Find_Visible_Index (Aux, Child_Node)) :=
                    Glib.Gint
                      (Find_Visible_Index (Node.Children, Child_Node));
               end if;

               Node_Vectors.Next (Position);
            end loop;
         end;
      end if;

      Node.Children := Aux;

      --  Do notification when necessary.

      if Is_Visible then
         declare
            Proxy_Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
              Self.Create_Iter (Node);
            Proxy_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
              Self.Get_Path (Proxy_Iter);

         begin
            Self.Rows_Reordered (Proxy_Path, Proxy_Iter, New_Order);
            Gtk.Tree_Model.Path_Free (Proxy_Path);
         end;
      end if;
   end On_Rows_Reordered;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Gtk_Abstract_Filter_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : constant Node_Access := Self.To_Node (Child);

   begin
      return Self.Create_Iter (Node.Parent);
   end Parent;

   ----------------------
   -- Set_Source_Model --
   ----------------------

   not overriding procedure Set_Source_Model
     (Self  : not null access Gtk_Abstract_Filter_Model_Record;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Self.Model := Gtk.Tree_Model.Gtk_Tree_Model (Model);
      Gtk_Abstract_Filter_Model_Callbacks.Object_Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Row_Changed,
         Gtk_Abstract_Filter_Model_Callbacks.To_Marshaller
           (On_Row_Changed'Access),
         Self);
      Gtk_Abstract_Filter_Model_Callbacks.Object_Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Row_Inserted,
         Gtk_Abstract_Filter_Model_Callbacks.To_Marshaller
           (On_Row_Inserted_Callback'Access),
         Self);
      Gtk_Abstract_Filter_Model_Callbacks.Object_Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Row_Deleted,
         Gtk_Abstract_Filter_Model_Callbacks.To_Marshaller
           (On_Row_Deleted_Callback'Access),
         Self);
      Gtk_Abstract_Filter_Model_Callbacks.Object_Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Rows_Reordered,
         On_Rows_Reordered'Access,
         Self);
      Construct_Tree (Self);
   end Set_Source_Model;

   ------------------
   -- Show_Visible --
   ------------------

   procedure Show_Visible
     (Self        : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Parent_Node : not null Node_Access;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Position    : Node_Vectors.Cursor := Parent_Node.Children.First;
      Node        : Node_Access;
      Has_Row     : Boolean := False;

   begin
      while Node_Vectors.Has_Element (Position) loop
         Node := Node_Vectors.Element (Position);

         --  Compute visibility of the node.

         if Self.Is_Visible (Parent_Node) then
            if Self.Is_Visible (Source_Path, Source_Iter) then
               Node.Visibility := Visible;

            else
               Node.Visibility := Invisible;
            end if;

         else
            Node.Visibility := Unknown;
         end if;

         if Node.Visibility = Visible then
            --  Do notifications when necessary.

            declare
               Node_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                 Self.Create_Path (Node);
               Node_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
                 Self.Create_Iter (Node);
               Dummy     : Boolean;
               pragma Unreferenced (Dummy);

            begin
               Self.Row_Inserted (Node_Path, Node_Iter);

               if not Has_Row and Parent_Node.Parent /= null then
                  --  Notify about change of children status.

                  Dummy := Gtk.Tree_Model.Up (Node_Path);
                  Node_Iter := Self.Parent (Node_Iter);
                  Self.Row_Has_Child_Toggled (Node_Path, Node_Iter);
                  Has_Row := True;
               end if;

               Gtk.Tree_Model.Path_Free (Node_Path);
            end;

            --  Show children.

            if not Node.Children.Is_Empty then
               declare
                  Child_Source_Path : constant
                    Gtk.Tree_Model.Gtk_Tree_Path :=
                      Gtk.Tree_Model.Copy (Source_Path);
                  Child_Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
                    Self.Model.Children (Source_Iter);

               begin
                  Gtk.Tree_Model.Down (Child_Source_Path);

                  Self.Show_Visible
                    (Node, Child_Source_Path, Child_Source_Iter);

                  Gtk.Tree_Model.Path_Free (Child_Source_Path);
               end;
            end if;
         end if;

         Gtk.Tree_Model.Next (Source_Path);
         Self.Model.Next (Source_Iter);
         Node_Vectors.Next (Position);
      end loop;
   end Show_Visible;

   -------------
   -- To_Node --
   -------------

   function To_Node
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Node_Access is
   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return Self.Root;
      end if;

      if Gtk.Tree_Model.Utils.Get_Stamp (Iter) /= Self.Stamp then
         return null;
      end if;

      return
        Node_Access
          (Conversions.To_Pointer
               (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter)));
   end To_Node;

   -------------------
   -- Visible_Child --
   -------------------

   function Visible_Child
     (Self   : not null access Gtk_Abstract_Filter_Model_Record'Class;
      Parent : not null Node_Access;
      Index  : Glib.Gint) return Node_Access
   is
      Current  : Natural             := 0;
      Position : Node_Vectors.Cursor := Parent.Children.First;

   begin
      while Node_Vectors.Has_Element (Position) loop
         if Node_Vectors.Element (Position).Visibility = Visible then
            if Natural (Index) = Current then
               return Node_Vectors.Element (Position);
            end if;

            Current := Current + 1;
         end if;

         Node_Vectors.Next (Position);
      end loop;

      return Self.Root;
   end Visible_Child;

end Gtkada.Abstract_Filter_Model;
