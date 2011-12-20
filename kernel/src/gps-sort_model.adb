------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

package body GPS.Sort_Model is

   use Glib;
   use Gtk.Tree_Model;
   use Gtk.Tree_Model.Utils;
   use Node_Vectors;

   type Gint_Array_Access is access all Gint_Array;

   procedure Finalize (Node : in out Node_Record);
   --  Deallocate node and all children nodes

   procedure Reorder
     (Self   : not null access GPS_Sort_Model_Record'Class;
      Parent : not null Node_Access;
      Deep   : Boolean);
   --  Resort children of the specified parent node

   function Create_Iter
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Node : Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Creates instance of Gtk_Tree_Iter corresponding to the specified node

   function Get_Node
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return not null Node_Access;
   --  Returns node pointed by iter. Returns Self.Root for Null_Iter.

   function Create_Proxy_Path
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Creates proxy's path to the specified node

   function Create_Source_Path
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Creates source's path to the specified node

   procedure On_Destroy
     (Data   : System.Address;
      Object : System.Address);
   pragma Convention (C, On_Destroy);
   --  Frees all internal data

   procedure On_Row_Inserted
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Handles insertion of new row and emits "row_inserted" signal to notify
   --  view or upper level model.

   procedure On_Row_Deleted
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Handles remove of the row and emits "row_deleted" signal to notify view
   --  or upper level model.

   procedure On_Row_Changed
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Handles change of the row and emits "row_changed" signal to notify view
   --  or upper level model.

   procedure On_Row_Has_Child_Toggled
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Emits "row_has_child_toggled" signal to notify view or upper level
   --  model.

   procedure On_Rows_Reordered
     (Self   : access GPS_Sort_Model_Record'Class;
      Params : Glib.Values.GValues);
   --  Handles reordering of the rows.

   package Gtk_Tree_Iter_Vectors is
     new Ada.Containers.Vectors (Valid_Index_Subtype, Gtk_Tree_Iter);

   package Gint_Vectors is
     new Ada.Containers.Vectors (Valid_Index_Subtype, Gint);

   package GPS_Proxy_Model_Callbacks is
     new Gtk.Handlers.Callback (GPS_Sort_Model_Record);

   package Address_To_Node_Access_Conversions is
     new System.Address_To_Access_Conversions (Node_Record);

   procedure Free is
     new Ada.Unchecked_Deallocation (Gint_Array, Gint_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Record, Node_Access);

   function To_Address is
     new Ada.Unchecked_Conversion (GPS_Sort_Model, System.Address);
   function To_Object is
     new Ada.Unchecked_Conversion (System.Address, GPS_Sort_Model);

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access GPS_Sort_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : Node_Access := Self.Get_Node (Parent);

   begin
      if Node.From_Proxy.Is_Empty then
         Node := null;

      else
         Node := Node.From_Proxy.First_Element;
      end if;

      return Self.Create_Iter (Node);
   end Children;

   -----------------
   -- Create_Iter --
   -----------------

   function Create_Iter
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Node : Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      if Node /= null and then Node /= Self.Root then
         return
           Init_Tree_Iter
             (Self.Stamp,
              Address_To_Node_Access_Conversions.To_Address
                (Address_To_Node_Access_Conversions.Object_Pointer (Node)));

      else
         return Null_Iter;
      end if;
   end Create_Iter;

   -----------------------
   -- Create_Proxy_Path --
   -----------------------

   function Create_Proxy_Path
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      pragma Unreferenced (Self);

      Aux   : Node_Access := Node;
      Path  : constant Gtk_Tree_Path := Gtk_New;
      Index : Gint;

   begin
      while Aux.Parent /= null loop
         Index := Aux.Parent.From_Proxy.Find_Index (Aux);
         Prepend_Index (Path, Index);
         Aux := Aux.Parent;
      end loop;

      return Path;
   end Create_Proxy_Path;

   ------------------------
   -- Create_Source_Path --
   ------------------------

   function Create_Source_Path
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      pragma Unreferenced (Self);

      Aux   : Node_Access := Node;
      Path  : constant Gtk_Tree_Path := Gtk_New;
      Index : Gint;

   begin
      while Aux.Parent /= null loop
         Index := Aux.Parent.From_Source.Find_Index (Aux);
         Prepend_Index (Path, Index);
         Aux := Aux.Parent;
      end loop;

      return Path;
   end Create_Source_Path;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Node : in out Node_Record) is
      Child : Node_Access;

   begin
      for J in Node.From_Proxy.First_Index .. Node.From_Proxy.Last_Index loop
         Child := Node.From_Proxy.Element (J);
         Finalize (Child.all);
         Free (Child);
      end loop;

      Node.From_Proxy.Clear;
      Node.From_Source.Clear;
   end Finalize;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access GPS_Sort_Model_Record;
      Index : Glib.Gint) return Glib.GType is
   begin
      return Self.Source.Get_Column_Type (Index);
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access GPS_Sort_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Gint_Array := Get_Indices (Path);
      Node    : Node_Access         := Self.Root;

   begin
      for J in Indices'Range loop
         if Indices (J) >= Gint (Node.From_Proxy.Length) then
            return Null_Iter;
         end if;

         Node := Node.From_Proxy.Element (Indices (J));
      end loop;

      return Self.Create_Iter (Node);
   end Get_Iter;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Self : not null access constant GPS_Sort_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return not null Node_Access is
   begin
      if Iter = Null_Iter then
         return Self.Root;

      else
         if Get_Stamp (Iter) /= Self.Stamp then
            raise Program_Error;

         else
            return
              Node_Access
                (Address_To_Node_Access_Conversions.To_Pointer
                     (Get_User_Data_1 (Iter)));
         end if;
      end if;
   end Get_Node;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access GPS_Sort_Model_Record)
      return Glib.Gint is
   begin
      return Self.Source.Get_N_Columns;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access GPS_Sort_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path is
   begin
      return Self.Create_Proxy_Path (Self.Get_Node (Iter));
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access GPS_Sort_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue) is
   begin
      Self.Source.Get_Value (Self.Map_To_Source (Iter), Column, Value);
   end Get_Value;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access GPS_Sort_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is
   begin
      return not Self.Get_Node (Iter).From_Proxy.Is_Empty;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access GPS_Sort_Model_Record'Class;
      Source : not null Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Populate
        (Parent_Iter : Gtk_Tree_Iter;
         Parent_Node : Node_Access);

      --------------
      -- Populate --
      --------------

      procedure Populate
        (Parent_Iter : Gtk_Tree_Iter;
         Parent_Node : Node_Access)
      is
         Iter : Gtk_Tree_Iter := Self.Source.Children (Parent_Iter);
         Node : Node_Access;

      begin
         while Iter /= Null_Iter loop
            Node :=
              new Node_Record'
                (Parent_Node,
                 Node_Vectors.Empty_Vector,
                 Node_Vectors.Empty_Vector);
            Parent_Node.From_Source.Append (Node);
            Parent_Node.From_Proxy.Append (Node);
            Populate (Iter, Node);

            Self.Source.Next (Iter);
         end loop;
      end Populate;

   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);

      Self.Root   := new Node_Record;
      Self.Source := Source;
      Self.Stamp  := 0;

      --  Attach callbacks to source model

      GPS_Proxy_Model_Callbacks.Object_Connect
        (Self.Source,
         Gtk.Tree_Model.Signal_Row_Changed,
         GPS_Proxy_Model_Callbacks.To_Marshaller (On_Row_Changed'Access),
         Self);
      GPS_Proxy_Model_Callbacks.Object_Connect
        (Self.Source,
         Gtk.Tree_Model.Signal_Row_Inserted,
         GPS_Proxy_Model_Callbacks.To_Marshaller (On_Row_Inserted'Access),
         Self);
      GPS_Proxy_Model_Callbacks.Object_Connect
        (Self.Source,
         Gtk.Tree_Model.Signal_Row_Deleted,
         GPS_Proxy_Model_Callbacks.To_Marshaller (On_Row_Deleted'Access),
         Self);
      GPS_Proxy_Model_Callbacks.Object_Connect
        (Self.Source,
         Gtk.Tree_Model.Signal_Row_Has_Child_Toggled,
         GPS_Proxy_Model_Callbacks.To_Marshaller
           (On_Row_Has_Child_Toggled'Access),
         Self);
      GPS_Proxy_Model_Callbacks.Object_Connect
        (Self.Source,
         Gtk.Tree_Model.Signal_Rows_Reordered,
         On_Rows_Reordered'Access,
         Self);

      --  Connect destroy handler

      Self.Weak_Ref (On_Destroy'Access, To_Address (GPS_Sort_Model (Self)));

      declare
         Iter : Gtk_Tree_Iter := Self.Source.Get_Iter_First;
         Node : Node_Access;

      begin
         while Iter /= Null_Iter loop
            Node :=
              new Node_Record'
                (Self.Root,
                 Node_Vectors.Empty_Vector,
                 Node_Vectors.Empty_Vector);
            Self.Root.From_Source.Append (Node);
            Self.Root.From_Proxy.Append (Node);
            Populate (Iter, Node);

            Self.Source.Next (Iter);
         end loop;
      end;

      Self.Invalidate;
   end Initialize;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (Self : not null access GPS_Sort_Model_Record'Class) is
   begin
      Self.Stamp := Self.Stamp + 1;

      if not Self.Root.From_Proxy.Is_Empty then
         Self.Reorder (Self.Root, True);
      end if;
   end Invalidate;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than
     (Self  : not null access GPS_Sort_Model_Record;
      Left  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Right : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Less_Than;

   ------------------
   -- Map_To_Proxy --
   ------------------

   function Map_To_Proxy
     (Self        : not null access GPS_Sort_Model_Record'Class;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Source_Path    : constant Gtk_Tree_Path :=
                         Self.Source.Get_Path (Source_Iter);
      Source_Indices : constant Gint_Array := Get_Indices (Source_Path);
      Node           : Node_Access := Self.Root;

   begin
      Path_Free (Source_Path);

      for J in Source_Indices'Range loop
         Node := Node.From_Source.Element (Source_Indices (J));
      end loop;

      return Self.Create_Iter (Node);
   end Map_To_Proxy;

   -------------------
   -- Map_To_Source --
   -------------------

   function Map_To_Source
     (Self       : not null access GPS_Sort_Model_Record'Class;
      Proxy_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node        : constant not null Node_Access :=
                      Self.Get_Node (Proxy_Iter);
      Source_Path : constant Gtk_Tree_Path := Self.Create_Source_Path (Node);
      Source_Iter : constant Gtk_Tree_Iter :=
                      Self.Source.Get_Iter (Source_Path);

   begin
      Path_Free (Source_Path);

      return Source_Iter;
   end Map_To_Source;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access GPS_Sort_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is
   begin
      return Gint (Self.Get_Node (Iter).From_Proxy.Length);
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access GPS_Sort_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Position : Node_Vectors.Cursor;
      Node     : Node_Access := Self.Get_Node (Iter);

   begin
      if Node.Parent /= null and then Node /= null then
         Position := Node.Parent.From_Source.Find (Node);

         if Has_Element (Position) then
            Next (Position);
         end if;

         if Has_Element (Position) then
            Node := Element (Position);

         else
            Node := null;
         end if;
      end if;

      Iter := Self.Create_Iter (Node);
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access GPS_Sort_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Parent_Node : constant not null Node_Access := Self.Get_Node (Parent);

   begin
      if N >= 0 and then N < Gint (Parent_Node.From_Proxy.Length) then
         return Self.Create_Iter (Parent_Node.From_Proxy.Element (N));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Data   : System.Address;
      Object : System.Address)
   is
      pragma Unreferenced (Object);

      Self : constant GPS_Sort_Model := To_Object (Data);

   begin
      Finalize (Self.Root.all);
      Free (Self.Root);
   end On_Destroy;

   --------------------
   -- On_Row_Changed --
   --------------------

   procedure On_Row_Changed
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Source_Indices : constant Gint_Array := Get_Indices (Source_Path);
      Node           : Node_Access         := Self.Root;
      Index          : Gint;
      Proxy_Path     : Gtk_Tree_Path;
      Proxy_Iter     : Gtk_Tree_Iter;

   begin
      for J in Source_Indices'Range loop
         Node := Node.From_Source.Element (Source_Indices (J));
      end loop;

      Index := Node.Parent.From_Proxy.Find_Index (Node);

      if (Index > 0
          and then Self.Less_Than
            (Source_Iter,
             Self.Map_To_Source
               (Self.Create_Iter
                  (Node.Parent.From_Proxy.Element (Index - 1)))))
        or else (Index < Gint (Node.Parent.From_Proxy.Length) - 1
                 and then Self.Less_Than
                   (Self.Map_To_Source
                      (Self.Create_Iter
                         (Node.Parent.From_Proxy.Element (Index + 1))),
                    Source_Iter))
      then
         Self.Reorder (Node.Parent, False);
      end if;

      Proxy_Iter := Self.Create_Iter (Node);
      Proxy_Path := Self.Create_Proxy_Path (Node);

      Self.Row_Changed (Proxy_Path, Proxy_Iter);
      Path_Free (Proxy_Path);
   end On_Row_Changed;

   --------------------
   -- On_Row_Deleted --
   --------------------

   procedure On_Row_Deleted
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      Source_Indices : constant Gint_Array := Get_Indices (Source_Path);
      Node           : Node_Access         := Self.Root;
      Proxy_Path     : Gtk_Tree_Path;

   begin
      for J in Source_Indices'Range loop
         Node := Node.From_Source.Element (Source_Indices (J));
      end loop;

      Proxy_Path := Self.Create_Proxy_Path (Node);

      Node.Parent.From_Proxy.Delete (Node.Parent.From_Proxy.Find_Index (Node));
      Node.Parent.From_Source.Delete
        (Node.Parent.From_Source.Find_Index (Node));
      Free (Node);

      Self.Row_Deleted (Proxy_Path);
      Path_Free (Proxy_Path);
   end On_Row_Deleted;

   ------------------------------
   -- On_Row_Has_Child_Toggled --
   ------------------------------

   procedure On_Row_Has_Child_Toggled
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Source_Iter);

      Source_Indices : constant Gint_Array := Get_Indices (Source_Path);
      Node           : Node_Access         := Self.Root;
      Proxy_Path     : Gtk_Tree_Path;
      Proxy_Iter     : Gtk_Tree_Iter;

   begin
      for J in Source_Indices'Range loop
         Node := Node.From_Source.Element (Source_Indices (J));
      end loop;

      Proxy_Iter := Self.Create_Iter (Node);
      Proxy_Path := Self.Create_Proxy_Path (Node);
      Self.Row_Has_Child_Toggled (Proxy_Path, Proxy_Iter);
      Path_Free (Proxy_Path);
   end On_Row_Has_Child_Toggled;

   ---------------------
   -- On_Row_Inserted --
   ---------------------

   procedure On_Row_Inserted
     (Self        : access GPS_Sort_Model_Record'Class;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Source_Indices : constant Gint_Array := Get_Indices (Source_Path);
      Source_Index   : constant Gint       :=
                         Source_Indices (Source_Indices'Last);
      Parent         : Node_Access         := Self.Root;
      Node           : Node_Access;
      Path           : Gtk_Tree_Path;
      Iter           : Gtk_Tree_Iter;
      Index          : Gint;
      Aux            : Boolean;
      pragma Unreferenced (Aux);

   begin
      Self.Stamp := Self.Stamp + 1;

      --  Resolve parent node

      for J in Source_Indices'First .. Source_Indices'Last - 1 loop
         Parent := Parent.From_Source.Element (Source_Indices (J));
      end loop;

      Node :=
        new Node_Record'
          (Parent, Node_Vectors.Empty_Vector, Node_Vectors.Empty_Vector);

      --  Insert new node into from_source mapping

      Parent.From_Source.Insert (Source_Index, Node);

      --  Insert new node into to_source mapping

      Path := Copy (Source_Path);
      Index := 0;

      for J in reverse 0 .. Gint (Parent.From_Proxy.Length) - 1 loop
         Aux := Up (Path);

         Append_Index
           (Path,
            Parent.From_Source.Find_Index (Parent.From_Proxy.Element (J)));
         Iter := Self.Source.Get_Iter (Path);

         if not Self.Less_Than (Source_Iter, Iter) then
            Index := J + 1;

            exit;
         end if;
      end loop;

      Path_Free (Path);

      Parent.From_Proxy.Insert (Index, Node);

      --  Notify view/upper level model

      Iter := Self.Create_Iter (Node);
      Path := Self.Create_Proxy_Path (Node);
      Self.Row_Inserted (Path, Iter);
      Path_Free (Path);
   end On_Row_Inserted;

   -----------------------
   -- On_Rows_Reordered --
   -----------------------

   procedure On_Rows_Reordered
     (Self   : access GPS_Sort_Model_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Self, Params);

   begin
      raise Program_Error with "Rows reordering not supported";
   end On_Rows_Reordered;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access GPS_Sort_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return Self.Create_Iter (Self.Get_Node (Child).Parent);
   end Parent;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
     (Self   : not null access GPS_Sort_Model_Record'Class;
      Parent : not null Node_Access;
      Deep   : Boolean)
   is
      Length       : constant Gint := Gint (Parent.From_Proxy.Length);
      New_Order    : Gint_Vectors.Vector;
      Source_Iter  : Gtk_Tree_Iter;
      Source_Iters : Gtk_Tree_Iter_Vectors.Vector;
      Index        : Gint;
      Node         : Node_Access;
      Changed      : Boolean;

   begin
      New_Order.Reserve_Capacity (Ada.Containers.Count_Type (Length));
      Source_Iters.Reserve_Capacity (Ada.Containers.Count_Type (Length));
      Source_Iters.Insert
        (0,
         Self.Map_To_Source
           (Self.Create_Iter (Parent.From_Proxy.Element (0))));
      Changed := False;

      --  Fill vector by initial values

      for J in 0 .. Length - 1 loop
         New_Order.Append (J);
      end loop;

      --  Do sorting

      for J in 1 .. Length - 1 loop
         Node := Parent.From_Proxy.Element (J);
         Source_Iter := Self.Map_To_Source (Self.Create_Iter (Node));
         Source_Iters.Insert (J, Source_Iter);
         Index := -1;

         for K in reverse 0 .. J - 1 loop
            if Self.Less_Than (Source_Iters.Element (K), Source_Iter) then
               Index := K + 1;

               exit;
            end if;
         end loop;

         if Index = -1 then
            if Self.Less_Than (Source_Iter, Source_Iters.Element (0)) then
               Index := 0;
            end if;
         end if;

         if Index /= -1 and then Index /= J then
            Changed := True;
            Parent.From_Proxy.Delete (J);
            Parent.From_Proxy.Insert (Index, Node);
            Source_Iters.Delete (J);
            Source_Iters.Insert (Index, Source_Iter);
            New_Order.Delete (J);
            New_Order.Insert (Index, J);
         end if;
      end loop;

      if Changed then
         declare
            Iter : constant Gtk_Tree_Iter := Self.Create_Iter (Parent);
            Path : constant Gtk_Tree_Path := Self.Get_Path (Iter);
            Aux  : Gint_Array_Access :=
              new Gint_Array (0 .. Integer (Length - 1));

         begin
            for J in Aux'Range loop
               Aux (J) := New_Order.Element (Gint (J));
            end loop;

            Self.Rows_Reordered (Path, Iter, Aux.all);

            Path_Free (Path);
            Free (Aux);
         end;
      end if;

      if Deep then
         for J in 0 .. Gint (Parent.From_Proxy.Length) - 1 loop
            if not Parent.From_Proxy.Element (J).From_Proxy.Is_Empty then
               Self.Reorder (Parent.From_Proxy.Element (J), Deep);
            end if;
         end loop;
      end if;
   end Reorder;

end GPS.Sort_Model;
