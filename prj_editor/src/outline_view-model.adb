------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with System;                      use System;
with Ada.Calendar;                use Ada.Calendar;
with Ada.Containers.Bounded_Hashed_Maps;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Glib.Object;                 use Glib.Object;
with Glib.Convert;                use Glib.Convert;
with Gtk.Tree_Model.Utils;        use Gtk.Tree_Model.Utils;

with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

with Basic_Types;                 use Basic_Types;
with GPS.Search;                  use GPS.Search;
with Language.Icons;              use Language.Icons;
with XML_Utils;                   use XML_Utils;

package body Outline_View.Model is

   use Sorted_Node_Vector;

   Me : constant Trace_Handle := Create ("OUTLINE");

   Sort_Nodes_Threshold : constant := 1000;
   --  Above a certain threshold, using Sorted_Insert to insert new nodes in
   --  the outline tree becomes slower than sorting the children beforehand.
   --  This is an heuristically determined threshold that has shown to give
   --  good performance

   --  This array provide a way of sorting / grouping entities when order
   --  is required.
   Sort_Entities : constant array (Language_Category) of Natural :=
     (Dependency_Category => 1,
      Namespace_Category  => 2,
      Type_Category       => 3,
      Subprogram_Category => 4,
      Data_Category       => 5,
      others              => 99);

   procedure Free is new Ada.Unchecked_Deallocation
     (Sorted_Node, Sorted_Node_Access);

   function Construct_Filter
     (Model : not null access Outline_Model_Record'Class;
      Node  : Semantic_Node'Class) return Boolean;
   --  Return False if the construct should be filtered out

   procedure Add_Recursive
     (Model                : access Outline_Model_Record'Class;
      Sem_Node, Sem_Parent : Semantic_Node'Class;
      Sort                 : Boolean := True);
   --  Add new nodes for New_Obj and its nested entities

   function New_Iter (Iter : Sorted_Node_Access) return Gtk_Tree_Iter;
   --  Create a new iterator out of a node

   function Nth_Child
     (Model : access Outline_Model_Record;
      Node  : Sorted_Node_Access;
      Nth   : Gint) return Sorted_Node_Access;
   --  Return the nth child of this node

   function Get_Path
     (Self : access Outline_Model_Record'Class;
      Node : Sorted_Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Retrieves the path of a node

   procedure Clear_Nodes
     (Model : access Outline_Model_Record'Class; Root : Sorted_Node_Access);
   --  Remove recursively all children nodes of the root given in parameter

   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      Node   : Semantic_Node'Class) return Sorted_Node_Access;
   --  Returns the Sorted_Node_Access that corresponds to the Semantic_Node
   --  Node

   function Get_Node_Next_Part
     (Model : access Outline_Model_Record'Class;
      Node  : Semantic_Node'Class) return Sorted_Node_Access;
   --  Same as Get_Node, but for the body or full view of the entity.

   procedure Add_Array_Recursive
     (Model      : access Outline_Model_Record'Class;
      Sem_Nodes  : Semantic_Node_Array'Class;
      Sem_Parent : Semantic_Node'Class);

   function S_Unique_Id (N : Semantic_Node'Class) return String
   is (Get (N.Unique_Id).all & (if N.Is_Declaration then "" else "B"));
   --  Unique Id function that adds the information of whether the entity is a
   --  spec or a body

   function S_Unique_Id (N : Semantic_Node_Info) return String
   is (Get (N.Unique_Id).all & (if N.Is_Decl then "" else "B"));
   --  Unique Id function that adds the information of whether the entity is a
   --  spec or a body

   procedure Reindex (Vec  : Sorted_Node_Vector.Vector; From : Natural := 0);
   --  Reindex every components in Vec starting at index From. Reindex
   --  means that the Index_In_Siblings components is gonna be set to
   --  the corresponding index in the vector

   procedure Clean_Node
     (Model : Outline_Model;
      Node  : in out Sorted_Node_Access);
   --  Completely clean the node, eg.:
   --  - Recursively clean every child of node
   --  - Free node

   function Element_Or_Null
     (Map : Sem_To_Tree_Maps.Map; Key : String) return Sorted_Node_Access
     with Inline_Always;
   --  Helper for the semantic_tree_node to sorted node maps, that will return
   --  the element if it exists, or null

   function Lt
     (Left, Right : Semantic_Node_Info; Sorted : Boolean) return Boolean;
   --  Less_Than comparison for Semantic_Node_Infos

   -------------
   -- Reindex --
   -------------

   procedure Reindex (Vec  : Sorted_Node_Vector.Vector; From : Natural := 0) is
   begin
      for I in From .. Vec.Last_Index loop
         Vec.Element (I).Index_In_Siblings := I;
      end loop;
   end Reindex;

   ---------------------
   -- Element_Or_Null --
   ---------------------

   function Element_Or_Null
     (Map : Sem_To_Tree_Maps.Map; Key : String) return Sorted_Node_Access
   is
      use Sem_To_Tree_Maps;
      C : constant Sem_To_Tree_Maps.Cursor := Map.Find (Key);
   begin
      return (if Has_Element (C) then Element (C) else null);
   end Element_Or_Null;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      Node     : Semantic_Node'Class) return Sorted_Node_Access
   is
     (Element_Or_Null (Model.Sem_To_Tree_Nodes, S_Unique_Id (Node)));

   ------------------------
   -- Get_Node_Next_Part --
   ------------------------

   function Get_Node_Next_Part
     (Model  : access Outline_Model_Record'Class;
      Node     : Semantic_Node'Class) return Sorted_Node_Access
   is
      C : constant Semantic_Node'Class := Node.Definition;
   begin
      if Model.Filter.Group_Spec_And_Body
        and then C.Is_Valid and then C.File = Node.File
      then
         return
           (if Node.Is_Declaration
            then Get_Node (Model, C)
            else Element_Or_Null
              (Model.Sem_To_Tree_Nodes, Get (Node.Unique_Id).all));
      end if;

      return null;
   end Get_Node_Next_Part;

   ----------------
   -- Clean_Node --
   ----------------

   procedure Clean_Node
     (Model : Outline_Model;
      Node  : in out Sorted_Node_Access)
   is
      Path : constant Gtk_Tree_Path := Get_Path (Model, Node);
   begin
      --  Kill the children if any
      Clear_Nodes (Model, Node);

      --  Remove the semantic to tree node mapping from the model
      if Node.Spec_Info /= No_Node_Info then
         Model.Sem_To_Tree_Nodes.Exclude (S_Unique_Id (Node.Spec_Info));
      end if;
      if Node.Body_Info /= No_Node_Info then
         Model.Sem_To_Tree_Nodes.Exclude (S_Unique_Id (Node.Body_Info));
      end if;

      --  Remove this node from the parent's list of children
      if Node.Parent /= null then
         Node.Parent.Children.Delete (Node.Index_In_Siblings);
      end if;

      --  Reindex the remaining children
      Reindex (Node.Parent.Children, Node.Index_In_Siblings);

      --  Now unlink & destroy this node
      if Path /= Null_Gtk_Tree_Path then
         Row_Deleted (+Model, Path);
      end if;

      Path_Free (Path);
      Free (Node);
   end Clean_Node;

   ----------------------
   -- Construct_Filter --
   ----------------------

   function Construct_Filter
     (Model : not null access Outline_Model_Record'Class;
      Node  : Semantic_Node'Class) return Boolean
   is (
       --  Don't show entities with no name
       Node.Name /= No_Symbol

       --  Static category filter
       and then (Node.Category in Cat_Package .. Cat_Structure
                     | Cat_Field | Cat_Variable | Type_Category | Cat_With)

       --  Dynamic category filters
       and then not
         ((Node.Category in Type_Category and then Model.Filter.Hide_Types)
          or else
            (Node.Category in Data_Category
             and then Model.Filter.Hide_Objects)
          or else
            (Node.Category in Cat_Task | Cat_Protected
             and then Model.Filter.Hide_Tasks)
          or else
            (Node.Category in Subprogram_Category
             and then Node.Is_Declaration
             and then Model.Filter.Hide_Declarations)
          or else
            (Node.Category = Cat_With and then Model.Filter.Hide_Withes))

       --  Filter entities by name if the user has entered such a filter
       and then (Model.Filter_Pattern = null or else
                 Model.Filter_Pattern.Start
                   (Get (Node.Name).all) /= No_Match));

   --------
   -- Lt --
   --------

   function Lt
     (Left, Right : Semantic_Node_Info; Sorted : Boolean) return Boolean
   is
      function Less_Than
        (L, R : String) return Boolean
         renames Ada.Strings.Less_Case_Insensitive;
   begin
      if Sorted then
         --  Alphabetical sort
         return Sort_Entities (Left.Category) < Sort_Entities (Right.Category)
           or else
             (Sort_Entities (Left.Category) = Sort_Entities (Right.Category)
              and then
                ((Left.Name = Right.Name and then
                Less_Than (S_Unique_Id (Left), S_Unique_Id (Right)))
                 or else
                 Less_Than (Get (Left.Name).all, Get (Right.Name).all)));
      else
         --  Positional sort
         return Left.Sloc_Start_No_Tab < Right.Sloc_Start_No_Tab;
      end if;
   end Lt;

   ---------
   -- "<" --
   ---------

   function "<"
     (L, R : Sorted_Node_Access) return Boolean
   is (Lt (Get_Info (L.all), Get_Info (R.all), L.Model.Filter.Sorted));

   -------------------------
   -- Add_Array_Recursive --
   -------------------------

   procedure Add_Array_Recursive
     (Model      : access Outline_Model_Record'Class;
      Sem_Nodes  : Semantic_Node_Array'Class;
      Sem_Parent : Semantic_Node'Class)
   is
   begin
      if Sem_Nodes.Length > Sort_Nodes_Threshold then
         --  Above a certain threshold, we want to sort the array before
         --  inserting
         declare
            N_Sem_Nodes : Semantic_Node_Array'Class := Sem_Nodes;

            --  ??? This is slow, since it will be recomputed several times
            --  for each node
            function "<"  (L, R : Semantic_Node'Class) return Boolean
            is (Lt (L.Info, R.Info, Model.Filter.Sorted));
         begin
            N_Sem_Nodes.Sort ("<"'Access);
            for I in 1 .. Sem_Nodes.Length loop
               Add_Recursive (Model, N_Sem_Nodes.Get (I), Sem_Parent,
                              Sort => False);
            end loop;
         end;
      else
         for I in 1 .. Sem_Nodes.Length loop
            Add_Recursive (Model, Sem_Nodes.Get (I), Sem_Parent);
         end loop;
      end if;
   end Add_Array_Recursive;

   -------------------
   -- Add_Recursive --
   -------------------

   procedure Add_Recursive
     (Model                : access Outline_Model_Record'Class;
      Sem_Node, Sem_Parent : Semantic_Node'Class;
      Sort                 : Boolean := True)
   is
      Parent_Node : Sorted_Node_Access;
      Root   : Sorted_Node_Access;  --  node for New_Obj
      Path   : Gtk_Tree_Path;
      use Sem_Tree_Holders;
      use Sorted_Node_Vector;
      Sem_Unique_Id : constant String := S_Unique_Id (Sem_Node);

      procedure Sorted_Insert (Vec : in out Vector; El : Sorted_Node_Access);
      --  Insert El in Vec
      --  Pre  : Vec is sorted
      --  Post : Vec is sorted

      procedure Sorted_Insert (Vec : in out Vector; El : Sorted_Node_Access) is
         Pos : Natural;
      begin
         Pos := Vec.First_Index;
         while Pos <= Vec.Last_Index and then Vec.Element (Pos) < El loop
            Pos := Pos + 1;
         end loop;
         Vec.Insert (Pos, El);
         Reindex (Vec, Pos);
      end Sorted_Insert;

   begin
      if Model = null
        or else Model.Semantic_Tree = Sem_Tree_Holders.Empty_Holder
      then
         return;
      end if;

      --  Issue: if we have children that would match, we still need to
      --  insert them. Depending on Group_Spec_And_Body, this might mean
      --  adding the current node as well.

      if
        --  Verify that the node is not filtered out
        Construct_Filter (Model, Sem_Node)
        --  Verify that a node with this ID doesn't already exist in the tree
        and then Get_Node (Model, Sem_Node) = null
      then
         if Sem_Unique_Id = "" then
            Trace (Me, "Cannot add a tree node with no Id");
         end if;

         if not (Model.Filter.Flat_View or else Sem_Parent = No_Semantic_Node)
         then
            Parent_Node := Get_Node (Model, Sem_Parent);
            if Model.Filter.Group_Spec_And_Body and then Parent_Node = null
            then
               Parent_Node := Get_Node_Next_Part (Model, Sem_Parent);
            end if;
         end if;

         --  Place any with-clause into top with-root node
         if Sem_Node.Category = Cat_With then
            Parent_Node := Model.Root_With;
         end if;

         if Parent_Node = null then
            --  By default, the parent node is the phantom root
            Parent_Node := Model.Phantom_Root'Access;
         end if;

         --  Add the node for the new object

         if Model.Filter.Group_Spec_And_Body then
            --  In that case we want only one node for the spec entity and for
            --  the body entity, so we get the next part for New_Node
            Root := Get_Node_Next_Part (Model, Sem_Node);

            --  If it does exist, it means that we already have a node for this
            --  entity's counterpart in the tree, so use it.
            if Root /= null then
               if
                 Sem_Node.Is_Declaration and then Root.Spec_Info = No_Node_Info
               then
                  Root.Spec_Info := Sem_Node.Info
                    (Show_Param_Names => Model.Filter.Show_Param_Names);
               elsif Root.Body_Info = No_Node_Info then
                  Root.Body_Info := Sem_Node.Info
                    (Show_Param_Names => Model.Filter.Show_Param_Names);
               else
                  Root := null;  --  should not happen, create new node
               end if;

               if Root /= null then
                  Path := Get_Path (Model, Root);
                  Row_Changed (+Model, Path, New_Iter (Root));
                  Path_Free (Path);
               end if;
            end if;
         end if;

         if Root = null then
            Root := new Sorted_Node'(Parent => Parent_Node,
                                     Model => Outline_Model (Model),
                                     others => <>);

            if Sem_Node.Is_Declaration then
               Root.Spec_Info := Sem_Node.Info
                 (Show_Param_Names => Model.Filter.Show_Param_Names);
            else
               Root.Body_Info := Sem_Node.Info
                 (Show_Param_Names => Model.Filter.Show_Param_Names);
            end if;

            pragma Assert
              (Root.Parent /= null,
               "Root parent of outline node should never be null");

            if Sort then
               Sorted_Insert (Root.Parent.Children, Root);
            else
               Root.Parent.Children.Append (Root);
               Root.Index_In_Siblings :=
                 Integer (Root.Parent.Children.Length - 1);
            end if;

            --  Notify the model of the change

            Path := Get_Path (Model, Root);
            Row_Inserted (+Model, Path, New_Iter (Root));
            Path_Free (Path);
         end if;

         Model.Sem_To_Tree_Nodes.Include (Sem_Unique_Id, Root);
      end if;

      if Model.Filter.Flat_View
        or else Construct_Filter (Model, Sem_Node)
        or else Model.Filter_Pattern /= null
        --  If there is a text filter on the model, we want to recurse on
        --  children no matter if the current node is shown or not
      then
         Add_Array_Recursive (Model, Sem_Node.Children, Sem_Node);
      end if;

   end Add_Recursive;

   --------------
   -- Set_Tree --
   --------------

   procedure Set_Tree
     (Model    : not null access Outline_Model_Record'Class;
      Sem_Tree : Semantic_Tree'Class;
      Filter   : Tree_Filter)
   is
      Path : Gtk_Tree_Path;
      T : constant Time := Clock;
   begin
      --  First delete the nodes, with the previous filters, otherwise we might
      --  be changing the ordering and therefore all operations on .Children
      --  would not find the nodes and clearing the tree would not work well.
      Model.Clear_Nodes (Model.Phantom_Root'Access);
      Model.Filter := Filter;
      Model.Semantic_Tree := Sem_Tree_Holders.To_Holder (Sem_Tree);
      if Sem_Tree = No_Semantic_Tree then
         return;
      end if;

      --  Create Root_With node and append it to model
      if not Model.Filter.Hide_Withes then

         Model.Root_With :=
           new Sorted_Node'(Parent => Model.Phantom_Root'Access,
                            Model => Outline_Model (Model),
                            Index_In_Siblings => 0, others => <>);

         Model.Root_With.Body_Info.Category := Cat_With;

         Model.Phantom_Root.Children.Append (Model.Root_With);

         Path := Get_Path (Model, Model.Root_With);
         Row_Inserted (+Model, Path, New_Iter (Model.Root_With));
         Path_Free (Path);
      end if;

      if Sem_Tree.Is_Ready then
         Add_Array_Recursive (Model, Sem_Tree.Root_Nodes, No_Semantic_Node);

         if Model.Root_With /= null
           and then Model.Root_With.Children.Is_Empty
         then
            --  Remove empty With_Clause node
            Path := Get_Path (Model, Model.Root_With);
            Model.Phantom_Root.Children.Delete_First;
            for Node of Model.Phantom_Root.Children loop
               Node.Index_In_Siblings := Node.Index_In_Siblings - 1;
            end loop;
            Free (Model.Root_With);
            Row_Deleted (+Model, Path);
            Path_Free (Path);
         end if;
      end if;

      Trace
        (Me, "Time elapsed to compute outline: " & Duration'Image (Clock - T));
   end Set_Tree;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Outline_Model_Record) return Glib.Gint
   is (4);

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Outline_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is (GType_String);

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Outline_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      if Get_Depth (Path) > 0 then
         declare
            Indices : constant Glib.Gint_Array := Get_Indices (Path);
            Current : Sorted_Node_Access := Self.Phantom_Root'Access;
         begin
            for J in Indices'Range loop
               Current := Nth_Child (Self, Current, Indices (J));
            end loop;

            return New_Iter (Current);
         end;
      else
         return Null_Iter;
      end if;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Self : access Outline_Model_Record'Class;
      Node : Sorted_Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Path : Gtk_Tree_Path;
      Cur  : Sorted_Node_Access := Node;
   begin
      Gtk_New (Path);

      while Cur /= null and then Cur /= Self.Phantom_Root'Access loop
         Prepend_Index (Path, Gint (Cur.Index_In_Siblings));
         Cur := Cur.Parent;
      end loop;

      return Path;
   end Get_Path;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
   begin
      return Get_Path (Self, Get_Sorted_Node (Iter));
   end Get_Path;

   -------------------
   -- Icon_For_Node --
   -------------------

   function Icon_For_Node (SN : Semantic_Node_Info) return String
   is (Stock_From_Category
       (Is_Declaration => SN.Is_Decl,
        Visibility     => SN.Visibility,
        Category       => SN.Category));

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Outline_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Info   : Semantic_Node_Info;
   begin
      if Column in Spec_Pixbuf_Column | Body_Pixbuf_Column then
         Init (Value, GType_String);

         Info := Get_Info (Self, Iter, Column);
         if Info /= No_Node_Info
           and then
             (
              --  If not in Group_Spec_And_Body mode, only the spec column is
              --  shown, so we want to show the icon on this column no
              --  matter what
              not Self.Filter.Group_Spec_And_Body
              --  If not, we want to only show the icon in the appropriate
              --  column
              or else (Column = Spec_Pixbuf_Column and then Info.Is_Decl)
              or else (Column = Body_Pixbuf_Column and then not Info.Is_Decl)
              )
         then
            Set_String (Value, Icon_For_Node (Info));
         elsif Iter /= Null_Iter
           and then Get_Sorted_Node (Iter) = Self.Root_With
         then
            Set_String
              (Value,
               Stock_From_Category (False, Visibility_Public, Cat_With));
         else
            Set_String (Value, "");
         end if;

      elsif Column = Has_Body_Column then
         --  We only display the second icon column when we group spec and
         --  bodies, for proper alignment of the text. In all other cases,
         --  the first icon will be either for spec or body, but this is
         --  computed above already.

         Init (Value, GType_Boolean);
         Set_Boolean (Value, Self.Filter.Group_Spec_And_Body);

      elsif Column = Display_Name_Column then
         Init (Value, GType_String);
         Info := Get_Info (Self, Iter, Column);
         if Info.Name /= No_Symbol then
            if Self.Filter.Show_Profile then
               declare
                  Profile : constant String := Get (Info.Profile).all;
               begin
                  Set_String
                    (Value, Escape_Text (Get (Info.Name).all)
                     & (if Profile /= "" then
                            " <span foreground=""#A0A0A0"">"
                       & XML_Utils.Protect
                         (Profile
                            (Profile'First ..
                             Integer'Min (Profile'Last, Profile'First + 500)))
                       & "</span>" else ""));
               end;
            else
               Set_String
                 (Value, Escape_Text (Get (Info.Name).all));
            end if;

         elsif Get_Sorted_Node (Iter) = Self.Root_With then
            Set_String (Value, "with clauses");
         else
            Set_String (Value, "no name");
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Init (Value, GType_String);
         Set_String (Value, "");
   end Get_Value;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Outline_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Unreferenced (Self);
      SN : constant Sorted_Node_Access := Get_Sorted_Node (Iter);
   begin
      Iter := Null_Iter;

      if SN /= null
        and then SN /= SN.Parent.Children.Last_Element
      then
         Iter := New_Iter
           (SN.Parent.Children.Element (SN.Index_In_Siblings + 1));
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Iter := Null_Iter;
   end Next;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : Sorted_Node_Access;
      C    : Sorted_Node_Vector.Cursor;
   begin
      if Parent = Null_Iter then
         Node := Self.Phantom_Root'Access;
      else
         Node := Get_Sorted_Node (Parent);
      end if;

      C := Node.Children.First;
      if Has_Element (C) then
         return New_Iter (Element (C));
      else
         return Null_Iter;
      end if;
   end Children;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is (Children (Self, Iter) /= Null_Iter);

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Node : Sorted_Node_Access;
   begin
      if Iter = Null_Iter then
         Node := Self.Phantom_Root'Access;
      else
         Node := Get_Sorted_Node (Iter);
      end if;

      return Gint (Node.Children.Length);
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Model : access Outline_Model_Record;
      Node  : Sorted_Node_Access;
      Nth   : Gint) return Sorted_Node_Access
   is
      C : Sorted_Node_Vector.Cursor;
   begin
      if Node = null then
         C := Model.Phantom_Root.Children.First;
      else
         C := Node.Children.First;
      end if;

      for J in 1 .. Nth loop
         if not Has_Element (C) then
            return null;
         end if;

         Next (C);
      end loop;

      if Has_Element (C) then
         return Element (C);
      else
         return null;
      end if;
   end Nth_Child;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
     (if Parent = Null_Iter
      then New_Iter (Nth_Child (Self, null, N))
      else New_Iter (Nth_Child (Self, Get_Sorted_Node (Parent), N)));

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Outline_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
     (if Child = Null_Iter
      then Null_Iter
      else New_Iter (Get_Sorted_Node (Child).Parent));

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Model   : not null access Outline_Model_Record'Class;
      Pattern : Search_Pattern_Access) is
   begin
      Free (Model.Filter_Pattern);
      Model.Filter_Pattern := Pattern;
   end Set_Filter;

   --------------
   -- Get_Info --
   --------------

   function Get_Info
     (Self   : not null access Outline_Model_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Gint := Display_Name_Column) return Semantic_Node_Info
   is
      pragma Unreferenced (Self);
      Node   : Sorted_Node_Access;
   begin
      if Iter = Null_Iter then
         return No_Node_Info;
      end if;

      Node := Get_Sorted_Node (Iter);
      return
        (if Column = Body_Pixbuf_Column or else Node.Spec_Info = No_Node_Info
         then Node.Body_Info
         else Node.Spec_Info);
   end Get_Info;

   ---------------------
   -- Get_Sorted_Node --
   ---------------------

   function Get_Sorted_Node
     (Iter : Gtk_Tree_Iter) return Sorted_Node_Access
   is
      function To_Node is new Ada.Unchecked_Conversion
        (System.Address, Sorted_Node_Access);
   begin
      if Iter = Null_Iter then
         return null;
      else
         return To_Node (Get_User_Data_1 (Iter));
      end if;
   end Get_Sorted_Node;

   --------------
   -- New_Iter --
   --------------

   function New_Iter (Iter : Sorted_Node_Access) return Gtk_Tree_Iter is
      function To_Address is new Ada.Unchecked_Conversion
        (Sorted_Node_Access, System.Address);
   begin
      if Iter /= null then
         return Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address (Iter),
            User_Data_2 => System.Null_Address,
            User_Data_3 => System.Null_Address);
      else
         return Null_Iter;
      end if;
   end New_Iter;

   -----------------
   -- Clear_Nodes --
   -----------------

   procedure Clear_Nodes
     (Model : access Outline_Model_Record'Class; Root : Sorted_Node_Access)
   is
      It        : Sorted_Node_Access;
   begin
      while not Root.Children.Is_Empty loop
         --  Start from the last element, so that the loop in Free which
         --  reindexes the elements has no effect
         It := Root.Children.Last_Element;
         Clean_Node (Outline_Model (Model), It);
      end loop;
   end Clear_Nodes;

   ----------
   -- Free --
   ----------

   procedure Free (Model : access Outline_Model_Record) is
   begin
      Clear_Nodes (Model, Model.Phantom_Root'Access);
      Free (Model.Filter_Pattern);
   end Free;

   ------------------
   -- File_Updated --
   ------------------

   procedure File_Updated
     (Model : access Outline_Model_Record;
      Tree  : Semantic_Tree'Class)
   is

      procedure Update_Nodes
        (Model_Nodes : Sorted_Node_Vector.Vector;
         Sem_Nodes : Semantic_Node_Array'Class;
         Sem_Parent : Semantic_Node'Class);

      procedure Update_Nodes
        (Model_Nodes : Sorted_Node_Vector.Vector;
         Sem_Nodes : Semantic_Node_Array'Class;
         Sem_Parent : Semantic_Node'Class)
      is
         New_Nodes : array (1 .. Sem_Nodes.Length) of Boolean
           := (others => True);
         C   : Sorted_Node_Vector.Cursor;
         To_Remove_Nodes : Sorted_Node_Vector.Vector;

         function Hash (H : Hash_Type) return Hash_Type is (H);

         package Ids_Hash_Map is new Ada.Containers.Bounded_Hashed_Maps
           (Hash_Type, Natural, Hash, "=", "=");
         Childs_Map : Ids_Hash_Map.Map (Count_Type (Sem_Nodes.Length), 100);

         function Find_Child_With_Id (Id : String) return Natural;

         function Find_Child_With_Id (Id : String) return Natural is
            use Ids_Hash_Map;
            El : constant Ids_Hash_Map.Cursor :=
              Childs_Map.Find (Ada.Strings.Hash (Id));
         begin
            if El /= Ids_Hash_Map.No_Element then
               return Element (El);
            else
               return 0;
            end if;
         end Find_Child_With_Id;
         use type Hash_Type;

         El : Sorted_Node_Access;

         procedure Process_Node (NI : Semantic_Node_Info);
         procedure Process_Node (NI : Semantic_Node_Info) is
         begin
            if NI = No_Node_Info then
               return;
            end if;

            declare
               Sem_Child_Idx : constant Natural :=
                 Find_Child_With_Id (S_Unique_Id (NI));
               Sem_Child : constant Semantic_Node'Class :=
                 (if Sem_Child_Idx /= 0
                  then Sem_Nodes.Get (Positive (Sem_Child_Idx))
                  else No_Semantic_Node);
            begin
               if Sem_Child /= No_Semantic_Node then
                  if Sem_Child.Is_Declaration then
                     El.Spec_Info := Sem_Child.Info;
                  else
                     El.Body_Info := Sem_Child.Info;
                  end if;

                  New_Nodes (Sem_Child_Idx) := False;
                  Update_Nodes
                    (El.Children, Sem_Child.Children, Sem_Child);
               elsif El /= Model.Root_With
                 and then
                   (To_Remove_Nodes.Is_Empty
                    or else To_Remove_Nodes.Last_Element /= Element (C))
               then
                  To_Remove_Nodes.Append (Element (C));
               end if;
            end;
         end Process_Node;

      begin
         for I in 1 .. Sem_Nodes.Length loop
            Childs_Map.Include
              (Ada.Strings.Hash
                 (S_Unique_Id (Sem_Nodes.Get (I))), I);
         end loop;

         C := Model_Nodes.First;
         while Has_Element (C) loop
            El := Element (C);
            Process_Node (El.Spec_Info);
            Process_Node (El.Body_Info);
            Next (C);
         end loop;

         for I in To_Remove_Nodes.First_Index .. To_Remove_Nodes.Last_Index
         loop
            Clean_Node (Outline_Model (Model), To_Remove_Nodes (I));
         end loop;

         for I in 1 .. Sem_Nodes.Length loop
            if New_Nodes (I)
              and then Construct_Filter (Model, Sem_Nodes.Get (I))
            then
               Add_Recursive (Model, Sem_Nodes.Get (I), Sem_Parent);
            end if;
         end loop;

      end Update_Nodes;

   begin
      --  ??? Using a Gtkada.Tree_View, we could just clear the model and
      --  recreate from scratch, which would be faster than trying to preserve
      --  existing nodes.

      --  Tree might have changed from the stored Semantic_Tree:
      if Tree /= Model.Semantic_Tree.Element
        or else Model.Filter.Flat_View
        or else Model.Filter_Pattern /= null
      then
         --  Don't use the selective update mode in flat view or when there
         --  is an active filter pattern. Just reset the tree. For the filter
         --  pattern, since we show node that don't necessarily have parents,
         --  the selective update does not make sense.
         Set_Tree (Model, Tree, Model.Filter);
      else
         Update_Nodes
           (Model.Phantom_Root.Children,
            Model.Semantic_Tree.Element.Root_Nodes, No_Semantic_Node);
      end if;
   end File_Updated;

   ---------------------------------
   -- Get_Path_Enclosing_Location --
   ---------------------------------

   function Get_Path_Enclosing_Location
     (Model        : access Outline_Model_Record;
      Line, Column : Integer) return Gtk_Tree_Path
   is
      Node     : Sorted_Node_Access := null;
      Path     : Gtk_Tree_Path;
      use type Sem_Tree_Holders.Holder;
   begin
      if Model.Semantic_Tree = Sem_Tree_Holders.Empty_Holder
        or else not Model.Semantic_Tree.Element.Is_Ready
      then
         Gtk_New (Path);
         return Path;
      end if;

      declare
         Sem_Node : Semantic_Node'Class
           := Model.Semantic_Tree.Element.Node_At
             ((Line, Visible_Column_Type (Column), 0));
      begin
         while Sem_Node /= No_Semantic_Node loop
            Node := Get_Node (Model, Sem_Node);
            exit when Node /= null;

            Node := Get_Node_Next_Part (Model, Sem_Node);
            exit when Node /= null;

            declare
               Parent : constant Semantic_Node'Class := Sem_Node.Parent;
            begin
               exit when Parent = No_Semantic_Node;
               Sem_Node := Parent;
            end;
         end loop;

         if Node = null then
            Gtk_New (Path);
            return Path;
         else
            return Get_Path (Model, Node);
         end if;
      end;
   end Get_Path_Enclosing_Location;

   -----------------------------
   -- Get_Path_From_Unique_ID --
   -----------------------------

   function Get_Path_From_Unique_ID
     (Model : access Outline_Model_Record;
      ID    : GNATCOLL.Symbols.Symbol) return Gtk_Tree_Path
   is
      S_ID : constant String := Get (ID).all;
      Node : constant Sorted_Node_Access :=
        (if Model.Sem_To_Tree_Nodes.Contains (S_ID) then
            Model.Sem_To_Tree_Nodes (S_ID)
         else
            null);
   begin
      if Node /= null then
         return Get_Path (Model, Node);
      else
         return Null_Gtk_Tree_Path;
      end if;
   end Get_Path_From_Unique_ID;

   --------------------
   -- Root_With_Iter --
   --------------------

   function Root_With_Iter
     (Model : access Outline_Model_Record) return Gtk_Tree_Iter is
   begin
      if Model.Root_With /= null then
         return New_Iter (Model.Root_With);
      else
         return Null_Iter;
      end if;
   end Root_With_Iter;

end Outline_View.Model;
