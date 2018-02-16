------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.OUTLINE");

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
   --  Add new nodes for Sem_Node and its nested entities

   function New_Iter (Node : Sorted_Node_Access) return Gtk_Tree_Iter;
   --  Create a new iterator out of a node

   function Nth_Child
     (Node  : not null Sorted_Node_Access;
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

   function S_Unique_Id (N : Semantic_Node'Class) return Node_Id
   is (N.Unique_Id, N.Is_Declaration);
   --  Unique Id function that adds the information of whether the entity is a
   --  spec or a body

   function S_Unique_Id (N : Semantic_Node_Info) return Node_Id
   is (N.Unique_Id, N.Is_Decl);
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
     (Map : Sem_To_Tree_Maps.Map; Key : Node_Id) return Sorted_Node_Access
     with Inline_Always;
   --  Helper for the semantic_tree_node to sorted node maps, that will return
   --  the element if it exists, or null

   function Lt
     (Left, Right : Semantic_Node_Info; Sorted : Boolean) return Boolean;
   --  Less_Than comparison for Semantic_Node_Infos

   function Get_Sorted_Node (Iter : Gtk_Tree_Iter) return Sorted_Node_Access;
   --  Return the node stored in the iter

   function Get_Sorted_Node_Or_Root
     (Self : access Outline_Model_Record'Class;
      Iter : Gtk_Tree_Iter) return not null Sorted_Node_Access;
   --  Return the node stored in the iter or Phantom_Root for Null_Iter

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
     (Map : Sem_To_Tree_Maps.Map; Key : Node_Id) return Sorted_Node_Access
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
     (Model : access Outline_Model_Record'Class;
      Node  : Semantic_Node'Class) return Sorted_Node_Access
   is
     (Element_Or_Null (Model.Sem_To_Tree_Nodes, S_Unique_Id (Node)));

   ------------------------
   -- Get_Node_Next_Part --
   ------------------------

   function Get_Node_Next_Part
     (Model : access Outline_Model_Record'Class;
      Node  : Semantic_Node'Class) return Sorted_Node_Access
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
              (Model.Sem_To_Tree_Nodes, (Node.Unique_Id, True)));
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

      case Node.Kind is
         when Leaf_Node_Kind =>
            --  Remove the semantic to tree node mapping from the model
            if Node.Spec_Info /= No_Node_Info then
               Model.Sem_To_Tree_Nodes.Exclude (S_Unique_Id (Node.Spec_Info));
            end if;
            if Node.Body_Info /= No_Node_Info then
               Model.Sem_To_Tree_Nodes.Exclude (S_Unique_Id (Node.Body_Info));
            end if;

         when Category_Node_Kind =>
            Model.Categories (Node.Category) := null;
      end case;

      --  Remove this node from the parent's list of children
      if Node.Parent /= null then
         Node.Parent.Children.Delete (Node.Index_In_Siblings);
         --  Reindex the remaining children
         Reindex (Node.Parent.Children, Node.Index_In_Siblings);
      end if;

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
      function Less_Than (L, R : Node_Id) return Boolean;

      function Less_Than
        (L, R : String) return Boolean
         renames Ada.Strings.Less_Case_Insensitive;

      ---------------
      -- Less_Than --
      ---------------

      function Less_Than (L, R : Node_Id) return Boolean is
         Left  : constant String := GNATCOLL.Symbols.Get (L.Identifier).all;
         Right : constant String := GNATCOLL.Symbols.Get (R.Identifier).all;
      begin
         return (Left = Right and L.Is_Declaration > R.Is_Declaration)
           or else Less_Than (Left, Right);
      end Less_Than;
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

   function "<" (L, R : Sorted_Node_Access) return Boolean is
      function Get_Info (S : Leaf_Node) return Semantic_Node_Info is
        (if S.Spec_Info /= No_Node_Info then S.Spec_Info else S.Body_Info);
   begin
      if L.Kind = R.Kind then
         case L.Kind is
            when Leaf_Node_Kind =>
               return Lt (Get_Info (L.all),
                          Get_Info (R.all),
                          L.Model.Filter.Sorted);
            when Category_Node_Kind =>
               return Sort_Entities (L.Category) < Sort_Entities (R.Category)
                 or else
                   (Sort_Entities (L.Category) = Sort_Entities (R.Category)
                    and then L.Category < R.Category);
         end case;
      else  --  Put category nodes before leaf, so with goes before unit
         return L.Kind > R.Kind;
      end if;
   end "<";

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
      use Sorted_Node_Vector;
      Parent_Node   : Sorted_Node_Access;
      Root          : Sorted_Node_Access;  --  node for Sem_Node
      Sem_Unique_Id : constant Node_Id := S_Unique_Id (Sem_Node);

      procedure Sorted_Insert (Vec : in out Vector; El : Sorted_Node_Access);
      --  Insert El in Vec
      --  Pre  : Vec is sorted
      --  Post : Vec is sorted

      function Get_Category_Node
        (Category : Language_Category) return Sorted_Node_Access;
      --  Return Category_Node for given Category. Create new if needed.

      procedure Row_Inserted (Node : Sorted_Node_Access);
      --  Notify the model of the change

      -----------------------
      -- Get_Category_Node --
      -----------------------

      function Get_Category_Node
        (Category : Language_Category) return Sorted_Node_Access
      is
         Result : Sorted_Node_Access := Model.Categories (Category);
      begin
         if Result = null then
            Result := new Sorted_Node'(Kind     => Category_Node_Kind,
                                       Model    => Outline_Model (Model),
                                       Category => Category,
                                       Parent   => Model.Phantom_Root'Access,
                                       others   => <>);
            Sorted_Insert (Model.Phantom_Root.Children, Result);

            Row_Inserted (Result);
            Model.Categories (Category) := Result;
         end if;

         return Result;
      end Get_Category_Node;

      procedure Row_Inserted (Node : Sorted_Node_Access) is
         Path : constant Gtk_Tree_Path := Get_Path (Model, Node);
      begin
         Row_Inserted (+Model, Path, New_Iter (Node));
         Path_Free (Path);
      end Row_Inserted;

      -------------------
      -- Sorted_Insert --
      -------------------

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
      if Model = null then
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
         if Sem_Unique_Id.Identifier = No_Symbol then
            Trace (Me, "Cannot add a tree node with no Id");
         end if;

         if not Model.Filter.Flat_View and Sem_Parent /= No_Semantic_Node then
            Parent_Node := Get_Node (Model, Sem_Parent);

            if Model.Filter.Group_Spec_And_Body and Parent_Node = null then
               Parent_Node := Get_Node_Next_Part (Model, Sem_Parent);
            end if;
         elsif Model.Filter.Flat_View and Model.Filter.Group_By_Category then
            Parent_Node := Get_Category_Node (Sem_Node.Category);
         elsif not Model.Filter.Flat_View
           and not Model.Filter.Hide_Withes
           and Sem_Node.Category = Cat_With
         then
            Parent_Node := Get_Category_Node (Sem_Node.Category);
         end if;

         if Parent_Node = null then
            --  By default, the parent node is the phantom root
            Parent_Node := Model.Phantom_Root'Access;
         end if;

         --  Add the node for the new object

         if Model.Filter.Group_Spec_And_Body then
            --  In that case we want only one node for the spec entity and for
            --  the body entity, so we get the next part for Sem_Node
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
                  declare
                     Path : constant Gtk_Tree_Path := Get_Path (Model, Root);
                  begin
                     Row_Changed (+Model, Path, New_Iter (Root));
                     Path_Free (Path);
                  end;
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

            Root.Sloc_Start := Sem_Node.Sloc_Start;
            Root.Sloc_End := Sem_Node.Sloc_End;

            if Sort then
               Sorted_Insert (Root.Parent.Children, Root);
            else
               Root.Parent.Children.Append (Root);
               Root.Index_In_Siblings := Root.Parent.Children.Last_Index;
            end if;

            Row_Inserted (Root);
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
      Now  : constant Time := Clock;
   begin
      --  First delete the nodes, with the previous filters, otherwise we might
      --  be changing the ordering and therefore all operations on .Children
      --  would not find the nodes and clearing the tree would not work well.
      Model.Clear_Nodes (Model.Phantom_Root'Access);
      Model.Filter := Filter;
      if Sem_Tree = No_Semantic_Tree then
         return;
      end if;

      if Sem_Tree.Is_Ready then
         Add_Array_Recursive (Model, Sem_Tree.Root_Nodes, No_Semantic_Node);
      end if;

      Trace
        (Me,
         "Time elapsed to compute outline:" & Duration'Image (Clock - Now));
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
               Current := Nth_Child (Current, Indices (J));
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

      while Cur not in null | Self.Phantom_Root'Access loop
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
      return Get_Path (Self, Get_Sorted_Node_Or_Root (Self, Iter));
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
      Node   : constant not null Sorted_Node_Access :=
        Get_Sorted_Node_Or_Root (Self, Iter);
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
         elsif Node.Kind = Category_Node_Kind then
            Set_String
              (Value,
               Stock_From_Category (False, Visibility_Public, Node.Category));
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

         elsif Node.Kind = Category_Node_Kind then
            Set_String (Value, Category_Name (Node.Category));
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
      Node : constant not null Sorted_Node_Access :=
        Get_Sorted_Node_Or_Root (Self, Parent);
   begin
      if Node.Children.Is_Empty then
         return Null_Iter;
      else
         return New_Iter (Node.Children.First_Element);
      end if;
   end Children;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is (not Get_Sorted_Node_Or_Root (Self, Iter).Children.Is_Empty);

   ----------
   -- Hash --
   ----------

   function Hash (Node : Node_Id) return Hash_Type is
      Result : Hash_Type := Hash (Node.Identifier);
   begin
      if Node.Is_Declaration then
         Result := Result + 1_000_000;
      end if;

      return Result;
   end Hash;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is
   begin
      return Gint (Get_Sorted_Node_Or_Root (Self, Iter).Children.Length);
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Node  : not null Sorted_Node_Access;
      Nth   : Gint) return Sorted_Node_Access
   is
      Index : constant Natural := Natural (Nth);
   begin
      if Index in Node.Children.First_Index .. Node.Children.Last_Index then
         return Node.Children.Element (Index);
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
     (New_Iter (Nth_Child (Get_Sorted_Node_Or_Root (Self, Parent), N)));

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Outline_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);
      Node : constant Sorted_Node_Access := Get_Sorted_Node (Child);
   begin
      if Node = null then
         return Null_Iter;
      else
         return New_Iter (Node.Parent);
      end if;
   end Parent;

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
      Node : constant not null Sorted_Node_Access :=
        Get_Sorted_Node_Or_Root (Self, Iter);
   begin
      return
        (if Node.Kind = Category_Node_Kind then No_Node_Info
         elsif Column = Body_Pixbuf_Column
           or else Node.Spec_Info = No_Node_Info
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

   -----------------------------
   -- Get_Sorted_Node_Or_Root --
   -----------------------------

   function Get_Sorted_Node_Or_Root
     (Self : access Outline_Model_Record'Class;
      Iter : Gtk_Tree_Iter) return not null Sorted_Node_Access
   is
      Result : constant Sorted_Node_Access := Get_Sorted_Node (Iter);
   begin
      if Result = null then
         return Self.Phantom_Root'Access;
      else
         return Result;
      end if;
   end Get_Sorted_Node_Or_Root;

   --------------
   -- New_Iter --
   --------------

   function New_Iter (Node : Sorted_Node_Access) return Gtk_Tree_Iter is
      function To_Address is new Ada.Unchecked_Conversion
        (Sorted_Node_Access, System.Address);
   begin
      if Node /= null then
         return Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address (Node),
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
      Node : Sorted_Node_Access;
   begin
      while not Root.Children.Is_Empty loop
         --  Start from the last element, so that the loop in Free which
         --  reindexes the elements has no effect
         Node := Root.Children.Last_Element;
         Clean_Node (Outline_Model (Model), Node);
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
     (Model  : access Outline_Model_Record;
      Tree   : Semantic_Tree'Class;
      Filter : Tree_Filter) is
   begin
      Set_Tree (Model, Tree, Filter);
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

      Target_Loc : constant Sloc_T := (Line, Visible_Column_Type (Column), 0);

      function Explore (Node : Sorted_Node_Access) return Sorted_Node_Access;
      --  The iterative explorer function, return a Sorted_Node_Access that
      --  contains the location, or null if nothing was found.

      function Explore (Node : Sorted_Node_Access) return Sorted_Node_Access is
         use type Visible_Column_Type;

         function "<=" (L, R : Sloc_T) return Boolean is
           (L.Line < R.Line
            or else (L.Line = R.Line and then L.Column <= R.Column));

      begin
         if Node = null then
            return null;
         end if;

         if Node.Sloc_Start = No_Sloc
           or else (Node.Sloc_Start <= Target_Loc
                    and then Target_Loc <= Node.Sloc_End)
         then
            --  We have found a node which either has no sloc (like a category
            --  node or the phantom root) or which contains the sloc.

            --  If this is not a leaf node, attempt to find a better one
            for E of Node.Children loop
               declare
                  Found_Node : constant Sorted_Node_Access := Explore (E);
               begin
                  if Found_Node /= null then
                     return Found_Node;
                  end if;
               end;
            end loop;

            --  This is a valid node, and there are no better node found in
            --  the children? return this.
            if Node.Sloc_Start /= No_Sloc then
               return Node;
            end if;
         end if;

         return null;
      end Explore;

   begin
      --  Iterate through all the rows, until we find a node that contains
      --  the given line, column.
      Node := Explore (Model.Phantom_Root'Access);
      if Node = null then
         Gtk_New (Path);
         return Path;
      else
         return Get_Path (Model, Node);
      end if;
   end Get_Path_Enclosing_Location;

   -----------------------------
   -- Get_Path_From_Unique_ID --
   -----------------------------

   function Get_Path_From_Unique_ID
     (Model : access Outline_Model_Record;
      ID    : GNATCOLL.Symbols.Symbol) return Gtk_Tree_Path
   is
      S_ID : constant Node_Id := (ID, True);
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
      return New_Iter (Model.Categories (Cat_With));
   end Root_With_Iter;

end Outline_View.Model;
