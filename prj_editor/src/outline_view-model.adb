------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with System; use System;
with String_Utils; use String_Utils;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Glib.Object;                 use Glib.Object;
with Glib.Convert;                use Glib.Convert;
with Gtk.Tree_Model.Utils;        use Gtk.Tree_Model.Utils;

with Basic_Types;                 use Basic_Types;
with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GPS.Search;                  use GPS.Search;
with Language.Icons;              use Language.Icons;
with XML_Utils; use XML_Utils;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with Ada.Containers.Bounded_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

package body Outline_View.Model is
   Me : constant Trace_Handle := Create ("OUTLINE");

   use Sorted_Node_Set;

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
     (Model           : access Outline_Model_Record'Class;
      Sem_Node, Sem_Parent : Semantic_Node'Class);
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
   --  Retreives the path of a node

   procedure Clear_Nodes
     (Model : access Outline_Model_Record'Class; Root : Sorted_Node_Access);
   --  Remove recursively all children nodes of the root given in parameter

   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      Node   : Semantic_Node'Class) return Sorted_Node_Access;

   function Get_Node_Next_Part
     (Model    : access Outline_Model_Record'Class;
      Node     : Semantic_Node'Class) return Sorted_Node_Access;
   --  Same as Get_Node, but for the body or full view of the entity.

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      Node     : Semantic_Node'Class) return Sorted_Node_Access
   is
      use Sem_To_Tree_Maps;
      C : constant Sem_To_Tree_Maps.Cursor := Model.Sem_To_Tree_Nodes.Find
        (Node.Unique_Id);
   begin
      return (if C = Sem_To_Tree_Maps.No_Element then null else Element (C));
   end Get_Node;

   ------------------------
   -- Get_Node_Next_Part --
   ------------------------

   function Get_Node_Next_Part
     (Model  : access Outline_Model_Record'Class;
      Node     : Semantic_Node'Class) return Sorted_Node_Access
   is
      C : constant Semantic_Node'Class := Node.Counterpart;
   begin
      return (if Model.Filter.Group_Spec_And_Body
              and then C.Is_Valid
              and then C.File = Node.File
              then Get_Node (Model, C)
              else null);
   end Get_Node_Next_Part;

   procedure Clean_Node (Model : Outline_Model;
                         Node  : in out Sorted_Node_Access);

   ----------------
   -- Clean_Node --
   ----------------

   procedure Clean_Node
     (Model : Outline_Model;
      Node  : in out Sorted_Node_Access)
   is
      Path : constant Gtk_Tree_Path := Get_Path (Model, Node);
      Cur  : Sorted_Node_Access;
   begin
      --  Kill the children if any
      Clear_Nodes (Model, Node);

      --  Remove the semantic to tree node mapping from the model
      if Node.Spec_Info /= No_Node_Info then
         Model.Sem_To_Tree_Nodes.Exclude (+Node.Spec_Info.Unique_Id);
      end if;
      if Node.Body_Info /= No_Node_Info then
         Model.Sem_To_Tree_Nodes.Exclude (+Node.Body_Info.Unique_Id);
      end if;

      --  Then update next siblings
      if Node.Prev /= null then
         Node.Prev.Next := Node.Next;
      end if;

      if Node.Next /= null then
         Node.Next.Prev := Node.Prev;

         Cur := Node.Next;
         while Cur /= null loop
            Cur.Index_In_Siblings := Cur.Index_In_Siblings - 1;
            Cur := Cur.Next;
         end loop;
      end if;

      --  Finally, free this node
      if Node.Parent /= null then
         Node.Parent.Children.Delete (Node);
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
      Node  : Semantic_Node'Class) return Boolean is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      case Node.Category is
         when Cat_Package .. Cat_Entry
            | Cat_Field | Cat_Variable
            | Type_Category =>

            if Model.Filter.Hide_Types
              and then Node.Category in Type_Category
            then
               return False;
            end if;

            if Model.Filter.Hide_Objects
              and then Node.Category in Data_Category
            then
               return False;
            end if;

            if Model.Filter.Hide_Tasks
              and then (Node.Category = Cat_Task
                          or Node.Category = Cat_Protected)
            then
               return False;
            end if;

            if Node.Category in Subprogram_Category
              and then Model.Filter.Hide_Declarations
              and then Node.Is_Declaration
            then
               return False;
            end if;

         when Cat_With =>
            if Model.Filter.Hide_Withes then
               return False;
            end if;

         when others =>
            return False;
      end case;

      if Node.Name = No_Symbol then
         return False;
      end if;

      if Model.Filter_Pattern /= null then
         return Model.Filter_Pattern.Start
           (Get (Node.Name).all) /= No_Match;
      end if;

      return True;
   end Construct_Filter;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Sorted_Node_Access) return Boolean is

      function Compare (Left, Right : String) return Integer;
      --  Does a case-insensitive comparison, returns -1 if Left < Right, 0 if
      --  equals, 1 if Left > Right.

      -------------
      -- Compare --
      -------------

      function Compare (Left, Right : String) return Integer is
         Left_I, Right_I : Integer;
      begin
         Left_I := Left'First;
         Right_I := Right'First;

         loop
            if Left_I <= Left'Last and then Right_I > Right'Last then
               return 1;
            elsif Left_I > Left'Last and then Right_I <= Right'Last then
               return -1;
            elsif Left_I > Left'Last and then Right_I > Right'Last then
               return 0;
            elsif To_Lower (Left (Left_I)) < To_Lower (Right (Right_I)) then
               return -1;
            elsif To_Lower (Left (Left_I)) > To_Lower (Right (Right_I)) then
               return 1;
            end if;

            Left_I := Left_I + 1;
            Right_I := Right_I + 1;
         end loop;
      end Compare;

      Comparison : Integer;

      Left_Info  : Semantic_Node_Info renames Get_Info (Left.all);
      Right_Info : Semantic_Node_Info renames Get_Info (Right.all);
   begin
      if Left = Right then
         return False;
      elsif Left.Model.Filter.Sorted then
         --  Alphabetical sort
         if Sort_Entities (Left_Info.Category)
           < Sort_Entities (Right_Info.Category)
         then
            return True;
         elsif Sort_Entities (Left_Info.Category)
           = Sort_Entities (Right_Info.Category)
         then
            Comparison := Compare (Get (Left_Info.Name).all,
                                   Get (Right_Info.Name).all);
            if Comparison = -1 then
               return True;
            elsif Comparison = 0 then
               Comparison := Compare (+Left_Info.Unique_Id,
                                      +Right_Info.Unique_Id);
               if Comparison = -1 then
                  return True;
               elsif Comparison = 0 then
                  --  We need to have a clear and definite way to differenciate
                  --  constructs, otherwise we'll have errors when adding them
                  --  to the set. If we can't do that alphabetically, then we
                  --  fall back to the sloc comparison.
                  return Left_Info.Sloc_Start < Right_Info.Sloc_Start;
               end if;
            end if;
         end if;
         return False;
      else
         --  Positional sort
         return Left_Info.Sloc_Start < Right_Info.Sloc_Start;
      end if;
   end "<";

   -------------------
   -- Add_Recursive --
   -------------------

   procedure Add_Recursive
     (Model      : access Outline_Model_Record'Class;
      Sem_Node,
      Sem_Parent : Semantic_Node'Class)
   is
      Parent_Node : Sorted_Node_Access;
      Root   : Sorted_Node_Access;  --  node for New_Obj
      Dummy  : Sorted_Node_Access;
      Path   : Gtk_Tree_Path;
      Position : Sorted_Node_Set.Cursor;
      Inserted : Boolean;
      use Sem_Tree_Holders;
      Sem_Unique_Id : constant String := Sem_Node.Unique_Id;
   begin

      if Model = null
        or else Model.Semantic_Tree = Sem_Tree_Holders.Empty_Holder
      then
         return;
      end if;

      --  Issue: if we have children that would match, we still need to
      --  insert them. Depending on Group_Spec_And_Body, this might mean
      --  adding the current node as well.

      if Root = null
        and then Construct_Filter (Model, Sem_Node)
      then
         --  pragma Assert
         --  (Sem_Unique_Id /= "", "Cannot add a tree node with no Id");
         if Sem_Unique_Id = "" then
            Put_Line ("Cannot add a tree node with no Id");
         end if;

         Root := Get_Node (Model, Sem_Node);

         if Model.Filter.Flat_View then
            Parent_Node := Model.Phantom_Root'Access;
         elsif Sem_Node.Category = Cat_With then
            --  Place any with-clause into top with-root node
            Parent_Node := Model.Root_With;
         else
            if Sem_Parent = No_Semantic_Node then
               Parent_Node := Model.Phantom_Root'Access;
            elsif Get_Node (Model, Sem_Node) = null then
               Parent_Node := Get_Node (Model, Sem_Parent);
               if Model.Filter.Group_Spec_And_Body
                 and then Parent_Node = null
               then
                  Parent_Node := Get_Node_Next_Part (Model, Sem_Parent);
               end if;
            end if;

            if Parent_Node = null then
               --  Parent node might have been filtered
               Parent_Node := Model.Phantom_Root'Access;
            end if;
         end if;

         --  Add the node for the new object

         if Model.Filter.Group_Spec_And_Body then
            --  In that case we want only one node for the spec entity and for
            --  the body entity, so we get the next part for New_Node
            Root := Get_Node_Next_Part (Model, Sem_Node);

            --  If it does exist, it means that we already have a node for this
            --  entity's counterpart in the tree, so use it.
            if Root /= null then
               if Sem_Node.Is_Declaration then
                  if Root.Spec_Info = No_Node_Info then
                     Root.Spec_Info := Sem_Node.Info;
                  else
                     Root := null;  --  should not happen, create new node
                  end if;
               else
                  if Root.Body_Info = No_Node_Info then
                     Root.Body_Info := Sem_Node.Info;
                  else
                     Root := null;  --  should not happen, create new node
                  end if;
               end if;

               if Root /= null then
                  --  ??? Should we also update the SLOC ?

                  Path := Get_Path (Model, Root);
                  Row_Changed (+Model, Path, New_Iter (Root));
                  Path_Free (Path);
               end if;
            end if;
         end if;

         if Root = null then
            Root := new Sorted_Node;

            if Sem_Node.Is_Declaration then
               Root.Spec_Info := Sem_Node.Info;
            else
               Root.Body_Info := Sem_Node.Info;
            end if;

            Root.Parent   := Parent_Node;
            Root.Model    := Outline_Model (Model);

            --  ??? Ordered_Index is only used to do the sorting of the
            --  tree, perhaps we could be more efficient.
            Insert
              (Container => Root.Parent.Children,
               New_Item  => Root,
               Position  => Position,
               Inserted  => Inserted);

            if Inserted then
               if Previous (Position) = Sorted_Node_Set.No_Element then
                  Root.Index_In_Siblings := 0;
               else
                  Dummy := Element (Previous (Position));
                  Dummy.Next := Root;
                  Root.Prev := Dummy;
                  Root.Index_In_Siblings := Dummy.Index_In_Siblings + 1;
               end if;

               if Has_Element (Next (Position)) then
                  Dummy := Element (Next (Position));
                  Root.Next := Dummy;
                  Dummy.Prev := Root;

                  --  Adjust the indexes for the node and its siblings, to
                  --  preserve sorting
                  while Dummy /= null loop
                     Dummy.Index_In_Siblings := Dummy.Index_In_Siblings + 1;
                     Dummy := Dummy.Next;
                  end loop;
               end if;

               --  Notify the model of the change

               Path := Get_Path (Model, Root);
               Row_Inserted (+Model, Path, New_Iter (Root));
               Path_Free (Path);
            end if;
         end if;

         Model.Sem_To_Tree_Nodes.Include (Sem_Unique_Id, Root);
      end if;

      if Model.Filter.Flat_View
        or else Construct_Filter (Model, Sem_Node)
        or else Model.Filter_Pattern /= null
        --  If there is a text filter on the model, we want to recurse on
        --  children no matter if the current node is shown or not
      then
         --  Then add all its children recursively
         declare
            A : constant Semantic_Node_Array'Class := Sem_Node.Children;
         begin
            for J in 1 .. A.Length loop
               Add_Recursive (Model, A.Get (J), Sem_Node);
            end loop;
         end;
      end if;

   end Add_Recursive;

   --------------
   -- Set_File --
   --------------

   procedure Set_Tree
     (Model    : not null access Outline_Model_Record'Class;
      Sem_Tree : Semantic_Tree'Class;
      Filter   : Tree_Filter)
   is
      procedure Add_Root_With;
      --  Create Root_With node and append it to model

      -------------------
      -- Add_Root_With --
      -------------------

      procedure Add_Root_With is
         Path   : Gtk_Tree_Path;
      begin
         if Model.Filter.Hide_Withes then
            return;
         end if;

         Model.Root_With := new Sorted_Node;
         Model.Root_With.Parent := Model.Phantom_Root'Access;
         Model.Root_With.Model := Outline_Model (Model);
         Model.Root_With.Index_In_Siblings := 0;
         Model.Root_With.Body_Info.Category := Cat_With;

         Insert
           (Container => Model.Phantom_Root.Children,
            New_Item  => Model.Root_With);

         Path := Get_Path (Model, Model.Root_With);
         Row_Inserted (+Model, Path, New_Iter (Model.Root_With));
         Path_Free (Path);
      end Add_Root_With;

   begin
      --  First delete the nodes, with the previous filters, otherwise we might
      --  be changing the ordering and therefore all operations on .Children
      --  would not find the nodes and clearing the tree would not work well.
      Model.Clear_Nodes (Model.Phantom_Root'Access);
      Model.Filter := Filter;
      Add_Root_With;

      Model.Semantic_Tree := Sem_Tree_Holders.To_Holder (Sem_Tree);
      if Sem_Tree = No_Semantic_Tree then
         return;
      end if;

      declare
         A : constant Semantic_Node_Array'Class := Sem_Tree.Root_Nodes;
      begin
         for J in 1 .. A.Length loop
            Add_Recursive (Model, A.Get (J), No_Semantic_Node);
         end loop;
      end;
   end Set_Tree;

   --------------
   -- Get_File --
   --------------

   function Get_Tree (Model : Outline_Model) return Semantic_Tree'Class is
   begin
      return Model.Semantic_Tree.Element;
   end Get_Tree;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Outline_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);
   begin
      return 3;
   end Get_N_Columns;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Outline_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);
   begin
      if Index = Spec_Pixbuf_Column then
         return GType_String;
      elsif Index = Body_Pixbuf_Column then
         return GType_String;
      elsif Index = Display_Name_Column then
         return GType_String;
      end if;

      return GType_String;
   end Get_Column_Type;

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
      Info   : constant Semantic_Node_Info := Get_Info (Self, Iter, Column);
   begin

      if Column = Spec_Pixbuf_Column
         or else Column = Body_Pixbuf_Column
      then
         Init (Value, GType_String);

         if Info /= No_Node_Info then
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

      elsif Column = Display_Name_Column then
         Init (Value, GType_String);

         if Info.Name /= No_Symbol then
            if Self.Filter.Show_Profile
              and then Info.Category in Subprogram_Category
            then
               declare
                  Profile : constant String := +Info.Profile;
               begin
                  Set_String
                    (Value, Escape_Text (Get (Info.Name).all)
                     & " <span foreground=""#A0A0A0"">"
                     & XML_Utils.Protect
                       (Profile (Profile'First ..
                            Integer'Min (Profile'Last, Profile'First + 500)))
                     & "</span>");
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
   begin
      if Iter = Null_Iter then
         Iter := Null_Iter;
      else
         Iter := New_Iter (Get_Sorted_Node (Iter).Next);
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
      C    : Sorted_Node_Set.Cursor;
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
   is
      Val : constant Boolean :=
        Children (Self, Iter) /= Null_Iter;
   begin
      return Val;
   end Has_Child;

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
      C : Sorted_Node_Set.Cursor;
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
   begin
      if Parent = Null_Iter then
         return New_Iter (Nth_Child (Self, null, N));
      else
         return New_Iter (Nth_Child (Self, Get_Sorted_Node (Parent), N));
      end if;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Outline_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);
   begin
      if Child = Null_Iter then
         return Null_Iter;
      else
         return New_Iter (Get_Sorted_Node (Child).Parent);
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
      pragma Unreferenced (Self);
      Node   : Sorted_Node_Access;
      Info      : Semantic_Node_Info;
   begin
      if Iter = Null_Iter then
         return No_Node_Info;
      end if;

      Node := Get_Sorted_Node (Iter);
      if Column = Body_Pixbuf_Column then
         Info := Node.Body_Info;
      else
         if Node.Spec_Info /= No_Node_Info then
            Info := Node.Spec_Info;
         else
            Info := Node.Body_Info;
         end if;
      end if;

      return Info;
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
     (Model    : access Outline_Model_Record)
   is

      procedure Update_Nodes
        (Model_Nodes : Sorted_Node_Set.Set;
         Sem_Nodes : Semantic_Node_Array'Class;
         Sem_Parent : Semantic_Node'Class);

      procedure Update_Nodes
        (Model_Nodes : Sorted_Node_Set.Set;
         Sem_Nodes : Semantic_Node_Array'Class;
         Sem_Parent : Semantic_Node'Class)
      is
         New_Nodes : array (1 .. Sem_Nodes.Length) of Boolean
           := (others => True);
         C   : Sorted_Node_Set.Cursor;
         To_Remove_Nodes : Sorted_Node_Vector.Vector;

         function Hash (H : Hash_Type) return Hash_Type is (H);

         package Ids_Hash_Map is new Ada.Containers.Bounded_Hashed_Maps
           (Hash_Type, Natural, Hash, "=", "=");
         Childs_Map : Ids_Hash_Map.Map (Count_Type (Sem_Nodes.Length), 100);

         function Find_Child_With_Id (Id : String) return Natural;

         function Find_Child_With_Id (Id : String) return Natural is
            use Ids_Hash_Map;
            El : constant Ids_Hash_Map.Cursor :=
              Childs_Map.Find (Ada.Strings.Hash_Case_Insensitive (Id));
         begin
            if El /= Ids_Hash_Map.No_Element then
               return Element (El);
            else
               return 0;
            end if;
         end Find_Child_With_Id;
         use type Hash_Type;
      begin
         for I in 1 .. Sem_Nodes.Length loop
            Childs_Map.Include
              (Ada.Strings.Hash_Case_Insensitive
                 (Sem_Nodes.Get (I).Unique_Id), I);
         end loop;

         C := Model_Nodes.First;

         while Has_Element (C) loop
            declare
               El : constant Sorted_Node_Access := Element (C);
               Sem_Child_Idx : constant Natural :=
                 Find_Child_With_Id (+Get_Info (El.all).Unique_Id);
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
               elsif El /= Model.Root_With then
                  To_Remove_Nodes.Append (Element (C));
               end if;
            end;
            Next (C);
         end loop;

         for I in 1 .. To_Remove_Nodes.Length loop
            Clean_Node (Outline_Model (Model), To_Remove_Nodes (Positive (I)));
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
      Update_Nodes (Model.Phantom_Root.Children,
                 Model.Semantic_Tree.Element.Root_Nodes, No_Semantic_Node);
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
      if Model.Semantic_Tree = Sem_Tree_Holders.Empty_Holder then
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

end Outline_View.Model;
