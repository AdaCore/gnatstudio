------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Glib.Object;                 use Glib.Object;
with Glib.Convert;                use Glib.Convert;
with Gtk.Tree_Model.Utils;        use Gtk.Tree_Model.Utils;

with Basic_Types;                 use Basic_Types;
with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GPS.Search;                  use GPS.Search;
with Language.Profile_Formaters;  use Language.Profile_Formaters;
with Language.Icons;              use Language.Icons;
with Project_Explorers_Common;    use Project_Explorers_Common;
with XML_Utils; use XML_Utils;

package body Outline_View.Model is
   Me : constant Trace_Handle := Create ("OUTLINE");

   use Construct_Annotations_Pckg;
   use Sorted_Node_Set;

   type Entity_Sort_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Is_Spec : Boolean;
      Node    : Sorted_Node_Access;
   end record;
   --  Is_Spec is needed so that Free knows which entity has been freed, since
   --  a node can be associated with multiple entities.

   overriding procedure Free (Obj : in out Entity_Sort_Annotation);

   procedure Free is new Ada.Unchecked_Deallocation
     (Sorted_Node, Sorted_Node_Access);

   --  This array provide a way of sorting / grouping entities when order
   --  is required.
   Sort_Entities : constant array (Language_Category) of Natural :=
     (Dependency_Category => 1,
      Namespace_Category  => 2,
      Type_Category       => 3,
      Subprogram_Category => 4,
      Data_Category       => 5,
      others              => 99);

   function Construct_Filter
     (Model     : not null access Outline_Model_Record'Class;
      Construct : access Simple_Construct_Information) return Boolean;
   --  Return False if the construct should be filtered out

   procedure Add_Recursive
     (Model   : access Outline_Model_Record'Class;
      New_Obj : Construct_Tree_Iterator);
   --  Add new nodes for New_Obj and its nested entities

   function Get_Sorted_Node
     (Iter : Gtk_Tree_Iter) return Sorted_Node_Access;
   --  Return the node stored in the iter

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
      Entity : Entity_Access) return Sorted_Node_Access;
   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      It     : Construct_Tree_Iterator) return Sorted_Node_Access;
   --  Return the node for the corresponding entity, or null if there is none.

   function Get_Node_Next_Part
     (Model  : access Outline_Model_Record'Class;
      Entity : Entity_Access) return Sorted_Node_Access;
   --  Same as Get_Node, but for the body or full view of the entity.

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      Entity : Entity_Access) return Sorted_Node_Access is
   begin
      return Get_Node (Model, To_Construct_Tree_Iterator (Entity));
   end Get_Node;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Model  : access Outline_Model_Record'Class;
      It     : Construct_Tree_Iterator) return Sorted_Node_Access
   is
      Tree : constant Construct_Tree := Get_Tree (Model.File);
      Annot : Annotation (Other_Kind);
   begin
      if Is_Set
        (Get_Annotation_Container (Tree, It).all, Model.Annotation_Key)
      then
         Get_Annotation
           (Get_Annotation_Container (Tree, It).all,
            Model.Annotation_Key,
            Annot);
         return Entity_Sort_Annotation (Annot.Other_Val.all).Node;
      else
         return null;
      end if;
   end Get_Node;

   ------------------------
   -- Get_Node_Next_Part --
   ------------------------

   function Get_Node_Next_Part
     (Model  : access Outline_Model_Record'Class;
      Entity : Entity_Access) return Sorted_Node_Access
   is
      E : Entity_Access;
   begin
      if Model.Filter.Group_Spec_And_Body then
         E := Find_Next_Part (Get_Tree_Language (Get_File (Entity)), Entity);
         if E /= Null_Entity_Access
           and then Get_File (E) = Model.File
         then
            return Get_Node (Model, E);
         end if;
      end if;
      return null;
   end Get_Node_Next_Part;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Obj : in out Entity_Sort_Annotation) is
      Model : constant Outline_Model := Obj.Node.Model;
      Path  : constant Gtk_Tree_Path := Get_Path (Model, Obj.Node);
      Keep  : constant Boolean :=
        (Obj.Is_Spec
         and then Obj.Node.Body_Entity /= Null_Entity_Persistent_Access)
        or else
          (not Obj.Is_Spec
           and then Obj.Node.Spec_Entity /= Null_Entity_Persistent_Access);
      Cur : Sorted_Node_Access;
   begin
      if Keep then
         --  ??? Should we update the sloc ?

         if Path /= Null_Gtk_Tree_Path then
            Row_Changed (+Model, Path, Get_Iter (+Model, Path));
         end if;

      else
         --  Kill the children if any
         Clear_Nodes (Model, Obj.Node);

         --  Then update next siblings

         if Obj.Node.Prev /= null then
            Obj.Node.Prev.Next := Obj.Node.Next;
         end if;

         if Obj.Node.Next /= null then
            Obj.Node.Next.Prev := Obj.Node.Prev;

            Cur := Obj.Node.Next;
            while Cur /= null loop
               Cur.Index_In_Siblings := Cur.Index_In_Siblings - 1;
               Cur := Cur.Next;
            end loop;
         end if;

         --  Finally, free this node

         if Obj.Node.Parent /= null then
            Obj.Node.Parent.Children.Delete (Obj.Node);
         end if;

         --  Now unlink & destroy this node

         if Path /= Null_Gtk_Tree_Path then
            Row_Deleted (+Model, Path);
         end if;
      end if;

      Path_Free (Path);

      if Obj.Is_Spec then
         Unref (Obj.Node.Spec_Entity);
         Obj.Node.Spec_Entity := Null_Entity_Persistent_Access;
      else
         Unref (Obj.Node.Body_Entity);
         Obj.Node.Body_Entity := Null_Entity_Persistent_Access;
      end if;

      if not Keep then
         Free (Obj.Node);
      end if;
   end Free;

   ----------------------
   -- Construct_Filter --
   ----------------------

   function Construct_Filter
     (Model     : not null access Outline_Model_Record'Class;
      Construct : access Simple_Construct_Information) return Boolean is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      case Construct.Category is
         when Cat_Package .. Cat_Entry
            | Cat_Field | Cat_Variable
            | Type_Category =>

            if Model.Filter.Hide_Types
              and then Construct.Category in Type_Category
            then
               return False;
            end if;

            if Model.Filter.Hide_Objects
              and then Construct.Category in Data_Category
            then
               return False;
            end if;

            if Model.Filter.Hide_Tasks
              and then (Construct.Category = Cat_Task
                          or Construct.Category = Cat_Protected)
            then
               return False;
            end if;

            if Construct.Category in Subprogram_Category
              and then Model.Filter.Hide_Declarations
              and then Construct.Is_Declaration
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

      if Construct.Name = No_Symbol then
         return False;
      end if;

      if Model.Filter_Pattern /= null then
         return Model.Filter_Pattern.Start
           (Get (Construct.Name).all) /= No_Match;
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

   begin
      if Left = Right then
         return False;

      elsif Left.Model.Filter.Sorted then
         --  Alphabetical sort
         if Sort_Entities (Left.Category) < Sort_Entities (Right.Category) then
            return True;

         elsif Sort_Entities (Left.Category)
           = Sort_Entities (Right.Category)
         then
            Comparison := Compare (Get (Left.Name).all, Get (Right.Name).all);
            if Comparison = -1 then
               return True;
            elsif Comparison = 0 then
               --  We need to have a clear and definite way to differenciate
               --  constructs, otherwise we'll have errors when adding them
               --  to the set. If we can't do that alphabetically, then
               --  we fall back to the sloc comparison.
               return Left.Sloc < Right.Sloc;
            end if;
         end if;

         return False;

      else
         --  Positional sort
         return Left.Sloc < Right.Sloc;
      end if;
   end "<";

   -------------------
   -- Add_Recursive --
   -------------------

   procedure Add_Recursive
     (Model   : access Outline_Model_Record'Class;
      New_Obj : Construct_Tree_Iterator)
   is
      Parent : Construct_Tree_Iterator;
      Parent_Node : Sorted_Node_Access;
      Root   : Sorted_Node_Access;  --  node for New_Obj
      It     : Construct_Tree_Iterator;
      Dummy  : Sorted_Node_Access;
      Path   : Gtk_Tree_Path;
      E      : Entity_Access;
      Position : Sorted_Node_Set.Cursor;
      Inserted : Boolean;
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (New_Obj);
      Annot    : Annotation (Other_Kind);
      Is_Spec  : Boolean := True;

   begin
      if Model = null or else Model.File = null then
         return;
      end if;

      Root := Get_Node (Model, New_Obj);

      if Root = null then
         --  Issue: if we have children that would match, we still need to
         --  insert them. Depending on Group_Spec_And_Body, this might mean
         --  adding the current node as well.
         if Construct_Filter (Model, Construct) then
            --  Compute parent node

            if Model.Filter.Flat_View then
               Parent_Node := Model.Phantom_Root'Access;
            elsif Construct.Category = Cat_With then
               --  Place any with-clause into top with-root node
               Parent_Node := Model.Root_With;
            else
               Parent := Get_Parent_Scope (Get_Tree (Model.File), New_Obj);
               if Parent = Null_Construct_Tree_Iterator then
                  Parent_Node := Model.Phantom_Root'Access;
               elsif Get_Node (Model, New_Obj) = null then
                  Parent_Node := Get_Node (Model, Parent);
                  if Model.Filter.Group_Spec_And_Body
                    and then Parent_Node = null
                  then
                     Parent_Node := Get_Node_Next_Part
                       (Model, To_Entity_Access (Model.File, Parent));
                  end if;
               end if;

               if Parent_Node = null then
                  --  Parent node might have been filtered
                  Parent_Node := Model.Phantom_Root'Access;
               end if;
            end if;

            --  Add the node for the new object

            E := To_Entity_Access (Model.File, New_Obj);
            if Model.Filter.Group_Spec_And_Body then
               Root := Get_Node_Next_Part (Model, E);

               if Root /= null then
                  if Construct.Is_Declaration then
                     if Root.Spec_Entity /= Null_Entity_Persistent_Access then
                        Root := null;  --  should not happen, create new node
                     else
                        Root.Spec_Entity := To_Entity_Persistent_Access (E);
                        Is_Spec := True;
                     end if;
                  else
                     if Root.Body_Entity /= Null_Entity_Persistent_Access then
                        Root := null;  --  should not happen, create new node
                     else
                        Root.Body_Entity := To_Entity_Persistent_Access (E);
                        Is_Spec := False;
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

               if not Model.Filter.Group_Spec_And_Body
                 or else Construct.Is_Declaration
               then
                  Root.Spec_Entity := To_Entity_Persistent_Access (E);
                  Is_Spec := True;
               else
                  Root.Body_Entity := To_Entity_Persistent_Access (E);
                  Is_Spec := False;
               end if;

               Root.Category := Construct.Category;
               Root.Name     := Construct.Name;
               Root.Sloc     := Construct.Sloc_Start;
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

            --  Always set the annotation on the entity, even when reusing the
            --  node.

            Annot.Other_Val := new Entity_Sort_Annotation'
              (Construct_Annotations_Pckg.General_Annotation_Record
               with Node => Root, Is_Spec => Is_Spec);
            Construct_Annotations_Pckg.Set_Annotation
              (Get_Annotation_Container (Get_Tree (Model.File), New_Obj).all,
               Model.Annotation_Key,
               Annot);
         end if;
      end if;

      --  Then add all its children recursively

      It := Next (Get_Tree (Model.File), New_Obj, Jump_Into);
      while It /= Null_Construct_Tree_Iterator
        and then Is_Parent_Scope (New_Obj, It)
      loop
         Add_Recursive (Model, It);
         It := Next (Get_Tree (Model.File), It, Jump_Over);
      end loop;
   end Add_Recursive;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Model  : not null access Outline_Model_Record'Class;
      File   : Structured_File_Access;
      Key    : Construct_Annotations_Pckg.Annotation_Key;
      Filter : Tree_Filter)
   is
      procedure Add_Root_With;
      --  Create Root_With node and append it to model

      It : Construct_Tree_Iterator;

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
         Model.Root_With.Category := Cat_With;
         Model.Root_With.Index_In_Siblings := 0;

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
      Model.Annotation_Key := Key;

      Add_Root_With;

      --  Order is important here, in case File=Model.File. This whole blocks
      --  also needs to be called after we clear the tree.
      Ref (File);
      Unref (Model.File);
      Model.File := File;

      It := First (Get_Tree (Model.File));
      while It /= Null_Construct_Tree_Iterator loop
         Add_Recursive (Model, It);
         It := Next (Get_Tree (Model.File), It, Jump_Over);
      end loop;
   end Set_File;

   --------------
   -- Get_File --
   --------------

   function Get_File (Model : Outline_Model) return Structured_File_Access is
   begin
      return Model.File;
   end Get_File;

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

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Outline_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Entity   : constant Entity_Access := Get_Entity (Self, Iter, Column);
      It       : Construct_Tree_Iterator;
      Formater : aliased Text_Profile_Formater;
   begin
      if Column = Spec_Pixbuf_Column
         or else Column = Body_Pixbuf_Column
      then
         Init (Value, GType_String);

         if Entity /= Null_Entity_Access then
            It := To_Construct_Tree_Iterator (Entity);
            Set_String (Value, Entity_Icon_Of (Get_Construct (It).all));
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
         It := To_Construct_Tree_Iterator (Entity);
         Init (Value, GType_String);

         if Get_Construct (It).Name /= No_Symbol then
            if Self.Filter.Show_Profile
              and then Get_Construct (It).Category in Subprogram_Category
            then
               Get_Profile
                 (Lang         => Get_Tree_Language (Self.File),
                  Entity       => Entity,
                  Formater     => Formater'Access);

               declare
                  Profile : constant String := Formater.Get_Text;
               begin
                  Set_String
                    (Value, Escape_Text (Get (Get_Construct (It).Name).all)
                     & " <span foreground=""#A0A0A0"">"
                     & Protect
                       (Profile (Profile'First ..
                            Integer'Min (Profile'Last, Profile'First + 500)))
                     & "</span>");
               end;
            else
               Set_String
                 (Value, Escape_Text (Get (Get_Construct (It).Name).all));
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

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self   : not null access Outline_Model_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Gint := Display_Name_Column) return Entity_Access
   is
      pragma Unreferenced (Self);
      Node   : Sorted_Node_Access;
      E      : Entity_Persistent_Access;
   begin
      if Iter = Null_Iter then
         return Null_Entity_Access;
      end if;

      Node := Get_Sorted_Node (Iter);

      if Column = Body_Pixbuf_Column then
         E := Node.Body_Entity;
      elsif Column = Spec_Pixbuf_Column then
         E := Node.Spec_Entity;
      else
         if Node.Spec_Entity /= Null_Entity_Persistent_Access then
            E := Node.Spec_Entity;
         else
            E := Node.Body_Entity;
         end if;
      end if;

      if E = Null_Entity_Persistent_Access or else not Exists (E) then
         return Null_Entity_Access;
      end if;

      return To_Entity_Access (E);
   end Get_Entity;

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

      procedure Delete_Root_With (Root : Sorted_Node_Access);
      --  Destroy Model.Root_With phantom node

      ----------------------
      -- Delete_Root_With --
      ----------------------

      procedure Delete_Root_With (Root : Sorted_Node_Access) is
         Path : constant Gtk_Tree_Path := Get_Path (Model, Root);
      begin
         --  Kill the children if any
         Clear_Nodes (Model, Root);

         --  Finally, free this node
         Root.Parent.Children.Delete (Root);

         --  Now unlink & destroy this node

         if Path /= Null_Gtk_Tree_Path then
            Row_Deleted (+Model, Path);
         end if;

         Free (Model.Root_With);
         Path_Free (Path);
      end Delete_Root_With;

      It        : Sorted_Node_Access;
      BE        : Entity_Persistent_Access;
      Construct : Construct_Tree_Iterator;
      Tree      : constant Construct_Tree := Get_Tree (Model.File);
   begin
      while not Root.Children.Is_Empty loop
         --  Start from the last element, so that the loop in Free which
         --  reindexes the elements has no effect

         It := Root.Children.Last_Element;

         --  We delete the annotations, which will also free the node when
         --  both entities have been deleted. In case there is only a spec
         --  entity, the node will be freed before we do the test for the body,
         --  so we need to capture the body first.

         BE := It.Body_Entity;

         if It.Spec_Entity /= Null_Entity_Persistent_Access then
            Construct := To_Construct_Tree_Iterator
              (To_Entity_Access (It.Spec_Entity));
            Construct_Annotations_Pckg.Free_Annotation
              (Get_Annotation_Container (Tree, Construct).all,
               Model.Annotation_Key);
         end if;

         if BE /= Null_Entity_Persistent_Access then
            Construct := To_Construct_Tree_Iterator (To_Entity_Access (BE));
            Construct_Annotations_Pckg.Free_Annotation
              (Get_Annotation_Container (Tree, Construct).all,
               Model.Annotation_Key);
         end if;

         if Model.Root_With = It then
            --  Delete phantom root of with clauses, because it hasn't annot.
            Delete_Root_With (Model.Root_With);
         end if;
      end loop;
   end Clear_Nodes;

   ----------
   -- Free --
   ----------

   procedure Free (Model : access Outline_Model_Record) is
   begin
      Clear_Nodes (Model, Model.Phantom_Root'Access);
      Unref (Model.File);
      Model.File := null;

      Free (Model.Filter_Pattern);
   end Free;

   ------------------
   -- File_Updated --
   ------------------

   procedure File_Updated
     (Model    : access Outline_Model_Record;
      File     : Structured_File_Access;
      Old_Tree : Construct_Tree;
      Kind     : Update_Kind)
   is
      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);
      --  Analyses differences between the two objects and perform object
      --  addition & deletion from the model.

      procedure Update_Node (Node : Sorted_Node_Access);
      --  Update buffered data of the node & children recursively if it exists

      -------------------
      -- Diff_Callback --
      -------------------

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind)
      is
         pragma Unreferenced (Old_Obj);
      begin
         case Kind is
            when Removed =>
               --  Nothing to be done here - nodes have already been removed
               --  when the annotation has been freed.
               null;

            when Added =>
               Add_Recursive (Model, New_Obj);

            when Preserved =>
               null;
         end case;
      end Diff_Callback;

      -----------------
      -- Update_Node --
      -----------------

      procedure Update_Node (Node : Sorted_Node_Access) is
         C   : Sorted_Node_Set.Cursor;
      begin
         if Exists (Node.Spec_Entity) then
            Node.Sloc := Get_Construct (Node.Spec_Entity).Sloc_Start;
         elsif Exists (Node.Body_Entity) then
            Node.Sloc := Get_Construct (Node.Body_Entity).Sloc_Start;
         end if;

         if Node = Model.Phantom_Root'Access
           or else Node = Model.Root_With
           or else Exists (Node.Spec_Entity)
           or else Exists (Node.Body_Entity)
         then
            C := Node.Children.First;
            while Has_Element (C) loop
               Update_Node (Element (C));
               Next (C);
            end loop;
         end if;
      end Update_Node;

   begin
      if File /= Model.File
        or else Old_Tree = Null_Construct_Tree
        or else Kind = Minor_Change
      then
         return;
      end if;

      --  We could simply Reset(Model), although we would need to modify the
      --  view to avoid flickering.

      --  First, update all the source locations for all the nodes of the
      --  model. This is needed in order to have proper ordering while
      --  adding and removing nodes

      Update_Node (Model.Phantom_Root'Access);

      --  Then, run a diff in order to add / remove nodes

      Diff
        (Lang     => Get_Tree_Language (File),
         Old_Tree => Old_Tree,
         New_Tree => Get_Tree (File),
         Callback => Diff_Callback'Unrestricted_Access);
   end File_Updated;

   ---------------------------------
   -- Get_Path_Enclosing_Location --
   ---------------------------------

   function Get_Path_Enclosing_Location
     (Model        : access Outline_Model_Record;
      Line, Column : Integer) return Gtk_Tree_Path
   is
      Tree : constant Construct_Tree := Get_Tree (Model.File);
      Node : Sorted_Node_Access := null;
      It   : Construct_Tree_Iterator;
      Path : Gtk_Tree_Path;
      E    : Entity_Access;

   begin
      if Model.File = null then
         Gtk_New (Path);
         return Path;
      end if;

      It := Get_Iterator_At
        (Tree     => Get_Tree (Model.File),
         Location => To_Location (Line, String_Index_Type (Column)),
         --  ??? not sure if that conversion is accurate - we don't know
         --  the type of column here!
         Position => Enclosing);
      while It /= Null_Construct_Tree_Iterator loop
         Node := Get_Node (Model, It);
         exit when Node /= null;

         E := To_Entity_Access (Model.File, It);
         Node := Get_Node_Next_Part (Model, E);
         exit when Node /= null;

         --  When there are several constructs on the same line Get_Iterator_At
         --  returns last one. Here we iterate over others on the same line
         --  before search for enclosing constructs.
         declare
            Try_Prev : constant Construct_Tree_Iterator :=
              Prev (Tree, It, Jump_Over);
         begin
            if Try_Prev /= Null_Construct_Tree_Iterator
              and then Get_Construct (Try_Prev).Sloc_Start.Line = Line
            then
               It := Try_Prev;
            else
               It := Get_Parent_Scope (Tree, It);
            end if;
         end;
      end loop;

      if Node = null then
         Gtk_New (Path);
         return Path;
      else
         return Get_Path (Model, Node);
      end if;
   end Get_Path_Enclosing_Location;

end Outline_View.Model;
