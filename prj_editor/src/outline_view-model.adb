------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with Gtk.Tree_Model.Utils;        use Gtk.Tree_Model.Utils;

with Basic_Types;                 use Basic_Types;
with Project_Explorers_Common;    use Project_Explorers_Common;
with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with GNATCOLL.Traces;             use GNATCOLL.Traces;

package body Outline_View.Model is
   Me : constant Trace_Handle := Create ("OUTLINE");

   use Construct_Annotations_Pckg;
   use Sorted_Node_Set;

   type Entity_Sort_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Node  : Sorted_Node_Access;
      Model : Outline_Model;
   end record;

   overriding procedure Free (Obj : in out Entity_Sort_Annotation);

   procedure Free is new Ada.Unchecked_Deallocation
     (Sorted_Node, Sorted_Node_Access);

   --  This array provide a way of sorting / grouping entities when order
   --  is required.
   Sort_Entities : constant array (Language_Category) of Natural :=
     (Namespace_Category  => 1,
      Subprogram_Category => 3,
      Type_Category       => 2,
      Data_Category       => 4,
      others              => 99);

   function Construct_Filter
     (Filter    : Tree_Filter;
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

   procedure Reset
     (Model : not null access Outline_Model_Record'Class);
   --  Recompute the whole contents of the model

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
      if Model.Group_Spec_And_Body then
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
      Path : constant Gtk_Tree_Path := Get_Path (Obj.Model, Obj.Node);
   begin
      --  Kill the children if any
      Clear_Nodes (Obj.Model, Obj.Node);

      --  Now unlink & destroy this node

      if Path /= Null_Gtk_Tree_Path then
         Row_Deleted (+Obj.Model, Path);
         Path_Free (Path);
      end if;

      --  First, ensuire parent consistency

      if Obj.Node.Parent /= null then
         if Obj.Node.Parent.First_Child = Obj.Node then
            Obj.Node.Parent.First_Child := Obj.Node.Next;
         end if;

         if Obj.Node.Parent.Last_Child = Obj.Node then
            Obj.Node.Parent.Last_Child := Obj.Node.Prev;
         end if;

         Obj.Node.Parent.N_Children := Obj.Node.Parent.N_Children - 1;
      end if;

      --  Then, change the previous siblings

      if Obj.Node.Prev /= null then
         Obj.Node.Prev.Next := Obj.Node.Next;
      end if;

      --  Then, updated next siblings

      if Obj.Node.Next /= null then
         Obj.Node.Next.Prev := Obj.Node.Prev;

         --  ??? We should have means to optimize this loop when supressing
         --  multiple nodes
         declare
            Cur : Sorted_Node_Access := Obj.Node.Next;
         begin
            while Cur /= null loop
               Cur.Index_In_Siblings := Cur.Index_In_Siblings - 1;

               Cur := Cur.Next;
            end loop;
         end;
      end if;

      --  Finally, free this node

      if Obj.Node.Parent /= null then
         Obj.Node.Parent.Ordered_Index.Delete (Obj.Node);
      end if;

      Unref (Obj.Node.Entity);
      Free (Obj.Node);
   end Free;

   ----------------------
   -- Construct_Filter --
   ----------------------

   function Construct_Filter
     (Filter    : Tree_Filter;
      Construct : access Simple_Construct_Information) return Boolean is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      case Construct.Category is
         when Cat_Package .. Cat_Entry
            | Cat_Field | Cat_Variable
            | Type_Category =>

            if Filter.Hide_Types
              and then Construct.Category in Type_Category
            then
               return False;
            end if;

            if Filter.Hide_Objects
              and then Construct.Category in Data_Category
            then
               return False;
            end if;

            if Filter.Hide_Tasks
              and then (Construct.Category = Cat_Task
                          or Construct.Category = Cat_Protected)
            then
               return False;
            end if;

            if Construct.Category in Subprogram_Category
              and then Filter.Hide_Declarations
              and then Construct.Is_Declaration
            then
               return False;
            end if;

         when others =>
            return False;
      end case;

      return Construct.Name /= No_Symbol;
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
      case Left.Order_Kind is
         when Alphabetical =>
            if Sort_Entities (Left.Category)
              < Sort_Entities (Right.Category)
            then
               return True;

            elsif Sort_Entities (Left.Category)
              = Sort_Entities (Right.Category)
            then
               Comparison := Compare
                 (Get (Left.Name).all, Get (Right.Name).all);

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

         when Positional =>
            return Left.Sloc < Right.Sloc;
      end case;
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

   begin
      if Model = null or else Model.File = null then
         return;
      end if;

      if not Construct_Filter (Model.Filter, Construct) then
         return;
      end if;

      --  Compute parent node

      if Model.Flat_View then
         Parent_Node := Model.Phantom_Root'Access;
      else
         Parent := Get_Parent_Scope (Get_Tree (Model.File), New_Obj);
         if Parent = Null_Construct_Tree_Iterator then
            Parent_Node := Model.Phantom_Root'Access;
         elsif Get_Node (Model, New_Obj) = null then
            Parent_Node := Get_Node (Model, Parent);
         end if;

         if Parent_Node = null then
            return;
         end if;
      end if;

      --  Add the node for the new object

      E := To_Entity_Access (Model.File, New_Obj);
      if Model.Group_Spec_And_Body
        and then Get_Node_Next_Part (Model, E) /= null
      then
         --  Already have a node for the other part
         Root := null;
      else
         Root := new Sorted_Node;
         Root.Entity   := To_Entity_Persistent_Access (E);
         Root.Category := Construct.Category;
         Root.Name     := Construct.Name;
         Root.Sloc     := Construct.Sloc_Start;
         Root.Parent   := Parent_Node;

         if Model.Sorted then
            Root.Order_Kind := Alphabetical;
         else
            Root.Order_Kind := Positional;
         end if;

         Annot.Other_Val := new Entity_Sort_Annotation'
           (Construct_Annotations_Pckg.General_Annotation_Record
            with Node => Root, Model => Outline_Model (Model));
         Construct_Annotations_Pckg.Set_Annotation
           (Get_Annotation_Container (Get_Tree (Model.File), New_Obj).all,
            Model.Annotation_Key,
            Annot);

         Insert
           (Container => Parent_Node.Ordered_Index,
            New_Item  => Root,
            Position  => Position,
            Inserted  => Inserted);

         if Inserted then
            if Previous (Position) = Sorted_Node_Set.No_Element then
               Parent_Node.First_Child := Root;
               Root.Index_In_Siblings := 0;
            else
               Dummy := Element (Previous (Position));
               Dummy.Next := Root;
               Root.Prev := Dummy;
               Root.Index_In_Siblings := Root.Prev.Index_In_Siblings + 1;
            end if;

            if Next (Position) = Sorted_Node_Set.No_Element then
               Parent_Node.Last_Child := Root;
            else
               Dummy := Element (Next (Position));
               Dummy.Prev := Root;
               Root.Next := Dummy;

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

      --  Then add all its children recursively

      It := Next (Get_Tree (Model.File), New_Obj, Jump_Into);
      while It /= Null_Construct_Tree_Iterator
        and then Is_Parent_Scope (New_Obj, It)
      loop
         Add_Recursive (Model, It);
         It := Next (Get_Tree (Model.File), It, Jump_Over);
      end loop;
   end Add_Recursive;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Node : Sorted_Node_Access) return Entity_Persistent_Access is
   begin
      return Node.Entity;
   end Get_Entity;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Model : not null access Outline_Model_Record'Class)
   is
      It : Construct_Tree_Iterator := First (Get_Tree (Model.File));
   begin
      Model.Phantom_Root.Ordered_Index.Clear;
      Model.Phantom_Root.N_Children := 0;

      while It /= Null_Construct_Tree_Iterator loop
         Add_Recursive (Model, It);
         It := Next (Get_Tree (Model.File), It, Jump_Over);
      end loop;
   end Reset;

   ----------------
   -- Init_Model --
   ----------------

   procedure Init_Model
     (Model     : access Outline_Model_Record'Class;
      Key       : Construct_Annotations_Pckg.Annotation_Key;
      File      : Structured_File_Access;
      Filter    : Tree_Filter;
      Sort      : Boolean;
      Add_Roots : Boolean := False)
   is
   begin
      Model.Annotation_Key := Key;
      Model.File := File;
      Model.Filter := Filter;
      Model.Sorted := Sort;

      Ref (File);

      if Add_Roots then
         Reset (Model);
      end if;
   end Init_Model;

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
         return Gdk.Pixbuf.Get_Type;
      elsif Index = Body_Pixbuf_Column then
         return Gdk.Pixbuf.Get_Type;
      elsif Index = Display_Name_Column then
         return GType_String;
      end if;

      return GType_String;

   exception
      when E : others =>
         Trace (Me, E);
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
      Entity : constant Entity_Access := Get_Entity (Self, Iter, Column);
      It     : Construct_Tree_Iterator;
   begin
      if Column = Spec_Pixbuf_Column then
         It := To_Construct_Tree_Iterator (Entity);
         Init (Value, Gdk.Pixbuf.Get_Type);
         Set_Object
           (Value, GObject (Entity_Icon_Of (Get_Construct (It).all)));

      elsif Column = Body_Pixbuf_Column then
         Init (Value, Gdk.Pixbuf.Get_Type);

         if Entity = Null_Entity_Access then
            Set_Object (Value, null);
         else
            It := To_Construct_Tree_Iterator (Entity);
            Set_Object
              (Value, GObject (Entity_Icon_Of (Get_Construct (It).all)));
         end if;

      elsif Column = Display_Name_Column then
         It := To_Construct_Tree_Iterator (Entity);
         Init (Value, GType_String);

         if Get_Construct (It).Name /= No_Symbol then
            if Self.Filter.Show_Profile
              and then Get_Construct (It).Category in Subprogram_Category
            then
               declare
                  Profile : constant String :=
                    Get_Profile
                    (Get_Tree_Language (Self.File),
                     Entity,
                     Raw_Format => True);
               begin
                  Set_String
                    (Value, Escape_Text (Get (Get_Construct (It).Name).all)
                     & " <span foreground=""#A0A0A0"">"
                     & Profile (Profile'First ..
                         Integer'Min (Profile'Last, Profile'First + 500))
                     & "</span>");
               end;
            else
               Set_String
                 (Value, Escape_Text (Get (Get_Construct (It).Name).all));
            end if;

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
   begin
      if Parent = Null_Iter then
         Node := Self.Phantom_Root'Access;
      else
         Node := Get_Sorted_Node (Parent);
      end if;

      return New_Iter (Node.First_Child);
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

      return Gint (Node.N_Children);
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Model : access Outline_Model_Record;
      Node  : Sorted_Node_Access;
      Nth   : Gint) return Sorted_Node_Access
   is
      Cur_It : Sorted_Node_Access;
   begin
      if Node = null then
         Cur_It := Model.Phantom_Root.First_Child;
      else
         Cur_It := Node.First_Child;
      end if;

      for J in 1 .. Nth loop
         if Cur_It = null then
            return null;
         end if;

         Cur_It := Cur_It.Next;
      end loop;

      return Cur_It;
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

   exception
      when E : others =>
         Trace (Me, E);
         return Null_Iter;
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

   exception
      when E : others =>
         Trace (Me, E);
         return Null_Iter;
   end Parent;

   -----------------------------
   -- Set_Group_Spec_And_Body --
   -----------------------------

   procedure Set_Group_Spec_And_Body
     (Model : not null access Outline_Model_Record'Class;
      Group : Boolean)
   is
   begin
      Model.Group_Spec_And_Body := Group;
   end Set_Group_Spec_And_Body;

   -------------------
   -- Set_Flat_View --
   -------------------

   procedure Set_Flat_View
     (Model : not null access Outline_Model_Record'Class;
      Flat  : Boolean)
   is
   begin
      Model.Flat_View := Flat;
   end Set_Flat_View;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self   : not null access Outline_Model_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Gint := Spec_Pixbuf_Column) return Entity_Access
   is
      E      : Entity_Persistent_Access;
      E2, E3 : Entity_Access;
   begin
      if Iter = Null_Iter then
         return Null_Entity_Access;
      end if;

      E := Get_Entity (Get_Sorted_Node (Iter));
      if E = Null_Entity_Persistent_Access or else not Exists (E) then
         return Null_Entity_Access;
      end if;

      E3 := To_Entity_Access (E);

      if Self.Group_Spec_And_Body and then Column = Body_Pixbuf_Column then
         E2 := Find_Next_Part (Get_Tree_Language (Get_File (E)), E3);

         --  Only show the "other" part if it is in the same file, otherwise
         --  it is irrelevant.
         if E3 = E2
           or else Get_File (E3) /= Get_File (E2)
         then
            return Null_Entity_Access;
         else
            return E2;
         end if;

      else
         return E3;
      end if;
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
      It        : Sorted_Node_Access;
      Construct : Construct_Tree_Iterator;
   begin
      It := Root.First_Child;

      while It /= null loop
         Construct := To_Construct_Tree_Iterator
           (To_Entity_Access (It.Entity));

         --  Deleting the annotation will have as effect the destruction of
         --  the node, which is why we need to iterate before.
         It := It.Next;

         Construct_Annotations_Pckg.Free_Annotation
           (Get_Annotation_Container
              (Get_Tree (Model.File), Construct).all,
            Model.Annotation_Key);
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
         Cur : Sorted_Node_Access;
      begin
         if Exists (Node.Entity) then
            Node.Sloc := Get_Construct (Node.Entity).Sloc_Start;
         end if;

         if Node = Model.Phantom_Root'Access or else Exists (Node.Entity) then
            Cur := Node.First_Child;
            while Cur /= null loop
               Update_Node (Cur);
               Cur := Cur.Next;
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

         It := Get_Parent_Scope (Tree, It);
      end loop;

      if Node = null then
         Gtk_New (Path);
         return Path;
      else
         return Get_Path (Model, Node);
      end if;
   end Get_Path_Enclosing_Location;

end Outline_View.Model;
