-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009-2010, AdaCore                 --
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

with Traces; use Traces;

package body Outline_View.Model is

   use Construct_Annotations_Pckg;

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

   procedure Compute_Sorted_Nodes
     (Model : access Outline_Model_Record'Class; Root : Sorted_Node_Access);

   function New_Node
     (Model  : access Outline_Model_Record'Class;
      Entity : Entity_Persistent_Access) return Sorted_Node_Access;
   --  Create a new node, and connect it to the entity via the appropriate
   --  annotation

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

   function Sort_And_Add
     (Model : access Outline_Model_Record'Class;
      Root  : Sorted_Node_Access;
      It    : Construct_Tree_Iterator) return Sorted_Node_Access;
   --  Add and sort the new item. This doesn't update sibling indexes

   procedure Add_In_Model
     (Model   : access Outline_Model_Record'Class;
      New_Obj : Construct_Tree_Iterator);
   --  Adds the iterator in the internal model

   ----------
   -- Free --
   ----------

   overriding procedure Free (Obj : in out Entity_Sort_Annotation) is
      Path : constant Gtk_Tree_Path := Get_Path (Obj.Model, Obj.Node);
   begin
      --  Kill the children if any
      Clear_Nodes (Obj.Model, Obj.Node);

      --  Now unlink & destroy this node

      if Path /= null then
         Row_Deleted (Obj.Model, Path);
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
      Free (Obj.Node.Name);
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
         when Subprogram_Explorer_Category
            | Cat_Package .. Cat_Task
            | Cat_Field | Cat_Variable
            | Type_Category =>

            if Filter.Hide_Types
              and then Construct.Category in Type_Category
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

      return Construct.Name /= null;
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
                 (Left.Name.all, Right.Name.all);

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

   ------------------
   -- Sort_And_Add --
   ------------------

   function Sort_And_Add
     (Model : access Outline_Model_Record'Class;
      Root  : Sorted_Node_Access;
      It    : Construct_Tree_Iterator) return Sorted_Node_Access
   is
      use Sorted_Node_Set;

      Node_It  : Sorted_Node_Access;
      Node     : Sorted_Node_Access;
      Position : Sorted_Node_Set.Cursor;
      Inserted : Boolean;

   begin
      Node := New_Node
        (Model,
         To_Entity_Persistent_Access (To_Entity_Access (Model.File, It)));

      Node.Parent := Root;

      Insert
        (Container => Root.Ordered_Index,
         New_Item  => Node,
         Position  => Position,
         Inserted  => Inserted);

      if Inserted then
         if Previous (Position) = Sorted_Node_Set.No_Element then
            Root.First_Child := Node;

         else
            Node_It := Element (Previous (Position));

            Node_It.Next := Node;
            Node.Prev := Node_It;
         end if;

         if Next (Position) = Sorted_Node_Set.No_Element then
            Root.Last_Child := Node;

         else
            Node_It := Element (Next (Position));

            Node_It.Prev := Node;
            Node.Next := Node_It;
         end if;

         Root.N_Children := Root.N_Children + 1;

         return Node;
      end if;

      raise Outline_Error with "couldn't add sorted element";
   end Sort_And_Add;

   --------------------------
   -- Compute_Sorted_Nodes --
   --------------------------

   procedure Compute_Sorted_Nodes
     (Model : access Outline_Model_Record'Class; Root : Sorted_Node_Access)
   is
      File   : Structured_File_Access;
      Parent : Construct_Tree_Iterator;

      It     : Construct_Tree_Iterator;
      Dummy  : Sorted_Node_Access;
      pragma Unreferenced (Dummy);
   begin
      if Model = null or else Model.File = null then
         return;
      end if;

      File := Model.File;

      if Root.N_Children /= -1 then
         --  In this case, we've already done the computation, just return
         --  the value
         return;

      elsif Root = Model.Phantom_Root'Access then
         --  If we're on the initial root node, then there's no parent
         Parent := Null_Construct_Tree_Iterator;

      else
         Parent := To_Construct_Tree_Iterator (To_Entity_Access (Root.Entity));
      end if;

      Root.N_Children := 0;

      if Parent = Null_Construct_Tree_Iterator then
         It := First (Get_Tree (File));
      else
         It := Next (Get_Tree (File), Parent, Jump_Into);
      end if;

      if It = Null_Construct_Tree_Iterator then
         return;
      end if;

      while It /= Null_Construct_Tree_Iterator
        and then Get_Parent_Scope (Get_Tree (File), It) = Parent
      loop
         if Construct_Filter (Model.Filter, Get_Construct (It)) then
            Dummy := Sort_And_Add (Model, Root, It);
         end if;

         It := Next (Get_Tree (File), It, Jump_Over);
      end loop;

      declare
         Node_It : Sorted_Node_Access := Root.First_Child;
         Ind : Natural := 0;
      begin
         while Node_It /= null loop
            Node_It.Index_In_Siblings := Ind;
            Ind := Ind + 1;
            Node_It := Node_It.Next;
         end loop;
      end;
   end Compute_Sorted_Nodes;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Model  : access Outline_Model_Record'Class;
      Entity : Entity_Persistent_Access) return Sorted_Node_Access
   is
      Node      : constant Sorted_Node_Access := new Sorted_Node;
      Annot     : Annotation (Other_Kind);
      Construct : constant Simple_Construct_Information :=
                    Get_Construct (Entity);
   begin
      Annot.Other_Val := new Entity_Sort_Annotation'
        (Construct_Annotations_Pckg.General_Annotation_Record
         with Node => Node, Model => Outline_Model (Model));

      Construct_Annotations_Pckg.Set_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (To_Entity_Access (Entity))).all,
         Model.Annotation_Key,
         Annot);

      Node.Entity := Entity;
      Ref (Node.Entity);

      if Model.Sorted then
         Node.Order_Kind := Alphabetical;
      else
         Node.Order_Kind := Positional;
      end if;

      Node.Category := Construct.Category;
      Node.Name := new String'(Construct.Name.all);
      Node.Sloc := Construct.Sloc_Start;

      return Node;
   end New_Node;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Node : Sorted_Node_Access) return Entity_Persistent_Access is
   begin
      return Node.Entity;
   end Get_Entity;

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
         Model.Phantom_Root.N_Children := 0;

         declare
            It    : Construct_Tree_Iterator;
         begin
            It := First (Get_Tree (Model.File));

            while It /= Null_Construct_Tree_Iterator loop
               if Construct_Filter (Filter, Get_Construct (It)) then
                  Add_In_Model (Model, It);
               end if;

               It := Next (Get_Tree (Model.File), It, Jump_Over);
            end loop;
         end;
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
      return 2;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return 2;
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
      if Index = Pixbuf_Column then
         return Gdk.Pixbuf.Get_Type;
      elsif Index = Display_Name_Column then
         return GType_String;
      end if;

      return GType_String;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
               Compute_Sorted_Nodes (Self, Current);
               Current := Nth_Child (Self, Current, Indices (J));
            end loop;

            return New_Iter (Current);
         end;
      else
         return Null_Iter;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Null_Iter;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Self : access Outline_Model_Record'Class;
      Node : Sorted_Node_Access) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Path : constant Gtk_Tree_Path := Gtk_New;
      Cur  : Sorted_Node_Access := Node;
   begin
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

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Gtk_New ("");
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
      Entity : constant Entity_Persistent_Access := Get_Entity (Iter);
      It     : Construct_Tree_Iterator;
   begin
      It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));

      if Column = Pixbuf_Column then
         Init (Value, Gdk.Pixbuf.Get_Type);
         Set_Object
           (Value, GObject (Entity_Icon_Of (Get_Construct (It).all)));

      elsif Column = Display_Name_Column then
         Init (Value, GType_String);

         if Get_Construct (It).Name /= null then
            if Self.Filter.Show_Profile
              and then Get_Construct (It).Category in Subprogram_Category
            then
               Set_String
                 (Value, Escape_Text (Get_Construct (It).Name.all)
                  & " <span foreground=""#A0A0A0"">"
                  & Get_Profile
                    (Get_Tree_Language (Self.File),
                     To_Entity_Access (Entity),
                     500)
                  & "</span>");
            else
               Set_String (Value, Escape_Text (Get_Construct (It).Name.all));
            end if;

         else
            Set_String (Value, "no name");
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
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

      Compute_Sorted_Nodes (Self, Node);

      return New_Iter (Node.First_Child);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Null_Iter;
   end Children;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is
   begin
      return Children (Self, Iter) /= Null_Iter;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
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

      Compute_Sorted_Nodes (Self, Node);

      return Gint (Node.N_Children);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return 0;
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
         Compute_Sorted_Nodes (Model, Model.Phantom_Root'Access);

         Cur_It := Model.Phantom_Root.First_Child;
      else
         Compute_Sorted_Nodes (Model, Node);

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
         Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
         return Null_Iter;
   end Parent;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Iter : Gtk_Tree_Iter) return Entity_Persistent_Access
   is
   begin
      if Iter = Null_Iter then
         return Null_Entity_Persistent_Access;
      else
         return Get_Entity (Get_Sorted_Node (Iter));
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
   -- Add_In_Model --
   ------------------

   procedure Add_In_Model
     (Model   : access Outline_Model_Record'Class;
      New_Obj : Construct_Tree_Iterator)
   is
      File         : constant Structured_File_Access := Model.File;
      Parent       : constant Construct_Tree_Iterator :=
                       Get_Parent_Scope (Get_Tree (File), New_Obj);
      Parent_Annot : Annotation (Other_Kind);
      Parent_Node  : Sorted_Node_Access;
      Child_Node   : Sorted_Node_Access;
      Path         : Gtk_Tree_Path;
   begin
      if Parent = Null_Construct_Tree_Iterator then
         Parent_Node := Model.Phantom_Root'Access;

      else
         if Is_Set
           (Get_Annotation_Container (Get_Tree (File), Parent).all,
            Model.Annotation_Key)
           and then not Is_Set
             (Get_Annotation_Container (Get_Tree (File), New_Obj).all,
              Model.Annotation_Key)
         then
            --  If the parent is in the model, then retreive the node and
            --  check if we need to explicitely add the child.
            --  The child may have been already added, which is tested by
            --  the Is_Set on the annotation. In which case, there's nothing
            --  to do here.

            Get_Annotation
              (Get_Annotation_Container (Get_Tree (File), Parent).all,
               Model.Annotation_Key,
               Parent_Annot);

            Parent_Node :=
              Entity_Sort_Annotation (Parent_Annot.Other_Val.all).Node;
         end if;
      end if;

      if Parent_Node /= null and then Parent_Node.N_Children /= -1 then
         --  In this case, the children of this node are in the model, so
         --  we need to add that new node as well

         Child_Node := Sort_And_Add (Model, Parent_Node, New_Obj);

         if Child_Node.Prev /= null then
            Child_Node.Index_In_Siblings :=
              Child_Node.Prev.Index_In_Siblings + 1;
         else
            Child_Node.Index_In_Siblings := 0;
         end if;

         declare
            Tmp_Node : Sorted_Node_Access := Child_Node.Next;
         begin
            while Tmp_Node /= null loop
               Tmp_Node.Index_In_Siblings :=
                 Tmp_Node.Index_In_Siblings + 1;
               Tmp_Node := Tmp_Node.Next;
            end loop;
         end;

         --  Notify the model of the change

         Path := Get_Path (Model, Child_Node);
         Row_Inserted (Model, Path, New_Iter (Child_Node));
         Path_Free (Path);

         --  Get the first level children and add them to the model

         Compute_Sorted_Nodes (Model, Child_Node);

         declare
            Tmp_Node : Sorted_Node_Access := Child_Node.First_Child;
         begin
            while Tmp_Node /= null loop
               Path := Get_Path (Model, Tmp_Node);
               Row_Inserted (Model, Path, New_Iter (Tmp_Node));
               Path_Free (Path);

               Tmp_Node := Tmp_Node.Next;
            end loop;
         end;
      end if;
   end Add_In_Model;

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
               if Construct_Filter (Model.Filter, Get_Construct (New_Obj)) then
                  Add_In_Model (Model, New_Obj);
               end if;

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

      --  First, update all the source locations for all the nodes of the
      --  model. This is needed in order to have proper ordering while
      --  adding and removing ndoes

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
      Tree      : constant Construct_Tree := Get_Tree (Model.File);
      Last_Node : Sorted_Node_Access := null;

      procedure Open_Node (It : Construct_Tree_Iterator);
      --  Open all the nodes to the iterator given in parameter

      ---------------
      -- Open_Node --
      ---------------

      procedure Open_Node (It : Construct_Tree_Iterator) is
         Annot : Annotation (Other_Kind);
      begin
         if It = Null_Construct_Tree_Iterator then
            return;
         end if;

         if not Is_Set
           (Get_Annotation_Container (Tree, It).all,
            Model.Annotation_Key)
         then
            Open_Node (Get_Parent_Scope (Tree, It));
         end if;

         if Is_Set
           (Get_Annotation_Container (Tree, It).all,
            Model.Annotation_Key)
         then
            Get_Annotation
              (Get_Annotation_Container (Tree, It).all,
               Model.Annotation_Key,
               Annot);

            Last_Node := Entity_Sort_Annotation (Annot.Other_Val.all).Node;

            Compute_Sorted_Nodes (Model, Last_Node);
         end if;
      end Open_Node;

   begin
      if Model.File = null then
         return Gtk_New;
      end if;

      Open_Node
        (Get_Iterator_At
           (Tree     => Get_Tree (Model.File),
            Location => To_Location (Line,  String_Index_Type (Column)),
            --  ??? not sure if that conversion is accurate - we don't know
            --  the type of column here!
            Position => Enclosing));

      if Last_Node = null then
         return Gtk_New;
      else
         return Get_Path (Model, Last_Node);
      end if;
   end Get_Path_Enclosing_Location;

end Outline_View.Model;
