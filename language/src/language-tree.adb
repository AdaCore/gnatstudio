------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with String_Utils;            use String_Utils;

package body Language.Tree is

   Me : constant Trace_Handle := Create ("GPS.LANGUAGE.TREE");

   --------------
   -- Contains --
   --------------

   function Contains (Scope, Item : Construct_Access) return Boolean is
   begin
      return Scope.Info.Sloc_Start <= Item.Info.Sloc_Start
        and then Scope.Info.Sloc_End >= Item.Info.Sloc_End;
   end Contains;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Construct_Tree) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Construct_Tree_Record, Construct_Tree);
      procedure Internal is new Ada.Unchecked_Deallocation
        (Referenced_Identifiers_List_Record, Access_Referenced_List);
      Ref     : Referenced_Identifiers_List;
      Garbage : Referenced_Identifiers_List;
   begin
      if Tree /= null then
         for J in Tree.Contents'Range loop
            Ref := Tree.Contents (J).Referenced_Ids;

            while Ref.Contents /= null loop
               Garbage := Ref;
               Ref := Ref.Contents.Next;
               Internal (Garbage.Contents);
            end loop;
         end loop;

         Internal (Tree);
      end if;
   end Free;

   ----------------------
   -- Free_Annotations --
   ----------------------

   procedure Free_Annotations (Tree : in out Construct_Tree) is
   begin
      if Tree /= null then
         for J in Tree.Contents'Range loop
            Construct_Annotations_Pckg.Free (Tree.Contents (J).Annotations);
         end loop;

         Tree_Annotations_Pckg.Free (Tree.Annotations);
      end if;
   end Free_Annotations;

   -----------------------
   -- To_Construct_Tree --
   -----------------------

   function To_Construct_Tree
     (List      : access Construct_List;
      Free_List : Boolean := False)
      return Construct_Tree
   is
      Size              : constant Natural := List.Size;
      Current_Construct : Construct_Access;
   begin
      if Size = 0 then
         return new Construct_Tree_Record (0);
      end if;

      declare
         Tree       : constant Construct_Tree :=
           new Construct_Tree_Record (Size);
         Tree_Index : Positive := Size + 1;

         procedure Analyze_Construct;

         procedure Analyze_Construct is
            Parent         : constant Construct_Access := Current_Construct;
            Start_Index    : constant Positive := Tree_Index;
            Previous_Index : Positive;
         begin
            Current_Construct := Current_Construct.Prev;

            while Current_Construct /= null
              and then Contains (Parent, Current_Construct)
            loop
               Previous_Index := Tree_Index;
               pragma Warnings (Off);
               --  We know that we don't have an infinite recursion here
               Analyze_Construct;
               pragma Warnings (On);

               if Previous_Index in Tree.Contents'Range then
                  --  This is false when we are on the root node
                  Tree.Contents (Previous_Index).Previous_Sibling_Index :=
                    Tree_Index;
               end if;
            end loop;

            Tree_Index := Tree_Index - 1;

            Tree.Contents (Tree_Index).Construct := Parent.Info;
            Tree.Contents (Tree_Index).Sub_Nodes_Length :=
              Start_Index - Tree_Index - 1;

            declare
               J         : Integer := Tree_Index + 1;
               End_Bound : constant Integer :=
                 Tree_Index + Tree.Contents (Tree_Index).Sub_Nodes_Length;
            begin
               while J <= End_Bound loop
                  Tree.Contents (J).Parent_Index := Tree_Index;

                  J := J + Tree.Contents (J).Sub_Nodes_Length + 1;
               end loop;
            end;
         end Analyze_Construct;

         Previous_Index : Positive;

      begin
         Current_Construct := List.Last;

         while Current_Construct /= null loop
            Previous_Index := Tree_Index;
            Analyze_Construct;

            if Previous_Index in Tree.Contents'Range then
               --  This is false when we are on the root node
               Tree.Contents (Previous_Index).Previous_Sibling_Index :=
                 Tree_Index;
            end if;
         end loop;

         if Free_List then
            Free (List.all);
         end if;

         return Tree;
      end;
   end To_Construct_Tree;

   -----------------------
   -- To_Construct_Tree --
   -----------------------

   function To_Construct_Tree
     (File   : GNATCOLL.VFS.Virtual_File;
      Buffer : String;
      Lang   : access Language_Root'Class)
      return Construct_Tree
   is
      List : aliased Construct_List;
   begin
      Parse_Constructs (Lang, File, Buffer, List);

      return To_Construct_Tree (List'Access, True);
   end To_Construct_Tree;

   ------------------------------
   -- Get_Annotation_Container --
   ------------------------------

   function Get_Annotation_Container
     (Tree : Construct_Tree)
      return access Tree_Annotations_Pckg.Annotation_Container
   is
   begin
      return Tree.Annotations'Access;
   end Get_Annotation_Container;

   -----------
   -- First --
   -----------

   function First (Tree : Construct_Tree) return Construct_Tree_Iterator is
   begin
      if Tree = Null_Construct_Tree then
         return Null_Construct_Tree_Iterator;
      elsif Tree.Contents'Length > 0 then
         return (Tree.Contents (1)'Access, 1);
      else
         return Null_Construct_Tree_Iterator;
      end if;
   end First;

   ----------
   -- Last --
   ----------

   function Last (Tree : Construct_Tree) return Construct_Tree_Iterator is
   begin
      if Tree.Contents'Length > 0 then
         return
           (Tree.Contents (Tree.Contents'Last)'Access, Tree.Contents'Last);
      else
         return Null_Construct_Tree_Iterator;
      end if;
   end Last;

   ----------------------
   -- Get_Parent_Scope --
   ----------------------

   function Get_Parent_Scope
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
     return Construct_Tree_Iterator
   is
   begin
      if Tree = null then
         return Null_Construct_Tree_Iterator;
      end if;

      if Active (Me) then
         Assert
            (Me, Iter.Node.Parent_Index = 0
             or else Iter.Node.Parent_Index in Tree.Contents'Range,
             "Get_Parent_Scope: invalid index"
             & Iter.Node.Parent_Index'Img
             & " Tree.Contents="
             & Tree.Contents'First'Img & ".."
             & Tree.Contents'Last'Img & " ",
             Raise_Exception => False);
      end if;

      if Iter.Node.Parent_Index /= 0
         and then Iter.Node.Parent_Index in Tree.Contents'Range
      then
         return
           (Tree.Contents
              (Iter.Node.Parent_Index)'Access, Iter.Node.Parent_Index);
      else
         return Null_Construct_Tree_Iterator;
      end if;
   end Get_Parent_Scope;

   ---------------------
   -- Is_Parent_Scope --
   ---------------------

   function Is_Parent_Scope
     (Scope, It : Construct_Tree_Iterator) return Boolean is
   begin
      return It /= Null_Construct_Tree_Iterator
        and then Scope.Index = It.Node.Parent_Index;
   end Is_Parent_Scope;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Iter : Construct_Tree_Iterator)
      return access Simple_Construct_Information
   is
   begin
      return Iter.Node.Construct'Access;
   end Get_Construct;

   -----------
   -- Is_In --
   -----------

   function Is_In
     (Cat : Language_Category; Categories : Category_Array) return Boolean
   is
   begin
      if Categories'Length = 0 then
         return True;
      end if;

      for J in Categories'Range loop
         if Cat = Categories (J) then
            return True;
         end if;
      end loop;

      return False;
   end Is_In;

   --------------------
   -- Is_In_Category --
   --------------------

   function Is_In_Category
     (Construct : Simple_Construct_Information; Categories : Category_Array)
      return Boolean
   is
   begin
      return Is_In (Construct.Category, Categories);
   end Is_In_Category;

   ----------------------
   -- Get_Child_Number --
   ----------------------

   function Get_Child_Number (Iter : Construct_Tree_Iterator) return Natural is
   begin
      return Iter.Node.Sub_Nodes_Length;
   end Get_Child_Number;

   ---------
   -- "=" --
   ---------

   function "="
     (Left : Text_Location; Right : Source_Location) return Boolean is
   begin
      case Left.Absolute_Offset is
         when True =>
            return Left.Offset = String_Index_Type (Right.Index);
         when False =>
            return Left.Line = Right.Line
              and then Left.Line_Offset = String_Index_Type (Right.Column);
      end case;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left : Referenced_Identifiers_List; Right : Normalized_Symbol)
      return Boolean
   is
   begin
      return Left.Contents /= null
        and then Left.Contents.Element = Right
        and then Left.Contents.Next.Contents = null;
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left : Normalized_Symbol; Right : Referenced_Identifiers_List)
      return Boolean
   is
   begin
      return Right.Contents /= null
        and then Right.Contents.Element = Left
        and then Right.Contents.Next.Contents = null;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<"
     (Left : Text_Location; Right : Source_Location) return Boolean is
   begin
      case Left.Absolute_Offset is
         when True =>
            return Left.Offset < String_Index_Type (Right.Index);
         when False =>
            return Left.Line < Right.Line
              or else
                (Left.Line = Right.Line
                 and then Left.Line_Offset <
                   String_Index_Type (Right.Column));
      end case;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="
     (Left : Text_Location; Right : Source_Location) return Boolean is
   begin
      case Left.Absolute_Offset is
         when True =>
            return Left.Offset <= String_Index_Type (Right.Index);
         when False =>
            return Left.Line <= Right.Line
              or else
                (Left.Line = Right.Line
                 and then Left.Line_Offset <=
                   String_Index_Type (Right.Column));
      end case;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">"
     (Left : Text_Location; Right : Source_Location) return Boolean is
   begin
      case Left.Absolute_Offset is
         when True =>
            return Left.Offset > String_Index_Type (Right.Index);
         when False =>
            return Left.Line > Right.Line
              or else
                (Left.Line = Right.Line
                 and then Left.Line_Offset >
                   String_Index_Type (Right.Column));
      end case;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="
     (Left : Text_Location; Right : Source_Location) return Boolean is
   begin
      case Left.Absolute_Offset is
         when True =>
            return Left.Offset >= String_Index_Type (Right.Index);
         when False =>
            return Left.Line >= Right.Line
              or else
                (Left.Line = Right.Line
                 and then Left.Line_Offset >=
                   String_Index_Type (Right.Column));
      end case;
   end ">=";

   -----------------
   -- To_Location --
   -----------------

   function To_Location (Offset : String_Index_Type) return Text_Location is
   begin
      return (True, Offset);
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location
     (Line        : Natural;
      Line_Offset : String_Index_Type)
      return Text_Location
   is
   begin
      return (False, Line, Line_Offset);
   end To_Location;

   -----------------
   -- To_Location --
   -----------------

   function To_Location (Loc : Source_Location) return Text_Location
   is
   begin
      return (False, Loc.Line, String_Index_Type (Loc.Column));
   end To_Location;

   ---------------------
   -- Get_Iterator_At --
   ---------------------

   function Get_Iterator_At
     (Tree              : Construct_Tree;
      Location          : Text_Location;
      From_Type         : Position_Type := Start_Construct;
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array := Null_Category_Array)
      return Construct_Tree_Iterator
   is
      function Match_Category (Cat : Language_Category) return Boolean;
      --  Return true if the category given in parameter is the one we expect

      function Is_After
        (Construct : Simple_Construct_Information) return Boolean;
      --  Return true if the position is strictly after the expected position

      function Is_On_Or_After
        (Construct : Simple_Construct_Information) return Boolean;
      --  Return true is the construct given in parameter is on or after the
      --  expected position.

      function Is_On (Construct : Simple_Construct_Information) return Boolean;
      --  Return true is the construct is on the specified position

      --------------------
      -- Match_Category --
      --------------------

      function Match_Category (Cat : Language_Category) return Boolean is
      begin
         if Categories_Seeked'Length = 0 then
            return True;
         else
            for J in Categories_Seeked'Range loop
               if Categories_Seeked (J) = Cat then
                  return True;
               end if;
            end loop;

            return False;
         end if;
      end Match_Category;

      --------------
      -- Is_After --
      --------------

      function Is_After
        (Construct : Simple_Construct_Information) return Boolean is
      begin
         if From_Type = Start_Construct then
            return Location < Construct.Sloc_Start;
         elsif From_Type = Start_Name then
            return Location < Construct.Sloc_Entity;
         else
            raise Constraint_Error;
         end if;
      end Is_After;

      --------------------
      -- Is_On_Or_After --
      --------------------

      function Is_On_Or_After
        (Construct : Simple_Construct_Information) return Boolean
      is
      begin
         if From_Type = Start_Construct then
            return Location <= Construct.Sloc_Start;
         elsif From_Type = Start_Name then
            return Location <= Construct.Sloc_Entity;
         else
            raise Constraint_Error;
         end if;
      end Is_On_Or_After;

      -----------
      -- Is_On --
      -----------

      function Is_On
        (Construct : Simple_Construct_Information) return Boolean is
      begin
         if From_Type = Start_Construct then
            return Location = Construct.Sloc_Start;
         elsif From_Type = Start_Name then
            return Location = Construct.Sloc_Entity;
         else
            raise Constraint_Error;
         end if;
      end Is_On;

      Last_Matched : Construct_Tree_Iterator :=
        Null_Construct_Tree_Iterator;

   begin
      if Tree = null or else Tree.Contents'Length = 0 then
         return Null_Construct_Tree_Iterator;
      end if;

      case Position is
         when Before =>
            if Match_Category (Tree.Contents (1).Construct.Category) then
               Last_Matched := (Tree.Contents (1)'Access, 1);
            end if;

            for J in 2 .. Tree.Contents'Last loop
               if Is_After (Tree.Contents (J).Construct) then
                  return Last_Matched;
               end if;

               if Match_Category (Tree.Contents (J).Construct.Category) then
                  Last_Matched := (Tree.Contents (J)'Access, J);
               end if;
            end loop;

            return Last_Matched;

         when After =>
            for J in 1 .. Tree.Contents'Last loop
               if Is_On_Or_After (Tree.Contents (J).Construct)
                 and then Match_Category (Tree.Contents (J).Construct.Category)
               then
                  return (Tree.Contents (J)'Access, J);
               end if;
            end loop;

         when Specified =>
            declare
               It : Construct_Tree_Iterator := First (Tree);
            begin
               while It /= Null_Construct_Tree_Iterator loop
                  if Is_On (It.Node.Construct)
                    and then Match_Category (It.Node.Construct.Category)
                  then
                     return It;
                  end if;

                  if Location < It.Node.Construct.Sloc_Entity then
                     return Null_Construct_Tree_Iterator;
                  elsif Location >= It.Node.Construct.Sloc_Start then
                     It := Next (Tree, It, Jump_Into);
                  else
                     It := Next (Tree, It, Jump_Over);
                  end if;
               end loop;
            end;

         when Enclosing =>
            declare
               It : Construct_Tree_Iterator := First (Tree);
            begin
               while It /= Null_Construct_Tree_Iterator
                 and then Location >= It.Node.Construct.Sloc_Start
               loop
                  if Location <= It.Node.Construct.Sloc_End then
                     if Match_Category (It.Node.Construct.Category) then
                        Last_Matched := It;
                     end if;

                     It := Next (Tree, It, Jump_Into);
                  else
                     It := Next (Tree, It, Jump_Over);
                  end if;
               end loop;
               return Last_Matched;
            end;
      end case;

      return Null_Construct_Tree_Iterator;
   end Get_Iterator_At;

   ----------
   -- Next --
   ----------

   function Next
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator
   is
      Next_Index : Positive;
   begin
      if Tree /= Null_Construct_Tree then
         if Scope_Policy = Jump_Into then
            Next_Index := Iter.Index + 1;
         else
            Next_Index := Iter.Index + Iter.Node.Sub_Nodes_Length + 1;
         end if;

         if Next_Index > Tree.Contents'Last then
            return Null_Construct_Tree_Iterator;
         else
            return (Tree.Contents (Next_Index)'Access, Next_Index);
         end if;
      else
         return Null_Construct_Tree_Iterator;
      end if;
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator
   is
      Next_Index : Natural;
   begin
      if Scope_Policy = Jump_Into then
         Next_Index := Iter.Index - 1;
      else
         if Iter.Node.Previous_Sibling_Index /= 0 then
            Next_Index := Iter.Node.Previous_Sibling_Index;
         else
            Next_Index := Iter.Index - 1;
         end if;
      end if;

      if Next_Index = 0 then
         return Null_Construct_Tree_Iterator;
      else
         return (Tree.Contents (Next_Index)'Access, Next_Index);
      end if;
   end Prev;

   ------------------
   -- Has_Children --
   ------------------

   function Has_Children (Iter : Construct_Tree_Iterator) return Boolean is
   begin
      return Iter.Node.Sub_Nodes_Length > 0;
   end Has_Children;

   --------------------
   -- Get_Last_Child --
   --------------------

   function Get_Last_Child
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator
   is
      Last_Index : constant Natural := Iter.Index + Iter.Node.Sub_Nodes_Length;
      It : Construct_Tree_Iterator :=
        (Tree.Contents (Last_Index)'Access, Last_Index);
   begin
      while It /= Iter and then not Is_Parent_Scope (Iter, It) loop
         It := Prev (Tree, It, Jump_Over);
      end loop;

      return It;
   end Get_Last_Child;

   --------------------
   -- Is_Same_Entity --
   --------------------

   function Is_Same_Entity
     (Tree : Construct_Tree; Iter1, Iter2 : Construct_Tree_Iterator)
      return Boolean
   is
   begin
      if Iter1 = Null_Construct_Tree_Iterator then
         return Iter2 = Null_Construct_Tree_Iterator;
      end if;

      if Iter2 = Null_Construct_Tree_Iterator then
         return False;
      end if;

      if Iter1.Node.Construct.Name = Iter2.Node.Construct.Name
        and then Iter1.Node.Construct.Category = Iter2.Node.Construct.Category
      then
         if Iter1.Node.Parent_Index = 0 then
            return Iter2.Node.Parent_Index = 0;
         elsif Iter2.Node.Parent_Index = 0 then
            return False;
         else
            return Is_Same_Entity
              (Tree,
               (Tree.Contents
                  (Iter1.Node.Parent_Index)'Access,
                Iter1.Node.Parent_Index),
               (Tree.Contents
                  (Iter2.Node.Parent_Index)'Access,
                Iter2.Node.Parent_Index));
         end if;
      else
         return False;
      end if;
   end Is_Same_Entity;

   --------------
   -- Encloses --
   --------------

   function Encloses
     (Tree : Construct_Tree; Scope, Iter : Construct_Tree_Iterator)
      return Boolean
   is
   begin
      if Iter.Node.Parent_Index = 0 then
         return False;
      elsif Is_Same_Entity
        (Tree,
         (Tree.Contents
            (Iter.Node.Parent_Index)'Access, Iter.Node.Parent_Index),
         Scope)
      then
         return True;
      else
         return Encloses
           (Tree,
            Scope,
            (Tree.Contents (Iter.Node.Parent_Index)'Access,
             Iter.Node.Parent_Index));
      end if;
   end Encloses;

   --------------
   -- Encloses --
   --------------

   function Encloses
     (Scope             : Construct_Tree_Iterator;
      Line, Line_Offset : Positive)
      return Boolean
   is
   begin
      if Line < Get_Construct (Scope).Sloc_Start.Line
        or else
          (Line = Get_Construct (Scope).Sloc_Start.Line
           and then Line_Offset < Get_Construct (Scope).Sloc_Start.Column)
      then
         return False;
      end if;

      if Line > Get_Construct (Scope).Sloc_End.Line
        or else
          (Line = Get_Construct (Scope).Sloc_End.Line
           and then Line_Offset > Get_Construct (Scope).Sloc_End.Column)
      then
         return False;
      end if;

      return True;
   end Encloses;

   --------------
   -- Encloses --
   --------------

   function Encloses
     (Scope  : Construct_Tree_Iterator;
      Offset : String_Index_Type)
      return Boolean
   is
   begin
      return Natural (Offset) >= Get_Construct (Scope).Sloc_Start.Index
        and then Natural (Offset) <= Get_Construct (Scope).Sloc_End.Index;
   end Encloses;

   -------------------
   -- Get_Full_Name --
   -------------------

   --  ??? This is language dependent, to be either moved into a language
   --  dependent package or made language indepenend
   function Get_Full_Name
     (Tree : Construct_Tree; It : Construct_Tree_Iterator)
      return String
   is
      Length  : Integer;
      Current : Construct_Tree_Iterator := Get_Parent_Scope (Tree, It);
   begin
      if Get_Construct (It).Name = No_Symbol then
         return "";
      end if;

      Length := Get (Get_Construct (It).Name)'Length;

      while Current /= Null_Construct_Tree_Iterator
        and then Get_Construct (Current).Category = Cat_Package
      loop
         Length := Length + 1 + Get (Get_Construct (Current).Name)'Length;
         Current := Get_Parent_Scope (Tree, Current);
      end loop;

      declare
         Name  : String (1 .. Length);
         Index : Natural := Length;
      begin
         Name (Index - Get (Get_Construct (It).Name)'Length + 1 .. Index) :=
           Get (Get_Construct (It).Name).all;

         Index := Index - Get (Get_Construct (It).Name)'Length;
         Current := Get_Parent_Scope (Tree, It);

         while Current /= Null_Construct_Tree_Iterator
           and then Get_Construct (Current).Category = Cat_Package
         loop
            Name (Index - Get (Get_Construct (Current).Name)'Length .. Index)
              := Get (Get_Construct (Current).Name).all & ".";

            Index := Index - 1 - Get (Get_Construct (Current).Name)'Length;
            Current := Get_Parent_Scope (Tree, Current);
         end loop;

         return Name;
      end;
   end Get_Full_Name;

   ------------------------------
   -- Get_Annotation_Container --
   ------------------------------

   function Get_Annotation_Container
     (Tree : Construct_Tree; It : Construct_Tree_Iterator)
      return access Construct_Annotations_Pckg.Annotation_Container
   is
   begin
      --  Using the tree is needed here, since we can't rely on the copy of
      --  the node contained in It.
      return Tree.Contents (It.Index).Annotations'Access;
   end Get_Annotation_Container;

   --------------------
   -- Has_Same_Scope --
   --------------------

   function Has_Same_Scope
     (Left, Right : Construct_Tree_Iterator) return Boolean is
   begin
      return Left.Node.Parent_Index = Right.Node.Parent_Index;
   end Has_Same_Scope;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (It : Construct_Tree_Iterator) return Integer is
   begin
      return It.Index;
   end Get_Index;

   ----------------------
   -- Get_Parent_Index --
   ----------------------

   function Get_Parent_Index (It : Construct_Tree_Iterator) return Integer is
   begin
      return It.Node.Parent_Index;
   end Get_Parent_Index;

   ---------------
   -- To_String --
   ---------------

   function To_String (It : Construct_Tree_Iterator) return String is
      Name    : Symbol;
   begin
      Name := It.Node.Construct.Name;

      return Get (Name).all & "(" & It.Node.Construct.Category'Img & ")"
        & " @" & It.Index'Img
        & "[" & It.Node.Construct.Sloc_Start.Line'Img
        & "," & It.Node.Construct.Sloc_Start.Column'Img & "]";
   end To_String;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left, Right : Construct_Tree_Iterator) return Boolean is
   begin
      --  This function is supposed to be called a lot, so it has to be as
      --  efficient as possible. That's why we overload it, in order to test
      --  only the indexes, and avoid comparison on the actual constructs,
      --  which is useless if the precondition (both iterators are coming from
      --  the same tree) is correct.

      return Left.Index = Right.Index;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Construct_Tree_Iterator) return Boolean is
   begin
      return Left.Index < Right.Index;
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Construct_Tree_Iterator) return Boolean is
   begin
      return Left.Index > Right.Index;
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Construct_Tree_Iterator) return Boolean is
   begin
      return Left.Index <= Right.Index;
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Construct_Tree_Iterator) return Boolean is
   begin
      return Left.Index >= Right.Index;
   end ">=";

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Composite_Identifier_Access) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Composite_Identifier, Composite_Identifier_Access);
   begin
      Internal_Free (This);
   end Free;

   ------------
   -- Length --
   ------------

   function Length (Id : Composite_Identifier) return Natural is
   begin
      return Id.Number_Of_Elements;
   end Length;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (Id : Composite_Identifier; Number : Natural)
     return String is
   begin
      if Number = 0
        or else Id = Null_Composite_Identifier
      then
         return "";
      else
         return Id.Identifier
           (Id.Position_Start (Number) .. Id.Position_End (Number));
      end if;
   end Get_Item;

   -------------
   -- Prepend --
   -------------

   function Prepend
     (Id         : Composite_Identifier;
      Word_Begin : Natural;
      Word_End   : Natural)
      return Composite_Identifier
   is
      Result : Composite_Identifier
        (Id.String_Length, Id.Number_Of_Elements + 1);
   begin
      Result.Identifier := Id.Identifier;
      Result.Position_Start (1) := Word_Begin;
      Result.Position_End (1) := Word_End;
      Result.Position_Start (2 .. Result.Position_Start'Last)
        := Id.Position_Start;
      Result.Position_End (2 .. Result.Position_End'Last) := Id.Position_End;

      return Result;
   end Prepend;

   -----------------------------
   -- To_Composite_Identifier --
   -----------------------------

   function To_Composite_Identifier (Identifier : String)
     return Composite_Identifier
   is
      Number_Of_Parts : Natural := 0;
      Number_Of_Chars : Natural := 0;
      Index_In_Id     : Integer := Identifier'First;

      Word_Begin, Word_End : Natural;

      Tmp : Composite_Identifier (Identifier'Length, Identifier'Length);

   begin
      if Identifier = "" then
         return Null_Composite_Identifier;
      end if;

      --  Compute the size of the result

      loop
         Skip_Blanks (Identifier, Index_In_Id);

         Word_Begin := Index_In_Id;
         Word_End   := Word_Begin;

         if Identifier (Index_In_Id) = '"' then
            Word_End := Word_End + 1;
            Skip_To_Char (Identifier, Word_End, '"');
            Word_End := Word_End + 1;

            --  If we can't find a matching ", then the identifier is ". This
            --  is the mark of an incomplete name.
            if Word_End > Identifier'Last + 1 then
               Word_End := Identifier'Last + 1;
            end if;
         else
            Skip_Word (Identifier, Word_End);
         end if;

         Index_In_Id := Word_End;
         Word_End := Word_End - 1;

         Tmp.Identifier
           (Number_Of_Chars + 1 ..
              Number_Of_Chars + 1 + Word_End - Word_Begin) :=
             Identifier (Word_Begin .. Word_End);
         Tmp.Position_Start (Number_Of_Parts + 1) := Number_Of_Chars + 1;
         Tmp.Position_End (Number_Of_Parts + 1) :=
           Number_Of_Chars + 1 + Word_End - Word_Begin;

         Number_Of_Parts := Number_Of_Parts + 1;
         Number_Of_Chars := Number_Of_Chars + Word_End - Word_Begin + 1;

         Skip_Blanks (Identifier, Index_In_Id);

         if Index_In_Id > Identifier'Last
           or else Identifier (Index_In_Id) /= '.'
         then
            exit;
         end if;

         if Index_In_Id < Identifier'Last then
            Index_In_Id := Index_In_Id + 1;
         else
            exit;
         end if;

         Tmp.Identifier (Number_Of_Chars + 1) := '.';
         Number_Of_Chars := Number_Of_Chars + 1;
      end loop;

      --  Then, do the same iteration a second time with the actual result

      declare
         Result : Composite_Identifier (Number_Of_Chars, Number_Of_Parts);
      begin
         Result.Identifier := Tmp.Identifier (1 .. Number_Of_Chars);
         Result.Position_Start := Tmp.Position_Start (1 .. Number_Of_Parts);
         Result.Position_End := Tmp.Position_End (1 .. Number_Of_Parts);

         return Result;
      end;
   end To_Composite_Identifier;

   ---------------
   -- To_String --
   ---------------

   function To_String (Identifier : Composite_Identifier) return String is
      Buffer     : String (1 .. Identifier.Identifier'Length);
      Buffer_Ind : Natural := 0;
   begin
      for J in 1 .. Length (Identifier) loop
         declare
            Part : constant String := Get_Item (Identifier, J);
         begin
            if J > 1 then
               Buffer (Buffer_Ind + 1) := '.';
               Buffer_Ind := Buffer_Ind + 1;
            end if;

            Buffer (Buffer_Ind + 1 .. Buffer_Ind + Part'Length) := Part;
            Buffer_Ind := Buffer_Ind + Part'Length;
         end;
      end loop;

      return Buffer (1 .. Buffer_Ind);
   end To_String;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Identifier : Composite_Identifier; From : Natural; To : Natural)
      return Composite_Identifier
   is
      Result : Composite_Identifier
        (Identifier.Position_End (To) - Identifier.Position_Start (From) + 1,
         To - From + 1);
   begin
      for J in From .. To loop
         Result.Position_Start (J - From + 1) :=
           Identifier.Position_Start (J)
           - Identifier.Position_Start (From) + 1;
         Result.Position_End (J - From + 1) :=
           Identifier.Position_End (J)
           - Identifier.Position_Start (From) + 1;
      end loop;

      Result.Identifier := Identifier.Identifier
        (Identifier.Position_Start (From) .. Identifier.Position_End (To));

      return Result;
   end Get_Slice;

   ------------------
   -- Is_Prefix_Of --
   ------------------

   function Is_Prefix_Of
     (Potential_Prefix, Full_Id : Composite_Identifier;
      Case_Sensitive            : Boolean)
     return Boolean
   is
   begin
      if Length (Full_Id) /= Length (Potential_Prefix) + 1 then
         return False;
      end if;

      for J in 1 .. Length (Potential_Prefix) loop
         if not Equal
           (Get_Item (Potential_Prefix, J),
            Get_Item (Full_Id, J),
            Case_Sensitive)
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_Prefix_Of;

   -----------
   -- Equal --
   -----------

   function Equal
     (Left, Right : Composite_Identifier; Case_Sensitive : Boolean)
      return Boolean
   is
   begin
      if Left.Number_Of_Elements /= Right.Number_Of_Elements then
         return False;
      end if;

      for J in 1 .. Length (Left) loop
         if not Equal
           (Get_Item (Left, J), Get_Item (Right, J), Case_Sensitive)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equal;

   -------------------------
   -- Full_Construct_Path --
   -------------------------

   function Full_Construct_Path
     (Tree         : Construct_Tree;
      Construct_It : Construct_Tree_Iterator)
      return Construct_Tree_Iterator_Array
   is
      It   : Construct_Tree_Iterator;
      Size : Integer := 0;
   begin
      if Construct_It.Index = 0 then
         return Null_Construct_Tree_Iterator_Array;
      end if;

      It := Construct_It;

      while It /= Null_Construct_Tree_Iterator loop
         It := Get_Parent_Scope (Tree, It);

         Size := Size + 1;
      end loop;

      declare
         Result : Construct_Tree_Iterator_Array (1 .. Size);
         Ind    : Integer := Result'Length;
      begin
         It := Construct_It;

         while It /= Null_Construct_Tree_Iterator loop
            Result (Ind) := It;

            It := Get_Parent_Scope (Tree, It);

            Ind := Ind - 1;
         end loop;

         return Result;
      end;
   end Full_Construct_Path;

   function Full_Construct_Path
     (Tree   : Construct_Tree;
      Offset : String_Index_Type) return Construct_Tree_Iterator_Array
   is
      It : constant Construct_Tree_Iterator := Get_Iterator_At
        (Tree              => Tree,
         Location          => To_Location (Offset),
         From_Type         => Start_Construct,
         Position          => Enclosing,
         Categories_Seeked => Null_Category_Array);
   begin
      if It /= Null_Construct_Tree_Iterator then
         return Full_Construct_Path (Tree, It);
      else
         return Null_Construct_Tree_Iterator_Array;
      end if;
   end Full_Construct_Path;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left, Right : Referenced_Identifiers_List) return Boolean
   is
      Left_Node  : Referenced_Identifiers_List := Left;
      Right_Node : Referenced_Identifiers_List := Right;
   begin
      while Left_Node.Contents /= null
        and then Right_Node.Contents /= null
      loop
         if Left_Node.Contents.Element /= Right_Node.Contents.Element then
            return False;
         end if;

         Left_Node := Left_Node.Contents.Next;
         Right_Node := Right_Node.Contents.Next;
      end loop;

      return Left_Node.Contents = null and then Right_Node.Contents = null;
   end "=";

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (It : Construct_Tree_Iterator) return Normalized_Symbol is
   begin
      return It.Node.Id;
   end Get_Identifier;

   --------------------------------
   -- Get_Referenced_Identifiers --
   --------------------------------

   function Get_Referenced_Identifiers
     (It : Construct_Tree_Iterator) return Referenced_Identifiers_List is
   begin
      return It.Node.Referenced_Ids;
   end Get_Referenced_Identifiers;

   -------------------------------------
   -- Get_Next_Referenced_Identifiers --
   -------------------------------------

   function Get_Next_Referenced_Identifiers
     (Ref : Referenced_Identifiers_List) return Referenced_Identifiers_List is
   begin
      return Ref.Contents.Next;
   end Get_Next_Referenced_Identifiers;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (Ref : Referenced_Identifiers_List) return Normalized_Symbol is
   begin
      if Ref.Contents /= null then
         return Ref.Contents.Element;
      else
         return No_Normalized_Symbol;
      end if;
   end Get_Identifier;

   ------------------------------------
   -- Analyze_Constructs_Identifiers --
   ------------------------------------

   procedure Analyze_Constructs_Identifiers
     (Lang : access Language_Root'Class;
      Tree : Construct_Tree) is
   begin
      for J in Tree.Contents'Range loop
         --  ??? We should store the normalized name in the construct tree
         Tree.Contents (J).Id := Find_Normalized
           (Lang.Symbols, Get (Tree.Contents (J).Construct.Name).all);
      end loop;
   end Analyze_Constructs_Identifiers;

   ---------------------------------
   -- Analyze_Referenced_Identifiers --
   ---------------------------------

   procedure Analyze_Referenced_Identifiers
     (Buffer  : String;
      Lang    : access Language_Root'Class;
      Tree    : Construct_Tree)
   is
      Sloc_Start, Sloc_End : Source_Location;
      Index                : Natural;
      Success              : Boolean;
      Current_Node         : Referenced_Identifiers_List;
   begin
      for J in Tree.Contents'Range loop
         Index := 0;

         loop
            Get_Referenced_Entity
              (Lang       => Lang,
               Buffer     => Buffer,
               Construct  => Tree.Contents (J).Construct,
               Sloc_Start => Sloc_Start,
               Sloc_End   => Sloc_End,
               Success    => Success,
               From_Index => Index);

            exit when not Success;

            if Index = 0 then
               Tree.Contents (J).Referenced_Ids.Contents :=
                 new Referenced_Identifiers_List_Record;
               Tree.Contents (J).Referenced_Ids.Contents.Element :=
                 Find_Normalized
                   (Lang.Symbols, Buffer (Sloc_Start.Index .. Sloc_End.Index));
               Current_Node := Tree.Contents (J).Referenced_Ids;
            else
               Current_Node.Contents.Next.Contents :=
                 new Referenced_Identifiers_List_Record;
               Current_Node.Contents.Next.Contents.Element :=
                 Find_Normalized
                   (Lang.Symbols, Buffer (Sloc_Start.Index .. Sloc_End.Index));
               Current_Node := Current_Node.Contents.Next;
            end if;

            Index := Sloc_End.Index + 1;
         end loop;
      end loop;
   end Analyze_Referenced_Identifiers;

   -----------
   -- Match --
   -----------

   function Match
     (Seeked_Name, Tested_Name : Normalized_Symbol;
      Seeked_Is_Partial : Boolean)
      return Boolean
   is
      Tested : Cst_String_Access;
   begin
      if not Seeked_Is_Partial then
         --  If the two names have to be strictly equals, then we'll just
         --  compare the value of their pointers, since these pointers are
         --  coming from the trie and each string has a unique corresponding
         --  pointer.

         return Seeked_Name = Tested_Name;
      else
         --  Otherwise, we'll compare their contents

         Tested := Get (Tested_Name);

         if Tested'Length = Get (Seeked_Name)'Length then
            return Seeked_Name = Tested_Name;
         elsif Tested'Length > Get (Seeked_Name)'Length then
            return Tested (Tested'First .. Tested'First
               + Get (Seeked_Name)'Length - 1) = Get (Seeked_Name).all;
         else
            return False;
         end if;
      end if;
   end Match;

   ---------------------
   -- Find_Normalized --
   ---------------------

   function Find_Normalized
     (Symbols : not null access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Name    : String) return Normalized_Symbol is
   begin
      return Normalized_Symbol (Symbols.Find (To_Lower (Name)));
   end Find_Normalized;

end Language.Tree;
