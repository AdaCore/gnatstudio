-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                           --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Basic_Types;       use Basic_Types;
with String_Utils;      use String_Utils;
with Generic_List;

with Ada.Unchecked_Deallocation;

package body Language.Tree is

   --------------
   -- Contains --
   --------------

   function Contains (Scope, Item : Construct_Access) return Boolean is
   begin
      return Scope.Sloc_Start.Index <= Item.Sloc_Start.Index
        and then Scope.Sloc_End.Index >= Item.Sloc_End.Index;
   end Contains;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Construct_Tree_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Construct_Tree, Construct_Tree_Access);
   begin
      Internal (Tree);
   end Free;

   -----------------------
   -- To_Construct_Tree --
   -----------------------

   function To_Construct_Tree
     (List : Construct_List; Compute_Scopes : Boolean := True)
      return Construct_Tree
   is
      Size              : Natural := 0;
      Current_Construct : Construct_Access;

      procedure Compute_Scope
        (Tree       : in out Construct_Tree;
         Base_Iter  : Construct_Tree_Iterator;
         Base_Scope : Construct_Tree_Iterator := Null_Construct_Tree_Iterator);
      --  Set the spec, public body and private body info for this iterator.
      --  If Base_Scope is null, then the search will start at the entity after
      --  Base_Iter, otherwise it will start at the first child of Base_Sope.

      -------------------
      -- Compute_Scope --
      -------------------

      procedure Compute_Scope
        (Tree       : in out Construct_Tree;
         Base_Iter  : Construct_Tree_Iterator;
         Base_Scope : Construct_Tree_Iterator := Null_Construct_Tree_Iterator)
      is
         Local_Iter  : Construct_Tree_Iterator;
         Local_Scope : Construct_Tree_Iterator;

         Spec_Index         : Natural := 0;
         First_Body_Index   : Natural := 0;
         Second_Body_Index  : Natural := 0;
      begin
         if Base_Scope = Null_Construct_Tree_Iterator then
            Local_Iter := Next (Tree, Base_Iter, Jump_Over);
            Local_Scope := Get_Parent_Scope (Tree, Base_Iter);
         else
            Local_Iter := Next (Tree, Base_Scope, Jump_Into);
            Local_Scope := Base_Scope;
         end if;

         while Local_Iter /= Null_Construct_Tree_Iterator
           and then Get_Parent_Scope (Tree, Local_Iter) = Local_Scope
         loop
            if Get_Construct (Local_Iter).Name /= null
              and then Get_Construct (Local_Iter).Name.all
              = Get_Construct (Base_Iter).Name.all
              and then Get_Construct (Base_Iter).Category
              = Get_Construct (Local_Iter).Category
            then
               if Get_Construct (Base_Iter).Category
                 in Subprogram_Category
               then
                  null;
                  --  ??? We can't really do anything here as long as we don't
                  --  have any information on the parameters. We could get
                  --  the buffer (would need to add a parameter there) and
                  --  normalize the parameters list. Since this is not yet
                  --  handled by the auto-completion, we can just wait before
                  --  implementing this case.

                  return;
               end if;

               if Tree (Base_Iter.Index).Spec_Index = 0 then
                  if Get_Construct (Base_Iter).Sloc_Start.Index <
                    Get_Construct (Local_Iter).Sloc_Start.Index
                  then
                     Spec_Index := Base_Iter.Index;
                     First_Body_Index := Local_Iter.Index;
                  else
                     Spec_Index := Local_Iter.Index;
                     First_Body_Index := Base_Iter.Index;
                  end if;
               else
                  if Get_Construct (Base_Iter).Sloc_Start.Index <
                    Get_Construct (Local_Iter).Sloc_Start.Index
                  then
                     Spec_Index := Base_Iter.Index;
                     First_Body_Index := Tree (Base_Iter.Index)
                       .First_Body_Index;
                     Second_Body_Index := Local_Iter.Index;
                  else
                     Spec_Index := Local_Iter.Index;
                     First_Body_Index := Base_Iter.Index;
                     Second_Body_Index := Tree (Base_Iter.Index)
                       .First_Body_Index;
                  end if;
               end if;

               Tree (Spec_Index).Spec_Index := Spec_Index;
               Tree (Spec_Index).First_Body_Index := First_Body_Index;
               Tree (Spec_Index).Second_Body_Index := Second_Body_Index;

               Tree (First_Body_Index).Spec_Index := Spec_Index;
               Tree (First_Body_Index).First_Body_Index := First_Body_Index;
               Tree (First_Body_Index).Second_Body_Index := Second_Body_Index;

               if Second_Body_Index /= 0 then
                  Tree (Second_Body_Index).Spec_Index := Spec_Index;
                  Tree (Second_Body_Index).First_Body_Index :=
                    First_Body_Index;
                  Tree (Second_Body_Index).Second_Body_Index :=
                    Second_Body_Index;
               end if;

               exit;
            end if;

            Local_Iter := Next (Tree, Local_Iter, Jump_Over);
         end loop;

         --  Look in the other parts if any

         if Local_Scope.Node.Spec_Index /= 0
           and then Local_Scope.Index /= Local_Scope.Node.Spec_Index
         then
            Compute_Scope
              (Tree,
               Base_Iter,
               (Tree (Local_Scope.Node.Spec_Index),
                Local_Scope.Node.Spec_Index));
         end if;

      end Compute_Scope;

   begin
      Current_Construct := List.First;

      while Current_Construct /= null loop
         Size := Size + 1;
         Current_Construct := Current_Construct.Next;
      end loop;

      if Size = 0 then
         return Null_Construct_Tree;
      end if;

      declare
         Tree       : Construct_Tree (1 .. Size);
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

               if Previous_Index in Tree'Range then
                  --  This is false when we are on the root node
                  Tree (Previous_Index).Previous_Sibling_Index := Tree_Index;
               end if;
            end loop;

            Tree_Index := Tree_Index - 1;
            Tree (Tree_Index).Construct := Parent;
            Tree (Tree_Index).Sub_Nodes_Length := Start_Index - Tree_Index - 1;

            for J in Tree_Index + 1
              .. Tree_Index + Tree (Tree_Index).Sub_Nodes_Length
            loop
               if Tree (J).Parent_Index = 0 then
                  Tree (J).Parent_Index := Tree_Index;
               end if;
            end loop;
         end Analyze_Construct;

      begin
         Current_Construct := List.Last;

         while Current_Construct /= null loop
            Analyze_Construct;
         end loop;

         if Compute_Scopes then
            declare
               Iter : Construct_Tree_Iterator;
            begin
               Iter := First (Tree);

               while Iter /= Null_Construct_Tree_Iterator loop
                  --  ??? Re-looking at the whole structure here is highly
                  --  innefficient. We should look for this during the initial
                  --  construction of the tree. Besides, the function below is
                  --  a bit Ada-specific. See if there is a way to make it
                  --  more language-independant (maybe by introducing a
                  --  language privitive between two constructs saying if they
                  --  are two part of the same entity or not).
                  Compute_Scope (Tree, Iter);

                  Iter := Next (Tree, Iter, Jump_Into);
               end loop;
            end;
         end if;

         return Tree;
      end;
   end To_Construct_Tree;

   -----------
   -- First --
   -----------

   function First (Tree : Construct_Tree) return Construct_Tree_Iterator is
   begin
      return (Tree (1), 1);
   end First;

   ----------------------
   -- Get_Parent_Scope --
   ----------------------

   function Get_Parent_Scope
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
     return Construct_Tree_Iterator
   is
   begin
      if Iter.Node.Parent_Index /= 0 then
         return (Tree (Iter.Node.Parent_Index), Iter.Node.Parent_Index);
      else
         return Null_Construct_Tree_Iterator;
      end if;
   end Get_Parent_Scope;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct (Iter : Construct_Tree_Iterator)
      return Construct_Access
   is
   begin
      return Iter.Node.Construct;
   end Get_Construct;

   ----------------------
   -- Get_Child_Number --
   ----------------------

   function Get_Child_Number (Iter : Construct_Tree_Iterator) return Natural is
   begin
      return Iter.Node.Sub_Nodes_Length;
   end Get_Child_Number;

   ---------------------
   -- Get_Iterator_At --
   ---------------------

   function Get_Iterator_At
     (Tree : Construct_Tree; Line, Line_Offset : Natural)
      return Construct_Tree_Iterator
   is
   begin
      for J in 2 .. Tree'Last loop
         if Tree (J).Construct.Sloc_Start.Line > Line
           and then Tree (J).Construct.Sloc_Start.Line > Line_Offset
         then
            return (Tree (J - 1), J - 1);
         end if;
      end loop;

      return (Tree (Tree'Last), Tree'Last);
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
      if Iter.Node.Sub_Nodes_Length > 0
        and then Scope_Policy = Jump_Into
      then
         Next_Index := Iter.Index + 1;
      else
         Next_Index := Iter.Index + Iter.Node.Sub_Nodes_Length + 1;
      end if;

      if Next_Index > Tree'Last then
         return Null_Construct_Tree_Iterator;
      else
         return (Tree (Next_Index), Next_Index);
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
         return (Tree (Next_Index), Next_Index);
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
      It : Construct_Tree_Iterator := (Tree (Last_Index), Last_Index);
   begin
      while It /= Iter and then Get_Parent_Scope (Tree, It) /= Iter loop
         It := Prev (Tree, It, Jump_Over);
      end loop;

      return It;
   end Get_Last_Child;

   -------------------
   -- Is_Same_Scope --
   -------------------

   function Is_Same_Entity
     (Tree : Construct_Tree; Iter1, Iter2 : Construct_Tree_Iterator)
      return Boolean
   is
   begin
      if Iter1.Node.Construct.Name.all = Iter2.Node.Construct.Name.all
        and then Iter1.Node.Construct.Category = Iter2.Node.Construct.Category
      then
         if Iter1.Node.Parent_Index = 0
           and then Iter2.Node.Parent_Index = 0
         then
            return True;
         elsif Iter1.Node.Parent_Index = 0
           and then Iter2.Node.Parent_Index /= 0
         then
            return False;
         else
            return Is_Same_Entity
              (Tree,
               (Tree (Iter1.Node.Parent_Index), Iter1.Node.Parent_Index),
               (Tree (Iter2.Node.Parent_Index), Iter2.Node.Parent_Index));
         end if;
      else
         return False;
      end if;
   end Is_Same_Entity;

   ------------------------------
   -- Get_Last_Relevant_Entity --
   ------------------------------

   function Get_Last_Relevant_Construct
     (Tree : Construct_Tree; Offset : Positive)
     return Construct_Tree_Iterator
   is
      Last_Relevant_Construct : Construct_Tree_Iterator :=
        Null_Construct_Tree_Iterator;
      It                      : Construct_Tree_Iterator;
   begin

      for J in reverse 1 .. Tree'Last loop
         if Tree (J).Construct.Sloc_Start.Index <= Offset then
            Last_Relevant_Construct := (Tree (J), J);
            It := Last_Relevant_Construct;

            while It /= Null_Construct_Tree_Iterator loop
               --  If we found the enclosing construct, nothing more to get.

               if Get_Construct (It).Sloc_End.Index >= Offset then
                  exit;
               end if;

               --  If the iterator is not anymore on the same scope, we have
               --  jumped in an enclosing scope, and therefore the last
               --  construct found is in fact unreacheable. It is the actual
               --  one.

               if Get_Parent_Scope (Tree, It)
                 /= Get_Parent_Scope (Tree, Last_Relevant_Construct)
               then
                  Last_Relevant_Construct := It;
               end if;

               It := Prev (Tree, It, Jump_Over);
            end loop;

            exit;
         end if;
      end loop;

      return Last_Relevant_Construct;
   end Get_Last_Relevant_Construct;

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
         (Tree (Iter.Node.Parent_Index), Iter.Node.Parent_Index),
         Scope)
      then
         return True;
      else
         return Encloses
           (Tree,
            Scope,
            (Tree (Iter.Node.Parent_Index), Iter.Node.Parent_Index));
      end if;
   end Encloses;

   --------------
   -- Encloses --
   --------------

   function Encloses
     (Tree              : Construct_Tree;
      Scope             : Construct_Tree_Iterator;
      Line, Line_Offset : Positive)
      return Boolean
   is
      Last_Relevant_Entity : Natural := 0;
   begin
      --  Find the closest scope

      for J in 1 .. Tree'Last loop
         if (Tree (J).Construct.Sloc_Start.Line < Line
           or else
             (Tree (J).Construct.Sloc_Start.Line = Line
              and then Tree (J).Construct.Sloc_Start.Column < Line_Offset))
           and then
             (Tree (J).Construct.Sloc_End.Line > Line
              or else
                (Tree (J).Construct.Sloc_End.Line = Line
                   and then Tree (J).Construct.Sloc_End.Column > Line_Offset))
         then
            Last_Relevant_Entity := J;
         end if;
      end loop;

      if Last_Relevant_Entity = 0 then
         return Encloses
           (Tree, Scope, (Tree (Last_Relevant_Entity), Last_Relevant_Entity))
           or else Is_Same_Entity
             (Tree,
              Scope,
              (Tree (Last_Relevant_Entity), Last_Relevant_Entity));
      else
         return False;
      end if;
   end Encloses;

   --------------
   -- Encloses --
   --------------

   function Encloses
     (Tree   : Construct_Tree;
      Scope  : Construct_Tree_Iterator;
      Offset : Positive)
      return Boolean
   is
      Last_Relevant_Entity : constant Construct_Tree_Iterator :=
        Get_Last_Relevant_Construct (Tree, Offset);
   begin
      if Last_Relevant_Entity /= Null_Construct_Tree_Iterator then
         return Encloses
           (Tree, Scope, Last_Relevant_Entity)
           or else Is_Same_Entity
             (Tree,
              Scope,
              Last_Relevant_Entity);
      else
         return False;
      end if;
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
      if Get_Construct (It).Name = null then
         return "";
      end if;

      Length := Get_Construct (It).Name.all'Length;

      while Current /= Null_Construct_Tree_Iterator
        and then Get_Construct (Current).Category = Cat_Package
      loop
         Length := Length + 1 + Get_Construct (Current).Name.all'Length;
         Current := Get_Parent_Scope (Tree, Current);
      end loop;

      declare
         Name  : String (1 .. Length);
         Index : Positive := Name'Last;
      begin
         Name (Index - Get_Construct (It).Name.all'Length + 1 .. Index) :=
           Get_Construct (It).Name.all;

         Current := Get_Parent_Scope (Tree, It);

         while Current /= Null_Construct_Tree_Iterator
           and then Get_Construct (Current).Category = Cat_Package
         loop
            Name (Index - Get_Construct (Current).Name.all'Length .. Index)
              := Get_Construct (Current).Name.all & ".";

            Index := Index - 1 - Get_Construct (Current).Name.all'Length;
            Current := Get_Parent_Scope (Tree, Current);
         end loop;

         return Name;
      end;
   end Get_Full_Name;

   --------------
   -- Get_Spec --
   --------------

   function Get_Spec (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
     return Construct_Tree_Iterator
   is
   begin
      if Iter.Node.Spec_Index = 0 then
         return Iter;
      else
         return (Tree (Iter.Node.Spec_Index), Iter.Node.Spec_Index);
      end if;
   end Get_Spec;

   --------------------
   -- Get_First_Body --
   --------------------

   function Get_First_Body
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator
   is
   begin
      if Iter.Node.First_Body_Index = 0 then
         return Iter;
      else
         return
           (Tree (Iter.Node.First_Body_Index), Iter.Node.First_Body_Index);
      end if;
   end Get_First_Body;

   ---------------------
   -- Get_Second_Body --
   ---------------------

   function Get_Second_Body
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator
   is
   begin
      if Iter.Node.Second_Body_Index = 0 then
         return Iter;
      else
         return
           (Tree (Iter.Node.Second_Body_Index), Iter.Node.Second_Body_Index);
      end if;
   end Get_Second_Body;

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
      return Id.Identifier
        (Id.Position_Start (Number) .. Id.Position_End (Number));
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
      Result.Position_End (2 .. Result.Position_End'Last)
        := Id.Position_End;

      return Result;
   end Prepend;

   -----------------------------
   -- To_Composite_Identifier --
   -----------------------------

   function To_Composite_Identifier (Identifier : String)
     return Composite_Identifier
   is
      Index : Natural;

      function Internal_To_Composite_Identifier return Composite_Identifier;

      function Internal_To_Composite_Identifier return Composite_Identifier is
         Word_Begin, Word_End : Natural;
      begin
         Skip_Blanks (Identifier, Index);

         Word_Begin := Index;
         Word_End := Word_Begin;
         Skip_Word (Identifier, Word_End);

         Index := Word_End;
         Word_End := Word_End - 1;

         Skip_Blanks (Identifier, Index);

         if Index > Identifier'Last or else Identifier (Index) /= '.' then
            declare
               Id : Composite_Identifier (Word_End - Identifier'First + 1, 1);
            begin
               Id.Identifier := Identifier (Identifier'First .. Word_End);
               Id.Position_Start (1) := Word_Begin - Identifier'First + 1;
               Id.Position_End (1) := Word_End - Identifier'First + 1;

               return Id;
            end;
         end if;

         if Index < Identifier'Last then
            Index := Index + 1;
         end if;

         return Prepend
           (Internal_To_Composite_Identifier,
            Word_Begin - Identifier'First + 1,
            Word_End - Identifier'First + 1);
      end Internal_To_Composite_Identifier;

   begin
      Index := Identifier'First;

      return Internal_To_Composite_Identifier;
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
           Identifier.Position_End (From)
           - Identifier.Position_Start (From) + 1;
      end loop;

      Result.Identifier := Identifier.Identifier
        (Identifier.Position_Start (From) .. Identifier.Position_End (To));

      return Result;
   end Get_Slice;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      Offset     : Natural;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False) return Construct_Tree_Iterator_Array
   is
   begin
      return
        Get_Visible_Constructs
          (Tree,
           Get_Last_Relevant_Construct (Tree, Offset),
           Name,
           Use_Wise,
           Is_Partial);
   end Get_Visible_Constructs;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   --   ??? This function is actually very language dependent. Consider either
   --  make it more general or put it into languages-dependant types.
   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      From       : Construct_Tree_Iterator;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False) return Construct_Tree_Iterator_Array
   is
      procedure Free (This : in out Construct_Tree_Iterator);

      package Construct_Iterator_List_Pckg is new Generic_List
        (Construct_Tree_Iterator);

      use Construct_Iterator_List_Pckg;

      Constructs_Found   : Construct_Iterator_List_Pckg.List;

      Seek_Iterator      : Construct_Tree_Iterator;
      Prev_Iterator      : Construct_Tree_Iterator;
      Initial_Parent     : Construct_Tree_Iterator;

      type Use_Record is record
         Name    : String_Access;
         --  ??? Would it save computation to store a Composite_Identifier
         --  here ?

         Removed : Boolean;
      end record;

      procedure Free (This : in out Use_Record);
      --  Free the memory associated to the Use_Record given in parameter.

      package Use_List_Pckg is new Generic_List (Use_Record);

      Use_List           : Use_List_Pckg.List;

      use Use_List_Pckg;

      function Name_Match (Name_Tested : String) return Boolean;
      --  Return true if the name given in parameter matches the expected one.
      --  This takes into account the value of Is_Partial.

      procedure Look_In_Package
        (Package_Iterator : Construct_Tree_Iterator;
         Allow_Private    : Boolean);
      --  See if we find the seeked entity in the given package. This will
      --  not check any of the use clause nor any of the entities in the
      --  encolsed or enclosing scopes. When the seeked entity is found, it is
      --  added to the list if relevant.

      procedure Add_If_Visible (It : Construct_Tree_Iterator);
      --  Add the given iterator to Construct_Found if and only if the iterator
      --  it visible even with the ones already in the list. If Last is false,
      --  the iterator will be preempted instead of being appened.

      procedure Analyze_Used_Package
        (Pckg     : Construct_Tree_Iterator;
         Root_Use : String;
         Begin_It : Use_List_Pckg.List_Node);
      --  Analyze the package considering the use information. If Root_Use is
      --  not an empty string, this means that the use taken into account has
      --  to be applied to a nested package. Use clauses after Begin_It will
      --  also be checked to see if they match a nested package.

      function Get_First_Item (Name : String) return String;
      --  Return first element of the name given in parameter

      function Remove_First_Item (Name : String) return String;
      --  Return the name given in parameter without the first element

      function Simplify_Scope_Name
        (Current_Scope : Composite_Identifier;
         Checked_Scope : Composite_Identifier) return Composite_Identifier;
      --  Return Checked_Scope without the elements found in Current_Scope.
      --  This simplifies use clauses, e.g., in:
      --
      --  package A is
      --
      --     ...
      --
      --     use A.B
      --
      --  for the name A.B this function will only return the element B.

      procedure Handle_Use_For (Pckg : Construct_Tree_Iterator);
      --  Handle the use clauses for the package given in parameter, in order
      --  to resolve visibile entities in it.

      ----------
      -- Free --
      ----------

      procedure Free (This : in out Use_Record) is
         pragma Unreferenced (This);
      begin
         null;
      end Free;

      ---------------------
      -- Look_In_Package --
      ---------------------

      procedure Look_In_Package
        (Package_Iterator : Construct_Tree_Iterator;
         Allow_Private    : Boolean)
      is
         It : Construct_Tree_Iterator;
      begin
         It := Get_Last_Child (Tree, Package_Iterator);

         while Get_Parent_Scope (Tree, It) = Package_Iterator loop
            if Get_Construct (It).Category in Cat_Package .. Cat_Field
              and then
                (Allow_Private
                 or else Get_Construct (It).Visibility = Visibility_Public)
              and then Get_Construct (It).Name /= null
              and then Name_Match (Get_Construct (It).Name.all)
            then
               Add_If_Visible (It);
            end if;

            It := Prev (Tree, It, Jump_Over);
         end loop;
      end Look_In_Package;

      ----------
      -- Free --
      ----------

      procedure Free (This : in out Construct_Tree_Iterator) is
         pragma Unreferenced (This);
      begin
         null;
      end Free;

      --------------------
      -- Add_If_Visible --
      --------------------

      procedure Add_If_Visible (It : Construct_Tree_Iterator) is
         Node : Construct_Iterator_List_Pckg.List_Node :=
           First (Constructs_Found);
      begin
         while Node /= Construct_Iterator_List_Pckg.Null_Node loop
            if Get_Construct (It).Name.all
              = Get_Construct (Data (Node)).Name.all
            then
               --  If we found an other node wich is not a subprogram, then the
               --  one we found is not visible.

               if Get_Construct (Data (Node)).Category not in
                 Subprogram_Category
               then
                  return;
               end if;
            end if;

            Node := Next (Node);
         end loop;

         Append (Constructs_Found, It);
      end Add_If_Visible;

      ----------------
      -- Name_Match --
      ----------------

      function Name_Match (Name_Tested : String) return Boolean is
      begin
         if Is_Partial then
            return Name_Tested'Length >= Name'Length
              and then Name_Tested
                (Name_Tested'First
                 .. Name_Tested'First + Name'Length - 1)
              = Name;
         else
            return Name = Name_Tested;
         end if;
      end Name_Match;

      --------------------------
      -- Analyze_Used_Package --
      --------------------------

      procedure Analyze_Used_Package
        (Pckg     : Construct_Tree_Iterator;
         Root_Use : String;
         Begin_It : Use_List_Pckg.List_Node)
      is
         Tree_It : Construct_Tree_Iterator;
         Use_It  : Use_List_Pckg.List_Node := Next (Begin_It);
      begin
         if Root_Use /= "" then
            --  See the sub entities of this Root_Use

            Tree_It := Next (Tree, Pckg, Jump_Into);

            while Get_Parent_Scope (Tree, Tree_It) = Pckg loop
               if Get_Construct (Tree_It).Category = Cat_Package
                 and then Get_Construct (Tree_It).Is_Declaration
               then
                  if Get_First_Item (Get_Construct (Tree_It).Name.all)
                    = Get_First_Item (Root_Use)
                  then
                     Analyze_Used_Package
                       (Tree_It,
                        Remove_First_Item (Root_Use),
                        Begin_It);
                  end if;
               end if;

               Tree_It := Next (Tree, Tree_It, Jump_Over);
            end loop;
         else
            --  Analyze the current package

            Look_In_Package (Pckg, False);
         end if;

         --  See the use that have been called after

         Tree_It := Next (Tree, Pckg, Jump_Into);

         while Get_Parent_Scope (Tree, Tree_It) = Pckg loop
            Use_It := Next (Begin_It);

            if Get_Construct (Tree_It).Category = Cat_Package
              and then Get_Construct (Tree_It).Is_Declaration
            then
               while Use_It /= Use_List_Pckg.Null_Node loop
                  if not Data (Use_It).Removed
                    and then Get_First_Item (Data (Use_It).Name.all)
                    = Get_First_Item (Get_Construct (Tree_It).Name.all)
                  then
                     Analyze_Used_Package
                       (Tree_It,
                        Remove_First_Item (Get_Construct (Tree_It).Name.all),
                        Use_It);

                     Data_Ref (Use_It).Removed := True;
                  end if;

                  Use_It := Next (Use_It);
               end loop;
            end if;

            Tree_It := Next (Tree, Tree_It, Jump_Over);
         end loop;

      end Analyze_Used_Package;

      --------------------
      -- Get_First_Item --
      --------------------

      function Get_First_Item (Name : String) return String is
         Index : Natural := Name'First;
      begin
         Skip_To_Char (Name, Index, '.');

         if Index > Name'Last then
            return Name;
         else
            return Name (Name'First .. Index - 1);
         end if;
      end Get_First_Item;

      -----------------------
      -- Remove_First_Item --
      -----------------------

      function Remove_First_Item (Name : String) return String is
         Index : Natural := Name'First;
      begin
         Skip_To_Char (Name, Index, '.');

         if Index > Name'Last then
            return "";
         else
            return Name (Index + 1 .. Name'Last);
         end if;
      end Remove_First_Item;

      -------------------------
      -- Simplify_Scope_Name --
      -------------------------

      function Simplify_Scope_Name
        (Current_Scope : Composite_Identifier;
         Checked_Scope : Composite_Identifier) return Composite_Identifier
      is
         Checked_Index   : Integer := 1;
         Has_To_Be_Equal : Boolean := False;
      begin
         for Current_Index in 1 .. Length (Current_Scope) loop
            if not Has_To_Be_Equal
              and then Get_Item (Current_Scope, Current_Index)
              = Get_Item (Checked_Scope, 1)
            then
               Has_To_Be_Equal := True;
               Checked_Index := 2;
            end if;

            if Has_To_Be_Equal then
               if Checked_Index > Length (Checked_Scope) or else
                 Get_Item (Current_Scope, Current_Index)
                 /= Get_Item (Checked_Scope, Checked_Index)
               then
                  return Checked_Scope;
               else
                  Checked_Index := Checked_Index + 1;
               end if;
            end if;
         end loop;

         return Get_Slice
           (Checked_Scope, Checked_Index, Length (Checked_Scope));
      end Simplify_Scope_Name;

      --------------------
      -- Handle_Use_For --
      --------------------

      procedure Handle_Use_For (Pckg : Construct_Tree_Iterator) is
         Full_Name : constant String := Get_Full_Name (Tree, Pckg);
         Use_It    : Use_List_Pckg.List_Node;
      begin
         Use_It := First (Use_List);

         while Use_It /= Use_List_Pckg.Null_Node loop
            if not Data (Use_It).Removed then
               if Get_First_Item (Data (Use_It).Name.all) =
                 Get_First_Item
                   (Get_Construct (Pckg).Name.all)
               then
                  --  In this case, the first element of the use clause matches

                  Analyze_Used_Package
                    (Pckg,
                     Remove_First_Item (Data (Use_It).Name.all),
                     Use_It);

                  Data_Ref (Use_It).Removed := True;
               else
                  --  Otherwise, check if by removing the eventual extra prefix
                  --  we find something

                  declare
                     Scope_Id   : constant Composite_Identifier :=
                       To_Composite_Identifier (Full_Name);
                     Use_Id     : constant Composite_Identifier :=
                       To_Composite_Identifier (Data (Use_It).Name.all);
                     New_Use_Id : constant Composite_Identifier :=
                       Simplify_Scope_Name (Scope_Id, Use_Id);
                  begin
                     if Length (New_Use_Id) > 0
                       and then Get_Item (New_Use_Id, 1)
                       = Get_First_Item (Get_Construct (Pckg).Name.all)
                     then
                        Analyze_Used_Package
                          (Pckg,
                           Remove_First_Item (To_String (New_Use_Id)),
                           Use_It);

                        Data_Ref (Use_It).Removed := True;
                     end if;
                  end;
               end if;
            end if;

            Use_It := Next (Use_It);
         end loop;

         --  Once we finished to analyze the package and its possible
         --  interresting entities trough the uses, we have to remove every use
         --  clause that have been resolved.

         declare
            It      : Use_List_Pckg.List_Node := First (Use_List);
            Garbage : Use_List_Pckg.List_Node;
         begin
            while It /= Use_List_Pckg.Null_Node loop
               if Data (It).Removed then
                  Garbage := It;
                  It := Next (It);
                  Remove_Nodes (Use_List, Garbage, Garbage);
               else
                  It := Next (It);
               end if;
            end loop;
         end;
      end Handle_Use_For;

   begin
      if From /= Null_Construct_Tree_Iterator then

         --  Look back to see if we find the entity

         Seek_Iterator := From;

         while Seek_Iterator /= Null_Construct_Tree_Iterator loop

            case Get_Construct (Seek_Iterator).Category is
               when Cat_Use =>

                  --  If we are on a use clause, then have a look at the
                  --  corresponding package and see if we find the
                  --  seeked entity

                  if Use_Wise then
                     Prepend
                       (Use_List,
                        ((Get_Construct (Seek_Iterator).Name),
                         False));
                  end if;

               when Cat_Package .. Cat_Field =>

                  --  If we are on a named construct, check if it's the one
                  --  we are actually looking for

                  if Get_Construct (Seek_Iterator).Name /= null
                    and then Name_Match
                      (Get_Construct (Seek_Iterator).Name.all)
                  then
                     Add_If_Visible (Seek_Iterator);
                  end if;

                  if Get_Construct (Seek_Iterator).Category = Cat_Package
                    and then Use_Wise
                    and then Get_Construct (Seek_Iterator).Is_Declaration
                  then
                     Handle_Use_For (Seek_Iterator);
                  end if;

               when others =>
                  null;

            end case;

            Prev_Iterator := Prev (Tree, Seek_Iterator, Jump_Over);

            if Get_Parent_Scope (Tree, Seek_Iterator) /=
              Get_Parent_Scope (Tree, Prev_Iterator)
            then
               --  We are about to leave the current scope. First, have a look
               --  at the specification if any.

               Initial_Parent := Get_Parent_Scope (Tree, Seek_Iterator);

               if Initial_Parent.Node.Spec_Index /= 0
                 and then Initial_Parent.Node.Spec_Index
                   /= Initial_Parent.Index
               then
                  --  There is actually a spec somewhere. Get it and look for
                  --  possible entities in jump_over mode.

                  Initial_Parent :=
                    (Tree (Initial_Parent.Node.Spec_Index),
                     Initial_Parent.Node.Spec_Index);

                  Look_In_Package (Initial_Parent, True);
               end if;
            end if;

            Seek_Iterator := Prev_Iterator;
         end loop;

      end if;

      Free (Use_List);

      declare
         Result : Construct_Tree_Iterator_Array
           (1 .. Length (Constructs_Found));

         Node : Construct_Iterator_List_Pckg.List_Node :=
           First (Constructs_Found);
      begin
         for J in reverse 1 .. Length (Constructs_Found) loop
            Result (J) := Data (Node);

            Node := Next (Node);
         end loop;

         Free (Constructs_Found);

         return Result;
      end;
   end Get_Visible_Constructs;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree         : Construct_Tree;
      Start_Entity : Construct_Tree_Iterator;
      Id           : Composite_Identifier;
      Use_Wise     : Boolean := True)
      return Construct_Tree_Iterator_Array
   is
      Current_Scope : Construct_Tree_Iterator;

      Visible_Constructs : constant Construct_Tree_Iterator_Array :=
        Get_Visible_Constructs
          (Tree, Start_Entity, Get_Item (Id, 1), Use_Wise);
   begin
      if Length (Id) = 1 then
         return Visible_Constructs;
      elsif Visible_Constructs'Length >= 1 then
         Current_Scope := Visible_Constructs (1);
      else
         return Null_Construct_Tree_Iterator_Array;
      end if;

      for J in 2 .. Length (Id) loop
         declare
            Name_Seeked        : constant String := Get_Item (Id, J);
            End_Of_Scope_Index : constant Natural :=
              Current_Scope.Index + Current_Scope.Node.Sub_Nodes_Length;
            End_Of_Scope       : constant Construct_Tree_Iterator :=
              (Tree (End_Of_Scope_Index), End_Of_Scope_Index);

            Visible_Constructs : constant Construct_Tree_Iterator_Array :=
              Get_Visible_Constructs
              (Tree, End_Of_Scope, Name_Seeked, Use_Wise);
         begin
            if J = Length (Id) then
               return Visible_Constructs;
            end if;

            if Visible_Constructs'Length >= 1 then
               --  ??? We do not handle cases where there are serveal
               --  possibilities here. Should we ?
               Current_Scope := Visible_Constructs (1);
            else
               return Null_Construct_Tree_Iterator_Array;
            end if;
         end;
      end loop;

      return Null_Construct_Tree_Iterator_Array;
   end Get_Visible_Constructs;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree     : Construct_Tree;
      Offset   : Natural;
      Id       : Composite_Identifier;
      Use_Wise : Boolean := True)
      return Construct_Tree_Iterator_Array
   is
   begin
      return
        Get_Visible_Constructs
          (Tree, Get_Last_Relevant_Construct (Tree, Offset), Id, Use_Wise);
   end Get_Visible_Constructs;

end Language.Tree;
