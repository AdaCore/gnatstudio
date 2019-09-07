------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System.Memory; use System.Memory;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body Tries is

   Me : constant Trace_Handle := Create ("GPS.COMMON.Tries");

   pragma Warnings (Off);
   --  These 4 UCs are safe aliasing-wise, so kill warning
   function Convert is new Ada.Unchecked_Conversion
     (Cell_Child_Array_Access, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Cell_Child_Array_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Data_Type_Array_Access, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Data_Type_Array_Access);
   pragma Warnings (On);

   Component_Size : constant size_t :=
     Cell_Child_Array'Component_Size / System.Storage_Unit;

   procedure Free (Cell : in out Cell_Child);
   --  Free the memory used by Cell and its own children

   function Get_Index (Cell : Cell_Child) return Cst_String_Access;
   --  Return the base index for the cell. Only the Cell.Index_Length first
   --  characters should be considered in the returned string.
   --  The returned string must not be freed.

   procedure Update_Children_Parent (Cell : Cell_Child_Access);
   --  This function is used when the children array of a cell has been
   --  reallocated. In this case, all children have to update their link to
   --  their parent.

   procedure Increment_Clock (Tree : in out Trie_Tree);
   --  This function increment the change counter of the tree. If such a
   --  counted is not yet created, it gets.

   procedure Adjust_If_Need
     (Tree_Root_Cell : Cell_Child_Access; Iter : in out Iterator);
   --  If the iterator is no more consistent with the data, then adjust it. If
   --  it's not possible, the iterator will be then set to At_End.

   --------------------
   --  Find_Cell_Child:
   --------------------

   --  Return the closest cell for Index. The first Last characters of Index
   --  have been processed so far on exit.  Kind describes the scenario, as
   --  in the following examples.
   --  Cell_Parent is the parent cell of Cell.
   --
   --  First case:  (eg: index_last=2, Last=4, Index="abcdef")
   --     It is garanteed there are extra characters in the index, otherwise
   --     the case 4 is raised
   --           "ab"                       "ab"
   --           /  \             =>        /  \
   --        "iv"  Cell:"cdhh"          "iv"  Cell:"cd"
   --                / \                       / \
   --                                       "hh"  "ef" (new cell)
   --                                       / \
   --
   --  Second case: (eg: Index="abcdef", Last=4)
   --           "ab"                       "ab"
   --           /  \              =>       /  \
   --        "iv"  Cell:"cdefgh"         "iv"  Cell:"cdef"  (change data)
   --                / \                        /
   --                                        "gh"
   --                                        / \
   --
   --  Third case: (eg: Index="abcdef"):  Full match
   --           "ab"                       "ab"
   --           /  \              =>        /  \
   --        "iv"  Cell:"cdef"           "iv"  Cell:"cdef" (change data)
   --                /  \                         / \
   --
   --  Fourth case:  (Last=2)
   --           Cell:"ab"                  Cell:"ab"
   --           /  \              =>        /  \     \
   --        "iv"  "to"                  "iv" "to"   "cdef" (new cell)
   --
   --  Fifth case: the tree is currently empty

   procedure Find_Cell_Child
     (Root_Cell      : Cell_Child_Access;
      Case_Sensitive : Boolean;
      Index          : String;
      Pointer        : out Cell_Pointer);
   --  Access a specific cell in the tree. The result value should only be
   --  used before the next write-access to the tree, or it becomes obsolete.
   --  Index should not be a null string.

   ----------
   -- Free --
   ----------

   procedure Free (Cell : in out Cell_Child) is
   begin
      Free (Cell.Data);

      if Cell.Children /= null then
         for C in Cell.Children'First .. Cell.Num_Children loop
            Free (Cell.Children (C));
         end loop;

         Free (Convert (Cell.Children));
         Cell.Children := null;
      end if;
   end Free;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Cell : Cell_Child) return Cst_String_Access is
      C   : Cell_Child := Cell;
      Ind : Cst_String_Access;
   begin
      loop
         Ind := Get_Index (C.Data);

         if Ind /= null then
            return Ind;
         end if;

         if C.Children = null then
            return Ind;
         end if;

         --  There is at least one child if we had No_Data stored in the
         --  tree.
         C := C.Children (C.Children'First);
      end loop;
   end Get_Index;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree : in out Trie_Tree) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Mod_Counter, Mod_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Cell_Child, Cell_Child_Access);
   begin
      if Tree.Child /= null then
         Free (Tree.Child.all);
         Free (Tree.Child);
      end if;

      Free (Tree.Mod_Clock);
   end Clear;

   ----------
   -- Dump --
   ----------

   procedure Dump (Tree : Trie_Tree) is
      procedure Dump (Cell : Cell_Child; Eliminate : Natural);
      --  Dump a cell

      procedure Dump (Cell : Cell_Child; Eliminate : Natural) is
         Ind : constant Cst_String_Access := Get_Index (Cell);
      begin
         if Ind = null then
            Put ("(<null>");
         elsif Ind'First + Cell.Index_Length - 1 > Ind'Last then
            Put ("('" & Ind (Ind'First + Eliminate .. Ind'Last)
                 & "(invalid length:" & Integer'Image (Cell.Index_Length)
                 & ")'");
         else
            Put ("('"
                 & Ind (Ind'First + Eliminate ..
                          Ind'First + Cell.Index_Length - 1) & "'");
         end if;

         Put (Cell.First_Char_Of_Key & ' ');
         Put (Cell.Data);
         Put (" ");

         if Cell.Children /= null then
            for C in Cell.Children'First .. Cell.Num_Children loop
               Dump (Cell.Children (C), Cell.Index_Length);
            end loop;
         end if;

         Put (")");
      end Dump;
   begin
      if Tree.Child = null or else Tree.Child.Children = null then
         Put ("('')");
      else
         Dump (Tree.Child.all, 0);
      end if;
   end Dump;

   ---------------------
   -- Find_Cell_Child --
   ---------------------

   procedure Find_Cell_Child
     (Root_Cell      : Cell_Child_Access;
      Case_Sensitive : Boolean;
      Index          : String;
      Pointer        : out Cell_Pointer)
   is
      Current  : Cell_Child_Access := Root_Cell;
      Start    : Integer := Index'First;
      Ind      : Cst_String_Access;
      Ind_First, Ind_Last : Natural;
      Child    : Integer;
      TL       : Character;
   begin
      pragma Assert (Index'Length /= 0);

      --  If we are processing the root node
      if Root_Cell.Children = null then
         Pointer.Cell_Parent := null;
         Pointer.Cell        := Current;
         Pointer.Last        := Index'First;
         Pointer.Scenario    := 5;
         return;
      end if;

      while Current /= null and then Current.Children /= null loop
         --  Find matching child. There is at most one of these.

         Child := Current.Children'First;

         if not Case_Sensitive then
            TL := To_Lower (Index (Start));
         end if;

         loop
            if Case_Sensitive then
               exit when
                 Current.Children (Child).First_Char_Of_Key = Index (Start);
            else
               exit when
                 To_Lower (Current.Children (Child).First_Char_Of_Key) = TL;
            end if;

            Child := Child + 1;

            if Child > Current.Num_Children then
               Pointer.Last     := Start;
               Pointer.Cell     := Current;
               Pointer.Scenario := 4;
               return;
            end if;
         end loop;

         Pointer.Cell_Parent := Current;
         Current := Current.Children (Child)'Access;
         Ind     := Get_Index (Current.all);

         if Ind = null then
            pragma Assert (False);

            --  This should never happen, but if it does, return a null
            --  entity rather than failing later on in case assertions
            --  are disabled.

            Pointer.Cell := null;
            Pointer.Scenario := 0;
            return;
         end if;

         Ind_First := Ind'First + Pointer.Cell_Parent.Index_Length;
         Ind_Last  := Ind'First + Current.Index_Length - 1;

         if Start = Index'Last then
            Pointer.Cell := Current;
            Pointer.Last := Index'Last;

            if Ind_First = Ind_Last then
               Pointer.Scenario := 3;
            else
               Pointer.First_Not_Matched :=
                 Ind (Ind'First + Pointer.Cell_Parent.Index_Length + 1);
               Pointer.Scenario     := 2;
               Pointer.Index_Length := Index'Length;
            end if;
            return;
         end if;

         Start := Start + 1;
         for J in Ind_First + 1 .. Ind_Last loop
            if (Case_Sensitive and then Ind (J) /= Index (Start))
              or else
                (not Case_Sensitive
                 and then To_Lower (Ind (J)) = To_Lower (Index (Start)))
            then
               --  If at least one character matched, this is the
               --  correct cell, although it will have to be split
               Pointer.Cell              := Current;
               Pointer.Last              := Start - 1;
               Pointer.Index_Length      := J - Ind'First;
               Pointer.First_Not_Matched := Ind (J);
               Pointer.Scenario          := 1;
               return;
            end if;

            Start := Start + 1;

            --  Cell matches, but will have to be splitted
            if Start > Index'Last then
               Pointer.Cell := Current;
               Pointer.Last := Start;

               --  If all the characters of the index matched,
               --  we have found our cell

               if J = Ind_Last then
                  Pointer.Scenario := 3;
               else
                  Pointer.Index_Length      := J - Ind'First + 1;
                  Pointer.First_Not_Matched := Ind (J + 1);
                  Pointer.Scenario          := 2;
               end if;
               return;
            end if;
         end loop;

         --  If at least one character matched, but the index was
         --  too short, check the children
      end loop;

      Pointer.Last     := Start;
      Pointer.Cell     := Current;
      Pointer.Scenario := 4;
   end Find_Cell_Child;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree : in out Trie_Tree;
      Data : Data_Type)
   is
      Pointer : Cell_Pointer;
      Index   : constant Cst_String_Access := Get_Index (Data);
   begin
      if Index = null then
         return;
      end if;

      if Tree.Child = null then
         Tree.Child := new Cell_Child;
      end if;

      Find_Cell_Child (Tree.Child, Tree.Case_Sensitive, Index.all, Pointer);

      if Pointer.Cell /= null then
         Increment_Clock (Tree);
         Insert (Index.all, Pointer, Data);
      end if;
   end Insert;

   ----------------------------
   -- Update_Children_Parent --
   ----------------------------

   procedure Update_Children_Parent (Cell : Cell_Child_Access) is
   begin
      for J in 1 .. Cell.Num_Children loop
         Cell.Children (J).Parent_Cell := Cell;
      end loop;
   end Update_Children_Parent;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Index : String; Pointer : Cell_Pointer; Data : Data_Type)
   is
      Initial_Array_Size : constant := 8;

      Children     : Cell_Child_Array_Access;
   begin
      case Pointer.Scenario is
         when 1 =>
            Children := Convert (Alloc (Initial_Array_Size * Component_Size));

            Children (Children'First) :=
              (Data              => Pointer.Cell.Data,
               First_Char_Of_Key => Pointer.First_Not_Matched,
               Index_Length      => Pointer.Cell.Index_Length,
               Num_Children      => Pointer.Cell.Num_Children,
               Children_Length   => Pointer.Cell.Children_Length,
               Children          => Pointer.Cell.Children,
               Parent_Cell       => Pointer.Cell,
               Number_In_Parent  => 1);

            Children (Children'First + 1) :=
              (Data              => Data,
               First_Char_Of_Key => Index (Index'First + Pointer.Index_Length),
               Index_Length      => Index'Length,
               Num_Children      => 0,
               Children_Length   => 0,
               Children          => null,
               Parent_Cell       => Pointer.Cell,
               Number_In_Parent  => 2);

            if Children (Children'First).First_Char_Of_Key >
              Children (Children'First + 1).First_Char_Of_Key
            then
               --  In this case, the two nodes are not in order. Swap them.

               declare
                  Tmp : constant Cell_Child := Children (Children'First);
               begin
                  Children (Children'First) := Children (Children'First + 1);
                  Children (Children'First + 1) := Tmp;

                  Children (Children'First).Number_In_Parent := 1;
                  Children (Children'First + 1).Number_In_Parent := 2;
               end;
            end if;

            Update_Children_Parent (Children (Children'First)'Access);
            Update_Children_Parent (Children (Children'First + 1)'Access);

            Pointer.Cell.all :=
              (Data              => No_Data,
               Index_Length      => Pointer.Index_Length,
               First_Char_Of_Key => Pointer.Cell.First_Char_Of_Key,
               Num_Children      => 2,
               Children_Length   => Initial_Array_Size,
               Children          => Children,
               Parent_Cell       => Pointer.Cell.Parent_Cell,
               Number_In_Parent  => Pointer.Cell.Number_In_Parent);

         when 2 =>
            Children := Convert (Alloc (Initial_Array_Size * Component_Size));
            Children (Children'First) :=
              (Data              => Pointer.Cell.Data,
               Index_Length      => Pointer.Cell.Index_Length,
               First_Char_Of_Key => Pointer.First_Not_Matched,
               Num_Children      => Pointer.Cell.Num_Children,
               Children_Length   => Pointer.Cell.Children_Length,
               Children          => Pointer.Cell.Children,
               Parent_Cell       => Pointer.Cell,
               Number_In_Parent  => 1);

            Update_Children_Parent (Children (Children'First)'Access);

            Pointer.Cell.all :=
              (Data              => Data,
               Index_Length      => Pointer.Index_Length,
               First_Char_Of_Key => Pointer.Cell.First_Char_Of_Key,
               Num_Children      => 1,
               Children_Length   => Initial_Array_Size,
               Children          => Children,
               Parent_Cell       => Pointer.Cell.Parent_Cell,
               Number_In_Parent  => Pointer.Cell.Number_In_Parent);

         when 3 =>
            Free (Pointer.Cell.Data);
            Pointer.Cell.Data := Data;

         when 4 | 5 =>
            if Pointer.Cell.Children /= null then
               if Pointer.Cell.Num_Children = Pointer.Cell.Children_Length then
                  Pointer.Cell.Children_Length :=
                    Pointer.Cell.Children_Length * 2;

                  Pointer.Cell.Children     := Convert
                    (Realloc (Convert (Pointer.Cell.Children),
                     size_t (Pointer.Cell.Children_Length) * Component_Size));
               end if;

               Pointer.Cell.Num_Children := Pointer.Cell.Num_Children + 1;
            else
               Pointer.Cell.Children     := Convert
                 (Alloc (Initial_Array_Size * Component_Size));
               Pointer.Cell.Children_Length := Initial_Array_Size;
               Pointer.Cell.Num_Children := 1;
            end if;

            Pointer.Cell.Children (Pointer.Cell.Num_Children) :=
              (Data              => Data,
               First_Char_Of_Key => Index (Pointer.Last),
               Index_Length      => Index'Length,
               Num_Children      => 0,
               Children_Length   => 0,
               Children          => null,
               Parent_Cell       => Pointer.Cell,
               Number_In_Parent  => Pointer.Cell.Num_Children);

            --  We know that there is only one element missplaced here, which
            --  is the last one. So we just give one round of bubble sort to
            --  place it at the proper location.

            for J in reverse 1 .. Pointer.Cell.Num_Children - 1 loop
               if Pointer.Cell.Children (J).First_Char_Of_Key >
                 Pointer.Cell.Children (J + 1).First_Char_Of_Key
               then
                  declare
                     Tmp : constant Cell_Child := Pointer.Cell.Children (J);
                  begin
                     Pointer.Cell.Children (J) :=
                       Pointer.Cell.Children (J + 1);
                     Pointer.Cell.Children (J + 1) := Tmp;

                     Pointer.Cell.Children (J).Number_In_Parent := J;
                     Pointer.Cell.Children (J + 1).Number_In_Parent := J + 1;
                  end;
               else
                  exit;
               end if;
            end loop;

            --  Now we have to update the value of "parent" for the new array.

            for J in 1 .. Pointer.Cell.Num_Children loop
               Update_Children_Parent (Pointer.Cell.Children (J)'Access);
            end loop;

         when others =>
            null;
      end case;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (Tree : in out Trie_Tree; Index : String) is
      Pointer : Cell_Pointer;
   begin
      if Tree.Child = null then
         Tree.Child := new Cell_Child;
      end if;

      Find_Cell_Child (Tree.Child, Tree.Case_Sensitive, Index, Pointer);

      if Pointer.Cell /= null then
         Increment_Clock (Tree);
         Remove (Tree, Pointer);
      end if;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (Tree : in out Trie_Tree; Pointer : Cell_Pointer) is
      Tmp : Cell_Child_Array_Access;
   begin
      --  Warning: Cell points into the .Children array of its parent.
      --  Modifying the later will indirectly also modify what is pointed to
      --  by Cell.

      if Pointer.Scenario = 3 then
         Free (Pointer.Cell.Data);
         Pointer.Cell.Data := No_Data;

         if Pointer.Cell.Children = null then
            if Pointer.Cell_Parent /= null then
               --  If there was one single child (the cell we are removing):
               if Pointer.Cell_Parent.Num_Children = 1 then
                  Free (Convert (Pointer.Cell_Parent.Children));
                  Pointer.Cell_Parent.Children := null;
                  Pointer.Cell_Parent.Num_Children := 0;
                  Pointer.Cell_Parent.Children_Length := 0;

               --  If there were two children, and the current node has no
               --  data, we can simply remove it.
               elsif Pointer.Cell_Parent.Num_Children = 2
                 and then Pointer.Cell_Parent.Data = No_Data
                 and then Pointer.Cell_Parent /= Tree.Child
               then
                  declare
                     Tmp : Cell_Child := Pointer.Cell_Parent.all;
                  begin
                     if Pointer.Cell_Parent.Children
                       (Pointer.Cell_Parent.Children'First)'Access
                       = Pointer.Cell
                     then
                        Pointer.Cell_Parent.all := Pointer.Cell_Parent.Children
                          (Pointer.Cell_Parent.Children'First + 1);
                     else
                        Pointer.Cell_Parent.all := Pointer.Cell_Parent.Children
                          (Pointer.Cell_Parent.Children'First);
                     end if;
                     Pointer.Cell_Parent.First_Char_Of_Key :=
                       Tmp.First_Char_Of_Key;
                     Free (Convert (Tmp.Children));
                     Tmp.Children := null;
                  end;

               else
                  Pointer.Cell.all := Pointer.Cell_Parent.Children
                    (Pointer.Cell_Parent.Num_Children);
                  Pointer.Cell_Parent.Num_Children :=
                    Pointer.Cell_Parent.Num_Children - 1;

                  --  ??? We don't remove anymore the actual array, since we
                  --  expect it to be filled later. However, this could be less
                  --  drastric and controlled by a switch.

--                    Pointer.Cell_Parent.Children := Convert
--                      (Realloc
--                         (Convert (Pointer.Cell_Parent.Children),
--                          size_t (Pointer.Cell_Parent.Num_Children)
--                          * Component_Size));
               end if;
            end if;

         elsif Pointer.Cell.Num_Children = 1 then
            if Pointer.Cell_Parent /= null then
               --  Replace the cell by its single child in the parent
               --  We cannot free Cell.Children, which still points to
               --  Cell_Parent.Children (C)

               Tmp := Pointer.Cell.Children;
               Pointer.Cell.Children
                 (Pointer.Cell.Children'First).First_Char_Of_Key :=
                 Pointer.Cell.First_Char_Of_Key;
               Pointer.Cell.all := Pointer.Cell.Children
                 (Pointer.Cell.Children'First);
               Free (Convert (Tmp));

            else
               Tree.Child.all := Pointer.Cell.all;
            end if;
         end if;

         Update_Children_Parent (Pointer.Cell_Parent);

      else
         Trace (Me, "Couldn't remove from Tree scenario="
                & Pointer.Scenario'Img);
      end if;
   end Remove;

   ---------
   -- Get --
   ---------

   function Get (Tree : access Trie_Tree; Index : String) return Data_Type is
      Pointer : Cell_Pointer;
   begin
      if Tree.Child = null then
         Tree.Child := new Cell_Child;
      end if;

      Find_Cell_Child (Tree.Child, Tree.Case_Sensitive, Index, Pointer);

      if Pointer.Scenario = 3 then
         return Pointer.Cell.Data;
      end if;

      return No_Data;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Pointer : Cell_Pointer) return Data_Type is
   begin
      if Pointer.Scenario = 3 then
         return Pointer.Cell.Data;
      end if;
      return No_Data;
   end Get;

   -----------
   -- Start --
   -----------

   function Start (Tree : access Trie_Tree; Prefix : String) return Iterator is
      Pointer   : Cell_Pointer;
      Iter      : Iterator := Null_Iterator;
   begin
      if Tree.Child = null then
         Iter.Current_Cell := null;
         Iter.Root_Cell := null;

         return Iter;
      end if;

      Iter.Mod_Clock := Tree.Mod_Clock;
      Iter.Case_Sensitive := Tree.Case_Sensitive;

      if Iter.Mod_Clock /= null then
         Iter.Initial_Timestamp := Iter.Mod_Clock.all;
      end if;

      if Prefix = "" then
         Iter.Current_Cell := Tree.Child;
         Iter.Current_Index := 1;
      else
         --  Find the closest cell that matches the prefix
         Find_Cell_Child (Tree.Child, Tree.Case_Sensitive, Prefix, Pointer);

         if Pointer.Scenario = 1 or else Pointer.Scenario in 4 .. 5 then
            Iter.Current_Cell := null;
            Iter.Root_Cell := null;

            return Iter;
         end if;

         Iter.Current_Cell := Pointer.Cell;
         Iter.Current_Index := 1;
      end if;

      Iter.Trie_Root_Cell := Tree.Child;
      Iter.Root_Cell := Iter.Current_Cell;
      Iter.Root_Name := new String'(Prefix);
      Iter.Current_Name := new String'("");
      Iter.Current_Name_Length := 0;

      if not Is_Valid (Iter) then
         Next (Iter);
      end if;

      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Iterator) is
      Force_Loop : Boolean := False;
   begin
      Adjust_If_Need (Iter.Trie_Root_Cell, Iter);

      loop
         Force_Loop := False;

         if At_End (Iter) then
            return;
         end if;

         if Iter.Current_Cell = Iter.Root_Cell
           and then Iter.Current_Cell.Children = null
         then
            --  We find only one element, which has already been returned, so
            --  this is the end of the search.

            Iter.Current_Cell := null;

            return;
         elsif Iter.Current_Index > Iter.Current_Cell.Num_Children then
            Iter.Current_Index := Iter.Current_Cell.Number_In_Parent + 1;

            Iter.Current_Cell := Iter.Current_Cell.Parent_Cell;

            if Iter.Current_Cell = Iter.Root_Cell
              and then Iter.Current_Index > Iter.Root_Cell.Num_Children
            then
               --  Here, we are at the end of the search
               Iter.Current_Cell := null;

               return;
            end if;

            Force_Loop := True;
         else
            Iter.Current_Cell := Iter.Current_Cell.Children
              (Iter.Current_Index)'Access;
            Iter.Current_Index := 1;
         end if;

         exit when not Force_Loop and then Is_Valid (Iter);
      end loop;

      if not At_End (Iter) then
         declare
            Full_Name : constant String :=
              Get_Index (Iter.Current_Cell.Data).all;
            Suffix    : constant String :=
              Full_Name
                (Full_Name'First + Iter.Root_Name'Length .. Full_Name'Last);

         begin
            if Iter.Current_Name'Length < Suffix'Length then
               Free (Iter.Current_Name);
               Iter.Current_Name := new String'(Suffix);
            else
               --  ??? To minimize memory copy, we could just copy the delta
               --  between this and its parent.
               Iter.Current_Name.all
                 (Iter.Current_Name'First
                  .. Iter.Current_Name'First + Suffix'Length - 1) := Suffix;
            end if;

            Iter.Current_Name_Length := Suffix'Length;
         end;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Iterator) return Data_Type is
   begin
      if Iter.Mod_Clock /= null
        and then Iter.Mod_Clock.all /= Iter.Initial_Timestamp
      then
         return No_Data;
      end if;

      if not At_End (Iter) then
         return Iter.Current_Cell.Data;
      else
         return No_Data;
      end if;
   end Get;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Iter : Iterator) return String is
   begin
      return Iter.Root_Name.all
        & Iter.Current_Name (1 .. Iter.Current_Name_Length);
   end Get_Index;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Iterator) return Boolean is
   begin
      return Iter.Current_Cell = null;
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Iter : Iterator) return Boolean is
   begin
      return At_End (Iter)
        or else Iter.Current_Cell.Data /= No_Data;
   end Is_Valid;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Iterator) is
   begin
      Free (Iter.Root_Name);
      Free (Iter.Current_Name);
   end Free;

   ---------------------
   -- Increment_Clock --
   ---------------------

   procedure Increment_Clock (Tree : in out Trie_Tree) is
   begin
      if Tree.Mod_Clock = null then
         Tree.Mod_Clock := new Mod_Counter'(0);
      end if;

      Tree.Mod_Clock.all := Tree.Mod_Clock.all + 1;
   end Increment_Clock;

   --------------------
   -- Adjust_If_Need --
   --------------------

   procedure Adjust_If_Need
     (Tree_Root_Cell : Cell_Child_Access; Iter : in out Iterator)
   is
      Pointer : Cell_Pointer;
   begin
      if Iter.Mod_Clock /= null
        and then Iter.Mod_Clock.all /= Iter.Initial_Timestamp
      then
         declare
            Full_Name : constant String := Iter.Root_Name.all
              & Iter.Current_Name
              (Iter.Current_Name'First
               .. Iter.Current_Name'First + Iter.Current_Name_Length - 1);
         begin
            --  Here, there have been modifications. We try to retreive the
            --  cells as they were. If we can't, it's the end of the iteration.

            if Iter.Root_Name.all = "" then
               Iter.Current_Cell := Tree_Root_Cell;
               Iter.Current_Index := 1;
               Iter.Initial_Timestamp := Iter.Mod_Clock.all;
               return;
            end if;

            Find_Cell_Child
              (Tree_Root_Cell, Iter.Case_Sensitive, Iter.Root_Name.all,
               Pointer);

            if Pointer.Scenario /= 3 then
               Iter.Current_Cell := null;
               return;
            end if;

            Iter.Root_Cell := Pointer.Cell;
            Find_Cell_Child
              (Tree_Root_Cell, Iter.Case_Sensitive, Full_Name, Pointer);

            if Pointer.Scenario /= 3 then
               Iter.Current_Cell := null;
               return;
            end if;

            Iter.Current_Cell := Pointer.Cell;
            Iter.Initial_Timestamp := Iter.Mod_Clock.all;
         end;
      end if;
   end Adjust_If_Need;

end Tries;
