-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2004                      --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Unchecked_Conversion;
with System.Memory; use System.Memory;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Traces; use Traces;

package body Tries is

   Me : constant Debug_Handle := Create ("Tries");

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
   Data_Component_Size : constant size_t :=
     Data_Type_Array'Component_Size / System.Storage_Unit;

   procedure Free (Cell : in out Cell_Child);
   --  Free the memory used by Cell and its own children

   function Get_Index (Cell : Cell_Child) return String_Access;
   --  Return the base index for the cell. Only the Cell.Index_Length first
   --  characters should be considered in the returned string.
   --  The returned string must not be freed.

   type Cell_Child_Access is access all Cell_Child;

   procedure Find_Cell_Child
     (Tree         : Trie_Tree;
      Index        : String;
      Cell         : out Cell_Child_Access;
      Cell_Parent  : in out Cell_Child_Access;
      Last         : out Natural;
      Index_Length : out Natural;
      Scenario     : out Natural;
      First_Not_Matched : out Character);
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

   function Get_Index (Cell : Cell_Child) return String_Access is
      C   : Cell_Child := Cell;
      Ind : String_Access;
   begin
      loop
         Ind := Get_Index (C.Data);
         if Ind /= null or else C.Children = null then
            return Ind;
         end if;

         --  There is at least one children if we had No_Data stored in the
         --  tree.
         C := C.Children (C.Children'First);
      end loop;
   end Get_Index;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree : in out Trie_Tree) is
   begin
      Free (Tree.Child);
   end Clear;

   ----------
   -- Dump --
   ----------

   procedure Dump (Tree : Trie_Tree) is
      procedure Dump (Cell : Cell_Child; Eliminate : Natural);
      --  Dump a cell

      procedure Dump (Cell : Cell_Child; Eliminate : Natural) is
         Ind : constant String_Access := Get_Index (Cell);
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
      if Tree.Child.Children = null then
         Put ("('')");
      else
         Dump (Tree.Child, 0);
      end if;
   end Dump;

   ---------------------
   -- Find_Cell_Child --
   ---------------------

   procedure Find_Cell_Child
     (Tree         : Trie_Tree;
      Index        : String;
      Cell         : out Cell_Child_Access;
      Cell_Parent  : in out Cell_Child_Access;
      Last         : out Natural;
      Index_Length : out Natural;
      Scenario     : out Natural;
      First_Not_Matched : out Character)
   is
      Current  : Cell_Child_Access := Tree.Child'Unrestricted_Access;
      Start    : Integer := Index'First;
      Ind      : String_Access;
      Ind_First, Ind_Last : Natural;
      Child    : Integer;
   begin
      --  If we are processing the root node
      if Tree.Child.Children = null then
         Cell_Parent := null;
         Cell        := Current;
         Last        := Index'First;
         Scenario    := 5;
         return;
      end if;

      while Current /= null and then Current.Children /= null loop
         --  Find matching child. There is at most one of these.

         Child := Current.Children'First;
         loop
            exit when
              Current.Children (Child).First_Char_Of_Key = Index (Start);
            Child := Child + 1;

            if Child > Current.Num_Children then
               Last     := Start;
               Cell     := Current;
               Scenario := 4;
               return;
            end if;
         end loop;

         Cell_Parent := Current;
         Current     := Current.Children (Child)'Unrestricted_Access;
         Ind         := Get_Index (Current.all);
         Ind_First   := Ind'First + Cell_Parent.Index_Length;
         Ind_Last    := Ind'First + Current.Index_Length - 1;

         if Start = Index'Last then
            Cell := Current;
            Last := Index'Last;

            if Ind_First = Ind_Last then
               Scenario     := 3;
            else
               First_Not_Matched :=
                 Ind (Ind'First + Cell_Parent.Index_Length + 1);
               Scenario          := 2;
               Index_Length      := Index'Length;
            end if;
            return;
         end if;

         Start := Start + 1;
         for J in Ind_First + 1 .. Ind_Last loop
            if Ind (J) /= Index (Start) then
               --  If at least one character matched, this is the
               --  correct cell, although it will have to be split
               Cell              := Current;
               Last              := Start - 1;
               Index_Length      := J - Ind'First;
               First_Not_Matched := Ind (J);
               Scenario          := 1;
               return;
            end if;

            Start := Start + 1;

            --  Cell matches, but will have to be splitted
            if Start > Index'Last then
               Cell := Current;
               Last := Start;

               --  If all the characters of the index matched,
               --  we have found our cell

               if J = Ind_Last then
                  Scenario := 3;
               else
                  Index_Length      := J - Ind'First + 1;
                  First_Not_Matched := Ind (J + 1);
                  Scenario          := 2;
               end if;
               return;
            end if;
         end loop;

         --  If at least one character matched, but the index was
         --  too short, check the children
      end loop;

      Last     := Start;
      Cell     := Current;
      Scenario := 4;
   end Find_Cell_Child;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree  : in out Trie_Tree;
      Data  : Data_Type)
   is
      Cell         : Cell_Child_Access;
      Cell_Parent  : Cell_Child_Access;
      Index        : constant String_Access := Get_Index (Data);
      Last         : Integer;
      Scenario     : Integer;
      Index_Length : Integer;
      First_Not_Matched : Character;
      Children     : Cell_Child_Array_Access;
   begin
      Find_Cell_Child
        (Tree, Index.all, Cell, Cell_Parent, Last,
         Index_Length, Scenario, First_Not_Matched);

      case Scenario is
         when 1 =>
            Children := Convert (Alloc (2 * Component_Size));
            Children (Children'First) :=
              (Data              => Cell.Data,
               First_Char_Of_Key => First_Not_Matched,
               Index_Length      => Cell.Index_Length,
               Num_Children      => Cell.Num_Children,
               Children          => Cell.Children);
            Children (Children'First + 1) :=
              (Data              => Data,
               First_Char_Of_Key => Index (Index'First + Index_Length),
               Index_Length      => Index'Length,
               Num_Children      => 0,
               Children          => null);
            Cell.all :=
              (Data              => No_Data,
               Index_Length      => Index_Length,
               First_Char_Of_Key => Cell.First_Char_Of_Key,
               Num_Children      => 2,
               Children          => Children);

         when 2 =>
            Children := Convert (Alloc (1 * Component_Size));
            Children (Children'First) :=
              (Data              => Cell.Data,
               Index_Length      => Cell.Index_Length,
               First_Char_Of_Key => First_Not_Matched,
               Num_Children      => Cell.Num_Children,
               Children          => Cell.Children);
            Cell.all :=
              (Data              => Data,
               Index_Length      => Index_Length,
               First_Char_Of_Key => Cell.First_Char_Of_Key,
               Num_Children      => 1,
               Children          => Children);

         when 3 =>
            Free (Cell.Data);
            Cell.Data := Data;

         when 4 | 5 =>
            if Cell.Children /= null then
               Cell.Children     := Convert
                 (Realloc (Convert (Cell.Children),
                           size_t (Cell.Num_Children) * Component_Size
                                   + Component_Size));
               Cell.Num_Children := Cell.Num_Children + 1;
            else
               Cell.Children     := Convert (Alloc (Component_Size));
               Cell.Num_Children := 1;
            end if;

            Cell.Children (Cell.Num_Children) :=
              (Data              => Data,
               First_Char_Of_Key => Index (Last),
               Index_Length      => Index'Length,
               Num_Children      => 0,
               Children          => null);

         when others =>
            null;
      end case;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (Tree : in out Trie_Tree; Index : String) is
      Cell              : Cell_Child_Access;
      Cell_Parent       : Cell_Child_Access;
      Last              : Integer;
      Scenario          : Integer;
      Index_Length      : Integer;
      First_Not_Matched : Character;
      Tmp               : Cell_Child_Array_Access;
   begin
      Find_Cell_Child
        (Tree, Index, Cell, Cell_Parent, Last,
         Index_Length, Scenario, First_Not_Matched);

      --  Warning: Cell points into the .Children array of its parent.
      --  Modifying the later will indirectly also modify what is pointed to
      --  by Cell.

      if Scenario = 3 then
         Free (Cell.Data);
         Cell.Data := No_Data;

         if Cell.Children = null then
            if Cell_Parent /= null then
               --  If there was one single child (the cell we are removing):
               if Cell_Parent.Num_Children = 1 then
                  Free (Convert (Cell_Parent.Children));
                  Cell_Parent.Children := null;
                  Cell_Parent.Num_Children := 0;

               --  If there were two children, and the current node has no
               --  data, we can simply remove it.
               elsif Cell_Parent.Num_Children = 2
                 and then Cell_Parent.Data = No_Data
                 and then Cell_Parent /= Tree.Child'Unrestricted_Access
               then
                  declare
                     Tmp : Cell_Child := Cell_Parent.all;
                  begin
                     if Cell_Parent.Children
                       (Cell_Parent.Children'First)'Unrestricted_Access = Cell
                     then
                        Cell_Parent.all := Cell_Parent.Children
                          (Cell_Parent.Children'First + 1);
                     else
                        Cell_Parent.all := Cell_Parent.Children
                          (Cell_Parent.Children'First);
                     end if;
                     Cell_Parent.First_Char_Of_Key := Tmp.First_Char_Of_Key;
                     Free (Convert (Tmp.Children));
                     Tmp.Children := null;
                  end;

               else
                  Cell.all := Cell_Parent.Children (Cell_Parent.Num_Children);
                  Cell_Parent.Num_Children := Cell_Parent.Num_Children - 1;
                  Cell_Parent.Children := Convert
                    (Realloc
                       (Convert (Cell_Parent.Children),
                        size_t (Cell_Parent.Num_Children) * Component_Size));
               end if;
            end if;

         elsif Cell.Num_Children = 1 then
            if Cell_Parent /= null then
               --  Replace the cell by its single child in the parent
               --  We cannot free Cell.Children, which still points to
               --  Cell_Parent.Children (C)

               Tmp := Cell.Children;
               Cell.Children (Cell.Children'First).First_Char_Of_Key :=
                 Cell.First_Char_Of_Key;
               Cell.all := Cell.Children (Cell.Children'First);
               Free (Convert (Tmp));

            else
               Tree.Child := Cell.all;
            end if;
         end if;

      else
         Trace (Me, "Couldn't remove from Tree: " & Index & " scenario="
                & Scenario'Img);
      end if;
   end Remove;

   ---------
   -- Get --
   ---------

   function Get (Tree : Trie_Tree; Index : String) return Data_Type is
      Cell         : Cell_Child_Access;
      Cell_Parent  : Cell_Child_Access;
      Last         : Integer;
      Scenario     : Integer;
      Index_Length : Integer;
      First_Not_Matched : Character;
   begin
      Find_Cell_Child
        (Tree, Index, Cell, Cell_Parent, Last,
         Index_Length, Scenario, First_Not_Matched);

      if Scenario = 3 then
         return Cell.Data;
      end if;
      return No_Data;
   end Get;

   -----------
   -- Start --
   -----------

   function Start (Tree : Trie_Tree; Prefix : String) return Iterator is
      Cell         : Cell_Child_Access;
      Cell_Parent  : Cell_Child_Access;
      Last         : Integer;
      Scenario     : Integer;
      Index_Length : Integer;
      Iter         : Iterator;
      First_Not_Matched : Character;

      procedure Process_Recursively (Cell : Cell_Child);
      --  Add cell to the list of cells that need to be returned by the
      --  iterator.

      procedure Process_Recursively (Cell : Cell_Child) is
      begin
         if Cell.Children /= null then
            for C in Cell.Children'First .. Cell.Num_Children loop
               Process_Recursively (Cell.Children (C));
            end loop;
         end if;

         if Cell.Data /= No_Data then
            if Iter.Cells = null then
               Iter.Num_Cells := 10;
               Iter.Cells     := Convert (Alloc (10 * Data_Component_Size));
               Iter.Last      := 0;

            elsif Iter.Last = Iter.Num_Cells then
               Iter.Num_Cells := Iter.Num_Cells * 2;
               Iter.Cells     := Convert
                 (Realloc (Convert (Iter.Cells),
                           size_t (Iter.Num_Cells) * Data_Component_Size));
            end if;

            Iter.Last              := Iter.Last + 1;
            Iter.Cells (Iter.Last) := Cell.Data;
         end if;
      end Process_Recursively;

   begin
      Free (Iter);

      if Prefix = "" then
         Process_Recursively (Tree.Child);

      else
         --  Find the closest cell that matches the prefix
         Find_Cell_Child
           (Tree, Prefix, Cell, Cell_Parent, Last,
            Index_Length, Scenario, First_Not_Matched);

         --  From there, we need to return the cell and all of its children
         --  recursively
         if Cell.Index_Length >= Prefix'Length then
            Process_Recursively (Cell.all);
         end if;
      end if;

      return Iter;
   end Start;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Iterator) is
   begin
      if Iter.Cells /= null then
         Free (Convert (Iter.Cells));
         Iter.Cells := null;
      end if;
      Iter.Num_Cells := 0;
   end Free;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Iterator) is
   begin
      Iter.Current := Iter.Current + 1;
   end Next;

   ------------
   -- Length --
   ------------

   function Length (Iter : Iterator) return Natural is
   begin
      return Iter.Last - Iter.Current + 1;
   end Length;

   ---------
   -- Get --
   ---------

   function Get (Iter : Iterator) return Data_Type is
   begin
      if Iter.Cells /= null and then Iter.Current <= Iter.Last then
         return Iter.Cells (Iter.Current);
      else
         return No_Data;
      end if;
   end Get;

end Tries;
