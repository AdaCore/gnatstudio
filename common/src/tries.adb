-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2003                      --
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

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Traces; use Traces;
--  with GNAT.IO; use GNAT.IO;

package body Tries is

   Me : constant Debug_Handle := Create ("Tries");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Cell_Child_Array, Cell_Child_Array_Access);

   procedure Free (Cell : in out Cell_Child);
   --  Free the memory used by Cell and its own children

   function Get_Index (Cell : Cell_Child) return String_Access;
   --  Return the base index for the cell. Only the Cell.Index_Length first
   --  characters should be considered in the returned string.
   --  The returned string must not be freed.

   type Cell_Child_Access is access all Cell_Child;

   procedure Find_Cell_Child
     (Tree         : Cell_Child_Access;
      Index        : String;
      Cell         : out Cell_Child_Access;
      Cell_Parent  : in out Cell_Child_Access;
      Last         : out Natural;
      Index_Length : out Natural;
      Scenario     : out Natural);
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
         for C in Cell.Children'Range loop
            Free (Cell.Children (C));
         end loop;
         Unchecked_Free (Cell.Children);
      end if;
   end Free;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Cell : Cell_Child) return String_Access is
      C    : Cell_Child := Cell;
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

   ---------------------
   -- Find_Cell_Child --
   ---------------------

   procedure Find_Cell_Child
     (Tree         : Cell_Child_Access;
      Index        : String;
      Cell         : out Cell_Child_Access;
      Cell_Parent  : in out Cell_Child_Access;
      Last         : out Natural;
      Index_Length : out Natural;
      Scenario     : out Natural)
   is
      Start : Integer;
      Ind      : String_Access;
      Ind_First, Ind_Last : Natural;
   begin
      --  If we are processing the root node
      if Tree.Index_Length = 0 then
         Cell_Parent := null;
      end if;

      if Tree.Children = null then
         Cell        := Tree;
         Scenario    := 5;
      else
         Cell_Parent := Tree;

         for C in Tree.Children'Range loop
            Ind := Get_Index (Tree.Children (C));

            if Ind /= null then
               Ind_First := Ind'First + Tree.Index_Length;
               Ind_Last := Ind'First + Tree.Children (C).Index_Length - 1;
               Assert (Me, Ind_Last <= Ind'Last, "Too long Index_Length");

               if Ind (Ind_First) = Index (Index'First) then
                  if Ind_Last - Ind_First = 0 and then Index'Length = 1 then
                     Cell        := Tree.Children (C)'Unrestricted_Access;
                     Last        := Index'Last;
                     Scenario    := 3;
                     return;
                  end if;

                  if Index'Length = 1 then
                     Scenario := 2;
                     Index_Length := Index'Length;
                     Last     := Index'Last;
                     Cell     := Tree.Children (C)'Unrestricted_Access;
                     return;
                  end if;

                  Start := Index'First + 1;
                  for J in Ind_First + 1 .. Ind_Last loop
                     if Ind (J) /= Index (Start) then
                        --  If at least one character matched, this is the
                        --  correct cell, although it will have to be split
                        Cell := Tree.Children (C)'Unrestricted_Access;
                        Last := Start - 1;
                        Index_Length := J - Ind'First;
                        Scenario := 1;
                        return;
                     else
                        Start := Start + 1;

                        --  Cell matches, but will have to be splitted
                        if Start > Index'Last then
                           Cell := Tree.Children (C)'Unrestricted_Access;
                           Cell_Parent := Tree;
                           Last := Start;

                           --  If all the characters of the index matched,
                           --  we have found our cell

                           if J = Ind_Last then
                              Scenario   := 3;
                           else
                              Index_Length := J - Ind'First + 1;
                              Scenario     := 2;
                           end if;
                           return;
                        end if;
                     end if;
                  end loop;

                  --  If at least one character matched, but the index was
                  --  too short, check the children

                  Find_Cell_Child
                    (Tree.Children (C)'Unrestricted_Access,
                     Index (Start .. Index'Last),
                     Cell, Cell_Parent, Last, Index_Length, Scenario);
                  return;
               end if;
            end if;
         end loop;
      end if;

      Last := Index'First - 1;
      Cell := Tree;
      Scenario := 4;
   end Find_Cell_Child;

   ----------
   -- Dump --
   ----------

   procedure Dump (Tree : Trie_Tree) is
      procedure Dump (Cell : Cell_Child; Eliminate : Natural);
      --  Dump a cell

      procedure Dump (Cell : Cell_Child; Eliminate : Natural) is
         Ind : constant String_Access := Get_Index (Cell);
      begin
         if Ind'First + Cell.Index_Length - 1 > Ind'Last then
            Put ("('" & Ind (Ind'First + Eliminate .. Ind'Last)
                 & "(invalid length:" & Integer'Image (Cell.Index_Length)
                 & ")'");
         else
            Put ("('"
                 & Ind (Ind'First + Eliminate ..
                          Ind'First + Cell.Index_Length - 1) & "'");
         end if;

         Put (Cell.Data);
         Put (" ");

         if Cell.Children /= null then
            for C in Cell.Children'Range loop
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

--     procedure Put (Str : Data_Type);
--     procedure Put (Str : Data_Type) is
--        pragma Unreferenced (Str);
--     begin
--        null;
--     end Put;
--     procedure My_Dump is new Dump;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree  : in out Trie_Tree;
      Data  : Data_Type)
   is
      pragma Suppress (All_Checks);
      Cell       : Cell_Child_Access;
      Cell_Parent : Cell_Child_Access;
      Index      : constant String_Access := Get_Index (Data);
      Last       : Integer;
      Scenario   : Integer;
      Index_Length : Integer;
      Tmp2       : Cell_Child_Array_Access;
   begin
      Find_Cell_Child
        (Tree.Child'Unrestricted_Access, Index.all, Cell, Cell_Parent, Last,
         Index_Length, Scenario);

      case Scenario is
         when 1 =>
            Cell.all :=
              (Data         => No_Data,
               Index_Length => Index_Length,
               Children     => new Cell_Child_Array'
                 (1 => (Data         => Cell.Data,
                        Index_Length => Cell.Index_Length,
                        Children     => Cell.Children),
                  2 => (Data         => Data,
                        Index_Length => Index'Length,
                        Children     => null)));
            Assert
              (Me, Get_Index (Data)'Length >= Index'Length,
               "Invalid length in scenario 1");

         when 2 =>
            Cell.all :=
              (Data         => Data,
               Index_Length => Index_Length,
               Children     => new Cell_Child_Array'
                 (1 => (Data         => Cell.Data,
                        Index_Length => Cell.Index_Length,
                        Children     => Cell.Children)));
            Assert (Me, Get_Index (Data)'Length >= Index_Length,
                    "Invalid length in scenario 2 "
                    & Get_Index (Data)'Length'Img
                    & Index_Length'Img);

         when 3 =>
            Free (Cell.Data);
            Cell.Data := Data;
            Assert (Me, Get_Index (Cell.Data)'Length >= Cell.Index_Length,
                    "Invalid length in scenario 3");

         when 4 | 5 =>
            Tmp2 := Cell.Children;
            if Tmp2 /= null then
               Cell.Children := new Cell_Child_Array
                 (Tmp2'First .. Tmp2'Last + 1);
               Cell.Children (Tmp2'Range) := Tmp2.all;
               Unchecked_Free (Tmp2);
            else
               Cell.Children := new Cell_Child_Array (1 .. 1);
            end if;

            Cell.Children (Cell.Children'Last) :=
              (Data         => Data,
               Index_Length => Index'Length,
               Children     => null);
            Assert (Me, Get_Index (Data)'Length >= Index'Length,
                    "Invalid length in scenario 4");

         when others =>
            null;
      end case;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (Tree : in out Trie_Tree; Index : String) is
      Cell         : Cell_Child_Access;
      Cell_Parent  : Cell_Child_Access;
      Last         : Integer;
      Scenario     : Integer;
      Index_Length : Integer;
      Tmp          : Cell_Child_Array_Access;
   begin
      Find_Cell_Child
        (Tree.Child'Unrestricted_Access, Index, Cell, Cell_Parent, Last,
         Index_Length, Scenario);

      if Scenario = 2 or else Scenario = 3 then
         Free (Cell.Data);
         Cell.Data := No_Data;

         if Cell.Children = null then
            if Cell_Parent /= null then
               --  If there was one single child, ie the cell we are removing:
               if Cell_Parent.Children'Length = 1 then
                  Unchecked_Free (Cell_Parent.Children);

               --  If there were two children, and the current node has no
               --  data, we can simply remove it.
               elsif Cell_Parent.Children'Length = 2
                 and then Cell_Parent.Data = No_Data
               then
                  declare
                     Tmp : Cell_Child := Cell_Parent.all;
                  begin
                     if Cell_Parent.Children
                       (Cell_Parent.Children'First)'Unrestricted_Access = Cell
                     then
                        Cell_Parent.all := Cell_Parent.Children
                          (Cell_Parent.Children'Last);
                     else
                        Cell_Parent.all := Cell_Parent.Children
                          (Cell_Parent.Children'First);
                     end if;
                     Unchecked_Free (Tmp.Children);
                  end;

               else
                  for C in Cell_Parent.Children'Range loop
                     if Cell_Parent.Children (C)'Unrestricted_Access =
                       Cell
                     then
                        Tmp := Cell_Parent.Children;
                        Cell_Parent.Children := new Cell_Child_Array'
                          (Tmp (Tmp'First .. C - 1) & Tmp (C + 1 .. Tmp'Last));
                        Unchecked_Free (Tmp);
                        exit;
                     end if;
                  end loop;
               end if;
            end if;

            Free (Cell.all);

         elsif Cell.Children'Length = 1 then
            if Cell_Parent /= null then
               for C in Cell_Parent.Children'Range loop
                  if Cell_Parent.Children (C)'Unrestricted_Access = Cell then
                     Cell_Parent.Children (C) :=
                       Cell.Children (Cell.Children'First);
                     exit;
                  end if;
               end loop;
               Free (Cell.all);
            else
               Tree.Child := Cell.all;
            end if;

         elsif Cell_Parent /= null
           and then Cell_Parent.Children'Length = 1
         then
            Tmp := Cell_Parent.Children;
            Cell_Parent.Children := Cell.Children;
            Unchecked_Free (Tmp);
            Cell.Children := null;
            Free (Cell.all);
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
   begin
      Find_Cell_Child
        (Tree.Child'Unrestricted_Access, Index, Cell, Cell_Parent, Last,
         Index_Length, Scenario);

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

      procedure Process_Recursively (Cell : Cell_Child);
      --  Add cell to the list of cells that need to be returned by the
      --  iterator.

      procedure Process_Recursively (Cell : Cell_Child) is
         Tmp : Cell_Child_Array_Access;
      begin
         if Cell.Data /= No_Data then
            if Iter.Cells = null or else Iter.Last = Iter.Cells'Last then
               Tmp := Iter.Cells;
               if Tmp /= null then
                  Iter.Cells :=
                    new Cell_Child_Array (Tmp'First .. Tmp'Last * 2);
                  Iter.Cells (Tmp'Range) := Tmp.all;
                  Unchecked_Free (Tmp);
               else
                  Iter.Cells := new Cell_Child_Array (1 .. 10);
                  Iter.Last  := 0;
               end if;
            end if;

            Iter.Last := Iter.Last + 1;
            Iter.Cells (Iter.Last) := Cell;
         end if;

         if Cell.Children /= null then
            for C in Cell.Children'Range loop
               Process_Recursively (Cell.Children (C));
            end loop;
         end if;
      end Process_Recursively;

   begin
      if Prefix = "" then
         Process_Recursively (Tree.Child);

      else
         --  Find the closest cell that matches the prefix
         Find_Cell_Child
           (Tree.Child'Unrestricted_Access, Prefix, Cell, Cell_Parent, Last,
            Index_Length, Scenario);

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
      Unchecked_Free (Iter.Cells);
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
      return Iter.Cells'Last - Iter.Current + 1;
   end Length;

   ---------
   -- Get --
   ---------

   function Get (Iter : Iterator) return Data_Type is
   begin
      if Iter.Cells /= null and then Iter.Current <= Iter.Last then
         return Iter.Cells (Iter.Current).Data;
      else
         return No_Data;
      end if;
   end Get;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Iter : Iterator) return String is
      Ind : String_Access;
   begin
      if Iter.Cells /= null and then Iter.Current <= Iter.Last then
         Ind := Get_Index (Iter.Cells (Iter.Current));
         if Ind = null then
            return "";
         else
            return Ind (Ind'First .. Ind'First
                        + Iter.Cells (Iter.Current).Index_Length - 1);
         end if;
      else
         return "";
      end if;
   end Get_Key;

end Tries;
