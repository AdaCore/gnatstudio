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

with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Tries is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Cell_Child_Array, Cell_Child_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Cell_Child_Index_Array, Cell_Child_Index_Array_Access);

   procedure Free (Cell : in out Cell_Child);
   --  Free the memory used by Cell and its own children

   type Cell_Child_Access is access all Cell_Child;

   procedure Find_Cell_Child
     (Tree       : Cell_Child_Access;
      Index      : String;
      Cell       : out Cell_Child_Access;
      Last       : out Natural;
      Index_Last : out Natural;
      Scenario   : out Natural);
   --  Return the closest cell for Index. The first Last characters of Index
   --  have been processed so far on exit.  Kind describes the scenario, as
   --  in the following examples.
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
      Free (Cell.Index);
      Free (Cell.Data);

      if Cell.Children /= null then
         for C in Cell.Children'Range loop
            Free (Cell.Children (C));
         end loop;
         Unchecked_Free (Cell.Children);
      end if;
   end Free;

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
     (Tree       : Cell_Child_Access;
      Index      : String;
      Cell       : out Cell_Child_Access;
      Last       : out Natural;
      Index_Last : out Natural;
      Scenario   : out Natural)
   is
      pragma Suppress (All_Checks);
      Start : Integer;
      Ind   : String_Access;
   begin
      if Tree.Children = null then
         Cell     := Tree;
         Scenario := 5;
      else
         for C in Tree.Children'Range loop
            Ind := Tree.Children (C).Index;
            if Ind (Ind'First) = Index (Index'First) then
               if Ind'Length = 1 and then Index'Length = 1 then
                  Cell     := Tree.Children (C)'Unrestricted_Access;
                  Last     := Index'Last;
                  Scenario := 3;
                  return;
               end if;

               Start := Index'First + 1;
               for J in Ind'First + 1 .. Ind'Last loop
                  if Tree.Children (C).Index (J) /= Index (Start) then

                     --  If at least one character matched, this is the correct
                     --  cell, although it will have to be splitted
                     Cell       := Tree.Children (C)'Unrestricted_Access;
                     Last       := Start - 1;
                     Index_Last := J - 1;
                     Scenario   := 1;
                     return;
                  else
                     Start := Start + 1;

                     --  Cell matches, but will have to be splitted
                     if Start > Index'Last then
                        Cell       := Tree.Children (C)'Unrestricted_Access;
                        Last       := Start;

                        --  If all the characters of the index matched, we have
                        --  found our cell

                        if J = Tree.Children (C).Index'Last then
                           Scenario   := 3;
                        else
                           Index_Last := J;
                           Scenario   := 2;
                        end if;
                        return;
                     end if;
                  end if;
               end loop;

               --  If at least one character matched, but the index was too
               --  short, check the children

               Find_Cell_Child
                 (Tree.Children (C)'Unrestricted_Access,
                  Index (Start .. Index'Last),
                  Cell, Last, Index_Last, Scenario);
               return;
            end if;
         end loop;
      end if;

      Last := Index'First - 1;
      Cell := Tree;
      Scenario := 4;
   end Find_Cell_Child;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree  : in out Trie_Tree;
      Index : String;
      Data  : Data_Type)
   is
      pragma Suppress (All_Checks);
      Cell       : Cell_Child_Access;
      Last       : Integer;
      Scenario   : Integer;
      Index_Last : Integer;
      Tmp        : String_Access;
      Tmp2       : Cell_Child_Array_Access;
   begin
      Find_Cell_Child
        (Tree.Child'Unrestricted_Access, Index, Cell, Last,
         Index_Last, Scenario);

      case Scenario is
         when 1 =>
            Tmp := Cell.Index;
            Cell.all :=
              (Data    => No_Data,
               Index   => new String'
                 (Cell.Index (Cell.Index'First .. Index_Last)),
               Children => new Cell_Child_Array'
                 (1 => (Data  => Cell.Data,
                           Index => new String'
                             (Cell.Index (Index_Last + 1 .. Cell.Index'Last)),
                           Children => Cell.Children),
                  2 => (Data  => Data,
                           Index => new String'
                             (Index (Last + 1 .. Index'Last)),
                           Children => null)));
            Free (Tmp);

         when 2 =>
            Tmp := Cell.Index;
            Cell.all :=
              (Data    => Data,
               Index   => new String'
                 (Cell.Index (Cell.Index'First .. Index_Last)),
               Children => new Cell_Child_Array'
                 (1 => (Data => Cell.Data,
                        Index => new String'
                          (Cell.Index (Index_Last + 1 .. Cell.Index'Last)),
                        Children => Cell.Children)));
            Free (Tmp);

         when 3 =>
            Free (Cell.Data);
            Cell.Data := Data;

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
              (Data     => Data,
               Index    => new String'(Index (Last + 1 .. Index'Last)),
               Children => null);

         when others =>
            null;
      end case;
   end Insert;

   ----------
   -- Dump --
   ----------

   procedure Dump (Tree : Trie_Tree; Size_Only : Boolean := False) is
      Length : Natural := 0;

      procedure Dump (Cell : Cell_Child; Tab : String);
      --  Dump a cell

      procedure Dump (Cell : Cell_Child; Tab : String) is
      begin
         if not Size_Only then
            if Cell.Index = null then
               Put_Line (Tab & "Index=<null>  "  & Image (Cell.Data));
            else
               Put_Line (Tab & "Index=" & Cell.Index.all
                         & "  " & Image (Cell.Data));
            end if;
         end if;

         if Cell.Index /= null then
            Length := Length + Cell.Index'Length;
         end if;

         if Cell.Children /= null then
            Length := Length + Cell.Children.all'Size / 8;
            for C in Cell.Children'Range loop
               Dump (Cell.Children (C), Tab & "  ");
            end loop;
         end if;
      end Dump;
   begin
      Dump (Tree.Child, "");
      Put_Line ("Total length=" & Length'Img);
   end Dump;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Tree  : in out Trie_Tree;
      Index : String)
   is
      Cell       : Cell_Child_Access;
      Last       : Integer;
      Scenario   : Integer;
      Index_Last : Integer;
   begin
      Find_Cell_Child
        (Tree.Child'Unrestricted_Access, Index, Cell, Last,
         Index_Last, Scenario);

      --  For efficiency, we do not rebalance the tree, just leave it as it is
      if Scenario = 3 then
         Free (Cell.Data);
         Cell.Data := No_Data;
      end if;
   end Delete;

   ---------
   -- Get --
   ---------

   function Get (Tree : Trie_Tree; Index : String) return Data_Type is
      Cell       : Cell_Child_Access;
      Last       : Integer;
      Scenario   : Integer;
      Index_Last : Integer;
   begin
      Find_Cell_Child
        (Tree.Child'Unrestricted_Access, Index, Cell, Last,
         Index_Last, Scenario);

      if Scenario = 3 then
         return Cell.Data;
      end if;
      return No_Data;
   end Get;

   -----------
   -- Start --
   -----------

   function Start (Tree : Trie_Tree; Prefix : String) return Iterator is
      Cell       : Cell_Child_Access;
      Last       : Integer;
      Scenario   : Integer;
      Index_Last : Integer;
      Iter       : Iterator;

      procedure Process_Recursively (Cell : Cell_Child; Prefix : String);
      --  Add cell to the list of cells that need to be returned by the
      --  iterator.

      procedure Process_Recursively (Cell : Cell_Child; Prefix : String) is
         Tmp : Cell_Child_Index_Array_Access;
      begin
         if Cell.Data /= No_Data then
            if Iter.Cells = null or else Iter.Last = Iter.Cells'Last then
               Tmp := Iter.Cells;
               if Tmp /= null then
                  Iter.Cells :=
                    new Cell_Child_Index_Array (Tmp'First .. Tmp'Last * 2);
                  Iter.Cells (Tmp'Range) := Tmp.all;
                  Unchecked_Free (Tmp);
               else
                  Iter.Cells := new Cell_Child_Index_Array (1 .. 10);
                  Iter.Last  := 0;
               end if;
            end if;

            Iter.Last := Iter.Last + 1;
            Iter.Cells (Iter.Last) :=
              (new String'(Prefix & Cell.Index.all), Cell);
         end if;

         if Cell.Children /= null then
            for C in Cell.Children'Range loop
               if Cell.Index /= null then
                  Process_Recursively
                    (Cell.Children (C), Prefix & Cell.Index.all);
               else
                  Process_Recursively (Cell.Children (C), Prefix);
               end if;
            end loop;
         end if;
      end Process_Recursively;

   begin
      if Prefix = "" then
         Process_Recursively (Tree.Child, "");

      else
         --  Find the closest cell that matches the prefix
         Find_Cell_Child
           (Tree.Child'Unrestricted_Access, Prefix, Cell, Last,
            Index_Last, Scenario);

         --  From there, we need to return the cell and all of its children
         --  recursively
         if Scenario < 4 then
            Process_Recursively
              (Cell.all, Prefix (Prefix'First .. Last - Cell.Index'Length));
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
         for C in Iter.Cells'First .. Iter.Last loop
            Free (Iter.Cells (C).Index);
         end loop;
         Unchecked_Free (Iter.Cells);
      end if;
   end Free;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Iterator) is
   begin
      Iter.Current := Iter.Current + 1;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Iterator) return Data_Type is
   begin
      if Iter.Cells /= null and then Iter.Current <= Iter.Last then
         return Iter.Cells (Iter.Current).Cell.Data;
      else
         return No_Data;
      end if;
   end Get;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Iter : Iterator) return String is
   begin
      if Iter.Cells /= null and then Iter.Current <= Iter.Last then
         return Iter.Cells (Iter.Current).Index.all;
      else
         return "";
      end if;
   end Get_Key;

end Tries;
