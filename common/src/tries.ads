-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003                           --
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

--  This package provides an implementation of the "tries" data structure.
--  This is used to store and retrieve efficiently strings. Compared to
--  htables, it provides an easier answer to questions like "find all strings
--  starting with the given prefix".

with GNAT.OS_Lib;

generic
   type Data_Type is private;
   No_Data : Data_Type;

   with function Get_Index (Data : Data_Type) return GNAT.OS_Lib.String_Access;
   --  Return the index to use for Data. The returned string is not modified
   --  or stored by the Trie tree, but it is more efficient to return a
   --  String_Access than a String.
   --  This should return null for No_Data.

   with procedure Free (Data : in out Data_Type) is <>;
   --  Free the memory occupied by Data. No_Data might be passed as an argument

package Tries is

   type Trie_Tree is private;
   Empty_Trie_Tree : constant Trie_Tree;

   procedure Clear (Tree : in out Trie_Tree);
   --  Clear the full contents of the tree

   procedure Insert
     (Tree  : in out Trie_Tree;
      Data  : Data_Type);
   --  Insert a new entry in the tree.
   --  Index mustn't be the empty string.

   procedure Remove (Tree : in out Trie_Tree; Index : String);
   --  Remove an entry from the tree.

   function Get (Tree : Trie_Tree; Index : String) return Data_Type;
   --  Return the data stored at a specific location in the tree

   type Iterator is private;

   function Start (Tree : Trie_Tree; Prefix : String) return Iterator;
   --  The iterator will return all entries whose index starts with Prefix.
   --  The iterator must be freed by the caller.
   --  As a special case, if Prefix is the empty string, the whole contents
   --  of the table will be returned.

   function Length (Iter : Iterator) return Natural;
   --  Return the number of elements that remains to be returned by Iter,
   --  including the current one.

   procedure Next (Iter : in out Iterator);
   --  Move to the next entry

   function Get     (Iter : Iterator) return Data_Type;
   function Get_Key (Iter : Iterator) return String;
   --  Return the current entry or null if there are no more entries

   procedure Free (Iter : in out Iterator);
   --  Free the memory occupied by the iterator

   generic
      with procedure Put (Str : String) is <>;
      with procedure Put (Data : Data_Type) is <>;
   procedure Dump (Tree : Trie_Tree);
   --  Dump function for debugging purposes.

private
   --  The structure of the tree is the following: this is n-ary tree. Each
   --  cell in the tree matches a substring of the index.
   --
   --      root  -- "t"  (cell for t)  --  "ree"  (cell for tree)
   --                                  --  "he"   (cell for the)
   --            -- "structure" (cell for structure)
   --
   --  Not two children of a cell have the same prefix, so one can simply
   --  check the first letter of the string to see if it matches

   type Cell_Child_Array;
   type Cell_Child_Array_Access is access Cell_Child_Array;
   --  Children of a cell are stored in an array. This is more costly when
   --  building the tree, but consumes less space and is faster to traverse,
   --  which is the emphasis for this tree.

   type Cell_Child is record
      First_Char_Of_Key : Character;

      Index_Length : Natural := 0;
      --  The number of characters that should be considered in
      --  Get_Index (Data) for this node.

      Data : Data_Type := No_Data;
      --  If there is any data associated with that cell

      Children : Cell_Child_Array_Access;
      --  The various children of the cell
   end record;

   type Cell_Child_Array is array (Natural range <>) of Cell_Child;

   type Trie_Tree is record
      Child : Cell_Child;
   end record;

   Empty_Trie_Tree : constant Trie_Tree :=
     (Child => (Index_Length => 0, First_Char_Of_Key => 'a',
                Data => No_Data, Children => null));

   type Iterator is record
      Cells : Cell_Child_Array_Access;
      --  All the cells that must be returned

      Last  : Integer := 0;
      --  The last relevant cell in Cells

      Current : Integer := 1;
   end record;

end Tries;
