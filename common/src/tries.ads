-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2006                      --
--                              AdaCore                              --
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

   type Trie_Tree_Access is access all Trie_Tree;

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

   function Get (Tree : access Trie_Tree; Index : String) return Data_Type;
   --  Return the data stored at a specific location in the tree

   type Iterator is private;

   function Start (Tree : access Trie_Tree; Prefix : String) return Iterator;
   --  The iterator will return all entries whose index starts with Prefix.
   --  The iterator must be freed by the caller.
   --  As a special case, if Prefix is the empty string, the whole contents
   --  of the table will be returned.

   function Length (Iter : Iterator) return Natural;
   --  Return the number of elements that remains to be returned by Iter,
   --  including the current one.

   procedure Next (Iter : in out Iterator);
   --  Move to the next entry

   function Get (Iter : Iterator) return Data_Type;
   --  Return the current entry or null if there are no more entries

   procedure Free (Iter : in out Iterator);
   --  Free the memory occupied by the iterator

   Null_Iterator : constant Iterator;

   generic
      with procedure Put (Str : String) is <>;
      with procedure Put (Data : Data_Type) is <>;
   procedure Dump (Tree : Trie_Tree);
   --  Dump function for debugging purposes.

   --  The functions below are lower-level versions of Get and Insert.
   --  They should be used with care.
   --  The idea is to avoid doing two search in the trie tree when checking
   --  whether an item is already there, and then inserting it in the tree.
   --  This will only work if the tree hasn't been modified since the return
   --  of Find_Cell_Child.

   type Cell_Pointer is private;

   procedure Find_Cell_Child
     (Tree : in out Trie_Tree; Index : String; Pointer : out Cell_Pointer);
   --  Access a specific cell in the tree. The result value should only be
   --  used before the next write-access to the tree, or it becomes obsolete.

   procedure Insert (Index : String; Pointer : Cell_Pointer; Data : Data_Type);
   --  Insert a new entry in the tree.

   function Get (Pointer : Cell_Pointer) return Data_Type;
   --  Return the data found in the pointed type

   procedure Remove (Tree : in out Trie_Tree; Pointer : Cell_Pointer);
   --  Return the data pointed to by Pointer

   Null_Cell_Pointer : constant Cell_Pointer;

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

      Num_Children : Natural := 0;
   end record;
   type Cell_Child_Access is access all Cell_Child;

   type Cell_Child_Array is array (Positive) of aliased Cell_Child;

   type Trie_Tree is record
      Child : aliased Cell_Child;
   end record;

   Empty_Trie_Tree : constant Trie_Tree :=
     (Child => (Index_Length => 0, First_Char_Of_Key => 'a',
                Data => No_Data, Children => null, Num_Children => 0));

   type Data_Type_Array is array (Positive) of Data_Type;
   type Data_Type_Array_Access is access Data_Type_Array;

   type Iterator is record
      Cells : Data_Type_Array_Access;
      --  All the cells that must be returned

      Num_Cells : Natural := 0;

      Last  : Integer := 0;
      --  The last relevant cell in Cells

      Current : Integer := 1;
   end record;

   Null_Iterator : constant Iterator := (null, 0, 0, 1);

   type Cell_Pointer is record
      Cell              : Cell_Child_Access;
      Cell_Parent       : Cell_Child_Access;
      Last              : Integer;
      Index_Length      : Integer;
      Scenario          : Short_Short_Integer;
      First_Not_Matched : Character;
   end record;

   Null_Cell_Pointer : constant Cell_Pointer :=
     (null, null, 0, 0, 0, ASCII.NUL);

end Tries;
