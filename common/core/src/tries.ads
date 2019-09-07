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

--  This package provides an implementation of the "tries" data structure.
--  This is used to store and retrieve efficiently strings. Compared to
--  htables, it provides an easier answer to questions like "find all strings
--  starting with the given prefix".

with GNAT.Strings;
with GNATCOLL.Utils;  use GNATCOLL.Utils;

generic
   type Data_Type is private;
   No_Data : Data_Type;

   with function Get_Index (Data : Data_Type) return Cst_String_Access;
   --  Return the index to use for Data. The returned string is not modified
   --  or stored by the Trie tree, but it is more efficient to return a
   --  String_Access than a String.
   --  This should return null for No_Data.

   with procedure Free (Data : in out Data_Type) is <>;
   --  Free the memory occupied by Data. No_Data might be passed as an argument

package Tries is

   type Trie_Tree (Case_Sensitive : Boolean) is private;
   --  The case sensitive cannot be changed once the trie tree has been created

   type Trie_Tree_Access is access all Trie_Tree;

   Empty_Case_Sensitive_Trie_Tree : constant Trie_Tree;
   Empty_Case_Insensitive_Trie_Tree : constant Trie_Tree;

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

   function At_End (Iter : Iterator) return Boolean;
   --  Return True if the iterator is at the end of the search.

   procedure Next (Iter : in out Iterator);
   --  Move to the next entry

   function Get (Iter : Iterator) return Data_Type;
   --  Return the current entry or null if there are no more entries. Note that
   --  the iterator might be invalidated here, that's why it's in / out
   --  parameter.

   function Get_Index (Iter : Iterator) return String;
   --  Return the index of the current iterator

   function Is_Valid (Iter : Iterator) return Boolean;
   --  Return true if the iterator is meant to be used by the user (including
   --  the case where it is at the end).

   procedure Free (Iter : in out Iterator);
   --  Free the memory associated to an iterator.

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

   type Cell_Child;

   type Cell_Child_Access is access all Cell_Child;

   type Cell_Child is record
      First_Char_Of_Key : Character := 'a';

      Index_Length      : Natural := 0;
      --  The number of characters that should be considered in
      --  Get_Index (Data) for this node.

      Data              : Data_Type := No_Data;
      --  If there is any data associated with that cell

      Children          : Cell_Child_Array_Access;
      --  The various children of the cell

      Num_Children      : Natural := 0;

      Children_Length   : Natural := 0;

      Parent_Cell       : Cell_Child_Access;

      Number_In_Parent  : Natural := 0;
   end record;

   type Cell_Child_Array is array (Positive) of aliased Cell_Child;

   type Mod_Counter is mod Integer'Last;
   --  This type is made modal in order to avoid crashing when it reaches its
   --  last values. This is almost save, as we use it to determine if there are
   --  been any chances between two iterations on the iterator.

   type Mod_Access is access all Mod_Counter;

   type Trie_Tree (Case_Sensitive : Boolean) is record
      Mod_Clock : Mod_Access;
      --  This value is incremented each time a modification is done in the
      --  trie tree. This way, if there is any change made during e.g. the
      --  iteration, it's detected.

      Child     : Cell_Child_Access := null;
   end record;

   Empty_Case_Sensitive_Trie_Tree : constant Trie_Tree :=
     (Case_Sensitive => True,
      Mod_Clock => null,
      Child     => null);
   Empty_Case_Insensitive_Trie_Tree : constant Trie_Tree :=
     (Case_Sensitive => False,
      Mod_Clock => null,
      Child     => null);

   type Data_Type_Array is array (Positive) of Data_Type;

   type Data_Type_Array_Access is access Data_Type_Array;

   type Iterator is record
      Trie_Root_Cell    : Cell_Child_Access;
      --  This is the cell at the root of the whole trie tree.

      Case_Sensitive    : Boolean;
      --  A property of the tree, but needed to traverse

      Mod_Clock         : Mod_Access;
      Initial_Timestamp : Mod_Counter;

      Root_Cell : Cell_Child_Access;
      Root_Name : GNAT.Strings.String_Access;

      Current_Cell        : Cell_Child_Access;
      Current_Name        : GNAT.Strings.String_Access;
      Current_Name_Length : Natural;
      --  This is only the variable part of the current name. To get the actual
      --  full name, it has to be concatened with Root_Name. Moreover, in order
      --  to minimize memory allocation, we never change it to a smaller
      --  string. Therefore, the actual size of the current variable part is
      --  recorded in Current_Name_Length.

      Current_Index : Natural;
   end record;

   Null_Iterator : constant Iterator :=
     (null, True, null, 0, null, null, null, null, 0, 0);

   type Cell_Pointer is record
      Cell              : Cell_Child_Access;
      Cell_Parent       : Cell_Child_Access;
      Last              : Integer;
      Index_Length      : Integer;
      Scenario          : Short_Short_Integer;
      --  ??? Need documentation on possible values for Scenario, and
      --  preferably replace Short_Short_Integer by an enumerated type.
      First_Not_Matched : Character;
   end record;

   Null_Cell_Pointer : constant Cell_Pointer :=
     (null, null, 0, 0, 0, ASCII.NUL);

end Tries;
