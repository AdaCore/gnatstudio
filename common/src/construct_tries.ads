-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007, AdaCore                    --
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

with GNAT.Strings;

with Language.Tree; use Language.Tree;
with Lazy_Vectors;
with Tries;

generic
   type Additional_Data_Type is private;
   --  This type is used to store additional data with a construct, in the
   --  trie tree.

   Null_Additional_Data_Type : Additional_Data_Type;
package Construct_Tries is

   type Construct_Trie is private;
   --  A construct trie holds construct iterators into a trie, based on their
   --  name. Several constructs can be stored under the same name.

   type Construct_Trie_Iterator is private;
   --  This type gives a way of iterating over the contents of a construct
   --  trie.

   Null_Construct_Trie_Iterator : constant Construct_Trie_Iterator;

   type Construct_Trie_Index is private;
   --  This index can be used to store a position in a trie, in a persistent
   --  fashion. User is responsible to know if the object still exist or not.

   Null_Construct_Trie_Index : constant Construct_Trie_Index;

   procedure Clear (Trie : access Construct_Trie);
   --  Remove all the entities from the construct trie.

   function Start
     (Trie : access Construct_Trie; Prefix : String; Is_Partial : Boolean)
      return Construct_Trie_Iterator;
   --  Start an iteration on the given prefix. If Is_Partial is true, then only
   --  names having the exact match will be returned. Note that this search is
   --  case sentitive.

   procedure Next (It : in out Construct_Trie_Iterator);
   --  Move the iterator to the next element.

   function At_End (It : Construct_Trie_Iterator) return Boolean;
   --  Return True if the iterator is after the last element to be iterated.

   function Is_Valid (It : Construct_Trie_Iterator) return Boolean;
   --  Return True if the iterator is in a valid state, either At_End or
   --  contains a valid value. This should be the case if the iteration is
   --  done in a single task, without modifications of the trie tree.

   function Get_Construct_It
     (It : Construct_Trie_Iterator) return Construct_Tree_Iterator;
   --  Return the construct iterator contained in the cell pointed by the
   --  iterator.

   function Get_Additional_Data
     (It : Construct_Trie_Iterator) return Additional_Data_Type;
   --  Return the additional data contained in the cell pointed by the
   --  iterator.

   function Get_Index (It : Construct_Trie_Iterator) return String;
   --  Return the index associated to this iterator, that is to say the index
   --  used in the trie tree.

   procedure Free (It : in out Construct_Trie_Iterator);
   --  Free the iterator. This has to be called on each iterator declared, in
   --  order to preven memory leaks.

   procedure Insert
     (Trie         : access Construct_Trie;
      Construct_It : Construct_Tree_Iterator;
      Data         : Additional_Data_Type;
      Lang         : access Tree_Language'Class;
      Index        : out Construct_Trie_Index);
   --  Insert an object into the trie tree, associated with a given Data. The
   --  returned index can be used to access / replace / remove this object
   --  afterwards.

   procedure Insert
     (Trie         : access Construct_Trie;
      Construct_It : Construct_Tree_Iterator;
      Name         : String;
      Data         : Additional_Data_Type;
      Lang         : access Tree_Language'Class;
      Index        : out Construct_Trie_Index);
   --  Same as above, but insert the object with a particular name instead
   --  of using the construct tree name.

   procedure Delete
     (Trie : access Construct_Trie; Index : Construct_Trie_Index);
   --  Delete the object stored at the index given in parameter.

   procedure Replace
     (Trie             : access Construct_Trie;
      Index            : Construct_Trie_Index;
      New_Construct_It : Construct_Tree_Iterator;
      New_Data         : Additional_Data_Type);
   --  Replace the object pointed by the iterator.

   function Get_Name_Index
     (Trie : access Construct_Trie; Name : String)
      return GNAT.Strings.String_Access;
   --  Return the unique string access corresponding to the name given in
   --  parameter. If none, will create one.

private

   type Construct_Node_Wrapper is record
      Node  : Construct_Tree_Iterator;
      Data  : Additional_Data_Type;
   end record;

   Null_Construct_Node_Wrapper : constant Construct_Node_Wrapper :=
     (Null_Construct_Tree_Iterator, Null_Additional_Data_Type);

   package Construct_Vector is new Lazy_Vectors
     (Construct_Node_Wrapper, Null_Construct_Node_Wrapper);

   use Construct_Vector;

   type Construct_Node_List is record
      Constructs : Construct_Vector.Lazy_Vector;
      Name       : GNAT.Strings.String_Access;
   end record;

   type Construct_Node_List_Access is access all Construct_Node_List;

   Null_Construct_Node_List : constant Construct_Node_List :=
     (Construct_Vector.Null_Lazy_Vector, null);

   function Get_Name
     (Node : Construct_Node_List_Access) return GNAT.Strings.String_Access;

   procedure Free (Node : in out Construct_Node_List_Access);
   --  Free the data associated to the node.

   package Construct_Trie_Trees is new Tries
     (Construct_Node_List_Access, null, Get_Name, Free);

   use Construct_Trie_Trees;

   type Construct_Trie is new Construct_Trie_Trees.Trie_Tree;

   type Construct_Trie_Iterator is record
      Is_Partial : Boolean;
      It_Vector  : Construct_Vector.Iterator;
      It_Db      : Construct_Trie_Trees.Iterator;
   end record;

   Null_Construct_Trie_Iterator : constant Construct_Trie_Iterator :=
     (False,
      Construct_Vector.Null_Iterator,
      Construct_Trie_Trees.Null_Iterator);

   type Construct_Trie_Index is record
      It   : Construct_Vector.Iterator;
      Name : GNAT.Strings.String_Access;
   end record;

   Null_Construct_Trie_Index : constant Construct_Trie_Index :=
     (Construct_Vector.Null_Iterator, null);

end Construct_Tries;
