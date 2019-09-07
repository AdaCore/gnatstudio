------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with GNATCOLL.Symbols;
with GNATCOLL.Utils;
with Lazy_Vectors;
with Tries;

generic
   type Data_Type is private;
   --  Data type that will be stored in the tree

   No_Data : Data_Type;
   --  Null value for the data in the three

package Vector_Tries is

   type Vector_Trie is private;
   --  This type is a container storing elements by their name. Several
   --  elements of the same name can be store at the same time.

   --  waiting for K120-007 resolution
   --  type Vector_Trie (Case_Sensitive : Boolean) is private;

   Empty_Vector_Trie : constant Vector_Trie;

   type Vector_Trie_Iterator is private;
   --  This type gives a way of iterating over the contents of a vector trie.

   Null_Vector_Trie_Iterator : constant Vector_Trie_Iterator;

   type Vector_Trie_Index is private;
   --  This index can be used to store a position in a trie, in a persistent
   --  fashion. User is responsible to know if the object still exist or not.

   Null_Vector_Trie_Index : constant Vector_Trie_Index;

   procedure Clear (Trie : in out Vector_Trie);
   --  Remove all the entities from the vector trie.

   function Start
     (Trie : access Vector_Trie; Prefix : String; Is_Partial : Boolean)
      return Vector_Trie_Iterator;
   --  Start an iteration on the given prefix. If Is_Partial is true, then only
   --  names having the exact match will be returned. Note that this search is
   --  case sentitive.

   procedure Next (It : in out Vector_Trie_Iterator);
   --  Move the iterator to the next element.

   function At_End (It : Vector_Trie_Iterator) return Boolean;
   --  Return True if the iterator is after the last element to be iterated.

   function Is_Valid (It : Vector_Trie_Iterator) return Boolean;
   --  Return True if the iterator is in a valid state, either At_End or
   --  contains a valid value. This should be the case if the iteration is
   --  done in a single task, without modifications of the trie tree.

   function Get
     (It : Vector_Trie_Iterator) return Data_Type;
   --  Return the data contained in the cell pointed by the iterator.

   function Get_Index (It : Vector_Trie_Iterator) return String;
   --  Return the index associated to this iterator, that is to say the index
   --  used in the trie tree.

   procedure Free (It : in out Vector_Trie_Iterator);
   --  Free the iterator. This has to be called on each iterator declared, in
   --  order to preven memory leaks.

   procedure Insert
     (Trie         : access Vector_Trie;
      Symbols      : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Element      : Data_Type;
      Name         : String;
      Index        : out Vector_Trie_Index);
   --  Insert an object into the trie tree, associated with a given Data. The
   --  returned index can be used to access / replace / remove this object
   --  afterwards.

   procedure Delete
     (Trie : in out Vector_Trie; Index : Vector_Trie_Index);
   --  Delete the object stored at the index given in parameter.

   procedure Replace
     (Trie             : access Vector_Trie;
      Index            : Vector_Trie_Index;
      New_Value        : Data_Type);
   --  Replace the object pointed by the iterator.

   function Get_Name_Index
     (Trie    : access Vector_Trie;
      Symbols : not null access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Name    : String)
      return GNATCOLL.Symbols.Symbol;
   --  Return the unique string access corresponding to the name given in
   --  parameter. If none, will create one.

private

   package Data_Vector is new Lazy_Vectors (Data_Type, No_Data);

   use Data_Vector;

   type Data_List is record
      Data : Data_Vector.Lazy_Vector;
      Name : GNATCOLL.Symbols.Symbol;
   end record;

   type Data_List_Access is access all Data_List;

   Null_Vector_Node_List : constant Data_List :=
     (Data_Vector.Null_Lazy_Vector, GNATCOLL.Symbols.No_Symbol);

   function Get_Name
     (Node : Data_List_Access)
      return GNATCOLL.Utils.Cst_String_Access;

   procedure Free (Node : in out Data_List_Access);

   package Vector_Trie_Trees is new Tries
     (Data_List_Access, null, Get_Name, Free);

   use Vector_Trie_Trees;

   type Vector_Trie is
     new Vector_Trie_Trees.Trie_Tree (Case_Sensitive => True);

   --  waiting for K120-007 resolution
   --     type Vector_Trie is
   --       new Vector_Trie_Trees.Trie_Tree (Case_Sensitive => Case_Sensitive);

   Empty_Vector_Trie : constant Vector_Trie :=
     Vector_Trie (Vector_Trie_Trees.Empty_Case_Sensitive_Trie_Tree);

   type Vector_Trie_Iterator is record
      Is_Partial : Boolean;
      It_Vector  : Data_Vector.Iterator;
      It_Db      : Vector_Trie_Trees.Iterator;
   end record;

   Null_Vector_Trie_Iterator : constant Vector_Trie_Iterator :=
     (False,
      Data_Vector.Null_Iterator,
      Vector_Trie_Trees.Null_Iterator);

   type Vector_Trie_Index is record
      It   : Data_Vector.Iterator;
      Name : GNATCOLL.Symbols.Symbol;
   end record;

   Null_Vector_Trie_Index : constant Vector_Trie_Index :=
     (Data_Vector.Null_Iterator, GNATCOLL.Symbols.No_Symbol);

end Vector_Tries;
