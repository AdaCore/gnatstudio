------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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
with Language.Tree; use Language.Tree;
with Vector_Tries;

generic
   type Additional_Data_Type is private;
   --  This type is used to store additional data with a construct, in the
   --  trie tree.

   Null_Additional_Data_Type : Additional_Data_Type;
package Construct_Tries is

   type Construct_Node_Wrapper is record
      Node  : Construct_Tree_Iterator;
      Data  : Additional_Data_Type;
   end record;

   Null_Construct_Node_Wrapper : constant Construct_Node_Wrapper :=
     (Null_Construct_Tree_Iterator, Null_Additional_Data_Type);

   package Construct_Trie_Trees is new Vector_Tries
     (Construct_Node_Wrapper, Null_Construct_Node_Wrapper);

   subtype Construct_Trie is Construct_Trie_Trees.Vector_Trie;
   --  A construct trie holds construct iterators into a trie, based on their
   --  name. Several constructs can be stored under the same name.

   --  waiting for K120-007 resolution
   --  subtype Construct_Trie is
   --       new Construct_Trie_Trees.Vector_Trie (Case_Sensitive => True);

   Empty_Construct_Trie : Construct_Trie renames
     Construct_Trie_Trees.Empty_Vector_Trie;

   subtype Construct_Trie_Iterator is
     Construct_Trie_Trees.Vector_Trie_Iterator;
   --  This type gives a way of iterating over the contents of a construct
   --  trie.

   Null_Construct_Trie_Iterator : Construct_Trie_Iterator renames
     Construct_Trie_Trees.Null_Vector_Trie_Iterator;

   subtype Construct_Trie_Index is Construct_Trie_Trees.Vector_Trie_Index;
   --  This index can be used to store a position in a trie, in a persistent
   --  fashion. User is responsible to know if the object still exist or not.

   Null_Construct_Trie_Index : Construct_Trie_Index renames
     Construct_Trie_Trees.Null_Vector_Trie_Index;

   function Get_Construct_It
     (It : Construct_Trie_Iterator) return Construct_Tree_Iterator;
   --  Return the construct iterator contained in the cell pointed by the
   --  iterator.

   function Get_Additional_Data
     (It : Construct_Trie_Iterator) return Additional_Data_Type;
   --  Return the additional data contained in the cell pointed by the
   --  iterator.

   procedure Insert
     (Trie         : access Construct_Trie;
      Symbols      : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Construct_It : Construct_Tree_Iterator;
      Data         : Additional_Data_Type;
      Lang         : access Abstract_Tree_Language'Class;
      Index        : out Construct_Trie_Index);
   --  Insert an object into the trie tree, associated with a given Data. The
   --  returned index can be used to access / replace / remove this object
   --  afterwards.

   procedure Insert
     (Trie         : access Construct_Trie;
      Symbols      : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Construct_It : Construct_Tree_Iterator;
      Name         : String;
      Data         : Additional_Data_Type;
      Lang         : access Abstract_Tree_Language'Class;
      Index        : out Construct_Trie_Index);
   --  Same as above, but insert the object with a particular name instead
   --  of using the construct tree name.

   procedure Replace
     (Trie             : access Construct_Trie;
      Index            : Construct_Trie_Index;
      New_Construct_It : Construct_Tree_Iterator;
      New_Data         : Additional_Data_Type);
   --  Replace the object pointed by the iterator.

end Construct_Tries;
