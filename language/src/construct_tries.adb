------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Language; use Language;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;

package body Construct_Tries is

   use Construct_Trie_Trees;

   ----------------------
   -- Get_Construct_It --
   ----------------------

   function Get_Construct_It
     (It : Construct_Trie_Iterator) return Construct_Tree_Iterator
   is
   begin
      return Get (It).Node;
   end Get_Construct_It;

   -------------------------
   -- Get_Additional_Data --
   -------------------------

   function Get_Additional_Data
     (It : Construct_Trie_Iterator) return Additional_Data_Type
   is
   begin
      return Get (It).Data;
   end Get_Additional_Data;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Trie         : access Construct_Trie;
      Symbols      : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Construct_It : Construct_Tree_Iterator;
      Data         : Additional_Data_Type;
      Lang         : access Abstract_Tree_Language'Class;
      Index        : out Construct_Trie_Index) is
   begin
      Insert
        (Trie,
         Symbols,
         Construct_It,
         Get (Get_Name_Index (Lang, Get_Construct (Construct_It).all)).all,
         Data,
         Lang,
         Index);
   end Insert;

   procedure Insert
     (Trie         : access Construct_Trie;
      Symbols      : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Construct_It : Construct_Tree_Iterator;
      Name         : String;
      Data         : Additional_Data_Type;
      Lang         : access Abstract_Tree_Language'Class;
      Index        : out Construct_Trie_Index)
   is
      pragma Unreferenced (Lang);

      Wrapper : Construct_Node_Wrapper;
   begin
      Wrapper.Node := Construct_It;
      Wrapper.Data := Data;

      Insert (Trie, Symbols, Wrapper, Name, Index);
   end Insert;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Trie             : access Construct_Trie;
      Index            : Construct_Trie_Index;
      New_Construct_It : Construct_Tree_Iterator;
      New_Data         : Additional_Data_Type)
   is
      Wrapper   : Construct_Node_Wrapper;
   begin
      Wrapper.Node := New_Construct_It;
      Wrapper.Data := New_Data;

      Replace (Trie, Index, Wrapper);
   end Replace;

end Construct_Tries;
