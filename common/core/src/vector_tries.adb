------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;

package body Vector_Tries is

   function Get_Or_Create_List
     (Trie    : access Vector_Trie;
      Symbols : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Name    : String)
      return Data_List_Access;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Trie : in out Vector_Trie) is
   begin
      Clear (Vector_Trie_Trees.Trie_Tree (Trie));
   end Clear;

   -----------
   -- Start --
   -----------

   function Start
     (Trie : access Vector_Trie; Prefix : String; Is_Partial : Boolean)
      return Vector_Trie_Iterator
   is
      It : Vector_Trie_Iterator;
      Lower_Prefix : constant String := To_Lower (Prefix);
   begin
      It.Is_Partial := Is_Partial;
      It.It_Db := Start (Trie, To_Lower (Lower_Prefix));

      if not At_End (It.It_Db) then
         It.It_Vector := First (Get (It.It_Db).Data);

         --  ??? Should have Lower_Prefix as a symbol
         if not It.Is_Partial
           and then Get (Get (It.It_Db).Name).all /= Lower_Prefix
         then
            Free (It.It_Db);
            It.It_Db := Vector_Trie_Trees.Null_Iterator;
         end if;
      end if;

      if not Is_Valid (It) then
         Next (It);
      end if;

      return It;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Vector_Trie_Iterator) is
   begin
      loop
         if not At_End (It.It_Vector) then
            Next (It.It_Vector);
         else
            if It.Is_Partial then
               Next (It.It_Db);

               if not At_End (It.It_Db) then
                  It.It_Vector := First (Get (It.It_Db).Data);
               end if;
            else
               Free (It.It_Db);
               It.It_Db := Vector_Trie_Trees.Null_Iterator;
            end if;
         end if;

         exit when Is_Valid (It);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Vector_Trie_Iterator) return Boolean is
   begin
      return At_End (It.It_Db);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Vector_Trie_Iterator) return Boolean is
   begin
      return At_End (It) or else not At_End (It.It_Vector);
   end Is_Valid;

   ---------
   -- Get --
   ---------

   function Get
     (It : Vector_Trie_Iterator) return Data_Type
   is
   begin
      return Get (It.It_Vector);
   end Get;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (It : Vector_Trie_Iterator) return String is
   begin
      return Get_Index (It.It_Db);
   end Get_Index;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Vector_Trie_Iterator) is
   begin
      Free (It.It_Db);
   end Free;

   ------------------------
   -- Get_Or_Create_List --
   ------------------------

   function Get_Or_Create_List
     (Trie    : access Vector_Trie;
      Symbols : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Name    : String)
      return Data_List_Access
   is
      List  : Data_List_Access := Get (Trie, Name);
   begin
      --  We add only named constructs in the database, and we dismiss some
      --  categories.

      if List = null then
         List := new Data_List;
         List.Name := Symbols.Find (Name);
         List.Data :=
           new Data_Vector.Lazy_Vector_Record;
         Insert (Vector_Trie (Trie.all), List);
      end if;

      return List;
   end Get_Or_Create_List;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Trie    : access Vector_Trie;
      Symbols : access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Element : Data_Type;
      Name    : String;
      Index   : out Vector_Trie_Index)
   is

      --  ??? This To_Lower should be dependent on language casing.
      Lower_Name : constant String := To_Lower (Name);
      List       : constant Data_List_Access :=
        Get_Or_Create_List (Trie, Symbols, Lower_Name);
   begin
      Insert
        (List.Data,
         Element,
         Index.It);

      Index.Name := List.Name;
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Trie : in out Vector_Trie; Index : Vector_Trie_Index)
   is
      pragma Unreferenced (Trie);
   begin
      if Index.It /= Data_Vector.Null_Iterator then
         Delete (Index.It);
      end if;
   end Delete;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Trie      : access Vector_Trie;
      Index     : Vector_Trie_Index;
      New_Value : Data_Type)
   is
      pragma Unreferenced (Trie);
   begin
      Data_Vector.Set (Index.It, New_Value);
   end Replace;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Node : Data_List_Access)
      return GNATCOLL.Utils.Cst_String_Access is
   begin
      if Node /= null then
         return Get (Node.Name);
      else
         return null;
      end if;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Node : in out Data_List_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Data_List, Data_List_Access);
   begin
      if Node /= null then
         Free (Node.Data);
         Internal (Node);
      end if;
   end Free;

   --------------------
   -- Get_Name_Index --
   --------------------

   function Get_Name_Index
     (Trie : access Vector_Trie;
      Symbols : not null access Symbol_Table_Record'Class;
      Name : String) return Symbol
   is
      List   : constant Data_List_Access :=
        Get_Or_Create_List (Trie, Symbols, To_Lower (Name));
      --  ??? This To_Lower should depend on some casing property.
   begin
      return List.Name;
   end Get_Name_Index;

end Vector_Tries;
