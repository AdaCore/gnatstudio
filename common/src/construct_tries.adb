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

with Ada.Unchecked_Deallocation;
with Language; use Language;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Construct_Tries is

   use GNAT.Strings;

   function Get_Or_Create_List
     (Trie : access Construct_Trie; Name : String)
      return Construct_Node_List_Access;

   -----------
   -- Clear --
   -----------

   procedure Clear (Trie : access Construct_Trie) is
   begin
      Clear (Construct_Trie_Trees.Trie_Tree (Trie.all));
   end Clear;

   -----------
   -- Start --
   -----------

   function Start
     (Trie : access Construct_Trie; Prefix : String; Is_Partial : Boolean)
      return Construct_Trie_Iterator
   is
      It : Construct_Trie_Iterator;
      Lower_Prefix : constant String := To_Lower (Prefix);
   begin
      It.Is_Partial := Is_Partial;
      It.It_Db := Start (Trie, To_Lower (Lower_Prefix));

      if not At_End (It.It_Db) then
         It.It_Vector := First (Get (It.It_Db).Constructs);

         if not It.Is_Partial
           and then Get (It.It_Db).Name.all /= Lower_Prefix
         then
            Free (It.It_Db);
            It.It_Db := Construct_Trie_Trees.Null_Iterator;
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

   procedure Next (It : in out Construct_Trie_Iterator) is
   begin
      loop
         if not At_End (It.It_Vector) then
            Next (It.It_Vector);
         else
            if It.Is_Partial then
               Next (It.It_Db);

               if not At_End (It.It_Db) then
                  It.It_Vector := First (Get (It.It_Db).Constructs);
               end if;
            else
               Free (It.It_Db);
               It.It_Db := Construct_Trie_Trees.Null_Iterator;
            end if;
         end if;

         exit when Is_Valid (It);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Construct_Trie_Iterator) return Boolean is
   begin
      return At_End (It.It_Db);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Construct_Trie_Iterator) return Boolean is
   begin
      return At_End (It) or else not At_End (It.It_Vector);
   end Is_Valid;

   ----------------------
   -- Get_Construct_It --
   ----------------------

   function Get_Construct_It
     (It : Construct_Trie_Iterator) return Construct_Tree_Iterator
   is
   begin
      return Get (It.It_Vector).Node;
   end Get_Construct_It;

   -------------------------
   -- Get_Additional_Data --
   -------------------------

   function Get_Additional_Data
     (It : Construct_Trie_Iterator) return Additional_Data_Type
   is
   begin
      return Get (It.It_Vector).Data;
   end Get_Additional_Data;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (It : Construct_Trie_Iterator) return String is
   begin
      return Get_Index (It.It_Db);
   end Get_Index;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Construct_Trie_Iterator) is
   begin
      Free (It.It_Db);
   end Free;

   ------------------------
   -- Get_Or_Create_List --
   ------------------------

   function Get_Or_Create_List
     (Trie : access Construct_Trie; Name : String)
      return Construct_Node_List_Access
   is
      List  : Construct_Node_List_Access := Get (Trie, Name);
   begin
      --  We add only named constructs in the database, and we dismiss some
      --  categories.

      if List = null then
         List := new Construct_Node_List;
         List.Name := new String'(Name);
         List.Constructs :=
           new Construct_Vector.Lazy_Vector_Record;
         Insert (Construct_Trie (Trie.all), List);
      end if;

      return List;
   end Get_Or_Create_List;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Trie         : access Construct_Trie;
      Construct_It : Construct_Tree_Iterator;
      Data         : Additional_Data_Type;
      Lang         : access Tree_Language'Class;
      Index        : out Construct_Trie_Index) is
   begin
      Insert
        (Trie,
         Construct_It,
         Get_Name_Index (Lang, Get_Construct (Construct_It).all),
         Data,
         Lang,
         Index);
   end Insert;

   procedure Insert
     (Trie         : access Construct_Trie;
      Construct_It : Construct_Tree_Iterator;
      Name         : String;
      Data         : Additional_Data_Type;
      Lang         : access Tree_Language'Class;
      Index        : out Construct_Trie_Index)
   is
      pragma Unreferenced (Lang);

      Wrapper : Construct_Node_Wrapper;
      --  ??? This To_Lower should be dependent on language casing.
      Lower_Name : constant String := To_Lower (Name);
      List       : constant Construct_Node_List_Access :=
        Get_Or_Create_List (Trie, Lower_Name);
   begin
      Wrapper.Node := Construct_It;
      Wrapper.Data := Data;

      Insert
        (List.Constructs,
         Wrapper,
         Index.It);

      Index.Name := List.Name;
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Trie : access Construct_Trie; Index : Construct_Trie_Index)
   is
      pragma Unreferenced (Trie);
   begin
      if Index.It /= Construct_Vector.Null_Iterator then
         Delete (Index.It);
      end if;
   end Delete;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Trie             : access Construct_Trie;
      Index            : Construct_Trie_Index;
      New_Construct_It : Construct_Tree_Iterator;
      New_Data         : Additional_Data_Type)
   is
      pragma Unreferenced (Trie);

      Wrapper   : Construct_Node_Wrapper;
   begin
      Wrapper.Node := New_Construct_It;
      Wrapper.Data := New_Data;

      Construct_Vector.Set (Index.It, Wrapper);
   end Replace;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Node : Construct_Node_List_Access) return GNAT.Strings.String_Access is
   begin
      if Node /= null then
         return Node.Name;
      else
         return null;
      end if;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Node : in out Construct_Node_List_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Construct_Node_List, Construct_Node_List_Access);
   begin
      if Node /= null then
         Free (Node.Name);

         Free (Node.Constructs);
         Internal (Node);
      end if;
   end Free;

   --------------------
   -- Get_Name_Index --
   --------------------

   function Get_Name_Index
     (Trie : access Construct_Trie; Name : String) return String_Access
   is
      List   : constant Construct_Node_List_Access :=
        Get_Or_Create_List (Trie, To_Lower (Name));
      --  ??? This To_Lower should depend on some casing property.
   begin
      return List.Name;
   end Get_Name_Index;

end Construct_Tries;
