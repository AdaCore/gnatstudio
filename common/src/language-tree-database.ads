-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                           --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides an efficient database suitable to record the relevant
--  construct information of an entire project

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Multisets;

with Lazy_Vectors;
with Tries;
with VFS;   use VFS;

package Language.Tree.Database is

   type Structured_File is private;
   --  This type reprensents a file with a structured view of its contents

   type Structured_File_Access is access all Structured_File;

   procedure Free (File : in out Structured_File_Access);
   --  Free the data associated to File.

   function Get_Full_Tree
     (File : Structured_File_Access) return Construct_Tree;
   --  Return the complete tree corresponding to this file. It the tree is not
   --  cached, then it will be computed.

   function Get_Public_Tree
     (File : Structured_File_Access) return Construct_Tree;
   --  Return the public tree corresponding to this file. The public tree is
   --  always in memory.

   function Get_Parent_File
     (File : Structured_File_Access) return Structured_File_Access;
   --  Return the parent file of the given file. The actual definition of
   --  "parent file" is langage-specific, and has to be implemented in the
   --  corresponding primitives.

   function Get_Buffer (File : Structured_File_Access) return String_Access;
   --  Return the buffer associated to this file note. Once the request is
   --  done, the buffer is stored in a cache so that the same buffer can be
   --  returned if this function is queried twice. The returned value must
   --  not be freed by the user.

   function Get_File_Path (File : Structured_File_Access) return Virtual_File;
   --  Return the file path of the given file node.

   procedure Update_Contents (File : Structured_File_Access);
   --  This function will re-analyze the full contents of the file

   type Construct_Database is private;
   --  A construct database is a data structure containing a file index and a
   --  construct index, both sorted alphabetically.

   type Construct_Database_Access is access all Construct_Database;

   procedure Free (This : in out Construct_Database_Access);
   --  Free the data associated to this database.

   function Get_Or_Create
     (Db   : Construct_Database_Access;
      File : Virtual_File;
      Lang : Tree_Language_Access) return Structured_File_Access;
   --  Return the file node corresponding to the given file path, and create
   --  one if needed. The creation of the file implies the addition of all its
   --  contents.

   procedure Update_Contents
     (Db : access Construct_Database; File : Virtual_File);
   --  Reload the file and its constructs. Previous constructs are removed from
   --  the database.

   procedure Clear (Db : access Construct_Database);
   --  Remove the contents of the database. New contents can still be added
   --  after this operation.

   type Construct_Db_Iterator is private;
   --  A construct db iterator is an iterator able to go over the various
   --  constructs stored in a database.

   function Start
     (Db : access Construct_Database; Prefix : String; Is_Partial : Boolean)
      return Construct_Db_Iterator;
   --  Return a pointer initialized on the begining of the construct database,
   --  contained to the prefix given in parameter. If Is_Partial is True, then
   --  only the entries matching exactly the prefix will be returned.

   function Get_Construct
     (It : Construct_Db_Iterator) return Construct_Tree_Iterator;
   --  Return an iterator pointing to the construct stored by the current
   --  database iterator.

   function Get_Current_Id (It : Construct_Db_Iterator) return String;
   --  Return the current identifier of the entity pointed by the iterator. The
   --  identifier is the one that has been recorded in the database, not the
   --  actual construct one.

   function Get_File
     (It : Construct_Db_Iterator) return Structured_File_Access;
   --  Return the file containing the construct pointed by the iterator given
   --  in parameter.

   procedure Next (It : in out Construct_Db_Iterator);
   --  Moves to the next entry corresponding to the query

   function At_End (It : Construct_Db_Iterator) return Boolean;
   --  Return true if the iterator is at the end of the buffer, false otherwise

   function Is_Valid (It : Construct_Db_Iterator) return Boolean;
   --  Return true if the iterator is in a usable state. If not, next has to be
   --  called in order to put it back in such a state

   procedure Free (It : in out Construct_Db_Iterator);
   --  Free the data associated to this iterator.

   Null_Construct_Db_Iterator : constant Construct_Db_Iterator;

   function "<" (Left, Right : Structured_File_Access) return Boolean;
   --  Needed by the file set package

   package File_Set is new Ada.Containers.Ordered_Multisets
     (Structured_File_Access);
   --  This package is used to index the file by their unit name

   use File_Set;

   function Start_File_Search (Db : Construct_Database) return File_Set.Cursor;
   --  Return a cursor pointing at the first element of the file database.

   function Is_In_Parents
     (Parent, Child : Structured_File_Access) return Boolean;
   --  Return true if Parent is one of the parents of Child (including the
   --  child itself).

private

   type Construct_Node_Wrapper is record
      Index : Natural;
      File  : Structured_File_Access;
      Node  : Construct_Tree_Node;
   end record;

   Null_Construct_Node_Wrapper : constant Construct_Node_Wrapper :=
     (0, null, Null_Construct_Tree_Node);

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

   type Construct_Db_Data is record
      Position : Construct_Vector.Iterator := Construct_Vector.Null_Iterator;
   end record;

   type Construct_Db_Data_Array is array
     (Natural range <>) of Construct_Db_Data;

   type Construct_Db_Data_Access is access all Construct_Db_Data_Array;

   procedure Free (This : in out  Construct_Db_Data_Access);

   type Structured_File is record
      File        : Virtual_File;
      Parent_File : Structured_File_Access;
      Lang        : Tree_Language_Access;

      Public_Tree  : Construct_Tree;
      Db_Data_Tree : Construct_Db_Data_Access;

      Cache_Tree   : Construct_Tree;
      Cache_Buffer : String_Access;

      Db           : access Construct_Database;
   end record;

   function "=" (Left, Right : Structured_File) return Boolean;

   package File_Map is new Ada.Containers.Ordered_Maps
     (Virtual_File, Structured_File_Access);

   use File_Map;

   type Construct_Database is record
      Files_Db        : File_Map.Map;
      Sorted_Files_Db : File_Set.Set;
      Entities_Db     : aliased Construct_Trie_Trees.Trie_Tree;
   end record;

   type Construct_Db_Iterator is record
      Is_Partial : Boolean;
      It_Vector  : Construct_Vector.Iterator;
      It_Db      : Construct_Trie_Trees.Iterator;
   end record;

   Null_Construct_Db_Iterator : constant Construct_Db_Iterator :=
     (Is_Partial => False,
      It_Vector  => Construct_Vector.Null_Iterator,
      It_Db      => Construct_Trie_Trees.Null_Iterator);

end Language.Tree.Database;
