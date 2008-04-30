-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

with GNAT.Strings; use GNAT.Strings;

with Construct_Tries;
with GNATCOLL.VFS;   use GNATCOLL.VFS;

package Language.Tree.Database is

   ---------------------
   -- Buffer_Provider --
   ---------------------

   type Buffer_Provider is abstract tagged private;
   --  A buffer provider is used to change the way the database is getting the
   --  buffer for a given file. By default, it loads the file from the disk,
   --  but we might need to give some other sources, for example an open
   --  editor.

   type Buffer_Provider_Access is access all Buffer_Provider'Class;

   function Get_Buffer
     (Provider : access Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return String_Access is abstract;
   --  Return the buffer corresponding to this buffer provider. The returned
   --  access type is supposed to be a copy of the actual buffer - it will be
   --  freed by the completion engine when needed.
   --  ??? What is the encoding of the returned data?

   procedure Free (This : in out Buffer_Provider_Access);
   --  Free the access type

   type File_Buffer_Provider is new Buffer_Provider with private;

   function Get_Buffer
     (Provider : access File_Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return String_Access;
   --  Return the buffer corresponding to the file given in parameter - by
   --  default just read the file.
   --  ??? What is the encoding of the returned data?

   ---------------------
   -- Structured_File --
   ---------------------

   type Structured_File is private;
   --  This type reprensents a file with a structured view of its contents

   type Structured_File_Access is access all Structured_File;

   procedure Free (File : in out Structured_File_Access);
   --  Free the data associated to File.

   function Get_Tree
     (File : Structured_File_Access) return Construct_Tree;
   --  Return the complete tree corresponding to this file. It the tree is not
   --  cached, then it will be computed.

   function Get_Buffer (File : Structured_File_Access) return String_Access;
   --  Return the buffer associated to this file note. Once the request is
   --  done, the buffer is stored in a cache so that the same buffer can be
   --  returned if this function is queried twice. The returned value must
   --  not be freed by the user.
   --  ??? What is the encoding of the returned data?

   function Get_File_Path (File : Structured_File_Access) return Virtual_File;
   --  Return the file path of the given file node.

   function Get_Offset_Of_Line
     (File : Structured_File_Access; Line : Integer) return Integer;
   --  Return the offset of the line given in parameter. For efficency, the
   --  line offsets of all lines are cached when calling this function,
   --  and freed either when the File is freed or updated.

   function Get_Language
     (File : Structured_File_Access) return Tree_Language_Access;
   --  Return the tree language associated to this file.

   procedure Update_Contents (File : Structured_File_Access);
   --  This function will re-analyze the full contents of the file

   -------------------
   -- Entity_Access --
   -------------------

   type Entity_Access is private;
   --  This type points to a precise entity in the database. It's not suitable
   --  for persistent storage: its data are not valid anymore after a refresh
   --  on the file where the entity is located.

   Null_Entity_Access : constant Entity_Access;

   type Entity_Array is array (Integer range <>) of Entity_Access;

   type Entity_Array_Access is access all Entity_Array;

   procedure Free (This : in out Entity_Array_Access);
   --  Free the data associated to this entity array access;

   function "<" (Left, Right : Entity_Access) return Boolean;
   --  Return a consistent order between the two entities.

   function "=" (Left, Right : Entity_Access) return Boolean;
   --  Return true if the two entities actually point to the same construct

   function To_Entity_Access
     (File       : Structured_File_Access;
      Construct  : Construct_Tree_Iterator) return Entity_Access;
   --  Created an entity access out of a construct and an iterator - the
   --  iterator has to come from the tree stored in the file.

   function To_Construct_Tree_Iterator
     (Entity : Entity_Access) return Construct_Tree_Iterator;
   --  Return the construct tree iterator referenced by this entity.

   function Get_File (Entity : Entity_Access) return Structured_File_Access;
   --  Return the file where this entity is located.

   function Get_Construct
     (Entity : Entity_Access) return access Simple_Construct_Information;
   --  Return the construct information referenced by this entity.

   ------------------------
   -- Construct_Database --
   ------------------------

   type Construct_Database is new Identifier_Manager with private;
   --  A construct database is a data structure containing a file index and a
   --  construct index, both sorted alphabetically.

   type Construct_Database_Access is access all Construct_Database;

   procedure Initialize
     (Db       : in out Construct_Database;
      Provider : Buffer_Provider_Access);
   --  This procedure has to be called before any other operation on the
   --  database.

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

   procedure Destroy (Db : access Construct_Database);
   --  Free the construct database - new contents cannot be added after this
   --  operation.

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

   function Get (It : Construct_Db_Iterator) return Entity_Access;
   --  Return the entity pointed by It.

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

   function Get_Tree_Annotation_Key_Registry
     (Db : Construct_Database_Access)
      return access Tree_Annotations_Pckg.Annotation_Key_Registry;
   --  Return the registry of keys used to create tree annotations.

   function Get_Construct_Annotation_Key_Registry
     (Db : Construct_Database_Access)
      return access Construct_Annotations_Pckg.Annotation_Key_Registry;
   --  Return the registry of keys used to create construct annotations.

   function "<" (Left, Right : Structured_File_Access) return Boolean;
   --  Needed by the file set package

   package File_Set is new Ada.Containers.Ordered_Multisets
     (Structured_File_Access);
   --  This package is used to index the file by their unit name

   use File_Set;

   function Start_File_Search (Db : Construct_Database) return File_Set.Cursor;
   --  Return a cursor pointing at the first element of the file database.

   function Get_Identifier
     (Manager : access Construct_Database; Name : String)
      return Distinct_Identifier;
   --  Return the unique identifier for the name given in parameter.

   function Get_Identifier (Entity : Entity_Access) return Distinct_Identifier;
   --  Return the identifier of this entity.

   ------------------------------
   -- Entity_Persistent_Access --
   ------------------------------

   type Entity_Persistent_Access is private;
   --  This type holds an entity that can survive during an update of its tree,
   --  provided that the merge engine manage to keep track of it.

   function "<" (Left, Right : Entity_Persistent_Access) return Boolean;
   --  Gives an arbitrary unique order between two entities.

   type Entity_Persistent_Array is array (Integer range <>)
   of Entity_Persistent_Access;

   type Entity_Persistent_Array_Access is access all Entity_Persistent_Array;

   procedure Free (This : in out Entity_Persistent_Array_Access);
   --  Free the data associated to This.

   function To_Entity_Access
     (Entity : Entity_Persistent_Access) return Entity_Access;
   --  Return the entity referenced by this entity access. If the entity is
   --  no longer accessible, then Null_Entity_Access will be returned.

   function To_Entity_Persistent_Access
     (Entity : Entity_Access) return Entity_Persistent_Access;
   --  Create a persistant entity based on this entity access.

   function Get_Construct
     (Entity : Entity_Persistent_Access) return Simple_Construct_Information;
   --  Return the construct pointed by this entity, if still available.

   procedure Ref (Entity : in out Entity_Persistent_Access);
   --  Increment the reference counter of this entity persistent access. The
   --  entity will not be freed as long as there's remaining references.

   procedure Unref (Entity : in out Entity_Persistent_Access);
   --  Decrement the reference counter of this entity. When the counter reaches
   --  zero, and if the parameter is not referencing an existing entity
   --  anymore, the entity persistant access will be freed.

   Null_Entity_Persistent_Access : constant Entity_Persistent_Access;

   function Exists (Entity : Entity_Persistent_Access) return Boolean;
   --  Return true if the entity still exist in the database, false if it has
   --  been removed.

   function Get_File
     (Entity : Entity_Persistent_Access) return Structured_File_Access;
   --  Return the file where the entity given in parameter is located.

   ------------------------
   -- Database_Assistant --
   ------------------------

   type Database_Assistant is abstract tagged private;

   type Database_Assistant_Access is access all Database_Assistant'Class;

   type Update_Kind is (Minor_Change, Structural_Change, Full_Change);

   procedure File_Updated
     (Assistant : access Database_Assistant;
      File      : Structured_File_Access;
      Kind      : Update_Kind) is null;
   --  Called whenever a file is updated. This is called after the changes, so
   --  the state of the file is the result of the update.
   --  There are three kinds of update:
   --    Minor_Change: no structural changes has been detected. Statements may
   --     have been added, or comment, blank lines, code may have been
   --     formatted, but constructs indexes are the same and no attribute has
   --     changed.
   --    Structural_Change: constructs may have been removed or added, or
   --     attributes may have changed. However, the update process managed to
   --     do a merge between the two versions of the trees.
   --    Full_Change: the file have been completely updated. This occures for
   --     example the first time the file is loaded.

   procedure Register_Assistant
     (Db        : Construct_Database_Access;
      Name      : String;
      Assistant : Database_Assistant_Access);
   --  Adds the assistant given in parameter under a given name - will raise
   --  an exception is an assistant with the same name is already registered.

   function Get_Assistant
     (Db : Construct_Database_Access; Name : String)
      return Database_Assistant_Access;
   --  Return the assistant that has been stored at the given name.

   function Get_Database
     (File : Structured_File_Access) return Construct_Database_Access;
   --  Return the database related to this file

   procedure Free (Assistant : in out Database_Assistant) is null;
   --  Free the internal data of the database assistant.

private

   type Buffer_Provider is abstract tagged null record;

   type File_Buffer_Provider is new Buffer_Provider with null record;

   type Trie_Additional_Data is record
      File  : Structured_File_Access;
   end record;

   Null_Trie_Additional_Data : constant Trie_Additional_Data := (File => null);

   package Construct_Db_Trie is new Construct_Tries
     (Trie_Additional_Data, Null_Trie_Additional_Data);

   use Construct_Db_Trie;

   type Construct_Db_Data_Array is array
     (Natural range <>) of Construct_Db_Trie.Construct_Trie_Index;

   type Construct_Db_Data_Access is access all Construct_Db_Data_Array;

   procedure Free (This : in out  Construct_Db_Data_Access);

   type Line_Start_Indexes is array (Natural range <>) of Natural;
   type Line_Start_Indexes_Access is access all Line_Start_Indexes;

   procedure Free is new Ada.Unchecked_Deallocation
     (Line_Start_Indexes, Line_Start_Indexes_Access);

   type Structured_File is record
      File        : Virtual_File;
      Lang        : Tree_Language_Access;

      Tree         : Construct_Tree;
      Db_Data_Tree : Construct_Db_Data_Access;

      Cache_Buffer : String_Access;

      Line_Starts  : Line_Start_Indexes_Access;

      Db           : access Construct_Database;
   end record;

   function "=" (Left, Right : Structured_File) return Boolean;

   package File_Map is new Ada.Containers.Ordered_Maps
     (Virtual_File, Structured_File_Access);

   use File_Map;

   package Assistant_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Database_Assistant_Access);

   package Assistant_List is new Ada.Containers.Doubly_Linked_Lists
     (Database_Assistant_Access);

   use Assistant_Map;

   use Assistant_List;

   type Construct_Database is new Identifier_Manager with record
      Files_Db           : File_Map.Map;
      Sorted_Files_Db    : File_Set.Set;
      --  ??? Do we really need these two now that we removed the assertion
      --  one file = one unit ? Probably not...

      Provider           : Buffer_Provider_Access;
      Entities_Db        : aliased Construct_Db_Trie.Construct_Trie;
      Assistants         : Assistant_Map.Map;
      Ordered_Assistants : Assistant_List.List;
      Tree_Registry      : aliased Tree_Annotations_Pckg.
        Annotation_Key_Registry;
      Construct_Registry : aliased Construct_Annotations_Pckg.
        Annotation_Key_Registry;
      Persistent_Entity_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   type Construct_Db_Iterator is record
      It : Construct_Db_Trie.Construct_Trie_Iterator;
   end record;

   Null_Construct_Db_Iterator : constant Construct_Db_Iterator :=
     (It => Construct_Db_Trie.Null_Construct_Trie_Iterator);

   type Database_Assistant is abstract tagged null record;

   type Entity_Access is record
      File : Structured_File_Access;
      It   : Construct_Tree_Iterator;
   end record;

   Null_Entity_Access : constant Entity_Access :=
     (null, Null_Construct_Tree_Iterator);

   type Entity_Persistent_Info is record
      Exists : Boolean := True;
      File   : Structured_File_Access;
      Index  : Integer;
      Refs   : Integer := 0;
   end record;

   type Entity_Persistent_Access is access all Entity_Persistent_Info;

   Null_Entity_Persistent_Access : constant Entity_Persistent_Access := null;

   type Entity_Persistent_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Info : Entity_Persistent_Access;
   end record;

   overriding
   procedure Free
     (Obj : in out Entity_Persistent_Annotation);

end Language.Tree.Database;
