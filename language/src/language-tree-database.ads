------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  This package provides an efficient database suitable to record the relevant
--  construct information of an entire project

with Ada.Finalization; use Ada.Finalization;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

with GNAT.Strings; use GNAT.Strings;

with Construct_Tries;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with Language.Profile_Formaters; use Language.Profile_Formaters;
with Language.Unknown;       use Language.Unknown;
with Ada.Containers; use Ada.Containers;

package Language.Tree.Database is

   type Entity_Access is private;
   --  This type points to a precise entity in the database. It's not suitable
   --  for persistent storage: its data are not valid anymore after a refresh
   --  on the file where the entity is located.
   --  See also Entity_Persistent_Access for longer term storage.

   type Construct_Database is tagged private;
   --  A construct database is a data structure containing a file index and a
   --  construct index, both sorted alphabetically.

   type Structured_File is private;
   --  This type reprensents a file with a structured view of its contents

   type Structured_File_Access is access all Structured_File;

   -------------------
   -- Tree_Language --
   -------------------

   type Tree_Language is abstract new Abstract_Tree_Language with private;
   --  This type represents the language of a given tree. It's used to provide
   --  various language-specific capabilities on a tree.

   type Tree_Language_Access is access all Tree_Language'Class;

   function Get_Language
     (Tree : access Tree_Language) return Language_Access is abstract;
   --  Return the language associated to this tree.

   procedure Get_Profile
     (Lang       : access Tree_Language;
      Entity     : Entity_Access;
      Formater   : access Profile_Formater'Class;
      With_Aspects : Boolean := False);
   --  Return a formatted view of the profile of this construct - if any.
   --  For example, for subprogram, this would return
   --       [(parameters)][return type]     on one line.
   --  For a variable, it would be:
   --       : [type]    on one line
   --  Formater is responsible for formating and keep resulting text.

   function Get_Profile
     (Lang             : access Tree_Language;
      Entity           : Entity_Access;
      With_Aspects     : Boolean := False;
      Show_Param_Names : Boolean := True) return String;
   --  Shortcut to Get_Profile that uses a default text formatter, and caches
   --  its results in some cases.

   function Get_Declaration
     (Lang   : access Tree_Language;
      Entity : Entity_Access) return Entity_Access;
   --  Return the declaration of the entity given in parameter.

   overriding function Get_Name_Index
     (Lang      : access Tree_Language;
      Construct : Simple_Construct_Information) return GNATCOLL.Symbols.Symbol;
   --  Return the name that should be used to index the given construct. Takes
   --  care of e.g. case handling. Default implementation return the actual
   --  construct name.

   overriding procedure Diff
     (Lang               : access Tree_Language;
      Old_Tree, New_Tree : Construct_Tree;
      Callback           : Diff_Callback);

   function Find_Declaration
     (Lang     : access Tree_Language;
      File     : Structured_File_Access;
      Line     : Integer;
      Column   : String_Index_Type) return Entity_Access;
   --  Find a declaration for the location given in parameter - this is
   --  best effort based. The implementer is responsible to decide if the
   --  information he has is accurate enough or not. By default, just
   --  return an null entity.

   type Entity_Reference_Details is record
      Index_Start : String_Index_Type := 0;
      Index_End   : String_Index_Type := 0;
      --  The entity name appears between Index_Start .. Index_End

      Is_Named_Parameter : Boolean := False;
      --  Whether the entity is the name of a parameter in the call to a
      --  subprogram, as in "Subp (Entity => 1)".
      --  This will always be False for languages that do not support this
      --  feature.

      Parenthesis_Loc    : String_Index_Type := 0;
      --  Location of the parenthesis following the entity (for instance in a
      --  function call, or an Ada aggregate)
   end record;
   --  Details about a reference to an entity in a source file

   Invalid_Reference : constant Entity_Reference_Details;

   function Find_Reference_Details
     (Lang    : access Tree_Language;
      File    : Structured_File_Access;
      Index   : String_Index_Type) return Entity_Reference_Details is abstract;
   --  Return details about an entity reference within the given file

   function Find_First_Part
     (Lang   : access Tree_Language;
      Entity : Entity_Access) return Entity_Access;
   --  Return the first part of the entity given in parameter. By default,
   --  return Entity.

   function Find_Next_Part
     (Lang   : access Tree_Language;
      Entity : Entity_Access) return Entity_Access;
   --  Find the next part of the entity given in parameter if there is any.
   --  By default, return Entity.

   type Unknown_Tree_Language is new Tree_Language with private;

   overriding function Get_Language
     (Tree : access Unknown_Tree_Language) return Language_Access;
   --  See inherited documentation

   overriding function Find_Reference_Details
     (Lang    : access Unknown_Tree_Language;
      File    : Structured_File_Access;
      Index   : String_Index_Type) return Entity_Reference_Details;
   --  See inherited documentation

   Unknown_Tree_Lang : constant Tree_Language_Access;

   -------------------------
   -- Language_Handler --
   -------------------------

   type Abstract_Language_Handler_Record is abstract tagged null record;
   type Abstract_Language_Handler
     is access all Abstract_Language_Handler_Record'Class;
   --  Type overridden in language_handlers.ads, which provide the necessary
   --  primitive operations to query the language associated with a file.

   function Get_Language_From_File
     (Handler           : access Abstract_Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return Language_Access
      is abstract;
   --  Find the language of a given file.
   --  Return Unknown_Lang if no other language could be found.

   function Get_Tree_Language_From_File
     (Handler           : access Abstract_Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False)
      return Tree_Language_Access is abstract;
   --  Same as above but returns the tree language

   ---------------------
   -- Buffer_Provider --
   ---------------------

   type Buffer_Provider is abstract tagged private;
   --  A buffer provider is used to change the way the database is getting the
   --  buffer for a given file. By default, it loads the file from the disk,
   --  but we might need to give some other sources, for example an open
   --  editor.

   type Buffer_Provider_Access is access all Buffer_Provider'Class;

   function Get_Timestamp
     (Provider : access Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return Integer is abstract;
   --  Return a logical timestamp indicating the state of the buffer, so that
   --  clients can be spared the expensive calls to Get_Buffer

   function Get_Buffer
     (Provider : access Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File)
      return GNAT.Strings.String_Access is abstract;
   --  Return the buffer corresponding to this buffer provider. The returned
   --  access type is supposed to be a copy of the actual buffer - it will be
   --  freed by the completion engine when needed.
   --  The returned buffer must be in UTF-8 format.

   procedure Free (This : in out Buffer_Provider_Access);
   --  Free the access type

   type File_Buffer_Provider is new Buffer_Provider with private;

   overriding function Get_Timestamp
     (Provider : access File_Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return Integer;
   --  See documentation of overridden declaration

   overriding function Get_Buffer
     (Provider : access File_Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return GNAT.Strings.String_Access;
   --  Return the buffer corresponding to the file given in parameter - by
   --  default just read the file.
   --  ??? What is the encoding of the returned data?

   ---------------------
   -- Structured_File --
   ---------------------

   overriding function "="
     (Left, Right : Structured_File_Access) return Boolean;
   pragma Inline ("=");
   --  This function performs a special equality between null pointers and
   --  "null" structured files (not associated with an actual file), which are
   --  always considered to be equal. All others cases are computing the
   --  regular equality between pointers.

   procedure Ref (File : Structured_File_Access);
   --  Increment reference counting. When there are references to a file,
   --  deletion is cancelled.

   procedure Unref (File : Structured_File_Access);
   --  Decrement reference counting on the file.

   function Is_Externally_Referenced
     (File : Structured_File_Access) return Boolean;
   --  Return true if the file is known to be referenced. Such files should
   --  not be deleted.

   function Get_Tree
     (File : Structured_File_Access) return Construct_Tree
     with Inline;
   --  Return the complete tree corresponding to this file. It the tree is not
   --  cached, then it will be computed.

   function Get_Buffer
     (File : Structured_File_Access) return GNAT.Strings.String_Access;
   --  Return the buffer associated to this file note. Once the request is
   --  done, the buffer is stored in a cache so that the same buffer can be
   --  returned if this function is queried twice. The returned value must
   --  not be freed by the user.
   --  ??? What is the encoding of the returned data?

   function Get_File_Path (File : Structured_File_Access) return Virtual_File;
   --  Return the file path of the given file node.

   function Get_Offset_Of_Line
     (File : Structured_File_Access; Line : Integer) return String_Index_Type;
   --  Return the offset of the line given in parameter. For efficency, the
   --  line offsets of all lines are cached when calling this function,
   --  and freed either when the File is freed or updated.
   --  The first line starts at index 1.

   function To_Visible_Column
     (File        : Structured_File_Access;
      Line        : Integer;
      Line_Offset : String_Index_Type) return Visible_Column_Type;
   --  Convert a line offset to a visible column, taking into account
   --  tabulations.

   function To_Line_String_Index
     (File   : Structured_File_Access;
      Line   : Integer;
      Column : Visible_Column_Type) return String_Index_Type;
   --  Convert a column into a line offset, taking into account tabulations.
   --  Note that the first column is 1, the first byte index is 1.

   function To_String_Index
     (File   : Structured_File_Access;
      Line   : Integer;
      Column : Visible_Column_Type) return String_Index_Type;
   --  Return the offset from the beginning of the file, according to Line and
   --  Column in File, considering that the file is a string starting at 1.

   procedure To_Line_Column
     (File                 : Structured_File_Access;
      Absolute_Byte_Offset : String_Index_Type;
      Line                 : out Integer;
      Column               : out Visible_Column_Type);
   --  Computes the line and the column of a string index, given the contents
   --  of a file.

   function Get_Tree_Language
     (File : Structured_File_Access) return Tree_Language_Access;
   --  Return the tree language associated to this file.

   procedure Update_Contents
     (File  : Structured_File_Access;
      Purge : Boolean := False);
   --  This function will re-analyze the full contents of the file.
   --  If Purge = True old content of File will be dropped and new one created
   --  from scratch. If there is a lot of changes it could be faster then
   --  trying to update old contents by applying changes one by one.

   function Is_Null (File : Structured_File_Access) return Boolean;
   --  Return true if the file is null, which can be either because the pointer
   --  is null, or becase the file refered to is No_File.

   type Update_Lock is limited new Limited_Controlled with private;
   --  This type is used to avoid updates on a given file for a limited amount
   --  of time. The lock can be released either explicitely, by doing calls to
   --  Unlock, or implicitely, at object finalization. If two or more locks are
   --  taken on a given file, then the system will wait for all of them to
   --  be released before re-allowing updates. If an update is queried while
   --  the file is locked, then it will be delayed until the last lock is
   --  released if lock kind is Defer_Updates, and ignored if Ignore_Updates.

   type Lock_Kind_Type is (Defer_Updates, Ignore_Updates);

   function Lock_Updates
     (File : Structured_File_Access; Kind : Lock_Kind_Type := Defer_Updates)
      return Update_Lock;
   --  Locks the file for further updates. If Kind is Ignore_Updates, then
   --  all updates will be ignored when this lock is active, that is to say
   --  when it's the last lock put on the file. If Kind is Defer_Update, then
   --  an update will be done when the topmost lock is released, if an update
   --  has been queried while this lock is active.

   procedure Unlock (This : in out Update_Lock);
   --  Unlock the locked file, if any, and release update event if there's no
   --  more lock.

   overriding procedure Finalize (This : in out Update_Lock);
   --  Same as before, but done automatically upon object finalization.

   -------------------
   -- Entity_Access --
   -------------------

   Null_Entity_Access : aliased constant Entity_Access;

   type Entity_Array is array (Integer range <>) of Entity_Access;

   type Entity_Array_Access is access all Entity_Array;

   procedure Free (This : in out Entity_Array_Access);
   --  Free the data associated to this entity array access;

   function "<" (Left, Right : Entity_Access) return Boolean;
   --  Return a consistent order between the two entities.

   overriding function "=" (Left, Right : Entity_Access) return Boolean;
   pragma Inline ("=");
   --  Return true if the two entities actually point to the same construct

   function To_Entity_Access
     (File       : Structured_File_Access;
      Construct  : Construct_Tree_Iterator) return Entity_Access;
   pragma Inline (To_Entity_Access);
   --  Created an entity access out of a construct and an iterator - the
   --  iterator has to come from the tree stored in the file.

   function To_Construct_Tree_Iterator
     (Entity : Entity_Access) return Construct_Tree_Iterator;
   pragma Inline (To_Construct_Tree_Iterator);
   --  Return the construct tree iterator referenced by this entity.

   function Get_File (Entity : Entity_Access) return Structured_File_Access;
   pragma Inline (Get_File);
   --  Return the file where this entity is located.

   function Get_Construct
     (Entity : Entity_Access) return access Simple_Construct_Information;
   pragma Inline (Get_Construct);
   --  Return the construct information referenced by this entity.

   function To_String (Entity : Entity_Access) return String;
   --  Return a string version of the entity, for debugging purposes.

   function Contains
     (Scope : Entity_Access; Entity : Entity_Access) return Boolean;
   --  Return true if the scope given in parameter contains the entity.

   ------------------------
   -- Construct_Database --
   ------------------------

   type Construct_Database_Access is access all Construct_Database;

   procedure Initialize
     (Db         : Construct_Database_Access;
      Lg_Handler : Abstract_Language_Handler);
   procedure Set_Provider
     (Db         : Construct_Database_Access;
      Provider   : Buffer_Provider_Access);
   --  This procedure has to be called before any other operation on the
   --  database. These are two separate procedures so that they can be called
   --  at different points in time.

   procedure Set_Symbols
     (Self    : access Construct_Database;
      Symbols : GNATCOLL.Symbols.Symbol_Table_Access);
   function Symbols
     (Self    : access Construct_Database)
      return GNATCOLL.Symbols.Symbol_Table_Access;
   --  Set the symbol table to use to store entity names.
   --  This table is shared with the kernel, but the kernel is not visible
   --  from this package. This also simplifies integration in GNATBench

   procedure Free (This : in out Construct_Database_Access);
   --  Free the data associated to this database.

   function Get_Or_Create
     (Db        : Construct_Database_Access;
      File      : Virtual_File;
      Project   : Project_Type := No_Project) return Structured_File_Access;
   --  Return the file node corresponding to the given file path, and create
   --  one if needed. The creation of the file implies the addition of all its
   --  contents. An empty file (not null) will be returned in case the input
   --  file path is not valid.
   --  If a file is created, then it will be created with the project given
   --  in parameter - othewise the project attribute will be ignored.

   function Get_File
     (Db   : Construct_Database_Access;
      File : Virtual_File) return Structured_File_Access;
   --  Return a file already stored in the construct db, null if none.

   procedure Remove_File
     (Db        : Construct_Database_Access;
      File      : Virtual_File);
   --  Remove the file from the database if is exist. If the file has external
   --  references, as set through the Ref primitive of Structured_File, then
   --  the removal will be aborted.

   function Get_Project
     (File : Structured_File_Access) return Project_Type;
   --  Return the project associated to this file.

   procedure Set_Project
     (File : Structured_File_Access; Project : Project_Type);
   --  Changes the association between the file and the project.

   procedure Update_Contents
     (Db    : access Construct_Database;
      File  : Virtual_File;
      Purge : Boolean := False);
   --  Reload the file and its constructs. Previous constructs are removed from
   --  the database. See above about Purge.

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
   pragma Inline (Get_Construct);
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

   function Get_Identifier
     (Entity : Entity_Access) return Normalized_Symbol;
   pragma Inline (Get_Identifier);
   --  Return the identifier of this entity.

   procedure Analyze_File_Differences
     (Db                         : Construct_Database_Access;
      New_Set                    : File_Array;
      Removed_Files, Added_Files : out File_Array_Access);
   --  Return the differences between the file set given in parameter and the
   --  current contents of the database. The caller is expected to free
   --  Removed_Files and Added_Files.

   ------------------------------
   -- Entity_Persistent_Access --
   ------------------------------

   type Entity_Persistent_Access is private;
   --  This type holds an entity that can survive during an update of its tree,
   --  provided that the merge engine manage to keep track of it.

   function "<" (Left, Right : Entity_Persistent_Access) return Boolean;
   --  Gives an arbitrary unique order between two entities.

   function Hash (Entity : Entity_Access) return Hash_Type;

   type Entity_Persistent_Array is array (Integer range <>)
   of Entity_Persistent_Access;

   type Entity_Persistent_Array_Access is access all Entity_Persistent_Array;

   procedure Free (This : in out Entity_Persistent_Array_Access);
   --  Free the data associated to This.

   function To_Entity_Access
     (Entity : Entity_Persistent_Access) return Entity_Access;
   pragma Inline (To_Entity_Access);
   --  Return the entity referenced by this entity access. If the entity is
   --  no longer accessible, then Null_Entity_Access will be returned.

   function To_Entity_Persistent_Access
     (Entity : Entity_Access) return Entity_Persistent_Access;
   --  Create a persistant entity based on this entity access.
   --  The result needs to be Unref-ed when no longer needed.

   function To_Unrefed_Entity_Persistent_Access
     (Entity : Entity_Access) return Entity_Persistent_Access;
   --  Same as To_Entity_Persistent_Access, but the result does not hold a
   --  reference, and will be cleaned the next time the same file is processed.
   --  You need to explictly call ref if you intend to keep the entity.
   --  In general, this function should not be used, prefer
   --  To_Entity_Persistent_Access instead.

   function Get_Construct
     (Entity : Entity_Persistent_Access) return Simple_Construct_Information;
   pragma Inline (Get_Construct);
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
   pragma Inline (Exists);
   --  Return true if the entity still exist in the database, false if it has
   --  been removed.

   function Get_File
     (Entity : Entity_Persistent_Access) return Structured_File_Access;
   --  Return the file where the entity given in parameter is located.

   ------------------------
   -- Database_Listener --
   ------------------------

   type Database_Listener is abstract tagged null record;
   --  ??? this should really be an interface.

   type Database_Listener_Access is access all Database_Listener'Class;

   type Update_Kind is
     (Minor_Change,
      Structural_Change,
      Full_Change,
      Project_Change,
      Removed);

   procedure File_Updated
     (Listener : access Database_Listener;
      File     : Structured_File_Access;
      Old_Tree : Construct_Tree;
      Kind     : Update_Kind) is null;
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

   procedure Before_Clear_Db
     (Listener : access Database_Listener;
      Db       : access Construct_Database'Class) is null;
   --  Called before a clear of the database.

   procedure Add_Database_Listener
     (Db       : Construct_Database_Access;
      Listener : Database_Listener_Access);
   --  Add a listener to the list of objects listenning to database changes

   procedure Remove_Database_Listener
     (Db       : Construct_Database_Access;
      Listener : Database_Listener_Access);
   --  Removes a listener from the list of objects listenning to database
   --  changes

   ------------------------
   -- Database_Assistant --
   ------------------------

   type Database_Assistant is abstract new Database_Listener with private;

   type Database_Assistant_Access is access all Database_Assistant'Class;

   procedure Register_Assistant
     (Db        : Construct_Database_Access;
      Name      : String;
      Assistant : Database_Assistant_Access);
   --  Adds the assistant given in parameter under a given name - will raise
   --  an exception is an assistant with the same name is already registered.
   --  This take care of registering the assistant in the list of listeners,
   --  which should not be added manually.

   function Get_Assistant
     (Db : Construct_Database_Access; Name : String)
      return Database_Assistant_Access;
   pragma Inline (Get_Assistant);
   --  Return the assistant that has been stored at the given name.

   function Get_Database
     (File : Structured_File_Access) return Construct_Database_Access;
   --  Return the database related to this file

   procedure Free (Assistant : in out Database_Assistant) is null;
   --  Free the internal data of the database assistant.

   procedure Lock_All_Updates;
   --  Until Unlock_All_Updates is called no more Update_Contents of
   --  Structured_File is done. Updates will be done when Unlock_All_Updates
   --  will be called.

   procedure Unlock_All_Updates;
   --  Terminate the global update contents lock session. All pending updates
   --  are realized.

private

   Invalid_Reference : constant Entity_Reference_Details := (0, 0, False, 0);

   type Tree_Language is abstract
   new Abstract_Tree_Language with null record;

   function Get_Last_Relevant_Construct
     (Tree   : Construct_Tree;
      Offset : Natural)
      return Construct_Tree_Iterator;
   --  Return the last construct representing the scope where the offset is.
   --  It can be either the last entity declared in the scope, or the scope
   --  itself.
   --
   --  Example:
   --
   --  package A is
   --
   --     V : Integer; <- here is the last relevant construct
   --     <-  here is the offset
   --
   --  other example:
   --
   --  package A is
   --
   --     V : Integer;
   --
   --     package B is <- here is the last relevant construct
   --        V2 : Integer;
   --     end B;
   --
   --     <-  here is the offset
   --
   --  last example:
   --
   --  package A is <- here is the last relevant construct
   --
   --     <-  here is the offset

   type Unknown_Tree_Language is new Tree_Language with null record;

   Unknown_Tree_Lang : constant Tree_Language_Access :=
     new Unknown_Tree_Language;

   type Buffer_Provider is abstract tagged null record;

   type File_Buffer_Provider is new Buffer_Provider with null record;

   type Trie_Additional_Data is record
      File  : Structured_File_Access;
   end record;

   Null_Trie_Additional_Data : constant Trie_Additional_Data := (File => null);

   package Construct_Db_Trie is new Construct_Tries
     (Trie_Additional_Data, Null_Trie_Additional_Data);

   use Construct_Db_Trie;
   use Construct_Db_Trie.Construct_Trie_Trees;

   type Construct_Db_Data_Array is array
     (Natural range <>) of Construct_Db_Trie.Construct_Trie_Index;

   type Construct_Db_Data_Access is access all Construct_Db_Data_Array;

   type Line_Start_Indexes is array (Natural range <>) of String_Index_Type;
   type Line_Start_Indexes_Access is access all Line_Start_Indexes;

   procedure Free is new Ada.Unchecked_Deallocation
     (Line_Start_Indexes, Line_Start_Indexes_Access);

   type Structured_File is record
      File      : Virtual_File;
      Lang      : Language_Access := Unknown_Lang;
      Tree_Lang : Tree_Language_Access;

      Timestamp : Integer := -1;
      --  A logical timestamp which is used to invalidate
      --  Tree only when the buffer has changed since
      --  last time it was calculated.

      Tree         : Construct_Tree;
      Db_Data_Tree : Construct_Db_Data_Access;

      Cache_Buffer : GNAT.Strings.String_Access;

      Line_Starts  : Line_Start_Indexes_Access;

      Db           : access Construct_Database;

      Lock_Depth    : Natural := 0;
      Lock_Kind     : Lock_Kind_Type := Defer_Updates;
      Update_Locked : Boolean := False;

      Ref           : Natural := 0;

      Project       : Project_Type := No_Project;
   end record;

   type Update_Lock is limited new Limited_Controlled with record
      File_Locked    : Structured_File_Access;
      Last_Lock_Kind : Lock_Kind_Type;
   end record;

   function "=" (Left, Right : Structured_File) return Boolean;
   pragma Inline ("=");

   package File_Map is new Ada.Containers.Hashed_Maps
     (Virtual_File, Structured_File_Access,
      Equivalent_Keys => "=",
      Hash            => GNATCOLL.VFS.Full_Name_Hash);
   use File_Map;

   package Assistant_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Database_Assistant_Access);

   package Database_Listeners is new Ada.Containers.Doubly_Linked_Lists
     (Database_Listener_Access);

   use Assistant_Map;

   use Database_Listeners;

   type Construct_Database is tagged record
      Files_Db           : File_Map.Map;
      Provider           : Buffer_Provider_Access;
      Entities_Db        : aliased Construct_Db_Trie.Construct_Trie;
      Assistants         : Assistant_Map.Map;
      Listeners          : Database_Listeners.List;
      Tree_Registry      : aliased Tree_Annotations_Pckg.
        Annotation_Key_Registry;
      Construct_Registry : aliased Construct_Annotations_Pckg.
        Annotation_Key_Registry;
      Persistent_Entity_Key : Construct_Annotations_Pckg.Annotation_Key;

      Null_Structured_File : aliased Structured_File;

      Symbols       : GNATCOLL.Symbols.Symbol_Table_Access;

      Lg_Handler    : Abstract_Language_Handler;
   end record;

   type Construct_Db_Iterator is record
      It : Construct_Db_Trie.Construct_Trie_Iterator;
   end record;

   Null_Construct_Db_Iterator : constant Construct_Db_Iterator :=
     (It => Construct_Db_Trie.Null_Construct_Trie_Iterator);

   type Database_Assistant is abstract new Database_Listener with null record;

   type Entity_Access is record
      File : Structured_File_Access;
      It   : Construct_Tree_Iterator;
   end record;

   Null_Entity_Access : aliased constant Entity_Access :=
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
