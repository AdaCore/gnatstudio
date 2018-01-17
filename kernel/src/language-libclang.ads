------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2018, AdaCore                   --
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

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Finalization;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Calendar;

with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GNATCOLL.Symbols.Streamable_Symbol_Table;

with Streamable_Access_Type;
with Array_Utils;

with Glib.Main;

with GPS.Core_Kernels;                use GPS.Core_Kernels;
with GPS.Kernel;
with GPS.Kernel.Modules;              use GPS.Kernel.Modules;

with Libclang.Task_Parser_Pool;       use Libclang.Task_Parser_Pool;
with clang_c_Index_h;                 use clang_c_Index_h;
with Libclang.Index;                  use Libclang.Index;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

-----------------------
-- Language.Libclang --
-----------------------

package Language.Libclang is

   --  This is the gate through which users wishing to use libclang will get
   --  the translation unit for a given file. IMPORTANT : EVERY translation
   --  unit parsing should go through this, even though the clang primitives
   --  are not hidden (yet)

   --  Translation units are stored in a cache with a fixed size, and each
   --  time a translation unit is parsed, a distinct reference cache is filled.
   --  This cache can be used to implement semantic search operations more
   --  efficiently, which is a necessity on big projects to keep a reasonnable
   --  run time.

   --  The reference cache has the following (simplified) structure:
   --
   --     File -> USR -> [Vector of references information]
   --
   --  Files are stored at the top level of the cache, so that it is easy to
   --  invalidate the cache for a given file. If will in turn make the search
   --  for references to a given USR longer than if the cache was the other way
   --  around, but this is a good compromise

   --  TODO ??? At some point, translation_unit should expose an async,
   --  non blocking way of getting the translation units, and GPS should be
   --  adjusted to not block on semantic operations. This will be done by
   --  means of a task

   type Clang_Module_Record is new Module_ID_Record with private;
   Clang_Module_Id : access Clang_Module_Record := null;

   --------------------------------
   --  Cache information records --
   --------------------------------

   --  NOTE: Those two types store Offsets for space efficiency reasons

   subtype Small_Cursor_Kind is CXCursorKind;

   type Decl_Info is record
      Loc        : Offset_T;
      Is_Def     : Boolean;
      Kind       : Small_Cursor_Kind;
   end record;
   --  This is the record for declarations information stored in the reference
   --  cache. Important because in C a single entity can have several
   --  declarations AND bodies

   type Ref_Info is record
      Loc         : Offset_T;
      Cursor_Kind : Small_Cursor_Kind;
   end record;
   --  This is the record for references information stored in the reference
   --  cache

   ----------------------------
   --  Cache data structures --
   ----------------------------

   package Ref_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Ref_Info);
   package Streamable_Ref_Info_Vectors_Accesses
   is new Streamable_Access_Type (Ref_Info_Vectors.Vector);
   subtype Ref_Info_Vector is Streamable_Ref_Info_Vectors_Accesses.Access_Type;
   --  A vector of references information

   package Decl_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Decl_Info);
   package Streamable_Decl_Info_Vectors_Accesses
   is new Streamable_Access_Type (Decl_Info_Vectors.Vector);
   subtype Decl_Info_Vector
     is Streamable_Decl_Info_Vectors_Accesses.Access_Type;
   --  A vector of declarations information

   type Info_Vectors is record
      Decls : Decl_Info_Vector := null;
      Refs : Ref_Info_Vector := null;
   end record;
   --  Record for the information for a (file, usr) couple. Keep tracks of
   --  every reference for a specific USR in a specific file

   package Clang_Symbol_Table_Pkg
   is new GNATCOLL.Symbols.Streamable_Symbol_Table;

   Clang_Symbol_Table : Clang_Symbol_Table_Pkg.Symbol_Table_Access
   renames Clang_Symbol_Table_Pkg.Symbol_Table;

   subtype Clang_Symbol is Clang_Symbol_Table_Pkg.Streamable_Symbol;
   use type Clang_Symbol;

   --------------------
   -- Sym_To_Loc_Map --
   --------------------

   --  File -> (USR -> Info_Vector) part of the cache

   package Symbol_To_Location_Maps is new Ada.Containers.Hashed_Maps
     (Clang_Symbol, Info_Vectors,
      Hash => Clang_Symbol_Table_Pkg.Hash, Equivalent_Keys => "=");

   type File_Cache_Record is record
      File_Name       : Unbounded_String;
      --  We keep the file name here, to be able to find it back easily when we
      --  have a cache entry. The key contains it but this allows us to avoid
      --  extracting it

      File_Time_Stamp : Ada.Calendar.Time;
      --  We keep the time stamp at which this cache entry was recorded, so
      --  that we can easily determine wether we need to recompute the entry
      --  when we start gps again.

      Active          : Boolean := True;
      --  This determines wether the cache is active or not. Since you can have
      --  several cache entries for a different file, this allows us to know
      --  which one is active

      Map             : Symbol_To_Location_Maps.Map;
      --  This is the map of cross references

   end record;
   --  A cache record keeps the cross-reference information for a *specific
   --  compilation* of a given file. This means that if for whatever reason
   --  you compile a given file with options that will yield different cross
   --  references, there will be *several* cache entries for each of them

   package Streamable_File_Cache_Accesses
   is new Streamable_Access_Type (File_Cache_Record);
   --  We use the Streamable_Access_Type generic package, so that accesses of
   --  this type are automatically streamable

   subtype File_Cache_Access is Streamable_File_Cache_Accesses.Access_Type;
   use type File_Cache_Access;

   -----------------
   -- VFS_To_Refs --
   -----------------

   --  (File -> USR) -> Info_Vector part of the cache

   type File_Key is new Unbounded_String;
   --  This type is the key that is used to index files in the clang cache. It
   --  is basically a conflation of the file full path + the switches that can
   --  change the cross references

   package File_To_Refs_Maps is new Ada.Containers.Hashed_Maps
     (File_Key, File_Cache_Access, Hash, "=");

   type Clang_Crossrefs_Cache_Type is record
      Map : File_To_Refs_Maps.Map;
   end record;

   package File_Type_Arrays is new Array_Utils (File_Cache_Access);
   subtype File_Cache_Array is File_Type_Arrays.Array_Type;
   type File_Cache_Array_Access is access all File_Cache_Array;

   package Streamable_Clang_Crossrefs_Cache_Accesses
   is new Streamable_Access_Type (Clang_Crossrefs_Cache_Type);

   subtype Clang_Crossrefs_Cache
     is Streamable_Clang_Crossrefs_Cache_Accesses.Access_Type;
   --  Main cross references cache data structure for libclang. Uses a
   --  streamable access type so we can serialize to disk

   type TU_Cache_Record is record
      TU       : Clang_Translation_Unit := No_Translation_Unit;
      Is_Ready : Boolean := False;
      Version  : Integer := 0;
   end record;
   type TU_Cache_Access is access all TU_Cache_Record;
   procedure Destroy (Tu_Cache : in out TU_Cache_Access);
   --  Record used to store a translation unit in the TU cache. We add a
   --  version to it, that is used to determine when the TU should be reparsed

   package TU_Maps is new Ada.Containers.Hashed_Maps
     (Unbounded_String, TU_Cache_Access, Hash, "=");
   type Tu_Map_Access is access all TU_Maps.Map;
   --  Map used to store the cache of translation units

   package LRU_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);
   type LRU_Vector_Access is access all LRU_Lists.List;
   --  List used to store a LIFO of the translation units to free when the
   --  cache is full

   --------------------------
   -- Translation unit API --
   --------------------------

   Default_Clang_Options : constant Clang_Translation_Unit_Flags :=
     Includebriefcommentsincodecompletion
     or Precompiledpreamble
     or Cachecompletionresults
     or Detailedpreprocessingrecord;
   --  This is the set of default clang options used by the clang module

   function Translation_Unit
     (Kernel       : Core_Kernel;
      File         : GNATCOLL.VFS.Virtual_File;
      Project      : Project_Type := No_Project;
      Reparse      : Boolean := False;
      Options      : Clang_Translation_Unit_Flags := Default_Clang_Options;
      Default_Lang : String := "c++")
      return Clang_Translation_Unit;
   --  Request the translation unit associated with file, and block while it's
   --  being computed, finally returning it

   procedure Enqueue_Translation_Unit
     (Kernel       : Core_Kernel;
      File         : GNATCOLL.VFS.Virtual_File;
      Reparse      : Boolean := False;
      Options      : Clang_Translation_Unit_Flags := Default_Clang_Options;
      Default_Lang : String := "c++";
      Prio         : Parsing_Request_Priority := Low;
      Callback     : in out Parse_Callback_Access);
   --  Request the translation unit associated with file and return
   --  immediately. Call Callback when the translation unit is ready

   function Has_Translation_Unit_In_Cache
     (File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return True iff there is a TU already in cache for File

   ----------------------------------------------------------
   -- Global accessors for the clang cross reference cache --
   ----------------------------------------------------------

   --  Those are the entry points in the cross reference cache.

   function Crossrefs_Cache return Clang_Crossrefs_Cache;
   --  Return the global cross-references cache

   function Get_Active_Files return File_Cache_Array;
   --  Return the files that are currently active in the cross references cache

   function Get_Index_Action return Clang_Index_Action;
   --  Return the global index action for the clang module

   function Construct_Cache_Key
     (Kernel    : Core_Kernel;
      File      : Virtual_File;
      Project   : Project_Type := No_Project) return File_Key;
   --  Construct a cache key given a file. If the Project is none, this
   --  function will take the first valid project for this
   --  ??? When completing aggregate project support, we'll remove the default
   --  value for project

   type Use_Index is private;
   --  This type is a controlled type that is used to notify the clang module
   --  that the clang cache is in use, and that it shouldn't be modified.
   --  Since it's a controlled object, here is the way it works:
   --
   --  procedure Use_The_Cache is
   --     I : Use_Index;
   --     --  As soon as the object is created, the cache is locked and cannot
   --     --  be modified
   --  begin
   --     Use_The_Cache_Somehow;
   --  end Use_The_Cache;
   --  --  When exiting the function, I is freed and the cache is unlocked

   ----------------------------------------------------
   -- Debug procedures for the clang reference cache --
   ----------------------------------------------------

   procedure Print_Decl_Info (D : Decl_Info);
   procedure Print_Ref_Info (D : Ref_Info);
   procedure Print_Cache (Cache : Clang_Crossrefs_Cache);
   procedure Print_File_Cache (File_Cache : File_Cache_Access);
   --  Debug procedures. Used to print the content of the libclang file cache

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the clang module globally

private

   Max_Nb_Tasks        : constant Natural := 6;
   --  We have made experiments and determined that above 6 tasks there was no
   --  improvement in analysis speed, even on machines with many processors.

   package Task_Parser_Pool is new Pool (User_Data => Core_Kernel);
   use Task_Parser_Pool;
   --  Parser pools using to parse clang translation unit in tasks
   --  asynchronously.

   type Clang_Module_Record is new Module_ID_Record with record
      Parsing_Timeout_Id   : Glib.Main.G_Source_Id;
      --  Id of the global timeout that is used to regularly index files that
      --  have been parsed

      Parsing_Tasks        : Parsing_Task_Array (1 .. Max_Nb_Tasks);
      --  Array of parsing tasks.

      Indexing_Active      : Boolean := True;
      --  Global variable used by the indexing timeout handler to know if it
      --  should index parsed files or not. This is used by procedures using
      --  the references cache, to notify the libclang engine that the cache
      --  is currently inspected and should not be modified

      TU_Cache             : Tu_Map_Access;
      LRU                  : LRU_Vector_Access;
      --  Those two components, together, constitute the LRU cache. The
      --  TU_Cache map is used to retrieve translation units by name, while
      --  the LRU vector is used to evict old translation units.

      Clang_Indexer        : Clang_Index;
      --  This is the global clang indexer, that is the gateway to the
      --  underlying libclang API. We have only one Indexer for everything
      --  for convenience, and because there is no pro to do it another way.

      Index_Action         : Clang_Index_Action;
      --  Index action used by the cross reference indexing machinery.

      Refs                 : Clang_Crossrefs_Cache;
      --  Clang cross references cache entry point.

      Active_Files         : File_Cache_Array_Access := null;
      --  Cache used because iteration on Hashed Maps is very slow.
   end record;
   --  This is the global cache record, containing information that is globally
   --  useful to the clang module

   overriding procedure Destroy (Id : in out Clang_Module_Record);
   --  Destroy procedure, freeing every resources associated with libclang

   type Use_Index is new Ada.Finalization.Controlled with null record;

   overriding procedure Initialize (Self : in out Use_Index);
   overriding procedure Finalize (Self : in out Use_Index);
end Language.Libclang;
