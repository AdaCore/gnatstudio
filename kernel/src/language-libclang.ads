------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with Libclang.Index; use Libclang.Index;
with GPS.Core_Kernels; use GPS.Core_Kernels;
with GPS.Kernel;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Ada.Containers.Vectors;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

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

   --------------------------------
   --  Cache information records --
   --------------------------------

   --  NOTE: Those two types store Offsets for space efficiency reasons !

   --  10 bits is enough to store the cursor kind
   type Small_Cursor_Kind is mod 2 ** 10;

   type Decl_Info is record
      Loc        : Offset_T;
      Is_Def     : Boolean;
      Kind       : Small_Cursor_Kind;
   end record;
--     pragma Pack (Decl_Info);
   --  This is the record for declarations information stored in the reference
   --  cache. Important because in C a single entity can have several
   --  declarations AND bodies

   type Ref_Info is record
      Loc         : Offset_T;
      Cursor_Kind : Small_Cursor_Kind;
   end record;
--     pragma Pack (Ref_Info);
   --  This is the record for references information stored in the reference
   --  cache

   ----------------------------
   --  Cache data structures --
   ----------------------------

   package Ref_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Ref_Info);
   type Ref_Info_Vector is access all Ref_Info_Vectors.Vector;
   --  A vector of references information

   package Decl_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Decl_Info);
   type Decl_Info_Vector is access all Decl_Info_Vectors.Vector;
   --  A vector of declarations information

   type Info_Vectors is record
      Decls : Decl_Info_Vector := null;
      Refs : Ref_Info_Vector := null;
   end record;
   --  Record for the information for a (file, usr) couple. Keep tracks of
   --  every reference for a specific USR in a specific file

   use GNATCOLL.Symbols;

   package Symbol_To_Location_Maps is new Ada.Containers.Hashed_Maps
     (GNATCOLL.Symbols.Symbol, Info_Vectors,
      Hash => GNATCOLL.Symbols.Hash, Equivalent_Keys => "=");
   type Sym_To_Loc_Map is access all Symbol_To_Location_Maps.Map;
   procedure Destroy (S : in out Sym_To_Loc_Map);
   --  File -> (USR -> Info_Vector) part of the cache

   package VFS_To_Refs_Maps is new Ada.Containers.Hashed_Maps
     (GNATCOLL.VFS.Virtual_File, Sym_To_Loc_Map, Full_Name_Hash, "=");
   type VFS_To_Refs is access all VFS_To_Refs_Maps.Map;
   --  (File -> USR) -> Info_Vector part of the cache

   function Hash (Project : Project_Type) return Hash_Type is
     (Ada.Strings.Hash (Project.Name));

   type TU_Cache_Record is record
      TU      : Clang_Translation_Unit;
      Version : Integer := 0;
   end record;
   type TU_Cache_Access is access all TU_Cache_Record;
   procedure Destroy (Tu_Cache : in out TU_Cache_Access);
   --  Record used to store a translation unit in the TU cache. We add a
   --  version to it, that is used to determine when the TU should be reparsed

   package TU_Maps is new Ada.Containers.Hashed_Maps
     (Virtual_File, TU_Cache_Access, Full_Name_Hash, "=");
   type Tu_Map_Access is access all TU_Maps.Map;
   --  Map used to store the cache of translation units

   package LRU_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Virtual_File);
   type LRU_Vector_Access is access all LRU_Lists.List;
   --  List used to store a LIFO of the translation units to free when the
   --  cache is full

   type Clang_Context is record
      TU_Cache      : Tu_Map_Access;
      LRU           : LRU_Vector_Access;
      Clang_Indexer : Clang_Index;
      Index_Action  : Clang_Index_Action;
      Sym_Table     : GNATCOLL.Symbols.Symbol_Table_Access;
      Refs          : VFS_To_Refs;
   end record;

   protected TU_Source is
      function Translation_Unit
        (Kernel : Core_Kernel;
         File : GNATCOLL.VFS.Virtual_File;
         Reparse : Boolean := False;
         Default_Lang : String := "c++")
         return Clang_Translation_Unit;

      function Context
        (Project : Project_Type) return Clang_Context;
   end TU_Source;
   --  This is the main entry point of the module. Access to the translation
   --  units goes through Translation_Unit.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

end Language.Libclang;
