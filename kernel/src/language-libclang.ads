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

   type Decl_Info is record
      Loc : Offset_T;
      Is_Def : Boolean;
   end record;

   type Ref_Info is record
      Loc : Offset_T;
   end record;

   package Ref_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Ref_Info);
   type Ref_Info_Vector is access all Ref_Info_Vectors.Vector;

   package Decl_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Decl_Info);
   type Decl_Info_Vector is access all Decl_Info_Vectors.Vector;

   type Info_Vectors is record
      Decls : Decl_Info_Vector := null;
      Refs : Ref_Info_Vector := null;
   end record;

   use GNATCOLL.Symbols;
   package Symbol_To_Location_Maps is new Ada.Containers.Hashed_Maps
     (GNATCOLL.Symbols.Symbol, Info_Vectors,
      Hash => GNATCOLL.Symbols.Hash, Equivalent_Keys => "=");
   type Sym_To_Loc_Map is access all Symbol_To_Location_Maps.Map;

   procedure Destroy (S : in out Sym_To_Loc_Map);

   package VFS_To_Refs_Maps is new Ada.Containers.Hashed_Maps
     (GNATCOLL.VFS.Virtual_File, Sym_To_Loc_Map, Full_Name_Hash, "=");
   type VFS_To_Refs is access all VFS_To_Refs_Maps.Map;

   function Hash (Project : Project_Type) return Hash_Type is
     (Ada.Strings.Hash (Project.Name));

   type TU_Cache_Record is record
      TU      : Clang_Translation_Unit;
      Version : Integer := 0;
   end record;
   type TU_Cache_Access is access all TU_Cache_Record;

   procedure Destroy (Tu_Cache : in out TU_Cache_Access);

   package TU_Maps is new Ada.Containers.Hashed_Maps
     (Virtual_File, TU_Cache_Access, Full_Name_Hash, "=");
   type Tu_Map_Access is access all TU_Maps.Map;

   package LRU_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Virtual_File);
   type LRU_Vector_Access is access all LRU_Lists.List;

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
         Reparse : Boolean := False)
         return Clang_Translation_Unit;

      function Context
        (Project : Project_Type) return Clang_Context;
   end TU_Source;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

end Language.Libclang;
