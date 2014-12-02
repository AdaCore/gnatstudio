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

with Ada.Containers.Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Utils; use GNATCOLL.Utils;
with Language.Libclang.Utils; use Language.Libclang.Utils;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPS.Editors; use GPS.Editors;
with clang_c_Index_h; use clang_c_Index_h;
with GPS.Kernel.Hooks; use GPS.Kernel.Hooks;
with GPS.Kernel; use GPS.Kernel;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Language.Libclang_Tree; use Language.Libclang_Tree;
with Ada.Containers.Vectors;
with String_Utils; use String_Utils;

package body Language.Libclang is

   LRU_Size : constant := 32;

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("LANGUAGE_LIBCLANG", On);

   Clang_Options : constant Clang_Translation_Unit_Flags :=
     Includebriefcommentsincodecompletion
     and Precompiledpreamble
     and Cachecompletionresults;

   function Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files)
      return Clang_Translation_Unit;

   ----------------------
   -- References cache --
   ----------------------

   type Decl_Info is record
      Sloc : Sloc_T;
      Is_Def : Boolean;
   end record;

   type Ref_Info is record
      Sloc : Sloc_T;
   end record;

   package Ref_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Ref_Info);
   type Ref_Info_Vector is access all Ref_Info_Vectors.Vector;
   procedure Free
   is new Ada.Unchecked_Deallocation
     (Ref_Info_Vectors.Vector, Ref_Info_Vector);

   package Decl_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Decl_Info);
   type Decl_Info_Vector is access all Decl_Info_Vectors.Vector;
   procedure Free
   is new Ada.Unchecked_Deallocation
     (Decl_Info_Vectors.Vector, Decl_Info_Vector);

   type Info_Vectors is record
      Decls : Decl_Info_Vector := null;
      Refs : Ref_Info_Vector := null;
   end record;

   use GNATCOLL.Symbols;
   package Symbol_To_Location_Maps is new Ada.Containers.Hashed_Maps
     (GNATCOLL.Symbols.Symbol, Info_Vectors,
      Hash => GNATCOLL.Symbols.Hash, Equivalent_Keys => "=");
   type Sym_To_Loc_Map is access all Symbol_To_Location_Maps.Map;
   procedure Free
   is new Ada.Unchecked_Deallocation
     (Symbol_To_Location_Maps.Map, Sym_To_Loc_Map);

   procedure Destroy (S : in out Sym_To_Loc_Map);

   package VFS_To_Refs_Maps is new Ada.Containers.Hashed_Maps
     (GNATCOLL.VFS.Virtual_File, Sym_To_Loc_Map, Full_Name_Hash, "=");
   type VFS_To_Refs is access all VFS_To_Refs_Maps.Map;
   procedure Free
   is new Ada.Unchecked_Deallocation
     (VFS_To_Refs_Maps.Map, VFS_To_Refs);

   function Hash (Project : Project_Type) return Hash_Type is
     (Ada.Strings.Hash (Project.Name));

   type TU_Cache_Record is record
      TU      : Clang_Translation_Unit;
      Version : Integer := 0;
   end record;
   type TU_Cache_Access is access all TU_Cache_Record;

   procedure Free
   is new Ada.Unchecked_Deallocation (TU_Cache_Record, TU_Cache_Access);

   procedure Destroy (Tu_Cache : in out TU_Cache_Access);

   package TU_Maps is new Ada.Containers.Hashed_Maps
     (Virtual_File, TU_Cache_Access, Full_Name_Hash, "=");
   type Tu_Map_Access is access all TU_Maps.Map;

   package LRU_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Virtual_File);
   type LRU_Vector_Access is access all LRU_Lists.List;
   procedure Free
   is new Ada.Unchecked_Deallocation (LRU_Lists.List, LRU_Vector_Access);

   type Clang_Context is record
      TU_Cache      : Tu_Map_Access;
      LRU           : LRU_Vector_Access;
      Clang_Indexer : Clang_Index;
      Index_Action  : Clang_Index_Action;
      Sym_Table     : GNATCOLL.Symbols.Symbol_Table_Access;
      Refs          : VFS_To_Refs;
   end record;

   procedure Free
   is new Ada.Unchecked_Deallocation (TU_Maps.Map, Tu_Map_Access);

   package Clang_Cache_Maps is new Ada.Containers.Hashed_Maps
     (Project_Type, Clang_Context, Hash, "=");

   type Clang_Module_Record is new Abstract_Module_Record with record
      Global_Cache : Clang_Cache_Maps.Map;
   end record;

   overriding procedure Destroy (Id : in out Clang_Module_Record);

   Clang_Module_Id : access Clang_Module_Record;

   procedure Initialize (Ctx : out Clang_Context);

   procedure Destroy (Ctx : in out Clang_Context);

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out Sym_To_Loc_Map) is
   begin
      for El of S.all loop
         if El.Refs /= null then
            Free (El.Refs);
         end if;
         if El.Decls /= null then
            Free (El.Decls);
         end if;
      end loop;
      Free (S);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Ctx : out Clang_Context)
   is
      Idx : constant Clang_Index := Create_Index (True, Active (Diagnostics));
   begin
      Ctx :=
        (Clang_Indexer => Idx,
         TU_Cache      => new TU_Maps.Map,
         LRU           => new LRU_Lists.List,
         Index_Action  => Create (Idx),
         Sym_Table     => GNATCOLL.Symbols.Allocate,
         Refs          => new VFS_To_Refs_Maps.Map);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Ctx : in out Clang_Context) is
   begin
      for C of Ctx.TU_Cache.all loop
         Destroy (C);
      end loop;
      Ctx.TU_Cache.Clear;
      Free (Ctx.TU_Cache);
      Free (Ctx.LRU);

      for M of Ctx.Refs.all loop
         Destroy (M);
      end loop;

      Free (Ctx.Refs);

      GNATCOLL.Symbols.Free (Ctx.Sym_Table);
      Dispose (Ctx.Index_Action);
   end Destroy;

   pragma Style_Checks (Off);
   pragma Warnings (Off);

   type Indexer_Data is record
      Syms_To_Locs : Sym_To_Loc_Map;
      Ctx          : Clang_Context;
   end record;

   -----------------
   -- Abort_Query --
   -----------------

   function Abort_Query
     (Client_Data : in out Indexer_Data) return Boolean is (False);

   ----------------
   -- Diagnostic --
   ----------------

   procedure Diagnostic
     (Client_Data : in out Indexer_Data;
      Diagnostics : Clang_Diagnostic_Set)
   is
   begin
      null;
   end Diagnostic;

   -----------------------
   -- Entered_Main_File --
   -----------------------

   procedure Entered_Main_File
     (Client_Data : in out Indexer_Data;
      File        : GNATCOLL.VFS.Virtual_File) is
   begin
      Put_Line ("IN ENTERED MAIN FILE");
   end Entered_Main_File;

   -------------------
   -- Included_File --
   -------------------

   procedure Included_File
     (Client_Data : in out Indexer_Data;
      Included_File_Info : Clang_Included_File_Info) is
   begin
      null;
   end Included_File;

   ------------------------------
   -- Started_Translation_Unit --
   ------------------------------

   procedure Started_Translation_Unit
     (Client_Data : in out Indexer_Data) is
   begin
      Put_Line ("IN STARTED TRANSLATION UNIT");
   end Started_Translation_Unit;

   -----------------------
   -- Index_Declaration --
   -----------------------

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Decl_Info)
   is
      use Interfaces.C.Strings;
      use Interfaces.C;
      Loc : Clang_Location :=
        clang_indexLoc_getCXSourceLocation (Info.loc);
      Sym : GNATCOLL.Symbols.Symbol;
      Info_Vector : Info_Vectors;
   begin
      if Is_From_Main_File (Loc)
      then
--           Put_Line
--             ("IN INDEX DECLARATION "
--              & (if Info.entityInfo.name /= Null_Ptr
--                then Interfaces.C.Strings.Value (Info.entityInfo.name)
--                else ""));

         Sym :=
           Client_Data.Ctx.Sym_Table.Find (Value (Info.entityInfo.USR));

         if not Client_Data.Syms_To_Locs.Contains (Sym) then
            Info_Vector := (new Decl_Info_Vectors.Vector,
                            new Ref_Info_Vectors.Vector);
            Client_Data.Syms_To_Locs.Include (Sym, Info_Vector);
         else
            Info_Vector := Client_Data.Syms_To_Locs.Element (Sym);
         end if;

         Info_Vector.Decls.Append
           (Decl_Info'(To_Sloc_T (Loc), Info.isDefinition /= 0));

      end if;
   end Index_Declaration;

   ---------------------
   -- Index_Reference --
   ---------------------

   procedure Index_Reference
     (Client_Data : in out Indexer_Data;
      Info   : Clang_Ref_Info)
   is
      use Interfaces.C.Strings;
      use Interfaces.C;
      Loc : Clang_Location :=
        clang_indexLoc_getCXSourceLocation (Info.loc);
      Sym : GNATCOLL.Symbols.Symbol;
      Info_Vector : Info_Vectors;

   begin
      if Is_From_Main_File (Loc)
      then
--           Put_Line ("IN INDEX REFERENCE");
         Sym :=
           Client_Data.Ctx.Sym_Table.Find (Value (Info.referencedEntity.USR));

         if not Client_Data.Syms_To_Locs.Contains (Sym) then
            Info_Vector := (new Decl_Info_Vectors.Vector,
                            new Ref_Info_Vectors.Vector);
            Client_Data.Syms_To_Locs.Include (Sym, Info_Vector);
         else
            Info_Vector := Client_Data.Syms_To_Locs.Element (Sym);
         end if;

         Info_Vector.Refs.Append
           (Ref_Info'(Sloc => To_Sloc_T (Loc)));

      end if;
   end Index_Reference;

   package Indexer is new Source_File_Indexer
     (Client_Data_T => Indexer_Data);

   pragma Style_Checks (On);
   pragma Warnings (On);

   ----------------------
   -- Translation_Unit --
   ----------------------
   protected body TU_Source is
      function Translation_Unit
        (Kernel : Core_Kernel;
         File : GNATCOLL.VFS.Virtual_File;
         Reparse : Boolean := False)
      return Clang_Translation_Unit
      is
         Buffer : constant Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get (File, False, False, False, False);
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Kernel.Registry.Tree.Info_Set
                (File).First_Element);
         Context : Clang_Context;
         Cache_Val : TU_Cache_Access;
         TU : Clang_Translation_Unit;
      begin
         if Reparse
           and then Buffer /= Nil_Editor_Buffer
           and then Clang_Module_Id.Global_Cache.Contains (F_Info.Project)
         then
            Context := Clang_Module_Id.Global_Cache.Element (F_Info.Project);

            if Context.TU_Cache.Contains (File) then
               Cache_Val := Context.TU_Cache.Element (File);
               if Cache_Val.Version < Buffer.Version then
                  declare
                     Buffer_Text : Ada.Strings.Unbounded.String_Access :=
                       new String'(Buffer.Get_Chars);
                  begin
                     TU := Translation_Unit
                       (Kernel, File,
                        (0 => Create_Unsaved_File
                             (String (File.Full_Name.all), Buffer_Text)));
                     Cache_Val.Version := Buffer.Version;
                     Free (Buffer_Text);
                     return TU;
                  end;
               end if;
            end if;

         end if;
         return Translation_Unit (Kernel, File, No_Unsaved_Files);
      end Translation_Unit;
   end TU_Source;

   ----------------------
   -- Translation_Unit --
   ----------------------

   function Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files)
      return Clang_Translation_Unit
   is
      --  ??? We should fill other unsaved_files! ??? Or should we ? I think
      --  that filling the current file as unsaved is enough. We can, at
      --  least in the first iteration of libclang, ask the user to save
      --  the other files if he expects to get completion. RA

      Lang             : constant String :=
        Kernel.Lang_Handler.Get_Language_From_File (File);
      C_Switches       : GNAT.Strings.String_List_Access;
      Ignored          : Boolean;

      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Kernel.Registry.Tree.Info_Set
             (File).First_Element);

      Context : Clang_Context;
   begin
      if not Clang_Module_Id.Global_Cache.Contains (F_Info.Project) then
         Initialize (Context);
         Clang_Module_Id.Global_Cache.Insert (F_Info.Project, Context);
      else
         Context := Clang_Module_Id.Global_Cache.Element (F_Info.Project);
      end if;

      if Unsaved_Files = No_Unsaved_Files
        and then Context.TU_Cache.Contains (File)
      then
         return Context.TU_Cache.Element (File).TU;
      end if;

      --  Retrieve the switches for this file
      Switches (F_Info.Project,
                "compiler", File, Lang, C_Switches, Ignored);

      declare
         The_Switches     : Unbounded_String_Array (C_Switches'Range);
         TU : Clang_Translation_Unit;
         Dummy : Boolean;
         Refs : Sym_To_Loc_Map;
      begin
         for J in C_Switches'Range loop
            The_Switches (J) := To_Unbounded_String (C_Switches (J).all);
         end loop;

         if Context.Refs.Contains (File) then
            Destroy (Context.Refs.Reference (File));
         end if;
         Refs := new Symbol_To_Location_Maps.Map;
         Context.Refs.Include (File, Refs);

         if Context.TU_Cache.Contains (File) then
            --  If the key is in the cache, we know that File_Content is not
            --  null, so we want to reparse
            TU := Context.TU_Cache.Element (File).TU;
            Dummy := Reparse_Translation_Unit (TU, Unsaved_Files,
                                               Options => Clang_Options);
         else
            declare
               Switches : constant GNATCOLL.Utils.Unbounded_String_Array
                 :=

               --  We pass to libclang a list of switches made of:
               --  ... the C/C++ switches specified in this project
                 The_Switches

               --  ... a -I<dir> for each directory in the subprojects
               --  of this project
                 & Get_Project_Source_Dirs
                 (Kernel, F_Info.Project, Lang)

                 --  ... a -I<dir> for each dir in the compiler search path
                 & Get_Compiler_Search_Paths
                 (Kernel, F_Info.Project, Lang);
            begin
               for S of Switches loop
                  Put_Line ("SWITCHES : " & (+S));
               end loop;

               --  In the other case, this is the first time we're parsing this
               --  file
               TU := Indexer.Index_Source_File
                 (Index_Action      => Context.Index_Action,
                  Client_Data       => Indexer_Data'(Refs, Context),
                  Index_Options     => CXIndexOpt_None,
                  Source_Filename   => String (File.Full_Name.all),
                  Command_Line_Args => Switches,
                  Unsaved_Files     => Unsaved_Files,
                  Options           => Clang_Options);
            end;

            Context.TU_Cache.Include
              (File, new TU_Cache_Record'(TU => TU, Version => 0));
            Context.LRU.Append (File);

            --  Remove elements from the cache if > LRU_Size

            if Context.TU_Cache.Length > LRU_Size then
               declare
                  F : constant GNATCOLL.VFS.Virtual_File :=
                    Context.LRU.First_Element;
               begin
                  Destroy (Context.TU_Cache.Reference (F));
                  Context.TU_Cache.Delete (F);
                  Context.LRU.Delete_First;
               end;
            end if;
         end if;

         GNAT.Strings.Free (C_Switches);
         return TU;
      end;
   end Translation_Unit;

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Clang_Module_Id.Destroy;
   end On_Project_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Clang_Module_Id := new Clang_Module_Record;
      Register_Module
        (Module => Clang_Module_Id,
         Kernel => Kernel);
      Add_Hook (Kernel, Project_View_Changed_Hook,
                Wrapper (On_Project_View_Changed'Access),
                Name => "libclang.project_view_changed");
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Clang_Module_Record) is
   begin
      for C of Id.Global_Cache loop
         Destroy (C);
         clang_disposeIndex (C.Clang_Indexer);
      end loop;
      Id.Global_Cache.Clear;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tu_Cache : in out TU_Cache_Access) is
   begin
      clang_disposeTranslationUnit (Tu_Cache.TU);
      Free (Tu_Cache);
   end Destroy;

end Language.Libclang;
