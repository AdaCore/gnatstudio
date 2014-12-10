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

with GNATCOLL.Utils; use GNATCOLL.Utils;
with Language.Libclang.Utils; use Language.Libclang.Utils;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPS.Editors; use GPS.Editors;
with clang_c_Index_h; use clang_c_Index_h;
with GPS.Kernel.Hooks; use GPS.Kernel.Hooks;
with GPS.Kernel; use GPS.Kernel;
with Interfaces.C.Strings;
with Language.Libclang_Tree; use Language.Libclang_Tree;
with Clang_Xref; use Clang_Xref;
with GPS.Kernel.Commands; use GPS.Kernel.Commands;

package body Language.Libclang is

   LRU_Size : constant := 16;

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("LANGUAGE_LIBCLANG", Off);

   Clang_Options : constant Clang_Translation_Unit_Flags :=
     Includebriefcommentsincodecompletion
     and Precompiledpreamble
     and Cachecompletionresults;

   function Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files)
      return Clang_Translation_Unit;

   procedure Free
   is new Ada.Unchecked_Deallocation (TU_Maps.Map, Tu_Map_Access);
   procedure Free
   is new Ada.Unchecked_Deallocation (LRU_Lists.List, LRU_Vector_Access);
   procedure Free
   is new Ada.Unchecked_Deallocation (TU_Cache_Record, TU_Cache_Access);
   procedure Free
   is new Ada.Unchecked_Deallocation
     (VFS_To_Refs_Maps.Map, VFS_To_Refs);
   procedure Free
   is new Ada.Unchecked_Deallocation
     (Symbol_To_Location_Maps.Map, Sym_To_Loc_Map);
   procedure Free
   is new Ada.Unchecked_Deallocation
     (Ref_Info_Vectors.Vector, Ref_Info_Vector);
   procedure Free
   is new Ada.Unchecked_Deallocation
     (Decl_Info_Vectors.Vector, Decl_Info_Vector);
   --  Love you so much Ada <3 <3 (okay)

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

   procedure Parse_One_File
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);

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
      Diagnostics : Clang_Diagnostic_Set) is null;

   procedure Entered_Main_File
     (Client_Data : in out Indexer_Data;
      File        : GNATCOLL.VFS.Virtual_File) is null;

   procedure Included_File
     (Client_Data : in out Indexer_Data;
      Included_File_Info : Clang_Included_File_Info) is null;

   procedure Started_Translation_Unit
     (Client_Data : in out Indexer_Data) is null;

   -----------------------
   -- Index_Declaration --
   -----------------------

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Decl_Info)
   is
      use Interfaces.C.Strings;
      use Interfaces.C;
      Loc : constant Clang_Location := +Info.loc;
      Sym : GNATCOLL.Symbols.Symbol;
      Info_Vector : Info_Vectors;
   begin
      if Is_From_Main_File (Loc)
      then

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
           (Decl_Info'(To_Offset_T (Loc), Info.isDefinition /= 0));

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
      Loc : constant Clang_Location := +Info.loc;
      Sym : GNATCOLL.Symbols.Symbol;
      Info_Vector : Info_Vectors;

   begin
      if Is_From_Main_File (Loc)
      then
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
           (Ref_Info'(Loc => To_Offset_T (Loc)));

      end if;
   end Index_Reference;

   package Indexer is new Source_File_Indexer
     (Client_Data_T => Indexer_Data);

   pragma Style_Checks (On);

   ---------------
   -- TU_Source --
   ---------------

   protected body TU_Source is

      ----------------------
      -- Translation_Unit --
      ----------------------

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

      function Context
        (Project : Project_Type) return Clang_Context
      is
      begin
         return Clang_Module_Id.Global_Cache.Element (Project);
      end Context;

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
            Indexer.Index_Translation_Unit
              (Index_Action  => Context.Index_Action,
               Client_Data   => Indexer_Data'(Refs, Context),
               Index_Options => CXIndexOpt_None,
               TU            => TU);
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

   package Virtual_File_Vectors is new Ada.Containers.Vectors
     (Positive, Virtual_File);

--     task type Indexer_Task_T
--     is
--        entry Index (Files : Virtual_File_Vectors.Vector;
--                     Kernel : Core_Kernel);
--        pragma Unreferenced (Index);
--     end Indexer_Task_T;
--
--     task body Indexer_Task_T is
--        Discard : Clang_Translation_Unit;
--        Current_Files : Virtual_File_Vectors.Vector;
--        Current_Kernel : Core_Kernel;
--     begin
--        loop
--           Put_Line ("IN Indexer_Task_T body !");
--           select
--              accept Index (Files : Virtual_File_Vectors.Vector;
--                            Kernel : Core_Kernel)
--              do
--                 Current_Files := Files;
--                 Current_Kernel := Kernel;
--              end Index;
--           or
--              terminate;
--           end select;
--           for File of Current_Files loop
--              Discard := TU_Source.Translation_Unit (Current_Kernel, File);
--  --              delay 0.01;
--           end loop;
--           Current_Files.Clear;
--           Current_Kernel := null;
--        end loop;
--     end Indexer_Task_T;
--
--     Indexer_Task : Indexer_Task_T;

   --------------------
   -- Parse_One_File --
   --------------------

   procedure Parse_One_File
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File)
   is
      Discard : Clang_Translation_Unit;
   begin
      Discard := TU_Source.Translation_Unit (Core_Kernel (Kernel), File);
   end Parse_One_File;

   -----------------------------
   -- On_Project_View_Changed --
   -----------------------------

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      P_Tree : constant GNATCOLL.Projects.Project_Tree_Access
        := Kernel.Get_Project_Tree;
      RP     : constant GNATCOLL.Projects.Project_Type := P_Tree.Root_Project;
      Files : File_Array_Access;
      Filtered_Files : Virtual_File_Vectors.Vector;
   begin
      Files := RP.Source_Files (Recursive => True);

      for F of Files.all
      loop
         if P_Tree.Info (F).Language = "c"
           or else P_Tree.Info (F).Language = "cpp"
         then
            Filtered_Files.Append (F);
         end if;
      end loop;

      declare
         Files_Array : constant File_Array_Access :=
           new File_Array
             (Filtered_Files.First_Index .. Filtered_Files.Last_Index);

      begin
         for I in Filtered_Files.First_Index .. Filtered_Files.Last_Index loop
            Files_Array (I) := Filtered_Files (I);
         end loop;

         Do_On_Each_File
           (Handle         => Kernel,
            Callback       => Parse_One_File'Access,
            Chunk_Size     => 1,
            Queue_Name     => "parse_clang_files_queue",
            Operation_Name => "parse clang files",
            Files          => Files_Array);
      end;

      Clang_Module_Id.Destroy;
   end On_Project_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Kernel.Databases.Lang_Specific_Databases.Include
        ("c", Clang_Database'(Kernel => Core_Kernel (Kernel)));
      Kernel.Databases.Lang_Specific_Databases.Include
        ("c++", Clang_Database'(Kernel => Core_Kernel (Kernel)));
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
