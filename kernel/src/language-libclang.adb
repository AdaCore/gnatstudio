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

package body Language.Libclang is

   LRU_Size : constant := 32;

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("COMPLETION_LIBCLANG.DIAGNOSTICS", Off);

   Clang_Options : constant Clang_Translation_Unit_Flags :=
     Includebriefcommentsincodecompletion
     and Precompiledpreamble
     and Cachecompletionresults;

   function Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files)
      return Clang_Translation_Unit;

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

   procedure Initialize (Ctx : in out Clang_Context);

   procedure Destroy (Ctx : in out Clang_Context);

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Ctx : in out Clang_Context) is
   begin
      Ctx.Clang_Indexer := Create_Index (True, Active (Diagnostics));
      Ctx.TU_Cache := new TU_Maps.Map;
      Ctx.LRU := new LRU_Lists.List;
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
   end Destroy;

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
      begin
         for J in C_Switches'Range loop
            The_Switches (J) := To_Unbounded_String (C_Switches (J).all);
         end loop;

         if Context.TU_Cache.Contains (File) then
            --  If the key is in the cache, we know that File_Content is not
            --  null, so we want to reparse
            TU := Context.TU_Cache.Element (File).TU;
            Dummy := Reparse_Translation_Unit (TU, Unsaved_Files,
                                               Options => Clang_Options);
         else
            --  In the other case, this is the first time we're parsing this
            --  file
            TU := Parse_Translation_Unit
              (Index             => Context.Clang_Indexer,
               Source_Filename   => String (File.Full_Name.all),
               Command_Line_Args =>

               --  We pass to libclang a list of switches made of:
               --  ... the C/C++ switches specified in this project
               The_Switches

               --  ... a -I<dir> for each directory in the subprojects
               --  of this project
               & Get_Project_Source_Dirs
                 (Kernel, F_Info.Project, Lang)

               --  ... a -I<dir> for each dir in the compiler search path
               & Get_Compiler_Search_Paths
                 (Kernel, F_Info.Project, Lang),

               Unsaved_Files     => Unsaved_Files,
               Options           => Clang_Options);

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
