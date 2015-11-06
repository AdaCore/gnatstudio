------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Language.Libclang.Utils; use Language.Libclang.Utils;
with Ada.Unchecked_Deallocation;
with GPS.Editors; use GPS.Editors;
with GPS.Kernel.Hooks; use GPS.Kernel.Hooks;
with GPS.Kernel; use GPS.Kernel;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with Interfaces.C.Strings;
with Language.Libclang_Tree; use Language.Libclang_Tree;
with Clang_Xref; use Clang_Xref;
with String_Utils; use String_Utils;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Real_Time;
with Glib.Main;
with Commands; use Commands;
with Commands.Generic_Asynchronous;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
pragma Warnings (Off);
with System.Traceback.Symbolic;
pragma Warnings (On);
with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNAT.Regpat; use GNAT.Regpat;
with System.Multiprocessors; use System.Multiprocessors;

package body Language.Libclang is

   Clang_Options : constant Clang_Translation_Unit_Flags :=
     Includebriefcommentsincodecompletion
     and Precompiledpreamble
     and Cachecompletionresults;

   LRU_Size : constant := 16;
   Nb_Tasks : constant Natural := Natural'Min (Natural (Number_Of_CPUs), 6);

   Me : constant Trace_Handle := GNATCOLL.Traces.Create ("LIBCLANG");

   Activate_Clang_XRef : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("LIBCLANG_XREF", On);

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("LIBCLANG_DIAGNOSTICS", Off);

   function Parsing_Timeout_Handler return Boolean;

   package Task_Parser_Pool is new Pool
     (User_Data => Clang_Context_Access);

   use Task_Parser_Pool;

   procedure Enqueue_Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Default_Lang : String := "c++";
      Prio     : Parsing_Request_Priority := Low;
      Callback : Parse_Callback_Access := null);

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

   procedure Index_One_File;

   package Clang_Cache_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Virtual_File, Clang_Context_Access, Full_Name_Hash, "=");

   type Clang_Module_Record is new Module_ID_Record with record
      Global_Cache : Clang_Cache_Maps.Map;
      Parsing_Timeout_Id : Glib.Main.G_Source_Id;
      Parsing_Tasks : Parsing_Task_Array (1 .. Nb_Tasks);
   end record;

   overriding procedure Destroy (Id : in out Clang_Module_Record);

   Clang_Module_Id : access Clang_Module_Record := null;

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);

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
      S.Clear;
      Free (S);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Clang_Context)
   is
      Idx : constant Clang_Index :=
        Create_Index
          (True,
           --  We never want to display diagnostics via libclang,
           --  because it will just dump them on stdout
           Display_Diagnostics => False);
   begin
      Self.Clang_Indexer := Idx;
      Self.TU_Cache      := new TU_Maps.Map;
      Self.LRU           := new LRU_Lists.List;
      Self.Index_Action  := Create (Idx);
      Self.Sym_Table     := GNATCOLL.Symbols.Allocate;
      Self.Refs          := new VFS_To_Refs_Maps.Map;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : access Clang_Context)
   is
   begin
      for C of Self.TU_Cache.all loop
         Destroy (C);
      end loop;
      Self.TU_Cache.Clear;
      Free (Self.TU_Cache);
      Free (Self.LRU);

      for M of Self.Refs.all loop
         Destroy (M);
      end loop;

      Free (Self.Refs);
      GNATCOLL.Symbols.Free (Self.Sym_Table);
      Dispose (Self.Index_Action);
      clang_disposeIndex (Self.Clang_Indexer);
   end Destroy;

   ----------------
   -- Reset_Refs --
   ----------------

   procedure Reset_Refs (Self : access Clang_Context;
                         File_Name : String; New_Refs : Sym_To_Loc_Map)
   is
   begin
      if Self.Refs.Contains (+File_Name) then
         Destroy (Self.Refs.Reference (+File_Name));
      end if;
      Self.Refs.Include (+File_Name, New_Refs);
   end Reset_Refs;

   ------------
   -- Has_TU --
   ------------

   function Has_TU (Self : access Clang_Context;
                    File_Name : String) return Boolean is
   begin
      return Self.TU_Cache.Contains (+File_Name)
        and then Self.TU_Cache.Element (+File_Name).TU /= No_Translation_Unit;
   end Has_TU;

   ------------
   -- Get_TU --
   ------------

   function Get_TU
     (Self : access Clang_Context;
      File_Name : String) return Translation_Unit_Wrapper'Class
   is
      U_File_Name : constant Unbounded_String := +File_Name;
      Cache : TU_Cache_Access;
   begin
      return Ret : Translation_Unit_Wrapper do
         if Self.TU_Cache.Contains (U_File_Name) then
            Cache := Self.TU_Cache.Element (U_File_Name);
         else
            Cache :=
              new TU_Cache_Record'(TU => No_Translation_Unit, Version => 0,
                                   Is_Ready => False);
            Self.TU_Cache.Include (+File_Name, Cache);
         end if;
         Ret.Cache := Cache;
      end return;
   end Get_TU;

   ---------------------
   -- Add_TU_To_Cache --
   ---------------------

   procedure Add_TU_To_Cache
     (Self : access Clang_Context;
      File_Name : String;
      Translation_Unit : Clang_Translation_Unit;
      Version : Integer := 0)
   is
      U_File_Name : constant Unbounded_String := +File_Name;
      Cache : TU_Cache_Access;
      use LRU_Lists;
   begin
      Trace (Me, "Adding TU to cache " & File_Name);

      if Self.TU_Cache.Contains (U_File_Name) then
         Cache := Self.TU_Cache.Element (U_File_Name);

         Trace (Me, "SELF CONTAINS TU, "
                & Boolean'Image (Cache.TU /= No_Translation_Unit) & " "
                & Boolean'Image (Cache.TU /= Translation_Unit));

         if Cache.TU /= No_Translation_Unit
           and then Cache.TU /= Translation_Unit
         then
            Trace (Me, "Freeing old TU " & File_Name);
            Dispose (Cache.TU);
         end if;

         Cache.TU := Translation_Unit;
         Cache.Is_Ready := True;
      else
         Cache :=
           new TU_Cache_Record'(TU => Translation_Unit, Version => Version,
                                Is_Ready => True);
         Self.TU_Cache.Include (U_File_Name, Cache);
      end if;

      --  Handle LRU bookkeeping

      declare
         C : LRU_Lists.Cursor := Self.LRU.Find (U_File_Name);
      begin
         --  If the file name is already in the LRU, we want to put it back at
         --  the beginning of the list, so remove the existing element

         if C /= No_Element then
            Delete (Self.LRU.all, C);
         end if;

         --  In every case, append the file name at the beginning of the list
         Self.LRU.Append (U_File_Name);
      end;

      --  Remove elements from the cache if > LRU_Size

      if Self.LRU.Length > LRU_Size then
         declare
            F : constant Unbounded_String := Self.LRU.First_Element;
         begin
            --  We are not actually removing element from the cache, so that we
            --  can trace dangling pointers
            Trace (Me, "Removing " & (+F) & " from cache");
            begin
               Dispose (Self.TU_Cache.Element (F).TU);
               Self.TU_Cache.Element (F).Is_Ready := False;
            exception
               when E : others =>
                  Trace (Me, "EXCEPTION !");
                  Trace (Me, System.Traceback.Symbolic.Symbolic_Traceback (E));
            end;
            Self.LRU.Delete_First;
         end;
      end if;
   end Add_TU_To_Cache;

   type Indexer_Data is record
      Syms_To_Locs : Sym_To_Loc_Map;
      Sym_Table    : Symbol_Table_Access;
   end record;

   procedure Index_Reference
     (Client_Data : in out Indexer_Data;
      Info   : Clang_Ref_Info);
   function Info_Vector
     (Map : Sym_To_Loc_Map; Sym : GNATCOLL.Symbols.Symbol) return Info_Vectors;
   procedure Index_Declaration
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Decl_Info);

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
      Diags : Clang_Diagnostic_Set);

   procedure Diagnostic
     (Client_Data : in out Indexer_Data;
      Diags : Clang_Diagnostic_Set)
   is
      pragma Unreferenced (Client_Data);
   begin
      for I in 0 .. clang_getNumDiagnosticsInSet (Diags) loop
         declare
            D : constant CXDiagnostic :=
              clang_getDiagnosticInSet (Diags, I);
            use type Interfaces.C.unsigned;
         begin
            Trace
              (Diagnostics,
               To_String
                 (clang_formatDiagnostic
                      (D, DisplaySourceLocationh
                       or DisplayColumn or DisplaySourceRanges)));
         end;
      end loop;
   end Diagnostic;

   procedure Entered_Main_File
     (Client_Data : in out Indexer_Data;
      File        : GNATCOLL.VFS.Virtual_File) is null;

   procedure Included_File
     (Client_Data : in out Indexer_Data;
      Included_File_Info : Clang_Included_File_Info) is null;

   procedure Started_Translation_Unit
     (Client_Data : in out Indexer_Data) is null;

   ---------------------
   -- Get_Info_Vector --
   ---------------------

   function Info_Vector
     (Map : Sym_To_Loc_Map; Sym : GNATCOLL.Symbols.Symbol) return Info_Vectors
   is
      Info_Vector : Info_Vectors;
   begin
      if not Map.Contains (Sym) then
         Info_Vector := (new Decl_Info_Vectors.Vector,
                         new Ref_Info_Vectors.Vector);
         Map.Include (Sym, Info_Vector);
      else
         Info_Vector := Map.Element (Sym);
      end if;
      return Info_Vector;
   end Info_Vector;

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
      use Cursors_Arrays;
   begin
      if Is_From_Main_File (Loc) then

         --  Add info for base specifiers references in C++ class declarations,
         --  because for some reasons they're not visited as references by the
         --  clang indexing process.

         if Info.entityInfo.kind = CXIdxEntity_CXXClass then
            for C of
              Get_Children
                (Clang_Cursor (Info.cursor), CXXBaseSpecifier)
            loop
               Sym :=
                 Client_Data.Sym_Table.Find (USR (Referenced (C)));
               Info_Vector (Client_Data.Syms_To_Locs, Sym).Refs.Append
                 (Ref_Info'
                    (To_Offset_T (Location (C)),
                     Small_Cursor_Kind (Kind (C))));
            end loop;
         end if;

         Sym := Client_Data.Sym_Table.Find (Value (Info.entityInfo.USR));

         Info_Vector (Client_Data.Syms_To_Locs, Sym).Decls.Append
           (Decl_Info'(To_Offset_T (Loc), Info.isDefinition /= 0,
            Small_Cursor_Kind (Info.cursor.kind)));

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
   begin
      if Is_From_Main_File (Loc) then
         Sym :=
           Client_Data.Sym_Table.Find (Value (Info.referencedEntity.USR));

         Info_Vector (Client_Data.Syms_To_Locs, Sym).Refs.Append
           (Ref_Info'(To_Offset_T (Loc),
            Small_Cursor_Kind (Info.cursor.kind)));

      end if;
   end Index_Reference;

   package Indexer is new Source_File_Indexer
     (Client_Data_T => Indexer_Data);

   function Full_Name (F : Virtual_File) return String
   is
     (String (F.Full_Name.all));

   ----------------------
   -- Translation_Unit --
   ----------------------

   procedure Enqueue_Translation_Unit
     (Kernel       : Core_Kernel;
      File         : GNATCOLL.VFS.Virtual_File;
      Reparse      : Boolean := False;
      Default_Lang : String := "c++";
      Prio         : Parsing_Request_Priority := Low;
      Callback     : in out Parse_Callback_Access)
   is
      Buffer : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File, False, False, False, False);
      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Kernel.Registry.Tree.Info_Set
             (File).First_Element);
      Context : Clang_Context_Access;
      Full_File_Name : constant String := Full_Name (File);
   begin
      if Reparse
        and then Buffer /= Nil_Editor_Buffer
        and then Clang_Module_Id.Global_Cache.Contains
          (F_Info.Project.Project_Path)
      then
         Context := Clang_Module_Id.Global_Cache.Element
           (F_Info.Project.Project_Path);

         if Context.Has_TU (Full_File_Name) then
            declare
               Cache_Val : constant Translation_Unit_Wrapper'Class
                 := Context.Get_TU (Full_File_Name);
            begin
               if Cache_Val.Cache.Version < Buffer.Version then
                  declare
                     Buffer_Text : constant
                       Ada.Strings.Unbounded.String_Access :=
                       new String'(Buffer.Get_Chars);
                  begin
                     Enqueue_Translation_Unit
                       (Kernel,
                        File,
                        (0 => Create_Unsaved_File
                             (String (File.Full_Name.all), Buffer_Text)),
                        Default_Lang, Prio => Prio, Callback => Callback);
                     Cache_Val.Cache.Version := Buffer.Version;
                     return;
                  end;
               else
                  if Callback /= null then
                     Callback.Call (File, Cache_Val.Cache.TU);
                     Free (Callback);
                  end if;
               end if;
            end;
         end if;

      end if;
      Enqueue_Translation_Unit (Kernel, File, No_Unsaved_Files, Prio => Prio,
                                Callback => Callback);
   end Enqueue_Translation_Unit;

   ----------------------
   -- Translation_Unit --
   ----------------------

   function Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Reparse : Boolean := False;
      Default_Lang : String := "c++")
      return Clang_Translation_Unit
   is
      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Kernel.Registry.Tree.Info_Set
             (File).First_Element);
      Context : Clang_Context_Access;

      Ret : Clang_Translation_Unit;
      Callback : Parse_Callback_Access := null;
   begin
      Enqueue_Translation_Unit
        (Kernel, File, Reparse, Default_Lang, Prio => High,
         Callback => Callback);

      if Clang_Module_Id /= null then
         Context :=
           Clang_Module_Id.Global_Cache.Element (F_Info.Project.Project_Path);
         Ret := Context.Get_TU (String (File.Full_Name.all)).Get_Blocking;
         return Ret;
      end if;

      return No_Translation_Unit;
   end Translation_Unit;

   -------------
   -- Context --
   -------------

   function Context
     (Project : Project_Type) return Clang_Context_Access
   is
   begin
      return Clang_Module_Id.Global_Cache.Element (Project.Project_Path);
   end Context;

   Empty_String_Array : constant GNATCOLL.Utils.Unbounded_String_Array (1 .. 0)
     := (others => <>);

   ------------
   -- Get_TU --
   ------------

   function Get_TU
     (C : access Clang_Context;
      File_Name : String) return Clang_Translation_Unit
   is
      Cache_Entry : constant Translation_Unit_Wrapper'Class :=
        C.Get_TU (File_Name);
   begin
      return Cache_Entry.Get;
   end Get_TU;

   Cpp_Header_Regex : constant Regpat.Pattern_Matcher :=
     Compile (".*?include\/c\+\+\/\d\..*?\/.*$");

   ------------------------------
   -- Enqueue_Translation_Unit --
   ------------------------------

   procedure Enqueue_Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Default_Lang : String := "c++";
      Prio : Parsing_Request_Priority := Low;
      Callback     : Parse_Callback_Access := null)
   is
      --  ??? We should fill other unsaved_files! ??? Or should we ? I think
      --  that filling the current file as unsaved is enough. We can, at
      --  least in the first iteration of libclang, ask the user to save
      --  the other files if he expects to get completion. RA

      Kernel_Lang      : constant String :=
        Kernel.Lang_Handler.Get_Language_From_File (File);

      function Lang return String;
      function Lang return String is
      begin
         if Match (Cpp_Header_Regex, String (File.Full_Name.all)) then
            return "c++";
         elsif Kernel_Lang = "" then
            return Default_Lang;
         else
            return Kernel_Lang;
         end if;
      end Lang;

      C_Switches       : GNAT.Strings.String_List_Access;
      Ignored          : Boolean;

      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Kernel.Registry.Tree.Info_Set
             (File).First_Element);

      Context : Clang_Context_Access;
      File_Name : constant String := Full_Name (File);
      Request : Parsing_Request;

   begin

      if not Clang_Module_Id.Global_Cache.Contains
        (F_Info.Project.Project_Path)
      then
         Context := new Clang_Context;
         Context.Initialize;
         Clang_Module_Id.Global_Cache.Insert
           (F_Info.Project.Project_Path, Context);
      else
         Context :=
           Clang_Module_Id.Global_Cache.Element (F_Info.Project.Project_Path);
      end if;

      if Unsaved_Files = No_Unsaved_Files
        and then Context.Has_TU (Full_Name (File))
      then
         return;
      end if;

      --  Retrieve the switches for this file
      Switches (F_Info.Project, "compiler", File, Lang, C_Switches, Ignored);

      declare
         The_Switches : Unbounded_String_Array (C_Switches'Range);
         TU           : Clang_Translation_Unit;
         TU_Wrapper   : constant Translation_Unit_Wrapper'Class
           := Context.Get_TU (File_Name);
      begin
         for J in C_Switches'Range loop
            The_Switches (J) := +C_Switches (J).all;
         end loop;

         if Context.Has_TU (File_Name) then

            --  If the key is in the cache, we know that File_Content is not
            --  null, so we want to reparse

            TU := TU_Wrapper.Cache.TU;
            TU_Wrapper.Cache.TU := No_Translation_Unit;
            TU_Wrapper.Cache.Is_Ready := False;

            Request := new Parsing_Request_Record'
              (TU            => TU,
               Context       => Context,
               Indexer       => Context.Clang_Indexer,
               Kind          => Reparse,
               Unsaved_Files => new Unsaved_File_Array'(Unsaved_Files),
               Options       => Clang_Options,
               File_Name     => +File_Name,
               Prio          => Prio,
               Project_Name  =>
                 +(String (F_Info.Project.Project_Path.Full_Name.all)),
               Callbacks     => <>);

            if Callback /= null then
               Request.Callbacks.Append (Callback);
            end if;

            Parsing_Request_Queue.Enqueue (Request);

         else
            --  In the other case, this is the first time we're parsing this
            --  file

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
                 & Get_Compiler_Search_Paths_Switches
                 (Kernel, F_Info.Project, Lang)

                 & (if Lang in "c++" | "cpp" then (+"-x", +"c++")
                    else Empty_String_Array);
            begin

               Trace (Diagnostics, "=== FILE     : " & File_Name);
               Trace (Diagnostics, "=== LANGUAGE : " & Lang);
               Trace (Diagnostics, "======= BEGIN SWITCHES ==============");
               for Sw of Switches loop
                  Trace (Diagnostics, +Sw);
               end loop;
               Trace (Diagnostics, "======= END SWITCHES ==============");

               Request := new Parsing_Request_Record'
                 (Kind         => Parse,
                  Context      => Context,
                  Indexer      => Context.Clang_Indexer,
                  File_Name    => +File_Name,
                  Switches     => new Unbounded_String_Array'(Switches),
                  Options      => Clang_Options,
                  Prio         => Prio,
                  Project_Name =>
                    +(String (F_Info.Project.Project_Path.Full_Name.all)),
                  Callbacks    => <>);

               if Callback /= null then
                  Request.Callbacks.Append (Callback);
               end if;

               Parsing_Request_Queue.Enqueue (Request);
            end;
         end if;

         GNAT.Strings.Free (C_Switches);
      end;
   end Enqueue_Translation_Unit;

   package Virtual_File_Vectors is new Ada.Containers.Vectors
     (Positive, Virtual_File);

   type Parse_Files_Data_Type is record
      Max_Idx, Current_Idx : Natural := 0;
   end record;
   type Parse_Files_Data_Type_Access is access all Parse_Files_Data_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Parse_Files_Data_Type, Parse_Files_Data_Type_Access);

   -------------------------
   -- Parse_Files_Iterate --
   -------------------------

   procedure Parse_Files_Iterate
     (Data    : in out Parse_Files_Data_Type_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type);

   procedure Parse_Files_Iterate
     (Data    : in out Parse_Files_Data_Type_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
   begin
      Command.Set_Progress
        (Progress_Record'(Running, Data.Current_Idx, Data.Max_Idx));

      if Data.Current_Idx = Data.Max_Idx then
         Result := Success;
      else
         Result := Execute_Again;
      end if;
   end Parse_Files_Iterate;

   package Parse_Files_Command is
     new Commands.Generic_Asynchronous (Parse_Files_Data_Type_Access);

   package Parse_Callback_Package is
      type Parse_For_Cache is new Parse_Callback with record
         Command : Parse_Files_Data_Type_Access;
      end record;

      overriding procedure Call
        (Self : access Parse_For_Cache;
         File : Virtual_File; TU : Clang_Translation_Unit);
   end Parse_Callback_Package;

   package body Parse_Callback_Package is
      overriding procedure Call
        (Self : access Parse_For_Cache;
         File : Virtual_File; TU : Clang_Translation_Unit) is
         pragma Unreferenced (TU);
      begin
         Trace (Me, "Finished parsing " & String (File.Full_Name.all));
         Self.Command.Current_Idx := Self.Command.Current_Idx + 1;
      end Call;
   end Parse_Callback_Package;

   -----------------
   -- Clean_Cache --
   -----------------
   procedure Clean_Cache (Id : in out Clang_Module_Record);

   procedure Clean_Cache (Id : in out Clang_Module_Record) is
   begin
      Trace (Me, "Cleaning all cache for libclang");
      for C of Id.Global_Cache loop
         C.all.Destroy;
      end loop;
      Id.Global_Cache.Clear;
   end Clean_Cache;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      P_Tree : constant GNATCOLL.Projects.Project_Tree_Access
        := Kernel.Get_Project_Tree;
      RP     : constant GNATCOLL.Projects.Project_Type := P_Tree.Root_Project;
      Files : File_Array_Access;
      Filtered_Files : Virtual_File_Vectors.Vector;

   begin

      --  Stop all the tasks

      for I in Clang_Module_Id.Parsing_Tasks'Range loop
         Clang_Module_Id.Parsing_Tasks (I).Stop;
      end loop;

      --  Remove all the pending requests

      declare
         Dummy_Request : Parsing_Request;
      begin
         while Parsing_Request_Queue.Length > 0 loop
            Parsing_Request_Queue.Dequeue (Dummy_Request);
         end loop;
      end;

      --  Remove all the pending responses. We do that the simple way for the
      --  moment but we don't need to index at this stage

      declare
         Dummy : constant Boolean := Parsing_Timeout_Handler;
      begin
         null;
      end;

      --  Clear the cache

      Clean_Cache (Clang_Module_Id.all);

      --  Restart all the tasks

      for I in Clang_Module_Id.Parsing_Tasks'Range loop
         Clang_Module_Id.Parsing_Tasks (I).Start;
      end loop;

      --  We actually only want to index if cross refs are activated

      if not Active (Activate_Clang_XRef) then
         return;
      end if;

      --  Fetch all of the project's files

      Files := RP.Source_Files (Recursive => True);

      --  Only keep those who are relevant to libclang

      for F of Files.all loop
         declare
            Info_Set : constant File_Info_Set := P_Tree.Info_Set (F);
            Language : constant String :=
              File_Info (Info_Set.First_Element).Language;

         begin
            if Language = "c"
              or else Language = "cpp"
              or else Language = "c++"
            then
               Filtered_Files.Append (F);
            end if;
         end;
      end loop;

      Unchecked_Free (Files);
      declare
         Command_Data : constant Parse_Files_Data_Type_Access
           := new Parse_Files_Data_Type;
         Command : Parse_Files_Command.Generic_Asynchronous_Command_Access;
      begin
         --  Call Translation_Unit on them to populate the cache for the file
         Command_Data.Max_Idx := Natural (Filtered_Files.Length);
         for F of Filtered_Files loop
            declare
               Callback : Parse_Callback_Access :=
                 new Parse_Callback_Package.Parse_For_Cache'
                   (Command => Command_Data);
            begin
               Enqueue_Translation_Unit (Core_Kernel (Kernel), F, False,
                                         Callback => Callback);
            end;
         end loop;

         --  Only start the monitoring command if there actually are files to
         --  parse
         if not Filtered_Files.Is_Empty then
            Parse_Files_Command.Create (Command, "libclang parsing files",
                                        Command_Data,
                                        Parse_Files_Iterate'Access);

            Launch_Background_Command (Kernel          => Kernel,
                                       Command         => Command,
                                       Active          => False,
                                       Show_Bar        => True);
         end if;
      end;
   end Execute;

   --------------------
   -- Index_One_File --
   --------------------

   procedure Index_One_File is
      use Ada.Real_Time;
      T1 : constant Ada.Real_Time.Time := Clock;
      Refs : constant Sym_To_Loc_Map := new Symbol_To_Location_Maps.Map;
      Response : Parsing_Response;
   begin
      Parsing_Response_Queue.Dequeue (Response);

      Response.Context.Add_TU_To_Cache
        (+Response.File_Name, Response.TU);
      Trace (Me, "Start indexing for file " & (+Response.File_Name));
      Indexer.Index_Translation_Unit
        (Index_Action  =>
           Response.Context.Index_Action,
         Client_Data   =>
           Indexer_Data'(Refs, Response.Context.Sym_Table),
         Index_Options => CXIndexOpt_None,
         TU            =>
           Response.Context.TU_Cache.Element (Response.File_Name).TU);

      --  Reset the references cache for file, and get the new cache
      Response.Context.Reset_Refs (+Response.File_Name, Refs);

      --  Call all the callbacks and free them
      for CB of Response.Callbacks loop
         CB.Call (Create (Filesystem_String (+Response.File_Name)),
                  Response.TU);
         Free (CB);
      end loop;

      Trace
        (Me, "Indexing finished, took "
         & Duration'Image (To_Duration (Clock - T1)));
   end Index_One_File;

   -----------------------------
   -- Parsing_Timeout_Handler --
   -----------------------------

   function Parsing_Timeout_Handler return Boolean is
   begin

      while Parsing_Response_Queue.Current_Use > 0 loop
         Index_One_File;
      end loop;
      return True;
   end Parsing_Timeout_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin

      --  Register cross references databases for c and c++
      if Active (Activate_Clang_XRef) then
         Kernel.Databases.Lang_Specific_Databases.Include
           ("c", Clang_Database'(Kernel => Core_Kernel (Kernel)));

         Kernel.Databases.Lang_Specific_Databases.Include
           ("c++", Clang_Database'(Kernel => Core_Kernel (Kernel)));
      end if;

      Clang_Module_Id := new Clang_Module_Record;
      Register_Module
        (Clang_Module_Id, Kernel, "clang_module", Default_Priority);

      Clang_Module_Id.Parsing_Timeout_Id :=
        Glib.Main.Timeout_Add (100, Parsing_Timeout_Handler'Access);

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Clang_Module_Record) is
   begin
      Clean_Cache (Id);
      for T of Clang_Module_Id.Parsing_Tasks loop
         T.Finish;
      end loop;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tu_Cache : in out TU_Cache_Access) is
   begin
      if Tu_Cache.TU /= No_Translation_Unit then
         Dispose (Tu_Cache.TU);
      end if;
      Free (Tu_Cache);
   end Destroy;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Translation_Unit_Wrapper) return Clang_Translation_Unit
   is
   begin
      return Self.Cache.TU;
   end Get;

   ------------------
   -- Get_Blocking --
   ------------------

   function Get_Blocking
     (Self : Translation_Unit_Wrapper) return Clang_Translation_Unit
   is
   begin
      loop
         if Self.Cache.Is_Ready = False then
            declare
               Dummy : Boolean := Parsing_Timeout_Handler;
            begin
               null;
            end;
            delay 0.01;
         else
            return Self.Cache.TU;
         end if;
      end loop;
   end Get_Blocking;

end Language.Libclang;
