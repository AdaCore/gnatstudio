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

with Ada.Task_Identification; use Ada.Task_Identification;
with Language.Libclang.Utils; use Language.Libclang.Utils;
with Ada.Unchecked_Deallocation;
with GPS.Editors; use GPS.Editors;
with clang_c_Index_h; use clang_c_Index_h;
with GPS.Kernel.Hooks; use GPS.Kernel.Hooks;
with GPS.Kernel; use GPS.Kernel;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with Interfaces.C.Strings;
with Language.Libclang_Tree; use Language.Libclang_Tree;
with Clang_Xref; use Clang_Xref;
with GPS.Kernel.Commands; use GPS.Kernel.Commands;
with String_Utils; use String_Utils;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Glib.Main;

package body Language.Libclang is

   LRU_Size : constant := 16;

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("LANGUAGE_LIBCLANG", Off);

   function Parsing_Timeout_Handler return Boolean;

   Clang_Options : constant Clang_Translation_Unit_Flags :=
     Includebriefcommentsincodecompletion
     and Precompiledpreamble
     and Cachecompletionresults;

   function Get_Priority (PR : Parsing_Request) return Parsing_Request_Priority
   is (PR.Prio);

   function Before (Left, Right : Parsing_Request_Priority) return Boolean
   is (Parsing_Request_Priority'Pos (Left)
       > Parsing_Request_Priority'Pos (Right));

   package Parsing_Request_Queues_Interface
   is new Ada.Containers.Synchronized_Queue_Interfaces (Parsing_Request);
   package Parsing_Request_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
     (Parsing_Request_Queues_Interface, Parsing_Request_Priority);

   package Parsing_Response_Queues_Interface
   is new Ada.Containers.Synchronized_Queue_Interfaces (Parsing_Response);
   package Parsing_Response_Queues
   is new Ada.Containers.Unbounded_Synchronized_Queues
     (Parsing_Response_Queues_Interface);

   Parsing_Request_Queue  : Parsing_Request_Queues.Queue;
   Parsing_Response_Queue : Parsing_Response_Queues.Queue;

   task type Parsing_Task is
      entry Start;
      entry Stop;
      entry Finish;
   end Parsing_Task;

   task body Parsing_Task is
      Request   : Parsing_Request;
      Dummy     : Boolean;
      TU        : Clang_Translation_Unit;
      Stopped   : Boolean := False;
      Do_Finish : Boolean := False;
   begin
      Put_Line ("IN PARSING TASK " & Image (Current_Task));
      loop
         select
            accept Start do Stopped := False; end Start;
         or
            accept Stop do Stopped := True; end Stop;
         or
            accept Finish do
               Put_Line ("TASK " & Image (Current_Task) & "FINISHING");
               Do_Finish := True;
            end Finish;
         or
            delay 0.1;
         end select;

         if Do_Finish then
            goto End_Label;
         end if;

         if Stopped then
            delay 0.1;
            goto Cont;
         end if;

         select
            Parsing_Request_Queue.Dequeue (Request);
         or
            --  If we cannot dequeue, timeout and restart the loop, so that the
            --  entries get a chance to get evaluated
            delay 0.1;
            goto Cont;
         end select;

         Put_Line ("PARSING " & (+Request.File_Name));
         Put_Line ("PRIORITY : " & Request.Prio'Img);
         case Request.Kind is
            when Parse =>
               TU := Parse_Translation_Unit
                 (Request.Context.Clang_Indexer,
                  Source_Filename   => +Request.File_Name,
                  Command_Line_Args => Request.Switches.all,
                  Unsaved_Files     => No_Unsaved_Files,
                  Options           => Clang_Options);
            when Reparse =>
               Put_Line ("REPARSING");
               Dummy := Reparse_Translation_Unit
                 (Request.TU, Request.Unsaved_Files.all,
                  Options => Request.Options);
         end case;
         Request.Cache_Entry.TU := TU;
         Parsing_Response_Queue.Enqueue
           (Parsing_Response'(TU, Request.File_Name, Request.Context));

         <<Cont>>

      end loop;

      <<End_Label>>

   exception
      when others =>
         Put_Line ("EXCEPTION ======================== !!!!!!!!!!!!!!!!!!!");
   end Parsing_Task;

   Nb_Tasks      : constant := 4;
   Parsing_Tasks : array (1 .. Nb_Tasks) of Parsing_Task;

   procedure Enqueue_Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Default_Lang : String := "c++";
      Prio : Parsing_Request_Priority := Low);

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

   package Clang_Cache_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Virtual_File, Clang_Context_Access, Full_Name_Hash, "=");

   type Clang_Module_Record is new Module_ID_Record with record
      Global_Cache : Clang_Cache_Maps.Map;
      Parsing_Timeout_Id : Glib.Main.G_Source_Id;
   end record;

   overriding procedure Destroy (Id : in out Clang_Module_Record);

   Clang_Module_Id : access Clang_Module_Record := null;

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class);

   procedure Parse_One_File
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File);

   -------------------
   -- P_Request_Set --
   -------------------

   protected body P_Request_Set is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Map := new Request_Maps.Map;
      end Initialize;

      --------------
      -- Contains --
      --------------

      function Contains (File_Name : String) return Boolean is
      begin
         return Map.Contains (+File_Name);
      end Contains;

      ---------
      -- Get --
      ---------

      function Get (File_Name : String) return Parsing_Request is
      begin
         return Map.Element (+File_Name);
      end Get;

      ---------
      -- Add --
      ---------

      procedure Add (Request : Parsing_Request) is
      begin
         Map.Include (Request.File_Name, Request);
      end Add;

      ------------
      -- Remove --
      ------------

      procedure Remove (File_Name : String) is
      begin
         Map.Delete (+File_Name);
      end Remove;

   end P_Request_Set;

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
        Create_Index (True, Active (Diagnostics));
   begin
      Self.Clang_Indexer := Idx;
      Self.TU_Cache      := new TU_Maps.Map;
      Self.LRU           := new LRU_Lists.List;
      Self.Index_Action  := Create (Idx);
      Self.Sym_Table     := GNATCOLL.Symbols.Allocate;
      Self.Refs          := new VFS_To_Refs_Maps.Map;
      Self.Pending_Requests.Initialize;
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
              new TU_Cache_Record'(TU => No_Translation_Unit, Version => 0);
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
   begin
      if Self.TU_Cache.Contains (U_File_Name) then
         Cache := Self.TU_Cache.Element (U_File_Name);
         Cache.TU := Translation_Unit;
      else
         Cache :=
           new TU_Cache_Record'(TU => Translation_Unit, Version => Version);
         Self.TU_Cache.Include (U_File_Name, Cache);
      end if;

      Self.LRU.Append (U_File_Name);

      --  Remove elements from the cache if > LRU_Size

      if Self.TU_Cache.Length > LRU_Size then
         declare
            F : constant Unbounded_String := Self.LRU.First_Element;
         begin
            Destroy (Self.TU_Cache.Reference (F));
            Self.TU_Cache.Delete (F);
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
      Diagnostics : Clang_Diagnostic_Set) is null;

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
      if Is_From_Main_File (Loc)
      then

         --  Add info for base specifiers references in C++ class declarations,
         --  because for some reasons they're not visited as references by the
         --  clang indexing process.

         if Info.entityInfo.kind = CXIdxEntity_CXXClass then
            for C of
              Get_Children
                (Clang_Cursor (Info.cursor), CXCursor_CXXBaseSpecifier)
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
      if Is_From_Main_File (Loc)
      then
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
      Prio         : Parsing_Request_Priority := Low)
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
                       (Kernel, File,
                        (0 => Create_Unsaved_File
                             (String (File.Full_Name.all), Buffer_Text)),
                        Default_Lang, Prio => Prio);
                     Cache_Val.Cache.Version := Buffer.Version;
                  end;
               end if;
            end;
         end if;

      end if;
      Enqueue_Translation_Unit (Kernel, File, No_Unsaved_Files, Prio => Prio);
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

   begin
      Enqueue_Translation_Unit
        (Kernel, File, Reparse, Default_Lang, Prio => High);

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

   ----------------------
   -- Translation_Unit --
   ----------------------

   procedure Enqueue_Translation_Unit
     (Kernel : Core_Kernel;
      File : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Default_Lang : String := "c++";
      Prio : Parsing_Request_Priority := Low)
   is
      --  ??? We should fill other unsaved_files! ??? Or should we ? I think
      --  that filling the current file as unsaved is enough. We can, at
      --  least in the first iteration of libclang, ask the user to save
      --  the other files if he expects to get completion. RA

      Kernel_Lang      : constant String :=
        Kernel.Lang_Handler.Get_Language_From_File (File);
      Lang : constant String :=
        (if Kernel_Lang = "" then Default_Lang else Kernel_Lang);

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

            Request := new Parsing_Request_Record'
              (TU            => TU,
               Context       => Context,
               Kind          => Reparse,
               Unsaved_Files => new Unsaved_File_Array'(Unsaved_Files),
               Options       => Clang_Options,
               File_Name     => +File_Name,
               Cache_Entry   => TU_Wrapper.Cache,
               Prio          => Prio);
            Parsing_Request_Queue.Enqueue (Request);
            Context.Pending_Requests.Add (Request);

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
                 (Kernel, F_Info.Project, Lang)

                 & (if Lang in "c++" | "cpp" then (+"-x", +"c++")
                    else Empty_String_Array);
            begin
               --  In the other case, this is the first time we're parsing this
               --  file
               Request := new Parsing_Request_Record'
                 (Kind      => Parse,
                  Context   => Context,
                  File_Name => +File_Name,
                  Switches  => new Unbounded_String_Array'(Switches),
                  Options   => Clang_Options,
                  Cache_Entry => TU_Wrapper.Cache,
                  Prio        => Prio);
               Parsing_Request_Queue.Enqueue (Request);
            end;
         end if;

         GNAT.Strings.Free (C_Switches);
      end;
   end Enqueue_Translation_Unit;

   package Virtual_File_Vectors is new Ada.Containers.Vectors
     (Positive, Virtual_File);

   --------------------
   -- Parse_One_File --
   --------------------

   procedure Parse_One_File
     (Kernel : access Kernel_Handle_Record'Class; File : Virtual_File)
   is
   begin
      Enqueue_Translation_Unit (Core_Kernel (Kernel), File, False);
   end Parse_One_File;

   -----------------
   -- Clean_Cache --
   -----------------
   procedure Clean_Cache (Id : in out Clang_Module_Record);

   procedure Clean_Cache (Id : in out Clang_Module_Record) is
   begin
      for C of Id.Global_Cache loop
         C.all.Destroy;
      end loop;
      Id.Global_Cache.Clear;
   end Clean_Cache;

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
      while Parsing_Request_Queue.Current_Use > 0 loop
         delay 0.1;
      end loop;

      --  Stop all the tasks

      for I in Parsing_Tasks'Range loop
         Parsing_Tasks (I).Stop;
      end loop;

      --  Remove all the pending requests

      declare
         Dummy_Request : Parsing_Request;
      begin
         while Parsing_Request_Queue.Current_Use > 0 loop
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

      for I in Parsing_Tasks'Range loop
         Parsing_Tasks (I).Start;
      end loop;

      --  Fetch all of the project's files

      Files := RP.Source_Files (Recursive => True);

      --  Only keep those who are relevant to libclang

      for F of Files.all
      loop
         if P_Tree.Info (F).Language = "c"
           or else P_Tree.Info (F).Language = "cpp"
           or else P_Tree.Info (F).Language = "c++"
         then
            Filtered_Files.Append (F);
         end if;
      end loop;

      --  Call Translation_Unit on them to populate the cache for the file

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

   end On_Project_View_Changed;

   -----------------------------
   -- Parsing_Timeout_Handler --
   -----------------------------

   function Parsing_Timeout_Handler return Boolean is
      use Ada.Real_Time;
      T1 : constant Ada.Real_Time.Time := Clock;
      Response : Parsing_Response;
   begin
      while Parsing_Response_Queue.Current_Use > 0 loop
         declare
            Refs : constant Sym_To_Loc_Map := new Symbol_To_Location_Maps.Map;
         begin
            Parsing_Response_Queue.Dequeue (Response);
            Response.Context.Add_TU_To_Cache
              (+Response.File_Name, Response.TU);
            Put_Line ("INDEXING START FOR FILE " & (+Response.File_Name));
            Indexer.Index_Translation_Unit
              (Index_Action  =>
                 Response.Context.Index_Action,
               Client_Data   =>
                 Indexer_Data'(Refs, Response.Context.Sym_Table),
               Index_Options => CXIndexOpt_None,
               TU            => Response.TU);

            --  Reset the references cache for file, and get the new cache
            Response.Context.Reset_Refs (+Response.File_Name, Refs);

            Put_Line
              ("FINISHED , " & Duration'Image (To_Duration (Clock - T1)));
         end;
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

      Kernel.Databases.Lang_Specific_Databases.Include
        ("c", Clang_Database'(Kernel => Core_Kernel (Kernel)));

      Kernel.Databases.Lang_Specific_Databases.Include
        ("c++", Clang_Database'(Kernel => Core_Kernel (Kernel)));

      Clang_Module_Id := new Clang_Module_Record;
      Register_Module
        (Clang_Module_Id, Kernel, "clang_module", Default_Priority);

      Clang_Module_Id.Parsing_Timeout_Id :=
        Glib.Main.Timeout_Add (100, Parsing_Timeout_Handler'Access);

      Add_Hook (Kernel, Project_View_Changed_Hook,
                Wrapper (On_Project_View_Changed'Access),
                Name => "libclang.project_view_changed");
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Clang_Module_Record) is
   begin
      Put_Line ("IN DESTROY");
      Clean_Cache (Id);
      for T of Parsing_Tasks loop
         Put_Line ("FINISHING TASK");
         T.Finish;
      end loop;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tu_Cache : in out TU_Cache_Access) is
   begin
      if Tu_Cache.TU /= No_Translation_Unit then
         clang_disposeTranslationUnit (Tu_Cache.TU);
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
         if Self.Cache.TU = No_Translation_Unit then
            delay 0.01;
         else
            declare
               Dummy : Boolean := Parsing_Timeout_Handler;
            begin
               null;
            end;
            return Self.Cache.TU;
         end if;
      end loop;
   end Get_Blocking;

end Language.Libclang;
