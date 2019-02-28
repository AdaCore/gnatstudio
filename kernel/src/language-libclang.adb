------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

with Ada.Unchecked_Deallocation;
with Ada.Streams.Stream_IO;         use Ada.Streams.Stream_IO;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Real_Time;
with Interfaces.C.Strings;
with GNAT.Regpat;                   use GNAT.Regpat;
with GNAT.Traceback.Symbolic;
with System.Multiprocessors;        use System.Multiprocessors;

with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Utils;                use GNATCOLL.Utils;

with Commands;                      use Commands;
with Commands.Generic_Asynchronous;
with Default_Preferences;           use Default_Preferences;
with String_Utils;                  use String_Utils;

with GPS.Editors;                   use GPS.Editors;
with GPS.Kernel.Hooks;              use GPS.Kernel.Hooks;
with GPS.Kernel;                    use GPS.Kernel;
with GPS.Kernel.Task_Manager;       use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;

with Language.Libclang_Tree;        use Language.Libclang_Tree;
with Language.Libclang.Utils;       use Language.Libclang.Utils;
with Clang_Xref;                    use Clang_Xref;

package body Language.Libclang is

   LRU_Size_Preference : Integer_Preference;
   --  Number of translation units that will be kept in the LRU cache at all
   --  times.

   Nb_Tasks_Preference : Integer_Preference;
   --  Number of concurrent tasks used for parsing.

   Me       : constant Trace_Handle := GNATCOLL.Traces.Create
     ("GPS.KERNEL.LIBCLANG");
   --  Main libclang trace

   Cache_Constant_Marker : constant Natural := 123454321;
   --  An integer to allow us to recognize cache files that are recent enough
   --  to have our version identifier.

   Libclang_Queue_Id : constant String := "libclang parsing files";
   --  Name of the queue in the task manager running the command to parse
   --  files.

   Diagnostics : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.KERNEL.LIBCLANG_DIAGNOSTICS", Off);
   --  Whether diagnostics should be shown in the traces or not

   Activate_Clang_XRef : constant Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.INTERNAL.LIBCLANG_XREF", On);
   --  Whether clang-based cross references should be activated or not

   type Translation_Unit_Wrapper
   is new Ada.Finalization.Controlled with record
      Cache : TU_Cache_Access;
   end record;

   function Get_Blocking
     (Self : Translation_Unit_Wrapper'Class) return Clang_Translation_Unit;
   --  Block until the Translation_Unit_Wrapper has a TU, then return it

   function Has_TU (File_Name : String) return Boolean;
   --  Whether we have a translation unit for File_Name in the cache or not

   function Get_TU (File_Name : String) return Translation_Unit_Wrapper'Class;
   --  Return the translation unit wrapper associated with File_Name. This will
   --  always return, irrespective of whether the translation_unit_wrapper
   --  contains a TU or not.

   function Parsing_Timeout_Handler return Boolean;
   --  Handler that will handle re-index files that have been parsed.
   --  Underneath will just call Index_One_File until the end of times.

   procedure Enqueue_Translation_Unit
     (Kernel        : Core_Kernel;
      File          : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Options       : Clang_Translation_Unit_Flags := Default_Clang_Options;
      Default_Lang  : String := "c++";
      Prio          : Parsing_Request_Priority := Low;
      Callback      : Parse_Callback_Access := null);
   --  Helper for the high level Enqueue_Translation_Unit procedure, that will
   --  take an Unsaved_Files array as parameter. This array is computed by the
   --  higher level Enqueue_Translation_Unit.

   procedure Unchecked_Free
   is new Ada.Unchecked_Deallocation (TU_Maps.Map, Tu_Map_Access);
   procedure Unchecked_Free
   is new Ada.Unchecked_Deallocation (LRU_Lists.List, LRU_Vector_Access);
   procedure Unchecked_Free
   is new Ada.Unchecked_Deallocation (TU_Cache_Record, TU_Cache_Access);
   procedure Unchecked_Free
   is new Ada.Unchecked_Deallocation
     (Clang_Crossrefs_Cache_Type, Clang_Crossrefs_Cache);
   procedure Unchecked_Free
   is new Ada.Unchecked_Deallocation
     (File_Cache_Record, File_Cache_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (File_Cache_Array, File_Cache_Array_Access);
   --  Deallocation procedures

   procedure Python_Get_TU
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the Libclang.get_translation_unit shell command.
   --  This handler will pass back addresses corresponding to the index and the
   --  translation, that will then be used on the python side to create a real
   --  python clang TranslationUnit

   function Construct_Cache_Key
     (File_Name : String;
      Switches  : Unbounded_String_Array) return File_Key;
   --  Helper procedure, that will construct a File_Key cache key from a file
   --  name and a list of switches

   procedure Index_One_File;
   --  This procedure will index one file placed in the clang response queue,
   --  meaning traverse its tree and put the cross references information in
   --  the global cache

   type Parse_Files_Data_Type is record
      Max_Idx, Current_Idx : Natural := 0;
      Kernel               : Core_Kernel;
   end record;
   type Parse_Files_Data_Type_Access is access all Parse_Files_Data_Type;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Parse_Files_Data_Type, Parse_Files_Data_Type_Access);

   package Parse_Files_Command is
     new Commands.Generic_Asynchronous (Parse_Files_Data_Type_Access,
                                        Free => Unchecked_Free);

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Command executed when the project view is changed. This will handle
   --  recomputing the clang cache.

   procedure Save_Crossrefs_Cache (Kernel : Core_Kernel);
   --  This procedure is used to save the crossref cache on disk. Since the
   --  cache is global, it just needs the kernel to get some global information
   --  about the state of GPS

   procedure Initialize_Crossrefs_Cache (Kernel : Core_Kernel);
   --  This procedure is used to initialize the crossref cache, and load it
   --  from disk if a persistence file exists.

   procedure Add_TU_To_Cache
     (File_Name        : String;
      Translation_Unit : Clang_Translation_Unit;
      Version          : Integer := 0);
   --  Helper procedure, used to add a translation unit to the LRU cache after
   --  it has been computed.

   procedure Destroy (S : in out File_Cache_Access);
   --  Destroy a file cache

   function Get_Switches
     (Kernel                    : Core_Kernel;
      File                      : Virtual_File;
      Project                   : Project_Type := No_Project;
      Default_Lang              : String := "c++";
      Include_Compiler_Switches : Boolean := True)
      return Unbounded_String_Array;
   --  Helper procedure that will return the switches for a given file and
   --  project

   function Get_Cache_File (Kernel : Core_Kernel) return Virtual_File;
   --  Return the Virtual file corresponding to the clang crossref cache file
   --  for the root project

   use Streamable_Decl_Info_Vectors_Accesses;
   use Streamable_Ref_Info_Vectors_Accesses;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out File_Cache_Access) is
   begin
      for El of S.Map loop
         if El.Refs /= null then
            Free (El.Refs);
         end if;
         if El.Decls /= null then
            Free (El.Decls);
         end if;
      end loop;
      S.Map.Clear;
      Unchecked_Free (S);
   end Destroy;

   ------------
   -- Has_TU --
   ------------

   function Has_TU (File_Name : String) return Boolean is
   begin
      return Clang_Module_Id.TU_Cache.Contains (+File_Name)
        and then Clang_Module_Id.TU_Cache.Element
          (+File_Name).TU /= No_Translation_Unit;
   end Has_TU;

   ------------
   -- Get_TU --
   ------------

   function Get_TU
     (File_Name : String) return Translation_Unit_Wrapper'Class
   is
      U_File_Name : constant Unbounded_String := +File_Name;
      Cache : TU_Cache_Access;
   begin
      return Ret : Translation_Unit_Wrapper do
         if Clang_Module_Id.TU_Cache.Contains (U_File_Name) then
            Cache := Clang_Module_Id.TU_Cache.Element (U_File_Name);
         else
            Cache :=
              new TU_Cache_Record'(TU => No_Translation_Unit, Version => 0,
                                   Is_Ready => False);
            Clang_Module_Id.TU_Cache.Include (+File_Name, Cache);
         end if;
         Ret.Cache := Cache;
      end return;
   end Get_TU;

   ---------------------
   -- Add_TU_To_Cache --
   ---------------------

   procedure Add_TU_To_Cache
     (File_Name        : String;
      Translation_Unit : Clang_Translation_Unit;
      Version          : Integer := 0)
   is
      U_File_Name : constant Unbounded_String := +File_Name;
      Cache       : TU_Cache_Access;
      use LRU_Lists;
   begin
      Trace (Me, "Adding TU to cache " & File_Name);

      if Clang_Module_Id.TU_Cache.Contains (U_File_Name) then
         Cache := Clang_Module_Id.TU_Cache.Element (U_File_Name);

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
         Clang_Module_Id.TU_Cache.Include (U_File_Name, Cache);
      end if;

      --  Handle LRU bookkeeping

      declare
         C : LRU_Lists.Cursor := Clang_Module_Id.LRU.Find (U_File_Name);
      begin
         --  If the file name is already in the LRU, we want to put it back at
         --  the beginning of the list, so remove the existing element

         if C /= No_Element then
            Delete (Clang_Module_Id.LRU.all, C);
         end if;

         --  In every case, append the file name at the end of the list
         Clang_Module_Id.LRU.Append (U_File_Name);
         Trace (Me, "Adding " & (+U_File_Name) & " to cache");
      end;

      --  Remove elements from the cache if > LRU_Size

      if Clang_Module_Id.LRU.Length >
        Count_Type (Integer'(LRU_Size_Preference.Get_Pref))
      then
         declare
            F : constant Unbounded_String := Clang_Module_Id.LRU.First_Element;
         begin
            --  First, let's remove the element from the LRU
            Clang_Module_Id.LRU.Delete_First;

            Trace (Me, "Removing " & (+F) & " from cache");
            begin
               --  We are not actually removing element from the cache, so that
               --  we can trace dangling pointers
               Dispose (Clang_Module_Id.TU_Cache.Element (F).TU);
               Clang_Module_Id.TU_Cache.Element (F).Is_Ready := False;
            exception
               when E : others =>
                  Trace (Me, "Exception !");
                  Trace (Me, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
            end;
         end;
      end if;
   end Add_TU_To_Cache;

   type Indexer_Data is record
      Syms_To_Locs : File_Cache_Access;
   end record;

   procedure Index_Reference
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Ref_Info);

   function Info_Vector
     (FC : File_Cache_Access; Sym : Clang_Symbol) return Info_Vectors;

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Decl_Info);

   -----------------
   -- Abort_Query --
   -----------------

   function Abort_Query
     (Dummy_Client_Data : in out Indexer_Data) return Boolean is (False);

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
                      (D, CXDiagnostic_DisplaySourceLocation
                       or CXDiagnostic_DisplayColumn
                       or CXDiagnostic_DisplaySourceRanges)));
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

   -----------------
   -- Info_Vector --
   -----------------

   function Info_Vector
     (FC : File_Cache_Access; Sym : Clang_Symbol) return Info_Vectors
   is
      Info_Vector : Info_Vectors;
   begin
      if not FC.Map.Contains (Sym) then
         Info_Vector := (new Decl_Info_Vectors.Vector,
                         new Ref_Info_Vectors.Vector);
         FC.Map.Include (Sym, Info_Vector);
      else
         Info_Vector := FC.Map.Element (Sym);
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
      use Cursors_Arrays;

      Loc : constant Clang_Location := +Info.loc;
      Sym : Clang_Symbol;
   begin
      if Is_From_Main_File (Loc) then

         --  Add info for base specifiers references in C++ class declarations,
         --  because for some reasons they're not visited as references by the
         --  clang indexing process.

         if Info.entityInfo.kind = CXIdxEntity_CXXClass then
            for C of
              Get_Children
                (Clang_Cursor (Info.cursor), CXCursor_CXXBaseSpecifier)
            loop
               Sym :=
                 Clang_Symbol_Table.Find (USR (Referenced (C)));
               Info_Vector (Client_Data.Syms_To_Locs, Sym).Refs.Append
                 (Ref_Info'
                    (To_Offset_T (Location (C)),
                     Small_Cursor_Kind (Kind (C))));
            end loop;
         end if;

         Sym := Clang_Symbol_Table.Find (Value (Info.entityInfo.USR));

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
      Info        : Clang_Ref_Info)
   is
      use Interfaces.C.Strings;
      use Interfaces.C;
      Loc : constant Clang_Location := +Info.loc;
      Sym : Clang_Symbol;
   begin
      if Is_From_Main_File (Loc) then
         Sym :=
           Clang_Symbol_Table.Find (Value (Info.referencedEntity.USR));

         Info_Vector (Client_Data.Syms_To_Locs, Sym).Refs.Append
           (Ref_Info'(To_Offset_T (Loc),
            Small_Cursor_Kind (Info.cursor.kind)));

      end if;
   end Index_Reference;

   package Indexer is new Source_File_Indexer
     (Client_Data_T => Indexer_Data);

   function Full_Name (F : Virtual_File) return String
   is (String (F.Full_Name (Normalize => True).all));

   ------------------------------
   -- Enqueue_Translation_Unit --
   ------------------------------

   procedure Enqueue_Translation_Unit
     (Kernel       : Core_Kernel;
      File         : GNATCOLL.VFS.Virtual_File;
      Reparse      : Boolean := False;
      Options      : Clang_Translation_Unit_Flags := Default_Clang_Options;
      Default_Lang : String := "c++";
      Prio         : Parsing_Request_Priority := Low;
      Callback     : in out Parse_Callback_Access)
   is
      Buffer         : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (File, False, False, False, False);
      Full_File_Name : constant String := Full_Name (File);
   begin

      --  If we're in reparse mode, we want to check if the buffer is open in
      --  an editor, which would imply that we potentially have a newer version
      --  than the one on disk.

      if Reparse
        and then Buffer /= Nil_Editor_Buffer
        and then Has_TU (Full_File_Name)
      then
         declare
            Cache_Val : constant Translation_Unit_Wrapper'Class
              := Get_TU (Full_File_Name);
         begin
            if Cache_Val.Cache.Version < Buffer.Version then
               declare
                  Buffer_Text                         : constant
                    Ada.Strings.Unbounded.String_Access :=
                      new String'(Buffer.Get_Chars);
               begin
                  Enqueue_Translation_Unit
                    (Kernel,
                     File,
                     Unsaved_Files =>
                       (0 =>
                        Create_Unsaved_File (Full_Name (File), Buffer_Text)),
                     Default_Lang => Default_Lang,
                     Prio         => Prio,
                     Callback     => Callback,
                     Options      => Options);
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
      elsif Has_TU (Full_Name (File)) then

         --  Since the files are reparsed incrementally, and here we're
         --  parsing the file itself, we're confident that we have an up to
         --  date TU. In this case we just want to call the callbacks with
         --  the existing TU.

         declare
            Cache_Val : constant Translation_Unit_Wrapper'Class
              := Get_TU (Full_File_Name);
         begin
            if Callback /= null then
               Callback.Call (File, Cache_Val.Cache.TU);
               Free (Callback);
            end if;
         end;
      else

         --  If we don't have a TU for this file, it's probably the first
         --  time we're parsing it. Let's enqueue a request.

         Enqueue_Translation_Unit
           (Kernel, File, No_Unsaved_Files,
            Prio     => Prio,
            Callback => Callback);

      end if;
   end Enqueue_Translation_Unit;

   ----------------------
   -- Translation_Unit --
   ----------------------

   function Translation_Unit
     (Kernel       : Core_Kernel;
      File         : GNATCOLL.VFS.Virtual_File;
      Project      : Project_Type := No_Project;
      Reparse      : Boolean := False;
      Options      : Clang_Translation_Unit_Flags := Default_Clang_Options;
      Default_Lang : String := "c++")
      return Clang_Translation_Unit
   is
      pragma Unreferenced (Project);
      Callback : Parse_Callback_Access := null;
   begin
      Enqueue_Translation_Unit
        (Kernel, File, Reparse,
         Options      => Options,
         Default_Lang => Default_Lang,
         Prio         => High,
         Callback     => Callback);

      if Clang_Module_Id /= null then
         return Get_TU (Full_Name (File)).Get_Blocking;
      end if;

      return No_Translation_Unit;
   end Translation_Unit;

   -----------------------------------
   -- Has_Translation_Unit_In_Cache --
   -----------------------------------

   function Has_Translation_Unit_In_Cache
     (File : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return Get_TU (Full_Name (File)).Cache.Is_Ready;
   end Has_Translation_Unit_In_Cache;

   ---------------------
   -- Crossrefs_Cache --
   ---------------------

   function Crossrefs_Cache return Clang_Crossrefs_Cache
   is
   begin
      return Clang_Module_Id.Refs;
   end Crossrefs_Cache;

   Empty_String_Array : constant GNATCOLL.Utils.Unbounded_String_Array (1 .. 0)
     := (others => <>);

   Cpp_Header_Regex : constant Regpat.Pattern_Matcher :=
     Compile (".*?include\/c\+\+\/\d\..*?\/.*$");

   ------------------------------
   -- Enqueue_Translation_Unit --
   ------------------------------

   procedure Enqueue_Translation_Unit
     (Kernel        : Core_Kernel;
      File          : GNATCOLL.VFS.Virtual_File;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Options       : Clang_Translation_Unit_Flags := Default_Clang_Options;
      Default_Lang  : String := "c++";
      Prio          : Parsing_Request_Priority := Low;
      Callback      : Parse_Callback_Access := null)
   is
      --  ??? We should fill other unsaved_files! ??? Or should we ? I think
      --  that filling the current file as unsaved is enough. We can, at
      --  least in the first iteration of libclang, ask the user to save
      --  the other files if he expects to get completion. RA

      File_Name : constant String := Full_Name (File);
      Request : Parsing_Request;

   begin

      declare
         TU           : Clang_Translation_Unit;
         TU_Wrapper   : constant Translation_Unit_Wrapper'Class
           := Get_TU (File_Name);

         Switches     : constant Unbounded_String_Array :=
           Get_Switches (Kernel, File, Default_Lang => Default_Lang);

         File_Project : constant Project_Type :=
           File_Info'Class
             (Kernel.Registry.Tree.Info_Set
                (File).First_Element).Project;
      begin

         if Has_TU (File_Name) then

            --  If the key is in the cache, we know that File_Content is not
            --  null, so we want to reparse

            TU := TU_Wrapper.Cache.TU;
            TU_Wrapper.Cache.TU := No_Translation_Unit;
            TU_Wrapper.Cache.Is_Ready := False;

            Request := new Parsing_Request_Record'
              (TU            => TU,
               Context       => Kernel,
               Indexer       => Clang_Module_Id.Clang_Indexer,
               Kind          => Reparse,
               Unsaved_Files => new Unsaved_File_Array'(Unsaved_Files),
               Options       => Options,
               File_Name     => +File_Name,
               Prio          => Prio,
               Project_Name  => +(Full_Name (File_Project.Project_Path)),
               Callbacks     => <>);

            if Callback /= null then
               Request.Callbacks.Append (Callback);
            end if;

            Trace (Me, "Enqueuing request for file " & File_Name);
            Parsing_Request_Queue.Enqueue (Request);

         else
            --  In the other case, this is the first time we're parsing this
            --  file

            Request := new Parsing_Request_Record'
              (Kind         => Parse,
               Context      => Kernel,
               Indexer      => Clang_Module_Id.Clang_Indexer,
               File_Name    => +File_Name,
               Switches     => new Unbounded_String_Array'(Switches),
               Options      => Options,
               Prio         => Prio,
               Project_Name => +(Full_Name (File_Project.Project_Path)),
               Callbacks    => <>);

            if Callback /= null then
               Request.Callbacks.Append (Callback);
            end if;

            Trace (Me, "Enqueuing request for file " & File_Name);
            Parsing_Request_Queue.Enqueue (Request);
         end if;

      end;
   end Enqueue_Translation_Unit;

   package Virtual_File_Vectors is new Ada.Containers.Vectors
     (Positive, Virtual_File);

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
         Save_Crossrefs_Cache (Data.Kernel);
      else
         Result := Execute_Again;
      end if;
   end Parse_Files_Iterate;

   package Parse_Callback_Package is
      type Parse_For_Cache is new Parse_Callback with record
         Command    : Parse_Files_Data_Type_Access;
         Start_Time : Ada.Calendar.Time;
      end record;

      overriding procedure Call
        (Self : access Parse_For_Cache;
         File : Virtual_File;
         TU   : Clang_Translation_Unit);
   end Parse_Callback_Package;

   package body Parse_Callback_Package is
      overriding procedure Call
        (Self : access Parse_For_Cache;
         File : Virtual_File;
         TU   : Clang_Translation_Unit) is
         pragma Unreferenced (TU);
         use Ada.Calendar;
      begin
         Trace (Me, "Finished parsing " & Full_Name (File));

         --  Check whether the command is still active.
         Self.Command.Current_Idx := Self.Command.Current_Idx + 1;

         if Self.Command.Current_Idx = Self.Command.Max_Idx then
            Trace (Me, "Total indexing time: "
                   & Duration'Image (Clock - Self.Start_Time));
         end if;

      end Call;
   end Parse_Callback_Package;

   ----------------------
   -- Print_File_Cache --
   ----------------------

   procedure Print_File_Cache (File_Cache : File_Cache_Access) is
   begin
      Put_Line ("File : " & (+File_Cache.File_Name));
      Put_Line ("    Elements : ");
      for C2 in File_Cache.Map.Iterate loop
         declare
            S  : constant Clang_Symbol := Symbol_To_Location_Maps.Key (C2);
            IV : constant Info_Vectors :=
              Symbol_To_Location_Maps.Element (C2);
         begin
            Put ("    " & Clang_Symbol_Table_Pkg.Get (S).all);

            Put ("[");
            for I in IV.Decls.First_Index .. IV.Decls.Last_Index loop
               Print_Decl_Info (IV.Decls.Element (I));
               Put (" ");
            end loop;
            Put ("] ");

            Put ("[");
            for I in IV.Refs.First_Index .. IV.Refs.Last_Index loop
               Print_Ref_Info (IV.Refs.Element (I));
               Put (" ");
            end loop;
            Put_Line ("]");
         end;
      end loop;

   end Print_File_Cache;

   ---------------------
   -- Print_Decl_Info --
   ---------------------

   procedure Print_Decl_Info (D : Decl_Info) is
   begin
      Put ("(" & D.Loc'Img & ", " & D.Is_Def'Img & ", " & D.Kind'Img & ")");
   end Print_Decl_Info;

   --------------------
   -- Print_Ref_Info --
   --------------------

   procedure Print_Ref_Info (D : Ref_Info) is
   begin
      Put ("(" & D.Loc'Img & ", " & D.Cursor_Kind'Img & ")");
   end Print_Ref_Info;

   -----------------
   -- Print_Cache --
   -----------------

   procedure Print_Cache (Cache : Clang_Crossrefs_Cache) is
   begin
      Put_Line ("========= Printing the cache =========");
      for C in Cache.Map.Iterate loop
         declare
            K : constant File_Key := File_To_Refs_Maps.Key (C);
         begin
            Put_Line ("Cache entry :" & (+Unbounded_String (K)));
            Print_File_Cache (File_To_Refs_Maps.Element (C));
            Put_Line ("=================================================");
         end;
      end loop;
   end Print_Cache;

   --------------------
   -- Get_Cache_File --
   --------------------

   function Get_Cache_File (Kernel : Core_Kernel) return Virtual_File
   is
      Project : constant Project_Type := Kernel.Get_Project_Tree.Root_Project;
   begin
      return Project.Artifacts_Dir / (+"clang_ref_cache.db");
   end Get_Cache_File;

   --------------------------------
   -- Initialize_Crossrefs_Cache --
   --------------------------------

   procedure Initialize_Crossrefs_Cache (Kernel : Core_Kernel)
   is
      Cache_File      : Ada.Streams.Stream_IO.File_Type;
      Cache_Stream    : Stream_Access;
      Cache_VFS       : constant Virtual_File := Get_Cache_File (Kernel);
   begin
      if Cache_VFS = No_File then
         return;
      end if;

      Trace (Me, "Loading the db: " & Full_Name (Cache_VFS));

      if not Cache_VFS.Is_Regular_File then
         Trace (Me, "No database file");
         return;
      end if;

      begin
         Ada.Streams.Stream_IO.Open
           (Cache_File, In_File, Full_Name (Cache_VFS));
      exception
         when E : others =>
            Trace (Me, "Couldn't stream to file");
            Trace (Me, Exception_Information (E));
            return;
      end;

      begin
         Cache_Stream := Stream (Cache_File);
      exception
         when E : others =>
            Trace (Me, "Failed to create a stream from file");
            Trace (Me, Exception_Information (E));
            return;
      end;

      declare
         Dummy : Boolean;
      begin
         for M of Clang_Module_Id.Refs.Map loop
            Destroy (M);
         end loop;

         Trace (Me, "Reading our special marker and version number");
         declare
            Marker  : Natural;
            Version : String (1 .. 10);
            Validated : Boolean := False;
         begin
            Marker := Natural'Input (Cache_Stream);
            if Marker /= Cache_Constant_Marker then
               Trace (Me, "Cache marker not found - removing this cache");
            else
               Version := String'Input (Cache_Stream);
               if Version = clang_c_Index_h.Version then
                  Validated := True;
               else
                  Trace (Me, "Cache version: " & Version & ASCII.LF
                         & "Clang version: " & clang_c_Index_h.Version
                         & ASCII.LF & "... removing this cache");
               end if;
            end if;

            if not Validated then
               Close (Cache_File);
               Cache_VFS.Delete (Dummy);
               return;
            end if;
         end;

         Clang_Module_Id.Refs := Clang_Crossrefs_Cache'Input (Cache_Stream);
      exception
         when E : others =>
            Trace (Me, "Failed loading the database from cache");
            Trace (Me, Exception_Information (E));
            Cache_VFS.Delete (Dummy);
      end;

      Close (Cache_File);
   end Initialize_Crossrefs_Cache;

   --------------------------
   -- Save_Crossrefs_Cache --
   --------------------------

   procedure Save_Crossrefs_Cache (Kernel : Core_Kernel)
   is
      Cache_VFS       : constant Virtual_File := Get_Cache_File (Kernel);
      Cache_File      : Ada.Streams.Stream_IO.File_Type;
      Cache_Stream    : Stream_Access;

      Dummy           : Boolean;
   begin
      if Cache_VFS = No_File then
         return;
      end if;

      if Clang_Module_Id.Refs.Map.Is_Empty then
         return;
      end if;

      Trace (Me, "Saving the crossref cache " & Full_Name (Cache_VFS));

      --  Remove the file if it already exists

      if Cache_VFS.Is_Regular_File then
         Trace (Me, "Removing existing file");
         Cache_VFS.Delete (Dummy);
      end if;

      begin
         Ada.Streams.Stream_IO.Create
           (Cache_File, Out_File, Full_Name (Cache_VFS));
      exception
         when Error : others =>
            Trace (Me, "ERROR: Cannot open database file for writing");
            Trace (Me, Exception_Information (Error));
      end;

      Cache_Stream := Stream (Cache_File);

      Trace (Me, "Writing our special marker and version number to cache");
      Natural'Output (Cache_Stream, Cache_Constant_Marker);
      String'Output (Cache_Stream, clang_c_Index_h.Version);

      Trace (Me, "Writing the cache to disk");
      Clang_Crossrefs_Cache'Output
        (Cache_Stream, Clang_Module_Id.Refs);

      Close (Cache_File);

   end Save_Crossrefs_Cache;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      P_Tree         : constant GNATCOLL.Projects.Project_Tree_Access
        := Kernel.Get_Project_Tree;

      RP             : constant GNATCOLL.Projects.Project_Type
        := P_Tree.Root_Project;

      Files          : File_Array_Access;
      Filtered_Files : Virtual_File_Vectors.Vector;
      Dummy          : Boolean;
   begin
      Trace (Me, "In Libclang project reload");
      Initialize_Crossrefs_Cache (Core_Kernel (Kernel));

      --  Stop all the tasks first.
      for I in Clang_Module_Id.Parsing_Tasks'Range loop
         Trace (Me, "Stopping task " & I'Img);
         select
            Clang_Module_Id.Parsing_Tasks (I).Stop;
         or
            delay 1.0;
            Trace (Me, "ERROR : Task " & I'Img & " seems to be dead !");
         end select;
      end loop;

      --  Remove all the pending requests. It's important that we stop the
      --  tasks first so that we don't block indefinitely on trying to dequeue
      --  a request that has been taken by a task.
      declare
         Dummy_Request : Parsing_Request;
      begin
         Trace (Me, "Dequeuing requests");
         while Parsing_Request_Queue.Length > 0 loop
            select
               Parsing_Request_Queue.Dequeue (Dummy_Request);
            or
                 -- We don't wait long here. If we can't dequeue it means that
                 -- another task is dequeuing requests, and we're supposed to
                 -- have stopped them all, so there is a logic error.
               delay 0.1;
               Trace (Me, "LOGIC ERROR : Can't dequeue request");
            end select;

            Free (Dummy_Request);
         end loop;
      exception
         when E : others =>
            Trace (Me, "Cannot dequeue requests");
            Trace (Me, Exception_Information (E));
      end;

      --  Remove all the pending responses. We do that the simple way for the
      --  moment but we don't need to index at this stage
      begin
         Trace (Me, "Remove all libclang pending responses");
         Dummy := Parsing_Timeout_Handler;
      exception
         when E : others =>
            Trace (Me, "Exception when indexing");
            Trace (Me, Exception_Information (E));
      end;

      --  Stop the potential command showing the status of the indexing
      --  process.
      Interrupt_Queue (Kernel, Libclang_Queue_Id);

      --  Put all the existing cache entries to inactive
      for Cache of Clang_Module_Id.Refs.Map loop
         Cache.Active := False;
      end loop;

      Unchecked_Free (Clang_Module_Id.Active_Files);

      --  Restart all the tasks

      for I in  1 .. Nb_Tasks_Preference.Get_Pref loop
         Clang_Module_Id.Parsing_Tasks (I).Start;
      end loop;

      --  We actually only want to index if cross refs are activated

      if not Active (Activate_Clang_XRef) then
         return;
      end if;

      --  Fetch all of the project's files
      Trace (Me, "Fetching C and C++ files for libclang xref cache");
      Files := RP.Source_Files (Recursive => True);

      --  Only keep those who are relevant to libclang
      for F of Files.all loop
         declare
            Info_Set : constant File_Info_Set := P_Tree.Info_Set (F);
            Language : constant String :=
              File_Info (Info_Set.First_Element).Language;
            Cache_Key : File_Key;
         begin
            if Language in "c" | "cpp" | "c++" then
               Cache_Key := Construct_Cache_Key (Core_Kernel (Kernel), F);

               if Clang_Module_Id.Refs.Map.Contains (Cache_Key) then
                  --  The cache already contains an entry for this file with
                  --  these switches.

                  declare
                     Cache : constant File_Cache_Access :=
                       Clang_Module_Id.Refs.Map.Element (Cache_Key);
                     use type Ada.Calendar.Time;
                  begin
                     --  Check if the time stamp is the same
                     if Cache.File_Time_Stamp /= F.File_Time_Stamp then

                        --  If it's not the same, we'll recompute the cache
                        Filtered_Files.Append (F);
                        Trace (Me, "Queuing " & Full_Name (F)
                               & " for parsing");

                     else

                        --  If it is the same, we just reactivate the cache key
                        Cache.Active := True;

                     end if;
                  end;

                  Trace (Me, "Loading " & Full_Name (F) & " from cache");
               else
                  Filtered_Files.Append (F);
                  Trace (Me, "Queuing " & Full_Name (F) & " for parsing");
               end if;
            end if;
         end;
      end loop;

      Unchecked_Free (Files);

      Trace (Me, "Sending files to parsing tasks");
      declare
         Command : Parse_Files_Command.Generic_Asynchronous_Command_Access;
         Command_Data : constant Parse_Files_Data_Type_Access
           := new Parse_Files_Data_Type;
      begin
         --  Call Translation_Unit on them to populate the cache for the file
         Command_Data.Max_Idx := Natural (Filtered_Files.Length);
         Command_Data.Kernel := Core_Kernel (Kernel);

         for F of Filtered_Files loop
            declare
               Callback : Parse_Callback_Access :=
                 new Parse_Callback_Package.Parse_For_Cache'
                   (Command    => Command_Data,
                    Start_Time => Ada.Calendar.Clock);
            begin
               Enqueue_Translation_Unit
                 (Core_Kernel (Kernel), F, False, Callback => Callback);
            end;
         end loop;

         --  Only start the monitoring command if there actually are files to
         --  parse
         if not Filtered_Files.Is_Empty then
            Trace (Me, "Starting libclang parsing files command");

            Parse_Files_Command.Create
              (Command, "libclang parsing files",
               Command_Data,
               Parse_Files_Iterate'Access);

            Launch_Background_Command
              (Kernel     => Kernel,
               Command    => Command,
               Active     => False,
               Show_Bar   => True,
               Block_Exit => False,
               Queue_Id   => Libclang_Queue_Id);
         end if;
      end;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception in libclang project reload");
         Trace (Me, Exception_Information (E));
   end Execute;

   --------------------
   -- Index_One_File --
   --------------------

   procedure Index_One_File is
      use Ada.Real_Time;

      T1       : constant Ada.Real_Time.Time := Clock;
      Response : Parsing_Response;
   begin
      if Parsing_Response_Queue.Current_Use = 0 then
         return;
      end if;

      select
         Parsing_Response_Queue.Dequeue (Response);
      or
         --  If we get here, there is a logic error in the tasking code.
         delay 0.1;
         Trace (Me, "LOGIC ERROR : Another thread is dequeuing responses");
         return;
      end select;

      Add_TU_To_Cache (+Response.File_Name, Response.TU);

      if Clang_Module_Id.Indexing_Active then

         --  Reset the references cache for file, and get the new cache
         declare
            use File_To_Refs_Maps;

            Cache_Key : constant File_Key :=
              Construct_Cache_Key
                (Response.Context, Create (+(+Response.File_Name)));
            Refs      :  File_Cache_Access;
            C         : File_To_Refs_Maps.Cursor;
         begin

            C := Clang_Module_Id.Refs.Map.Find (Cache_Key);
            if C = No_Element then
               Refs := new File_Cache_Record;
               Refs.File_Name := Response.File_Name;
               Clang_Module_Id.Refs.Map.Include (Cache_Key, Refs);
            else
               Refs := Element (C);
               Refs.Map.Clear;
            end if;

            Refs.File_Time_Stamp :=
              Create (+(+Response.File_Name)).File_Time_Stamp;

            Trace (Me, "Start indexing for file " & (+Response.File_Name));
            Indexer.Index_Translation_Unit
              (Index_Action  =>
                 Clang_Module_Id.Index_Action,
               Client_Data   =>
                 Indexer_Data'(Syms_To_Locs => Refs),
               Index_Options => CXIndexOpt_None,
               TU            =>
                 Clang_Module_Id.TU_Cache.Element (Response.File_Name).TU);
         end;
      end if;

      --  Call all the callbacks and free them
      for CB of Response.Callbacks loop
         CB.Call (Create (Filesystem_String (+Response.File_Name)),
                  Response.TU);
         Free (CB);
      end loop;

      Trace (Me, "Indexing finished, took "
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

   Libclang_Class_Name : constant String := "Libclang";

   -------------------
   -- Python_Get_TU --
   -------------------

   procedure Python_Get_TU
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);

      Kernel : constant Kernel_Handle := Get_Kernel (Data);

      File : constant Virtual_File := Get_Data
        (Nth_Arg
           (Data, 1, Get_File_Class (Kernel),
            Default => No_Class_Instance, Allow_Null => True));

      --  ??? TEMPORARY HACK: Don't use the project passed by the user, because
      --  we don't support aggregate projects correctly for the moment.
      --  Project : GNATCOLL.Projects.Project_Type := Get_Data (Data, 2);
      Project : constant GNATCOLL.Projects.Project_Type := No_Project;

      CTU    : constant Clang_Translation_Unit := Translation_Unit
        (Kernel  => Core_Kernel (Get_Kernel (Data)),
         File    => File,
         Project => Project);

   begin

      Set_Return_Value_As_List (Data);
      Set_Address_Return_Value (Data, System.Address (CTU));
      Set_Address_Return_Value
        (Data, System.Address (Clang_Module_Id.Clang_Indexer));
   end Python_Get_TU;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Language_Class : constant Class_Type :=
        New_Class (Kernel, Libclang_Class_Name);

      Idx            : constant Clang_Index :=
        Create_Index
          (True,
           --  We never want to display diagnostics via libclang,
           --  because it will just dump them on stdout
           Display_Diagnostics => False);
   begin

      Register_Command
        (Kernel.Scripts, "_get_translation_unit",
         (Param ("file"), Param ("project")),
         Python_Get_TU'Access,
         Language_Class,
         Static_Method => True);

      --  Register cross references databases for c and c++
      if Active (Activate_Clang_XRef) then
         Kernel.Databases.Lang_Specific_Databases.Include
           ("c", Clang_Database'(null record));

         Kernel.Databases.Lang_Specific_Databases.Include
           ("c++", Clang_Database'(null record));
      end if;

      Clang_Module_Id := new Clang_Module_Record;

      Register_Module
        (Clang_Module_Id, Kernel, "clang_module", Default_Priority);

      Clang_Module_Id.Parsing_Timeout_Id :=
        Glib.Main.Timeout_Add (100, Parsing_Timeout_Handler'Access);

      Project_View_Changed_Hook.Add (new On_Project_View_Changed);

      Clang_Module_Id.Clang_Indexer := Idx;
      Clang_Module_Id.TU_Cache      := new TU_Maps.Map;
      Clang_Module_Id.LRU           := new LRU_Lists.List;
      Clang_Module_Id.Index_Action  := Create (Idx);
      Clang_Module_Id.Refs          := new Clang_Crossrefs_Cache_Type;

      --  Register the "Clang advanced settings" group explicitly, because we
      --  want it to be the last group on the C & C++ page.

      Kernel.Get_Preferences.Get_Registered_Page
        ("Editor/C & C++/", Create_If_Needed => True).Register_Group
        ("Clang advanced settings", new Preferences_Group_Record, -10, True);

      LRU_Size_Preference := Default_Preferences.Create
        (Manager      => Kernel.Get_Preferences,
         Name         => "Libclang-LRU-Size",
         Label        => "Cache size",
         Doc          => "Number of compilation units kept in memory.",
         Path         => "Editor/C & C++:Clang advanced settings",
         Default      => 8, Minimum => 1, Maximum => 128);

      Nb_Tasks_Preference := Default_Preferences.Create
        (Manager      => Kernel.Get_Preferences,
         Name         => "Libclang-Nb-Parsing-Tasks",
         Label        => "Parsing tasks count",
         Doc          => "Number of concurrent tasks to parse files.",
         Path         => "Editor/C & C++:Clang advanced settings",

         Default      => Natural'Max
           (Natural'Min (Natural (Number_Of_CPUs + 1) / 2, Max_Nb_Tasks), 1),

         --  We prefer to be conservative on the number of tasks allocated to
         --  clang parsing, in order to not provoke freezes on user's machines.
         --  This expression will give the following transfer:
         --
         --  +---------------+------------------+
         --  | Nb processors | Nb parsing tasks |
         --  |       1       |         1        |
         --  |       2       |         1        |
         --  |       3       |         2        |
         --  |       4       |         2        |
         --  |       5       |         3        |
         --  |       6       |         3        |
         --  |       7       |         4        |
         --  |       8       |         4        |
         --  +---------------+------------------+

         Minimum      => 1, Maximum => Max_Nb_Tasks);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Id : in out Clang_Module_Record) is
   begin
      Trace (Me, "Cleaning all cache for libclang");

      --  Kill all tasks *before* we deallocate any resources
      for T of Clang_Module_Id.Parsing_Tasks loop
         T.Finish;
      end loop;

      Unchecked_Free (Id.Active_Files);

      for C of Id.TU_Cache.all loop
         Destroy (C);
      end loop;
      Id.TU_Cache.Clear;
      Unchecked_Free (Id.TU_Cache);
      Unchecked_Free (Id.LRU);

      for M of Id.Refs.Map loop
         Destroy (M);
      end loop;
      Unchecked_Free (Id.Refs);

      Dispose (Id.Index_Action);
      clang_disposeIndex (Id.Clang_Indexer);

      Clang_Symbol_Table.Free;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tu_Cache : in out TU_Cache_Access) is
   begin
      if Tu_Cache.TU /= No_Translation_Unit then
         Dispose (Tu_Cache.TU);
      end if;
      Unchecked_Free (Tu_Cache);
   end Destroy;

   ------------------
   -- Get_Blocking --
   ------------------

   function Get_Blocking
     (Self : Translation_Unit_Wrapper'Class) return Clang_Translation_Unit
   is
   begin
      loop
         if Self.Cache.Is_Ready = False then

            --  While waiting on the translation unit to be ready, we'll index
            --  one file at a time, and eventually our request will have been
            --  processed.

            Index_One_File;
            delay 0.005;
         else
            return Self.Cache.TU;
         end if;
      end loop;
   end Get_Blocking;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Kernel                    : Core_Kernel;
      File                      : Virtual_File;
      Project                   : Project_Type := No_Project;
      Default_Lang              : String := "c++";
      Include_Compiler_Switches : Boolean := True)
      return Unbounded_String_Array
   is

      Kernel_Lang : constant String :=
        Kernel.Lang_Handler.Get_Language_From_File (File);

      function Lang return String;
      function Lang return String is
      begin
         if Match (Cpp_Header_Regex, Full_Name (File)) then
            return "c++";
         elsif Kernel_Lang = "" then
            return Default_Lang;
         else
            return Kernel_Lang;
         end if;
      end Lang;

      Ignored          : Boolean;

      File_Project : constant Project_Type :=
        (if Project /= No_Project then Project
         else
            File_Info'Class
              (Kernel.Registry.Tree.Info_Set
                (File).First_Element).Project);

      function Get_File_Switches return Unbounded_String_Array;
      --  Wrap the fetching of file switches for this file and project as
      --  unbounded string array

      function Get_File_Switches return Unbounded_String_Array is
         C_Switches       : GNAT.Strings.String_List_Access;
      begin
         --  Retrieve the switches for this file
         Switches
           (File_Project, "compiler", File, Lang, C_Switches, Ignored);
         return S : Unbounded_String_Array (C_Switches.all'Range) do
            for J in C_Switches'Range loop
               S (J) := +C_Switches (J).all;
            end loop;
            GNAT.Strings.Free (C_Switches);
         end return;
      end Get_File_Switches;

      Switches     : constant GNATCOLL.Utils.Unbounded_String_Array
        :=

      --  We pass to libclang a list of switches made of:
      --  ... the C/C++ switches specified in this project
        Get_File_Switches

      --  ... a -I<dir> for each directory in the subprojects
      --  of this project
        & Get_Project_Source_Dirs
        (Kernel, File_Project, Lang)

        --  ... a -I<dir> for each dir in the compiler search path
        & (if Include_Compiler_Switches
           then Get_Compiler_Search_Paths_Switches (Kernel, File_Project, Lang)
           else Empty_String_Array)

        & (if Lang in "c++" | "cpp" then (+"-x", +"c++")
           else Empty_String_Array);
   begin

      Trace (Diagnostics, "=== File     : " & Full_Name (File));
      Trace (Diagnostics, "=== Language : " & Lang);
      Trace (Diagnostics, "======= Begin switches ==============");
      for Sw of Switches loop
         Trace (Diagnostics, +Sw);
      end loop;
      Trace (Diagnostics, "======= End switches ==============");

      return Switches;
   end Get_Switches;

   -------------------------
   -- Construct_Cache_Key --
   -------------------------

   function Construct_Cache_Key
     (Kernel    : Core_Kernel;
      File      : Virtual_File;
      Project   : Project_Type := No_Project) return File_Key
   is
   begin
      return Construct_Cache_Key
        (Full_Name (File),
         Get_Switches
           (Kernel, File, Project,
            --  We don't want to include compiler switches in the cache key, we
            --  presume that they are always the same
            Include_Compiler_Switches => False));
   end Construct_Cache_Key;

   -------------------------
   -- Construct_Cache_Key --
   -------------------------

   function Construct_Cache_Key
     (File_Name : String;
      Switches  : Unbounded_String_Array) return File_Key
   is
      Result : Unbounded_String;
   begin
      Append (Result, +File_Name);

      for Switch of Switches loop
         Append (Result, Switch);
      end loop;

      return File_Key (Result);
   end Construct_Cache_Key;

   ----------------------
   -- Get_Active_Files --
   ----------------------

   function Get_Active_Files return File_Cache_Array
   is
      F : File_Cache_Array (1 .. Positive (Clang_Module_Id.Refs.Map.Length));
      I : Positive := 1;

      function Filter (FCA : File_Cache_Access) return Boolean is (FCA.Active);
   begin
      if Clang_Module_Id.Active_Files = null then
         for El of Clang_Module_Id.Refs.Map loop
            F (I) := El;
            I := I + 1;
         end loop;

         Clang_Module_Id.Active_Files
           := new File_Cache_Array'
             (File_Type_Arrays.Filter (F, Filter'Access));
      end if;

      return Clang_Module_Id.Active_Files.all;
   end Get_Active_Files;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Use_Index) is
      pragma Unreferenced (Self);
   begin
      Clang_Module_Id.Indexing_Active := False;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Use_Index) is
      pragma Unreferenced (Self);
   begin
      Clang_Module_Id.Indexing_Active := True;
   end Finalize;

   ----------------------
   -- Get_Index_Action --
   ----------------------

   function Get_Index_Action return Clang_Index_Action is
   begin
      return Clang_Module_Id.Index_Action;
   end Get_Index_Action;

end Language.Libclang;
