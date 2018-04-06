------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014-2018, AdaCore                   --
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

with System; use System;

with Ada.Unchecked_Conversion;

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with clang_c_CXString_h; use clang_c_CXString_h;
with Libclang.File;
with System.Address_To_Access_Conversions;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with Ada.Unchecked_Deallocation;
with String_Utils; use String_Utils;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;

package body Libclang.Index is

   Debug : constant Boolean := False;
   Me : constant Trace_Handle := GNATCOLL.Traces.Create ("LIBCLANG");

   type Complete_Results_Access is access all CXCodeCompleteResults;

   function Convert is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => Complete_Results_Access);

   type CXCompletionResult_Array is array
     (Natural range <>) of aliased CXCompletionResult;

   package CXCompletionResult_Pointer is new Interfaces.C.Pointers
     (Index              => Natural,
      Element            => CXCompletionResult,
      Element_Array      => CXCompletionResult_Array,
      Default_Terminator => No_CXCompletionResult);

   function Visit_And_Filter_Children
     (C : Clang_Cursor;
      Visitor : CXCursorVisitor;
      Filter : access function (Cursor : Clang_Cursor) return Boolean := null)
      return Cursors_Arrays.Array_Type;

   function Toplevel_Nodes_Visitor
     (Child  : CXCursor;
      Parent : CXCursor;
      UData   : CXClientData) return CXChildVisitResult;
   pragma Convention (C, Toplevel_Nodes_Visitor);

   ------------------
   -- Create_Index --
   ------------------

   function Create_Index
     (Exclude_Declarations_From_PCH : Boolean;
      Display_Diagnostics           : Boolean)
      return Clang_Index
   is
      R : Clang_Index;
   begin
      R := clang_createIndex
        (int (Boolean'Pos (Exclude_Declarations_From_PCH)),
         int (Boolean'Pos (Display_Diagnostics)));
      return R;
   end Create_Index;

   -------------
   -- Dispose --
   -------------

   procedure Dispose (Index : Clang_Index) is
   begin
      clang_disposeIndex (Index);
   end Dispose;

   ----------------------------
   -- Parse_Translation_Unit --
   ----------------------------

   function Parse_Translation_Unit
     (Index                 : Clang_Index;
      Source_Filename       : String;
      Command_Line_Args     : GNATCOLL.Utils.Unbounded_String_Array;
      Unsaved_Files         : Unsaved_File_Array := No_Unsaved_Files;
      Options               : Clang_Translation_Unit_Flags :=
        No_Translation_Unit_Flags)
      return Clang_Translation_Unit
   is
      TU : Clang_Translation_Unit;

      type Char_Ptr_Ptr is array (Positive range <>) of chars_ptr;

      CL : Char_Ptr_Ptr (Command_Line_Args'Range);

      C_Source_Filename : chars_ptr;

      function local_clang_parseTranslationUnit
        (C_Idxx                : CXIndex;
         Source_Filename       : chars_ptr;
         Command_Line_Args     : System.Address;
         Num_Command_Line_Args : int;
         Unsaved_Files         : System.Address;
         Num_Unsaved_Files     : unsigned;
         Options               : unsigned) return Clang_Translation_Unit;
      pragma Import (C, local_clang_parseTranslationUnit,
                     "clang_parseTranslationUnit");

      C_Command_Line_Args : System.Address;
      C_Unsaved_Files     : System.Address;

      First_Free : Natural := CL'First;
   begin
      Trace (Me, "Parsing translation unit " & Source_Filename);

      for J in Command_Line_Args'Range loop
         declare
            Arg : constant String := To_String (Command_Line_Args (J));
         begin
            --  Never pass the "-v" argument: this causes libclang to
            --  output the header search settings to the standard error, which
            --  is not suitable under Windows where GPS does not have a
            --  console.
            if Arg /= "-v" then
               CL (First_Free) := New_String (Arg);
               First_Free := First_Free + 1;
            end if;
         end;
      end loop;

      if First_Free > CL'First then
         C_Command_Line_Args := CL (CL'First)'Address;
      else
         --  ??? This is wrong, the code won't accept a null address below
         C_Command_Line_Args := System.Null_Address;
      end if;

      if Unsaved_Files'Length = 0 then
         C_Unsaved_Files := System.Null_Address;
      else
         C_Unsaved_Files := Unsaved_Files (Unsaved_Files'First)'Address;
      end if;

      C_Source_Filename := New_String (Source_Filename);

      TU :=
        local_clang_parseTranslationUnit
          (C_Idxx                => Index,
           Source_Filename       => C_Source_Filename,
           Command_Line_Args     => C_Command_Line_Args,
           Num_Command_Line_Args => int (First_Free - CL'First),
           Unsaved_Files         => C_Unsaved_Files,
           Num_Unsaved_Files     => Unsaved_Files'Length,
           Options               => unsigned (Options));

      Free (C_Source_Filename);

      for J in CL'First .. First_Free - 1 loop
         Free (CL (J));
      end loop;

      return TU;
   end Parse_Translation_Unit;

   -------------------------
   -- Source_File_Indexer --
   -------------------------

   package body Source_File_Indexer is
      package CX_To_Client_Data is new System.Address_To_Access_Conversions
        (Client_Data_T);

      function To_Client_Data
        (A : CXClientData) return CX_To_Client_Data.Object_Pointer
      is
        (CX_To_Client_Data.To_Pointer (System.Address (A)));

      function Abort_Query_Internal
        (arg1 : CXClientData; arg2 : System.Address) return int;
      pragma Convention (C, Abort_Query_Internal);
      procedure Diagnostic_Internal
        (arg1 : CXClientData;
         arg2 : CXDiagnosticSet;
         arg3 : System.Address);
      pragma Convention (C, Diagnostic_Internal);
      function Entered_Main_File_Internal
        (arg1 : CXClientData;
         arg2 : CXFile;
         arg3 : System.Address) return CXIdxClientFile;
      pragma Convention (C, Entered_Main_File_Internal);
      function Included_File_Internal
        (arg1 : CXClientData;
         arg2 : access constant CXIdxIncludedFileInfo) return CXIdxClientFile;
      pragma Convention (C, Included_File_Internal);
      function Started_Translation_Unit_Internal
        (arg1 : CXClientData;
         arg2 : System.Address) return CXIdxClientContainer;
      pragma Convention (C, Started_Translation_Unit_Internal);
      procedure Index_Reference_Internal
        (arg1 : CXClientData; arg2 : access constant CXIdxEntityRefInfo);
      pragma Convention (C, Index_Reference_Internal);
      procedure Index_Declaration_Internal
        (arg1 : CXClientData; arg2 : access constant CXIdxDeclInfo);
      pragma Convention (C, Index_Declaration_Internal);

      --------------------------
      -- Abort_Query_Internal --
      --------------------------

      function Abort_Query_Internal
        (arg1 : CXClientData; arg2 : System.Address) return int is
         pragma Unreferenced (arg2);
      begin
         return
           (if Abort_Query
              (To_Client_Data (arg1).all)
            then 1 else 0);
      end Abort_Query_Internal;

      -------------------------
      -- Diagnostic_Internal --
      -------------------------

      procedure Diagnostic_Internal
        (arg1 : CXClientData; arg2 : CXDiagnosticSet; arg3 : System.Address)
      is
         pragma Unreferenced (arg3);
      begin
         Diagnostic (To_Client_Data (arg1).all, arg2);
      end Diagnostic_Internal;

      --------------------------------
      -- Entered_Main_File_Internal --
      --------------------------------

      function Entered_Main_File_Internal
        (arg1 : CXClientData;
         arg2 : CXFile;
         arg3 : System.Address) return CXIdxClientFile is
         pragma Unreferenced (arg3);
      begin
         Entered_Main_File (To_Client_Data (arg1).all,
                            Libclang.File.File (arg2));
         return CXIdxClientFile (System.Null_Address);
      end Entered_Main_File_Internal;

      ----------------------------
      -- Included_File_Internal --
      ----------------------------

      function Included_File_Internal
        (arg1 : CXClientData;
         arg2 : access constant CXIdxIncludedFileInfo) return CXIdxClientFile
      is
      begin
         Included_File (To_Client_Data (arg1).all, arg2.all);
         return CXIdxClientFile (System.Null_Address);
      end Included_File_Internal;

      ---------------------------------------
      -- Started_Translation_Unit_Internal --
      ---------------------------------------

      function Started_Translation_Unit_Internal
        (arg1 : CXClientData; arg2 : System.Address)
         return CXIdxClientContainer is
         pragma Unreferenced (arg2);
      begin
         Started_Translation_Unit (To_Client_Data (arg1).all);
         return CXIdxClientContainer (System.Null_Address);
      end Started_Translation_Unit_Internal;

      --------------------------------
      -- Index_Declaration_Internal --
      --------------------------------

      procedure Index_Declaration_Internal
        (arg1 : CXClientData; arg2 : access constant CXIdxDeclInfo) is
      begin
         Index_Declaration (To_Client_Data (arg1).all, arg2.all);
      end Index_Declaration_Internal;

      ------------------------------
      -- Index_Reference_Internal --
      ------------------------------

      procedure Index_Reference_Internal
        (arg1 : CXClientData; arg2 : access constant CXIdxEntityRefInfo) is
      begin
         Index_Reference (To_Client_Data (arg1).all, arg2.all);
      end Index_Reference_Internal;

      function Imported_AST_File_Internal
        (Dummy_arg1 : CXClientData;
         Dummy_arg2 : access constant CXIdxImportedASTFileInfo)
         return CXIdxClientASTFile
      is
        (CXIdxClientASTFile (System.Null_Address));
      pragma Convention (C, Imported_AST_File_Internal);

      Indexer_Callbacks : aliased constant IndexerCallbacks :=
        (Abort_Query_Internal'Access,
         Diagnostic_Internal'Access,
         Entered_Main_File_Internal'Access,
         Included_File_Internal'Access,
         Imported_AST_File_Internal'Access,
         Started_Translation_Unit_Internal'Access,
         Index_Declaration_Internal'Access,
         Index_Reference_Internal'Access);

      -----------------------
      -- Index_Source_File --
      -----------------------

      function Index_Source_File
        (Index_Action      : Clang_Index_Action;
         Client_Data       : Client_Data_T;
         Index_Options     : Clang_Index_Options;
         Source_Filename   : String;
         Command_Line_Args : GNATCOLL.Utils.Unbounded_String_Array;
         Unsaved_Files     : Unsaved_File_Array := No_Unsaved_Files;
         Options           : Clang_Translation_Unit_Flags :=
           No_Translation_Unit_Flags) return Clang_Translation_Unit
      is
         TU : aliased Clang_Translation_Unit;

         type Char_Ptr_Ptr is array (Positive range <>) of chars_ptr;

         CL : Char_Ptr_Ptr (Command_Line_Args'Range);

         C_Source_Filename : chars_ptr;
         C_Command_Line_Args : System.Address;
         C_Unsaved_Files     : System.Address;

         First_Free : Natural := CL'First;

         Error_Code : int;
         pragma Unreferenced (Error_Code);
      begin
         for J in Command_Line_Args'Range loop
            declare
               Arg : constant String := To_String (Command_Line_Args (J));
            begin
               --  Never pass the "-v" argument: this causes libclang to output
               --  the header search settings to the standard error, which
               --  is not suitable under Windows where GPS does not have a
               --  console.
               if Arg /= "-v" then
                  CL (First_Free) := New_String (Arg);
                  First_Free := First_Free + 1;
               end if;
            end;
         end loop;

         if First_Free > CL'First then
            C_Command_Line_Args := CL (CL'First)'Address;
         else
            --  ??? This is wrong, the code won't accept a null address below
            C_Command_Line_Args := System.Null_Address;
         end if;

         if Unsaved_Files'Length = 0 then
            C_Unsaved_Files := System.Null_Address;
         else
            C_Unsaved_Files := Unsaved_Files (Unsaved_Files'First)'Address;
         end if;

         C_Source_Filename := New_String (Source_Filename);

         Error_Code :=
           clang_indexSourceFile
             (Index_Action,
              CXClientData (Client_Data'Address),
              Indexer_Callbacks'Access,
              Indexer_Callbacks'Size,
              Index_Options,
              source_filename       => C_Source_Filename,
              command_line_args     => C_Command_Line_Args,
              num_command_line_args => int (First_Free - CL'First),
              unsaved_files         => C_Unsaved_Files,
              num_unsaved_files     => Unsaved_Files'Length,
              out_TU                => TU'Address,
              TU_options               => unsigned (Options));

         Free (C_Source_Filename);

         for J in CL'First .. First_Free - 1 loop
            Free (CL (J));
         end loop;

         return TU;
      end Index_Source_File;

      ----------------------------
      -- Index_Translation_Unit --
      ----------------------------

      procedure Index_Translation_Unit
        (Index_Action : Clang_Index_Action;
         Client_Data : Client_Data_T;
         Index_Options : Clang_Index_Options;
         TU : Clang_Translation_Unit)
      is
         Error_Code : int;
         pragma Unreferenced (Error_Code);
      begin
         Error_Code := clang_indexTranslationUnit (Index_Action,
                                     CXClientData (Client_Data'Address),
                                     Indexer_Callbacks'Access,
                                     Indexer_Callbacks'Size,
                                     Index_Options, TU);
      end Index_Translation_Unit;

   end Source_File_Indexer;

   ------------------------------
   -- Reparse_Translation_Unit --
   ------------------------------

   function Reparse_Translation_Unit
     (TU            : Clang_Translation_Unit;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Options       : Clang_Translation_Unit_Flags :=
        No_Translation_Unit_Flags) return Boolean
   is

      function local_clang_reparseTranslationUnit
        (tu                : Clang_Translation_Unit;
         Num_Unsaved_Files : unsigned;
         Unsaved_Files     : System.Address;
         Options           : unsigned) return int;
      pragma Import (C, local_clang_reparseTranslationUnit,
                     "clang_reparseTranslationUnit");

      C_Unsaved_Files     : System.Address;

      C_Result : int;
   begin
      if Unsaved_Files'Length = 0 then
         C_Unsaved_Files := System.Null_Address;
      else
         C_Unsaved_Files := Unsaved_Files (Unsaved_Files'First)'Address;
      end if;

      C_Result :=
        local_clang_reparseTranslationUnit
          (tu                    => TU,
           Unsaved_Files         => C_Unsaved_Files,
           Num_Unsaved_Files     => Unsaved_Files'Length,
           Options               => unsigned (Options));

      return C_Result = 0;
   end Reparse_Translation_Unit;

   -------------
   -- Dispose --
   -------------

   procedure Dispose (TU : in out Clang_Translation_Unit) is
   begin
      Trace (Me, "Freeing translation unit" & Spelling (TU));
      if TU = No_Translation_Unit then
         Trace (Me, "WARNING : Trying to dispose of already freed TU");
         return;
      end if;

      clang_disposeTranslationUnit (TU);
      TU := No_Translation_Unit;
   end Dispose;

   -----------------
   -- Complete_At --
   -----------------

   function Complete_At
     (TU            : Clang_Translation_Unit;
      Filename      : String;
      Line          : Natural;
      Column        : Natural;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Options       : Clang_Code_Complete_Flags := 0)
      return Clang_Complete_Results
   is
      C_Filename : chars_ptr;
      C_Unsaved_Files     : System.Address;

      Result : Clang_Complete_Results;

      function local_clang_codeCompleteAt
        (TU                : Clang_Translation_Unit;
         complete_filename : chars_ptr;
         complete_line     : unsigned;
         complete_column   : unsigned;
         unsaved_files     : System.Address;
         num_unsaved_files : unsigned;
         options           : unsigned)
         return System.Address;
      pragma Import (C, local_clang_codeCompleteAt, "clang_codeCompleteAt");

      C_Returned : System.Address;

   begin
      C_Filename := New_String (Filename);

      if Unsaved_Files'Length = 0 then
         C_Unsaved_Files := System.Null_Address;
      else
         C_Unsaved_Files := Unsaved_Files (Unsaved_Files'First)'Address;
      end if;

      C_Returned :=
        local_clang_codeCompleteAt
          (TU                => TU,
           complete_filename => C_Filename,
           complete_line     => unsigned (Line),
           complete_column   => unsigned (Column),
           unsaved_files     => C_Unsaved_Files,
           num_unsaved_files => Unsaved_Files'Length,
           options           => unsigned (Options));

      Free (C_Filename);

      if C_Returned = System.Null_Address then
         return No_Complete_Results;
      else
         Result.CXCodeCompleteResults := Convert (C_Returned);

         --  We have the results in raw format: create an Ada array

      end if;

      return Result;
   end Complete_At;

   -------------
   -- Dispose --
   -------------

   procedure Dispose (Results : in out Clang_Complete_Results) is
   begin
      if Results = No_Complete_Results then
         return;
      end if;

      clang_disposeCodeCompleteResults (Results.CXCodeCompleteResults);
      Results := No_Complete_Results;
   end Dispose;

   ----------
   -- Sort --
   ----------

   procedure Sort (Results : in out Clang_Complete_Results) is
   begin
      clang_sortCodeCompletionResults
        (Results.CXCodeCompleteResults.Results,
         Results.CXCodeCompleteResults.NumResults);
   end Sort;

   -----------------
   -- Num_Results --
   -----------------

   function Num_Results (Results : Clang_Complete_Results) return Natural is
   begin
      return Natural (Results.CXCodeCompleteResults.NumResults);
   end Num_Results;

   ----------------
   -- Nth_Result --
   ----------------

   function Nth_Result
     (Results : Clang_Complete_Results;
      N       : Positive) return Clang_Completion_Result
   is
      use CXCompletionResult_Pointer;

      P : CXCompletionResult_Pointer.Pointer :=
        CXCompletionResult_Pointer.Pointer
          (Results.CXCodeCompleteResults.Results);

      Returned : Clang_Completion_Result;
   begin
      P := P + ptrdiff_t (N - 1);

      Returned := Clang_Completion_Result (P);

      return Returned;
   end Nth_Result;

   ---------------
   -- To_String --
   ---------------

   function To_String (Chunk : Clang_Completion_Chunk) return String
   is
      Kind_Image : constant String := Kind (Chunk)'Img;
      Prefix : constant String := "CXCompletionChunk_";
   begin
      return "(" & Kind_Image (Prefix'Length .. Kind_Image'Length)
        & ", """ & Text (Chunk) & """)";
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Completion : Clang_Completion_Result) return String is
      A : Unbounded_String;
   begin
      Append (A, "Completion (");
      for Chunk of Get_Chunks (Completion) loop
         if Chunk.Index /= 0 then
            Append (A, ", ");
         end if;

         Append (A, To_String (Chunk));
      end loop;
      Append (A, ")");
      return +A;
   end To_String;

   ----------
   -- Text --
   ----------

   function Text
     (Chunk : Clang_Completion_Chunk) return String
   is
     (To_String
        (clang_getCompletionChunkText
             (Chunk.Completion.CompletionString,
              Chunk.Index)));

   ----------
   -- Kind --
   ----------

   function Kind
     (Chunk : Clang_Completion_Chunk) return Clang_Completion_Chunk_Kind
   is
     (clang_getCompletionChunkKind
        (Chunk.Completion.CompletionString, Chunk.Index));

   ----------------
   -- Get_Chunks --
   ----------------

   function Get_Chunks (R : Clang_Completion_Result) return Chunk_Array
   is
   begin
      return
        Ret : Chunk_Array
          (1 .. Natural
             (clang_getNumCompletionChunks (R.CompletionString)))
      do
         for I in Ret'Range loop
            Ret (I) := Clang_Completion_Chunk'(R, unsigned (I - 1));
         end loop;
      end return;
   end Get_Chunks;

   ----------------
   -- Typed_Text --
   ----------------

   function Typed_Text (C : Clang_Completion_Result) return String
   is
      function Is_Typed_Text_Chunk
        (Chunk : Clang_Completion_Chunk) return Boolean
      is (Kind (Chunk) = CXCompletionChunk_TypedText);

      function Find
      is new Chunk_Arrays.Find_Gen (Is_Typed_Text_Chunk);
   begin
      return Text
        (Find (Get_Chunks (C)).Element);
   end Typed_Text;

   --------------
   -- Spellings --
   --------------

   function Spellings
     (Result : Clang_Completion_Result) return Completion_Strings
   is
      Returned : Completion_Strings;
      Chunks   : constant Chunk_Array := Get_Chunks (Result);
   begin
      if Result = No_Completion_Results then
         return Returned;
      end if;
      case Chunks'Length is
      when 0 =>
         null;

      when 1 =>
         Returned.Completion := +Text (Chunks (1));
         Returned.Doc := +Text (Chunks (1));
      when others =>
         for Chunk of Chunks loop
            if Kind (Chunk) in
              CXCompletionChunk_TypedText .. CXCompletionChunk_Text
            then
               Append (Returned.Completion, Text (Chunk));
            end if;

            if Debug then
               Append (Returned.Doc, " [" & Kind (Chunk)'Img & "]");
            end if;

            Append (Returned.Doc, Text (Chunk));

            if Chunk.Index = 0 then
               Append (Returned.Doc, " ");
            end if;
         end loop;
      end case;

      return Returned;
   end Spellings;

   ----------
   -- Pred --
   ----------

   function Pred (El : Clang_Completion_Chunk) return Boolean is
     (Kind (El) = CXCompletionChunk_CurrentParameter);

   -----------------------------
   -- Is_Parameter_Completion --
   -----------------------------

   function Is_Parameter_Completion
     (Result : Clang_Completion_Result) return Boolean
   is
      use Chunk_Arrays;
   begin
      return Contains (Get_Chunks (Result), Pred'Access);
   end Is_Parameter_Completion;

   -----------------------------
   -- Get_Current_Param_Index --
   -----------------------------

   function Get_Current_Param_Index
     (Result : Clang_Completion_Result) return Natural
   is
      use Chunk_Arrays;
   begin
      return Find (Get_Chunks (Result), Pred'Access);
   end Get_Current_Param_Index;

   -----------------------------------
   -- Extract_Param_Name_From_Chunk --
   -----------------------------------

   function Extract_Param_Name_From_Chunk
     (Chunk : Clang_Completion_Chunk) return String
   is
      C : constant Pattern_Matcher := Compile ("[\w_$]+$");
      Match_Arr : Match_Array (0 .. 1) := (others => No_Match);
      T : constant String := Text (Chunk);
   begin
      Match (C, T, Match_Arr);
      if Match_Arr (0) /= No_Match then
         return T (Match_Arr (0).First .. Match_Arr (0).Last);
      else
         return "";
      end if;
   end Extract_Param_Name_From_Chunk;

   --------------
   -- Priority --
   --------------

   function Priority
     (Result : Clang_Completion_Result) return Natural
   is
   begin
      return Natural (clang_getCompletionPriority (Result.CompletionString));
   end Priority;

   ----------
   -- Kind --
   ----------

   function Kind
     (Result : Clang_Completion_Result) return clang_c_Index_h.CXCursorKind is
   begin
      return Result.CursorKind;
   end Kind;

   --------------------------
   -- Destroy_Unsaved_File --
   --------------------------

   procedure Destroy_Unsaved_File (F : in out Unsaved_File) is
   begin
      Free (F.Filename);
      Free (F.Contents);
   end Destroy_Unsaved_File;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Files : in out Unsaved_File_Array_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Unsaved_File_Array, Unsaved_File_Array_Access);
   begin
      for F of Files.all loop
         Destroy_Unsaved_File (F);
      end loop;
      Free (Files);
   end Destroy;

   -------------------------
   -- Create_Unsaved_File --
   -------------------------

   function Create_Unsaved_File
     (Filename : String;
      Buffer   : String_Access) return Unsaved_File
   is
      Returned : Unsaved_File;
      C_Filename : chars_ptr;
   begin
      C_Filename := New_String (Filename);
      --  ??? Who frees this?
      Returned.Filename := C_Filename;

      if Buffer = null then
         Returned.Contents := Null_Ptr;
         Returned.Length := 0;
      else
         Returned.Contents := New_String (Buffer.all);
         Returned.Length := Buffer'Length;
      end if;

      return Returned;
   end Create_Unsaved_File;

   ----------------------
   -- Dbg_Get_Children --
   ----------------------

   function Dbg_Get_Children
     (Cursor : Clang_Cursor) return Cursors_Arrays.Array_Type
   is
   begin
      return Get_Children (Cursor);
   end Dbg_Get_Children;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Cursor : Clang_Cursor;
      Kind : Clang_Cursor_Kind) return Cursors_Arrays.Array_Type
   is
      function Has_Kind (C : Clang_Cursor) return Boolean is
        (Libclang.Index.Kind (C) = Kind);
      --  Filter predicate, returns true if C has kind Kind
   begin
      return Get_Children (Cursor, Has_Kind'Access);
   end Get_Children;

   -----------------
   -- Root_Cursor --
   -----------------

   function Root_Cursor
     (TU : Clang_Translation_Unit) return Clang_Cursor is
   begin
      return clang_getTranslationUnitCursor (TU);
   end Root_Cursor;

   type Visitor_Data is record
      Vec : Cursors_Vectors.Vector;
   end record;

   package Addr_To_Vis_Data is new System.Address_To_Access_Conversions
     (Visitor_Data);

   --------------------------
   -- Get_Children_Visitor --
   --------------------------

   function Get_Children_Visitor
     (Child  : CXCursor;
      Parent : CXCursor;
      UData   : CXClientData) return CXChildVisitResult;
   pragma Convention (C, Get_Children_Visitor);

   --------------------------
   -- Get_Children_Visitor --
   --------------------------

   function Get_Children_Visitor
     (Child  : CXCursor;
      Parent : CXCursor;
      UData  : CXClientData) return CXChildVisitResult
   is
      pragma Unreferenced (Parent);
      Data : constant Addr_To_Vis_Data.Object_Pointer
        := Addr_To_Vis_Data.To_Pointer (System.Address (UData));
   begin
      Data.Vec.Append (Clang_Cursor (Child));
      return CXChildVisit_Continue;
   end Get_Children_Visitor;

   function Visit_And_Filter_Children
     (C : Clang_Cursor;
      Visitor : CXCursorVisitor;
      Filter : access function (Cursor : Clang_Cursor) return Boolean := null)
      return Cursors_Arrays.Array_Type
   is
      V : aliased Visitor_Data;
      V_Ptr : constant Addr_To_Vis_Data.Object_Pointer := V'Unchecked_Access;
      Discard : unsigned;
   begin
      Discard := clang_visitChildren
        (C, Visitor,
         CXClientData (Addr_To_Vis_Data.To_Address (V_Ptr)));

      if V.Vec.Length = 0 then
         return Cursors_Arrays.Empty_Array;
      end if;

      declare
         Out_Array : Cursors_Arrays.Array_Type
           (1 .. Positive (V.Vec.Length));
      begin

         for I in V.Vec.First_Index .. V.Vec.Last_Index loop
            Out_Array (I) := V.Vec.Element (I);
         end loop;

         return (if Filter = null then Out_Array
                 else Cursors_Arrays.Filter (Out_Array, Filter));
      end;

   end Visit_And_Filter_Children;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Cursor : Clang_Cursor;
      Filter : access function (Cursor : Clang_Cursor) return Boolean := null)
      return Cursors_Arrays.Array_Type
   is
   begin
      return Visit_And_Filter_Children
        (Cursor, Get_Children_Visitor'Access, Filter);
   end Get_Children;

   ----------------------------
   -- Toplevel_Nodes_Visitor --
   ----------------------------

   function Toplevel_Nodes_Visitor
     (Child  : CXCursor;
      Parent : CXCursor;
      UData  : CXClientData) return CXChildVisitResult
   is
      pragma Unreferenced (Parent);
      Data : constant Addr_To_Vis_Data.Object_Pointer
        := Addr_To_Vis_Data.To_Pointer (System.Address (UData));
   begin
      if
        clang_Location_isFromMainFile (clang_getCursorLocation (Child)) /= 0
        and then Lexical_Parent (Clang_Cursor (Child))
          = Root_Cursor (clang_Cursor_getTranslationUnit (Child))
      then
         Data.Vec.Append (Clang_Cursor (Child));
      end if;
      return CXChildVisit_Continue;
   end Toplevel_Nodes_Visitor;

   --------------------
   -- Toplevel_Nodes --
   --------------------

   function Toplevel_Nodes
     (TU : Clang_Translation_Unit;
      Filter : access function (C : Clang_Cursor) return Boolean := null)
      return Cursors_Arrays.Array_Type
   is
   begin
      return Visit_And_Filter_Children
        (Root_Cursor (TU), Toplevel_Nodes_Visitor'Access, Filter);
   end Toplevel_Nodes;

   --------------
   -- Spelling --
   --------------

   function Spelling
     (T : Clang_Type) return String
   is
   begin
      return To_String (clang_getTypeSpelling (T));
   end Spelling;

   --------------
   -- Spelling --
   --------------

   function Spelling
     (Cursor : Clang_Cursor) return String
   is
   begin
      return To_String (clang_getCursorSpelling (Cursor));
   end Spelling;

   -----------
   -- Value --
   -----------

   function Value (Location : Clang_Location) return Clang_Raw_Location
   is
      Line, Column, Offset : aliased unsigned;
      F : aliased CXFile;
   begin
      clang_getFileLocation
        (Location, F'Access, Line'Access,
         Column'Access, Offset'Access);

      return Clang_Raw_Location'
        (Libclang.File.File (F), Line, Column, Offset);
   end Value;

   ------------
   -- Offset --
   ------------

   function Offset (Location : Clang_Location) return unsigned is
      Offset : aliased unsigned;
   begin
      clang_getFileLocation
        (Location, null, null, null, Offset'Access);

      return Offset;
   end Offset;

   --------------
   -- In_Range --
   --------------

   function In_Range (Sought, Containing : Clang_Cursor) return Boolean
   is
   begin
      if not Is_From_Main_File (Location (Containing)) then
         return False;
      elsif Sought = Containing then
         return True;
      end if;

      declare
         Sought_Loc : constant unsigned := Offset (Location (Sought));
         Containing_Range : constant Clang_Source_Range := Extent (Containing);
      begin
         return Sought_Loc > Offset (Range_Start (Containing_Range))
           and then
             Sought_Loc < Offset (Range_End (Containing_Range));
      end;
   end In_Range;

   --------------
   -- Location --
   --------------

   function Location
     (TU : Clang_Translation_Unit;
      File : GNATCOLL.VFS.Virtual_File;
      Line, Column : Natural) return Clang_Location
   is
   begin
      return clang_getLocation
        (TU, Libclang.File.File (TU, File),
         unsigned (Line), unsigned (Column));
   end Location;

   --------------
   -- Location --
   --------------

   function Location
     (TU : Clang_Translation_Unit;
      File : GNATCOLL.VFS.Virtual_File;
      Offset : Natural) return Clang_Location is
   begin
      return clang_getLocationForOffset
        (TU, Libclang.File.File (TU, File),
         unsigned (Offset));
   end Location;

   --------------
   -- Location --
   --------------

   function Location (Cursor : Clang_Cursor) return Clang_Raw_Location
   is
   begin
      return Value (Location (Cursor));
   end Location;

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name
     (Cursor : Clang_Cursor) return String
   is
   begin
      return To_String (clang_getCursorDisplayName (Cursor));
   end Display_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Clang_String : clang_c_CXString_h.CXString) return String
   is
      C_String : constant chars_ptr :=
        clang_getCString (Clang_String);
   begin
      if C_String /= Null_Ptr then
         declare
            Str : constant String := Value (C_String);
         begin
            clang_disposeString (Clang_String);
            return Str;
         end;
      else
         return "";
      end if;
   end To_String;

   ---------------------
   -- PP_Clang_Cursor --
   ---------------------

   procedure PP_Clang_Cursor (C : Clang_Cursor) is
   begin
      Put_Line ("<Clang cursor " & Spelling (C) & " " & Kind (C)'Img
                & Location (C).Line'Img & ":" & Location (C).Column'Img & ">");
   end PP_Clang_Cursor;

end Libclang.Index;
