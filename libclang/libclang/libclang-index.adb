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

with System; use System;

with Ada.Unchecked_Conversion;

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with clang_c_CXString_h;
with clang_c_Index_h; use clang_c_Index_h;

package body Libclang.Index is

   Debug : constant Boolean := False;

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
      R.CXIndex := clang_createIndex
        (int (Boolean'Pos (Exclude_Declarations_From_PCH)),
         int (Boolean'Pos (Display_Diagnostics)));
      return R;
   end Create_Index;

   -------------
   -- Dispose --
   -------------

   procedure Dispose (Index : Clang_Index) is
   begin
      clang_disposeIndex (Index.CXIndex);
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
        (C_Idxx : CXIndex;
         Source_Filename : Interfaces.C.Strings.chars_ptr;
         Command_Line_Args : System.Address;
         Num_Command_Line_Args : int;
         Unsaved_Files : System.Address;
         Num_Unsaved_Files : unsigned;
         Options : unsigned) return CXTranslationUnit;
      pragma Import (C, local_clang_parseTranslationUnit,
                     "clang_parseTranslationUnit");

      C_Command_Line_Args : System.Address;
      C_Unsaved_Files     : System.Address;
   begin
      for J in CL'Range loop
         CL (J) := New_String (To_String (Command_Line_Args (J)));
      end loop;

      if CL'Length = 0 then
         C_Command_Line_Args := System.Null_Address;
      else
         C_Command_Line_Args := CL (CL'First)'Address;
      end if;

      if Unsaved_Files'Length = 0 then
         C_Unsaved_Files := System.Null_Address;
      else
         C_Unsaved_Files := Unsaved_Files (Unsaved_Files'First)'Address;
      end if;

      C_Source_Filename := New_String (Source_Filename);

      TU.CX_Translation_Unit :=
        local_clang_parseTranslationUnit
          (C_Idxx                => Index.CXIndex,
           Source_Filename       => C_Source_Filename,
           Command_Line_Args     => C_Command_Line_Args,
           Num_Command_Line_Args => CL'Length,
           Unsaved_Files         => C_Unsaved_Files,
           Num_Unsaved_Files     => Unsaved_Files'Length,
           Options               => unsigned (Options));

      Free (C_Source_Filename);

      for J in CL'Range loop
         Free (CL (J));
      end loop;

      return TU;
   end Parse_Translation_Unit;

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
        (tu : CXTranslationUnit;
         Unsaved_Files : System.Address;
         Num_Unsaved_Files : unsigned;
         Options : unsigned) return int;
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
          (tu                    => TU.CX_Translation_Unit,
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
      if TU = No_Translation_Unit then
         return;
      end if;

      clang_disposeTranslationUnit (TU.CX_Translation_Unit);
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
        (TU : CXTranslationUnit;
         complete_filename : Interfaces.C.Strings.chars_ptr;
         complete_line : unsigned;
         complete_column : unsigned;
         unsaved_files : System.Address;
         num_unsaved_files : unsigned;
         options : unsigned)
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
          (TU                => TU.CX_Translation_Unit,
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

      Returned.Result := P;

      return Returned;
   end Nth_Result;

   --------------
   -- Spelling --
   --------------

   function Spelling
     (Result : Clang_Completion_Result) return Completion_Strings
   is
      Returned : Completion_Strings;
      Len      : Interfaces.C.unsigned;

      function Chunk_Text (Chunk_Num : Interfaces.C.unsigned) return String;
      --  Return the string for the chunk at position Chunk_Num

      ----------------
      -- Chunk_Text --
      ----------------

      function Chunk_Text (Chunk_Num : Interfaces.C.unsigned) return String is
      begin
         return Value
           (clang_c_CXString_h.clang_getCString
              (clang_getCompletionChunkText
                   (Result.Result.CompletionString, Chunk_Num)));
      end Chunk_Text;

   begin
      if Result = No_Completion_Results then
         return Returned;
      end if;

      Len := clang_getNumCompletionChunks (Result.Result.CompletionString);

      if Len = 0 then
         null;

      elsif Len = 1 then
         Returned.Completion := To_Unbounded_String (Chunk_Text (0));
         Returned.Doc := To_Unbounded_String (Chunk_Text (0));
      else
         for Chunk in 0 .. Len - 1 loop
            if clang_getCompletionChunkKind
              (Result.Result.CompletionString, Chunk) in
              CXCompletionChunk_TypedText .. CXCompletionChunk_Text
            then
               Append (Returned.Completion, Chunk_Text (Chunk));
            end if;

            if Debug then
               Append (Returned.Doc,
                       " [" &
                         clang_getCompletionChunkKind
                         (Result.Result.CompletionString, Chunk)'Img &
                       "]");
            end if;

            Append (Returned.Doc, Chunk_Text (Chunk));

            if Chunk = 0 then
               Append (Returned.Doc, " ");
            end if;
         end loop;
      end if;

      return Returned;
   end Spelling;

   ----------
   -- Kind --
   ----------

   function Kind
     (Result : Clang_Completion_Result) return clang_c_Index_h.CXCursorKind is
   begin
      return Result.Result.CursorKind;
   end Kind;

   -------------------------
   -- Create_Unsaved_File --
   -------------------------

   function Create_Unsaved_File
     (Filename : String;
      Buffer   : String_Access) return Unsaved_File
   is
      Returned : Unsaved_File;
      C_Filename : chars_ptr;
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, chars_ptr);
   begin
      C_Filename := New_String (Filename);
      --  ??? Who frees this?
      Returned.Filename := C_Filename;

      if Buffer = null then
         Returned.Contents := Convert (System.Null_Address);
         Returned.Length := 0;
      else
         Returned.Contents := Convert (Buffer (Buffer'First)'Address);
         Returned.Length := Buffer'Length;
      end if;

      return Returned;
   end Create_Unsaved_File;

end Libclang.Index;
