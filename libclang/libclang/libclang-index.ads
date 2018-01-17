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

with clang_c_Index_h; use clang_c_Index_h;

with System;

with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNATCOLL.VFS;
use GNATCOLL.VFS;
with Ada.Containers.Vectors;
with clang_c_CXString_h;
with Ada.Containers; use Ada.Containers;
with Array_Utils;

package Libclang.Index is

   -----------
   -- Index --
   -----------

   subtype Clang_Index is clang_c_Index_h.CXIndex;

   No_Index : constant Clang_Index;

   function Create_Index
     (Exclude_Declarations_From_PCH : Boolean;
      Display_Diagnostics           : Boolean) return Clang_Index;
   --  Provides a shared context for creating translation units.
   --
   --  It provides two options:
   --
   --  - excludeDeclarationsFromPCH : When non-zero, allows enumeration of
   --  "local" declarations (when loading any new translation units). A "local"
   --  declaration is one that belongs in the translation unit itself and not
   --  in a precompiled header that was used by the translation unit. If zero,
   --  all declarations will be enumerated.
   --
   --  Here is an example:
   --
   --  \code
   --    // excludeDeclsFromPCH = 1, displayDiagnostics=1
   --    Idx = clang_createIndex(1, 1);
   --
   --    // IndexTest.pch was produced with the following command:
   --    // "clang -x c IndexTest.h -emit-ast -o IndexTest.pch"
   --    TU = clang_createTranslationUnit(Idx, "IndexTest.pch");
   --
   --    // This will load all the symbols from 'IndexTest.pch'
   --    clang_visitChildren(clang_getTranslationUnitCursor(TU),
   --                        TranslationUnitVisitor, 0);
   --    clang_disposeTranslationUnit(TU);
   --
   --  // This will load all the symbols from 'IndexTest.c', excluding symbols
   --    // from 'IndexTest.pch'.
   --    char *args[] = { "-Xclang", "-include-pch=IndexTest.pch" };
   --    TU = clang_createTranslationUnitFromSourceFile
   --            (Idx, "IndexTest.c", 2, args,
   --            0, 0);
   --    clang_visitChildren(clang_getTranslationUnitCursor(TU),
   --                        TranslationUnitVisitor, 0);
   --    clang_disposeTranslationUnit(TU);
   --  \endcode
   --
   --  This process of creating the 'pch', loading it separately, and using
   --  it (via -include-pch) allows 'excludeDeclsFromPCH' to remove redundant
   --  callbacks (which gives the indexer the same performance benefit as the
   --  compiler).

   procedure Dispose (Index : Clang_Index);
   --  Free Index

   -------------------
   -- Unsaved files --
   -------------------

   type Unsaved_File is new clang_c_Index_h.CXUnsavedFile;
   No_Unsaved_File : constant Unsaved_File :=
     (Filename => Interfaces.C.Strings.Null_Ptr,
      Contents => Interfaces.C.Strings.Null_Ptr,
      Length   => 0);

   function Create_Unsaved_File
     (Filename : String;
      Buffer   : String_Access) return Unsaved_File;
   --  Create an Unsaved file.
   --  If Length = -1, do not consider it, but compute the length of Buffer.

   procedure Destroy_Unsaved_File (F : in out Unsaved_File);

   type Unsaved_File_Array is array (Natural range <>) of Unsaved_File;
   type Unsaved_File_Array_Access is access all Unsaved_File_Array;

   procedure Destroy (Files : in out Unsaved_File_Array_Access);

   No_Unsaved_Files : constant Unsaved_File_Array (1 .. 0) :=
     (others => No_Unsaved_File);

   -----------------------
   -- Translation units --
   -----------------------

   subtype Clang_Translation_Unit is clang_c_Index_h.CXTranslationUnit;

   No_Translation_Unit : constant Clang_Translation_Unit;

   type Clang_Translation_Unit_Flags is mod 2 ** Integer'Size;
   No_Translation_Unit_Flags    : constant Clang_Translation_Unit_Flags := 0;
   Detailedpreprocessingrecord  : constant Clang_Translation_Unit_Flags := 1;
   Incomplete                   : constant Clang_Translation_Unit_Flags := 2;
   Precompiledpreamble          : constant Clang_Translation_Unit_Flags := 4;
   Cachecompletionresults       : constant Clang_Translation_Unit_Flags := 8;
   Forserialization             : constant Clang_Translation_Unit_Flags := 16;
   Cxxchainedpch                : constant Clang_Translation_Unit_Flags := 32;
   Skipfunctionbodies           : constant Clang_Translation_Unit_Flags := 64;
   Includebriefcommentsincodecompletion : constant
     Clang_Translation_Unit_Flags := 128;

   function Parse_Translation_Unit
     (Index                 : Clang_Index;
      Source_Filename       : String;
      Command_Line_Args     : GNATCOLL.Utils.Unbounded_String_Array;
      Unsaved_Files         : Unsaved_File_Array := No_Unsaved_Files;
      Options               : Clang_Translation_Unit_Flags :=
        No_Translation_Unit_Flags)
      return Clang_Translation_Unit;
  --  \brief Parse the given source file and the translation unit corresponding
  --  to that file.
  --
  --  This routine is the main entry point for the Clang C API, providing the
  --  ability to parse a source file into a translation unit that can then
  --  be queried by other functions in the API. This routine accepts a set of
  --  command-line arguments so that the compilation can be configured in the
  --  same way that the compiler is configured on the command line.
  --
  --  \param CIdx The index object with which the translation unit will be
  --  associated.
  --
  --  \param source_filename The name of the source file to load, or NULL if
  --  the source file is included in \c command_line_args.
  --
  --  \param command_line_args The command-line arguments that would be passed
  --  to the \c clang executable if it were being invoked out-of-process. These
  --  command-line options will be parsed and will affect how the translation
  --  unit is parsed. Note that the following options are ignored: '-c',
  --  '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output
  --  file>'.
  --
  --  \param num_command_line_args The number of command-line arguments in \c
  --  command_line_args.
  --
  --  \param unsaved_files the files that have not yet been saved to disk but
  --  may be required for parsing, including the contents of those files. The
  --  contents and name of these files (as specified by CXUnsavedFile) are
  --  copied when necessary, so the client only needs to guarantee their
  --  validity until the call to this function returns.
  --
  --  \param num_unsaved_files the number of unsaved file entries in \p
  --  unsaved_files.
  --
  --  \param options A bitmask of options that affects how the translation unit
  --  is managed but not its compilation. This should be a bitwise OR of the
  --  CXTranslationUnit_XXX flags.
  --
  --  \param[out] out_TU A non-NULL pointer to store the created \c
  --  CXTranslationUnit, describing the parsed code and containing
  --  any diagnostics produced by the compiler.
  --
  --  \returns Zero on success, otherwise returns an error code.

   function Reparse_Translation_Unit
     (TU            : Clang_Translation_Unit;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Options       : Clang_Translation_Unit_Flags :=
        No_Translation_Unit_Flags) return Boolean;
  --  \brief Reparse the source files that produced this translation unit.
  --
  --  This routine can be used to re-parse the source files that originally
  --  created the given translation unit, for example because those source
  --  files have changed (either on disk or as passed via \p unsaved_files).
  --  The source code will be reparsed with the same command-line options as
  --  it was originally parsed.
  --
  --  Reparsing a translation unit invalidates all cursors and source locations
  --  that refer into that translation unit. This makes reparsing a translation
  --  unit semantically equivalent to destroying the translation unit and then
  --  creating a new translation unit with the same command-line arguments.
  --  However, it may be more efficient to reparse a translation unit using
  --  this routine.
  --
  --  \param TU The translation unit whose contents will be re-parsed.
  --  The translation unit must originally have been built with \c
  --  clang_createTranslationUnitFromSourceFile().
  --
  --  \param num_unsaved_files The number of unsaved file entries in \p
  --  unsaved_files.
  --
  --  \param unsaved_files The files that have not yet been saved to disk but
  --  may be required for parsing, including the contents of those files. The
  --  contents and name of these files (as specified by CXUnsavedFile) are
  --  copied when necessary, so the client only needs to guarantee their
  --  validity until the call to this function returns.
  --
  --  \param options A bitset of options composed of the flags in
  --  CXReparse_Flags. The function \c clang_defaultReparseOptions() produces
  --  a default set of options recommended for most uses, based on the
  --  translation unit.
  --
  --  \returns True if the sources could be reparsed. A non-zero error code
  --  will be returned if reparsing was impossible, such that the translation
  --  unit is invalid. In such cases, the only valid call for \c TU is \c
  --  clang_disposeTranslationUnit(TU). The error codes returned by this
  --  routine are described by the \c CXErrorCode enum. /

   procedure Dispose (TU : in out Clang_Translation_Unit);
   --  Free TU

   ----------------
   -- Completion --
   ----------------

   type Clang_Code_Complete_Flags is mod 2 ** Integer'Size;
   Include_Macros         : constant Clang_Code_Complete_Flags := 1;
   Include_Code_Patterns  : constant Clang_Code_Complete_Flags := 2;
   Include_Brief_Comments : constant Clang_Code_Complete_Flags := 4;

   type Clang_Complete_Results is record
      CXCodeCompleteResults : access clang_c_Index_h.CXCodeCompleteResults;
   end record;
   --  A set of completion results

   No_Complete_Results : constant Clang_Complete_Results;

   procedure Dispose (Results : in out Clang_Complete_Results);
   --  Free memory associated to Results

   procedure Sort (Results : in out Clang_Complete_Results);

   function Complete_At
     (TU            : Clang_Translation_Unit;
      Filename      : String;
      Line          : Natural;
      Column        : Natural;
      Unsaved_Files : Unsaved_File_Array := No_Unsaved_Files;
      Options       : Clang_Code_Complete_Flags := 0)
      return Clang_Complete_Results;
   --  Perform code completion at a given location in a translation unit.
   --
   --  This function performs code completion at a particular file, line, and
   --  column within source code, providing results that suggest potential
   --  code snippets based on the context of the completion. The basic model
   --  for code completion is that Clang will parse a complete source file,
   --  performing syntax checking up to the location where code-completion has
   --  been requested. At that point, a special code-completion token is passed
   --  to the parser, which recognizes this token and determines, based on the
   --  current location in the C/Objective-C/C++ grammar and the state of
   --  semantic analysis, what completions to provide. These completions are
   --  returned via a new \c CXCodeCompleteResults structure.
   --
   --  Code completion itself is meant to be triggered by the client when the
   --  user types punctuation characters or whitespace, at which point the
   --  code-completion location will coincide with the cursor. For example,
   --  if \c p is a pointer, code-completion might be triggered after the "-"
   --  and then after the ">" in \c p->. When the code-completion location is
   --  afer the ">", the completion results will provide, e.g., the members
   --  of the struct that "p" points to. The client is responsible for placing
   --  the cursor at the beginning of the token currently being typed, then
   --  filtering the results based on the contents of the token. For example,
   --  when code-completing for the expression \c p->get, the client should
   --  provide the location just after the ">" (e.g., pointing at the "g") to
   --  this code-completion hook. Then, the client can filter the results based
   --  on the current token text ("get"), only showing those results that start
   --  with "get". The intent of this interface is to separate the relatively
   --  high-latency acquisition of code-completion results from the filtering
   --  of results on a per-character basis, which must have a lower latency.
   --
   --  \param TU The translation unit in which code-completion should
   --  occur. The source files for this translation unit need not be
   --  completely up-to-date (and the contents of those source files may
   --  be overridden via \p unsaved_files). Cursors referring into the
   --  translation unit may be invalidated by this invocation.
   --
   --  \param complete_filename The name of the source file where code
   --  completion should be performed. This filename may be any file
   --  included in the translation unit.
   --
   --  \param complete_line The line at which code-completion should occur.
   --
   --  \param complete_column The column at which code-completion should occur.
   --  Note that the column should point just after the syntactic construct
   --  that initiated code completion, and not in the middle of a lexical
   --  token.
   --
   --  \param unsaved_files the Tiles that have not yet been saved to disk but
   --  may be required for parsing or code completion, including the contents
   --  of those files. The contents and name of these files (as specified by
   --  CXUnsavedFile) are copied when necessary, so the client only needs to
   --  guarantee their validity until the call to this function returns.
   --
   --  \param num_unsaved_files The number of unsaved file entries in \p
   --  unsaved_files.
   --
   --  \param options Extra options that control the behavior of code
   --  completion, expressed as a bitwise OR of the enumerators of the
   --  CXCodeComplete_Flags enumeration. The
   --  \c clang_defaultCodeCompleteOptions() function returns a default set
   --  of code-completion options.
   --
   --  \returns If successful, a new \c CXCodeCompleteResults structure
   --  containing code-completion results, which should eventually be
   --  freed with \c clang_disposeCodeCompleteResults(). If code
   --  completion fails, returns Null.

   type Clang_Completion_Result
   is access all clang_c_Index_h.CXCompletionResult;
   --  One completion result

   No_Completion_Results : constant Clang_Completion_Result;

   function Num_Results (Results : Clang_Complete_Results) return Natural;
   --  Return the number of results in Results.

   function Nth_Result
     (Results : Clang_Complete_Results;
      N       : Positive) return Clang_Completion_Result;
   --  Return the Nth result in Results

   subtype Clang_Completion_Chunk_Kind is CXCompletionChunkKind;
   type Clang_Completion_Chunk is record
      Completion : Clang_Completion_Result;
      Index      : Interfaces.C.unsigned;
   end record;
   --  Wrapper around a clang completion chunk.

   function Text (Chunk : Clang_Completion_Chunk) return String;
   --  Get the text associated with this completion chunk

   function Kind
     (Chunk : Clang_Completion_Chunk) return Clang_Completion_Chunk_Kind;
   --  Get the kind of this completion chunk

   function To_String (Chunk : Clang_Completion_Chunk) return String;
   --  Get a string representation of this completion chunk

   package Chunk_Arrays is new Array_Utils (Clang_Completion_Chunk);
   subtype Chunk_Array is Chunk_Arrays.Array_Type;
   --  Wrapper around the internal collection of chunks. Created so that we can

   function Get_Chunks (R : Clang_Completion_Result) return Chunk_Array;
   --  Get the array of chunks from a completion result

   function To_String (Completion : Clang_Completion_Result) return String;
   --  Get the string repr of a completion result

   type Completion_Strings is record
      Completion : Unbounded_String := Null_Unbounded_String;
      --  The completion string (the identifier that is being< proposed

      Doc        : Unbounded_String := Null_Unbounded_String;
      --  The full doc to display
   end record;
   --  A collated data used for completion

   function Spellings
     (Result : Clang_Completion_Result) return Completion_Strings;
   --  Return the texts (completion + doc) associated with Result

   function Typed_Text (C : Clang_Completion_Result) return String;
   --  Returns the typed text asssociated with the result, that is the
   --  completion associated with the result.

   function Kind
     (Result : Clang_Completion_Result) return clang_c_Index_h.CXCursorKind;
   --  Return the kind of the result

   function Priority
     (Result : Clang_Completion_Result) return Natural;
   --  Return the priority of this completion result. The lowest the priority
   --  is, the likeliest the result is meaningful in the context according to
   --  libclang

   function Is_Parameter_Completion
     (Result : Clang_Completion_Result) return Boolean;
   --  Return wether this completion result corresponds to a current_parameter
   --  completion result (a completion that indicates the current parameter
   --  of a function call)

   function Get_Current_Param_Index
     (Result : Clang_Completion_Result) return Natural;
   --  In the case where the result is a current parameter completion result,
   --  return the index of the chunk corresponding to the current parameter

   function Extract_Param_Name_From_Chunk
     (Chunk : Clang_Completion_Chunk) return String;
   --  Return the name from a chunk representing a function/method parameter

   ------------------------------
   -- Cursors & Tree traversal --
   ------------------------------

   type Clang_Cursor is new clang_c_Index_h.CXCursor;
   subtype Clang_Type is clang_c_Index_h.CXType;
   subtype Clang_Location is CXSourceLocation;

   procedure PP_Clang_Cursor (C : Clang_Cursor);
   --  Debug helper, pretty print a clang cursor

   use Interfaces.C;

   overriding function "=" (Left, Right : Clang_Cursor) return Boolean is
     (clang_equalCursors (Left, Right) /= 0);
   --  Returns true when Left and Right are considered equal by libclang.
   --  BEWARE: If you have two cursors that point to the same underlying
   --  entity, but come from different translation units, they *will not* be
   --  equal. In this case you need to use Same_Entity.

   subtype Clang_Cursor_Kind is clang_c_Index_h.CXCursorKind;

   subtype Clang_Visit_Result is clang_c_Index_h.CXChildVisitResult;

   No_Cursor : constant Clang_Cursor;

   type Clang_Cursor_Visitor is access function
     (Parent, Child : Clang_Cursor) return Clang_Visit_Result;

   function Root_Cursor
     (TU : Clang_Translation_Unit) return Clang_Cursor;
   --  Returns the root cursor for the translation unit

   package Cursors_Vectors is new Ada.Containers.Vectors
     (Positive, Clang_Cursor);

   package Cursors_Arrays is new Array_Utils (Clang_Cursor);

   function Toplevel_Nodes
     (TU : Clang_Translation_Unit;
      Filter : access function (C : Clang_Cursor) return Boolean := null)
      return Cursors_Arrays.Array_Type;

   type Cursor_Filter is
     access function (Cursor : Clang_Cursor) return Boolean;

   function Get_Children
     (Cursor : Clang_Cursor;
      Filter : access function (Cursor : Clang_Cursor) return Boolean := null)
      return Cursors_Arrays.Array_Type;
   --  Returns the children of Cursor. If Filter is provided, returns a
   --  filtered collection that contains only the children fullfilling the
   --  Filter predicate.

   function Get_Children
     (Cursor : Clang_Cursor;
      Kind : Clang_Cursor_Kind) return Cursors_Arrays.Array_Type;
   --  Get all the children of cursor that have the given kind. Utility
   --  function

   function Dbg_Get_Children
     (Cursor : Clang_Cursor) return Cursors_Arrays.Array_Type;
   --  Convenience function to get the children of a cursor. Used because Gdb
   --  doesn't handle function with default arguments very well

   function Kind
     (Cursor : Clang_Cursor) return Clang_Cursor_Kind
   is (Clang_Cursor_Kind (Cursor.kind));
   --  Returns the kind of Cursor

   function Is_Definition
     (Cursor : Clang_Cursor) return Boolean
   is
     (clang_isCursorDefinition (Cursor) /= 0);
   --  Returns wether Cursor is a definition according to clang.

   function To_String
     (Clang_String : clang_c_CXString_h.CXString) return String;
   --  Utility function to convert a Clang_String to a regular Ada String,
   --  hiding the necessary allocation and deallocation

   function Spelling
     (Kind : Clang_Cursor_Kind) return String
   is
     (To_String (clang_getCursorKindSpelling (CXCursorKind (Kind))));
   --  Return the spelling of a given kind. Note that now that cursor kinds are
   --  enums, this is much less useful since you can just call 'Image on the
   --  kinds.

   function Spelling
     (Cursor : Clang_Cursor) return String;
   --  Returns the spelling of a given clang cursor

   function Referenced (Cursor : Clang_Cursor) return Clang_Cursor is
     (clang_getCursorReferenced (Cursor));
   --  Returns the cursor that Cursor references. Note that for convenience,
   --  this will return a result for a lot of cursors, even some that are not
   --  reference cursors according to clang (call expression for example)

   function Canonical (Cursor : Clang_Cursor) return Clang_Cursor is
     (clang_getCanonicalCursor (Cursor));
   --  Returns the canonical cursor for a given cursor. Given an entity that
   --  can be declared several times (functions/classes/methods/etc), Canonical
   --  (entity) will return the same cursor for any definition of entity.

   function Definition (Cursor : Clang_Cursor) return Clang_Cursor is
     (clang_getCursorDefinition (Cursor));
   --  Returns the definition of the entity pointed by by Cursor. In ada
   --  parlance, this will return the body.
   --  BEWARE: The definition has to exist in the same translation unit as the
   --  declaration cursor is pointing to ! If it doesn't, this method will
   --  return null. To have a more advanced Get_Body operation, look at
   --  the clang_xref module.

   function Semantic_Parent (Cursor : Clang_Cursor) return Clang_Cursor
   is
     (clang_getCursorSemanticParent (Cursor));
   --  Return the semantic parent of cursor, if this operation has meaning for
   --  the given cursor

   function Lexical_Parent (Cursor : Clang_Cursor) return Clang_Cursor
   is
     (clang_getCursorLexicalParent (Cursor));
   --  Return the lexical parent of Cursor.
   --  BEWARE: This operation has quite unpredictable results with libclang.
   --  It's use is very restricted in client code.

   function Typedef_Underlying_Type
     (T : Clang_Cursor) return Clang_Type
   is
     (clang_getTypedefDeclUnderlyingType (T));
   --  Returns the underlying type for a typedef cursor

   function Is_Method_Pure_Virtual (Method : Clang_Cursor) return Boolean
   is (clang_CXXMethod_isPureVirtual (Method) /= 0);
   --  Given a cursor that points to a method, this will return wether the
   --  method is a pure virtual method or not .

   function Element_Type (Array_Type : Clang_Type) return Clang_Type
   is
     (clang_getArrayElementType (Array_Type));

   function Declaration
     (T : Clang_Type) return Clang_Cursor is (clang_getTypeDeclaration (T));
   --  Given a Clang_Type T, return the declaration of it as a clang cursor

   function USR (Cursor : Clang_Cursor) return String
   is
     (To_String (clang_getCursorUSR (Cursor)));
   --  Unified symbol resolution for Cursor. Provided Cursor is a definition,
   --  USR will return an unique string representation for it.

   function Same_Entity (L, R : Clang_Cursor) return Boolean
   is
     (
      --  Try direct cursor equality first
      L = R
      or else

      --  Then canonical cursors equality
      Canonical (L) = Canonical (R)

      or else

      --  Then only USR equality if kinds are the same
      (Kind (L) = Kind (R) and then USR (L) = USR (R)));
   --  This function returns true if cursors point to the same underlying
   --  entity. "=" for clang cursor is just a wrapper around
   --  clang_cursorsEqual, which is strict cursor equality (must be the exact
   --  same node in the tree)

   function Get_Type (Cursor : Clang_Cursor) return Clang_Type
   is
     (clang_getCursorType (Cursor));
   --  Given a reference or declaration cursor, return the C/C++ type of it.

   function Pointee_Type (T : Clang_Type) return Clang_Type
   is
     (clang_getPointeeType (T));
   --  Given a pointer type, return the type pointed to

   function Result_Type (T : Clang_Type) return Clang_Type
   is (clang_getResultType (T));
   --  Given a function/method type, return the type of the result

   function Hash (Cursor : Clang_Cursor) return Hash_Type
   is (Hash_Type (clang_hashCursor (Cursor)));
   --  Return a hash value for the given cursor

   type Clang_Raw_Location is record
      File : Virtual_File;
      Line, Column, Offset : unsigned;
   end record;

   function Location (Cursor : Clang_Cursor) return Clang_Raw_Location;

   subtype Clang_Linkage_Kind is CXLinkageKind;

   function Linkage (Cursor : Clang_Cursor) return Clang_Linkage_Kind
   is
      (clang_getCursorLinkage (Cursor));

   function Spelling
     (T : Clang_Type) return String;

   function Display_Name
     (Cursor : Clang_Cursor) return String;

   function Spelling (TU : Clang_Translation_Unit) return String is
     (To_String (clang_getTranslationUnitSpelling (TU)));

   function Location (Cursor : Clang_Cursor) return CXSourceLocation
   is
     (clang_getCursorLocation (Cursor));

   function Value (Location : Clang_Location) return Clang_Raw_Location;
   function Offset (Location : Clang_Location) return unsigned;
   pragma Inline_Always (Offset);

   function "+"
     (Location : Clang_Location) return Clang_Raw_Location renames Value;

   function Location
     (TU : Clang_Translation_Unit;
      File : GNATCOLL.VFS.Virtual_File;
      Line, Column : Natural) return Clang_Location;

   function Location
     (TU : Clang_Translation_Unit;
      File : GNATCOLL.VFS.Virtual_File;
      Offset : Natural) return Clang_Location;

   function Cursor_At (TU : Clang_Translation_Unit;
                       File : GNATCOLL.VFS.Virtual_File;
                       Line, Column : Natural) return Clang_Cursor
   is
      (clang_getCursor (TU, Location (TU, File, Line, Column)));

   function "+" (Loc : CXIdxLoc) return Clang_Location
   is (clang_indexLoc_getCXSourceLocation (Loc));

   subtype Clang_Index_Action is CXIndexAction;

   function Create (Index : Clang_Index) return Clang_Index_Action
   renames clang_IndexAction_create;

   procedure Dispose (Index_Action : Clang_Index_Action)
                      renames clang_IndexAction_dispose;

   subtype Clang_Diagnostic_Set is CXDiagnosticSet;
   subtype Clang_Diagnostic is CXDiagnostic;

   subtype Clang_Included_File_Info is CXIdxIncludedFileInfo;
   subtype Clang_Decl_Info is CXIdxDeclInfo;
   subtype Clang_Ref_Info is CXIdxEntityRefInfo;
   subtype Clang_Index_Options is CXIndexOptFlags;

   subtype Clang_Source_Range is CXSourceRange;

   function Range_Start (R : Clang_Source_Range) return Clang_Location
   is
     (clang_getRangeStart (R));

   function Range_End (R : Clang_Source_Range) return Clang_Location
   is
     (clang_getRangeEnd (R));

   function Comment_Range (Comment : Clang_Cursor) return Clang_Source_Range
   is
     (clang_Cursor_getCommentRange (Comment));

   function Extent (C : Clang_Cursor) return Clang_Source_Range
   is
     (clang_getCursorExtent (C));

   function In_Range (Sought, Containing : Clang_Cursor) return Boolean;
   pragma Inline (In_Range);

   procedure Dispose_Overridden
     (overridden : access Clang_Cursor) renames clang_disposeOverriddenCursors;

   generic
      type Client_Data_T is private;

      with function Abort_Query
        (Client_Data : in out Client_Data_T) return Boolean is <>;

      with procedure Diagnostic
        (Client_Data : in out Client_Data_T;
         Diagnostics : Clang_Diagnostic_Set) is <>;

      with procedure Entered_Main_File
        (Client_Data : in out Client_Data_T;
         File        : GNATCOLL.VFS.Virtual_File) is <>;

      with procedure Included_File
        (Client_Data : in out Client_Data_T;
         Included_File_Info : Clang_Included_File_Info) is <>;

      with procedure Started_Translation_Unit
        (Client_Data : in out Client_Data_T) is <>;

      with procedure Index_Declaration
        (Client_Data : in out Client_Data_T;
         Decl_Info   : Clang_Decl_Info) is <>;

      with procedure Index_Reference
        (Client_Data : in out Client_Data_T;
         Ref_Info   : Clang_Ref_Info) is <>;

   package Source_File_Indexer is

      function Index_Source_File
        (Index_Action      : Clang_Index_Action;
         Client_Data       : Client_Data_T;
         Index_Options     : Clang_Index_Options;
         Source_Filename   : String;
         Command_Line_Args : GNATCOLL.Utils.Unbounded_String_Array;
         Unsaved_Files     : Unsaved_File_Array := No_Unsaved_Files;
         Options           : Clang_Translation_Unit_Flags :=
           No_Translation_Unit_Flags) return Clang_Translation_Unit;

      procedure Index_Translation_Unit
        (Index_Action : Clang_Index_Action;
         Client_Data : Client_Data_T;
         Index_Options : Clang_Index_Options;
         TU : Clang_Translation_Unit);

   end Source_File_Indexer;

   function Is_From_Main_File (Loc : Clang_Location) return Boolean
   is
     (clang_Location_isFromMainFile (Loc) /= 0);

   -----------------------
   --  Kind predicates  --
   -----------------------

   function Is_Function (K : Clang_Cursor_Kind) return Boolean
   is
     (K in CXCursor_FunctionDecl
        | CXCursor_FunctionTemplate | CXCursor_CXXMethod
          | CXCursor_ConversionFunction);
   --  Helper to determine if a cursor kind is a function kind

   function Is_Object_Type (K : Clang_Cursor_Kind) return Boolean
   is
     (K in CXCursor_ClassDecl | CXCursor_ClassTemplate
        | CXCursor_ClassTemplatePartialSpecialization
          | CXCursor_StructDecl);
   --  Helper to determine if type is an object type, eg. can it have methods.

   function Is_Type (K : Clang_Cursor_Kind) return Boolean is
     (Is_Object_Type (K) or else
      K in CXCursor_TypeAliasDecl
        | CXCursor_TypedefDecl | CXCursor_EnumDecl | CXCursor_UnionDecl);
   --  Helper to determine if cursor is a type

   function Is_Array (K : CXTypeKind) return Boolean
   is
     (K in CXType_ConstantArray
        | CXType_VariableArray
          | CXType_DependentSizedArray
            | CXType_IncompleteArray);
   --  Helper to determine if a kind is one of an array

   function Is_Subprogram (K : Clang_Cursor_Kind) return Boolean is
     (K in CXCursor_FunctionDecl | CXCursor_CXXMethod
        | CXCursor_FunctionTemplate);

   function Is_Container
     (K : Clang_Cursor_Kind) return Boolean
   is
     (Is_Object_Type (K) or else Is_Function (K));

   function Is_Generic
     (K : Clang_Cursor_Kind) return Boolean is
     (K in
        CXCursor_ClassTemplatePartialSpecialization
          | CXCursor_ClassTemplate
            | CXCursor_FunctionTemplate
              | CXCursor_TemplateRef);

private

   No_Index : constant Clang_Index := CXIndex (System.Null_Address);

   No_Translation_Unit : constant Clang_Translation_Unit :=
        Clang_Translation_Unit (System.Null_Address);

   Aliased_0 : aliased constant Interfaces.C.unsigned := 0;

   No_CXCompletionResult : aliased clang_c_Index_h.CXCompletionResult
     := (clang_c_Index_h.CXCursor_UnexposedDecl,
         clang_c_Index_h.CXCompletionString (System.Null_Address));

   No_CXCodeCompleteResults : aliased clang_c_Index_h.CXCodeCompleteResults
     := (Results    => No_CXCompletionResult'Access,
         NumResults => Aliased_0);

   No_Complete_Results : constant Clang_Complete_Results :=
     (CXCodeCompleteResults => null);

   No_Completion_Results : constant Clang_Completion_Result := null;

   No_Cursor : constant Clang_Cursor := clang_getNullCursor;

end Libclang.Index;
