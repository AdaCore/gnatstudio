pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;
with clang_c_CXString_h;
--  \1
with Interfaces.C.Extensions;
with clang_c_CXErrorCode_h;

package clang_c_Index_h is


   CINDEX_VERSION_MAJOR : constant := 0;  --  /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:34
   CINDEX_VERSION_MINOR : constant := 27;  --  /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:35
   --  arg-macro: function CINDEX_VERSION_ENCODE (major, minor)
   --    return  ((major) * 10000) + ((minor) * 1);
   --  unsupported macro: CINDEX_VERSION CINDEX_VERSION_ENCODE( CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR )
   --  unsupported macro: CINDEX_VERSION_STRINGIZE_(major,minor) #major"."#minor
   --  arg-macro: procedure CINDEX_VERSION_STRINGIZE (major, minor)
   --    CINDEX_VERSION_STRINGIZE_(major, minor)
   --  unsupported macro: CINDEX_VERSION_STRING CINDEX_VERSION_STRINGIZE( CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR)

   type CXIndex is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:81

   --  skipped empty struct CXTranslationUnitImpl

   type CXTranslationUnit is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:86

   type CXClientData is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:92

   type CXUnsavedFile is record
      Filename : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:107
      Contents : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:112
      Length : aliased unsigned_long;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:117
   end record;
   pragma Convention (C_Pass_By_Copy, CXUnsavedFile);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:101

   type CXAvailabilityKind is
     (CXAvailability_Available,
      CXAvailability_Deprecated,
      CXAvailability_NotAvailable,
      CXAvailability_NotAccessible);
   pragma Convention (C, CXAvailabilityKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:125

   type CXVersion is record
      Major : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:154
      Minor : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:160
      Subminor : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:166
   end record;
   pragma Convention (C_Pass_By_Copy, CXVersion);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:149

   function clang_createIndex (excludeDeclarationsFromPCH : int; displayDiagnostics : int) return CXIndex;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:209
   pragma Import (C, clang_createIndex, "clang_createIndex");

   procedure clang_disposeIndex (index : CXIndex);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:218
   pragma Import (C, clang_disposeIndex, "clang_disposeIndex");

   type CXGlobalOptFlags is
     (CXGlobalOpt_None,
      CXGlobalOpt_ThreadBackgroundPriorityForIndexing,
      CXGlobalOpt_ThreadBackgroundPriorityForEditing,
      CXGlobalOpt_ThreadBackgroundPriorityForAll);
   pragma Convention (C, CXGlobalOptFlags);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:252

   procedure clang_CXIndex_setGlobalOptions (arg1 : CXIndex; options : unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:267
   pragma Import (C, clang_CXIndex_setGlobalOptions, "clang_CXIndex_setGlobalOptions");

   function clang_CXIndex_getGlobalOptions (arg1 : CXIndex) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:275
   pragma Import (C, clang_CXIndex_getGlobalOptions, "clang_CXIndex_getGlobalOptions");

   type CXFile is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:286

   function clang_getFileName (SFile : CXFile) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:292
   pragma Import (C, clang_getFileName, "clang_getFileName");

--  \1
--  \1

   type CXFileUniqueID_data_array is array (0 .. 2) of aliased Extensions.unsigned_long_long;
   type CXFileUniqueID is record
      data : aliased CXFileUniqueID_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:304
   end record;
   pragma Convention (C_Pass_By_Copy, CXFileUniqueID);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:305

   --  skipped anonymous struct anon_3

   function clang_getFileUniqueID (file : CXFile; outID : access CXFileUniqueID) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:315
   pragma Import (C, clang_getFileUniqueID, "clang_getFileUniqueID");

   function clang_isFileMultipleIncludeGuarded (tu : CXTranslationUnit; file : CXFile) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:323
   pragma Import (C, clang_isFileMultipleIncludeGuarded, "clang_isFileMultipleIncludeGuarded");

   function clang_getFile (tu : CXTranslationUnit; file_name : Interfaces.C.Strings.chars_ptr) return CXFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:335
   pragma Import (C, clang_getFile, "clang_getFile");

   type CXSourceLocation_ptr_data_array is array (0 .. 1) of System.Address;
   type CXSourceLocation is record
      ptr_data : aliased CXSourceLocation_ptr_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:363
      int_data : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:364
   end record;
   pragma Convention (C_Pass_By_Copy, CXSourceLocation);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:365

   --  skipped anonymous struct anon_4

   type CXSourceRange_ptr_data_array is array (0 .. 1) of System.Address;
   type CXSourceRange is record
      ptr_data : aliased CXSourceRange_ptr_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:374
      begin_int_data : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:375
      end_int_data : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:376
   end record;
   pragma Convention (C_Pass_By_Copy, CXSourceRange);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:377

   --  skipped anonymous struct anon_5

   function clang_getNullLocation return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:382
   pragma Import (C, clang_getNullLocation, "clang_getNullLocation");

   function clang_equalLocations (loc1 : CXSourceLocation; loc2 : CXSourceLocation) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:392
   pragma Import (C, clang_equalLocations, "clang_equalLocations");

   function clang_getLocation
     (tu : CXTranslationUnit;
      file : CXFile;
      line : unsigned;
      column : unsigned) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:399
   pragma Import (C, clang_getLocation, "clang_getLocation");

   function clang_getLocationForOffset
     (tu : CXTranslationUnit;
      file : CXFile;
      offset : unsigned) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:407
   pragma Import (C, clang_getLocationForOffset, "clang_getLocationForOffset");

   function clang_Location_isInSystemHeader (location : CXSourceLocation) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:414
   pragma Import (C, clang_Location_isInSystemHeader, "clang_Location_isInSystemHeader");

   function clang_Location_isFromMainFile (location : CXSourceLocation) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:420
   pragma Import (C, clang_Location_isFromMainFile, "clang_Location_isFromMainFile");

   function clang_getNullRange return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:425
   pragma Import (C, clang_getNullRange, "clang_getNullRange");

   function clang_getRange (c_begin : CXSourceLocation; c_end : CXSourceLocation) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:431
   pragma Import (C, clang_getRange, "clang_getRange");

   function clang_equalRanges (range1 : CXSourceRange; range2 : CXSourceRange) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:439
   pragma Import (C, clang_equalRanges, "clang_equalRanges");

   function clang_Range_isNull (c_range : CXSourceRange) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:445
   pragma Import (C, clang_Range_isNull, "clang_Range_isNull");

   procedure clang_getExpansionLocation
     (location : CXSourceLocation;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:469
   pragma Import (C, clang_getExpansionLocation, "clang_getExpansionLocation");

   procedure clang_getPresumedLocation
     (location : CXSourceLocation;
      filename : access clang_c_CXString_h.CXString;
      line : access unsigned;
      column : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:515
   pragma Import (C, clang_getPresumedLocation, "clang_getPresumedLocation");

   procedure clang_getInstantiationLocation
     (location : CXSourceLocation;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:528
   pragma Import (C, clang_getInstantiationLocation, "clang_getInstantiationLocation");

   procedure clang_getSpellingLocation
     (location : CXSourceLocation;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:556
   pragma Import (C, clang_getSpellingLocation, "clang_getSpellingLocation");

   procedure clang_getFileLocation
     (location : CXSourceLocation;
      file : access CXFile;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:585
   pragma Import (C, clang_getFileLocation, "clang_getFileLocation");

   function clang_getRangeStart (c_range : CXSourceRange) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:595
   pragma Import (C, clang_getRangeStart, "clang_getRangeStart");

   function clang_getRangeEnd (c_range : CXSourceRange) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:601
   pragma Import (C, clang_getRangeEnd, "clang_getRangeEnd");

   type CXSourceRangeList is record
      count : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:608
      ranges : access CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:612
   end record;
   pragma Convention (C_Pass_By_Copy, CXSourceRangeList);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:613

   --  skipped anonymous struct anon_6

   function clang_getSkippedRanges (tu : CXTranslationUnit; file : CXFile) return access CXSourceRangeList;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:621
   pragma Import (C, clang_getSkippedRanges, "clang_getSkippedRanges");

   procedure clang_disposeSourceRangeList (ranges : access CXSourceRangeList);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:627
   pragma Import (C, clang_disposeSourceRangeList, "clang_disposeSourceRangeList");

   type CXDiagnosticSeverity is
     (CXDiagnostic_Ignored,
      CXDiagnostic_Note,
      CXDiagnostic_Warning,
      CXDiagnostic_Error,
      CXDiagnostic_Fatal);
   pragma Convention (C, CXDiagnosticSeverity);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:642

   type CXDiagnostic is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:678

   type CXDiagnosticSet is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:683

   function clang_getNumDiagnosticsInSet (Diags : CXDiagnosticSet) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:688
   pragma Import (C, clang_getNumDiagnosticsInSet, "clang_getNumDiagnosticsInSet");

   function clang_getDiagnosticInSet (Diags : CXDiagnosticSet; Index : unsigned) return CXDiagnostic;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:699
   pragma Import (C, clang_getDiagnosticInSet, "clang_getDiagnosticInSet");

   type CXLoadDiag_Error is
     (CXLoadDiag_None,
      CXLoadDiag_Unknown,
      CXLoadDiag_CannotLoad,
      CXLoadDiag_InvalidFile);
   pragma Convention (C, CXLoadDiag_Error);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:707

   function clang_loadDiagnostics
     (file : Interfaces.C.Strings.chars_ptr;
      error : access CXLoadDiag_Error;
      errorString : access clang_c_CXString_h.CXString) return CXDiagnosticSet;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:745
   pragma Import (C, clang_loadDiagnostics, "clang_loadDiagnostics");

   procedure clang_disposeDiagnosticSet (Diags : CXDiagnosticSet);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:752
   pragma Import (C, clang_disposeDiagnosticSet, "clang_disposeDiagnosticSet");

   function clang_getChildDiagnostics (D : CXDiagnostic) return CXDiagnosticSet;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:760
   pragma Import (C, clang_getChildDiagnostics, "clang_getChildDiagnostics");

   function clang_getNumDiagnostics (Unit : CXTranslationUnit) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:766
   pragma Import (C, clang_getNumDiagnostics, "clang_getNumDiagnostics");

   function clang_getDiagnostic (Unit : CXTranslationUnit; Index : unsigned) return CXDiagnostic;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:777
   pragma Import (C, clang_getDiagnostic, "clang_getDiagnostic");

   function clang_getDiagnosticSetFromTU (Unit : CXTranslationUnit) return CXDiagnosticSet;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:787
   pragma Import (C, clang_getDiagnosticSetFromTU, "clang_getDiagnosticSetFromTU");

   procedure clang_disposeDiagnostic (Diagnostic : CXDiagnostic);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:792
   pragma Import (C, clang_disposeDiagnostic, "clang_disposeDiagnostic");

   subtype CXDiagnosticDisplayOptions is unsigned;
   DisplaySourceLocationh : constant CXDiagnosticDisplayOptions := 1;
   DisplayColumn : constant CXDiagnosticDisplayOptions := 2;
   DisplaySourceRanges : constant CXDiagnosticDisplayOptions := 4;
   DisplayOption : constant CXDiagnosticDisplayOptions := 8;
   DisplayCategoryId : constant CXDiagnosticDisplayOptions := 16;
   DisplayCategoryName : constant CXDiagnosticDisplayOptions := 32;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:800

   function clang_formatDiagnostic (Diagnostic : CXDiagnostic; Options : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:877
   pragma Import (C, clang_formatDiagnostic, "clang_formatDiagnostic");

   function clang_defaultDiagnosticDisplayOptions return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:887
   pragma Import (C, clang_defaultDiagnosticDisplayOptions, "clang_defaultDiagnosticDisplayOptions");

   function clang_getDiagnosticSeverity (arg1 : CXDiagnostic) return CXDiagnosticSeverity;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:893
   pragma Import (C, clang_getDiagnosticSeverity, "clang_getDiagnosticSeverity");

   function clang_getDiagnosticLocation (arg1 : CXDiagnostic) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:901
   pragma Import (C, clang_getDiagnosticLocation, "clang_getDiagnosticLocation");

   function clang_getDiagnosticSpelling (arg1 : CXDiagnostic) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:906
   pragma Import (C, clang_getDiagnosticSpelling, "clang_getDiagnosticSpelling");

   function clang_getDiagnosticOption (Diag : CXDiagnostic; Disable : access clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:920
   pragma Import (C, clang_getDiagnosticOption, "clang_getDiagnosticOption");

   function clang_getDiagnosticCategory (arg1 : CXDiagnostic) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:933
   pragma Import (C, clang_getDiagnosticCategory, "clang_getDiagnosticCategory");

   function clang_getDiagnosticCategoryName (Category : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:946
   pragma Import (C, clang_getDiagnosticCategoryName, "clang_getDiagnosticCategoryName");

   function clang_getDiagnosticCategoryText (arg1 : CXDiagnostic) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:953
   pragma Import (C, clang_getDiagnosticCategoryText, "clang_getDiagnosticCategoryText");

   function clang_getDiagnosticNumRanges (arg1 : CXDiagnostic) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:959
   pragma Import (C, clang_getDiagnosticNumRanges, "clang_getDiagnosticNumRanges");

   function clang_getDiagnosticRange (Diagnostic : CXDiagnostic; c_Range : unsigned) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:974
   pragma Import (C, clang_getDiagnosticRange, "clang_getDiagnosticRange");

   function clang_getDiagnosticNumFixIts (Diagnostic : CXDiagnostic) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:981
   pragma Import (C, clang_getDiagnosticNumFixIts, "clang_getDiagnosticNumFixIts");

   function clang_getDiagnosticFixIt
     (Diagnostic : CXDiagnostic;
      FixIt : unsigned;
      ReplacementRange : access CXSourceRange) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1008
   pragma Import (C, clang_getDiagnosticFixIt, "clang_getDiagnosticFixIt");

   function clang_getTranslationUnitSpelling (CTUnit : CXTranslationUnit) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1030
   pragma Import (C, clang_getTranslationUnitSpelling, "clang_getTranslationUnitSpelling");

   function clang_createTranslationUnitFromSourceFile
     (CIdx : CXIndex;
      source_filename : Interfaces.C.Strings.chars_ptr;
      num_clang_command_line_args : int;
      clang_command_line_args : System.Address;
      num_unsaved_files : unsigned;
      unsaved_files : access CXUnsavedFile) return CXTranslationUnit;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1072
   pragma Import (C, clang_createTranslationUnitFromSourceFile, "clang_createTranslationUnitFromSourceFile");

   function clang_createTranslationUnit (CIdx : CXIndex; ast_filename : Interfaces.C.Strings.chars_ptr) return CXTranslationUnit;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1086
   pragma Import (C, clang_createTranslationUnit, "clang_createTranslationUnit");

   function clang_createTranslationUnit2
     (CIdx : CXIndex;
      ast_filename : Interfaces.C.Strings.chars_ptr;
      out_TU : System.Address) return clang_c_CXErrorCode_h.CXErrorCode;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1098
   pragma Import (C, clang_createTranslationUnit2, "clang_createTranslationUnit2");

   subtype CXTranslationUnit_Flags is unsigned;
   CXTranslationUnit_None : constant CXTranslationUnit_Flags := 0;
   CXTranslationUnit_DetailedPreprocessingRecord : constant CXTranslationUnit_Flags := 1;
   CXTranslationUnit_Incomplete : constant CXTranslationUnit_Flags := 2;
   CXTranslationUnit_PrecompiledPreamble : constant CXTranslationUnit_Flags := 4;
   CXTranslationUnit_CacheCompletionResults : constant CXTranslationUnit_Flags := 8;
   CXTranslationUnit_ForSerialization : constant CXTranslationUnit_Flags := 16;
   CXTranslationUnit_CXXChainedPCH : constant CXTranslationUnit_Flags := 32;
   CXTranslationUnit_SkipFunctionBodies : constant CXTranslationUnit_Flags := 64;
   CXTranslationUnit_IncludeBriefCommentsInCodeCompletion : constant CXTranslationUnit_Flags := 128;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1110

   function clang_defaultEditingTranslationUnitOptions return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1214
   pragma Import (C, clang_defaultEditingTranslationUnitOptions, "clang_defaultEditingTranslationUnitOptions");

   function clang_parseTranslationUnit
     (CIdx : CXIndex;
      source_filename : Interfaces.C.Strings.chars_ptr;
      command_line_args : System.Address;
      num_command_line_args : int;
      unsaved_files : access CXUnsavedFile;
      num_unsaved_files : unsigned;
      options : unsigned) return CXTranslationUnit;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1223
   pragma Import (C, clang_parseTranslationUnit, "clang_parseTranslationUnit");

   function clang_parseTranslationUnit2
     (CIdx : CXIndex;
      source_filename : Interfaces.C.Strings.chars_ptr;
      command_line_args : System.Address;
      num_command_line_args : int;
      unsaved_files : access CXUnsavedFile;
      num_unsaved_files : unsigned;
      options : unsigned;
      out_TU : System.Address) return clang_c_CXErrorCode_h.CXErrorCode;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1276
   pragma Import (C, clang_parseTranslationUnit2, "clang_parseTranslationUnit2");

   type CXSaveTranslationUnit_Flags is
     (CXSaveTranslationUnit_None);
   pragma Convention (C, CXSaveTranslationUnit_Flags);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1292

   function clang_defaultSaveOptions (TU : CXTranslationUnit) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1308
   pragma Import (C, clang_defaultSaveOptions, "clang_defaultSaveOptions");

   type CXSaveError is
     (CXSaveError_None,
      CXSaveError_Unknown,
      CXSaveError_TranslationErrors,
      CXSaveError_InvalidTU);
   pragma Convention (C, CXSaveError);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1314

   function clang_saveTranslationUnit
     (TU : CXTranslationUnit;
      FileName : Interfaces.C.Strings.chars_ptr;
      options : unsigned) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1368
   pragma Import (C, clang_saveTranslationUnit, "clang_saveTranslationUnit");

   procedure clang_disposeTranslationUnit (arg1 : CXTranslationUnit);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1375
   pragma Import (C, clang_disposeTranslationUnit, "clang_disposeTranslationUnit");

   type CXReparse_Flags is
     (CXReparse_None);
   pragma Convention (C, CXReparse_Flags);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1384

   function clang_defaultReparseOptions (TU : CXTranslationUnit) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1401
   pragma Import (C, clang_defaultReparseOptions, "clang_defaultReparseOptions");

   function clang_reparseTranslationUnit
     (TU : CXTranslationUnit;
      num_unsaved_files : unsigned;
      unsaved_files : access CXUnsavedFile;
      options : unsigned) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1442
   pragma Import (C, clang_reparseTranslationUnit, "clang_reparseTranslationUnit");

   subtype CXTUResourceUsageKind is unsigned;
   CXTUResourceUsage_AST : constant CXTUResourceUsageKind := 1;
   CXTUResourceUsage_Identifiers : constant CXTUResourceUsageKind := 2;
   CXTUResourceUsage_Selectors : constant CXTUResourceUsageKind := 3;
   CXTUResourceUsage_GlobalCompletionResults : constant CXTUResourceUsageKind := 4;
   CXTUResourceUsage_SourceManagerContentCache : constant CXTUResourceUsageKind := 5;
   CXTUResourceUsage_AST_SideTables : constant CXTUResourceUsageKind := 6;
   CXTUResourceUsage_SourceManager_Membuffer_Malloc : constant CXTUResourceUsageKind := 7;
   CXTUResourceUsage_SourceManager_Membuffer_MMap : constant CXTUResourceUsageKind := 8;
   CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc : constant CXTUResourceUsageKind := 9;
   CXTUResourceUsage_ExternalASTSource_Membuffer_MMap : constant CXTUResourceUsageKind := 10;
   CXTUResourceUsage_Preprocessor : constant CXTUResourceUsageKind := 11;
   CXTUResourceUsage_PreprocessingRecord : constant CXTUResourceUsageKind := 12;
   CXTUResourceUsage_SourceManager_DataStructures : constant CXTUResourceUsageKind := 13;
   CXTUResourceUsage_Preprocessor_HeaderSearch : constant CXTUResourceUsageKind := 14;
   CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN : constant CXTUResourceUsageKind := 1;
   CXTUResourceUsage_MEMORY_IN_BYTES_END : constant CXTUResourceUsageKind := 14;
   CXTUResourceUsage_First : constant CXTUResourceUsageKind := 1;
   CXTUResourceUsage_Last : constant CXTUResourceUsageKind := 14;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1450

   function clang_getTUResourceUsageName (kind : CXTUResourceUsageKind) return Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1478
   pragma Import (C, clang_getTUResourceUsageName, "clang_getTUResourceUsageName");

   type CXTUResourceUsageEntry is record
      kind : aliased CXTUResourceUsageKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1482
      amount : aliased unsigned_long;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1485
   end record;
   pragma Convention (C_Pass_By_Copy, CXTUResourceUsageEntry);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1480

   type CXTUResourceUsage is record
      data : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1493
      numEntries : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1496
      entries : access CXTUResourceUsageEntry;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1500
   end record;
   pragma Convention (C_Pass_By_Copy, CXTUResourceUsage);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1491

   function clang_getCXTUResourceUsage (TU : CXTranslationUnit) return CXTUResourceUsage;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1508
   pragma Import (C, clang_getCXTUResourceUsage, "clang_getCXTUResourceUsage");

   procedure clang_disposeCXTUResourceUsage (usage : CXTUResourceUsage);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1510
   pragma Import (C, clang_disposeCXTUResourceUsage, "clang_disposeCXTUResourceUsage");

   subtype CXCursorKind is unsigned;
   CXCursor_UnexposedDecl : constant CXCursorKind := 1;
   CXCursor_StructDecl : constant CXCursorKind := 2;
   CXCursor_UnionDecl : constant CXCursorKind := 3;
   CXCursor_ClassDecl : constant CXCursorKind := 4;
   CXCursor_EnumDecl : constant CXCursorKind := 5;
   CXCursor_FieldDecl : constant CXCursorKind := 6;
   CXCursor_EnumConstantDecl : constant CXCursorKind := 7;
   CXCursor_FunctionDecl : constant CXCursorKind := 8;
   CXCursor_VarDecl : constant CXCursorKind := 9;
   CXCursor_ParmDecl : constant CXCursorKind := 10;
   CXCursor_ObjCInterfaceDecl : constant CXCursorKind := 11;
   CXCursor_ObjCCategoryDecl : constant CXCursorKind := 12;
   CXCursor_ObjCProtocolDecl : constant CXCursorKind := 13;
   CXCursor_ObjCPropertyDecl : constant CXCursorKind := 14;
   CXCursor_ObjCIvarDecl : constant CXCursorKind := 15;
   CXCursor_ObjCInstanceMethodDecl : constant CXCursorKind := 16;
   CXCursor_ObjCClassMethodDecl : constant CXCursorKind := 17;
   CXCursor_ObjCImplementationDecl : constant CXCursorKind := 18;
   CXCursor_ObjCCategoryImplDecl : constant CXCursorKind := 19;
   CXCursor_TypedefDecl : constant CXCursorKind := 20;
   CXCursor_CXXMethod : constant CXCursorKind := 21;
   CXCursor_Namespace : constant CXCursorKind := 22;
   CXCursor_LinkageSpec : constant CXCursorKind := 23;
   CXCursor_Constructor : constant CXCursorKind := 24;
   CXCursor_Destructor : constant CXCursorKind := 25;
   CXCursor_ConversionFunction : constant CXCursorKind := 26;
   CXCursor_TemplateTypeParameter : constant CXCursorKind := 27;
   CXCursor_NonTypeTemplateParameter : constant CXCursorKind := 28;
   CXCursor_TemplateTemplateParameter : constant CXCursorKind := 29;
   CXCursor_FunctionTemplate : constant CXCursorKind := 30;
   CXCursor_ClassTemplate : constant CXCursorKind := 31;
   CXCursor_ClassTemplatePartialSpecialization : constant CXCursorKind := 32;
   CXCursor_NamespaceAlias : constant CXCursorKind := 33;
   CXCursor_UsingDirective : constant CXCursorKind := 34;
   CXCursor_UsingDeclaration : constant CXCursorKind := 35;
   CXCursor_TypeAliasDecl : constant CXCursorKind := 36;
   CXCursor_ObjCSynthesizeDecl : constant CXCursorKind := 37;
   CXCursor_ObjCDynamicDecl : constant CXCursorKind := 38;
   CXCursor_CXXAccessSpecifier : constant CXCursorKind := 39;
   CXCursor_FirstDecl : constant CXCursorKind := 1;
   CXCursor_LastDecl : constant CXCursorKind := 39;
   CXCursor_FirstRef : constant CXCursorKind := 40;
   CXCursor_ObjCSuperClassRef : constant CXCursorKind := 40;
   CXCursor_ObjCProtocolRef : constant CXCursorKind := 41;
   CXCursor_ObjCClassRef : constant CXCursorKind := 42;
   CXCursor_TypeRef : constant CXCursorKind := 43;
   CXCursor_CXXBaseSpecifier : constant CXCursorKind := 44;
   CXCursor_TemplateRef : constant CXCursorKind := 45;
   CXCursor_NamespaceRef : constant CXCursorKind := 46;
   CXCursor_MemberRef : constant CXCursorKind := 47;
   CXCursor_LabelRef : constant CXCursorKind := 48;
   CXCursor_OverloadedDeclRef : constant CXCursorKind := 49;
   CXCursor_VariableRef : constant CXCursorKind := 50;
   CXCursor_LastRef : constant CXCursorKind := 50;
   CXCursor_FirstInvalid : constant CXCursorKind := 70;
   CXCursor_InvalidFile : constant CXCursorKind := 70;
   CXCursor_NoDeclFound : constant CXCursorKind := 71;
   CXCursor_NotImplemented : constant CXCursorKind := 72;
   CXCursor_InvalidCode : constant CXCursorKind := 73;
   CXCursor_LastInvalid : constant CXCursorKind := 73;
   CXCursor_FirstExpr : constant CXCursorKind := 100;
   CXCursor_UnexposedExpr : constant CXCursorKind := 100;
   CXCursor_DeclRefExpr : constant CXCursorKind := 101;
   CXCursor_MemberRefExpr : constant CXCursorKind := 102;
   CXCursor_CallExpr : constant CXCursorKind := 103;
   CXCursor_ObjCMessageExpr : constant CXCursorKind := 104;
   CXCursor_BlockExpr : constant CXCursorKind := 105;
   CXCursor_IntegerLiteral : constant CXCursorKind := 106;
   CXCursor_FloatingLiteral : constant CXCursorKind := 107;
   CXCursor_ImaginaryLiteral : constant CXCursorKind := 108;
   CXCursor_StringLiteral : constant CXCursorKind := 109;
   CXCursor_CharacterLiteral : constant CXCursorKind := 110;
   CXCursor_ParenExpr : constant CXCursorKind := 111;
   CXCursor_UnaryOperator : constant CXCursorKind := 112;
   CXCursor_ArraySubscriptExpr : constant CXCursorKind := 113;
   CXCursor_BinaryOperator : constant CXCursorKind := 114;
   CXCursor_CompoundAssignOperator : constant CXCursorKind := 115;
   CXCursor_ConditionalOperator : constant CXCursorKind := 116;
   CXCursor_CStyleCastExpr : constant CXCursorKind := 117;
   CXCursor_CompoundLiteralExpr : constant CXCursorKind := 118;
   CXCursor_InitListExpr : constant CXCursorKind := 119;
   CXCursor_AddrLabelExpr : constant CXCursorKind := 120;
   CXCursor_StmtExpr : constant CXCursorKind := 121;
   CXCursor_GenericSelectionExpr : constant CXCursorKind := 122;
   CXCursor_GNUNullExpr : constant CXCursorKind := 123;
   CXCursor_CXXStaticCastExpr : constant CXCursorKind := 124;
   CXCursor_CXXDynamicCastExpr : constant CXCursorKind := 125;
   CXCursor_CXXReinterpretCastExpr : constant CXCursorKind := 126;
   CXCursor_CXXConstCastExpr : constant CXCursorKind := 127;
   CXCursor_CXXFunctionalCastExpr : constant CXCursorKind := 128;
   CXCursor_CXXTypeidExpr : constant CXCursorKind := 129;
   CXCursor_CXXBoolLiteralExpr : constant CXCursorKind := 130;
   CXCursor_CXXNullPtrLiteralExpr : constant CXCursorKind := 131;
   CXCursor_CXXThisExpr : constant CXCursorKind := 132;
   CXCursor_CXXThrowExpr : constant CXCursorKind := 133;
   CXCursor_CXXNewExpr : constant CXCursorKind := 134;
   CXCursor_CXXDeleteExpr : constant CXCursorKind := 135;
   CXCursor_UnaryExpr : constant CXCursorKind := 136;
   CXCursor_ObjCStringLiteral : constant CXCursorKind := 137;
   CXCursor_ObjCEncodeExpr : constant CXCursorKind := 138;
   CXCursor_ObjCSelectorExpr : constant CXCursorKind := 139;
   CXCursor_ObjCProtocolExpr : constant CXCursorKind := 140;
   CXCursor_ObjCBridgedCastExpr : constant CXCursorKind := 141;
   CXCursor_PackExpansionExpr : constant CXCursorKind := 142;
   CXCursor_SizeOfPackExpr : constant CXCursorKind := 143;
   CXCursor_LambdaExpr : constant CXCursorKind := 144;
   CXCursor_ObjCBoolLiteralExpr : constant CXCursorKind := 145;
   CXCursor_ObjCSelfExpr : constant CXCursorKind := 146;
   CXCursor_LastExpr : constant CXCursorKind := 146;
   CXCursor_FirstStmt : constant CXCursorKind := 200;
   CXCursor_UnexposedStmt : constant CXCursorKind := 200;
   CXCursor_LabelStmt : constant CXCursorKind := 201;
   CXCursor_CompoundStmt : constant CXCursorKind := 202;
   CXCursor_CaseStmt : constant CXCursorKind := 203;
   CXCursor_DefaultStmt : constant CXCursorKind := 204;
   CXCursor_IfStmt : constant CXCursorKind := 205;
   CXCursor_SwitchStmt : constant CXCursorKind := 206;
   CXCursor_WhileStmt : constant CXCursorKind := 207;
   CXCursor_DoStmt : constant CXCursorKind := 208;
   CXCursor_ForStmt : constant CXCursorKind := 209;
   CXCursor_GotoStmt : constant CXCursorKind := 210;
   CXCursor_IndirectGotoStmt : constant CXCursorKind := 211;
   CXCursor_ContinueStmt : constant CXCursorKind := 212;
   CXCursor_BreakStmt : constant CXCursorKind := 213;
   CXCursor_ReturnStmt : constant CXCursorKind := 214;
   CXCursor_GCCAsmStmt : constant CXCursorKind := 215;
   CXCursor_AsmStmt : constant CXCursorKind := 215;
   CXCursor_ObjCAtTryStmt : constant CXCursorKind := 216;
   CXCursor_ObjCAtCatchStmt : constant CXCursorKind := 217;
   CXCursor_ObjCAtFinallyStmt : constant CXCursorKind := 218;
   CXCursor_ObjCAtThrowStmt : constant CXCursorKind := 219;
   CXCursor_ObjCAtSynchronizedStmt : constant CXCursorKind := 220;
   CXCursor_ObjCAutoreleasePoolStmt : constant CXCursorKind := 221;
   CXCursor_ObjCForCollectionStmt : constant CXCursorKind := 222;
   CXCursor_CXXCatchStmt : constant CXCursorKind := 223;
   CXCursor_CXXTryStmt : constant CXCursorKind := 224;
   CXCursor_CXXForRangeStmt : constant CXCursorKind := 225;
   CXCursor_SEHTryStmt : constant CXCursorKind := 226;
   CXCursor_SEHExceptStmt : constant CXCursorKind := 227;
   CXCursor_SEHFinallyStmt : constant CXCursorKind := 228;
   CXCursor_MSAsmStmt : constant CXCursorKind := 229;
   CXCursor_NullStmt : constant CXCursorKind := 230;
   CXCursor_DeclStmt : constant CXCursorKind := 231;
   CXCursor_OMPParallelDirective : constant CXCursorKind := 232;
   CXCursor_OMPSimdDirective : constant CXCursorKind := 233;
   CXCursor_OMPForDirective : constant CXCursorKind := 234;
   CXCursor_OMPSectionsDirective : constant CXCursorKind := 235;
   CXCursor_OMPSectionDirective : constant CXCursorKind := 236;
   CXCursor_OMPSingleDirective : constant CXCursorKind := 237;
   CXCursor_OMPParallelForDirective : constant CXCursorKind := 238;
   CXCursor_OMPParallelSectionsDirective : constant CXCursorKind := 239;
   CXCursor_OMPTaskDirective : constant CXCursorKind := 240;
   CXCursor_OMPMasterDirective : constant CXCursorKind := 241;
   CXCursor_OMPCriticalDirective : constant CXCursorKind := 242;
   CXCursor_OMPTaskyieldDirective : constant CXCursorKind := 243;
   CXCursor_OMPBarrierDirective : constant CXCursorKind := 244;
   CXCursor_OMPTaskwaitDirective : constant CXCursorKind := 245;
   CXCursor_OMPFlushDirective : constant CXCursorKind := 246;
   CXCursor_SEHLeaveStmt : constant CXCursorKind := 247;
   CXCursor_LastStmt : constant CXCursorKind := 247;
   CXCursor_TranslationUnit : constant CXCursorKind := 300;
   CXCursor_FirstAttr : constant CXCursorKind := 400;
   CXCursor_UnexposedAttr : constant CXCursorKind := 400;
   CXCursor_IBActionAttr : constant CXCursorKind := 401;
   CXCursor_IBOutletAttr : constant CXCursorKind := 402;
   CXCursor_IBOutletCollectionAttr : constant CXCursorKind := 403;
   CXCursor_CXXFinalAttr : constant CXCursorKind := 404;
   CXCursor_CXXOverrideAttr : constant CXCursorKind := 405;
   CXCursor_AnnotateAttr : constant CXCursorKind := 406;
   CXCursor_AsmLabelAttr : constant CXCursorKind := 407;
   CXCursor_PackedAttr : constant CXCursorKind := 408;
   CXCursor_PureAttr : constant CXCursorKind := 409;
   CXCursor_ConstAttr : constant CXCursorKind := 410;
   CXCursor_NoDuplicateAttr : constant CXCursorKind := 411;
   CXCursor_CUDAConstantAttr : constant CXCursorKind := 412;
   CXCursor_CUDADeviceAttr : constant CXCursorKind := 413;
   CXCursor_CUDAGlobalAttr : constant CXCursorKind := 414;
   CXCursor_CUDAHostAttr : constant CXCursorKind := 415;
   CXCursor_LastAttr : constant CXCursorKind := 415;
   CXCursor_PreprocessingDirective : constant CXCursorKind := 500;
   CXCursor_MacroDefinition : constant CXCursorKind := 501;
   CXCursor_MacroExpansion : constant CXCursorKind := 502;
   CXCursor_MacroInstantiation : constant CXCursorKind := 502;
   CXCursor_InclusionDirective : constant CXCursorKind := 503;
   CXCursor_FirstPreprocessing : constant CXCursorKind := 500;
   CXCursor_LastPreprocessing : constant CXCursorKind := 503;
   CXCursor_ModuleImportDecl : constant CXCursorKind := 600;
   CXCursor_FirstExtraDecl : constant CXCursorKind := 600;
   CXCursor_LastExtraDecl : constant CXCursorKind := 600;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:1519

   type CXCursor_data_array is array (0 .. 2) of System.Address;
   type CXCursor is record
      kind : aliased CXCursorKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2270
      xdata : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2271
      data : aliased CXCursor_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2272
   end record;
   function "=" (Left, Right : CXCursor) return Boolean is abstract;
   pragma Convention (C_Pass_By_Copy, CXCursor);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2273

   --  skipped anonymous struct anon_7

   function clang_getNullCursor return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2284
   pragma Import (C, clang_getNullCursor, "clang_getNullCursor");

   function clang_getTranslationUnitCursor (arg1 : CXTranslationUnit) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2292
   pragma Import (C, clang_getTranslationUnitCursor, "clang_getTranslationUnitCursor");

   function clang_equalCursors (arg1 : CXCursor; arg2 : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2297
   pragma Import (C, clang_equalCursors, "clang_equalCursors");

   function clang_Cursor_isNull (cursor : CXCursor) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2302
   pragma Import (C, clang_Cursor_isNull, "clang_Cursor_isNull");

   function clang_hashCursor (arg1 : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2307
   pragma Import (C, clang_hashCursor, "clang_hashCursor");

   function clang_getCursorKind (arg1 : CXCursor) return CXCursorKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2312
   pragma Import (C, clang_getCursorKind, "clang_getCursorKind");

   function clang_isDeclaration (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2317
   pragma Import (C, clang_isDeclaration, "clang_isDeclaration");

   function clang_isReference (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2327
   pragma Import (C, clang_isReference, "clang_isReference");

   function clang_isExpression (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2332
   pragma Import (C, clang_isExpression, "clang_isExpression");

   function clang_isStatement (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2337
   pragma Import (C, clang_isStatement, "clang_isStatement");

   function clang_isAttribute (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2342
   pragma Import (C, clang_isAttribute, "clang_isAttribute");

   function clang_isInvalid (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2348
   pragma Import (C, clang_isInvalid, "clang_isInvalid");

   function clang_isTranslationUnit (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2354
   pragma Import (C, clang_isTranslationUnit, "clang_isTranslationUnit");

   function clang_isPreprocessing (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2360
   pragma Import (C, clang_isPreprocessing, "clang_isPreprocessing");

   function clang_isUnexposed (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2366
   pragma Import (C, clang_isUnexposed, "clang_isUnexposed");

   type CXLinkageKind is
     (CXLinkage_Invalid,
      CXLinkage_NoLinkage,
      CXLinkage_Internal,
      CXLinkage_UniqueExternal,
      CXLinkage_External);
   pragma Convention (C, CXLinkageKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2371

   function clang_getCursorLinkage (cursor : CXCursor) return CXLinkageKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2392
   pragma Import (C, clang_getCursorLinkage, "clang_getCursorLinkage");

   function clang_getCursorAvailability (cursor : CXCursor) return CXAvailabilityKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2403
   pragma Import (C, clang_getCursorAvailability, "clang_getCursorAvailability");

   type CXPlatformAvailability is record
      Platform : aliased clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2416
      Introduced : aliased CXVersion;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2420
      Deprecated : aliased CXVersion;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2425
      Obsoleted : aliased CXVersion;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2430
      Unavailable : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2434
      Message : aliased clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2439
   end record;
   pragma Convention (C_Pass_By_Copy, CXPlatformAvailability);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2409

   function clang_getCursorPlatformAvailability
     (cursor : CXCursor;
      always_deprecated : access int;
      deprecated_message : access clang_c_CXString_h.CXString;
      always_unavailable : access int;
      unavailable_message : access clang_c_CXString_h.CXString;
      availability : access CXPlatformAvailability;
      availability_size : int) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2479
   pragma Import (C, clang_getCursorPlatformAvailability, "clang_getCursorPlatformAvailability");

   procedure clang_disposeCXPlatformAvailability (availability : access CXPlatformAvailability);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2491
   pragma Import (C, clang_disposeCXPlatformAvailability, "clang_disposeCXPlatformAvailability");

   type CXLanguageKind is
     (CXLanguage_Invalid,
      CXLanguage_C,
      CXLanguage_ObjC,
      CXLanguage_CPlusPlus);
   pragma Convention (C, CXLanguageKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2496

   function clang_getCursorLanguage (cursor : CXCursor) return CXLanguageKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2506
   pragma Import (C, clang_getCursorLanguage, "clang_getCursorLanguage");

   function clang_Cursor_getTranslationUnit (arg1 : CXCursor) return CXTranslationUnit;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2511
   pragma Import (C, clang_Cursor_getTranslationUnit, "clang_Cursor_getTranslationUnit");

   --  skipped empty struct CXCursorSetImpl

   type CXCursorSet is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2517

   function clang_createCXCursorSet return CXCursorSet;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2522
   pragma Import (C, clang_createCXCursorSet, "clang_createCXCursorSet");

   procedure clang_disposeCXCursorSet (cset : CXCursorSet);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2527
   pragma Import (C, clang_disposeCXCursorSet, "clang_disposeCXCursorSet");

   function clang_CXCursorSet_contains (cset : CXCursorSet; cursor : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2534
   pragma Import (C, clang_CXCursorSet_contains, "clang_CXCursorSet_contains");

   function clang_CXCursorSet_insert (cset : CXCursorSet; cursor : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2542
   pragma Import (C, clang_CXCursorSet_insert, "clang_CXCursorSet_insert");

   function clang_getCursorSemanticParent (cursor : CXCursor) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2578
   pragma Import (C, clang_getCursorSemanticParent, "clang_getCursorSemanticParent");

   function clang_getCursorLexicalParent (cursor : CXCursor) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2614
   pragma Import (C, clang_getCursorLexicalParent, "clang_getCursorLexicalParent");

   procedure clang_getOverriddenCursors
     (cursor : CXCursor;
      overridden : System.Address;
      num_overridden : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2659
   pragma Import (C, clang_getOverriddenCursors, "clang_getOverriddenCursors");

   procedure clang_disposeOverriddenCursors (overridden : access CXCursor);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2667
   pragma Import (C, clang_disposeOverriddenCursors, "clang_disposeOverriddenCursors");

   function clang_getIncludedFile (cursor : CXCursor) return CXFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2673
   pragma Import (C, clang_getIncludedFile, "clang_getIncludedFile");

   function clang_getCursor (arg1 : CXTranslationUnit; arg2 : CXSourceLocation) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2705
   pragma Import (C, clang_getCursor, "clang_getCursor");

   function clang_getCursorLocation (arg1 : CXCursor) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2717
   pragma Import (C, clang_getCursorLocation, "clang_getCursorLocation");

   function clang_getCursorExtent (arg1 : CXCursor) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2730
   pragma Import (C, clang_getCursorExtent, "clang_getCursorExtent");

   subtype CXTypeKind is unsigned;
   CXType_Invalid : constant CXTypeKind := 0;
   CXType_Unexposed : constant CXTypeKind := 1;
   CXType_Void : constant CXTypeKind := 2;
   CXType_Bool : constant CXTypeKind := 3;
   CXType_Char_U : constant CXTypeKind := 4;
   CXType_UChar : constant CXTypeKind := 5;
   CXType_Char16 : constant CXTypeKind := 6;
   CXType_Char32 : constant CXTypeKind := 7;
   CXType_UShort : constant CXTypeKind := 8;
   CXType_UInt : constant CXTypeKind := 9;
   CXType_ULong : constant CXTypeKind := 10;
   CXType_ULongLong : constant CXTypeKind := 11;
   CXType_UInt128 : constant CXTypeKind := 12;
   CXType_Char_S : constant CXTypeKind := 13;
   CXType_SChar : constant CXTypeKind := 14;
   CXType_WChar : constant CXTypeKind := 15;
   CXType_Short : constant CXTypeKind := 16;
   CXType_Int : constant CXTypeKind := 17;
   CXType_Long : constant CXTypeKind := 18;
   CXType_LongLong : constant CXTypeKind := 19;
   CXType_Int128 : constant CXTypeKind := 20;
   CXType_Float : constant CXTypeKind := 21;
   CXType_Double : constant CXTypeKind := 22;
   CXType_LongDouble : constant CXTypeKind := 23;
   CXType_NullPtr : constant CXTypeKind := 24;
   CXType_Overload : constant CXTypeKind := 25;
   CXType_Dependent : constant CXTypeKind := 26;
   CXType_ObjCId : constant CXTypeKind := 27;
   CXType_ObjCClass : constant CXTypeKind := 28;
   CXType_ObjCSel : constant CXTypeKind := 29;
   CXType_FirstBuiltin : constant CXTypeKind := 2;
   CXType_LastBuiltin : constant CXTypeKind := 29;
   CXType_Complex : constant CXTypeKind := 100;
   CXType_Pointer : constant CXTypeKind := 101;
   CXType_BlockPointer : constant CXTypeKind := 102;
   CXType_LValueReference : constant CXTypeKind := 103;
   CXType_RValueReference : constant CXTypeKind := 104;
   CXType_Record : constant CXTypeKind := 105;
   CXType_Enum : constant CXTypeKind := 106;
   CXType_Typedef : constant CXTypeKind := 107;
   CXType_ObjCInterface : constant CXTypeKind := 108;
   CXType_ObjCObjectPointer : constant CXTypeKind := 109;
   CXType_FunctionNoProto : constant CXTypeKind := 110;
   CXType_FunctionProto : constant CXTypeKind := 111;
   CXType_ConstantArray : constant CXTypeKind := 112;
   CXType_Vector : constant CXTypeKind := 113;
   CXType_IncompleteArray : constant CXTypeKind := 114;
   CXType_VariableArray : constant CXTypeKind := 115;
   CXType_DependentSizedArray : constant CXTypeKind := 116;
   CXType_MemberPointer : constant CXTypeKind := 117;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2745

   subtype CXCallingConv is unsigned;
   CXCallingConv_Default : constant CXCallingConv := 0;
   CXCallingConv_C : constant CXCallingConv := 1;
   CXCallingConv_X86StdCall : constant CXCallingConv := 2;
   CXCallingConv_X86FastCall : constant CXCallingConv := 3;
   CXCallingConv_X86ThisCall : constant CXCallingConv := 4;
   CXCallingConv_X86Pascal : constant CXCallingConv := 5;
   CXCallingConv_AAPCS : constant CXCallingConv := 6;
   CXCallingConv_AAPCS_VFP : constant CXCallingConv := 7;
   CXCallingConv_PnaclCall : constant CXCallingConv := 8;
   CXCallingConv_IntelOclBicc : constant CXCallingConv := 9;
   CXCallingConv_X86_64Win64 : constant CXCallingConv := 10;
   CXCallingConv_X86_64SysV : constant CXCallingConv := 11;
   CXCallingConv_Invalid : constant CXCallingConv := 100;
   CXCallingConv_Unexposed : constant CXCallingConv := 200;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2812

   type CXType_data_array is array (0 .. 1) of System.Address;
   type CXType is record
      kind : aliased CXTypeKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2836
      data : aliased CXType_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2837
   end record;
   pragma Convention (C_Pass_By_Copy, CXType);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2838

   --  skipped anonymous struct anon_8

   function clang_getCursorType (C : CXCursor) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2843
   pragma Import (C, clang_getCursorType, "clang_getCursorType");

   function clang_getTypeSpelling (CT : CXType) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2851
   pragma Import (C, clang_getTypeSpelling, "clang_getTypeSpelling");

   function clang_getTypedefDeclUnderlyingType (C : CXCursor) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2859
   pragma Import (C, clang_getTypedefDeclUnderlyingType, "clang_getTypedefDeclUnderlyingType");

   function clang_getEnumDeclIntegerType (C : CXCursor) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2867
   pragma Import (C, clang_getEnumDeclIntegerType, "clang_getEnumDeclIntegerType");

   function clang_getEnumConstantDeclValue (C : CXCursor) return Long_Long_Integer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2877
   pragma Import (C, clang_getEnumConstantDeclValue, "clang_getEnumConstantDeclValue");

   function clang_getEnumConstantDeclUnsignedValue (C : CXCursor) return Extensions.unsigned_long_long;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2887
   pragma Import (C, clang_getEnumConstantDeclUnsignedValue, "clang_getEnumConstantDeclUnsignedValue");

   function clang_getFieldDeclBitWidth (C : CXCursor) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2894
   pragma Import (C, clang_getFieldDeclBitWidth, "clang_getFieldDeclBitWidth");

   function clang_Cursor_getNumArguments (C : CXCursor) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2903
   pragma Import (C, clang_Cursor_getNumArguments, "clang_Cursor_getNumArguments");

   function clang_Cursor_getArgument (C : CXCursor; i : unsigned) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2912
   pragma Import (C, clang_Cursor_getArgument, "clang_Cursor_getArgument");

   function clang_equalTypes (A : CXType; B : CXType) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2920
   pragma Import (C, clang_equalTypes, "clang_equalTypes");

   function clang_getCanonicalType (T : CXType) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2930
   pragma Import (C, clang_getCanonicalType, "clang_getCanonicalType");

   function clang_isConstQualifiedType (T : CXType) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2937
   pragma Import (C, clang_isConstQualifiedType, "clang_isConstQualifiedType");

   function clang_isVolatileQualifiedType (T : CXType) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2944
   pragma Import (C, clang_isVolatileQualifiedType, "clang_isVolatileQualifiedType");

   function clang_isRestrictQualifiedType (T : CXType) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2951
   pragma Import (C, clang_isRestrictQualifiedType, "clang_isRestrictQualifiedType");

   function clang_getPointeeType (T : CXType) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2956
   pragma Import (C, clang_getPointeeType, "clang_getPointeeType");

   function clang_getTypeDeclaration (T : CXType) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2961
   pragma Import (C, clang_getTypeDeclaration, "clang_getTypeDeclaration");

   function clang_getDeclObjCTypeEncoding (C : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2966
   pragma Import (C, clang_getDeclObjCTypeEncoding, "clang_getDeclObjCTypeEncoding");

   function clang_getTypeKindSpelling (K : CXTypeKind) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2971
   pragma Import (C, clang_getTypeKindSpelling, "clang_getTypeKindSpelling");

   function clang_getFunctionTypeCallingConv (T : CXType) return CXCallingConv;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2978
   pragma Import (C, clang_getFunctionTypeCallingConv, "clang_getFunctionTypeCallingConv");

   function clang_getResultType (T : CXType) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2985
   pragma Import (C, clang_getResultType, "clang_getResultType");

   function clang_getNumArgTypes (T : CXType) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:2993
   pragma Import (C, clang_getNumArgTypes, "clang_getNumArgTypes");

   function clang_getArgType (T : CXType; i : unsigned) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3001
   pragma Import (C, clang_getArgType, "clang_getArgType");

   function clang_isFunctionTypeVariadic (T : CXType) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3006
   pragma Import (C, clang_isFunctionTypeVariadic, "clang_isFunctionTypeVariadic");

   function clang_getCursorResultType (C : CXCursor) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3013
   pragma Import (C, clang_getCursorResultType, "clang_getCursorResultType");

   function clang_isPODType (T : CXType) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3019
   pragma Import (C, clang_isPODType, "clang_isPODType");

   function clang_getElementType (T : CXType) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3027
   pragma Import (C, clang_getElementType, "clang_getElementType");

   function clang_getNumElements (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3035
   pragma Import (C, clang_getNumElements, "clang_getNumElements");

   function clang_getArrayElementType (T : CXType) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3042
   pragma Import (C, clang_getArrayElementType, "clang_getArrayElementType");

   function clang_getArraySize (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3049
   pragma Import (C, clang_getArraySize, "clang_getArraySize");

   subtype CXTypeLayoutError is unsigned;
   CXTypeLayoutError_Invalid : constant CXTypeLayoutError := -1;
   CXTypeLayoutError_Incomplete : constant CXTypeLayoutError := -2;
   CXTypeLayoutError_Dependent : constant CXTypeLayoutError := -3;
   CXTypeLayoutError_NotConstantSize : constant CXTypeLayoutError := -4;
   CXTypeLayoutError_InvalidFieldName : constant CXTypeLayoutError := -5;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3059

   function clang_Type_getAlignOf (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3094
   pragma Import (C, clang_Type_getAlignOf, "clang_Type_getAlignOf");

   function clang_Type_getClassType (T : CXType) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3101
   pragma Import (C, clang_Type_getClassType, "clang_Type_getClassType");

   function clang_Type_getSizeOf (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3112
   pragma Import (C, clang_Type_getSizeOf, "clang_Type_getSizeOf");

   function clang_Type_getOffsetOf (T : CXType; S : Interfaces.C.Strings.chars_ptr) return Long_Long_Integer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3127
   pragma Import (C, clang_Type_getOffsetOf, "clang_Type_getOffsetOf");

   type CXRefQualifierKind is
     (CXRefQualifier_None,
      CXRefQualifier_LValue,
      CXRefQualifier_RValue);
   pragma Convention (C, CXRefQualifierKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3129

   function clang_Type_getNumTemplateArguments (T : CXType) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3145
   pragma Import (C, clang_Type_getNumTemplateArguments, "clang_Type_getNumTemplateArguments");

   function clang_Type_getTemplateArgumentAsType (T : CXType; i : unsigned) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3154
   pragma Import (C, clang_Type_getTemplateArgumentAsType, "clang_Type_getTemplateArgumentAsType");

   function clang_Type_getCXXRefQualifier (T : CXType) return CXRefQualifierKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3162
   pragma Import (C, clang_Type_getCXXRefQualifier, "clang_Type_getCXXRefQualifier");

   function clang_Cursor_isBitField (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3168
   pragma Import (C, clang_Cursor_isBitField, "clang_Cursor_isBitField");

   function clang_isVirtualBase (arg1 : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3174
   pragma Import (C, clang_isVirtualBase, "clang_isVirtualBase");

   type CX_CXXAccessSpecifier is
     (CX_CXXInvalidAccessSpecifier,
      CX_CXXPublic,
      CX_CXXProtected,
      CX_CXXPrivate);
   pragma Convention (C, CX_CXXAccessSpecifier);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3180

   function clang_getCXXAccessSpecifier (arg1 : CXCursor) return CX_CXXAccessSpecifier;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3194
   pragma Import (C, clang_getCXXAccessSpecifier, "clang_getCXXAccessSpecifier");

   function clang_getNumOverloadedDecls (cursor : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3205
   pragma Import (C, clang_getNumOverloadedDecls, "clang_getNumOverloadedDecls");

   function clang_getOverloadedDecl (cursor : CXCursor; index : unsigned) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3221
   pragma Import (C, clang_getOverloadedDecl, "clang_getOverloadedDecl");

   function clang_getIBOutletCollectionType (arg1 : CXCursor) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3240
   pragma Import (C, clang_getIBOutletCollectionType, "clang_getIBOutletCollectionType");

   type CXChildVisitResult is
     (CXChildVisit_Break,
      CXChildVisit_Continue,
      CXChildVisit_Recurse);
   pragma Convention (C, CXChildVisitResult);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3262

   type CXCursorVisitor is access function
        (arg1 : CXCursor;
         arg2 : CXCursor;
         arg3 : CXClientData) return CXChildVisitResult;
   pragma Convention (C, CXCursorVisitor);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3291

   function clang_visitChildren
     (parent : CXCursor;
      visitor : CXCursorVisitor;
      client_data : CXClientData) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3317
   pragma Import (C, clang_visitChildren, "clang_visitChildren");

   function clang_getCursorUSR (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3368
   pragma Import (C, clang_getCursorUSR, "clang_getCursorUSR");

   function clang_constructUSR_ObjCClass (class_name : Interfaces.C.Strings.chars_ptr) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3373
   pragma Import (C, clang_constructUSR_ObjCClass, "clang_constructUSR_ObjCClass");

   function clang_constructUSR_ObjCCategory (class_name : Interfaces.C.Strings.chars_ptr; category_name : Interfaces.C.Strings.chars_ptr) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3379
   pragma Import (C, clang_constructUSR_ObjCCategory, "clang_constructUSR_ObjCCategory");

   function clang_constructUSR_ObjCProtocol (protocol_name : Interfaces.C.Strings.chars_ptr) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3386
   pragma Import (C, clang_constructUSR_ObjCProtocol, "clang_constructUSR_ObjCProtocol");

   function clang_constructUSR_ObjCIvar (name : Interfaces.C.Strings.chars_ptr; classUSR : clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3393
   pragma Import (C, clang_constructUSR_ObjCIvar, "clang_constructUSR_ObjCIvar");

   function clang_constructUSR_ObjCMethod
     (name : Interfaces.C.Strings.chars_ptr;
      isInstanceMethod : unsigned;
      classUSR : clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3400
   pragma Import (C, clang_constructUSR_ObjCMethod, "clang_constructUSR_ObjCMethod");

   function clang_constructUSR_ObjCProperty (property : Interfaces.C.Strings.chars_ptr; classUSR : clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3408
   pragma Import (C, clang_constructUSR_ObjCProperty, "clang_constructUSR_ObjCProperty");

   function clang_getCursorSpelling (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3414
   pragma Import (C, clang_getCursorSpelling, "clang_getCursorSpelling");

   function clang_Cursor_getSpellingNameRange
     (arg1 : CXCursor;
      pieceIndex : unsigned;
      options : unsigned) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3427
   pragma Import (C, clang_Cursor_getSpellingNameRange, "clang_Cursor_getSpellingNameRange");

   function clang_getCursorDisplayName (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3438
   pragma Import (C, clang_getCursorDisplayName, "clang_getCursorDisplayName");

   function clang_getCursorReferenced (arg1 : CXCursor) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3450
   pragma Import (C, clang_getCursorReferenced, "clang_getCursorReferenced");

   function clang_getCursorDefinition (arg1 : CXCursor) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3480
   pragma Import (C, clang_getCursorDefinition, "clang_getCursorDefinition");

   function clang_isCursorDefinition (arg1 : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3486
   pragma Import (C, clang_isCursorDefinition, "clang_isCursorDefinition");

   function clang_getCanonicalCursor (arg1 : CXCursor) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3512
   pragma Import (C, clang_getCanonicalCursor, "clang_getCanonicalCursor");

   function clang_Cursor_getObjCSelectorIndex (arg1 : CXCursor) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3526
   pragma Import (C, clang_Cursor_getObjCSelectorIndex, "clang_Cursor_getObjCSelectorIndex");

   function clang_Cursor_isDynamicCall (C : CXCursor) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3539
   pragma Import (C, clang_Cursor_isDynamicCall, "clang_Cursor_isDynamicCall");

   function clang_Cursor_getReceiverType (C : CXCursor) return CXType;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3545
   pragma Import (C, clang_Cursor_getReceiverType, "clang_Cursor_getReceiverType");

   subtype CXObjCPropertyAttrKind is unsigned;
   CXObjCPropertyAttr_noattr : constant CXObjCPropertyAttrKind := 0;
   CXObjCPropertyAttr_readonly : constant CXObjCPropertyAttrKind := 1;
   CXObjCPropertyAttr_getter : constant CXObjCPropertyAttrKind := 2;
   CXObjCPropertyAttr_assign : constant CXObjCPropertyAttrKind := 4;
   CXObjCPropertyAttr_readwrite : constant CXObjCPropertyAttrKind := 8;
   CXObjCPropertyAttr_retain : constant CXObjCPropertyAttrKind := 16;
   CXObjCPropertyAttr_copy : constant CXObjCPropertyAttrKind := 32;
   CXObjCPropertyAttr_nonatomic : constant CXObjCPropertyAttrKind := 64;
   CXObjCPropertyAttr_setter : constant CXObjCPropertyAttrKind := 128;
   CXObjCPropertyAttr_atomic : constant CXObjCPropertyAttrKind := 256;
   CXObjCPropertyAttr_weak : constant CXObjCPropertyAttrKind := 512;
   CXObjCPropertyAttr_strong : constant CXObjCPropertyAttrKind := 1024;
   CXObjCPropertyAttr_unsafe_unretained : constant CXObjCPropertyAttrKind := 2048;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3564

   function clang_Cursor_getObjCPropertyAttributes (C : CXCursor; reserved : unsigned) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3573
   pragma Import (C, clang_Cursor_getObjCPropertyAttributes, "clang_Cursor_getObjCPropertyAttributes");

   subtype CXObjCDeclQualifierKind is unsigned;
   CXObjCDeclQualifier_None : constant CXObjCDeclQualifierKind := 0;
   CXObjCDeclQualifier_In : constant CXObjCDeclQualifierKind := 1;
   CXObjCDeclQualifier_Inout : constant CXObjCDeclQualifierKind := 2;
   CXObjCDeclQualifier_Out : constant CXObjCDeclQualifierKind := 4;
   CXObjCDeclQualifier_Bycopy : constant CXObjCDeclQualifierKind := 8;
   CXObjCDeclQualifier_Byref : constant CXObjCDeclQualifierKind := 16;
   CXObjCDeclQualifier_Oneway : constant CXObjCDeclQualifierKind := 32;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3588

   function clang_Cursor_getObjCDeclQualifiers (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3596
   pragma Import (C, clang_Cursor_getObjCDeclQualifiers, "clang_Cursor_getObjCDeclQualifiers");

   function clang_Cursor_isObjCOptional (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3603
   pragma Import (C, clang_Cursor_isObjCOptional, "clang_Cursor_isObjCOptional");

   function clang_Cursor_isVariadic (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3608
   pragma Import (C, clang_Cursor_isVariadic, "clang_Cursor_isVariadic");

   function clang_Cursor_getCommentRange (C : CXCursor) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3615
   pragma Import (C, clang_Cursor_getCommentRange, "clang_Cursor_getCommentRange");

   function clang_Cursor_getRawCommentText (C : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3621
   pragma Import (C, clang_Cursor_getRawCommentText, "clang_Cursor_getRawCommentText");

   function clang_Cursor_getBriefCommentText (C : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3628
   pragma Import (C, clang_Cursor_getBriefCommentText, "clang_Cursor_getBriefCommentText");

   type CXModule is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3642

   function clang_Cursor_getModule (C : CXCursor) return CXModule;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3647
   pragma Import (C, clang_Cursor_getModule, "clang_Cursor_getModule");

   function clang_getModuleForFile (arg1 : CXTranslationUnit; arg2 : CXFile) return CXModule;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3653
   pragma Import (C, clang_getModuleForFile, "clang_getModuleForFile");

   function clang_Module_getASTFile (Module : CXModule) return CXFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3660
   pragma Import (C, clang_Module_getASTFile, "clang_Module_getASTFile");

   function clang_Module_getParent (Module : CXModule) return CXModule;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3668
   pragma Import (C, clang_Module_getParent, "clang_Module_getParent");

   function clang_Module_getName (Module : CXModule) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3676
   pragma Import (C, clang_Module_getName, "clang_Module_getName");

   function clang_Module_getFullName (Module : CXModule) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3683
   pragma Import (C, clang_Module_getFullName, "clang_Module_getFullName");

   function clang_Module_isSystem (Module : CXModule) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3690
   pragma Import (C, clang_Module_isSystem, "clang_Module_isSystem");

   function clang_Module_getNumTopLevelHeaders (arg1 : CXTranslationUnit; Module : CXModule) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3697
   pragma Import (C, clang_Module_getNumTopLevelHeaders, "clang_Module_getNumTopLevelHeaders");

   function clang_Module_getTopLevelHeader
     (arg1 : CXTranslationUnit;
      Module : CXModule;
      Index : unsigned) return CXFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3708
   pragma Import (C, clang_Module_getTopLevelHeader, "clang_Module_getTopLevelHeader");

   function clang_CXXMethod_isPureVirtual (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3728
   pragma Import (C, clang_CXXMethod_isPureVirtual, "clang_CXXMethod_isPureVirtual");

   function clang_CXXMethod_isStatic (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3734
   pragma Import (C, clang_CXXMethod_isStatic, "clang_CXXMethod_isStatic");

   function clang_CXXMethod_isVirtual (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3741
   pragma Import (C, clang_CXXMethod_isVirtual, "clang_CXXMethod_isVirtual");

   function clang_CXXMethod_isConst (C : CXCursor) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3747
   pragma Import (C, clang_CXXMethod_isConst, "clang_CXXMethod_isConst");

   function clang_getTemplateCursorKind (C : CXCursor) return CXCursorKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3766
   pragma Import (C, clang_getTemplateCursorKind, "clang_getTemplateCursorKind");

   function clang_getSpecializedCursorTemplate (C : CXCursor) return CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3796
   pragma Import (C, clang_getSpecializedCursorTemplate, "clang_getSpecializedCursorTemplate");

   function clang_getCursorReferenceNameRange
     (C : CXCursor;
      NameFlags : unsigned;
      PieceIndex : unsigned) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3816
   pragma Import (C, clang_getCursorReferenceNameRange, "clang_getCursorReferenceNameRange");

   subtype CXNameRefFlags is unsigned;
   CXNameRange_WantQualifier : constant CXNameRefFlags := 1;
   CXNameRange_WantTemplateArgs : constant CXNameRefFlags := 2;
   CXNameRange_WantSinglePiece : constant CXNameRefFlags := 4;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3820

   type CXTokenKind is
     (CXToken_Punctuation,
      CXToken_Keyword,
      CXToken_Identifier,
      CXToken_Literal,
      CXToken_Comment);
   pragma Convention (C, CXTokenKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3863

   type CXToken_int_data_array is array (0 .. 3) of aliased unsigned;
   type CXToken is record
      int_data : aliased CXToken_int_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3894
      ptr_data : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3895
   end record;
   pragma Convention (C_Pass_By_Copy, CXToken);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3896

   --  skipped anonymous struct anon_11

   function clang_getTokenKind (arg1 : CXToken) return CXTokenKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3901
   pragma Import (C, clang_getTokenKind, "clang_getTokenKind");

   function clang_getTokenSpelling (arg1 : CXTranslationUnit; arg2 : CXToken) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3909
   pragma Import (C, clang_getTokenSpelling, "clang_getTokenSpelling");

   function clang_getTokenLocation (arg1 : CXTranslationUnit; arg2 : CXToken) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3914
   pragma Import (C, clang_getTokenLocation, "clang_getTokenLocation");

   function clang_getTokenExtent (arg1 : CXTranslationUnit; arg2 : CXToken) return CXSourceRange;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3920
   pragma Import (C, clang_getTokenExtent, "clang_getTokenExtent");

   procedure clang_tokenize
     (TU : CXTranslationUnit;
      c_Range : CXSourceRange;
      Tokens : System.Address;
      NumTokens : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3939
   pragma Import (C, clang_tokenize, "clang_tokenize");

   procedure clang_annotateTokens
     (TU : CXTranslationUnit;
      Tokens : access CXToken;
      NumTokens : unsigned;
      Cursors : access CXCursor);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3972
   pragma Import (C, clang_annotateTokens, "clang_annotateTokens");

   procedure clang_disposeTokens
     (TU : CXTranslationUnit;
      Tokens : access CXToken;
      NumTokens : unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3979
   pragma Import (C, clang_disposeTokens, "clang_disposeTokens");

   function clang_getCursorKindSpelling (Kind : CXCursorKind) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3996
   pragma Import (C, clang_getCursorKindSpelling, "clang_getCursorKindSpelling");

   procedure clang_getDefinitionSpellingAndExtent
     (arg1 : CXCursor;
      startBuf : System.Address;
      endBuf : System.Address;
      startLine : access unsigned;
      startColumn : access unsigned;
      endLine : access unsigned;
      endColumn : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:3997
   pragma Import (C, clang_getDefinitionSpellingAndExtent, "clang_getDefinitionSpellingAndExtent");

   procedure clang_enableStackTraces;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4004
   pragma Import (C, clang_enableStackTraces, "clang_enableStackTraces");

   procedure clang_executeOnThread
     (fn : access procedure (arg1 : System.Address);
      user_data : System.Address;
      stack_size : unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4005
   pragma Import (C, clang_executeOnThread, "clang_executeOnThread");

   type CXCompletionString is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4037

   type CXCompletionResult is record
      CursorKind : aliased CXCursorKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4053
      CompletionString : CXCompletionString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4059
   end record;
   pragma Convention (C_Pass_By_Copy, CXCompletionResult);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4060

   --  skipped anonymous struct anon_12

   type CXCompletionChunkKind is
     (CXCompletionChunk_Optional,
      CXCompletionChunk_TypedText,
      CXCompletionChunk_Text,
      CXCompletionChunk_Placeholder,
      CXCompletionChunk_Informative,
      CXCompletionChunk_CurrentParameter,
      CXCompletionChunk_LeftParen,
      CXCompletionChunk_RightParen,
      CXCompletionChunk_LeftBracket,
      CXCompletionChunk_RightBracket,
      CXCompletionChunk_LeftBrace,
      CXCompletionChunk_RightBrace,
      CXCompletionChunk_LeftAngle,
      CXCompletionChunk_RightAngle,
      CXCompletionChunk_Comma,
      CXCompletionChunk_ResultType,
      CXCompletionChunk_Colon,
      CXCompletionChunk_SemiColon,
      CXCompletionChunk_Equal,
      CXCompletionChunk_HorizontalSpace,
      CXCompletionChunk_VerticalSpace);
   pragma Convention (C, CXCompletionChunkKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4069

   function clang_getCompletionChunkKind (completion_string : CXCompletionString; chunk_number : unsigned) return CXCompletionChunkKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4243
   pragma Import (C, clang_getCompletionChunkKind, "clang_getCompletionChunkKind");

   function clang_getCompletionChunkText (completion_string : CXCompletionString; chunk_number : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4257
   pragma Import (C, clang_getCompletionChunkText, "clang_getCompletionChunkText");

   function clang_getCompletionChunkCompletionString (completion_string : CXCompletionString; chunk_number : unsigned) return CXCompletionString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4272
   pragma Import (C, clang_getCompletionChunkCompletionString, "clang_getCompletionChunkCompletionString");

   function clang_getNumCompletionChunks (completion_string : CXCompletionString) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4279
   pragma Import (C, clang_getNumCompletionChunks, "clang_getNumCompletionChunks");

   function clang_getCompletionPriority (completion_string : CXCompletionString) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4294
   pragma Import (C, clang_getCompletionPriority, "clang_getCompletionPriority");

   function clang_getCompletionAvailability (completion_string : CXCompletionString) return CXAvailabilityKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4305
   pragma Import (C, clang_getCompletionAvailability, "clang_getCompletionAvailability");

   function clang_getCompletionNumAnnotations (completion_string : CXCompletionString) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4317
   pragma Import (C, clang_getCompletionNumAnnotations, "clang_getCompletionNumAnnotations");

   function clang_getCompletionAnnotation (completion_string : CXCompletionString; annotation_number : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4331
   pragma Import (C, clang_getCompletionAnnotation, "clang_getCompletionAnnotation");

   function clang_getCompletionParent (completion_string : CXCompletionString; kind : access CXCursorKind) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4351
   pragma Import (C, clang_getCompletionParent, "clang_getCompletionParent");

   function clang_getCompletionBriefComment (completion_string : CXCompletionString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4359
   pragma Import (C, clang_getCompletionBriefComment, "clang_getCompletionBriefComment");

   function clang_getCursorCompletionString (cursor : CXCursor) return CXCompletionString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4371
   pragma Import (C, clang_getCursorCompletionString, "clang_getCursorCompletionString");

   type CXCodeCompleteResults is record
      Results : access CXCompletionResult;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4384
      NumResults : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4390
   end record;
   pragma Convention (C_Pass_By_Copy, CXCodeCompleteResults);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4391

   --  skipped anonymous struct anon_13

   subtype CXCodeComplete_Flags is unsigned;
   CXCodeComplete_IncludeMacros : constant CXCodeComplete_Flags := 1;
   CXCodeComplete_IncludeCodePatterns : constant CXCodeComplete_Flags := 2;
   CXCodeComplete_IncludeBriefComments : constant CXCodeComplete_Flags := 4;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4400

   subtype CXCompletionContext is unsigned;
   CXCompletionContext_Unexposed : constant CXCompletionContext := 0;
   CXCompletionContext_AnyType : constant CXCompletionContext := 1;
   CXCompletionContext_AnyValue : constant CXCompletionContext := 2;
   CXCompletionContext_ObjCObjectValue : constant CXCompletionContext := 4;
   CXCompletionContext_ObjCSelectorValue : constant CXCompletionContext := 8;
   CXCompletionContext_CXXClassTypeValue : constant CXCompletionContext := 16;
   CXCompletionContext_DotMemberAccess : constant CXCompletionContext := 32;
   CXCompletionContext_ArrowMemberAccess : constant CXCompletionContext := 64;
   CXCompletionContext_ObjCPropertyAccess : constant CXCompletionContext := 128;
   CXCompletionContext_EnumTag : constant CXCompletionContext := 256;
   CXCompletionContext_UnionTag : constant CXCompletionContext := 512;
   CXCompletionContext_StructTag : constant CXCompletionContext := 1024;
   CXCompletionContext_ClassTag : constant CXCompletionContext := 2048;
   CXCompletionContext_Namespace : constant CXCompletionContext := 4096;
   CXCompletionContext_NestedNameSpecifier : constant CXCompletionContext := 8192;
   CXCompletionContext_ObjCInterface : constant CXCompletionContext := 16384;
   CXCompletionContext_ObjCProtocol : constant CXCompletionContext := 32768;
   CXCompletionContext_ObjCCategory : constant CXCompletionContext := 65536;
   CXCompletionContext_ObjCInstanceMessage : constant CXCompletionContext := 131072;
   CXCompletionContext_ObjCClassMessage : constant CXCompletionContext := 262144;
   CXCompletionContext_ObjCSelectorName : constant CXCompletionContext := 524288;
   CXCompletionContext_MacroName : constant CXCompletionContext := 1048576;
   CXCompletionContext_NaturalLanguage : constant CXCompletionContext := 2097152;
   CXCompletionContext_Unknown : constant CXCompletionContext := 4194303;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4426

   function clang_defaultCodeCompleteOptions return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4555
   pragma Import (C, clang_defaultCodeCompleteOptions, "clang_defaultCodeCompleteOptions");

   function clang_codeCompleteAt
     (TU : CXTranslationUnit;
      complete_filename : Interfaces.C.Strings.chars_ptr;
      complete_line : unsigned;
      complete_column : unsigned;
      unsaved_files : access CXUnsavedFile;
      num_unsaved_files : unsigned;
      options : unsigned) return access CXCodeCompleteResults;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4626
   pragma Import (C, clang_codeCompleteAt, "clang_codeCompleteAt");

   procedure clang_sortCodeCompletionResults (Results : access CXCompletionResult; NumResults : unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4642
   pragma Import (C, clang_sortCodeCompletionResults, "clang_sortCodeCompletionResults");

   procedure clang_disposeCodeCompleteResults (Results : access CXCodeCompleteResults);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4649
   pragma Import (C, clang_disposeCodeCompleteResults, "clang_disposeCodeCompleteResults");

   function clang_codeCompleteGetNumDiagnostics (Results : access CXCodeCompleteResults) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4656
   pragma Import (C, clang_codeCompleteGetNumDiagnostics, "clang_codeCompleteGetNumDiagnostics");

   function clang_codeCompleteGetDiagnostic (Results : access CXCodeCompleteResults; Index : unsigned) return CXDiagnostic;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4668
   pragma Import (C, clang_codeCompleteGetDiagnostic, "clang_codeCompleteGetDiagnostic");

   function clang_codeCompleteGetContexts (Results : access CXCodeCompleteResults) return Extensions.unsigned_long_long;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4681
   pragma Import (C, clang_codeCompleteGetContexts, "clang_codeCompleteGetContexts");

   function clang_codeCompleteGetContainerKind (Results : access CXCodeCompleteResults; IsIncomplete : access unsigned) return CXCursorKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4701
   pragma Import (C, clang_codeCompleteGetContainerKind, "clang_codeCompleteGetContainerKind");

   function clang_codeCompleteGetContainerUSR (Results : access CXCodeCompleteResults) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4715
   pragma Import (C, clang_codeCompleteGetContainerUSR, "clang_codeCompleteGetContainerUSR");

   function clang_codeCompleteGetObjCSelector (Results : access CXCodeCompleteResults) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4730
   pragma Import (C, clang_codeCompleteGetObjCSelector, "clang_codeCompleteGetObjCSelector");

   function clang_getClangVersion return clang_c_CXString_h.CXString;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4747
   pragma Import (C, clang_getClangVersion, "clang_getClangVersion");

   procedure clang_toggleCrashRecovery (isEnabled : unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4756
   pragma Import (C, clang_toggleCrashRecovery, "clang_toggleCrashRecovery");

   type CXInclusionVisitor is access procedure
        (arg1 : CXFile;
         arg2 : access CXSourceLocation;
         arg3 : unsigned;
         arg4 : CXClientData);
   pragma Convention (C, CXInclusionVisitor);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4769

   procedure clang_getInclusions
     (tu : CXTranslationUnit;
      visitor : CXInclusionVisitor;
      client_data : CXClientData);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4780
   pragma Import (C, clang_getInclusions, "clang_getInclusions");

   type CXRemapping is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4796

   function clang_getRemappings (path : Interfaces.C.Strings.chars_ptr) return CXRemapping;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4806
   pragma Import (C, clang_getRemappings, "clang_getRemappings");

   function clang_getRemappingsFromFileList (filePaths : System.Address; numFiles : unsigned) return CXRemapping;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4819
   pragma Import (C, clang_getRemappingsFromFileList, "clang_getRemappingsFromFileList");

   function clang_remap_getNumFiles (arg1 : CXRemapping) return unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4825
   pragma Import (C, clang_remap_getNumFiles, "clang_remap_getNumFiles");

   procedure clang_remap_getFilenames
     (arg1 : CXRemapping;
      index : unsigned;
      original : access clang_c_CXString_h.CXString;
      transformed : access clang_c_CXString_h.CXString);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4835
   pragma Import (C, clang_remap_getFilenames, "clang_remap_getFilenames");

   procedure clang_remap_dispose (arg1 : CXRemapping);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4841
   pragma Import (C, clang_remap_dispose, "clang_remap_dispose");

   type CXVisitorResult is
     (CXVisit_Break,
      CXVisit_Continue);
   pragma Convention (C, CXVisitorResult);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4852

   type CXCursorAndRangeVisitor is record
      context : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4858
      visit : access function
           (arg1 : System.Address;
            arg2 : CXCursor;
            arg3 : CXSourceRange) return CXVisitorResult;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4859
   end record;
   pragma Convention (C_Pass_By_Copy, CXCursorAndRangeVisitor);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4860

   --  skipped anonymous struct anon_14

   type CXResult is
     (CXResult_Success,
      CXResult_Invalid,
      CXResult_VisitBreak);
   pragma Convention (C, CXResult);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4877

   function clang_findReferencesInFile
     (cursor : CXCursor;
      file : CXFile;
      visitor : CXCursorAndRangeVisitor) return CXResult;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4893
   pragma Import (C, clang_findReferencesInFile, "clang_findReferencesInFile");

   function clang_findIncludesInFile
     (TU : CXTranslationUnit;
      file : CXFile;
      visitor : CXCursorAndRangeVisitor) return CXResult;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4908
   pragma Import (C, clang_findIncludesInFile, "clang_findIncludesInFile");

   type CXIdxClientFile is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4932

   type CXIdxClientEntity is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4937

   type CXIdxClientContainer is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4943

   type CXIdxClientASTFile is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4949

   type CXIdxLoc_ptr_data_array is array (0 .. 1) of System.Address;
   type CXIdxLoc is record
      ptr_data : aliased CXIdxLoc_ptr_data_array;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4955
      int_data : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4956
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxLoc);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4957

   --  skipped anonymous struct anon_16

   type CXIdxIncludedFileInfo is record
      hashLoc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4966
      filename : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4970
      file : CXFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4974
      isImport : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4975
      isAngled : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4976
      isModuleImport : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4981
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxIncludedFileInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4982

   --  skipped anonymous struct anon_17

   type CXIdxImportedASTFileInfo is record
      file : CXFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4991
      module : CXModule;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4995
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:4999
      isImplicit : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5004
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxImportedASTFileInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5006

   --  skipped anonymous struct anon_18

   type CXIdxEntityKind is
     (CXIdxEntity_Unexposed,
      CXIdxEntity_Typedef,
      CXIdxEntity_Function,
      CXIdxEntity_Variable,
      CXIdxEntity_Field,
      CXIdxEntity_EnumConstant,
      CXIdxEntity_ObjCClass,
      CXIdxEntity_ObjCProtocol,
      CXIdxEntity_ObjCCategory,
      CXIdxEntity_ObjCInstanceMethod,
      CXIdxEntity_ObjCClassMethod,
      CXIdxEntity_ObjCProperty,
      CXIdxEntity_ObjCIvar,
      CXIdxEntity_Enum,
      CXIdxEntity_Struct,
      CXIdxEntity_Union,
      CXIdxEntity_CXXClass,
      CXIdxEntity_CXXNamespace,
      CXIdxEntity_CXXNamespaceAlias,
      CXIdxEntity_CXXStaticVariable,
      CXIdxEntity_CXXStaticMethod,
      CXIdxEntity_CXXInstanceMethod,
      CXIdxEntity_CXXConstructor,
      CXIdxEntity_CXXDestructor,
      CXIdxEntity_CXXConversionFunction,
      CXIdxEntity_CXXTypeAlias,
      CXIdxEntity_CXXInterface);
   pragma Convention (C, CXIdxEntityKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5041

   type CXIdxEntityLanguage is
     (CXIdxEntityLang_None,
      CXIdxEntityLang_C,
      CXIdxEntityLang_ObjC,
      CXIdxEntityLang_CXX);
   pragma Convention (C, CXIdxEntityLanguage);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5048

   type CXIdxEntityCXXTemplateKind is
     (CXIdxEntity_NonTemplate,
      CXIdxEntity_Template,
      CXIdxEntity_TemplatePartialSpecialization,
      CXIdxEntity_TemplateSpecialization);
   pragma Convention (C, CXIdxEntityCXXTemplateKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5065

   type CXIdxAttrKind is
     (CXIdxAttr_Unexposed,
      CXIdxAttr_IBAction,
      CXIdxAttr_IBOutlet,
      CXIdxAttr_IBOutletCollection);
   pragma Convention (C, CXIdxAttrKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5072

   type CXIdxAttrInfo is record
      kind : aliased CXIdxAttrKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5075
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5076
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5077
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxAttrInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5078

   --  skipped anonymous struct anon_23

   type CXIdxEntityInfo is record
      kind : aliased CXIdxEntityKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5081
      templateKind : aliased CXIdxEntityCXXTemplateKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5082
      lang : aliased CXIdxEntityLanguage;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5083
      name : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5084
      USR : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5085
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5086
      attributes : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5087
      numAttributes : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5088
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxEntityInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5089

   --  skipped anonymous struct anon_24

   type CXIdxContainerInfo is record
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5092
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxContainerInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5093

   --  skipped anonymous struct anon_25

   type CXIdxIBOutletCollectionAttrInfo is record
      attrInfo : access constant CXIdxAttrInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5096
      objcClass : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5097
      classCursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5098
      classLoc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5099
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxIBOutletCollectionAttrInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5100

   --  skipped anonymous struct anon_26

   subtype CXIdxDeclInfoFlags is unsigned;
   CXIdxDeclFlag_Skipped : constant CXIdxDeclInfoFlags := 1;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5104

   type CXIdxDeclInfo is record
      entityInfo : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5107
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5108
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5109
      semanticContainer : access constant CXIdxContainerInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5110
      lexicalContainer : access constant CXIdxContainerInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5115
      isRedeclaration : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5116
      isDefinition : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5117
      isContainer : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5118
      declAsContainer : access constant CXIdxContainerInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5119
      isImplicit : aliased int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5124
      attributes : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5125
      numAttributes : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5126
      flags : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5128
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5130

   --  skipped anonymous struct anon_28

   type CXIdxObjCContainerKind is
     (CXIdxObjCContainer_ForwardRef,
      CXIdxObjCContainer_Interface,
      CXIdxObjCContainer_Implementation);
   pragma Convention (C, CXIdxObjCContainerKind);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5136

   type CXIdxObjCContainerDeclInfo is record
      declInfo : access constant CXIdxDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5139
      kind : aliased CXIdxObjCContainerKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5140
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCContainerDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5141

   --  skipped anonymous struct anon_30

   type CXIdxBaseClassInfo is record
      base : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5144
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5145
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5146
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxBaseClassInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5147

   --  skipped anonymous struct anon_31

   type CXIdxObjCProtocolRefInfo is record
      protocol : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5150
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5151
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5152
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCProtocolRefInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5153

   --  skipped anonymous struct anon_32

   type CXIdxObjCProtocolRefListInfo is record
      protocols : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5156
      numProtocols : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5157
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCProtocolRefListInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5158

   --  skipped anonymous struct anon_33

   type CXIdxObjCInterfaceDeclInfo is record
      containerInfo : access constant CXIdxObjCContainerDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5161
      superInfo : access constant CXIdxBaseClassInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5162
      protocols : access constant CXIdxObjCProtocolRefListInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5163
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCInterfaceDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5164

   --  skipped anonymous struct anon_34

   type CXIdxObjCCategoryDeclInfo is record
      containerInfo : access constant CXIdxObjCContainerDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5167
      objcClass : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5168
      classCursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5169
      classLoc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5170
      protocols : access constant CXIdxObjCProtocolRefListInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5171
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCCategoryDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5172

   --  skipped anonymous struct anon_35

   type CXIdxObjCPropertyDeclInfo is record
      declInfo : access constant CXIdxDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5175
      getter : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5176
      setter : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5177
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCPropertyDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5178

   --  skipped anonymous struct anon_36

   type CXIdxCXXClassDeclInfo is record
      declInfo : access constant CXIdxDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5181
      bases : System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5182
      numBases : aliased unsigned;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5183
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxCXXClassDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5184

   --  skipped anonymous struct anon_37

   subtype CXIdxEntityRefKind is unsigned;
   CXIdxEntityRef_Direct : constant CXIdxEntityRefKind := 1;
   CXIdxEntityRef_Implicit : constant CXIdxEntityRefKind := 2;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5199
--
   type CXIdxEntityRefInfo is record
      kind : aliased CXIdxEntityRefKind;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5205
      cursor : aliased CXCursor;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5209
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5210
      referencedEntity : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5214
      parentEntity : access constant CXIdxEntityInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5226
      container : access constant CXIdxContainerInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5230
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxEntityRefInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5231

   --  skipped anonymous struct anon_39

   type IndexerCallbacks is record
      abortQuery : access function (arg1 : CXClientData; arg2 : System.Address) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5242
      diagnostic : access procedure
           (arg1 : CXClientData;
            arg2 : CXDiagnosticSet;
            arg3 : System.Address);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5248
      enteredMainFile : access function
           (arg1 : CXClientData;
            arg2 : CXFile;
            arg3 : System.Address) return CXIdxClientFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5251
      ppIncludedFile : access function (arg1 : CXClientData; arg2 : access constant CXIdxIncludedFileInfo) return CXIdxClientFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5257
      importedASTFile : access function (arg1 : CXClientData; arg2 : access constant CXIdxImportedASTFileInfo) return CXIdxClientASTFile;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5268
      startedTranslationUnit : access function (arg1 : CXClientData; arg2 : System.Address) return CXIdxClientContainer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5274
      indexDeclaration : access procedure (arg1 : CXClientData; arg2 : access constant CXIdxDeclInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5277
      indexEntityReference : access procedure (arg1 : CXClientData; arg2 : access constant CXIdxEntityRefInfo);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5283
   end record;
   pragma Convention (C_Pass_By_Copy, IndexerCallbacks);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5285

   --  skipped anonymous struct anon_40

   function clang_index_isEntityObjCContainerKind (arg1 : CXIdxEntityKind) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5287
   pragma Import (C, clang_index_isEntityObjCContainerKind, "clang_index_isEntityObjCContainerKind");

   function clang_index_getObjCContainerDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCContainerDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5289
   pragma Import (C, clang_index_getObjCContainerDeclInfo, "clang_index_getObjCContainerDeclInfo");

   function clang_index_getObjCInterfaceDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCInterfaceDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5292
   pragma Import (C, clang_index_getObjCInterfaceDeclInfo, "clang_index_getObjCInterfaceDeclInfo");

   function clang_index_getObjCCategoryDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCCategoryDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5296
   pragma Import (C, clang_index_getObjCCategoryDeclInfo, "clang_index_getObjCCategoryDeclInfo");

   function clang_index_getObjCProtocolRefListInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCProtocolRefListInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5299
   pragma Import (C, clang_index_getObjCProtocolRefListInfo, "clang_index_getObjCProtocolRefListInfo");

   function clang_index_getObjCPropertyDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCPropertyDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5302
   pragma Import (C, clang_index_getObjCPropertyDeclInfo, "clang_index_getObjCPropertyDeclInfo");

   function clang_index_getIBOutletCollectionAttrInfo (arg1 : access constant CXIdxAttrInfo) return access constant CXIdxIBOutletCollectionAttrInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5305
   pragma Import (C, clang_index_getIBOutletCollectionAttrInfo, "clang_index_getIBOutletCollectionAttrInfo");

   function clang_index_getCXXClassDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxCXXClassDeclInfo;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5308
   pragma Import (C, clang_index_getCXXClassDeclInfo, "clang_index_getCXXClassDeclInfo");

   function clang_index_getClientContainer (arg1 : access constant CXIdxContainerInfo) return CXIdxClientContainer;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5315
   pragma Import (C, clang_index_getClientContainer, "clang_index_getClientContainer");

   procedure clang_index_setClientContainer (arg1 : access constant CXIdxContainerInfo; arg2 : CXIdxClientContainer);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5322
   pragma Import (C, clang_index_setClientContainer, "clang_index_setClientContainer");

   function clang_index_getClientEntity (arg1 : access constant CXIdxEntityInfo) return CXIdxClientEntity;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5328
   pragma Import (C, clang_index_getClientEntity, "clang_index_getClientEntity");

   procedure clang_index_setClientEntity (arg1 : access constant CXIdxEntityInfo; arg2 : CXIdxClientEntity);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5334
   pragma Import (C, clang_index_setClientEntity, "clang_index_setClientEntity");

   type CXIndexAction is new System.Address;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5340

   function clang_IndexAction_create (CIdx : CXIndex) return CXIndexAction;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5348
   pragma Import (C, clang_IndexAction_create, "clang_IndexAction_create");

   procedure clang_IndexAction_dispose (arg1 : CXIndexAction);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5356
   pragma Import (C, clang_IndexAction_dispose, "clang_IndexAction_dispose");

   subtype CXIndexOptFlags is unsigned;
   CXIndexOpt_None : constant CXIndexOptFlags := 0;
   CXIndexOpt_SuppressRedundantRefs : constant CXIndexOptFlags := 1;
   CXIndexOpt_IndexFunctionLocalSymbols : constant CXIndexOptFlags := 2;
   CXIndexOpt_IndexImplicitTemplateInstantiations : constant CXIndexOptFlags := 4;
   CXIndexOpt_SuppressWarnings : constant CXIndexOptFlags := 8;
   CXIndexOpt_SkipParsedBodiesInSession : constant CXIndexOptFlags := 16;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5395

   function clang_indexSourceFile
     (arg1 : CXIndexAction;
      client_data : CXClientData;
      index_callbacks : access constant IndexerCallbacks;
      index_callbacks_size : unsigned;
      index_options : unsigned;
      Source_Filename : Interfaces.C.Strings.chars_ptr;
      Command_Line_Args : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files : System.Address;
      Num_Unsaved_Files : unsigned;
      Out_TU : System.Address;
      TU_Options : unsigned) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5422
   pragma Import (C, clang_indexSourceFile, "clang_indexSourceFile");

   function clang_indexTranslationUnit
     (arg1 : CXIndexAction;
      client_data : CXClientData;
      index_callbacks : access constant IndexerCallbacks;
      index_callbacks_size : unsigned;
      index_options : unsigned;
      arg6 : CXTranslationUnit) return int;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5451
   pragma Import (C, clang_indexTranslationUnit, "clang_indexTranslationUnit");

   procedure clang_indexLoc_getFileLocation
     (loc : CXIdxLoc;
      indexFile : System.Address;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5466
   pragma Import (C, clang_indexLoc_getFileLocation, "clang_indexLoc_getFileLocation");

   function clang_indexLoc_getCXSourceLocation (loc : CXIdxLoc) return CXSourceLocation;  -- /export/work/setton/src/GPS/src/gps/libclang/cfe-3.5.0.src/include/clang-c/Index.h:5477
   pragma Import (C, clang_indexLoc_getCXSourceLocation, "clang_indexLoc_getCXSourceLocation");

end clang_c_Index_h;
