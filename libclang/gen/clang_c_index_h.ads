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

   --  unsupported macro: CINDEX_VERSION_MAJOR 0
   --  unsupported macro: CINDEX_VERSION_MINOR 30
   --  arg-macro: function CINDEX_VERSION_ENCODE ( ((major) * 10000) + ((minor) * 1)
   --    return  ((major) * 10000) + ((minor) * 1);
   --  unsupported macro: CINDEX_VERSION CINDEX_VERSION_ENCODE( CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR )
   --  unsupported macro: CINDEX_VERSION_STRINGIZE_(major,minor) #major"."#minor
   --  arg-macro: procedure CINDEX_VERSION_STRINGIZE CINDEX_VERSION_STRINGIZE_(major, minor)
   --    CINDEX_VERSION_STRINGIZE_(major, minor)
   --  unsupported macro: CINDEX_VERSION_STRING CINDEX_VERSION_STRINGIZE( CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR)
   type CXIndex is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:81

   --  skipped empty struct CXTranslationUnitImpl

   type CXTranslationUnit is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:86

   type CXClientData is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:92

   type CXUnsavedFile is record
      Filename : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:107
      Contents : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:112
      Length : aliased unsigned_long;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:117
   end record;
   pragma Convention (C_Pass_By_Copy, CXUnsavedFile);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:101

   type CXAvailabilityKind is 
     (CXAvailability_Available,
      CXAvailability_Deprecated,
      CXAvailability_NotAvailable,
      CXAvailability_NotAccessible);
   pragma Convention (C, CXAvailabilityKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:125

   type CXVersion is record
      Major : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:154
      Minor : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:160
      Subminor : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:166
   end record;
   pragma Convention (C_Pass_By_Copy, CXVersion);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:149

   function clang_createIndex (excludeDeclarationsFromPCH : int; displayDiagnostics : int) return CXIndex;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:209
   pragma Import (C, clang_createIndex, "clang_createIndex");

   procedure clang_disposeIndex (index : CXIndex);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:218
   pragma Import (C, clang_disposeIndex, "clang_disposeIndex");

   type CXGlobalOptFlags is 
     (CXGlobalOpt_None,
      CXGlobalOpt_ThreadBackgroundPriorityForIndexing,
      CXGlobalOpt_ThreadBackgroundPriorityForEditing,
      CXGlobalOpt_ThreadBackgroundPriorityForAll);
   pragma Convention (C, CXGlobalOptFlags);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:252

   procedure clang_CXIndex_setGlobalOptions (arg1 : CXIndex; options : unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:267
   pragma Import (C, clang_CXIndex_setGlobalOptions, "clang_CXIndex_setGlobalOptions");

   function clang_CXIndex_getGlobalOptions (arg1 : CXIndex) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:275
   pragma Import (C, clang_CXIndex_getGlobalOptions, "clang_CXIndex_getGlobalOptions");

   type CXFile is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:286

   function clang_getFileName (SFile : CXFile) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:292
   pragma Import (C, clang_getFileName, "clang_getFileName");

--  \1
--  \1

   type CXFileUniqueID_data_array is array (0 .. 2) of aliased Extensions.unsigned_long_long;
   type CXFileUniqueID is record
      data : aliased CXFileUniqueID_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:304
   end record;
   pragma Convention (C_Pass_By_Copy, CXFileUniqueID);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:305

   --  skipped anonymous struct anon_3

   function clang_getFileUniqueID (file : CXFile; outID : access CXFileUniqueID) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:315
   pragma Import (C, clang_getFileUniqueID, "clang_getFileUniqueID");

   function clang_isFileMultipleIncludeGuarded (tu : CXTranslationUnit; file : CXFile) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:323
   pragma Import (C, clang_isFileMultipleIncludeGuarded, "clang_isFileMultipleIncludeGuarded");

   function clang_getFile (tu : CXTranslationUnit; file_name : Interfaces.C.Strings.chars_ptr) return CXFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:335
   pragma Import (C, clang_getFile, "clang_getFile");

   function clang_File_isEqual (file1 : CXFile; file2 : CXFile) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:342
   pragma Import (C, clang_File_isEqual, "clang_File_isEqual");

   type CXSourceLocation_ptr_data_array is array (0 .. 1) of System.Address;
   type CXSourceLocation is record
      ptr_data : aliased CXSourceLocation_ptr_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:369
      int_data : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:370
   end record;
   pragma Convention (C_Pass_By_Copy, CXSourceLocation);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:371

   --  skipped anonymous struct anon_4

   type CXSourceRange_ptr_data_array is array (0 .. 1) of System.Address;
   type CXSourceRange is record
      ptr_data : aliased CXSourceRange_ptr_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:380
      begin_int_data : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:381
      end_int_data : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:382
   end record;
   pragma Convention (C_Pass_By_Copy, CXSourceRange);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:383

   --  skipped anonymous struct anon_5

   function clang_getNullLocation return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:388
   pragma Import (C, clang_getNullLocation, "clang_getNullLocation");

   function clang_equalLocations (loc1 : CXSourceLocation; loc2 : CXSourceLocation) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:398
   pragma Import (C, clang_equalLocations, "clang_equalLocations");

   function clang_getLocation
     (tu : CXTranslationUnit;
      file : CXFile;
      line : unsigned;
      column : unsigned) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:405
   pragma Import (C, clang_getLocation, "clang_getLocation");

   function clang_getLocationForOffset
     (tu : CXTranslationUnit;
      file : CXFile;
      offset : unsigned) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:413
   pragma Import (C, clang_getLocationForOffset, "clang_getLocationForOffset");

   function clang_Location_isInSystemHeader (location : CXSourceLocation) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:420
   pragma Import (C, clang_Location_isInSystemHeader, "clang_Location_isInSystemHeader");

   function clang_Location_isFromMainFile (location : CXSourceLocation) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:426
   pragma Import (C, clang_Location_isFromMainFile, "clang_Location_isFromMainFile");

   function clang_getNullRange return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:431
   pragma Import (C, clang_getNullRange, "clang_getNullRange");

   function clang_getRange (c_begin : CXSourceLocation; c_end : CXSourceLocation) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:437
   pragma Import (C, clang_getRange, "clang_getRange");

   function clang_equalRanges (range1 : CXSourceRange; range2 : CXSourceRange) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:445
   pragma Import (C, clang_equalRanges, "clang_equalRanges");

   function clang_Range_isNull (c_range : CXSourceRange) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:451
   pragma Import (C, clang_Range_isNull, "clang_Range_isNull");

   procedure clang_getExpansionLocation
     (location : CXSourceLocation;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:475
   pragma Import (C, clang_getExpansionLocation, "clang_getExpansionLocation");

   procedure clang_getPresumedLocation
     (location : CXSourceLocation;
      filename : access clang_c_CXString_h.CXString;
      line : access unsigned;
      column : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:521
   pragma Import (C, clang_getPresumedLocation, "clang_getPresumedLocation");

   procedure clang_getInstantiationLocation
     (location : CXSourceLocation;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:534
   pragma Import (C, clang_getInstantiationLocation, "clang_getInstantiationLocation");

   procedure clang_getSpellingLocation
     (location : CXSourceLocation;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:562
   pragma Import (C, clang_getSpellingLocation, "clang_getSpellingLocation");

   procedure clang_getFileLocation
     (location : CXSourceLocation;
      file : access CXFile;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:591
   pragma Import (C, clang_getFileLocation, "clang_getFileLocation");

   function clang_getRangeStart (c_range : CXSourceRange) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:601
   pragma Import (C, clang_getRangeStart, "clang_getRangeStart");

   function clang_getRangeEnd (c_range : CXSourceRange) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:607
   pragma Import (C, clang_getRangeEnd, "clang_getRangeEnd");

   type CXSourceRangeList is record
      count : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:614
      ranges : access CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:618
   end record;
   pragma Convention (C_Pass_By_Copy, CXSourceRangeList);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:619

   --  skipped anonymous struct anon_6

   function clang_getSkippedRanges (tu : CXTranslationUnit; file : CXFile) return access CXSourceRangeList;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:627
   pragma Import (C, clang_getSkippedRanges, "clang_getSkippedRanges");

   procedure clang_disposeSourceRangeList (ranges : access CXSourceRangeList);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:633
   pragma Import (C, clang_disposeSourceRangeList, "clang_disposeSourceRangeList");

   type CXDiagnosticSeverity is 
     (CXDiagnostic_Ignored,
      CXDiagnostic_Note,
      CXDiagnostic_Warning,
      CXDiagnostic_Error,
      CXDiagnostic_Fatal);
   pragma Convention (C, CXDiagnosticSeverity);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:648

   type CXDiagnostic is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:684

   type CXDiagnosticSet is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:689

   function clang_getNumDiagnosticsInSet (Diags : CXDiagnosticSet) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:694
   pragma Import (C, clang_getNumDiagnosticsInSet, "clang_getNumDiagnosticsInSet");

   function clang_getDiagnosticInSet (Diags : CXDiagnosticSet; Index : unsigned) return CXDiagnostic;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:705
   pragma Import (C, clang_getDiagnosticInSet, "clang_getDiagnosticInSet");

   type CXLoadDiag_Error is 
     (CXLoadDiag_None,
      CXLoadDiag_Unknown,
      CXLoadDiag_CannotLoad,
      CXLoadDiag_InvalidFile);
   pragma Convention (C, CXLoadDiag_Error);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:713

   function clang_loadDiagnostics
     (file : Interfaces.C.Strings.chars_ptr;
      error : access CXLoadDiag_Error;
      errorString : access clang_c_CXString_h.CXString) return CXDiagnosticSet;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:751
   pragma Import (C, clang_loadDiagnostics, "clang_loadDiagnostics");

   procedure clang_disposeDiagnosticSet (Diags : CXDiagnosticSet);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:758
   pragma Import (C, clang_disposeDiagnosticSet, "clang_disposeDiagnosticSet");

   function clang_getChildDiagnostics (D : CXDiagnostic) return CXDiagnosticSet;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:766
   pragma Import (C, clang_getChildDiagnostics, "clang_getChildDiagnostics");

   function clang_getNumDiagnostics (Unit : CXTranslationUnit) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:772
   pragma Import (C, clang_getNumDiagnostics, "clang_getNumDiagnostics");

   function clang_getDiagnostic (Unit : CXTranslationUnit; Index : unsigned) return CXDiagnostic;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:783
   pragma Import (C, clang_getDiagnostic, "clang_getDiagnostic");

   function clang_getDiagnosticSetFromTU (Unit : CXTranslationUnit) return CXDiagnosticSet;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:793
   pragma Import (C, clang_getDiagnosticSetFromTU, "clang_getDiagnosticSetFromTU");

   procedure clang_disposeDiagnostic (Diagnostic : CXDiagnostic);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:798
   pragma Import (C, clang_disposeDiagnostic, "clang_disposeDiagnostic");

   subtype CXDiagnosticDisplayOptions is unsigned;
   CXDiagnostic_DisplaySourceLocation : constant CXDiagnosticDisplayOptions := 1;
   CXDiagnostic_DisplayColumn : constant CXDiagnosticDisplayOptions := 2;
   CXDiagnostic_DisplaySourceRanges : constant CXDiagnosticDisplayOptions := 4;
   CXDiagnostic_DisplayOption : constant CXDiagnosticDisplayOptions := 8;
   CXDiagnostic_DisplayCategoryId : constant CXDiagnosticDisplayOptions := 16;
   CXDiagnostic_DisplayCategoryName : constant CXDiagnosticDisplayOptions := 32;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:806

   function clang_formatDiagnostic (Diagnostic : CXDiagnostic; Options : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:883
   pragma Import (C, clang_formatDiagnostic, "clang_formatDiagnostic");

   function clang_defaultDiagnosticDisplayOptions return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:893
   pragma Import (C, clang_defaultDiagnosticDisplayOptions, "clang_defaultDiagnosticDisplayOptions");

   function clang_getDiagnosticSeverity (arg1 : CXDiagnostic) return CXDiagnosticSeverity;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:899
   pragma Import (C, clang_getDiagnosticSeverity, "clang_getDiagnosticSeverity");

   function clang_getDiagnosticLocation (arg1 : CXDiagnostic) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:907
   pragma Import (C, clang_getDiagnosticLocation, "clang_getDiagnosticLocation");

   function clang_getDiagnosticSpelling (arg1 : CXDiagnostic) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:912
   pragma Import (C, clang_getDiagnosticSpelling, "clang_getDiagnosticSpelling");

   function clang_getDiagnosticOption (Diag : CXDiagnostic; Disable : access clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:926
   pragma Import (C, clang_getDiagnosticOption, "clang_getDiagnosticOption");

   function clang_getDiagnosticCategory (arg1 : CXDiagnostic) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:939
   pragma Import (C, clang_getDiagnosticCategory, "clang_getDiagnosticCategory");

   function clang_getDiagnosticCategoryName (Category : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:952
   pragma Import (C, clang_getDiagnosticCategoryName, "clang_getDiagnosticCategoryName");

   function clang_getDiagnosticCategoryText (arg1 : CXDiagnostic) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:959
   pragma Import (C, clang_getDiagnosticCategoryText, "clang_getDiagnosticCategoryText");

   function clang_getDiagnosticNumRanges (arg1 : CXDiagnostic) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:965
   pragma Import (C, clang_getDiagnosticNumRanges, "clang_getDiagnosticNumRanges");

   function clang_getDiagnosticRange (Diagnostic : CXDiagnostic; c_Range : unsigned) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:980
   pragma Import (C, clang_getDiagnosticRange, "clang_getDiagnosticRange");

   function clang_getDiagnosticNumFixIts (Diagnostic : CXDiagnostic) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:987
   pragma Import (C, clang_getDiagnosticNumFixIts, "clang_getDiagnosticNumFixIts");

   function clang_getDiagnosticFixIt
     (Diagnostic : CXDiagnostic;
      FixIt : unsigned;
      ReplacementRange : access CXSourceRange) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1014
   pragma Import (C, clang_getDiagnosticFixIt, "clang_getDiagnosticFixIt");

   function clang_getTranslationUnitSpelling (CTUnit : CXTranslationUnit) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1036
   pragma Import (C, clang_getTranslationUnitSpelling, "clang_getTranslationUnitSpelling");

   function clang_createTranslationUnitFromSourceFile
     (CIdx : CXIndex;
      source_filename : Interfaces.C.Strings.chars_ptr;
      num_clang_command_line_args : int;
      clang_command_line_args : System.Address;
      num_unsaved_files : unsigned;
      unsaved_files : access CXUnsavedFile) return CXTranslationUnit;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1078
   pragma Import (C, clang_createTranslationUnitFromSourceFile, "clang_createTranslationUnitFromSourceFile");

   function clang_createTranslationUnit (CIdx : CXIndex; ast_filename : Interfaces.C.Strings.chars_ptr) return CXTranslationUnit;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1092
   pragma Import (C, clang_createTranslationUnit, "clang_createTranslationUnit");

   function clang_createTranslationUnit2
     (CIdx : CXIndex;
      ast_filename : Interfaces.C.Strings.chars_ptr;
      out_TU : System.Address) return clang_c_CXErrorCode_h.CXErrorCode;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1104
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
   CXTranslationUnit_IncludeBriefCommentsInCodeCompletion : constant CXTranslationUnit_Flags := 128;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1116

   function clang_defaultEditingTranslationUnitOptions return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1220
   pragma Import (C, clang_defaultEditingTranslationUnitOptions, "clang_defaultEditingTranslationUnitOptions");

   function clang_parseTranslationUnit
     (CIdx : CXIndex;
      source_filename : Interfaces.C.Strings.chars_ptr;
      command_line_args : System.Address;
      num_command_line_args : int;
      unsaved_files : access CXUnsavedFile;
      num_unsaved_files : unsigned;
      options : unsigned) return CXTranslationUnit;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1229
   pragma Import (C, clang_parseTranslationUnit, "clang_parseTranslationUnit");

   function clang_parseTranslationUnit2
     (CIdx : CXIndex;
      source_filename : Interfaces.C.Strings.chars_ptr;
      command_line_args : System.Address;
      num_command_line_args : int;
      unsaved_files : access CXUnsavedFile;
      num_unsaved_files : unsigned;
      options : unsigned;
      out_TU : System.Address) return clang_c_CXErrorCode_h.CXErrorCode;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1282
   pragma Import (C, clang_parseTranslationUnit2, "clang_parseTranslationUnit2");

   type CXSaveTranslationUnit_Flags is 
     (CXSaveTranslationUnit_None);
   pragma Convention (C, CXSaveTranslationUnit_Flags);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1298

   function clang_defaultSaveOptions (TU : CXTranslationUnit) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1314
   pragma Import (C, clang_defaultSaveOptions, "clang_defaultSaveOptions");

   type CXSaveError is 
     (CXSaveError_None,
      CXSaveError_Unknown,
      CXSaveError_TranslationErrors,
      CXSaveError_InvalidTU);
   pragma Convention (C, CXSaveError);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1320

   function clang_saveTranslationUnit
     (TU : CXTranslationUnit;
      FileName : Interfaces.C.Strings.chars_ptr;
      options : unsigned) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1374
   pragma Import (C, clang_saveTranslationUnit, "clang_saveTranslationUnit");

   procedure clang_disposeTranslationUnit (arg1 : CXTranslationUnit);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1381
   pragma Import (C, clang_disposeTranslationUnit, "clang_disposeTranslationUnit");

   type CXReparse_Flags is 
     (CXReparse_None);
   pragma Convention (C, CXReparse_Flags);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1390

   function clang_defaultReparseOptions (TU : CXTranslationUnit) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1407
   pragma Import (C, clang_defaultReparseOptions, "clang_defaultReparseOptions");

   function clang_reparseTranslationUnit
     (TU : CXTranslationUnit;
      num_unsaved_files : unsigned;
      unsaved_files : access CXUnsavedFile;
      options : unsigned) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1448
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
   CXTUResourceUsage_Last : constant CXTUResourceUsageKind := 14;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1456

   function clang_getTUResourceUsageName (kind : CXTUResourceUsageKind) return Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1484
   pragma Import (C, clang_getTUResourceUsageName, "clang_getTUResourceUsageName");

   type CXTUResourceUsageEntry is record
      kind : aliased CXTUResourceUsageKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1488
      amount : aliased unsigned_long;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1491
   end record;
   pragma Convention (C_Pass_By_Copy, CXTUResourceUsageEntry);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1486

   type CXTUResourceUsage is record
      data : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1499
      numEntries : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1502
      entries : access CXTUResourceUsageEntry;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1506
   end record;
   pragma Convention (C_Pass_By_Copy, CXTUResourceUsage);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1497

   function clang_getCXTUResourceUsage (TU : CXTranslationUnit) return CXTUResourceUsage;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1514
   pragma Import (C, clang_getCXTUResourceUsage, "clang_getCXTUResourceUsage");

   procedure clang_disposeCXTUResourceUsage (usage : CXTUResourceUsage);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1516
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
   CXCursor_OMPOrderedDirective : constant CXCursorKind := 248;
   CXCursor_OMPAtomicDirective : constant CXCursorKind := 249;
   CXCursor_OMPForSimdDirective : constant CXCursorKind := 250;
   CXCursor_OMPParallelForSimdDirective : constant CXCursorKind := 251;
   CXCursor_OMPTargetDirective : constant CXCursorKind := 252;
   CXCursor_OMPTeamsDirective : constant CXCursorKind := 253;
   CXCursor_OMPTaskgroupDirective : constant CXCursorKind := 254;
   CXCursor_OMPCancellationPointDirective : constant CXCursorKind := 255;
   CXCursor_OMPCancelDirective : constant CXCursorKind := 256;
   CXCursor_LastStmt : constant CXCursorKind := 256;
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
   CXCursor_CUDASharedAttr : constant CXCursorKind := 416;
   CXCursor_LastAttr : constant CXCursorKind := 416;
   CXCursor_PreprocessingDirective : constant CXCursorKind := 500;
   CXCursor_MacroDefinition : constant CXCursorKind := 501;
   CXCursor_MacroExpansion : constant CXCursorKind := 502;
   CXCursor_MacroInstantiation : constant CXCursorKind := 502;
   CXCursor_InclusionDirective : constant CXCursorKind := 503;
   CXCursor_FirstPreprocessing : constant CXCursorKind := 500;
   CXCursor_LastPreprocessing : constant CXCursorKind := 503;
   CXCursor_ModuleImportDecl : constant CXCursorKind := 600;
   CXCursor_FirstExtraDecl : constant CXCursorKind := 600;
   CXCursor_LastExtraDecl : constant CXCursorKind := 600;
   CXCursor_OverloadCandidate : constant CXCursorKind := 700;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:1525

   CXCursor_MIN_VALUE : constant CXCursorKind := 1;
   CXCursor_MAX_VALUE : constant CXCursorKind := 700;
   
   type CXCursor_data_array is array (0 .. 2) of System.Address;
   type CXCursor is record
      kind : aliased CXCursorKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2318
      xdata : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2319
      data : aliased CXCursor_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2320
   end record;
   pragma Convention (C_Pass_By_Copy, CXCursor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2321

   --  skipped anonymous struct anon_7

   function clang_getNullCursor return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2332
   pragma Import (C, clang_getNullCursor, "clang_getNullCursor");

   function clang_getTranslationUnitCursor (arg1 : CXTranslationUnit) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2340
   pragma Import (C, clang_getTranslationUnitCursor, "clang_getTranslationUnitCursor");

   function clang_equalCursors (arg1 : CXCursor; arg2 : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2345
   pragma Import (C, clang_equalCursors, "clang_equalCursors");

   function clang_Cursor_isNull (cursor : CXCursor) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2350
   pragma Import (C, clang_Cursor_isNull, "clang_Cursor_isNull");

   function clang_hashCursor (arg1 : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2355
   pragma Import (C, clang_hashCursor, "clang_hashCursor");

   function clang_getCursorKind (arg1 : CXCursor) return CXCursorKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2360
   pragma Import (C, clang_getCursorKind, "clang_getCursorKind");

   function clang_isDeclaration (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2365
   pragma Import (C, clang_isDeclaration, "clang_isDeclaration");

   function clang_isReference (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2375
   pragma Import (C, clang_isReference, "clang_isReference");

   function clang_isExpression (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2380
   pragma Import (C, clang_isExpression, "clang_isExpression");

   function clang_isStatement (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2385
   pragma Import (C, clang_isStatement, "clang_isStatement");

   function clang_isAttribute (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2390
   pragma Import (C, clang_isAttribute, "clang_isAttribute");

   function clang_isInvalid (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2396
   pragma Import (C, clang_isInvalid, "clang_isInvalid");

   function clang_isTranslationUnit (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2402
   pragma Import (C, clang_isTranslationUnit, "clang_isTranslationUnit");

   function clang_isPreprocessing (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2408
   pragma Import (C, clang_isPreprocessing, "clang_isPreprocessing");

   function clang_isUnexposed (arg1 : CXCursorKind) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2414
   pragma Import (C, clang_isUnexposed, "clang_isUnexposed");

   type CXLinkageKind is 
     (CXLinkage_Invalid,
      CXLinkage_NoLinkage,
      CXLinkage_Internal,
      CXLinkage_UniqueExternal,
      CXLinkage_External);
   pragma Convention (C, CXLinkageKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2419

   function clang_getCursorLinkage (cursor : CXCursor) return CXLinkageKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2440
   pragma Import (C, clang_getCursorLinkage, "clang_getCursorLinkage");

   function clang_getCursorAvailability (cursor : CXCursor) return CXAvailabilityKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2451
   pragma Import (C, clang_getCursorAvailability, "clang_getCursorAvailability");

   type CXPlatformAvailability is record
      Platform : aliased clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2464
      Introduced : aliased CXVersion;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2468
      Deprecated : aliased CXVersion;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2473
      Obsoleted : aliased CXVersion;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2478
      Unavailable : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2482
      Message : aliased clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2487
   end record;
   pragma Convention (C_Pass_By_Copy, CXPlatformAvailability);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2457

   function clang_getCursorPlatformAvailability
     (cursor : CXCursor;
      always_deprecated : access int;
      deprecated_message : access clang_c_CXString_h.CXString;
      always_unavailable : access int;
      unavailable_message : access clang_c_CXString_h.CXString;
      availability : access CXPlatformAvailability;
      availability_size : int) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2527
   pragma Import (C, clang_getCursorPlatformAvailability, "clang_getCursorPlatformAvailability");

   procedure clang_disposeCXPlatformAvailability (availability : access CXPlatformAvailability);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2539
   pragma Import (C, clang_disposeCXPlatformAvailability, "clang_disposeCXPlatformAvailability");

   type CXLanguageKind is 
     (CXLanguage_Invalid,
      CXLanguage_C,
      CXLanguage_ObjC,
      CXLanguage_CPlusPlus);
   pragma Convention (C, CXLanguageKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2544

   function clang_getCursorLanguage (cursor : CXCursor) return CXLanguageKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2554
   pragma Import (C, clang_getCursorLanguage, "clang_getCursorLanguage");

   function clang_Cursor_getTranslationUnit (arg1 : CXCursor) return CXTranslationUnit;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2559
   pragma Import (C, clang_Cursor_getTranslationUnit, "clang_Cursor_getTranslationUnit");

   --  skipped empty struct CXCursorSetImpl

   type CXCursorSet is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2565

   function clang_createCXCursorSet return CXCursorSet;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2570
   pragma Import (C, clang_createCXCursorSet, "clang_createCXCursorSet");

   procedure clang_disposeCXCursorSet (cset : CXCursorSet);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2575
   pragma Import (C, clang_disposeCXCursorSet, "clang_disposeCXCursorSet");

   function clang_CXCursorSet_contains (cset : CXCursorSet; cursor : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2582
   pragma Import (C, clang_CXCursorSet_contains, "clang_CXCursorSet_contains");

   function clang_CXCursorSet_insert (cset : CXCursorSet; cursor : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2590
   pragma Import (C, clang_CXCursorSet_insert, "clang_CXCursorSet_insert");

   function clang_getCursorSemanticParent (cursor : CXCursor) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2626
   pragma Import (C, clang_getCursorSemanticParent, "clang_getCursorSemanticParent");

   function clang_getCursorLexicalParent (cursor : CXCursor) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2662
   pragma Import (C, clang_getCursorLexicalParent, "clang_getCursorLexicalParent");

   procedure clang_getOverriddenCursors
     (cursor : CXCursor;
      overridden : System.Address;
      num_overridden : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2707
   pragma Import (C, clang_getOverriddenCursors, "clang_getOverriddenCursors");

   procedure clang_disposeOverriddenCursors (overridden : access CXCursor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2715
   pragma Import (C, clang_disposeOverriddenCursors, "clang_disposeOverriddenCursors");

   function clang_getIncludedFile (cursor : CXCursor) return CXFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2721
   pragma Import (C, clang_getIncludedFile, "clang_getIncludedFile");

   function clang_getCursor (arg1 : CXTranslationUnit; arg2 : CXSourceLocation) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2753
   pragma Import (C, clang_getCursor, "clang_getCursor");

   function clang_getCursorLocation (arg1 : CXCursor) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2765
   pragma Import (C, clang_getCursorLocation, "clang_getCursorLocation");

   function clang_getCursorExtent (arg1 : CXCursor) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2778
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
   CXType_MemberPointer : constant CXTypeKind := 117;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2793

   subtype CXCallingConv is unsigned;
   CXCallingConv_Default : constant CXCallingConv := 0;
   CXCallingConv_C : constant CXCallingConv := 1;
   CXCallingConv_X86StdCall : constant CXCallingConv := 2;
   CXCallingConv_X86FastCall : constant CXCallingConv := 3;
   CXCallingConv_X86ThisCall : constant CXCallingConv := 4;
   CXCallingConv_X86Pascal : constant CXCallingConv := 5;
   CXCallingConv_AAPCS : constant CXCallingConv := 6;
   CXCallingConv_AAPCS_VFP : constant CXCallingConv := 7;
   CXCallingConv_IntelOclBicc : constant CXCallingConv := 9;
   CXCallingConv_X86_64Win64 : constant CXCallingConv := 10;
   CXCallingConv_X86_64SysV : constant CXCallingConv := 11;
   CXCallingConv_X86VectorCall : constant CXCallingConv := 12;
   CXCallingConv_Invalid : constant CXCallingConv := 100;
   CXCallingConv_Unexposed : constant CXCallingConv := 200;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2860

   type CXType_data_array is array (0 .. 1) of System.Address;
   type CXType is record
      kind : aliased CXTypeKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2885
      data : aliased CXType_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2886
   end record;
   pragma Convention (C_Pass_By_Copy, CXType);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2887

   --  skipped anonymous struct anon_8

   function clang_getCursorType (C : CXCursor) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2892
   pragma Import (C, clang_getCursorType, "clang_getCursorType");

   function clang_getTypeSpelling (CT : CXType) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2900
   pragma Import (C, clang_getTypeSpelling, "clang_getTypeSpelling");

   function clang_getTypedefDeclUnderlyingType (C : CXCursor) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2908
   pragma Import (C, clang_getTypedefDeclUnderlyingType, "clang_getTypedefDeclUnderlyingType");

   function clang_getEnumDeclIntegerType (C : CXCursor) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2916
   pragma Import (C, clang_getEnumDeclIntegerType, "clang_getEnumDeclIntegerType");

   function clang_getEnumConstantDeclValue (C : CXCursor) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2926
   pragma Import (C, clang_getEnumConstantDeclValue, "clang_getEnumConstantDeclValue");

   function clang_getEnumConstantDeclUnsignedValue (C : CXCursor) return Extensions.unsigned_long_long;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2936
   pragma Import (C, clang_getEnumConstantDeclUnsignedValue, "clang_getEnumConstantDeclUnsignedValue");

   function clang_getFieldDeclBitWidth (C : CXCursor) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2943
   pragma Import (C, clang_getFieldDeclBitWidth, "clang_getFieldDeclBitWidth");

   function clang_Cursor_getNumArguments (C : CXCursor) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2952
   pragma Import (C, clang_Cursor_getNumArguments, "clang_Cursor_getNumArguments");

   function clang_Cursor_getArgument (C : CXCursor; i : unsigned) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2961
   pragma Import (C, clang_Cursor_getArgument, "clang_Cursor_getArgument");

   type CXTemplateArgumentKind is 
     (CXTemplateArgumentKind_Null,
      CXTemplateArgumentKind_Type,
      CXTemplateArgumentKind_Declaration,
      CXTemplateArgumentKind_NullPtr,
      CXTemplateArgumentKind_Integral,
      CXTemplateArgumentKind_Template,
      CXTemplateArgumentKind_TemplateExpansion,
      CXTemplateArgumentKind_Expression,
      CXTemplateArgumentKind_Pack,
      CXTemplateArgumentKind_Invalid);
   pragma Convention (C, CXTemplateArgumentKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2969

   function clang_Cursor_getNumTemplateArguments (C : CXCursor) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:2999
   pragma Import (C, clang_Cursor_getNumTemplateArguments, "clang_Cursor_getNumTemplateArguments");

   function clang_Cursor_getTemplateArgumentKind (C : CXCursor; I : unsigned) return CXTemplateArgumentKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3017
   pragma Import (C, clang_Cursor_getTemplateArgumentKind, "clang_Cursor_getTemplateArgumentKind");

   function clang_Cursor_getTemplateArgumentType (C : CXCursor; I : unsigned) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3038
   pragma Import (C, clang_Cursor_getTemplateArgumentType, "clang_Cursor_getTemplateArgumentType");

   function clang_Cursor_getTemplateArgumentValue (C : CXCursor; I : unsigned) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3058
   pragma Import (C, clang_Cursor_getTemplateArgumentValue, "clang_Cursor_getTemplateArgumentValue");

   function clang_Cursor_getTemplateArgumentUnsignedValue (C : CXCursor; I : unsigned) return Extensions.unsigned_long_long;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3078
   pragma Import (C, clang_Cursor_getTemplateArgumentUnsignedValue, "clang_Cursor_getTemplateArgumentUnsignedValue");

   function clang_equalTypes (A : CXType; B : CXType) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3087
   pragma Import (C, clang_equalTypes, "clang_equalTypes");

   function clang_getCanonicalType (T : CXType) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3097
   pragma Import (C, clang_getCanonicalType, "clang_getCanonicalType");

   function clang_isConstQualifiedType (T : CXType) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3104
   pragma Import (C, clang_isConstQualifiedType, "clang_isConstQualifiedType");

   function clang_isVolatileQualifiedType (T : CXType) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3111
   pragma Import (C, clang_isVolatileQualifiedType, "clang_isVolatileQualifiedType");

   function clang_isRestrictQualifiedType (T : CXType) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3118
   pragma Import (C, clang_isRestrictQualifiedType, "clang_isRestrictQualifiedType");

   function clang_getPointeeType (T : CXType) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3123
   pragma Import (C, clang_getPointeeType, "clang_getPointeeType");

   function clang_getTypeDeclaration (T : CXType) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3128
   pragma Import (C, clang_getTypeDeclaration, "clang_getTypeDeclaration");

   function clang_getDeclObjCTypeEncoding (C : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3133
   pragma Import (C, clang_getDeclObjCTypeEncoding, "clang_getDeclObjCTypeEncoding");

   function clang_getTypeKindSpelling (K : CXTypeKind) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3138
   pragma Import (C, clang_getTypeKindSpelling, "clang_getTypeKindSpelling");

   function clang_getFunctionTypeCallingConv (T : CXType) return CXCallingConv;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3145
   pragma Import (C, clang_getFunctionTypeCallingConv, "clang_getFunctionTypeCallingConv");

   function clang_getResultType (T : CXType) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3152
   pragma Import (C, clang_getResultType, "clang_getResultType");

   function clang_getNumArgTypes (T : CXType) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3160
   pragma Import (C, clang_getNumArgTypes, "clang_getNumArgTypes");

   function clang_getArgType (T : CXType; i : unsigned) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3168
   pragma Import (C, clang_getArgType, "clang_getArgType");

   function clang_isFunctionTypeVariadic (T : CXType) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3173
   pragma Import (C, clang_isFunctionTypeVariadic, "clang_isFunctionTypeVariadic");

   function clang_getCursorResultType (C : CXCursor) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3180
   pragma Import (C, clang_getCursorResultType, "clang_getCursorResultType");

   function clang_isPODType (T : CXType) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3186
   pragma Import (C, clang_isPODType, "clang_isPODType");

   function clang_getElementType (T : CXType) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3194
   pragma Import (C, clang_getElementType, "clang_getElementType");

   function clang_getNumElements (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3202
   pragma Import (C, clang_getNumElements, "clang_getNumElements");

   function clang_getArrayElementType (T : CXType) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3209
   pragma Import (C, clang_getArrayElementType, "clang_getArrayElementType");

   function clang_getArraySize (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3216
   pragma Import (C, clang_getArraySize, "clang_getArraySize");

   subtype CXTypeLayoutError is unsigned;
   CXTypeLayoutError_Invalid : constant CXTypeLayoutError := -1;
   CXTypeLayoutError_Incomplete : constant CXTypeLayoutError := -2;
   CXTypeLayoutError_Dependent : constant CXTypeLayoutError := -3;
   CXTypeLayoutError_NotConstantSize : constant CXTypeLayoutError := -4;
   CXTypeLayoutError_InvalidFieldName : constant CXTypeLayoutError := -5;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3226

   function clang_Type_getAlignOf (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3261
   pragma Import (C, clang_Type_getAlignOf, "clang_Type_getAlignOf");

   function clang_Type_getClassType (T : CXType) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3268
   pragma Import (C, clang_Type_getClassType, "clang_Type_getClassType");

   function clang_Type_getSizeOf (T : CXType) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3279
   pragma Import (C, clang_Type_getSizeOf, "clang_Type_getSizeOf");

   function clang_Type_getOffsetOf (T : CXType; S : Interfaces.C.Strings.chars_ptr) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3294
   pragma Import (C, clang_Type_getOffsetOf, "clang_Type_getOffsetOf");

   function clang_Cursor_getOffsetOfField (C : CXCursor) return Long_Long_Integer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3309
   pragma Import (C, clang_Cursor_getOffsetOfField, "clang_Cursor_getOffsetOfField");

   function clang_Cursor_isAnonymous (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3315
   pragma Import (C, clang_Cursor_isAnonymous, "clang_Cursor_isAnonymous");

   type CXRefQualifierKind is 
     (CXRefQualifier_None,
      CXRefQualifier_LValue,
      CXRefQualifier_RValue);
   pragma Convention (C, CXRefQualifierKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3318

   function clang_Type_getNumTemplateArguments (T : CXType) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3334
   pragma Import (C, clang_Type_getNumTemplateArguments, "clang_Type_getNumTemplateArguments");

   function clang_Type_getTemplateArgumentAsType (T : CXType; i : unsigned) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3343
   pragma Import (C, clang_Type_getTemplateArgumentAsType, "clang_Type_getTemplateArgumentAsType");

   function clang_Type_getCXXRefQualifier (T : CXType) return CXRefQualifierKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3351
   pragma Import (C, clang_Type_getCXXRefQualifier, "clang_Type_getCXXRefQualifier");

   function clang_Cursor_isBitField (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3357
   pragma Import (C, clang_Cursor_isBitField, "clang_Cursor_isBitField");

   function clang_isVirtualBase (arg1 : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3363
   pragma Import (C, clang_isVirtualBase, "clang_isVirtualBase");

   type CX_CXXAccessSpecifier is 
     (CX_CXXInvalidAccessSpecifier,
      CX_CXXPublic,
      CX_CXXProtected,
      CX_CXXPrivate);
   pragma Convention (C, CX_CXXAccessSpecifier);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3369

   function clang_getCXXAccessSpecifier (arg1 : CXCursor) return CX_CXXAccessSpecifier;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3383
   pragma Import (C, clang_getCXXAccessSpecifier, "clang_getCXXAccessSpecifier");

   type CX_StorageClass is 
     (CX_SC_Invalid,
      CX_SC_None,
      CX_SC_Extern,
      CX_SC_Static,
      CX_SC_PrivateExtern,
      CX_SC_OpenCLWorkGroupLocal,
      CX_SC_Auto,
      CX_SC_Register);
   pragma Convention (C, CX_StorageClass);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3389

   function clang_Cursor_getStorageClass (arg1 : CXCursor) return CX_StorageClass;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3406
   pragma Import (C, clang_Cursor_getStorageClass, "clang_Cursor_getStorageClass");

   function clang_getNumOverloadedDecls (cursor : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3417
   pragma Import (C, clang_getNumOverloadedDecls, "clang_getNumOverloadedDecls");

   function clang_getOverloadedDecl (cursor : CXCursor; index : unsigned) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3433
   pragma Import (C, clang_getOverloadedDecl, "clang_getOverloadedDecl");

   function clang_getIBOutletCollectionType (arg1 : CXCursor) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3452
   pragma Import (C, clang_getIBOutletCollectionType, "clang_getIBOutletCollectionType");

   type CXChildVisitResult is 
     (CXChildVisit_Break,
      CXChildVisit_Continue,
      CXChildVisit_Recurse);
   pragma Convention (C, CXChildVisitResult);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3474

   type CXCursorVisitor is access function
        (arg1 : CXCursor;
         arg2 : CXCursor;
         arg3 : CXClientData) return CXChildVisitResult;
   pragma Convention (C, CXCursorVisitor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3503

   function clang_visitChildren
     (parent : CXCursor;
      visitor : CXCursorVisitor;
      client_data : CXClientData) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3529
   pragma Import (C, clang_visitChildren, "clang_visitChildren");

   function clang_getCursorUSR (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3580
   pragma Import (C, clang_getCursorUSR, "clang_getCursorUSR");

   function clang_constructUSR_ObjCClass (class_name : Interfaces.C.Strings.chars_ptr) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3585
   pragma Import (C, clang_constructUSR_ObjCClass, "clang_constructUSR_ObjCClass");

   function clang_constructUSR_ObjCCategory (class_name : Interfaces.C.Strings.chars_ptr; category_name : Interfaces.C.Strings.chars_ptr) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3591
   pragma Import (C, clang_constructUSR_ObjCCategory, "clang_constructUSR_ObjCCategory");

   function clang_constructUSR_ObjCProtocol (protocol_name : Interfaces.C.Strings.chars_ptr) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3598
   pragma Import (C, clang_constructUSR_ObjCProtocol, "clang_constructUSR_ObjCProtocol");

   function clang_constructUSR_ObjCIvar (name : Interfaces.C.Strings.chars_ptr; classUSR : clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3605
   pragma Import (C, clang_constructUSR_ObjCIvar, "clang_constructUSR_ObjCIvar");

   function clang_constructUSR_ObjCMethod
     (name : Interfaces.C.Strings.chars_ptr;
      isInstanceMethod : unsigned;
      classUSR : clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3612
   pragma Import (C, clang_constructUSR_ObjCMethod, "clang_constructUSR_ObjCMethod");

   function clang_constructUSR_ObjCProperty (property : Interfaces.C.Strings.chars_ptr; classUSR : clang_c_CXString_h.CXString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3620
   pragma Import (C, clang_constructUSR_ObjCProperty, "clang_constructUSR_ObjCProperty");

   function clang_getCursorSpelling (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3626
   pragma Import (C, clang_getCursorSpelling, "clang_getCursorSpelling");

   function clang_Cursor_getSpellingNameRange
     (arg1 : CXCursor;
      pieceIndex : unsigned;
      options : unsigned) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3639
   pragma Import (C, clang_Cursor_getSpellingNameRange, "clang_Cursor_getSpellingNameRange");

   function clang_getCursorDisplayName (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3650
   pragma Import (C, clang_getCursorDisplayName, "clang_getCursorDisplayName");

   function clang_getCursorReferenced (arg1 : CXCursor) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3662
   pragma Import (C, clang_getCursorReferenced, "clang_getCursorReferenced");

   function clang_getCursorDefinition (arg1 : CXCursor) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3692
   pragma Import (C, clang_getCursorDefinition, "clang_getCursorDefinition");

   function clang_isCursorDefinition (arg1 : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3698
   pragma Import (C, clang_isCursorDefinition, "clang_isCursorDefinition");

   function clang_getCanonicalCursor (arg1 : CXCursor) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3724
   pragma Import (C, clang_getCanonicalCursor, "clang_getCanonicalCursor");

   function clang_Cursor_getObjCSelectorIndex (arg1 : CXCursor) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3738
   pragma Import (C, clang_Cursor_getObjCSelectorIndex, "clang_Cursor_getObjCSelectorIndex");

   function clang_Cursor_isDynamicCall (C : CXCursor) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3751
   pragma Import (C, clang_Cursor_isDynamicCall, "clang_Cursor_isDynamicCall");

   function clang_Cursor_getReceiverType (C : CXCursor) return CXType;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3757
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
   CXObjCPropertyAttr_unsafe_unretained : constant CXObjCPropertyAttrKind := 2048;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3776

   function clang_Cursor_getObjCPropertyAttributes (C : CXCursor; reserved : unsigned) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3785
   pragma Import (C, clang_Cursor_getObjCPropertyAttributes, "clang_Cursor_getObjCPropertyAttributes");

   subtype CXObjCDeclQualifierKind is unsigned;
   CXObjCDeclQualifier_None : constant CXObjCDeclQualifierKind := 0;
   CXObjCDeclQualifier_In : constant CXObjCDeclQualifierKind := 1;
   CXObjCDeclQualifier_Inout : constant CXObjCDeclQualifierKind := 2;
   CXObjCDeclQualifier_Out : constant CXObjCDeclQualifierKind := 4;
   CXObjCDeclQualifier_Bycopy : constant CXObjCDeclQualifierKind := 8;
   CXObjCDeclQualifier_Byref : constant CXObjCDeclQualifierKind := 16;
   CXObjCDeclQualifier_Oneway : constant CXObjCDeclQualifierKind := 32;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3800

   function clang_Cursor_getObjCDeclQualifiers (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3808
   pragma Import (C, clang_Cursor_getObjCDeclQualifiers, "clang_Cursor_getObjCDeclQualifiers");

   function clang_Cursor_isObjCOptional (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3815
   pragma Import (C, clang_Cursor_isObjCOptional, "clang_Cursor_isObjCOptional");

   function clang_Cursor_isVariadic (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3820
   pragma Import (C, clang_Cursor_isVariadic, "clang_Cursor_isVariadic");

   function clang_Cursor_getCommentRange (C : CXCursor) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3827
   pragma Import (C, clang_Cursor_getCommentRange, "clang_Cursor_getCommentRange");

   function clang_Cursor_getRawCommentText (C : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3833
   pragma Import (C, clang_Cursor_getRawCommentText, "clang_Cursor_getRawCommentText");

   function clang_Cursor_getBriefCommentText (C : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3840
   pragma Import (C, clang_Cursor_getBriefCommentText, "clang_Cursor_getBriefCommentText");

   function clang_Cursor_getMangling (arg1 : CXCursor) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3854
   pragma Import (C, clang_Cursor_getMangling, "clang_Cursor_getMangling");

   type CXModule is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3868

   function clang_Cursor_getModule (C : CXCursor) return CXModule;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3873
   pragma Import (C, clang_Cursor_getModule, "clang_Cursor_getModule");

   function clang_getModuleForFile (arg1 : CXTranslationUnit; arg2 : CXFile) return CXModule;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3879
   pragma Import (C, clang_getModuleForFile, "clang_getModuleForFile");

   function clang_Module_getASTFile (Module : CXModule) return CXFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3886
   pragma Import (C, clang_Module_getASTFile, "clang_Module_getASTFile");

   function clang_Module_getParent (Module : CXModule) return CXModule;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3894
   pragma Import (C, clang_Module_getParent, "clang_Module_getParent");

   function clang_Module_getName (Module : CXModule) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3902
   pragma Import (C, clang_Module_getName, "clang_Module_getName");

   function clang_Module_getFullName (Module : CXModule) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3909
   pragma Import (C, clang_Module_getFullName, "clang_Module_getFullName");

   function clang_Module_isSystem (Module : CXModule) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3916
   pragma Import (C, clang_Module_isSystem, "clang_Module_isSystem");

   function clang_Module_getNumTopLevelHeaders (arg1 : CXTranslationUnit; Module : CXModule) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3923
   pragma Import (C, clang_Module_getNumTopLevelHeaders, "clang_Module_getNumTopLevelHeaders");

   function clang_Module_getTopLevelHeader
     (arg1 : CXTranslationUnit;
      Module : CXModule;
      Index : unsigned) return CXFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3934
   pragma Import (C, clang_Module_getTopLevelHeader, "clang_Module_getTopLevelHeader");

   function clang_CXXMethod_isPureVirtual (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3954
   pragma Import (C, clang_CXXMethod_isPureVirtual, "clang_CXXMethod_isPureVirtual");

   function clang_CXXMethod_isStatic (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3960
   pragma Import (C, clang_CXXMethod_isStatic, "clang_CXXMethod_isStatic");

   function clang_CXXMethod_isVirtual (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3967
   pragma Import (C, clang_CXXMethod_isVirtual, "clang_CXXMethod_isVirtual");

   function clang_CXXMethod_isConst (C : CXCursor) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3973
   pragma Import (C, clang_CXXMethod_isConst, "clang_CXXMethod_isConst");

   function clang_getTemplateCursorKind (C : CXCursor) return CXCursorKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:3992
   pragma Import (C, clang_getTemplateCursorKind, "clang_getTemplateCursorKind");

   function clang_getSpecializedCursorTemplate (C : CXCursor) return CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4022
   pragma Import (C, clang_getSpecializedCursorTemplate, "clang_getSpecializedCursorTemplate");

   function clang_getCursorReferenceNameRange
     (C : CXCursor;
      NameFlags : unsigned;
      PieceIndex : unsigned) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4042
   pragma Import (C, clang_getCursorReferenceNameRange, "clang_getCursorReferenceNameRange");

   subtype CXNameRefFlags is unsigned;
   CXNameRange_WantQualifier : constant CXNameRefFlags := 1;
   CXNameRange_WantTemplateArgs : constant CXNameRefFlags := 2;
   CXNameRange_WantSinglePiece : constant CXNameRefFlags := 4;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4046

   type CXTokenKind is 
     (CXToken_Punctuation,
      CXToken_Keyword,
      CXToken_Identifier,
      CXToken_Literal,
      CXToken_Comment);
   pragma Convention (C, CXTokenKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4089

   type CXToken_int_data_array is array (0 .. 3) of aliased unsigned;
   type CXToken is record
      int_data : aliased CXToken_int_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4120
      ptr_data : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4121
   end record;
   pragma Convention (C_Pass_By_Copy, CXToken);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4122

   --  skipped anonymous struct anon_11

   function clang_getTokenKind (arg1 : CXToken) return CXTokenKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4127
   pragma Import (C, clang_getTokenKind, "clang_getTokenKind");

   function clang_getTokenSpelling (arg1 : CXTranslationUnit; arg2 : CXToken) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4135
   pragma Import (C, clang_getTokenSpelling, "clang_getTokenSpelling");

   function clang_getTokenLocation (arg1 : CXTranslationUnit; arg2 : CXToken) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4140
   pragma Import (C, clang_getTokenLocation, "clang_getTokenLocation");

   function clang_getTokenExtent (arg1 : CXTranslationUnit; arg2 : CXToken) return CXSourceRange;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4146
   pragma Import (C, clang_getTokenExtent, "clang_getTokenExtent");

   procedure clang_tokenize
     (TU : CXTranslationUnit;
      c_Range : CXSourceRange;
      Tokens : System.Address;
      NumTokens : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4165
   pragma Import (C, clang_tokenize, "clang_tokenize");

   procedure clang_annotateTokens
     (TU : CXTranslationUnit;
      Tokens : access CXToken;
      NumTokens : unsigned;
      Cursors : access CXCursor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4198
   pragma Import (C, clang_annotateTokens, "clang_annotateTokens");

   procedure clang_disposeTokens
     (TU : CXTranslationUnit;
      Tokens : access CXToken;
      NumTokens : unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4205
   pragma Import (C, clang_disposeTokens, "clang_disposeTokens");

   function clang_getCursorKindSpelling (Kind : CXCursorKind) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4222
   pragma Import (C, clang_getCursorKindSpelling, "clang_getCursorKindSpelling");

   procedure clang_getDefinitionSpellingAndExtent
     (arg1 : CXCursor;
      startBuf : System.Address;
      endBuf : System.Address;
      startLine : access unsigned;
      startColumn : access unsigned;
      endLine : access unsigned;
      endColumn : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4223
   pragma Import (C, clang_getDefinitionSpellingAndExtent, "clang_getDefinitionSpellingAndExtent");

   procedure clang_enableStackTraces;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4230
   pragma Import (C, clang_enableStackTraces, "clang_enableStackTraces");

   procedure clang_executeOnThread
     (fn : access procedure (arg1 : System.Address);
      user_data : System.Address;
      stack_size : unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4231
   pragma Import (C, clang_executeOnThread, "clang_executeOnThread");

   type CXCompletionString is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4263

   type CXCompletionResult is record
      CursorKind : aliased CXCursorKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4279
      CompletionString : CXCompletionString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4285
   end record;
   pragma Convention (C_Pass_By_Copy, CXCompletionResult);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4286

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
   pragma Convention (C, CXCompletionChunkKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4295

   function clang_getCompletionChunkKind (completion_string : CXCompletionString; chunk_number : unsigned) return CXCompletionChunkKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4469
   pragma Import (C, clang_getCompletionChunkKind, "clang_getCompletionChunkKind");

   function clang_getCompletionChunkText (completion_string : CXCompletionString; chunk_number : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4483
   pragma Import (C, clang_getCompletionChunkText, "clang_getCompletionChunkText");

   function clang_getCompletionChunkCompletionString (completion_string : CXCompletionString; chunk_number : unsigned) return CXCompletionString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4498
   pragma Import (C, clang_getCompletionChunkCompletionString, "clang_getCompletionChunkCompletionString");

   function clang_getNumCompletionChunks (completion_string : CXCompletionString) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4505
   pragma Import (C, clang_getNumCompletionChunks, "clang_getNumCompletionChunks");

   function clang_getCompletionPriority (completion_string : CXCompletionString) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4520
   pragma Import (C, clang_getCompletionPriority, "clang_getCompletionPriority");

   function clang_getCompletionAvailability (completion_string : CXCompletionString) return CXAvailabilityKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4531
   pragma Import (C, clang_getCompletionAvailability, "clang_getCompletionAvailability");

   function clang_getCompletionNumAnnotations (completion_string : CXCompletionString) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4543
   pragma Import (C, clang_getCompletionNumAnnotations, "clang_getCompletionNumAnnotations");

   function clang_getCompletionAnnotation (completion_string : CXCompletionString; annotation_number : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4557
   pragma Import (C, clang_getCompletionAnnotation, "clang_getCompletionAnnotation");

   function clang_getCompletionParent (completion_string : CXCompletionString; kind : access CXCursorKind) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4577
   pragma Import (C, clang_getCompletionParent, "clang_getCompletionParent");

   function clang_getCompletionBriefComment (completion_string : CXCompletionString) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4585
   pragma Import (C, clang_getCompletionBriefComment, "clang_getCompletionBriefComment");

   function clang_getCursorCompletionString (cursor : CXCursor) return CXCompletionString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4597
   pragma Import (C, clang_getCursorCompletionString, "clang_getCursorCompletionString");

   type CXCodeCompleteResults is record
      Results : access CXCompletionResult;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4610
      NumResults : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4616
   end record;
   pragma Convention (C_Pass_By_Copy, CXCodeCompleteResults);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4617

   --  skipped anonymous struct anon_13

   subtype CXCodeComplete_Flags is unsigned;
   CXCodeComplete_IncludeMacros : constant CXCodeComplete_Flags := 1;
   CXCodeComplete_IncludeCodePatterns : constant CXCodeComplete_Flags := 2;
   CXCodeComplete_IncludeBriefComments : constant CXCodeComplete_Flags := 4;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4626

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
   CXCompletionContext_Unknown : constant CXCompletionContext := 4194303;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4652

   function clang_defaultCodeCompleteOptions return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4781
   pragma Import (C, clang_defaultCodeCompleteOptions, "clang_defaultCodeCompleteOptions");

   function clang_codeCompleteAt
     (TU : CXTranslationUnit;
      complete_filename : Interfaces.C.Strings.chars_ptr;
      complete_line : unsigned;
      complete_column : unsigned;
      unsaved_files : access CXUnsavedFile;
      num_unsaved_files : unsigned;
      options : unsigned) return access CXCodeCompleteResults;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4852
   pragma Import (C, clang_codeCompleteAt, "clang_codeCompleteAt");

   procedure clang_sortCodeCompletionResults (Results : access CXCompletionResult; NumResults : unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4868
   pragma Import (C, clang_sortCodeCompletionResults, "clang_sortCodeCompletionResults");

   procedure clang_disposeCodeCompleteResults (Results : access CXCodeCompleteResults);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4875
   pragma Import (C, clang_disposeCodeCompleteResults, "clang_disposeCodeCompleteResults");

   function clang_codeCompleteGetNumDiagnostics (Results : access CXCodeCompleteResults) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4882
   pragma Import (C, clang_codeCompleteGetNumDiagnostics, "clang_codeCompleteGetNumDiagnostics");

   function clang_codeCompleteGetDiagnostic (Results : access CXCodeCompleteResults; Index : unsigned) return CXDiagnostic;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4894
   pragma Import (C, clang_codeCompleteGetDiagnostic, "clang_codeCompleteGetDiagnostic");

   function clang_codeCompleteGetContexts (Results : access CXCodeCompleteResults) return Extensions.unsigned_long_long;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4907
   pragma Import (C, clang_codeCompleteGetContexts, "clang_codeCompleteGetContexts");

   function clang_codeCompleteGetContainerKind (Results : access CXCodeCompleteResults; IsIncomplete : access unsigned) return CXCursorKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4927
   pragma Import (C, clang_codeCompleteGetContainerKind, "clang_codeCompleteGetContainerKind");

   function clang_codeCompleteGetContainerUSR (Results : access CXCodeCompleteResults) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4941
   pragma Import (C, clang_codeCompleteGetContainerUSR, "clang_codeCompleteGetContainerUSR");

   function clang_codeCompleteGetObjCSelector (Results : access CXCodeCompleteResults) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4956
   pragma Import (C, clang_codeCompleteGetObjCSelector, "clang_codeCompleteGetObjCSelector");

   function clang_getClangVersion return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4973
   pragma Import (C, clang_getClangVersion, "clang_getClangVersion");

   procedure clang_toggleCrashRecovery (isEnabled : unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4982
   pragma Import (C, clang_toggleCrashRecovery, "clang_toggleCrashRecovery");

   type CXInclusionVisitor is access procedure
        (arg1 : CXFile;
         arg2 : access CXSourceLocation;
         arg3 : unsigned;
         arg4 : CXClientData);
   pragma Convention (C, CXInclusionVisitor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:4995

   procedure clang_getInclusions
     (tu : CXTranslationUnit;
      visitor : CXInclusionVisitor;
      client_data : CXClientData);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5006
   pragma Import (C, clang_getInclusions, "clang_getInclusions");

   type CXRemapping is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5022

   function clang_getRemappings (path : Interfaces.C.Strings.chars_ptr) return CXRemapping;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5032
   pragma Import (C, clang_getRemappings, "clang_getRemappings");

   function clang_getRemappingsFromFileList (filePaths : System.Address; numFiles : unsigned) return CXRemapping;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5045
   pragma Import (C, clang_getRemappingsFromFileList, "clang_getRemappingsFromFileList");

   function clang_remap_getNumFiles (arg1 : CXRemapping) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5051
   pragma Import (C, clang_remap_getNumFiles, "clang_remap_getNumFiles");

   procedure clang_remap_getFilenames
     (arg1 : CXRemapping;
      index : unsigned;
      original : access clang_c_CXString_h.CXString;
      transformed : access clang_c_CXString_h.CXString);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5061
   pragma Import (C, clang_remap_getFilenames, "clang_remap_getFilenames");

   procedure clang_remap_dispose (arg1 : CXRemapping);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5067
   pragma Import (C, clang_remap_dispose, "clang_remap_dispose");

   type CXVisitorResult is 
     (CXVisit_Break,
      CXVisit_Continue);
   pragma Convention (C, CXVisitorResult);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5078

   type CXCursorAndRangeVisitor is record
      context : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5084
      visit : access function
           (arg1 : System.Address;
            arg2 : CXCursor;
            arg3 : CXSourceRange) return CXVisitorResult;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5085
   end record;
   pragma Convention (C_Pass_By_Copy, CXCursorAndRangeVisitor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5086

   --  skipped anonymous struct anon_14

   type CXResult is 
     (CXResult_Success,
      CXResult_Invalid,
      CXResult_VisitBreak);
   pragma Convention (C, CXResult);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5103

   function clang_findReferencesInFile
     (cursor : CXCursor;
      file : CXFile;
      visitor : CXCursorAndRangeVisitor) return CXResult;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5119
   pragma Import (C, clang_findReferencesInFile, "clang_findReferencesInFile");

   function clang_findIncludesInFile
     (TU : CXTranslationUnit;
      file : CXFile;
      visitor : CXCursorAndRangeVisitor) return CXResult;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5134
   pragma Import (C, clang_findIncludesInFile, "clang_findIncludesInFile");

   type CXIdxClientFile is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5158

   type CXIdxClientEntity is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5163

   type CXIdxClientContainer is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5169

   type CXIdxClientASTFile is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5175

   type CXIdxLoc_ptr_data_array is array (0 .. 1) of System.Address;
   type CXIdxLoc is record
      ptr_data : aliased CXIdxLoc_ptr_data_array;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5181
      int_data : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5182
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxLoc);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5183

   --  skipped anonymous struct anon_16

   type CXIdxIncludedFileInfo is record
      hashLoc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5192
      filename : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5196
      file : CXFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5200
      isImport : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5201
      isAngled : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5202
      isModuleImport : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5207
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxIncludedFileInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5208

   --  skipped anonymous struct anon_17

   type CXIdxImportedASTFileInfo is record
      file : CXFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5217
      module : CXModule;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5221
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5225
      isImplicit : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5230
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxImportedASTFileInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5232

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
   pragma Convention (C, CXIdxEntityKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5267

   type CXIdxEntityLanguage is 
     (CXIdxEntityLang_None,
      CXIdxEntityLang_C,
      CXIdxEntityLang_ObjC,
      CXIdxEntityLang_CXX);
   pragma Convention (C, CXIdxEntityLanguage);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5274

   type CXIdxEntityCXXTemplateKind is 
     (CXIdxEntity_NonTemplate,
      CXIdxEntity_Template,
      CXIdxEntity_TemplatePartialSpecialization,
      CXIdxEntity_TemplateSpecialization);
   pragma Convention (C, CXIdxEntityCXXTemplateKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5291

   type CXIdxAttrKind is 
     (CXIdxAttr_Unexposed,
      CXIdxAttr_IBAction,
      CXIdxAttr_IBOutlet,
      CXIdxAttr_IBOutletCollection);
   pragma Convention (C, CXIdxAttrKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5298

   type CXIdxAttrInfo is record
      kind : aliased CXIdxAttrKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5301
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5302
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5303
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxAttrInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5304

   --  skipped anonymous struct anon_23

   type CXIdxEntityInfo is record
      kind : aliased CXIdxEntityKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5307
      templateKind : aliased CXIdxEntityCXXTemplateKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5308
      lang : aliased CXIdxEntityLanguage;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5309
      name : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5310
      USR : Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5311
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5312
      attributes : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5313
      numAttributes : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5314
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxEntityInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5315

   --  skipped anonymous struct anon_24

   type CXIdxContainerInfo is record
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5318
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxContainerInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5319

   --  skipped anonymous struct anon_25

   type CXIdxIBOutletCollectionAttrInfo is record
      attrInfo : access constant CXIdxAttrInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5322
      objcClass : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5323
      classCursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5324
      classLoc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5325
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxIBOutletCollectionAttrInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5326

   --  skipped anonymous struct anon_26

   subtype CXIdxDeclInfoFlags is unsigned;
   CXIdxDeclFlag_Skipped : constant CXIdxDeclInfoFlags := 1;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5330

   type CXIdxDeclInfo is record
      entityInfo : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5333
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5334
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5335
      semanticContainer : access constant CXIdxContainerInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5336
      lexicalContainer : access constant CXIdxContainerInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5341
      isRedeclaration : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5342
      isDefinition : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5343
      isContainer : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5344
      declAsContainer : access constant CXIdxContainerInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5345
      isImplicit : aliased int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5350
      attributes : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5351
      numAttributes : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5352
      flags : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5354
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5356

   --  skipped anonymous struct anon_28

   type CXIdxObjCContainerKind is 
     (CXIdxObjCContainer_ForwardRef,
      CXIdxObjCContainer_Interface,
      CXIdxObjCContainer_Implementation);
   pragma Convention (C, CXIdxObjCContainerKind);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5362

   type CXIdxObjCContainerDeclInfo is record
      declInfo : access constant CXIdxDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5365
      kind : aliased CXIdxObjCContainerKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5366
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCContainerDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5367

   --  skipped anonymous struct anon_30

   type CXIdxBaseClassInfo is record
      base : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5370
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5371
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5372
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxBaseClassInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5373

   --  skipped anonymous struct anon_31

   type CXIdxObjCProtocolRefInfo is record
      protocol : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5376
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5377
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5378
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCProtocolRefInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5379

   --  skipped anonymous struct anon_32

   type CXIdxObjCProtocolRefListInfo is record
      protocols : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5382
      numProtocols : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5383
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCProtocolRefListInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5384

   --  skipped anonymous struct anon_33

   type CXIdxObjCInterfaceDeclInfo is record
      containerInfo : access constant CXIdxObjCContainerDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5387
      superInfo : access constant CXIdxBaseClassInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5388
      protocols : access constant CXIdxObjCProtocolRefListInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5389
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCInterfaceDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5390

   --  skipped anonymous struct anon_34

   type CXIdxObjCCategoryDeclInfo is record
      containerInfo : access constant CXIdxObjCContainerDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5393
      objcClass : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5394
      classCursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5395
      classLoc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5396
      protocols : access constant CXIdxObjCProtocolRefListInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5397
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCCategoryDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5398

   --  skipped anonymous struct anon_35

   type CXIdxObjCPropertyDeclInfo is record
      declInfo : access constant CXIdxDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5401
      getter : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5402
      setter : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5403
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxObjCPropertyDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5404

   --  skipped anonymous struct anon_36

   type CXIdxCXXClassDeclInfo is record
      declInfo : access constant CXIdxDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5407
      bases : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5408
      numBases : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5409
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxCXXClassDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5410

   --  skipped anonymous struct anon_37

   subtype CXIdxEntityRefKind is unsigned;
   CXIdxEntityRef_Direct : constant CXIdxEntityRefKind := 1;
   CXIdxEntityRef_Implicit : constant CXIdxEntityRefKind := 2;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5425

   type CXIdxEntityRefInfo is record
      kind : aliased CXIdxEntityRefKind;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5431
      cursor : aliased CXCursor;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5435
      loc : aliased CXIdxLoc;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5436
      referencedEntity : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5440
      parentEntity : access constant CXIdxEntityInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5452
      container : access constant CXIdxContainerInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5456
   end record;
   pragma Convention (C_Pass_By_Copy, CXIdxEntityRefInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5457

   --  skipped anonymous struct anon_39

   type IndexerCallbacks is record
      abortQuery : access function (arg1 : CXClientData; arg2 : System.Address) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5468
      diagnostic : access procedure
           (arg1 : CXClientData;
            arg2 : CXDiagnosticSet;
            arg3 : System.Address);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5474
      enteredMainFile : access function
           (arg1 : CXClientData;
            arg2 : CXFile;
            arg3 : System.Address) return CXIdxClientFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5477
      ppIncludedFile : access function (arg1 : CXClientData; arg2 : access constant CXIdxIncludedFileInfo) return CXIdxClientFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5483
      importedASTFile : access function (arg1 : CXClientData; arg2 : access constant CXIdxImportedASTFileInfo) return CXIdxClientASTFile;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5494
      startedTranslationUnit : access function (arg1 : CXClientData; arg2 : System.Address) return CXIdxClientContainer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5500
      indexDeclaration : access procedure (arg1 : CXClientData; arg2 : access constant CXIdxDeclInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5503
      indexEntityReference : access procedure (arg1 : CXClientData; arg2 : access constant CXIdxEntityRefInfo);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5509
   end record;
   pragma Convention (C_Pass_By_Copy, IndexerCallbacks);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5511

   --  skipped anonymous struct anon_40

   function clang_index_isEntityObjCContainerKind (arg1 : CXIdxEntityKind) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5513
   pragma Import (C, clang_index_isEntityObjCContainerKind, "clang_index_isEntityObjCContainerKind");

   function clang_index_getObjCContainerDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCContainerDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5515
   pragma Import (C, clang_index_getObjCContainerDeclInfo, "clang_index_getObjCContainerDeclInfo");

   function clang_index_getObjCInterfaceDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCInterfaceDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5518
   pragma Import (C, clang_index_getObjCInterfaceDeclInfo, "clang_index_getObjCInterfaceDeclInfo");

   function clang_index_getObjCCategoryDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCCategoryDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5522
   pragma Import (C, clang_index_getObjCCategoryDeclInfo, "clang_index_getObjCCategoryDeclInfo");

   function clang_index_getObjCProtocolRefListInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCProtocolRefListInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5525
   pragma Import (C, clang_index_getObjCProtocolRefListInfo, "clang_index_getObjCProtocolRefListInfo");

   function clang_index_getObjCPropertyDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxObjCPropertyDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5528
   pragma Import (C, clang_index_getObjCPropertyDeclInfo, "clang_index_getObjCPropertyDeclInfo");

   function clang_index_getIBOutletCollectionAttrInfo (arg1 : access constant CXIdxAttrInfo) return access constant CXIdxIBOutletCollectionAttrInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5531
   pragma Import (C, clang_index_getIBOutletCollectionAttrInfo, "clang_index_getIBOutletCollectionAttrInfo");

   function clang_index_getCXXClassDeclInfo (arg1 : access constant CXIdxDeclInfo) return access constant CXIdxCXXClassDeclInfo;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5534
   pragma Import (C, clang_index_getCXXClassDeclInfo, "clang_index_getCXXClassDeclInfo");

   function clang_index_getClientContainer (arg1 : access constant CXIdxContainerInfo) return CXIdxClientContainer;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5541
   pragma Import (C, clang_index_getClientContainer, "clang_index_getClientContainer");

   procedure clang_index_setClientContainer (arg1 : access constant CXIdxContainerInfo; arg2 : CXIdxClientContainer);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5548
   pragma Import (C, clang_index_setClientContainer, "clang_index_setClientContainer");

   function clang_index_getClientEntity (arg1 : access constant CXIdxEntityInfo) return CXIdxClientEntity;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5554
   pragma Import (C, clang_index_getClientEntity, "clang_index_getClientEntity");

   procedure clang_index_setClientEntity (arg1 : access constant CXIdxEntityInfo; arg2 : CXIdxClientEntity);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5560
   pragma Import (C, clang_index_setClientEntity, "clang_index_setClientEntity");

   type CXIndexAction is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5566

   function clang_IndexAction_create (CIdx : CXIndex) return CXIndexAction;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5574
   pragma Import (C, clang_IndexAction_create, "clang_IndexAction_create");

   procedure clang_IndexAction_dispose (arg1 : CXIndexAction);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5582
   pragma Import (C, clang_IndexAction_dispose, "clang_IndexAction_dispose");

   subtype CXIndexOptFlags is unsigned;
   CXIndexOpt_None : constant CXIndexOptFlags := 0;
   CXIndexOpt_SuppressRedundantRefs : constant CXIndexOptFlags := 1;
   CXIndexOpt_IndexFunctionLocalSymbols : constant CXIndexOptFlags := 2;
   CXIndexOpt_IndexImplicitTemplateInstantiations : constant CXIndexOptFlags := 4;
   CXIndexOpt_SuppressWarnings : constant CXIndexOptFlags := 8;
   CXIndexOpt_SkipParsedBodiesInSession : constant CXIndexOptFlags := 16;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5621

   function clang_indexSourceFile
     (arg1 : CXIndexAction;
      client_data : CXClientData;
      index_callbacks : access constant IndexerCallbacks;
      index_callbacks_size : unsigned;
      index_options : unsigned;
      source_filename : Interfaces.C.Strings.chars_ptr;
      command_line_args : System.Address;
      num_command_line_args : int;
      unsaved_files : System.Address;
      num_unsaved_files : unsigned;
      out_TU : System.Address;
      TU_options : unsigned) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5648
   pragma Import (C, clang_indexSourceFile, "clang_indexSourceFile");

   function clang_indexTranslationUnit
     (arg1 : CXIndexAction;
      client_data : CXClientData;
      index_callbacks : access constant IndexerCallbacks;
      index_callbacks_size : unsigned;
      index_options : unsigned;
      arg6 : CXTranslationUnit) return int;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5677
   pragma Import (C, clang_indexTranslationUnit, "clang_indexTranslationUnit");

   procedure clang_indexLoc_getFileLocation
     (loc : CXIdxLoc;
      indexFile : System.Address;
      file : System.Address;
      line : access unsigned;
      column : access unsigned;
      offset : access unsigned);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5692
   pragma Import (C, clang_indexLoc_getFileLocation, "clang_indexLoc_getFileLocation");

   function clang_indexLoc_getCXSourceLocation (loc : CXIdxLoc) return CXSourceLocation;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5703
   pragma Import (C, clang_indexLoc_getCXSourceLocation, "clang_indexLoc_getCXSourceLocation");

   type CXFieldVisitor is access function (arg1 : CXCursor; arg2 : CXClientData) return CXVisitorResult;
   pragma Convention (C, CXFieldVisitor);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5716

   function clang_Type_visitFields
     (T : CXType;
      visitor : CXFieldVisitor;
      client_data : CXClientData) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/Index.h:5738
   pragma Import (C, clang_Type_visitFields, "clang_Type_visitFields");

end clang_c_Index_h;
