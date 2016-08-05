pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;
with clang_c_CXString_h;

package clang_c_CXCompilationDatabase_h is

   type CXCompilationDatabase is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:38

   type CXCompileCommands is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:49

   type CXCompileCommand is new System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:54

   type CXCompilationDatabase_Error is 
     (CXCompilationDatabase_NoError,
      CXCompilationDatabase_CanNotLoadDatabase);
   pragma Convention (C, CXCompilationDatabase_Error);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:70

   function clang_CompilationDatabase_fromDirectory (BuildDir : Interfaces.C.Strings.chars_ptr; ErrorCode : access CXCompilationDatabase_Error) return CXCompilationDatabase;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:80
   pragma Import (C, clang_CompilationDatabase_fromDirectory, "clang_CompilationDatabase_fromDirectory");

   procedure clang_CompilationDatabase_dispose (arg1 : CXCompilationDatabase);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:87
   pragma Import (C, clang_CompilationDatabase_dispose, "clang_CompilationDatabase_dispose");

   function clang_CompilationDatabase_getCompileCommands (arg1 : CXCompilationDatabase; CompleteFileName : Interfaces.C.Strings.chars_ptr) return CXCompileCommands;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:94
   pragma Import (C, clang_CompilationDatabase_getCompileCommands, "clang_CompilationDatabase_getCompileCommands");

   function clang_CompilationDatabase_getAllCompileCommands (arg1 : CXCompilationDatabase) return CXCompileCommands;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:101
   pragma Import (C, clang_CompilationDatabase_getAllCompileCommands, "clang_CompilationDatabase_getAllCompileCommands");

   procedure clang_CompileCommands_dispose (arg1 : CXCompileCommands);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:106
   pragma Import (C, clang_CompileCommands_dispose, "clang_CompileCommands_dispose");

   function clang_CompileCommands_getSize (arg1 : CXCompileCommands) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:112
   pragma Import (C, clang_CompileCommands_getSize, "clang_CompileCommands_getSize");

   function clang_CompileCommands_getCommand (arg1 : CXCompileCommands; I : unsigned) return CXCompileCommand;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:120
   pragma Import (C, clang_CompileCommands_getCommand, "clang_CompileCommands_getCommand");

   function clang_CompileCommand_getDirectory (arg1 : CXCompileCommand) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:126
   pragma Import (C, clang_CompileCommand_getDirectory, "clang_CompileCommand_getDirectory");

   function clang_CompileCommand_getNumArgs (arg1 : CXCompileCommand) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:133
   pragma Import (C, clang_CompileCommand_getNumArgs, "clang_CompileCommand_getNumArgs");

   function clang_CompileCommand_getArg (arg1 : CXCompileCommand; I : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:142
   pragma Import (C, clang_CompileCommand_getArg, "clang_CompileCommand_getArg");

   function clang_CompileCommand_getNumMappedSources (arg1 : CXCompileCommand) return unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:148
   pragma Import (C, clang_CompileCommand_getNumMappedSources, "clang_CompileCommand_getNumMappedSources");

   function clang_CompileCommand_getMappedSourcePath (arg1 : CXCompileCommand; I : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:154
   pragma Import (C, clang_CompileCommand_getMappedSourcePath, "clang_CompileCommand_getMappedSourcePath");

   function clang_CompileCommand_getMappedSourceContent (arg1 : CXCompileCommand; I : unsigned) return clang_c_CXString_h.CXString;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXCompilationDatabase.h:160
   pragma Import (C, clang_CompileCommand_getMappedSourceContent, "clang_CompileCommand_getMappedSourceContent");

end clang_c_CXCompilationDatabase_h;
