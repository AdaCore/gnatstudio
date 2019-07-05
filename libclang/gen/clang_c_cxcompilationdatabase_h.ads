pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with clang_c_CXString_h;

package clang_c_CXCompilationDatabase_h is

   type CXCompilationDatabase is new System.Address;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:38

   type CXCompileCommands is new System.Address;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:49

   type CXCompileCommand is new System.Address;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:54

   type CXCompilationDatabase_Error is 
     (CXCompilationDatabase_NoError,
      CXCompilationDatabase_CanNotLoadDatabase)
   with Convention => C;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:70

   function clang_CompilationDatabase_fromDirectory (BuildDir : Interfaces.C.Strings.chars_ptr; ErrorCode : access CXCompilationDatabase_Error) return CXCompilationDatabase  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompilationDatabase_fromDirectory";

   procedure clang_CompilationDatabase_dispose (arg1 : CXCompilationDatabase)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompilationDatabase_dispose";

   function clang_CompilationDatabase_getCompileCommands (arg1 : CXCompilationDatabase; CompleteFileName : Interfaces.C.Strings.chars_ptr) return CXCompileCommands  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompilationDatabase_getCompileCommands";

   function clang_CompilationDatabase_getAllCompileCommands (arg1 : CXCompilationDatabase) return CXCompileCommands  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:101
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompilationDatabase_getAllCompileCommands";

   procedure clang_CompileCommands_dispose (arg1 : CXCompileCommands)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommands_dispose";

   function clang_CompileCommands_getSize (arg1 : CXCompileCommands) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommands_getSize";

   function clang_CompileCommands_getCommand (arg1 : CXCompileCommands; I : unsigned) return CXCompileCommand  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommands_getCommand";

   function clang_CompileCommand_getDirectory (arg1 : CXCompileCommand) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:126
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getDirectory";

   function clang_CompileCommand_getFilename (arg1 : CXCompileCommand) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:132
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getFilename";

   function clang_CompileCommand_getNumArgs (arg1 : CXCompileCommand) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:139
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getNumArgs";

   function clang_CompileCommand_getArg (arg1 : CXCompileCommand; I : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:148
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getArg";

   function clang_CompileCommand_getNumMappedSources (arg1 : CXCompileCommand) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getNumMappedSources";

   function clang_CompileCommand_getMappedSourcePath (arg1 : CXCompileCommand; I : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:160
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getMappedSourcePath";

   function clang_CompileCommand_getMappedSourceContent (arg1 : CXCompileCommand; I : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXCompilationDatabase.h:166
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getMappedSourceContent";

end clang_c_CXCompilationDatabase_h;
