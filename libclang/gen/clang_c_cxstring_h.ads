pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;

package clang_c_CXString_h is

   --  skipped anonymous struct anon_1

   type CXString is record
      data : System.Address;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:39
      private_flags : aliased unsigned;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:40
   end record
   with Convention => C_Pass_By_Copy;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:41

   --  skipped anonymous struct anon_2

   type CXStringSet is record
      Strings : access CXString;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:44
      Count : aliased unsigned;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:45
   end record
   with Convention => C_Pass_By_Copy;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:46

   function clang_getCString (string : CXString) return Interfaces.C.Strings.chars_ptr  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCString";

   procedure clang_disposeString (string : CXString)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeString";

   procedure clang_disposeStringSet (set : access CXStringSet)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-8.0.0.src/include/clang-c/CXString.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeStringSet";

end clang_c_CXString_h;
