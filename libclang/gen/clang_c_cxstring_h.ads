pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;

package clang_c_CXString_h is

   type CXString is record
      data : System.Address;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXString.h:39
      private_flags : aliased unsigned;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXString.h:40
   end record;
   pragma Convention (C_Pass_By_Copy, CXString);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXString.h:41

   --  skipped anonymous struct anon_1

   function clang_getCString (string : CXString) return Interfaces.C.Strings.chars_ptr;  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXString.h:46
   pragma Import (C, clang_getCString, "clang_getCString");

   procedure clang_disposeString (string : CXString);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXString.h:51
   pragma Import (C, clang_disposeString, "clang_disposeString");

end clang_c_CXString_h;
