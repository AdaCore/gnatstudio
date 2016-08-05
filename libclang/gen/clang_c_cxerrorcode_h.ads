pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package clang_c_CXErrorCode_h is

   type CXErrorCode is 
     (CXError_Success,
      CXError_Failure,
      CXError_Crashed,
      CXError_InvalidArguments,
      CXError_ASTReadError);
   pragma Convention (C, CXErrorCode);  -- /export/work/setton/src/ANCR/src/gps/libclang/cfe-3.7.1.src/include/clang-c/CXErrorCode.h:29

end clang_c_CXErrorCode_h;
