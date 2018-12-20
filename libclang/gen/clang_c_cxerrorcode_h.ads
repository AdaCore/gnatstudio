pragma Ada_2012;
pragma Style_Checks (Off);

--  with Interfaces.C; use Interfaces.C;

package clang_c_CXErrorCode_h is

   type CXErrorCode is 
     (CXError_Success,
      CXError_Failure,
      CXError_Crashed,
      CXError_InvalidArguments,
      CXError_ASTReadError)
   with Convention => C;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/CXErrorCode.h:29

end clang_c_CXErrorCode_h;
