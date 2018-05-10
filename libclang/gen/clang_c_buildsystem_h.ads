pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with clang_c_CXErrorCode_h;
with System;

package clang_c_BuildSystem_h is

   function clang_getBuildSessionTimestamp return Extensions.unsigned_long_long  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:34
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getBuildSessionTimestamp";

   type CXVirtualFileOverlayImpl is null record;   -- incomplete struct

   type CXVirtualFileOverlay is access all CXVirtualFileOverlayImpl;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:40

   function clang_VirtualFileOverlay_create (options : unsigned) return CXVirtualFileOverlay  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_create";

   function clang_VirtualFileOverlay_addFileMapping
     (arg1 : CXVirtualFileOverlay;
      virtualPath : Interfaces.C.Strings.chars_ptr;
      realPath : Interfaces.C.Strings.chars_ptr) return clang_c_CXErrorCode_h.CXErrorCode  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_addFileMapping";

   function clang_VirtualFileOverlay_setCaseSensitivity (arg1 : CXVirtualFileOverlay; caseSensitive : int) return clang_c_CXErrorCode_h.CXErrorCode  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_setCaseSensitivity";

   function clang_VirtualFileOverlay_writeToBuffer
     (arg1 : CXVirtualFileOverlay;
      options : unsigned;
      out_buffer_ptr : System.Address;
      out_buffer_size : access unsigned) return clang_c_CXErrorCode_h.CXErrorCode  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_writeToBuffer";

   procedure clang_free (buffer : System.Address)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "clang_free";

   procedure clang_VirtualFileOverlay_dispose (arg1 : CXVirtualFileOverlay)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:96
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VirtualFileOverlay_dispose";

   type CXModuleMapDescriptorImpl is null record;   -- incomplete struct

   type CXModuleMapDescriptor is access all CXModuleMapDescriptorImpl;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:101

   function clang_ModuleMapDescriptor_create (options : unsigned) return CXModuleMapDescriptor  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_create";

   function clang_ModuleMapDescriptor_setFrameworkModuleName (arg1 : CXModuleMapDescriptor; name : Interfaces.C.Strings.chars_ptr) return clang_c_CXErrorCode_h.CXErrorCode  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_setFrameworkModuleName";

   function clang_ModuleMapDescriptor_setUmbrellaHeader (arg1 : CXModuleMapDescriptor; name : Interfaces.C.Strings.chars_ptr) return clang_c_CXErrorCode_h.CXErrorCode  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:125
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_setUmbrellaHeader";

   function clang_ModuleMapDescriptor_writeToBuffer
     (arg1 : CXModuleMapDescriptor;
      options : unsigned;
      out_buffer_ptr : System.Address;
      out_buffer_size : access unsigned) return clang_c_CXErrorCode_h.CXErrorCode  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_writeToBuffer";

   procedure clang_ModuleMapDescriptor_dispose (arg1 : CXModuleMapDescriptor)  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/BuildSystem.h:145
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ModuleMapDescriptor_dispose";

end clang_c_BuildSystem_h;
