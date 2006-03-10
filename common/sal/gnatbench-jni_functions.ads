-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2005 - 2006                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  The purpose of this package is to provide wrappers for the Java eclipse
--  plugin, via the JNI interface.

with JNI; use JNI;

package GNATbench.JNI_Functions is

   function Java_GPSJni_analyzeAdaSourceInt
     (Env : JNIEnv; This : Jobject; Str : Jstring) return Jint;
   pragma Export
     (C,
      Java_GPSJni_analyzeAdaSourceInt,
      "Java_com_adacore_gnatbench_core_gpsjni_GPSJni_analyseAdaSourceInt");

   procedure Java_GPSJni_indentAdaBufferInt
     (Env               : JNIEnv;
      This              : Jobject;
      Buffer            : Jstring;
      Line_From         : Jint;
      Line_To           : Jint;
      Callback_Object   : Jobject;
      Callback_Class    : Jclass;
      Callback_Function : Jstring);
   pragma Export
     (C,
      Java_GPSJni_indentAdaBufferInt,
      "Java_com_adacore_gnatbench_core_gpsjni_GPSJni_indentAdaBufferInt");

   function Java_ConstructAccess_getSlocStartInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C, Java_ConstructAccess_getSlocStartInt,
      "Java_com_adacore_gnatbench_core_" &
      "gpsjni_ConstructAccess_getSlocStartInt");

   function Java_ConstructAccess_getSlocEntityInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getSlocEntityInt,
      "Java_com_adacore_gnatbench_core_" &
      "gpsjni_ConstructAccess_getSlocEntityInt");

   function Java_ConstructAccess_getSlocEndInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getSlocEndInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructAccess_getSlocEndInt");

   function Java_ConstructAccess_getPrevInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getPrevInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructAccess_getPrevInt");

   function Java_ConstructAccess_getNextInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getNextInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructAccess_getNextInt");

   function Java_ConstructAccess_getCategoryInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getCategoryInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructAccess_getCategoryInt");

   function Java_ConstructAccess_getNameInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jstring;
   pragma Export
     (C,
      Java_ConstructAccess_getNameInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructAccess_getNameInt");

   function Java_ConstructAccess_getVisibilityInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getVisibilityInt,
      "Java_com_adacore_gnatbench_core_" &
      "gpsjni_ConstructAccess_getVisibilityInt");

   function Java_ConstructAccess_getProfileInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jstring;
   pragma Export
     (C,
      Java_ConstructAccess_getProfileInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructAccess_getProfileInt");

   function Java_ConstructAccess_getIsDeclarationInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructAccess_getIsDeclarationInt,
      "Java_com_adacore_gnatbench_core_" &
      "gpsjni_ConstructAccess_getIsDeclarationInt");

   function Java_ConstructList_getFirstInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructList_getFirstInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructList_getFirstInt");

   function Java_ConstructList_getCurrentInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructList_getCurrentInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructList_getCurrentInt");

   function Java_ConstructList_getLastInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_ConstructList_getLastInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructList_getLastInt");

   procedure Java_ConstructList_freeInt
     (Env : JNIEnv; This : Jobject; Addr : Jint);
   pragma Export
     (C,
      Java_ConstructList_freeInt,
      "Java_com_adacore_gnatbench_core_gpsjni_ConstructList_freeInt");

   function Java_SourceLocation_getLineInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_SourceLocation_getLineInt,
      "Java_com_adacore_gnatbench_core_gpsjni_SourceLocation_getLineInt");

   function Java_SourceLocation_getColumnInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_SourceLocation_getColumnInt,
      "Java_com_adacore_gnatbench_core_gpsjni_SourceLocation_getColumnInt");

   function Java_SourceLocation_getIndexInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint;
   pragma Export
     (C,
      Java_SourceLocation_getIndexInt,
      "Java_com_adacore_gnatbench_core_gpsjni_SourceLocation_getIndexInt");

end GNATbench.JNI_Functions;
