with Interfaces.C;
with Interfaces.C.Strings;
with System;

package JNI is

   type Jint is new Interfaces.C.int;
   type Jstring is new System.Address;
   type Jobject is new System.Address;
   type JNIEnv is new System.Address;
   type Jbyte is new Interfaces.C.unsigned_char;

   function GetStringUTFChars (Env : JNIEnv; String : Jstring) return
     Interfaces.C.Strings.chars_ptr;
   pragma Import (C, GetStringUTFChars, "W_GetStringUTFChars");

   procedure ReleaseStringUTFChars
     (Env : JNIEnv;
      String : Jstring;
      Str : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, ReleaseStringUTFChars, "W_ReleaseStringUTFChars");

   function NewStringUTF
     (Env : JNIEnv;
      Str : Interfaces.C.Strings.chars_ptr) return Jstring;
   pragma Import (C, NewStringUTF, "W_NewStringUTF");

end JNI;

