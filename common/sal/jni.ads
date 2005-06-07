with Interfaces.C;
with Interfaces.C.Strings;
with System;

package JNI is

   type Jint is new Interfaces.C.int;
   type Jstring is new System.Address;
   type Jobject is new System.Address;
   type JNIEnv is new System.Address;
   type Jclass is new System.Address;
   type JmethodID is new System.Address;
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

   function GetMethodID
     (Env     : JNIEnv;
      Class   : Jclass;
      Name    : Interfaces.C.Strings.chars_ptr;
      Profile : Interfaces.C.Strings.chars_ptr) return JmethodID;
   pragma Import (C, GetMethodID, "W_GetMethodID");

   procedure CallVoidMethodIIIS
    (Env    : JNIEnv;
     Object : Jobject;
     Id     : JmethodID;
     P1     : Jint;
     P2     : Jint;
     P3     : Jint;
     P4     : Jstring);
   pragma Import (C, CallVoidMethodIIIS, "W_CallVoidMethodIIIS");

end JNI;

