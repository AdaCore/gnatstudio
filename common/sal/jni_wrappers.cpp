#include <jni.h>

extern "C"
{
   const char * W_GetStringUTFChars (JNIEnv * Env, jstring String);
   void W_ReleaseStringUTFChars (JNIEnv * Env, jstring String, const char * Str);
   jstring W_NewStringUTF (JNIEnv * Env, const char * Str);
}

const char * W_GetStringUTFChars (JNIEnv * Env, jstring String)
{
   return Env->GetStringUTFChars (String, NULL);
}

void W_ReleaseStringUTFChars (JNIEnv * Env, jstring String, const char * Str)
{
   Env->ReleaseStringUTFChars (String, Str);
}

jstring W_NewStringUTF (JNIEnv * Env, const char * Str)
{
   return Env->NewStringUTF (Str);
}
