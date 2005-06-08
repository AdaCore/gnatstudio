#include <jni.h>

// TODO: See if there are existing C profiles, in order to avoid the
// use of those wrappers.

const char * W_GetStringUTFChars (JNIEnv * Env, jstring String);
void W_ReleaseStringUTFChars
   (JNIEnv * Env, jstring String, const char * Str);
jstring W_NewStringUTF (JNIEnv * Env, const char * Str);
jmethodID W_GetMethodID
 (JNIEnv * Env, jclass Class, const char * Name, const char * Profile);
void W_CallVoidMethodIIIS
 (JNIEnv * Env,
  jobject Object,
  jmethodID Id,
  jint P1, jint P2, jint P3, jstring P4);

const char * W_GetStringUTFChars (JNIEnv * Env, jstring String)
{
   return (*Env)->GetStringUTFChars (Env, String, NULL);
}

void W_ReleaseStringUTFChars (JNIEnv * Env, jstring String, const char * Str)
{
   (*Env)->ReleaseStringUTFChars (Env, String, Str);
}

jstring W_NewStringUTF (JNIEnv * Env, const char * Str)
{
   return (*Env)->NewStringUTF (Env, Str);
}

jmethodID W_GetMethodID
  (JNIEnv * Env, jclass Class, const char * Name, const char * Profile)
{
   return (*Env)->GetMethodID (Env, Class, Name, Profile);
}

void W_CallVoidMethodIIIS
    (JNIEnv * Env,
     jobject Object,
     jmethodID Id,
     jint P1, jint P2, jint P3, jstring P4)
{
   (*Env)->CallVoidMethod (Env, Object, Id, P1, P2, P3, P4);
}

