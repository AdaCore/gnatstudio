/*---------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005 - 2006                     --
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
---------------------------------------------------------------------*/

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

#ifdef __MANAGE_SEGV_HANDLER__

/* This case has to be used on solaris or linux, by setting
 * -D__MANAGE_SEGV_HANDLERS__ on the compilation line of this file */

#include <signal.h>

jint Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_getSEGVHandlerInt
  (JNIEnv * Env, jclass * this)
{
  struct sigaction * handler =
    (struct sigaction *) malloc (sizeof (struct sigaction));

  sigaction (SIGSEGV, NULL, handler);

  return (jint) handler;
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_setSEGVHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
  struct sigaction * handler = (struct sigaction *) addr;

  sigaction (SIGSEGV, handler, NULL);
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_freeSEGVHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
  struct sigaction * handler = (struct sigaction *) addr;
  free (handler);
}

jint Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_getTERMHandlerInt
  (JNIEnv * Env, jclass * this)
{
  struct sigaction * handler =
    (struct sigaction *) malloc (sizeof (struct sigaction));

  sigaction (SIGTERM, NULL, handler);

  return (jint) handler;
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_setTERMHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
  struct sigaction * handler = (struct sigaction *) addr;

  sigaction (SIGTERM, handler, NULL);
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_freeTERMHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
  struct sigaction * handler = (struct sigaction *) addr;
  free (handler);
}

#else

jint Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_getSEGVHandlerInt
  (JNIEnv * Env, jclass * this)
{
  return 0;
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_setSEGVHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_freeSEGVHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
}


jint Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_getTERMHandlerInt
  (JNIEnv * Env, jclass * this)
{
  return 0;
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_setTERMHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
}

void Java_com_adacore_gnatbench_core_gpsjni_SystemHandler_freeTERMHandlerInt
  (JNIEnv * Env, jclass * this, jint addr)
{
}

#endif

void gps_utilsinit (void);

/**
 * This function is used to manually initialize the ada library (since there
 * is no way to do it automatically under win32).
 */
void Java_com_adacore_gnatbench_core_gpsjni_GPSJni_libInitInt
  (JNIEnv * Env, jclass * this)
{
  gps_utilsinit ();
}

