-----------------------------------------------------------------------
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
-----------------------------------------------------------------------

with Language; use Language;
with System; use System;
with System.Address_To_Access_Conversions;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada_Analyzer; use Ada_Analyzer;
with Basic_Types; use Basic_Types;
with Case_Handling; use Case_Handling;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body GNATbench.JNI_Functions is

   procedure Db (Str : String);
   pragma Unreferenced (Db);
   --  ???

   function Db (Str : String) return Integer;
   pragma Unreferenced (Db);
   --  ???

   function To_Jint is new Ada.Unchecked_Conversion (Address, Jint);
   function To_Address is new Ada.Unchecked_Conversion (Jint, Address);
   --  Convert between System.Address and Jint.

   procedure Db (Str : String) is
   begin
      Put_Line (Str);
      Flush;
   end Db;

   function Db (Str : String) return Integer is
   begin
      Put_Line (Str);
      Flush;
      return 0;
   end Db;

   type Construct_List_Ptr is access all Construct_List;
   type Source_Location_Ptr is access all Source_Location;
   type Construct_Access_Ptr is access all Construct_Information;

   package Construct_List_Conversion is new
     System.Address_To_Access_Conversions (Construct_List);
   use Construct_List_Conversion;

   package Construct_Information_Conversion is new
     System.Address_To_Access_Conversions (Construct_Information);
   use Construct_Information_Conversion;

   package Source_Location_Conversion is new
     System.Address_To_Access_Conversions (Source_Location);
   use Source_Location_Conversion;

   ------------------------------------
   -- Java_GpsJni_parseConstructsInt --
   ------------------------------------

   function Java_GPSJni_analyzeAdaSourceInt
     (Env : JNIEnv;
      This : Jobject;
      Str : Jstring)
      return Jint
   is
      pragma Unreferenced (This);
      Constructs_Stored : aliased constant
        Construct_List_Ptr := new Construct_List;

      function Convert is new Ada.Unchecked_Conversion
        (chars_ptr, System.Address);

      C_Str : constant Interfaces.C.Strings.chars_ptr :=
        GetStringUTFChars (Env, Str);

      Ada_Str : constant Unchecked_String_Access :=
        To_Unchecked_String (Convert (C_Str));
      pragma Suppress (Access_Check, Ada_Str);

   begin
      Analyze_Ada_Source
        (Buffer            => Ada_Str (1 .. Integer (Strlen (C_Str))),
         Indent_Params     => Default_Indent_Parameters,
         Format            => False,
         Constructs        => Construct_List_Access (Constructs_Stored));

      ReleaseStringUTFChars (Env, Str, C_Str);

      return To_Jint (Constructs_Stored.all'Address);
   end Java_GPSJni_analyzeAdaSourceInt;

   ------------------------------------
   -- Java_GpsJni_indentAdaBufferInt --
   ------------------------------------

   procedure Java_GPSJni_indentAdaBufferInt
     (Env               : JNIEnv;
      This              : Jobject;
      Buffer            : Jstring;
      Line_From         : Jint;
      Line_To           : Jint;
      Callback_Object   : Jobject;
      Callback_Class    : Jclass;
      Callback_Function : Jstring)
   is
      pragma Unreferenced (This);

      function Convert is new Ada.Unchecked_Conversion
        (chars_ptr, System.Address);

      C_Str : constant Interfaces.C.Strings.chars_ptr :=
        GetStringUTFChars (Env, Buffer);

      Ada_Str : constant Unchecked_String_Access :=
        To_Unchecked_String (Convert (C_Str));
      pragma Suppress (Access_Check, Ada_Str);

      Id : JmethodID;
      C_Function : constant Interfaces.C.Strings.chars_ptr :=
        GetStringUTFChars (Env, Callback_Function);
      C_Profile  : Interfaces.C.Strings.chars_ptr :=
        New_String ("(IIILjava/lang/String;)V");

      procedure Callback
        (Line    : Natural;
         First   : Natural;
         Last    : Natural;
         Replace : String);
      --  ???

      procedure Callback
         (Line    : Natural;
          First   : Natural;
          Last    : Natural;
          Replace : String)
      is
         C_Replace    : Interfaces.C.Strings.chars_ptr :=
           New_String (Replace);
         Java_Replace : constant Jstring := NewStringUTF (Env, C_Replace);
      begin
         CallVoidMethodIIIS
           (Env,
            Callback_Object,
            Id,
            Jint (Line),
            Jint (First),
            Jint (Last),
            Java_Replace);
         Free (C_Replace);
      end Callback;

      GNATbench_Indent_Parameters : constant
        Indent_Parameters :=
          (Indent_Level        => 3,
           Indent_Continue     => 2,
           Indent_Conditional  => 0,
           Indent_Record       => 3,
           Indent_Decl         => 0,
           Tab_Width           => 8,
           Indent_Case_Extra   => Automatic,
           Casing_Policy       => Case_Handling.Disabled,
           Reserved_Casing     => Case_Handling.Unchanged,
           Ident_Casing        => Case_Handling.Unchanged,
           Format_Operators    => False,
           Use_Tabs            => False,
           Align_On_Colons     => False,
           Align_On_Arrows     => False,
           Align_Decl_On_Colon => False);

   begin
      Id := GetMethodID
        (Env,
         Callback_Class,
         C_Function,
         C_Profile);
      Analyze_Ada_Source
        (Buffer            => Ada_Str (1 .. Integer (Strlen (C_Str))),
         Indent_Params     => GNATbench_Indent_Parameters,
         Format            => True,
         From              => Integer (Line_From),
         To                => Integer (Line_To),
         Replace           => Callback'Unrestricted_Access);

      ReleaseStringUTFChars (Env, Callback_Function, C_Function);
      Free (C_Profile);
   end Java_GPSJni_indentAdaBufferInt;

   ------------------------------------------
   -- Java_ConstructAccess_getSlocStartInt --
   ------------------------------------------

   function Java_ConstructAccess_getSlocStartInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return To_Jint (Construct.Sloc_Start'Address);
   end Java_ConstructAccess_getSlocStartInt;

   -------------------------------------------
   -- Java_ConstructAccess_getSlocEntityInt --
   -------------------------------------------

   function Java_ConstructAccess_getSlocEntityInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer
           (To_Address (Addr)));
   begin
      return To_Jint (Construct.Sloc_Entity'Address);
   end Java_ConstructAccess_getSlocEntityInt;

   ----------------------------------------
   -- Java_ConstructAccess_getSlocEndInt --
   ----------------------------------------

   function Java_ConstructAccess_getSlocEndInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return To_Jint (Construct.Sloc_End'Address);
   end Java_ConstructAccess_getSlocEndInt;

   -------------------------------------
   -- Java_ConstructAccess_getPrevInt --
   -------------------------------------

   function Java_ConstructAccess_getPrevInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Construct.Prev = null then
         return 0;
      else
         return To_Jint (Construct.Prev.all'Address);
      end if;
   end Java_ConstructAccess_getPrevInt;

   -------------------------------------
   -- Java_ConstructAccess_getNextInt --
   -------------------------------------

   function Java_ConstructAccess_getNextInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Construct.Next = null then
         return 0;
      else
         return To_Jint (Construct.Next.all'Address);
      end if;
   end Java_ConstructAccess_getNextInt;

   -----------------------------------------
   -- Java_ConstructAccess_getCategoryInt --
   -----------------------------------------

   function Java_ConstructAccess_getCategoryInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return Jint (Language_Category'Pos (Construct.Category));
   end Java_ConstructAccess_getCategoryInt;

   -------------------------------------
   -- Java_ConstructAccess_getNameInt --
   -------------------------------------

   function Java_ConstructAccess_getNameInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jstring
   is
      pragma Unreferenced (This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Construct.Name = null then
         return NewStringUTF (Env, New_String ("<no name>"));
      else
         return NewStringUTF (Env, New_String (Construct.Name.all));
      end if;
   end Java_ConstructAccess_getNameInt;

   -------------------------------------------
   -- Java_ConstructAccess_getVisibilityInt --
   -------------------------------------------

   function Java_ConstructAccess_getVisibilityInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint
   is
      pragma Unreferenced (This, Env);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return Jint (Construct_Visibility'Pos (Construct.Visibility));
   end Java_ConstructAccess_getVisibilityInt;

   ----------------------------------------
   -- Java_ConstructAccess_getProfileInt --
   ----------------------------------------

   function Java_ConstructAccess_getProfileInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jstring
   is
      pragma Unreferenced (This);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Construct.Profile = null then
         return NewStringUTF (Env, New_String (""));
      else
         return NewStringUTF (Env, New_String (Construct.Profile.all));
      end if;
   end Java_ConstructAccess_getProfileInt;

   ----------------------------------------------
   -- Java_ConstructAccess_getIsDeclarationInt --
   ----------------------------------------------

   function Java_ConstructAccess_getIsDeclarationInt
     (Env : JNIEnv; This : Jobject; Addr : Jint) return Jint
   is
      pragma Unreferenced (This, Env);
      Construct : constant Construct_Access_Ptr := Construct_Access_Ptr
        (Construct_Information_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return Jint (Boolean'Pos (Construct.Is_Declaration));
   end Java_ConstructAccess_getIsDeclarationInt;

   ------------------------------------
   -- Java_ConstructList_getFirstInt --
   ------------------------------------

   function Java_ConstructList_getFirstInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Constructs : constant Construct_List_Ptr := Construct_List_Ptr
        (Construct_List_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Constructs.First = null then
         return 0;
      else
         return To_Jint (Constructs.First.all'Address);
      end if;
   end Java_ConstructList_getFirstInt;

   --------------------------------------
   -- Java_ConstructList_getCurrentInt --
   --------------------------------------

   function Java_ConstructList_getCurrentInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Constructs : constant Construct_List_Ptr := Construct_List_Ptr
        (Construct_List_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Constructs.Current = null then
         return 0;
      else
         return To_Jint (Constructs.Current.all'Address);
      end if;
   end Java_ConstructList_getCurrentInt;

   -----------------------------------
   -- Java_ConstructList_getLastInt --
   -----------------------------------

   function Java_ConstructList_getLastInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Constructs : constant Construct_List_Ptr := Construct_List_Ptr
        (Construct_List_Conversion.To_Pointer (To_Address (Addr)));
   begin
      if Constructs.Last = null then
         return 0;
      else
         return To_Jint (Constructs.Last.all'Address);
      end if;
   end Java_ConstructList_getLastInt;

   --------------------------------
   -- Java_ConstructList_freeInt --
   --------------------------------

   procedure Java_ConstructList_freeInt
     (Env : JNIEnv; This : Jobject; Addr : Jint)
   is
      pragma Unreferenced (Env, This);
      Constructs : constant Construct_List_Ptr := Construct_List_Ptr
        (Construct_List_Conversion.To_Pointer (To_Address (Addr)));
   begin
      Free (Constructs.all);
   end Java_ConstructList_freeInt;

   ------------------------------------
   -- Java_SourceLocation_getLineInt --
   ------------------------------------

   function Java_SourceLocation_getLineInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Location : constant Source_Location_Ptr := Source_Location_Ptr
        (Source_Location_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return Jint (Location.Line);
   end Java_SourceLocation_getLineInt;

   --------------------------------------
   -- Java_SourceLocation_getColumnInt --
   --------------------------------------

   function Java_SourceLocation_getColumnInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Location : constant Source_Location_Ptr := Source_Location_Ptr
        (Source_Location_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return Jint (Location.Column);
   end Java_SourceLocation_getColumnInt;

   -------------------------------------
   -- Java_SourceLocation_getIndexInt --
   -------------------------------------

   function Java_SourceLocation_getIndexInt
     (Env : JNIEnv;
      This : Jobject;
      Addr : Jint)
      return Jint
   is
      pragma Unreferenced (Env, This);
      Location : constant Source_Location_Ptr := Source_Location_Ptr
        (Source_Location_Conversion.To_Pointer (To_Address (Addr)));
   begin
      return Jint (Location.Index);
   end Java_SourceLocation_getIndexInt;

end GNATbench.JNI_Functions;
