------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Tags;                          use Ada.Tags;

with Language.Debugger;                 use Language.Debugger;
with Language.Cpp;                      use Language.Cpp;
with GNATCOLL.Utils;                    use GNATCOLL.Utils;

with String_Utils;                      use String_Utils;
with GVD.Variables.Types;               use GVD.Variables.Types;
with GVD.Variables.Types.Records;       use GVD.Variables.Types.Records;
with GVD.Variables.Types.Classes;       use GVD.Variables.Types.Classes;
with GVD.Variables.Types.Simples;       use GVD.Variables.Types.Simples;

with Language.Debugger.Lldb.C;          use Language.Debugger.Lldb.C;
with Debugger.LLDB;                     use Debugger.LLDB;

package body Language.Debugger.Lldb.Cpp is

   procedure Parse_Class_Type
     (Lang      : access LLDB_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Result    : out GVD.Variables.Types.GVD_Type_Holder);
   --  Parse the description of a class type. Index should point after the
   --  keyword "class ".
   --  The output of gdb looks like
   --  class First_Class {
   --   public:
   --     int public_var;
   --   protected:
   --     int protected_var;
   --   private:
   --     int private_var;
   --
   --   public:
   --     First_Class & operator=(First_Class const &);
   --     First_Class(void);
   --  protected:
   --     void protected_func(void);
   --  }

   procedure Parse_Class_Contents
     (Lang     : access LLDB_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Is_Union : Boolean;
      Result   : out GVD.Variables.Types.GVD_Type_Holder)
     with Pre => Type_Str (Index - 1) = '{';
   --  Parse the contents of a class/union in C++ (ie the part after '{'
   --  Index should point to the character after '{'

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access LLDB_Cpp_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_Simple_Type (Cpp_Lang, Str);
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access LLDB_Cpp_Language)
      return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Cpp_Lang);
   end Keywords;

   overriding function Keywords
     (Lang : access LLDB_Cpp_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Cpp_Lang);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   overriding function Get_Language_Context
     (Lang : access LLDB_Cpp_Language) return Language.Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Language_Context (Cpp_Lang);
   end Get_Language_Context;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access LLDB_Cpp_Language) return Language.Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Explorer_Regexps (Cpp_Lang);
   end Explorer_Regexps;

   --------------------
   -- Is_System_File --
   --------------------

   overriding function Is_System_File
     (Lang : access LLDB_Cpp_Language; File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_System_File (Cpp_Lang, File_Name);
   end Is_System_File;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access LLDB_Cpp_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Dereference_Name (Cpp_Lang, Name);
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access LLDB_Cpp_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Array_Item_Name (Cpp_Lang, Name, Index);
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access LLDB_Cpp_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Record_Field_Name (Cpp_Lang, Name, Field);
   end Record_Field_Name;

   ----------------
   -- Parse_Type --
   ----------------

   overriding procedure Parse_Type
     (Lang     : access LLDB_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Tmp : constant Natural := Index;
   begin
      --  First check whether we have an access or array type.

      C_Detect_Composite_Type (Lang, Type_Str, Entity, Index, Result);
      if Result /= Empty_GVD_Type_Holder then
         return;
      end if;

      --  A class can have static members, that are reported as:
      --    type = class CL {\n
      --        private:\n
      --        static double x;\n
      --  Thus, we must ignore the static keyword

      if Looking_At (Type_Str, Tmp, "static ") then
         Index := Tmp + 7;
      else
         Index := Tmp;
      end if;

      --  Else handle specially two cases: classes, which are new to C++, and
      --  unions, since gdb prints a 'public:' indication at the
      --  beginning of the union (why ?)

      if Looking_At (Type_Str, Index, "class ")
         or else Looking_At (Type_Str, Index, "struct ")
      then
         Index := Index + 6; --  skips "class "
         Parse_Class_Type (Lang, Type_Str, Entity, Index, Result);

      elsif Looking_At (Type_Str, Index, "union ") then
         Index := Index + 6;
         Skip_To_Char (Type_Str, Index, '{');
         Index := Index + 1;
         Parse_Class_Contents (Lang, Type_Str, Entity, Index, True, Result);

      else
         C_Parse_Type (Lang, Type_Str, Entity, Index, Result);
      end if;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   overriding procedure Parse_Value
     (Lang       : access LLDB_Cpp_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive)
   is
      Ancestor   : Natural := 1;
      V          : GVD_Type_Holder;
      Num_Ancestors : Natural;

   begin
      pragma Assert (Result /= Empty_GVD_Type_Holder);
      pragma Assert (Index <= Type_Str'Last);

      --  Special handling for class types.
      --  As opposed to what happens in other languages, the result of "print"
      --  gives imbricated structs, as in
      --  "<Class2> = {<Class2> = {var1 = 1}, var2 = 4},
      --   <Class3> = {var3 = 10},
      --   var4 = 7}"

      if Result.Get_Type.all'Tag = GVD_Class_Type'Tag then
         if Type_Str (Index) = '{' then
            Index := Index + 1;
         end if;

         --  Parse the ancestors
         --  We cannot simply skip while we see a '<', with the following
         --  example returned by gdb:
         --     <CL2> = {_vptr.CL2 = 0x8049e68, x = 10}, <No data fields>}

         Num_Ancestors := GVD_Class_Type_Access
           (Result.Get_Type).Get_Num_Ancestors;
         while Ancestor <= Num_Ancestors
           and then Type_Str (Index) = '<'
         loop
            Skip_To_Char (Type_Str, Index, '>');
            Index := Index + 5;  --  skips "> = "
            V := GVD_Class_Type_Access
              (Result.Get_Type).Get_Ancestor (Ancestor);
            Parse_Value (Lang, Type_Str, Index, V, Repeat_Num);
            pragma Assert (Looking_At (Type_Str, Index, ", "));
            Index := Index + 2; --  skips ", "
            Ancestor := Ancestor + 1;
         end loop;

         --  Parse the child
         V := GVD_Class_Type_Access (Result.Get_Type).Get_Child;

         --  Recent versions of gdb display the virtual table as
         --     {_vptr.CLASS = 0xffff, a = 1}

         if Looking_At (Type_Str, Index, "_vptr.") then
            Index := Index + 6;
            while Type_Str (Index) /= ','
              and then Type_Str (Index) /= '}'
            loop
               Index := Index + 1;
            end loop;

            Index := Index + 1;
         end if;

         Internal_Parse_Value
           (Lang, Type_Str, Index, V, Repeat_Num,
            Parent => Result);

         --  If the class uses virtual methods, there is an extra field
         --  called '_vptr.' that should simply be skipped for now ???
         --     ", _vptr. = 0x8049b60 <X::B virtual table>"
         --
         --  We can also have the following format:
         --     ", _vptr. = 0x9049b60}, "    (ie no <..>)

         if Looking_At (Type_Str, Index, ", _vptr.")  then
            Index := Index + 11;
            while Type_Str (Index) /= '}' loop
               Index := Index + 1;
            end loop;
         end if;

         Repeat_Num := 1;

         if Index <= Type_Str'Last and then Type_Str (Index) = '}' then
            Index := Index + 1;
         end if;

      --  Else use the standard parsing functions

      else
         Internal_Parse_Value
           (Lang, Type_Str, Index, Result, Repeat_Num,
            Parent => Empty_GVD_Type_Holder);
      end if;
   end Parse_Value;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   overriding procedure Parse_Array_Type
     (Lang         : access LLDB_Cpp_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder) is
   begin
      C_Parse_Array_Type
        (Lang, Type_Str, Entity, Index, Start_Of_Dim, Result);
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   overriding procedure Parse_Record_Type
     (Lang      : access LLDB_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
      End_On    : String) is
   begin
      C_Parse_Record_Type
        (Lang, Type_Str, Entity, Index, Is_Union, Result, End_On);
   end Parse_Record_Type;

   --------------------------
   -- Parse_Class_Contents --
   --------------------------

   procedure Parse_Class_Contents
     (Lang     : access LLDB_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Is_Union : Boolean;
      Result   : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Num_Fields  : Natural := 0;
      Tmp         : Natural := Index;
      Tmp2        : Natural;
      Field       : Natural := 1;
      Field_Value : GVD_Type_Holder;
      Name_Start, Name_End, Field_End : Natural;

   begin
      --  Count the number of fields.
      --  Gdb first displays the fields (along with public:, protected: or
      --  private:), then a blank line, and the methods with their visibility.
      --
      --  If there is no blank line, there there is no field or no methods.

      while Tmp <= Type_Str'Last
        and then (Type_Str (Tmp) /= ASCII.LF
                  or else Type_Str (Tmp + 1) /= ASCII.LF)
      loop
         Tmp2 := Tmp;
         Skip_To_Char (Type_Str, Tmp, ';');

         --  This is a field if there are no parenthesis, unless we have a
         --  pointer to subprogram:
         --      void (*foo) ()  is a field
         --      void foo ()     is a method
         if Tmp <= Type_Str'Last then
            Skip_To_Char (Type_Str (Tmp2 .. Tmp - 1), Tmp2, '(');
            if Tmp2 >= Tmp - 1
              or else Type_Str (Tmp2 + 1) = '*'
            then
               Num_Fields := Num_Fields + 1;
            end if;
         end if;

         Tmp := Tmp + 1;
      end loop;

      if Is_Union then
         Result := New_Union_Type (Num_Fields);
      else
         Result := New_Record_Type (Num_Fields);
      end if;

      --  Parse the type
      while Field <= Num_Fields loop
         Skip_Blanks (Type_Str, Index);
         if Looking_At (Type_Str, Index, "public:") then
            Index := Index + 7;
         elsif Looking_At (Type_Str, Index, "protected:") then
            Index := Index + 10;
         elsif Looking_At (Type_Str, Index, "private:") then
            Index := Index + 8;
         end if;

         Skip_Blanks (Type_Str, Index);
         C_Field_Name
           (Lang, Entity, Type_Str, Index, Name_Start, Name_End,
            Field_End, Field_Value);
         GVD_Record_Type_Access (Result.Get_Type).Set_Field_Name
           (Field, Type_Str (Name_Start .. Name_End), Variant_Parts => 0);

         if Field_Value = Empty_GVD_Type_Holder then
            Field_Value := New_Simple_Type;
         end if;

         GVD_Record_Type_Access (Result.Get_Type).Set_Value
           (Field_Value, Field);

         Index := Field_End + 1;
         Field := Field + 1;
      end loop;
   end Parse_Class_Contents;

   ----------------------
   -- Parse_Class_Type --
   ----------------------

   procedure Parse_Class_Type
     (Lang      : access LLDB_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Result    : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Initial        : constant Natural := Index;
      Visibility     : Natural;
      Num_Ancestors  : Natural := 0;
      Tmp            : Natural := Index;
      Ancestor       : Natural := 1;
      Ancestor_Start : Natural;
      Child          : GVD_Type_Holder;

   begin
      pragma Assert (Index <= Type_Str'Last);

      --  Count the number of ancestors. Recent versions of gdb now output
      --  the class names as
      --     "class Namespace::class:ancestor {"

      while Type_Str (Tmp) /= '{' loop
         if Type_Str (Tmp) = ':' then
            if Type_Str (Tmp + 1) = ':' then
               Tmp := Tmp + 2;
            else
               Num_Ancestors := Num_Ancestors + 1;
               Tmp := Tmp + 1;
            end if;
         elsif Type_Str (Tmp) = ',' then
            Num_Ancestors := Num_Ancestors + 1;
            Tmp := Tmp + 1;
         else
            Tmp := Tmp + 1;
         end if;
      end loop;

      Result := New_Class_Type (Num_Ancestors => Num_Ancestors);

      --  Set all the ancestors

      Tmp := Index;
      Ancestor_Start := Tmp;
      Visibility := Tmp;

      while Ancestor <= Num_Ancestors loop
         if Type_Str (Tmp) = ',' or else Type_Str (Tmp) = '{' then
            declare
               Ancestor_Name : constant String :=
                 Type_Str (Ancestor_Start .. Tmp - 1);
               Ancestor_Type : constant String :=
                 Type_Of (Get_Debugger (Lang), Ancestor_Name);
               Tmp2          : Natural := Ancestor_Type'First;
               Parent        : GVD_Type_Holder;

            begin
               Parse_Type
                 (Lang, Ancestor_Type, Ancestor_Name, Tmp2, Parent);
               GVD_Class_Type_Access (Result.Get_Type).Add_Ancestor
                 (Ancestor, Parent);
               Parent.Get_Type.Set_Type_Name
                 (Type_Str (Visibility .. Tmp - 1));
               Ancestor := Ancestor + 1;
            end;
         end if;

         if Type_Str (Tmp) = ':' or else Type_Str (Tmp) = ',' then
            Tmp := Tmp + 2;
            Visibility := Tmp;

            if Looking_At (Type_Str, Tmp, "public") then
               Tmp := Tmp + 7;
            elsif Looking_At (Type_Str, Tmp, "protected") then
               Tmp := Tmp + 10;
            elsif Looking_At (Type_Str, Tmp, "private") then
               Tmp := Tmp + 8;
            elsif Looking_At (Type_Str, Tmp, "virtual") then
               Tmp := Tmp + 8;
            end if;

            Ancestor_Start := Tmp;

         else
            Tmp := Tmp + 1;
         end if;
      end loop;

      --  Count the fields. Stop at the first empty line, since this is the
      --  separation between fields and methods.

      Skip_To_Char (Type_Str, Index, '{');
      Index := Index + 1;
      Parse_Class_Contents (Lang, Type_Str, Entity, Index, False, Child);
      GVD_Class_Type_Access (Result.Get_Type).Set_Child (Child);

      --  Get the type name
      Tmp := Initial;
      Skip_Word (Type_Str, Tmp);
      Child.Get_Type.Set_Type_Name (Type_Str (Initial .. Tmp - 1));
   end Parse_Class_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   overriding procedure Parse_Array_Value
     (Lang     : access LLDB_Cpp_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out GVD.Variables.Types.GVD_Type_Holder)
   is
      Lang_C : aliased LLDB_C_Language;
   begin
      Set_Debugger (Lang_C'Access, Get_Debugger (Lang));
      Parse_Array_Value
        (Lang_C'Access, Type_Str, Index, Result);
   end Parse_Array_Value;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   overriding function Get_Language_Debugger_Context
     (Lang : access LLDB_Cpp_Language) return Language_Debugger_Context
   is
      pragma Unreferenced (Lang);

      Lang_C : aliased LLDB_C_Language;
   begin
      return Get_Language_Debugger_Context (Lang_C'Access);
   end Get_Language_Debugger_Context;

   ------------------
   -- Set_Variable --
   ------------------

   overriding function Set_Variable
     (Lang     : access LLDB_Cpp_Language;
      Var_Name : String;
      Value    : String) return String
   is
      pragma Unreferenced (Lang);

      Lang_C : aliased LLDB_C_Language;
   begin
      return Set_Variable (Lang_C'Access, Var_Name, Value);
   end Set_Variable;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Lang : access LLDB_Cpp_Language) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "c++";
   end Get_Name;

end Language.Debugger.Lldb.Cpp;
