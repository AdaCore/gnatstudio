-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  ??? Must add support for "show print vtbl"

with Language.Debugger; use Language.Debugger;
with Language.Cpp;      use Language.Cpp;

with String_Utils;   use String_Utils;
with Items;          use Items;
with Items.Arrays;   use Items.Arrays;
with Items.Records;  use Items.Records;
with Items.Classes;  use Items.Classes;

with Ada.Tags;       use Ada.Tags;
with Debugger.Gdb.C; use Debugger.Gdb.C;

package body Debugger.Gdb.Cpp is

   use Language;

   procedure Parse_Class_Type
     (Lang      : access Gdb_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Result    : out Generic_Type_Access);
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
     (Lang     : access Gdb_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Is_Union : Boolean;
      Result   : out Generic_Type_Access);
   --  Parse the contents of a class/union in C++ (ie the part after '{'
   --  Index should point to the character after '{'
   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Gdb_Cpp_Language; Str : String) return Boolean is
   begin
      return Is_Simple_Type (Cpp_Lang, Str);
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Gdb_Cpp_Language) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Keywords (Cpp_Lang);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Gdb_Cpp_Language) return Language.Language_Context is
   begin
      return Get_Language_Context (Cpp_Lang);
   end Get_Language_Context;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Gdb_Cpp_Language) return Language.Explorer_Categories is
   begin
      return Explorer_Regexps (Cpp_Lang);
   end Explorer_Regexps;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang : access Gdb_Cpp_Language; File_Name : String) return Boolean is
   begin
      return Is_System_File (Cpp_Lang, File_Name);
   end Is_System_File;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Gdb_Cpp_Language;
      Name : String) return String is
   begin
      return Dereference_Name (Cpp_Lang, Name);
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Gdb_Cpp_Language;
      Name  : String;
      Index : String) return String is
   begin
      return Array_Item_Name (Cpp_Lang, Name, Index);
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Gdb_Cpp_Language;
      Name  : String;
      Field : String) return String is
   begin
      return Record_Field_Name (Cpp_Lang, Name, Field);
   end Record_Field_Name;

   ---------------------
   -- Break Exception --
   ---------------------

   function Break_Exception
     (Debugger  : access Gdb_Cpp_Language;
      Name      : String  := "";
      Unhandled : Boolean := False) return String is
   begin
      if Name = "" then
         return "break __raise_exception";
      else
         return "catch " & Name;
      end if;
   end Break_Exception;

   ----------------
   -- Parse_Type --
   ----------------

   procedure Parse_Type
     (Lang     : access Gdb_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access)
   is
      Tmp : constant Natural := Index;
   begin
      --  First check whether we have an access or array type.

      C_Detect_Composite_Type (Lang, Type_Str, Entity, Index, Result);
      if Result /= null then
         return;
      end if;

      --  Else handle specially two cases: classes, which are new to C++, and
      --  unions, since gdb prints a 'public:' indication at the
      --  beginning of the union (why ?)

      if Looking_At (Type_Str, Tmp, "class ") then
         Index := Tmp + 6; --  skips "class "
         Parse_Class_Type (Lang, Type_Str, Entity, Index, Result);

      elsif Looking_At (Type_Str, Tmp, "union ") then
         Index := Tmp + 6;
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

   procedure Parse_Value
     (Lang       : access Gdb_Cpp_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Type_Access;
      Repeat_Num : out Positive)
   is
      Ancestor   : Natural := 1;
      V          : Generic_Type_Access;

   begin
      pragma Assert (Result /= null);
      pragma Assert (Index <= Type_Str'Last);

      --  Special handling for class types.
      --  As opposed to what happens in other languages, the result of "print"
      --  gives imbricated structs, as in
      --  "<Class2> = {<Class2> = {var1 = 1}, var2 = 4},
      --   <Class3> = {var3 = 10},
      --   var4 = 7}"

      if Result'Tag = Class_Type'Tag then
         if Type_Str (Index) = '{' then
            Index := Index + 1;
         end if;

         --  Parse the ancestors
         while Type_Str (Index) = '<' loop
            Skip_To_Char (Type_Str, Index, '>');
            Index := Index + 5;  --  skips "> = "
            V := Get_Ancestor (Class_Type (Result.all), Ancestor);
            Parse_Value (Lang, Type_Str, Index, V, Repeat_Num);
            pragma Assert (Looking_At (Type_Str, Index, ", "));
            Index := Index + 2; --  skips ", "
            Ancestor := Ancestor + 1;
         end loop;

         --  Parse the child
         V := Get_Child (Class_Type (Result.all));
         Internal_Parse_Value
           (Lang, Type_Str, Index, V, Repeat_Num,
            Parent => Result);

         --  If the class uses virtual methods, there is an extra field
         --  called '_vptr.' that should simply be skipped for now ???
         --     ", _vptr. = 0x8049b60 <X::B virtual table>"

         if Looking_At (Type_Str, Index, ", _vptr. = ") then
            Index := Index + 11;
            Skip_To_Char (Type_Str, Index, '>');
            Index := Index + 1;
         end if;

         Repeat_Num := 1;

         if Index <= Type_Str'Last and then Type_Str (Index) = '}' then
            Index := Index + 1;
         end if;

      --  Else use the standard parsing functions

      else
         Internal_Parse_Value
           (Lang, Type_Str, Index, Result, Repeat_Num, Parent => null);
      end if;
   end Parse_Value;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   procedure Parse_Array_Type
     (Lang      : access Gdb_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Generic_Type_Access) is
   begin
      C_Parse_Array_Type
        (Lang, Type_Str, Entity, Index, Start_Of_Dim, Result);
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   procedure Parse_Record_Type
     (Lang      : access Gdb_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Generic_Type_Access;
      End_On    : String) is
   begin
      C_Parse_Record_Type
        (Lang, Type_Str, Entity, Index, Is_Union, Result, End_On);
   end Parse_Record_Type;

   --------------------------
   -- Parse_Class_Contents --
   --------------------------

   procedure Parse_Class_Contents
     (Lang     : access Gdb_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Is_Union : Boolean;
      Result   : out Generic_Type_Access)
   is
      Num_Fields  : Natural := 0;
      Tmp         : Natural := Index;
      Field       : Natural := 1;
      Field_Value : Generic_Type_Access := null;
      Name_Start, Name_End, Field_End : Natural;

   begin
      pragma Assert (Result = null);
      pragma Assert (Type_Str (Index - 1) = '{');

      --  Count the number of fields.
      --  Gdb first displays the fields (along with public:, protected: or
      --  private:), then a blank line, and the methods with their visibility.
      --  If there is no blank line, there there is no field.

      while Tmp <= Type_Str'Last
        and then (Type_Str (Tmp) /= ASCII.LF
                  or else Type_Str (Tmp + 1) /= ASCII.LF)
      loop
         Skip_To_Char (Type_Str, Tmp, ';');
         Num_Fields := Num_Fields + 1;
         Tmp := Tmp + 1;
      end loop;

      --  No field ?
      if Tmp > Type_Str'Last then
         Num_Fields := 0;
      end if;

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
         Set_Field_Name
           (Record_Type (Result.all), Field,
            Type_Str (Name_Start .. Name_End), Variant_Parts => 0);
         Set_Value (Record_Type (Result.all), Field_Value, Field);

         Index := Field_End + 1;
         Field := Field + 1;
      end loop;
   end Parse_Class_Contents;

   ----------------------
   -- Parse_Class_Type --
   ----------------------

   procedure Parse_Class_Type
     (Lang      : access Gdb_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Result    : out Generic_Type_Access)
   is
      Initial        : constant Natural := Index;
      Visibility     : Natural;
      Num_Ancestors  : Natural := 0;
      Tmp            : Natural := Index;
      Ancestor       : Natural := 1;
      Ancestor_Start :  Natural;
      Child          : Generic_Type_Access;

   begin
      pragma Assert (Index <= Type_Str'Last);

      --  Count the number of ancestors

      while Type_Str (Tmp) /= '{' loop
         if Type_Str (Tmp) = ':' or else Type_Str (Tmp) = ',' then
            Num_Ancestors := Num_Ancestors + 1;
         end if;
         Tmp := Tmp + 1;
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
               Parent        : Generic_Type_Access;

            begin
               Parse_Type
                 (Lang, Ancestor_Type, Ancestor_Name, Tmp2, Parent);
               Add_Ancestor
                 (Class_Type (Result.all), Ancestor,
                  Class_Type_Access (Parent));
               Set_Type_Name (Parent, Type_Str (Visibility .. Tmp - 1));
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
      Set_Child (Class_Type (Result.all), Record_Type_Access (Child));

      --  Get the type name
      Tmp := Initial;
      Skip_Word (Type_Str, Tmp);
      Set_Type_Name (Child, Type_Str (Initial .. Tmp - 1));
   end Parse_Class_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   procedure Parse_Array_Value
     (Lang     : access Gdb_Cpp_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
      Lang_C : aliased Gdb_C_Language;
   begin
      Set_Debugger (Lang_C'Access, Get_Debugger (Lang));
      Parse_Array_Value
        (Lang_C'Access, Type_Str, Index, Result);
   end Parse_Array_Value;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   function Get_Language_Debugger_Context
     (Lang : access Gdb_Cpp_Language) return Language_Debugger_Context
   is
      Lang_C : aliased Gdb_C_Language;
   begin
      return Get_Language_Debugger_Context (Lang_C'Access);
   end Get_Language_Debugger_Context;

   ------------------
   -- Set_Variable --
   ------------------

   function Set_Variable
     (Lang     : access Gdb_Cpp_Language;
      Var_Name : String;
      Value    : String) return String
   is
      Lang_C : aliased Gdb_C_Language;
   begin
      return Set_Variable (Lang_C'Access, Var_Name, Value);
   end Set_Variable;

   -----------
   -- Start --
   -----------

   function Start (Debugger : access Gdb_Cpp_Language) return String is
      Lang_C : aliased Gdb_C_Language;
   begin
      return Start (Lang_C'Access);
   end Start;

end Debugger.Gdb.Cpp;
