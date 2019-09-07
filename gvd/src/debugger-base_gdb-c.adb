------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GNATCOLL.Utils;                  use GNATCOLL.Utils;
with String_Utils;                    use String_Utils;
with Language.Debugger;               use Language.Debugger;
with Language.C;                      use Language.C;

with GVD.Variables.Types;             use GVD.Variables.Types;
with GVD.Variables.Types.Simples;     use GVD.Variables.Types.Simples;
with GVD.Variables.Types.Arrays;      use GVD.Variables.Types.Arrays;
with GVD.Variables.Types.Records;     use GVD.Variables.Types.Records;

package body Debugger.Base_Gdb.C is

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Gdb_C_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_Simple_Type (C_Lang, Str);
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Gdb_C_Language)
      return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (C_Lang);
   end Keywords;

   overriding function Keywords
     (Lang : access Gdb_C_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (C_Lang);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   overriding function Get_Language_Context
     (Lang : access Gdb_C_Language) return Language.Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Language_Context (C_Lang);
   end Get_Language_Context;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access Gdb_C_Language) return Language.Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Explorer_Regexps (C_Lang);
   end Explorer_Regexps;

   --------------------
   -- Is_System_File --
   --------------------

   overriding function Is_System_File
     (Lang : access Gdb_C_Language; File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_System_File (C_Lang, File_Name);
   end Is_System_File;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access Gdb_C_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Dereference_Name (C_Lang, Name);
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access Gdb_C_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Array_Item_Name (C_Lang, Name, Index);
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access Gdb_C_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Record_Field_Name (C_Lang, Name, Field);
   end Record_Field_Name;

   -----------------------------
   -- C_Detect_Composite_Type --
   -----------------------------

   procedure C_Detect_Composite_Type
     (Lang     : access Language.Debugger.Language_Debugger'Class;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Tmp  : Natural := Index;
      Save : Natural := Index;
      Last : Natural := Type_Str'Last;

   begin
      --  First: Skip the type itself, to check whether we have in fact an
      --  array or access type.
      --  Several cases to be considered here:
      --     unsigned int foo;
      --     int foo[5];    (as part of a struct field)
      --     struct { ... } foo;
      --     struct foo a;
      --     struct foo *a;  --  need to see the access type!
      --  Thus we have to skip even several words

      --  Skip to the right-most access or array definition
      --  For instance, when looking at 'int* [4]' we should detect an array
      --  type, not an access type.

      while Index <= Last loop
         --  Access type ?
         if Type_Str (Index) = '*' then
            Save := Index;
            Index := Index + 1;

         --  Array type ?
         elsif Type_Str (Index) = '[' then
            --  Leave Index at the beginning of a multi-dimensional array, as
            --  in 'int [2][3]'.
            if Type_Str (Save) /= '[' then
               Save := Index;
            end if;
            Skip_To_Char (Type_Str, Index, ']');
            Index := Index + 1;

         --  Begin of union or struct => skip to the end
         elsif Type_Str (Index) = '{' then
            declare
               Count : Natural := 1;
            begin
               Index := Index + 1;

               while Count /= 0 and then Index < Type_Str'Last loop
                  if Type_Str (Index) = '}' then
                     Count := Count - 1;
                  elsif Type_Str (Index) = '{' then
                     Count := Count + 1;
                  end if;

                  Index := Index + 1;
               end loop;
            end;

         --  Access to subprogram
         elsif Type_Str (Index) = '(' then

            if Type_Str (Index + 1) = '*' then
               Index := Index + 1;
            end if;
            Save := Index;

            --  Skip the field name (if any), as in: "void (*field1[2])();"
            --  or "void (*[2])()" for a type defined as "void (*asa[2]) ();"
            --  The parsing should stop after the closing parenthesis.
            while Index <= Type_Str'Last
              and then Type_Str (Index) /= ')'
              and then Type_Str (Index) /= '['
            loop
               Index := Index + 1;
            end loop;

            Last := Index;
            while Last <= Type_Str'Last
              and then Type_Str (Last) /= ')'
            loop
               Last := Last + 1;
            end loop;
            Last := Last - 1;

         else
            Skip_CPP_Token (Type_Str, Index);
         end if;

         Skip_Blanks (Type_Str, Index);
      end loop;

      Index := Save;

      --  An access type ?
      --  or access to subprogram

      if Index <= Type_Str'Last
        and then (Type_Str (Index) = '*' or else Type_Str (Index) = '(')
      then
         Index := Index + 1;
         Result := New_Access_Type;

         --  Even though doing a Get_Type_Info is more costly, we must do
         --  it, since otherwise it is hard to get the real type name directly
         --  from the general Type_Str (see  void (*field[2])(int a))

         Result.Get_Type.Set_Type_Name
           (Get_Type_Info
              (Lang.Get_Debugger, Entity,
               Type_Str (Type_Str'First .. Index - 1)));

      --  An array type ?

      elsif Index < Type_Str'Last and then Type_Str (Index) = '[' then
         Parse_Array_Type (Lang, Type_Str, Entity, Tmp, Index, Result);
         Index := Tmp;

      else
         Result := Empty_GVD_Type_Holder;
         Index  := Tmp;
      end if;
   end C_Detect_Composite_Type;

   ------------------
   -- C_Parse_Type --
   ------------------

   procedure C_Parse_Type
     (Lang     : access Language.Debugger.Language_Debugger'Class;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Context : constant Language_Debugger_Context :=
        Get_Language_Debugger_Context (Lang);
      Tmp     : Natural := Index;
      Start   : constant Natural := Index;

   begin
      if Looking_At (Type_Str, Index, "const ") then
         Index := Index + 6;
      elsif Looking_At (Type_Str, Index, "volatile ") then
         Index := Index + 9;
      end if;

      C_Detect_Composite_Type (Lang, Type_Str, Entity, Index, Result);

      --  Do we have an access or array type ?
      if Result /= Empty_GVD_Type_Holder then
         return;
      end if;

      --  Else a simple type
      if Index in Type_Str'Range then
         case Type_Str (Index) is
            when '<' =>
               --  Simple types, like <4-byte integer> and <4-byte float>
               Skip_To_Char (Type_Str, Index, '>');
               Result := New_Simple_Type;
               Result.Get_Type.Set_Type_Name (Type_Str (Tmp .. Index));
               Index := Index + 1;
               return;

            when 'e' =>
               --  Enumeration type.
               --  Either this is the type itself "enum {....}", or this is the
               --  field of a struct or union "enum name;".

               if Looking_At (Type_Str, Index, "enum ") then
                  if Type_Str (Index + 5) = '{' then
                     --  We are in the "enum {....}" case
                     Result := New_Enum_Type;
                     Result.Get_Type.Set_Type_Name ("enum");
                     return;
                  end if;

                  Index := Index + 5;
                  Skip_CPP_Token (Type_Str, Index);  --  skips enum name
                  Index := Index + 1;

                  if Index <= Type_Str'Last
                    and then Type_Str (Index) = '{'
                  then
                     Skip_To_Char (Type_Str, Index, '}');
                     Index := Index + 1;
                  end if;

                  Result := New_Enum_Type;
                  Result.Get_Type.Set_Type_Name
                    (Type_Str (Type_Str'First .. Index - 1));
                  return;
               end if;

               --  Else falls through

            when 'r' =>
               --  Range types

               if Looking_At (Type_Str, Index, "range ") then
                  declare
                     Min, Max : Long_Integer;
                  begin
                     Index := Index + 6;
                     Parse_Num (Type_Str, Index, Min);
                     Index := Index + 4; --  skips ' .. '
                     Parse_Num (Type_Str, Index, Max);
                     Result := New_Range_Type (Min, Max);
                     Result.Get_Type.Set_Type_Name
                       (Type_Str (Start .. Index - 1));
                     return;
                  end;
               end if;

            when 's' =>
               --  Structures.
               --  There are several possible cases here:
               --      "struct My_Record { ... }"
               --   or "struct My_Record a"
               --   or "struct { ... } a"   (anonymous types)
               --  The second case needs a further ptype to get the real
               --  definition.

               if Looking_At (Type_Str, Index, "struct ") then
                  Tmp   := Index;
                  Index := Index + 7;           --  skips "struct "

                  if Type_Str (Index) /= Context.Record_Start then
                     Skip_CPP_Token (Type_Str, Index);  --  skips struct name
                     while Index + 1 <= Type_Str'Last
                       and then Type_Str (Index .. Index + 1) = "::"
                     loop
                        Index := Index + 2;
                        Skip_CPP_Token (Type_Str, Index);
                     end loop;
                  end if;

                  Skip_Blanks (Type_Str, Index);

                  if Index <= Type_Str'Last
                    and then Type_Str (Index) = Context.Record_Start
                  then
                     Index := Index + 1;
                     Parse_Record_Type
                       (Lang, Type_Str, Entity, Index,
                        Is_Union => False, Result => Result, End_On => "}");

                  else
                     Result := Parse_Type
                       (Get_Debugger (Lang), Type_Str (Tmp .. Index - 1));
                  end if;

                  return;
               end if;
               --  Else falls through

            when 'u' =>
               if Looking_At (Type_Str, Index, "union ") then
                  Tmp   := Index;
                  Index := Index + 6;           --  skips "union "

                  if Type_Str (Index) /= Context.Record_Start then
                     Skip_CPP_Token (Type_Str, Index);  --  skips union name
                  end if;

                  Skip_Blanks (Type_Str, Index);

                  if Index <= Type_Str'Last
                    and then Type_Str (Index) = Context.Record_Start
                  then
                     Index := Index + 1;
                     Parse_Record_Type
                       (Lang, Type_Str, Entity, Index, Is_Union => True,
                        Result => Result, End_On => "}");

                  else
                     Result := Parse_Type
                       (Get_Debugger (Lang), Type_Str (Tmp .. Index - 1));
                  end if;

                  return;
               end if;
               --  Else falls through

            when others =>
               null;
         end case;
      end if;

      --  Do we have a simple type ?

      if Is_Simple_Type (Lang, Type_Str (Tmp .. Type_Str'Last)) then
         Result := New_Simple_Type;
         Result.Get_Type.Set_Type_Name (Type_Str (Tmp .. Type_Str'Last));
         return;
      end if;

      --  Else ask for more information
      --  This is needed when Type_Str didn't start with a known keyword,
      --  like "__time_t" for instance

      declare
         Ent : constant String := Type_Str (Tmp .. Type_Str'Last);
         T   : constant String := Type_Of (Get_Debugger (Lang), Ent);
         J   : Natural := T'First;

      begin
         if T = Type_Str then
            --  Also a simple (unknown) type

            Result := New_Simple_Type;
            Result.Get_Type.Set_Type_Name (Ent);
            return;
         end if;

         --  In some cases, T might have a null length (for instance if we
         --  had a C++ class, since Ent="class" and T="" in that case).
         if T'Length /= 0 then
            Parse_Type (Lang, T, Ent, J, Result);
         else
            Result := Empty_GVD_Type_Holder;
         end if;

         Index := Type_Str'Last;
      end;
   end C_Parse_Type;

   ----------------
   -- Parse_Type --
   ----------------

   overriding procedure Parse_Type
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder) is
   begin
      C_Parse_Type (Lang, Type_Str, Entity, Index, Result);
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   overriding procedure Parse_Value
     (Lang       : access Gdb_C_Language;
      Entity     : String;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive) is
   begin
      Internal_Parse_Value
        (Lang, Entity, Type_Str, Index, Result, Repeat_Num,
         Parent => Empty_GVD_Type_Holder);
   end Parse_Value;

   ------------------------
   -- C_Parse_Array_Type --
   ------------------------

   procedure C_Parse_Array_Type
     (Lang         : access Language.Debugger.Language_Debugger'Class;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Num_Dim   : Integer := 0;
      Initial   : Natural := Index;
      Tmp_Index : Natural;
      R         : GVD.Variables.Types.GVD_Type_Holder;
      Last      : Long_Integer;
      Item_Type : GVD.Variables.Types.GVD_Type_Holder;

   begin
      --  Find the number of dimensions

      Index := Start_Of_Dim;
      Tmp_Index := Index;

      while Tmp_Index <= Type_Str'Last
        and then Type_Str (Tmp_Index) = '['
      loop
         Num_Dim := Num_Dim + 1;
         Skip_To_Char (Type_Str, Tmp_Index, ']');
         Tmp_Index := Tmp_Index + 1;
      end loop;

      --  Create the type

      Result := New_Array_Type (Num_Dimensions => Num_Dim);
      R := Result;
      R.Get_Type.Set_Type_Name
        (Get_Type_Info (Lang.Get_Debugger, Entity, Type_Str));

      --  Then parse the dimensions.
      Num_Dim := 0;

      while Index <= Type_Str'Last
        and then Type_Str (Index) = '['
      loop
         Num_Dim := Num_Dim + 1;
         Index := Index + 1;
         Parse_Num (Type_Str, Index, Last);
         GVD_Array_Type_Access (R.Get_Type).Set_Dimensions
           (Num_Dim, (0, Last - 1));
         Index := Index + 1;
      end loop;

      --  Finally parse the type of items.
      --  Note that we have to delete the name of the field, so that simple
      --  types can be correctly detected (from "int foo" to "int"). This
      --  needs to be done only if we are parsing a struct field, not a normal
      --  array.

      Tmp_Index := Start_Of_Dim - 1;
      if Type_Str (Tmp_Index) = ' ' then
         Tmp_Index := Tmp_Index - 1;
      end if;

      Parse_Type
        (Lang,
         Type_Str (Initial .. Tmp_Index),
         Array_Item_Name (Lang, Entity, "0"),
         Initial,
         Item_Type);
      GVD_Array_Type_Access (R.Get_Type).Set_Item_Type (Item_Type);
   end C_Parse_Array_Type;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   overriding procedure Parse_Array_Type
     (Lang         : access Gdb_C_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder)
   is
   begin
      C_Parse_Array_Type (Lang, Type_Str, Entity, Index, Start_Of_Dim, Result);
   end Parse_Array_Type;

   ------------------
   -- C_Field_Name --
   ------------------

   procedure C_Field_Name
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Entity     : String;
      Type_Str   : String;
      Index      : Natural;
      Name_Start : out Natural;
      Name_End   : out Natural;
      Field_End  : out Natural;
      Result     : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Tmp            : Natural := Index;
      Semi_Colon_Pos : Natural;
      Count          : Natural;
      Force_Method   : Boolean := False;

   begin
      --  Get the field name (last word before ;)
      --  There is a small exception here for access-to-subprograms fields,
      --  which look like "void (*field1[2])(void);"
      --  Whereas older versions of gdb were never specifying the parameters,
      --  thus printing only (), more recent versions now display them.
      --  We also skip embedded unions or structs ("struct foo {..}" or
      --  "struct {...}" completly).

      if Looking_At (Type_Str, Tmp, "struct {")
        or else Looking_At (Type_Str, Tmp, "union {")
        or else Looking_At (Type_Str, Tmp, "volatile struct {")
        or else Looking_At (Type_Str, Tmp, "volatile union {")
        or else Looking_At (Type_Str, Tmp, "const struct {")
        or else Looking_At (Type_Str, Tmp, "const union {")
      then
         Skip_To_Char (Type_Str, Tmp, '{');
         Tmp := Tmp + 1;
         Count := 1;

         while Count /= 0 and then Tmp < Type_Str'Last loop
            if Type_Str (Tmp) = '{' then
               Count := Count + 1;
            elsif Type_Str (Tmp) = '}' then
               Count := Count - 1;
            end if;

            Tmp := Tmp + 1;
         end loop;
      end if;

      Skip_To_Char (Type_Str, Tmp, ';');
      Semi_Colon_Pos := Tmp;
      Field_End := Tmp;
      Tmp := Tmp - 1;

      --  This is probably a pointer to subprogram, as in:
      --     void (*foo) (void)
      --  Some recent versions of gdb now display:
      --     void foo(void);
      --  instead

      if Type_Str (Tmp) = ')' then
         Skip_To_Char (Type_Str, Tmp, '(', Step => -1);
         Tmp := Tmp - 1;
         Skip_Blanks_Backward (Type_Str, Tmp);

         if Type_Str (Tmp) = ')' then
            --  Skip array definition if any
            if Type_Str (Tmp - 1) = ']' then
               Skip_To_Char (Type_Str, Tmp, '[', Step => -1);
            end if;
            Name_End := Tmp - 1;
            Skip_To_Char (Type_Str, Tmp, '*', Step => -1);
         else
            Name_End := Tmp;
            Skip_Word (Type_Str, Tmp, Step => -1);
            Force_Method := True;
         end if;

      else
         Name_End := Field_End - 1;

         --  Skip array definition as in
         --  "  GdkColor fg[5];"

         while Type_Str (Tmp) = ']' loop
            Skip_To_Char (Type_Str, Tmp, '[', Step => -1);
            Tmp := Tmp - 1;
            Name_End := Tmp;
         end loop;

         --  The size of the field can optionally be indicated between the
         --  name and the semicolon, as in "__time_t tv_sec : 32;".
         --  We simply ignore the size.
         --  However, in c++ mode the name can also be
         --     "My_Record::My_Field1_Type field"

         Skip_CPP_Token (Type_Str, Tmp, Step => -1);
         if Type_Str (Tmp - 1) = ':' then
            Field_End := Tmp - 2;
            Tmp := Tmp - 3;
            Name_End := Tmp;
            Skip_CPP_Token (Type_Str, Tmp, Step => -1);
         end if;
      end if;
      Name_Start := Tmp + 1;

      --  Avoid some calls to ptype if possible. Note that we have to get
      --  rid of the field's name before calling Is_Simple_Type, since
      --  otherwise "int a" is not recognized as a simple type.
      --  We also need to handle properly cases of "int a[2]". The only
      --  solution here is to remove the field name from the string before
      --  calling recursively C_Detect_Composite_Type, or simple types like
      --  the above generate an extra ptype for "int".

      Tmp := Index;

      if Force_Method then
         C_Detect_Composite_Type
           (Lang, Type_Str (Index .. Name_Start - 1) & "(*)"
            & Type_Str (Name_End + 1 .. Field_End - 1),
            Entity, Tmp, Result);
      else
         C_Detect_Composite_Type
           (Lang, Type_Str (Index .. Name_Start - 1)
            & Type_Str (Name_End + 1 .. Field_End - 1),
            Entity, Tmp, Result);
      end if;

      --  if not an access or array:
      if Result = Empty_GVD_Type_Holder then
         if Is_Simple_Type (Lang, Type_Str (Index .. Name_Start - 2)) then
            Result := New_Simple_Type;
            Result.Get_Type.Set_Type_Name (Type_Str (Index .. Field_End - 1));
         else
            declare
               --  We query the type of the field by the field name, instead of
               --  relying on the type name itsef. A problem occurs otherwise
               --  with nested records, such as:
               --     struct My_Record_Of_Record {
               --        struct Field1_Record { int a; int* b} c;
               --        int d;
               --     };
               --     struct My_Record_Of_Record Mror;
               --  querying the type for "struct Field1_Record" would fail,
               --  whereas querying the type for Mror.c works as expected

               T : constant String := Type_Of
                 (Get_Debugger (Lang),
                  "(" & Entity & ")" & "." &
                  Type_Str (Name_Start .. Name_End));
            begin
               Tmp := T'First;
               Parse_Type
                 (Lang,
                  Type_Str =>  T,
                  Entity   => Record_Field_Name
                    (Lang, Entity, Type_Str (Name_Start .. Name_End)),
                  Index    => Tmp,
                  Result   => Result);
            end;
         end if;
      end if;

      Field_End := Semi_Colon_Pos;
   end C_Field_Name;

   -------------------------
   -- C_Parse_Record_Type --
   -------------------------

   procedure C_Parse_Record_Type
     (Lang      : access Language.Debugger.Language_Debugger'Class;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
      End_On    : String)
   is
      pragma Unreferenced (End_On);

      Num_Fields        : Natural := 0;
      Field             : Natural := 1;
      Initial           : constant Natural := Index;
      R                 : GVD_Type_Holder;
      Field_Value       : GVD_Type_Holder;
      Tmp               : Natural;
      End_Of_Name, Save : Natural;

   begin
      --  Count the number of fields

      while Index <= Type_Str'Last
        and then Type_Str (Index) /= '}'
      loop
         --  Embedded unions or structs

         if Type_Str (Index) = '{' then
            Tmp := 1;

            while Tmp /= 0 and then Index < Type_Str'Last loop
               Index := Index + 1;

               if Type_Str (Index) = '{' then
                  Tmp := Tmp + 1;
               elsif Type_Str (Index) = '}' then
                  Tmp := Tmp - 1;
               end if;
            end loop;

         elsif Type_Str (Index) = ';' then
            Num_Fields := Num_Fields + 1;
         end if;

         Index := Index + 1;
      end loop;

      --  Create the type

      if Is_Union then
         Result := New_Union_Type (Num_Fields);
      else
         Result := New_Record_Type (Num_Fields);
      end if;

      R := Result;
      GVD_Record_Type_Access (R.Get_Type).Set_Type_Name
        (Type_Str (Type_Str'First .. Initial - 2));

      --  Parse the type

      Index := Initial;

      while Field <= Num_Fields loop
         Skip_Blanks (Type_Str, Index);

         C_Field_Name
           (Lang, Entity,
            Type_Str, Index, Tmp, End_Of_Name, Save, Field_Value);
         if Field_Value = Empty_GVD_Type_Holder then
            Result := Empty_GVD_Type_Holder;
            return;
         end if;

         GVD_Record_Type_Access (R.Get_Type).Set_Field_Name
           (Field, Type_Str (Tmp .. End_Of_Name), Variant_Parts => 0);
         GVD_Record_Type_Access (R.Get_Type).Set_Value (Field_Value, Field);
         Index := Save + 1;
         Field := Field + 1;
      end loop;
   end C_Parse_Record_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   overriding procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
      End_On    : String)
   is
   begin
      C_Parse_Record_Type
        (Lang, Type_Str, Entity, Index, Is_Union, Result, End_On);
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   overriding procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out GVD.Variables.Types.GVD_Type_Holder)
   is
      Dim           : Natural      := 0; --  current dimension
      Current_Index : Long_Integer := 0; --  Current index in the parsed array
      Int           : Natural;

      procedure Parse_Item;
      --  Parse the value of a single item, and add it to the contents of
      --  Result.

      ----------------
      -- Parse_Item --
      ----------------

      procedure Parse_Item is
         Tmp         : GVD_Type_Holder;
         Repeat_Num  : Integer;
         Start_Index : constant Natural := Index;

      begin
         --  Parse the next item
         Tmp := GVD_Array_Type_Access
           (Result.Get_Type).Get_Value (Current_Index);

         if Tmp = Empty_GVD_Type_Holder then
            Tmp := GVD_Array_Type_Access (Result.Get_Type).Get_Item_Type.Clone;
         end if;

         Internal_Parse_Value
           (Lang, "", Type_Str, Index, Tmp, Repeat_Num, Parent => Result);
         GVD_Array_Type_Access (Result.Get_Type).Set_Value
           (Elem_Value => Tmp,
            Elem_Index => Current_Index,
            Repeat_Num => Repeat_Num);
         Current_Index := Current_Index + Long_Integer (Repeat_Num);

         --  Avoid infinite loop if we can't parse the value
         if Index = Start_Index then
            Index := Type_Str'Last;
         end if;
      end Parse_Item;

   begin
      loop
         case Type_Str (Index) is
            when '}' =>
               Dim   := Dim - 1;
               Index := Index + 1;

            when '{' =>
               --  A parenthesis is either the start of a sub-array (for
               --  other dimensions, or one of the items in case it is a
               --  record or an array. The distinction can be made by
               --  looking at the current dimension being parsed.

               if Dim = GVD_Array_Type_Access
                 (Result.Get_Type).Num_Dimensions
               then
                  Parse_Item;
               else
                  Dim   := Dim + 1;
                  Index := Index + 1;
               end if;

               --  Looking at "index = ".
               Int := Index;
               while Int <= Type_Str'Last
                 and then Type_Str (Int) /= ','  --  item separator
                 and then Type_Str (Int) /= '}'  --  end of sub-array
                 and then Type_Str (Int) /= '{'  --  start of sub-array
                 and then Type_Str (Int) /= '='  --  index of the item
               loop
                  Int := Int + 1;
               end loop;

               if Type_Str (Int) = '=' then
                  Index := Int + 2;  --  skip "index = "
               end if;

            when ',' | ' ' =>
               Index := Index + 1;

            when others =>
               Parse_Item;

               --  Since access types can be followed by junk
               --  ("{0x804845c <foo>, 0x804845c <foo>}"), skip everything
               --  till the next character we know about.
               while Index <= Type_Str'Last
                 and then Type_Str (Index) /= ','
                 and then Type_Str (Index) /= '{'
                 and then Type_Str (Index) /= '}'
               loop
                  Index := Index + 1;
               end loop;

         end case;
         exit when Dim = 0;
      end loop;

      --  Shrink the table of values.
      GVD_Array_Type_Access (Result.Get_Type).Shrink_Values;
   end Parse_Array_Value;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   overriding function Get_Language_Debugger_Context
     (Lang : access Gdb_C_Language) return Language_Debugger_Context
   is
      pragma Unreferenced (Lang);
   begin
      return (Record_Field_Length  => 1,
              Record_Start         => '{',
              Record_End           => '}',
              Array_Start          => '{',
              Array_End            => '}',
              Record_Field         => "=");
   end Get_Language_Debugger_Context;

   ------------------
   -- Set_Variable --
   ------------------

   overriding function Set_Variable
     (Lang     : access Gdb_C_Language;
      Var_Name : String;
      Value    : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "set variable " & Var_Name & " = " & Value;
   end Set_Variable;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name (Lang : access Gdb_C_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "c";
   end Get_Name;

end Debugger.Base_Gdb.C;
