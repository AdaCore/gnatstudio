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

with GVD.Strings;   use GVD.Strings;
with Language.Debugger; use Language.Debugger;

with Items;         use Items;
with Items.Simples; use Items.Simples;
with Items.Arrays;  use Items.Arrays;
with Items.Records; use Items.Records;

with Ada.Text_IO; use Ada.Text_IO;

package body Debugger.Gdb.C is

   use Language;

   ---------------------
   -- Break Exception --
   ---------------------

   function Break_Exception
     (Debugger  : access Gdb_C_Language;
      Name      : String  := "";
      Unhandled : Boolean := False) return String is
   begin
      --  ??? Unsupported, should we raise an exception, so that the menu
      --  can be disabled ?
      return "";
   end Break_Exception;

   -----------------------------
   -- C_Detect_Composite_Type --
   -----------------------------

   procedure C_Detect_Composite_Type
     (Lang     : access Language.Debugger.Language_Debugger'Class;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access)
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
               Num : Natural := 1;
            begin
               Index := Index + 1;
               while Num /= 0 loop
                  if Type_Str (Index) = '}' then
                     Num := Num - 1;
                  elsif Type_Str (Index) = '{' then
                     Num := Num + 1;
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
            Skip_Word (Type_Str, Index);
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

         Set_Type_Name
           (Result,
            Unknown_Type_Prefix & Entity & ASCII.LF
            & Type_Str (Type_Str'First .. Index - 1));

      --  An array type ?

      elsif Index < Type_Str'Last and then Type_Str (Index) = '[' then
         Parse_Array_Type (Lang, Type_Str, Entity, Tmp, Index, Result);
         Index := Tmp;

      else
         Result := null;
         Index := Tmp;
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
      Result   : out Items.Generic_Type_Access)
   is
      Context : constant Language_Debugger_Context :=
        Get_Language_Debugger_Context (Lang);
      Tmp     : Natural := Index;

   begin
      C_Detect_Composite_Type (Lang, Type_Str, Entity, Index, Result);

      --  Do we have an access or array type ?
      if Result /= null then
         return;
      end if;

      --  Else a simple type

      case Type_Str (Index) is
         when 'e' =>
            --  Enumeration type.
            --  Either this is the type itself "enum {....}", or this is the
            --  field of a struct or union "enum name;".

            if Looking_At (Type_Str, Index, "enum ") then
               Index := Index + 5;
               Skip_Word (Type_Str, Index);  --  skips enum name
               Index := Index + 1;

               if Index <= Type_Str'Last and then Type_Str (Index) = '{' then
                  Skip_To_Char (Type_Str, Index, '}');
                  Index := Index + 1;
               end if;

               Result := New_Enum_Type;
               Set_Type_Name (Result, Type_Str (Type_Str'First .. Index - 1));
               return;
            end if;

            --  Else falls through

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
                  Skip_Word (Type_Str, Index);  --  skips struct name
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
                  Skip_Word (Type_Str, Index);  --  skips union name
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

      --  Do we have a simple type ?

      if Is_Simple_Type (Lang, Type_Str (Tmp .. Type_Str'Last)) then
         Result := New_Simple_Type;
         Set_Type_Name (Result, Type_Str (Tmp .. Type_Str'Last));
         return;
      end if;

      --  Else ask for more information
      --  This is needed when Type_Str didn't start with a known keyword,
      --  like "__time_t" for instance

      declare
         Ent : constant String := Type_Str (Tmp .. Type_Str'Last);
         T : constant String := Type_Of (Get_Debugger (Lang), Ent);
         J : Natural := T'First;
      begin
         --  In some cases, T might have a null length (for instance if we
         --  had a C++ class, since Ent="class" and T="" in that case).
         if T'Length /= 0 then
            Parse_Type (Lang, T, Ent, J, Result);
         else
            Result := null;
         end if;
         Index := Type_Str'Last;
      end;
   end C_Parse_Type;

   ----------------
   -- Parse_Type --
   ----------------

   procedure Parse_Type
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access)
   is
   begin
      C_Parse_Type (Lang, Type_Str, Entity, Index, Result);
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Lang       : access Gdb_C_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Type_Access;
      Repeat_Num : out Positive) is
   begin
      Internal_Parse_Value
        (Lang, Type_Str, Index, Result, Repeat_Num, Parent => null);
   end Parse_Value;

   ------------------------
   -- C_Parse_Array_Type --
   ------------------------

   procedure C_Parse_Array_Type
     (Lang         : access Language.Debugger.Language_Debugger'Class;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : in Natural;
      Result       : out Items.Generic_Type_Access)
   is
      Num_Dim   : Integer := 0;
      Initial   : Natural := Index;
      Tmp_Index : Natural;
      R         : Array_Type_Access;
      Last      : Long_Integer;
      Item_Type : Generic_Type_Access;

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
      R := Array_Type_Access (Result);
      Set_Type_Name (R, Unknown_Type_Prefix & Entity & ASCII.LF & Type_Str);

      --  Then parse the dimensions.
      Num_Dim := 0;

      while Index <= Type_Str'Last
        and then Type_Str (Index) = '['
      loop
         Num_Dim := Num_Dim + 1;
         Index := Index + 1;
         Parse_Num (Type_Str, Index, Last);
         Set_Dimensions (R.all, Num_Dim, (0, Last - 1));
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
      Set_Item_Type (R.all, Item_Type);
   end C_Parse_Array_Type;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   procedure Parse_Array_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Generic_Type_Access)
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
      Result     : out Items.Generic_Type_Access)
   is
      Tmp : Natural := Index;
      Semi_Colon_Pos : Natural;

   begin
      --  Get the field name (last word before ;)
      --  There is a small exception here for access-to-subprograms fields,
      --  which look like "void (*field1[2])();"
      --  gdb seems to ignore all the parameters to the function, so
      --  we take the simplest way and consider there is always '()' for
      --  the parameter list.
      --  We also skip embedded unions or structs ("struct foo {..}" or
      --  "struct {...}" completly).

      if Looking_At (Type_Str, Tmp, "struct {")
        or else Looking_At (Type_Str, Tmp, "union {")
      then
         Skip_To_Char (Type_Str, Tmp, '}');
      end if;

      Skip_To_Char (Type_Str, Tmp, ';');
      Semi_Colon_Pos := Tmp;
      Field_End := Tmp;
      Tmp := Tmp - 1;

      if Type_Str (Tmp) = ')' then
         Tmp := Tmp - 2;
         Skip_To_Char (Type_Str, Tmp, '(', Step => -1);
         Tmp := Tmp + 1;
         Name_End := Tmp + 2;
         Skip_Word (Type_Str, Name_End);
         Name_End := Name_End - 1;

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

         Skip_Word (Type_Str, Tmp, Step => -1);
         if Type_Str (Tmp - 1) = ':' then
            Field_End := Tmp - 2;
            Tmp := Tmp - 3;
            Name_End := Tmp;
            Skip_Word (Type_Str, Tmp, Step => -1);
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
      C_Detect_Composite_Type
        (Lang, Type_Str (Index .. Name_Start - 1)
         & Type_Str (Name_End + 1 .. Field_End - 1),
         Entity, Tmp, Result);

      --  if not an access or array:
      if Result = null then
         if Is_Simple_Type (Lang, Type_Str (Index .. Name_Start - 2)) then
            Result := New_Simple_Type;
            Set_Type_Name (Result, Type_Str (Index .. Field_End - 1));
         else
            Tmp := Index;
            Parse_Type
              (Lang,
               Type_Str (Index .. Name_Start - 1)
               & Type_Str (Name_End + 1 .. Field_End - 1),
               Record_Field_Name
               (Lang, Entity, Type_Str (Name_Start .. Name_End)),
               Tmp, Result);
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
      Result    : out Items.Generic_Type_Access;
      End_On    : String)
   is
      Num_Fields : Natural := 0;
      Field      : Natural := 1;
      Initial    : constant Natural := Index;
      R          : Record_Type_Access;
      Field_Value : Generic_Type_Access;
      Tmp         : Natural;
      End_Of_Name, Save : Natural;

   begin
      --  Count the number of fields

      while Index <= Type_Str'Last
        and then Type_Str (Index) /= '}'
      loop
         --  embedded unions or structs
         if Type_Str (Index) = '{' then
            Skip_To_Char (Type_Str, Index, '}');

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

      R := Record_Type_Access (Result);
      Set_Type_Name (R, Type_Str (Type_Str'First .. Initial - 2));

      --  Parse the type

      Index := Initial;

      while Field <= Num_Fields loop
         Skip_Blanks (Type_Str, Index);
         C_Field_Name
           (Lang, Entity,
            Type_Str, Index, Tmp, End_Of_Name, Save, Field_Value);
         Set_Field_Name
           (R.all, Field, Type_Str (Tmp .. End_Of_Name), Variant_Parts => 0);
         Set_Value (R.all, Field_Value, Field);
         Index := Save + 1;
         Field := Field + 1;
      end loop;
   end C_Parse_Record_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Generic_Type_Access;
      End_On    : String)
   is
   begin
      C_Parse_Record_Type
        (Lang, Type_Str, Entity, Index, Is_Union, Result, End_On);
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
      Dim     : Natural := 0;            --  current dimension
      Current_Index : Long_Integer := 0; --  Current index in the parsed array

      procedure Parse_Item;
      --  Parse the value of a single item, and add it to the contents of
      --  Result.

      ----------------
      -- Parse_Item --
      ----------------

      procedure Parse_Item is
         Tmp        : Generic_Type_Access;
         Repeat_Num : Integer;

      begin
         --  Parse the next item
         Tmp := Get_Value (Result.all, Current_Index);

         if Tmp = null then
            Tmp := Clone (Get_Item_Type (Result.all).all);
         end if;

         Internal_Parse_Value
           (Lang, Type_Str, Index, Tmp, Repeat_Num,
            Parent => Generic_Type_Access (Result));
         Set_Value
           (Item       => Result.all,
            Elem_Value => Tmp,
            Elem_Index => Current_Index,
            Repeat_Num => Repeat_Num);
         Current_Index := Current_Index + Long_Integer (Repeat_Num);
      end Parse_Item;

   begin
      loop
         case Type_Str (Index) is
            when '}' =>
               Dim := Dim - 1;
               Index := Index + 1;

            when '{' =>
               --  A parenthesis is either the start of a sub-array (for
               --  other dimensions, or one of the items in case it is a
               --  record or an array. The distinction can be made by
               --  looking at the current dimension being parsed.

               if Dim = Num_Dimensions (Result.all) then
                  Parse_Item;
               else
                  Dim := Dim + 1;
                  Index := Index + 1;
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
      Shrink_Values (Result.all);
   end Parse_Array_Value;

   -----------------
   -- Thread_List --
   -----------------

   function Thread_List (Lang : access Gdb_C_Language) return String is
   begin
      return "info threads";
   end Thread_List;

   -------------------
   -- Thread_Switch --
   -------------------

   function Thread_Switch
     (Lang   : access Gdb_C_Language;
      Thread : Natural) return String is
   begin
      return "thread" & Natural'Image (Thread);
   end Thread_Switch;

   -----------------------
   -- Parse_Thread_List --
   -----------------------

   function Parse_Thread_List
     (Lang   : access Gdb_C_Language;
      Output : String) return Thread_Information_Array
   is
      Result      : Thread_Information_Array (1 .. 0);
   begin
      --  ??? Unsupported, should we raise an exception ?
      return Result;
   end Parse_Thread_List;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   function Get_Language_Debugger_Context
     (Lang : access Gdb_C_Language) return Language_Debugger_Context is
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

   function Set_Variable
     (Lang     : access Gdb_C_Language;
      Var_Name : String;
      Value    : String) return String is
   begin
      return "set variable " & Var_Name & " = " & Value;
   end Set_Variable;

   -----------
   -- Start --
   -----------

   function Start (Debugger : access Gdb_C_Language) return String is
   begin
      return "tbreak main" & ASCII.LF & "run";
   end Start;

end Debugger.Gdb.C;
