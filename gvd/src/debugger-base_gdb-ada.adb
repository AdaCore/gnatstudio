------------------------------------------------------------------------------
--                                   GPS                                    --
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

with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Strings;                         use Ada.Strings;
with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.Utils;                      use GNATCOLL.Utils;
with GVD.Variables.Types.Arrays;          use GVD.Variables.Types.Arrays;
with GVD.Variables.Types.Classes;         use GVD.Variables.Types.Classes;
with GVD.Variables.Types.Records;         use GVD.Variables.Types.Records;
with GVD.Variables.Types.Simples;         use GVD.Variables.Types.Simples;
with GVD.Variables.Types.Simples.Strings;
use GVD.Variables.Types.Simples.Strings;
with GVD.Variables.Types;                 use GVD.Variables.Types;
with Language.Ada;                        use Language.Ada;
with Language.Debugger;                   use Language.Debugger;
with String_Utils;                        use String_Utils;

pragma Warnings (Off);
with GVD.Variables.Types.Classes.Ada.Finalization;
with GVD.Variables.Types.Classes.Ada.Strings.Unbounded;
pragma Warnings (On);

package body Debugger.Base_Gdb.Ada is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.GDB.ADA", Off);

   Variant_Name : constant String := "<variant>";
   --  Name used for fields with a variant part

   String_Pattern : constant Pattern_Matcher := Compile
     ("^array \((\d+) \.\. (\d+)\) of character.*");
   --  To detect strings

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Lang : access Gdb_Ada_Language) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "ada";
   end Get_Name;

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Gdb_Ada_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_Simple_Type (Ada_Lang, Str);
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Gdb_Ada_Language)
      return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Ada_Lang);
   end Keywords;

   overriding function Keywords
     (Lang : access Gdb_Ada_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Ada_Lang);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   overriding function Get_Language_Context
     (Lang : access Gdb_Ada_Language) return Language.Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Language_Context (Ada_Lang);
   end Get_Language_Context;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access Gdb_Ada_Language) return Language.Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Explorer_Regexps (Ada_Lang);
   end Explorer_Regexps;

   --------------------
   -- Is_System_File --
   --------------------

   overriding function Is_System_File
     (Lang : access Gdb_Ada_Language; File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_System_File (Ada_Lang, File_Name);
   end Is_System_File;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access Gdb_Ada_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Dereference_Name (Ada_Lang, Name);
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access Gdb_Ada_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Array_Item_Name (Ada_Lang, Name, Index);
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access Gdb_Ada_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Record_Field_Name (Ada_Lang, Name, Field);
   end Record_Field_Name;

   ----------------
   -- Parse_Type --
   ----------------

   overriding procedure Parse_Type
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder)
   is
      Start : constant Natural := Index;
   begin
      Trace (Me, "Parse_Type: " & Type_Str (Index .. Type_Str'Last));

      if Looking_At (Type_Str, Index, "const ") then
         Index := Index + 6;
      end if;

      case Type_Str (Index) is
         when '<' =>
            --  A union type

            if Looking_At (Type_Str, Index, "<union ") then
               Index := Index + 7;
               Skip_To_Char (Type_Str, Index, '{');
               Index := Index + 1;
               Parse_Record_Type (Lang, Type_Str, Entity, Index,
                                  Is_Union => True, Result => Result,
                                  End_On => "}>");

            --  A reference (e.g for some "in out" parameters)
            --  (<ref> array (...) of character) @0x300000: "The")
            --  ??? For now ignore that fact, and parse the type...

            elsif Looking_At (Type_Str, Index, "<ref>") then
               Index := Index + 6;
               Parse_Type (Lang, Type_Str, Entity, Index, Result);

            --  Simple types, like <4-byte integer> and <4-byte float>

            elsif Looking_At
              (Type_Str, Index, "<<data variable, no debug info>>")
            then
               Skip_To_Char (Type_Str, Index, '>');
               Index := Index + 2;

            else
               Skip_To_Char (Type_Str, Index, '>');
               Result := New_Simple_Type;
               Result.Get_Type.Set_Type_Name (Type_Str (Start .. Index));
               Index := Index + 1;
            end if;

         when 'a' =>
            --  Arrays, as in "array (1 .. 4, 3 .. 5) of character"

            if Looking_At (Type_Str, Index, "array ") then
               Parse_Array_Type
                 (Lang, Type_Str, Entity, Index,
                  Start_Of_Dim => Index + 6, Result => Result);

            --  Access types

            elsif Looking_At (Type_Str, Index, "access ") then
               Result := New_Access_Type;

               --  Try to retrieve the type name and not the type description
               Result.Get_Type.Set_Type_Name
                 (Get_Type_Info (Lang.Get_Debugger, Entity, Type_Str));
            else
               raise Unexpected_Type;
            end if;

         when 'd' =>
            --  A delta type, as for "Duration" types (delta 1e-09)

            if Looking_At (Type_Str, Index, "delta ") then
               Result := New_Simple_Type;
               Result.Get_Type.Set_Type_Name (Type_Str);

            else
               raise Unexpected_Type;
            end if;

         when 'f' =>
            --  A function that comes from the dereferencing of an access type
            if Looking_At (Type_Str, Index, "function ") then
               Result := Empty_GVD_Type_Holder;
            else
               raise Unexpected_Type;
            end if;

         when 'm' =>
            --  Modular types

            if Looking_At (Type_Str, Index, "mod ") then
               declare
                  Modulo : Long_Integer;
               begin
                  Index := Index + 4;
                  Parse_Num (Type_Str, Index, Modulo);
                  Result := New_Mod_Type (Modulo);
                  Result.Get_Type.Set_Type_Name
                    (Type_Str (Start .. Index - 1));
               end;
            else
               raise Unexpected_Type;
            end if;

         when 'n' =>
            --  A tagged record type, as in
            --  new tagged_type with record c : float; end record;

            if Looking_At (Type_Str, Index, "new ") then
               declare
                  Child  : GVD.Variables.Types.GVD_Type_Holder;
                  Parent : GVD.Variables.Types.GVD_Type_Holder;
                  Last   : Natural;
                  C      : Predefined_Type_Maps.Cursor;
               begin
                  Index := Index + 4;

                  declare
                     Type_Name : constant String :=
                       Lang.Get_Debugger.Get_Type_Info (Entity, "");
                  begin
                     C := Predefined_Type_Reestr.Find (Type_Name);
                     if Predefined_Type_Maps.Has_Element (C) then
                        Result := Predefined_Type_Maps.Element (C).all;
                     else
                        Result := New_Class_Type (Num_Ancestors => 1);
                     end if;
                  end;

                  --  What is the ancestor ?

                  Last := Index;

                  while Type_Str (Last) /= ' ' loop
                     Last := Last + 1;
                  end loop;

                  C := Predefined_Type_Reestr.Find
                    (Type_Str (Index .. Last - 1));
                  if Predefined_Type_Maps.Has_Element (C) then
                     Parent := Predefined_Type_Maps.Element (C).all;
                  else
                     declare
                        Ancestor_Type : constant String :=
                          Type_Of (Get_Debugger (Lang),
                                   Type_Str (Index .. Last - 1));
                        Tmp : Natural := Ancestor_Type'First;

                     begin
                        Parse_Type (Lang, Ancestor_Type,
                                    Type_Str (Index .. Last - 1),
                                    Tmp, Parent);
                     end;

                     Parent.Get_Type.Set_Type_Name
                       (Type_Str (Index .. Last - 1));
                  end if;

                  GVD_Class_Type_Access
                    (Result.Get_Type).Add_Ancestor (1, Parent);

                  --  Get the child (skip "with record")

                  Index := Last + 12;
                  Parse_Record_Type (Lang, Type_Str, Entity, Index,
                                     Is_Union => False, Result => Child,
                                     End_On => "end record");
                  GVD_Class_Type_Access (Result.Get_Type).Set_Child (Child);
               end;
            else
               raise Unexpected_Type;
            end if;

         when 'p' =>
            --  A procedure that comes from the dereferencing of an access type
            if Looking_At (Type_Str, Index, "procedure")
              and then (Type_Str'Last = Index + 8
                        or else Type_Str (Index + 9) = ' ')
            then
               Result := Empty_GVD_Type_Holder;
            else
               raise Unexpected_Type;
            end if;

         when 'r' =>
            --  A record type, as in 'record field1: integer; end record'

            if Looking_At (Type_Str, Index, "record") then
               Index := Index + 7;
               Parse_Record_Type
                 (Lang, Type_Str, Entity, Index,
                  Is_Union => False,
                  Result   => Result,
                  End_On   => "end record");

            --  Range types

            elsif Looking_At (Type_Str, Index, "range ") then
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
               end;

            else
               raise Unexpected_Type;
            end if;

         --  ??? We could handle "string" as well as a standard type
         --  when 's' =>

         when 't' =>
            --  A tagged type

            if Looking_At (Type_Str, Index, "tagged record") then
               Index := Index + 14;

               declare
                  Child : GVD.Variables.Types.GVD_Type_Holder;
               begin
                  Parse_Record_Type
                    (Lang, Type_Str, Entity, Index,
                     Is_Union => False, Result => Child,
                     End_On => "end record");
                  Result := New_Class_Type (Num_Ancestors => 0);
                  GVD_Class_Type_Access (Result.Get_Type).Set_Child (Child);
               end;
            else
               raise Unexpected_Type;
            end if;

         when '(' =>
            --  Enumeration type

            Skip_To_Char (Type_Str, Index, ')');
            Result := New_Enum_Type;

            --  Try to retrieve the type name and not the type description
            Result.Get_Type.Set_Type_Name
              (Get_Type_Info
                 (Lang.Get_Debugger, Entity, Type_Str (Start .. Index)));
            Index := Index + 1;

         --  A type we do not expect

         when others =>
            raise Unexpected_Type
              with "cannot parse type " & Type_Str (Index .. Type_Str'Last);
      end case;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   overriding procedure Parse_Value
     (Lang       : access Gdb_Ada_Language;
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

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   overriding procedure Parse_Array_Type
     (Lang         : access Gdb_Ada_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder)
   is
      pragma Unreferenced (Start_Of_Dim);
      Item_Separator : constant Character := ',';
      Dimension_End  : constant Character := ')';
      Num_Dim        : Integer := 1;
      Tmp_Index      : Natural := Index;
      R              : GVD_Type_Holder;
      Index_Str      : Unbounded_String;
      G              : GVD_Type_Holder;

      Matched : Match_Array (0 .. 2);
   begin
      --  A special case for strings

      if Looking_At (Type_Str, Tmp_Index, "array (<>) of character")
        or else Looking_At  --  Bounded & Unbounded strings
          (Type_Str, Tmp_Index, "array (1 .. max_length) of character")
      then
         Result := New_Simple_Type;
         Result.Get_Type.Set_Type_Name ("character");
         return;
      end if;

      Match (String_Pattern, Type_Str (Tmp_Index .. Type_Str'Last), Matched);
      if Matched (0) /= No_Match then
         Result := New_String_Type;
         Result.Get_Type.Set_Type_Name
           ("string (" &
              Type_Str (Matched (1).First .. Matched (1).Last) &
              " .. " & Type_Str (Matched (2).First .. Matched (2).Last) & ')');
         return;
      end if;

      --  As a special case, if we have (<>) for the dimensions (ie an
      --  unconstrained array), this is treated as an access type and not an
      --  array type).

      if Looking_At (Type_Str, Tmp_Index, "array (<>)") then
         Result := New_Access_Type;
         Result.Get_Type.Set_Type_Name (Type_Str);
         return;
      end if;

      --  First, find the number of dimensions

      while Tmp_Index <= Type_Str'Last
        and then Type_Str (Tmp_Index) /= Dimension_End
      loop
         if Type_Str (Tmp_Index) = Item_Separator then
            Num_Dim := Num_Dim + 1;
         end if;

         Tmp_Index := Tmp_Index + 1;
      end loop;

      --  Create the type

      Result := New_Array_Type (Num_Dimensions => Num_Dim);
      R := Result;
      R.Get_Type.Set_Type_Name
        (Get_Type_Info (Lang.Get_Debugger, Entity, Type_Str));

      --  Then parse the dimensions

      Num_Dim := 1;
      Index   := Index + 7;

      while Num_Dim <= GVD_Array_Type_Access (R.Get_Type).Num_Dimensions loop
         declare
            First, Last       : Long_Integer;
            Discriminant_Type : Unbounded_String;

            function Get_Discriminant_Value
              (Name : String) return Long_Integer;
            --  Converts discriminant name into its integer representation.

            ----------------------------
            -- Get_Discriminant_Value --
            ----------------------------

            function Get_Discriminant_Value
              (Name : String) return Long_Integer is
            begin
               if Discriminant_Type = "" then
                  Discriminant_Type := To_Unbounded_String
                    (Lang.Get_Debugger.Get_Type_Info (Entity & "'First", ""));
               end if;

               return Long_Integer'Value
                 (Lang.Get_Debugger.Value_Of
                    ((if Discriminant_Type = ""
                     then Name
                     else To_String (Discriminant_Type) & "'(" & Name & ")"),
                     Format => Decimal));
            end Get_Discriminant_Value;

         begin
            --  The dimensions might not be numbers.
            --  For instance, when a record field is an array constrained by
            --  a discriminant, the range has the discriminant's name in it.
            --  ??? Should we have some flag that indicate the dynamic aspect
            --  of the bounds, instead of relying on special values.

            if Type_Str (Index) in '0' .. '9'
              or else Type_Str (Index) = '-'
            then
               Parse_Num (Type_Str, Index, First);
            else
               Tmp_Index := Index;
               Skip_To_Char (Type_Str, Index, ' ');

               --  Evaluate First to decimal value
               begin
                  First := Get_Discriminant_Value
                    (Type_Str (Tmp_Index .. Index - 1));
               exception
                  when Constraint_Error =>
                     First := Long_Integer'Last;
               end;
            end if;

            Index := Index + 4;  --  skips ' .. '

            if Type_Str (Index) in '0' .. '9'
              or else Type_Str (Index) = '-'
            then
               Parse_Num (Type_Str, Index, Last);
            else
               Tmp_Index := Index;

               while Index <= Type_Str'Last
                 and then Type_Str (Index) /= ','
                 and then Type_Str (Index) /= ')'
               loop
                  Index := Index + 1;
               end loop;

               --  Evaluate Last to decimal value
               begin
                  Last := Get_Discriminant_Value
                    (Type_Str (Tmp_Index .. Index - 1));
               exception
                  when Constraint_Error =>
                     Last := Long_Integer'First;
               end;
            end if;

            Index := Index + 2;  --  skips ', ' or ') '
            GVD_Array_Type_Access
              (R.Get_Type).Set_Dimensions (Num_Dim, (First, Last));
            Num_Dim := Num_Dim + 1;
         end;
      end loop;

      --  Gdb, in Ada mode, allows one to specify one-dimensional slices for
      --  arrays, as in 'print U (2..3)'.
      --  In that case, the bounds for the first dimension should be taken
      --  from Entity rather than from the result of ptype.

      Tmp_Index := Entity'Last;
      Skip_Blanks_Backward (Entity, Tmp_Index);

      if Tmp_Index >= Entity'First and then Entity (Tmp_Index) = ')' then
         Tmp_Index := Tmp_Index - 1;
         while Tmp_Index >= Entity'First
           and then (Entity (Tmp_Index) in '0' .. '9'
                     or else Entity (Tmp_Index) = ' ')
         loop
            Tmp_Index := Tmp_Index - 1;
         end loop;

         --  Do we have a slice, or a simple array item ?

         if Tmp_Index >= Entity'First and then Entity (Tmp_Index) = '.' then
            Skip_To_Char (Entity, Tmp_Index, '(', Step => -1);
            Tmp_Index := Tmp_Index + 1;

            declare
               First, Last : Long_Integer;
            begin
               Parse_Num (Entity, Tmp_Index, First);
               while Entity (Tmp_Index) not in '0' .. '9' loop
                  Tmp_Index := Tmp_Index + 1;
               end loop;
               Parse_Num (Entity, Tmp_Index, Last);
               GVD_Array_Type_Access
                 (R.Get_Type).Set_Dimensions (1, (First, Last));
            end;
         end if;
      end if;

      --  Skip the type of the items

      Index := Index + 3; --  skips 'of '
      Tmp_Index := Index;
      Skip_To_Blank (Type_Str, Index);

      --  If we have a simple type, no need to ask gdb, for efficiency reasons

      if Is_Simple_Type (Lang, Type_Str (Tmp_Index .. Index - 1)) then
         G := New_Simple_Type;
         G.Get_Type.Set_Type_Name (Type_Str (Tmp_Index .. Index - 1));
         GVD_Array_Type_Access (R.Get_Type).Set_Item_Type (G);

      elsif Tmp_Index + 6 <= Type_Str'Last
        and then Type_Str (Tmp_Index .. Tmp_Index + 5) = "access"
      then
         G := New_Access_Type;
         G.Get_Type.Set_Type_Name
           (Get_Type_Info
              (Lang.Get_Debugger,
               Array_Item_Name (Lang, Entity, "0"),
               "array"));
         GVD_Array_Type_Access (R.Get_Type).Set_Item_Type (New_Access_Type);

      else
         --  Get the type of the items.
         --  Note that we can not simply do a "ptype" on the string we read
         --  after "of ", since we might not be in the right context, for
         --  instance if Entity is something like "Foo::entity".
         --  Thus, we have to do a "ptype" directly on the first item of the
         --  array.

         for J in 1 .. GVD_Array_Type_Access (R.Get_Type).Num_Dimensions loop
            declare
               Img : constant String := Long_Integer'Image
                 (GVD_Array_Type_Access
                    (R.Get_Type).Get_Dimensions (J).First);
            begin
               Append
                 (Index_Str,
                  (if Img (Img'First) = ' '
                   then Img (Img'First + 1 .. Img'Last)
                   else Img));
            end;

            if J /= GVD_Array_Type_Access (R.Get_Type).Num_Dimensions then
               Append (Index_Str, ",");
            end if;
         end loop;

         GVD_Array_Type_Access (R.Get_Type).Set_Item_Type
           (Parse_Type (Get_Debugger (Lang),
            Array_Item_Name (Lang, Entity, To_String (Index_Str))));
      end if;
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   overriding procedure Parse_Record_Type
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Is_Union : Boolean;
      Result   : out GVD.Variables.Types.GVD_Type_Holder;
      End_On   : String)
   is
      Tmp_Index : Natural;
      Fields    : Natural := 0;
      R         : GVD_Type_Holder;
      Num_Parts : Natural := 0;
      G         : GVD_Type_Holder;
      Part      : GVD_Type_Holder;

      Unchecked_Union : Boolean := False;

   begin
      Trace (Me, "Parse_Record_Type: " & Type_Str (Index .. Type_Str'Last));
      Skip_Blanks (Type_Str, Index);
      Tmp_Index := Index;

      --  Count the number of fields

      while Tmp_Index <= Type_Str'Last
        and then not Looking_At (Type_Str, Tmp_Index, End_On)
      loop

         --  A null field ? Do no increment the count
         if Looking_At (Type_Str, Tmp_Index, "null;") then
            Tmp_Index := Tmp_Index + 5;

         --  An incomplete type ? Do not increment the count, e.g:
         --  "record <incomplete type> end record"

         elsif Looking_At (Type_Str, Tmp_Index, "<incomplete ") then
            Tmp_Index := Tmp_Index + 17;

         --  A record with a variant part? This counts as
         --  only one field

         elsif Looking_At (Type_Str, Tmp_Index, "case ") then
            Tmp_Index := Tmp_Index + 5;
            Skip_To_String (Type_Str, Tmp_Index, "end case;");
            Tmp_Index := Tmp_Index + 9;
            Fields := Fields + 1;

            --  In record with a variant part and pragma Unchecked_Union
            --  Skip "record (?) is" then count total fields in all parts

         elsif Looking_At (Type_Str, Tmp_Index, "(?) is") then

            Skip_To_String (Type_Str, Tmp_Index, "case ? is");
            Tmp_Index := Tmp_Index + 9;
            Unchecked_Union := True;

            --  In record with a variant part and pragma Unchecked_Union
            --  Skip "when ? =>" then count total fields
         elsif Looking_At (Type_Str, Tmp_Index, "when ? =>") then

            Tmp_Index := Tmp_Index + 9;

            --  In record with a variant part and pragma Unchecked_Union
            --  Skip "end case;"
         elsif Looking_At (Type_Str, Tmp_Index, "end case;") then

            Tmp_Index := Tmp_Index + 9;

            --  Else a standard field

         else
            Skip_To_Char (Type_Str, Tmp_Index, ':');
            Tmp_Index := Tmp_Index + 1;
            Skip_Blanks (Type_Str, Tmp_Index);

            if Looking_At (Type_Str, Tmp_Index, "access function ") then
               Tmp_Index := Tmp_Index + 16;
               Skip_To_String (Type_Str, Tmp_Index, " return ");
               Tmp_Index := Tmp_Index + 8;

            elsif Looking_At (Type_Str, Tmp_Index, "access procedure ") then
               Tmp_Index := Tmp_Index + 16;
               Skip_To_Char (Type_Str, Tmp_Index, ')');
               Tmp_Index := Tmp_Index + 1;

            elsif Looking_At (Type_Str, Tmp_Index, "access procedure;") then
               Tmp_Index := Tmp_Index + 16;
            end if;

            Skip_To_Char (Type_Str, Tmp_Index, ';');
            Tmp_Index := Tmp_Index + 1;
            Fields := Fields + 1;
         end if;

         Skip_Blanks (Type_Str, Tmp_Index);
      end loop;

      if Is_Union or Unchecked_Union then
         Result := New_Union_Type (Fields);
      else
         Result := New_Record_Type (Fields);
      end if;

      R := Result;

      GVD_Record_Type_Access (R.Get_Type).Set_Type_Name
        (Get_Type_Info (Lang.Get_Debugger, Entity, Type_Str));

      --  Now parse all the fields

      Fields := 1;

      while Fields <= GVD_Record_Type_Access (R.Get_Type).Num_Fields loop
         if Looking_At (Type_Str, Index, "null;") then
            Index := Index + 5;

         elsif Looking_At (Type_Str, Index, "<incomplete ") then
            Index := Index + 17;

         elsif Looking_At (Type_Str, Index, "case ") then
            Index := Index + 5;
            Tmp_Index := Index;

            Skip_To_Char (Type_Str, Index, ' ');

            --  Count the number of alternatives in the variant part

            Tmp_Index := Index;

            while not Looking_At (Type_Str, Tmp_Index, "end case") loop
               if Type_Str (Tmp_Index .. Tmp_Index + 1) = "=>" then
                  Num_Parts := Num_Parts + 1;
               end if;

               Tmp_Index := Tmp_Index + 1;
            end loop;

            if Num_Parts > 0 then
               GVD_Record_Type_Access (R.Get_Type).Set_Field_Name
                 (Fields, Variant_Name, Num_Parts);
            else
               GVD_Record_Type_Access (R.Get_Type).Set_Field_Name
                 (Fields, Type_Str (Tmp_Index .. Index - 1), 0);
            end if;

            --  Parses the parts, and create a record for each

            Num_Parts := 0;

            while Num_Parts < GVD_Record_Type_Access
              (R.Get_Type).Get_Variant_Parts (Fields)
              and then not Looking_At (Type_Str, Index, "end ")
            loop
               Skip_To_String (Type_Str, Index, "=>");

               Index := Index + 2;
               Num_Parts := Num_Parts + 1;

               if Num_Parts = GVD_Record_Type_Access
                 (R.Get_Type).Get_Variant_Parts (Fields)
               then
                  Parse_Record_Type (Lang, Type_Str, Entity,
                                     Index, Is_Union => False,
                                     Result => Part, End_On => "end case");
               else
                  Parse_Record_Type (Lang, Type_Str, Entity,
                                     Index, Is_Union => False,
                                     Result => Part, End_On => "when ");
               end if;

               GVD_Record_Type_Access (R.Get_Type).Set_Variant_Field
                 (Fields, Num_Parts, Part);

               Skip_Blanks (Type_Str, Index);
            end loop;

            Index := Index + 9;
            Fields := Fields + 1;

            --  Skip syntax elements of Unchecked_Union
         elsif Looking_At (Type_Str, Index, "(?) is") then

            Skip_To_String (Type_Str, Index, "case ? is");
            Index := Index + 9;

         elsif Looking_At (Type_Str, Index, "when ? =>") then

            Index := Index + 9;

            --  Else a standard field

         else
            --  Get the name of the field

            Tmp_Index := Index;
            Skip_To_Char (Type_Str, Index, ':');
            GVD_Record_Type_Access (R.Get_Type).Set_Field_Name
              (Fields, Type_Str (Tmp_Index .. Index - 1), Variant_Parts => 0);

            --  Get the type of the field

            Index := Index + 2;
            Tmp_Index := Index;

            Skip_Blanks (Type_Str, Index);

            if Looking_At (Type_Str, Index, "access function ") then
               Index := Index + 16;
               Skip_To_String (Type_Str, Index, " return ");
               Index := Index + 8;

            elsif Looking_At (Type_Str, Index, "access procedure ") then
               Index := Index + 16;
               Skip_To_Char (Type_Str, Index, ')');
               Index := Index + 1;

            elsif Looking_At (Type_Str, Index, "access procedure;") then
               Index := Tmp_Index + 16;
            end if;

            Skip_To_Char (Type_Str, Index, ';');

            --  If we have a simple type, no need to ask gdb, for efficiency
            --  reasons.

            if Is_Simple_Type (Lang, Type_Str (Tmp_Index .. Index - 1)) then
               G := New_Simple_Type;
               GVD_Record_Type_Access (R.Get_Type).Set_Value
                 (Value => G, Field => Fields);

               --  Do not get the result of "whatis", since it is more
               --  interesting for debugging purposes to display the type as
               --  seen by the debugger.

               G.Get_Type.Set_Type_Name (Type_Str (Tmp_Index .. Index - 1));

            else
               declare
                  Result : GVD_Type_Holder;
                  J      : Natural := Tmp_Index;
               begin
                  --  First try to parse the type as if it was already the
                  --  result of a "ptype" command. This happens for instance
                  --  when gdb outputs the field type as being "mod 15;" or
                  --  "access foo".
                  Parse_Type
                    (Lang,
                     Type_Str (Tmp_Index .. Index - 1),
                     Record_Field_Name
                       (Lang, Entity, GVD_Record_Type_Access
                            (R.Get_Type).Get_Field_Name (Fields)),
                     J, Result);

                  GVD_Record_Type_Access (R.Get_Type).Set_Value
                    (Result, Field => Fields);

               exception
                  when Unexpected_Type =>
                     --  However, sometimes we actually need to do a ptype.
                     --  For efficiency, we try to do it directly on the type
                     --  of the field (so as to avoid a costly request to gdb
                     --  with A.B.C...).

                     GVD_Record_Type_Access (R.Get_Type).Set_Value
                       (Parse_Type (Get_Debugger (Lang),
                        Entity & "." & GVD_Record_Type_Access
                          (R.Get_Type).Get_Field_Name (Fields)),
                        Field => Fields);
               end;
            end if;

            Index := Index + 1;
            Fields := Fields + 1;
         end if;

         Skip_Blanks (Type_Str, Index);

         --  Skip "end case;" of Unchecked_Union
         if Unchecked_Union and Looking_At (Type_Str, Index, "end case;") then
            Index := Index + 9;
            Skip_Blanks (Type_Str, Index);
         end if;

      end loop;
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   overriding procedure Parse_Array_Value
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out GVD_Type_Holder)
   is
      Dim     : Natural := 0;            --  current dimension
      Current_Index : Long_Integer := 0; --  Current index in the parsed array
      Bounds  : Dimension;
      Lengths : array (1 ..  GVD_Array_Type_Access
                       (Result.Get_Type).Num_Dimensions) of Long_Integer;
      --  The number of items in each dimension

      Previous_Index : Integer;
      Previous_Dim   : Natural;

      procedure Parse_Item;
      --  Parse the value of a single item, and add it to the contents of
      --  Result. We have three cases to parse here:
      --   * 1 => <item> - we parse first bound and item itself
      --   * <item>      - we parse just item itself, increment last bound
      --   * 1 => (      - we parse first bound of subarray. '(' isn't parsed

      ----------------
      -- Parse_Item --
      ----------------

      procedure Parse_Item is
         Int        : Natural := Index;
         Tmp        : GVD_Type_Holder;
         Repeat_Num : Integer;
      begin
         --  Does gdb indicate the number of the item (as in '24 =>')
         --  We search the first character that does not belong to the item
         --  value.

         while Int <= Type_Str'Last
           and then Type_Str (Int) /= ','  --  item separator
           and then Type_Str (Int) /= ')'  --  end of sub-array
           and then Type_Str (Int) /= '('  --  start of sub-array
           and then Type_Str (Int) /= '='  --  index of the item
         loop
            Int := Int + 1;
         end loop;

         Bounds := GVD_Array_Type_Access
           (Result.Get_Type).Get_Dimensions (Dim);

         if Type_Str (Int) = '=' then
            --  Looking at "index => ".
            --  If we have an array with dynamic bounds, now is a good time to
            --  find the index.

            if Bounds.Last < Bounds.First then
               Parse_Num (Type_Str, Index, Bounds.First);
               GVD_Array_Type_Access
                 (Result.Get_Type).Set_Dimensions (Dim, Bounds);
            end if;

            Index := Int + 3;  --  skip "index => "

         end if;
         --  If we are not parsing the most internal dimension, we in fact
         --  saw the start of a new dimension, as in:
         --    (3 => ((6 => 1, 2), (6 => 1, 2)), ((6 => 1, 2), (6 => 1, 2)))
         --  for array (3 .. 4, 1 .. 2, 6 .. 7) of integer
         --  In that case, don't try to parse the item.

         if Dim /= GVD_Array_Type_Access (Result.Get_Type).Num_Dimensions then
            return;
         end if;

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
         Lengths (Dim) := Lengths (Dim) + Long_Integer (Repeat_Num);

         --  In case the bounds were dynamic and we weren't able before to
         --  get the number of items correctly...

         if Bounds.Last < Bounds.First + Lengths (Dim) - 1 then
            Bounds.Last := Bounds.First + Lengths (Dim) - 1;
            GVD_Array_Type_Access
              (Result.Get_Type).Set_Dimensions (Dim, Bounds);
         end if;
      end Parse_Item;

   begin
      --  This loop parses sequence of items separated by '(', ')' and ','
      --  tokens. Each item parsed by Parse_Item procedure.
      loop
         Previous_Dim := Dim;
         Previous_Index := Index;

         case Type_Str (Index) is
            when ')' =>
               --  If we have an array with a dynamic range (ie not known
               --  until we parse the value), now is a good time to
               --  get the range.

               Bounds := GVD_Array_Type_Access
                 (Result.Get_Type).Get_Dimensions (Dim);

               if Bounds.Last = Long_Integer'First then
                  Bounds.Last := Bounds.First + Lengths (Dim) - 1;
                  GVD_Array_Type_Access
                    (Result.Get_Type).Set_Dimensions (Dim, Bounds);
               end if;

               Dim := Dim - 1;
               Index := Index + 1;

               if Dim > 0 then
                  --  If we have parsed an subarray, then adjust length of
                  --  enclosing array.
                  Lengths (Dim) := Lengths (Dim) + 1;
                  Bounds := GVD_Array_Type_Access
                    (Result.Get_Type).Get_Dimensions (Dim);

                  if Bounds.Last < Bounds.First + Lengths (Dim) - 1 then
                     Bounds.Last := Bounds.First + Lengths (Dim) - 1;
                     GVD_Array_Type_Access
                       (Result.Get_Type).Set_Dimensions (Dim, Bounds);
                  end if;
               end if;

            when '(' =>
               --  A parenthesis is either the start of a sub-array (for
               --  other dimensions, or one of the items in case it is a
               --  record or an array. The distinction can be made by
               --  looking at the current dimension being parsed.

               if Dim /= GVD_Array_Type_Access
                 (Result.Get_Type).Num_Dimensions
               then
                  Dim := Dim + 1;
                  Index := Index + 1;
                  Lengths (Dim) := 0;

                  --  Now is a good time to find the index just in case, if we
                  --  have an array but the "index => " was not used by GDB.
                  --  ??? GDB does not use the "index => " notation when the
                  --  range used for the index starts with the first element
                  --  of the index type as in:
                  --     type Index is (A, B, C, D);
                  --     type Index_Range is new Index range A .. C;
                  --     type Matrix is array (Index_Range, Index_Range) of
                  --       Integer;

                  Bounds := GVD_Array_Type_Access
                    (Result.Get_Type).Get_Dimensions (Dim);

                  if Bounds.First = Long_Integer'Last then
                     Bounds.First := 0;
                     GVD_Array_Type_Access
                       (Result.Get_Type).Set_Dimensions (Dim, Bounds);
                  end if;

               end if;

               if Type_Str (Index) = ')' then
                  --  Set last bound for an empty array
                  Bounds.Last := Bounds.First - 1;
                  GVD_Array_Type_Access
                    (Result.Get_Type).Set_Dimensions (Dim, Bounds);
               else
                  Parse_Item;
               end if;

            when ',' | ' ' =>
               Index := Index + 1;

            when others =>
               Parse_Item;
         end case;

         exit when Dim = 0 or else Index >= Type_Str'Last;

         --  If the loop exit condition did not change, do not attempt to parse
         --  the item as an array.

         if Dim = Previous_Dim
           and then Index = Previous_Index
         then
            Result.Get_Type.Set_Valid (False);
            return;
         end if;
      end loop;

      --  Shrink the table of values

      GVD_Array_Type_Access (Result.Get_Type).Shrink_Values;
   end Parse_Array_Value;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   overriding function Get_Language_Debugger_Context
     (Lang : access Gdb_Ada_Language) return Language_Debugger_Context
   is
      pragma Unreferenced (Lang);
   begin
      return (Record_Field_Length  => 2,
              Record_Start         => '(',
              Record_End           => ')',
              Array_Start          => '(',
              Array_End            => ')',
              Record_Field         => "=>");
   end Get_Language_Debugger_Context;

   ------------------
   -- Set_Variable --
   ------------------

   overriding function Set_Variable
     (Lang     : access Gdb_Ada_Language;
      Var_Name : String;
      Value    : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "set variable " & Var_Name & " := " & Value;
   end Set_Variable;

   ---------------------------
   -- Can_Tooltip_On_Entity --
   ---------------------------

   overriding function Can_Tooltip_On_Entity
     (Lang   : access Gdb_Ada_Language;
      Entity : String) return Boolean
   is
      --  Note: It is not possible to directly get the result of "ptype"
      --  or "whatis" for the entity, since gdb in fact gives the type of the
      --  return value.
      --  Instead, we get the info for a pointer to the entity.
      Info : constant String :=
        Type_Of (Get_Debugger (Lang), "&" & Entity);
   begin
      return Info /= ""
        and then
          (Info'Length < 16
           or else Info (Info'First .. Info'First + 15) /= "access procedure")
        and then
          (Info'Length < 15
           or else Info (Info'First .. Info'First + 14) /= "access function");
   end Can_Tooltip_On_Entity;

end Debugger.Base_Gdb.Ada;
