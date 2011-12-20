------------------------------------------------------------------------------
--                                   GPS                                    --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings;           use Ada.Strings;
with GNATCOLL.Tribooleans;  use GNATCOLL.Tribooleans;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with Items.Arrays;          use Items.Arrays;
with Items.Classes;         use Items.Classes;
with Items.Records;         use Items.Records;
with Items.Simples;         use Items.Simples;
with Items;                 use Items;
with Language.Ada;          use Language.Ada;
with Language.Debugger;     use Language.Debugger;
with String_Utils;          use String_Utils;

package body Debugger.Gdb.Ada is

   Me : constant Trace_Handle := Create ("Debug.Gdb.Ada", Off);

   Variant_Name : constant String := "<variant>";
   --  Name used for fields with a variant part

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

   ---------------------
   -- Break Exception --
   ---------------------

   overriding function Break_Exception
     (Debugger  : access Gdb_Ada_Language;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String
   is
      Break   : aliased constant String := "break";
      Catch   : aliased constant String := "catch";

      Command : access constant String := Break'Access;

   begin
      if Debugger.Use_Catch_For_Exceptions = Indeterminate then
         --  Check whether we should use "catch" or "break" to set a breakpoint
         --  on exceptions. The former is the newer syntax, but wasn't
         --  supported in older versions of the debugger

         declare
            Help : constant String := Send_Full
              (Get_Debugger (Debugger), "help catch");
         begin
            Debugger.Use_Catch_For_Exceptions :=
              To_TriBoolean (Index (Help, "catch exception") >= 1);
         end;
      end if;

      if Debugger.Use_Catch_For_Exceptions = GNATCOLL.Tribooleans.True then
         Command := Catch'Access;
      end if;

      if Unhandled then
         if Temporary then
            return "t" & Command.all & " exception unhandled";
         else
            return Command.all & " exception unhandled";
         end if;

      elsif Name /= "" and then Name /= "all" then
         if Temporary then
            return "t" & Command.all & " exception " & Name;
         else
            return Command.all & " exception " & Name;
         end if;

      else
         if Temporary then
            return "t" & Command.all & " exception";
         else
            return Command.all & " exception";
         end if;
      end if;
   end Break_Exception;

   ----------------
   -- Parse_Type --
   ----------------

   overriding procedure Parse_Type
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access)
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

            else
               Skip_To_Char (Type_Str, Index, '>');
               Result := New_Simple_Type;
               Set_Type_Name (Result, Type_Str (Start .. Index));
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

               --  Use the result of "whatis" so as to get a more interesting
               --  information
               Set_Type_Name
                 (Result,
                  Unknown_Type_Prefix & Entity & ASCII.LF & Type_Str);
            else
               raise Unexpected_Type;
            end if;

         when 'd' =>
            --  A delta type, as for "Duration" types (delta 1e-09)

            if Looking_At (Type_Str, Index, "delta ") then
               Result := New_Simple_Type;
               Set_Type_Name (Result, Type_Str);

            else
               raise Unexpected_Type;
            end if;

         when 'f' =>
            --  A function that comes from the dereferencing of an access type
            if Looking_At (Type_Str, Index, "function ") then
               Result := null;
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
                  Set_Type_Name
                    (Result, Type_Str (Start .. Index - 1));
               end;
            else
               raise Unexpected_Type;
            end if;

         when 'n' =>
            --  A tagged record type, as in
            --  new tagged_type with record c : float; end record;

            if Looking_At (Type_Str, Index, "new ") then
               declare
                  Child  : Generic_Type_Access;
                  Parent : Generic_Type_Access;
                  Last   : Natural;
               begin
                  Index := Index + 4;
                  Result := New_Class_Type (Num_Ancestors => 1);

                  --  What is the ancestor ?

                  Last := Index;

                  while Type_Str (Last) /= ' ' loop
                     Last := Last + 1;
                  end loop;

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

                  Add_Ancestor (Class_Type (Result.all), 1,
                                Class_Type_Access (Parent));
                  Set_Type_Name (Parent, Type_Str (Index .. Last - 1));

                  --  Get the child (skip "with record")

                  Index := Last + 12;
                  Parse_Record_Type (Lang, Type_Str, Entity, Index,
                                     Is_Union => False, Result => Child,
                                     End_On => "end record");
                  Set_Child (Class_Type (Result.all),
                             Record_Type_Access (Child));
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
               Result := null;
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
                  Set_Type_Name (Result, Type_Str (Start .. Index - 1));
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
                  Child : Generic_Type_Access;
               begin
                  Parse_Record_Type
                    (Lang, Type_Str, Entity, Index,
                     Is_Union => False, Result => Child,
                     End_On => "end record");
                  Result := New_Class_Type (Num_Ancestors => 0);
                  Set_Child (Class_Type (Result.all),
                             Record_Type_Access (Child));
               end;
            else
               raise Unexpected_Type;
            end if;

         when '(' =>
            --  Enumeration type

            Skip_To_Char (Type_Str, Index, ')');
            Result := New_Enum_Type;

            --  Get the result of "whatis" so as to get a more concise
            --  information.
            Set_Type_Name
              (Result, Unknown_Type_Prefix & Entity
               & ASCII.LF & Type_Str (Start .. Index));
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
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive) is
   begin
      Internal_Parse_Value
        (Lang, Type_Str, Index, Result, Repeat_Num, Parent => null);
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
      Result       : out Generic_Type_Access)
   is
      pragma Unreferenced (Start_Of_Dim);
      Item_Separator : constant Character := ',';
      Dimension_End  : constant Character := ')';
      Num_Dim        : Integer := 1;
      Tmp_Index      : Natural := Index;
      R              : Array_Type_Access;
      Index_Str      : Unbounded_String;
      G              : Generic_Type_Access;

   begin
      --  As a special case, if we have (<>) for the dimensions (ie an
      --  unconstrained array), this is treated as an access type and not an
      --  array type).

      if Looking_At (Type_Str, Tmp_Index, "array (<>)") then
         Result := New_Access_Type;
         Set_Type_Name (Result, Type_Str);
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
      R := Array_Type_Access (Result);
      Set_Type_Name (R, Unknown_Type_Prefix & Entity & ASCII.LF & Type_Str);

      --  Then parse the dimensions

      Num_Dim := 1;
      Index   := Index + 7;

      while Num_Dim <= Num_Dimensions (R.all) loop
         declare
            First, Last : Long_Integer;
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
               First := Long_Integer'Last;
               Skip_To_Char (Type_Str, Index, ' ');
            end if;

            Index := Index + 4;  --  skips ' .. '

            if Type_Str (Index) in '0' .. '9'
              or else Type_Str (Index) = '-'
            then
               Parse_Num (Type_Str, Index, Last);
            else
               Last := Long_Integer'First;

               while Index <= Type_Str'Last
                 and then Type_Str (Index) /= ','
                 and then Type_Str (Index) /= ')'
               loop
                  Index := Index + 1;
               end loop;
            end if;

            Index := Index + 2;  --  skips ', ' or ') '
            Set_Dimensions (R.all, Num_Dim, (First, Last));
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
               Set_Dimensions (R.all, 1, (First, Last));
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
         Set_Type_Name (G, Type_Str (Tmp_Index .. Index - 1));
         Set_Item_Type (R.all, G);

      elsif Tmp_Index + 6 <= Type_Str'Last
        and then Type_Str (Tmp_Index .. Tmp_Index + 5) = "access"
      then
         G := New_Access_Type;
         Set_Type_Name
           (G, Unknown_Type_Prefix
            & Array_Item_Name (Lang, Entity, "0")
            & ASCII.LF & "array");
         Set_Item_Type (R.all, New_Access_Type);

      else
         --  Get the type of the items.
         --  Note that we can not simply do a "ptype" on the string we read
         --  after "of ", since we might not be in the right context, for
         --  instance if Entity is something like "Foo::entity".
         --  Thus, we have to do a "ptype" directly on the first item of the
         --  array.

         for J in 1 .. Num_Dimensions (R.all) loop
            Append (Index_Str,
                    Long_Integer'Image (Get_Dimensions (R.all, J).First));

            if J /= Num_Dimensions (R.all) then
               Append (Index_Str, ",");
            end if;
         end loop;

         Set_Item_Type (R.all,
           Parse_Type (Get_Debugger (Lang),
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
      Result   : out Generic_Type_Access;
      End_On   : String)
   is
      Tmp_Index : Natural;
      Fields    : Natural := 0;
      R         : Record_Type_Access;
      Num_Parts : Natural := 0;
      G         : Generic_Type_Access;
      Part      : Generic_Type_Access;

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

      if Is_Union then
         Result := New_Union_Type (Fields);
      else
         Result := New_Record_Type (Fields);
      end if;

      R := Record_Type_Access (Result);

      Set_Type_Name (R, Unknown_Type_Prefix & Entity & ASCII.LF & Type_Str);

      --  Now parse all the fields

      Fields := 1;

      while Fields <= Num_Fields (R.all) loop
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
               Set_Field_Name (R.all, Fields, Variant_Name, Num_Parts);
            else
               Set_Field_Name
                 (R.all, Fields, Type_Str (Tmp_Index .. Index - 1), 0);
            end if;

            --  Parses the parts, and create a record for each

            Num_Parts := 0;

            while Num_Parts < Get_Variant_Parts (R.all, Fields)
              and then not Looking_At (Type_Str, Index, "end ")
            loop
               Skip_To_String (Type_Str, Index, "=>");

               Index := Index + 2;
               Num_Parts := Num_Parts + 1;

               if Num_Parts = Get_Variant_Parts (R.all, Fields) then
                  Parse_Record_Type (Lang, Type_Str, Entity,
                                     Index, Is_Union => False,
                                     Result => Part, End_On => "end case");
               else
                  Parse_Record_Type (Lang, Type_Str, Entity,
                                     Index, Is_Union => False,
                                     Result => Part, End_On => "when ");
               end if;

               Set_Variant_Field (R.all, Fields, Num_Parts,
                                  Record_Type_Access (Part));

               Skip_Blanks (Type_Str, Index);
            end loop;

            Index := Index + 9;
            Fields := Fields + 1;

         --  Else a standard field

         else
            --  Get the name of the field

            Tmp_Index := Index;
            Skip_To_Char (Type_Str, Index, ':');
            Set_Field_Name (R.all, Fields, Type_Str (Tmp_Index .. Index - 1),
                            Variant_Parts => 0);

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
               Set_Value (Item  => R.all, Value => G, Field => Fields);

               --  Do not get the result of "whatis", since it is more
               --  interesting for debugging purposes to display the type as
               --  seen by the debugger.

               Set_Type_Name (G, Type_Str (Tmp_Index .. Index - 1));

            else
               declare
                  Result : Generic_Type_Access;
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
                       (Lang, Entity, Get_Field_Name (R.all, Fields).all),
                     J, Result);

                  Set_Value (R.all, Result, Field => Fields);

               exception
                  when Unexpected_Type =>
                     --  However, sometimes we actually need to do a ptype.
                     --  For efficiency, we try to do it directly on the type
                     --  of the field (so as to avoid a costly request to gdb
                     --  with A.B.C...).

                     Set_Value (R.all,
                                Parse_Type (Get_Debugger (Lang),
                                            Entity & "."
                                              & Get_Field_Name
                                              (R.all, Fields).all),
                                Field => Fields);
               end;
            end if;

            Index := Index + 1;
            Fields := Fields + 1;
         end if;

         Skip_Blanks (Type_Str, Index);
      end loop;
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   overriding procedure Parse_Array_Value
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
      Dim     : Natural := 0;            --  current dimension
      Current_Index : Long_Integer := 0; --  Current index in the parsed array
      Bounds  : Dimension;
      Lengths : array (1 ..  Num_Dimensions (Result.all)) of Long_Integer;
      --  The number of items in each dimension

      Previous_Index : Integer;
      Previous_Dim   : Natural;

      procedure Parse_Item;
      --  Parse the value of a single item, and add it to the contents of
      --  Result.

      ----------------
      -- Parse_Item --
      ----------------

      procedure Parse_Item is
         Int        : Natural := Index;
         Tmp        : Generic_Type_Access;
         Repeat_Num : Integer;
      begin
         if Dim = 0 then
            return;
         end if;

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

         Bounds := Get_Dimensions (Result.all, Dim);

         if Type_Str (Int) = '=' then
            --  Looking at "index => ".
            --  If we have an array with dynamic bounds, now is a good time to
            --  find the index.

            if Bounds.Last < Bounds.First then
               Parse_Num (Type_Str, Index, Bounds.First);
               Set_Dimensions (Result.all, Dim, Bounds);
            end if;

            Index := Int + 3;  --  skip "index => "

            --  If we are not parsing the most internal dimension, we in fact
            --  saw the start of a new dimension, as in:
            --    (3 => ((6 => 1, 2), (6 => 1, 2)), ((6 => 1, 2), (6 => 1, 2)))
            --  for array (3 .. 4, 1 .. 2, 6 .. 7) of integer
            --  In that case, don't try to parse the item.

            if Dim /= Num_Dimensions (Result.all) then
               return;
            end if;

         elsif Type_Str (Int) = '(' then
            --  If we have an array but the "index => " was not used by GDB,
            --  now is also a good time to find the index.
            --  ??? GDB does not use the "index => " notation when the range
            --  used for the index starts with the first element of the index
            --  type as in:
            --     type Index is (A, B, C, D);
            --     type Index_Range is new Index range A .. C;
            --     type Matrix is array (Index_Range, Index_Range) of Integer;

            if Bounds.Last < Bounds.First then
               Parse_Num (Type_Str, Index, Bounds.First);
               Set_Dimensions (Result.all, Dim, Bounds);
            end if;

            --  If we are not parsing the most internal dimension, we in fact
            --  saw the start of a new dimension.
            --  In that case, don't try to parse the item.

            if Dim /= Num_Dimensions (Result.all) then
               return;
            end if;

         elsif Bounds.Last < Bounds.First then
            if Bounds.First = Long_Integer'Last then
               Bounds.First := 0;
               Bounds.Last := Lengths (Dim) - 1;
            else
               Bounds.First := 1;
               Bounds.Last := Lengths (Dim);
            end if;

            Set_Dimensions (Result.all, Dim, Bounds);
         end if;

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
         Lengths (Dim) := Lengths (Dim) + Long_Integer (Repeat_Num);

         --  In case the bounds were dynamic and we weren't able before to
         --  get the number of items correctly...

         if Bounds.Last < Bounds.First + Lengths (Dim) - 1 then
            Bounds.Last := Bounds.First + Lengths (Dim) - 1;
            Set_Dimensions (Result.all, Dim, Bounds);
         end if;
      end Parse_Item;

   begin
      loop
         Previous_Dim := Dim;
         Previous_Index := Index;

         case Type_Str (Index) is
            when ')' =>
               --  If we have an array with a dynamic range (ie not known
               --  until we parse the value), now is a good time to
               --  get the range.

               Bounds := Get_Dimensions (Result.all, Dim);

               if Bounds.Last < Bounds.First then
                  if Bounds.First = Long_Integer'Last
                    and then Bounds.Last = Long_Integer'First
                  then
                     --  if we did not find the bound before, it is very
                     --  likely 0 (e.g. array of enum).

                     Set_Dimensions (Result.all, Dim, (0, Lengths (Dim) - 1));

                  elsif Bounds.First = Long_Integer'Last then
                     Set_Dimensions
                       (Result.all, Dim,
                        (Bounds.Last - Lengths (Dim) + 1, Bounds.Last));

                  elsif Bounds.Last = Long_Integer'First then
                     Set_Dimensions
                       (Result.all, Dim,
                        (Bounds.First, Bounds.First + Lengths (Dim) - 1));
                  end if;
               end if;

               Dim := Dim - 1;
               Index := Index + 1;

            when '(' =>
               --  A parenthesis is either the start of a sub-array (for
               --  other dimensions, or one of the items in case it is a
               --  record or an array. The distinction can be made by
               --  looking at the current dimension being parsed.

               Parse_Item;

               if Dim /= Num_Dimensions (Result.all) then
                  Dim := Dim + 1;
                  Index := Index + 1;
                  Lengths (Dim) := 0;
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
            Set_Valid (Result, False);
            return;
         end if;
      end loop;

      --  Shrink the table of values

      Shrink_Values (Result.all);
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

   -----------
   -- Start --
   -----------

   overriding function Start
     (Debugger : access Gdb_Ada_Language) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "begin";
   end Start;

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

end Debugger.Gdb.Ada;
