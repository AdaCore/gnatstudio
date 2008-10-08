-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2000-2008, AdaCore                  --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNAT.Regpat;       use GNAT.Regpat;

with String_Utils;      use String_Utils;
with C_Analyzer;        use C_Analyzer;

package body Language.C is

   Keywords_Regexp : aliased String :=
     "auto|break|c(ase|on(st|tinue)|har)|d(efault|o|ouble)|e(lse|num|xtern)" &
     "|f(loat|or)|goto|i(f|n(t|line))|long|re(gister|strict|turn)" &
     "|s(hort|i(gned|zeof)|t(atic|ruct)|witch)|un(ion|signed)|vo(id|latile)" &
     "|while|typedef";

   Keywords_List : aliased Pattern_Matcher :=
                     Compile ("^(" & Keywords_Regexp & ")\W");

   The_Keywords : constant GNAT.Strings.String_List :=
                    (1  => new String'("auto"),
                     2  => new String'("break"),
                     3  => new String'("case"),
                     4  => new String'("const"),
                     5  => new String'("continue"),
                     6  => new String'("char"),
                     7  => new String'("default"),
                     8  => new String'("do"),
                     9  => new String'("double"),
                     10  => new String'("else"),
                     11  => new String'("enum"),
                     12  => new String'("extern"),
                     13  => new String'("float"),
                     14  => new String'("for"),
                     15  => new String'("goto"),
                     16  => new String'("if"),
                     17  => new String'("int"),
                     18  => new String'("inline"),
                     19  => new String'("long"),
                     20  => new String'("register"),
                     21  => new String'("restrict"),
                     22  => new String'("return"),
                     23  => new String'("short"),
                     24  => new String'("signed"),
                     25  => new String'("sizeof"),
                     26  => new String'("static"),
                     27  => new String'("struct"),
                     28  => new String'("switch"),
                     29  => new String'("union"),
                     30  => new String'("unsigned"),
                     31  => new String'("void"),
                     32  => new String'("volatile"),
                     33  => new String'("while"),
                     34  => new String'("typedef"));

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^\w+\s*"                         --  type specs; there can be no
        & "([\w_*]+\s+)?"                 --  more than 3 tokens, right?
        & "([\w_*]+\s+)?"
        & "([*&]+\s*)?"                   --  pointer
        & "(\(\*\s*)?([\w_]+[a-z][\w_]*)\s*\)?"
                                          --  subprogram name or access to subp
        & "(\s[\w_]+\s*\()?"              --  handling of macros, as in
                                          --  "void pa_exit PARAMS ((int))"
        & "\([^(]",
        Multiple_Lines);

   C_Explorer_Categories : constant Explorer_Categories :=
     (1 => (Category       => Cat_Function,
            Category_Name  => null,
            Regexp         => Subprogram_RE'Access,
            Position_Index => 5,
            End_Index      => 0,
            Make_Entry     => null));

   C_Project_Fields : constant Project_Field_Array :=
     (1 => (Attribute_Name  => new String'("compiler_command"),
            Attribute_Index => new String'("c"),
            Description     => new String'("C compiler"),
            Values          => null,
            Editable        => True));

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access C_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return    Str = "int"
        or else Str = "char"
        or else Str = "float"
        or else Str = "double"
        or else Str = "long"
        or else Str = "short"

         --  "unsigned int", "unsigned char"
        or else (Str'Length >= 9
                 and then Str (Str'First .. Str'First + 8) = "unsigned ")

         --  "long int", "long unsigned int"
        or else (Str'Length >= 5
                 and then Str (Str'First .. Str'First + 4) = "long ")

         --  "short int", "short unsigned int"
        or else (Str'Length >= 6
                 and then Str (Str'First .. Str'First + 5) = "short ")

        or else Str = "void";
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access C_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "(*" & Name & ")";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access C_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name & '[' & Index & ']';
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access C_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      if Index (Name, "*") = 0 then
         return Name & '.' & Field;
      else
         --  Name is complex, protect it
         return '(' & Name & ")." & Field;
      end if;
   end Record_Field_Name;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access C_Language) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return C_Explorer_Categories;
   end Explorer_Regexps;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access C_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_Regexp'Access;
   end Keywords;

   overriding function Keywords
     (Lang : access C_Language) return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   overriding function Keywords
     (Lang : access C_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return The_Keywords;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   C_Context             : aliased Language_Context :=
     (Comment_Start_Length          => 2,
      Comment_End_Length            => 2,
      Comment_Start                 => "/*",
      Comment_End                   => "*/",
      New_Line_Comment_Start        => new String'("//"),
      New_Line_Comment_Start_Regexp => null,
      String_Delimiter              => '"',
      Quote_Character               => '\',
      Constant_Character            => ''',
      Can_Indent                    => True,
      Syntax_Highlighting           => True,
      Case_Sensitive                => True);

   overriding function Get_Language_Context
     (Lang : access C_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return C_Context'Access;
   end Get_Language_Context;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   overriding procedure Parse_Constructs
     (Lang   : access C_Language;
      Buffer : Glib.UTF8_String;
      Result : out Construct_List)
   is
      pragma Unreferenced (Lang);
      Constructs : aliased Construct_List;
   begin
      Analyze_C_Source
        (Buffer,
         Default_Indent_Parameters,
         Format     => False,
         Constructs => Constructs'Unchecked_Access);
      Result := Constructs;
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
     (Lang     : access C_Language;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      pragma Unreferenced (Lang);
      pragma Suppress (All_Checks);
      --  ??? For some reason we're sometimes getting a CE in this procedure
      --  with no apparent reason if checks are enabled.

   begin
      Analyze_C_Source
        (Buffer        => Buffer,
         Indent_Params => Default_Indent_Parameters,
         Format        => False,
         Callback      => Callback);
   end Parse_Entities;

   -------------------
   -- Format_Buffer --
   -------------------

   overriding procedure Format_Buffer
     (Lang                : access C_Language;
      Buffer              : String;
      Replace             : Replace_Text_Callback;
      From, To            : Natural := 0;
      Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
      Indent_Offset       : Natural := 0;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null)
   is
      pragma Unreferenced (Lang, Case_Exceptions, Is_Optional_Keyword);
   begin
      Analyze_C_Source
        (Buffer,
         Indent_Params,
         True,
         From,
         To,
         Replace,
         Indent_Offset => Indent_Offset);
   end Format_Buffer;

   ------------------
   -- Comment_Line --
   ------------------

   overriding function Comment_Line
     (Lang    : access C_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String
   is
      pragma Unreferenced (Lang);
      Index_Start : Natural;
      Index_Last  : Natural;
      After_Start : Natural;
      Before_Last : Natural;
   begin
      if Comment then
         --  Append "/* " at the beginning and " */" before the line end.

         declare
            New_Line : String (1 .. Line'Length + 3);
            Index    : Integer;
         begin
            Index := Line'First;

            while Index <= Line'Last
              and then Line (Index) /= ASCII.LF
              and then Line (Index) /= ASCII.CR
            loop
               Index := Index + 1;
            end loop;

            if Index = Line'First then
               return Line;
            end if;

            --  Insert the " */" before the end of line.

            if Index <= Line'Last then
               New_Line (1 .. Index - Line'First) :=
                 Line (Line'First .. Index - 1);

               New_Line (Index - Line'First + 1 .. Index - Line'First + 3) :=
                 " */";
               New_Line (Index - Line'First + 4 .. New_Line'Last) :=
                 Line (Index .. Line'Last);

               return "/* " & New_Line;
            else
               return "/* " & Line & " */";
            end if;
         end;
      else  --  Uncomment
         for Index in Line'First .. Line'Last - 1 loop
            if Line (Index .. Index + 1) = "//" then
               --  Single line comment

               if Index + 3 <= Line'Last and then
                 Line (Index .. Index + 3) = "//  "
               then
                  if Clean then
                     Index_Start := Index + 4;
                     Skip_Blanks (Line, Index_Start);
                     return Line (Index_Start .. Line'Last);
                  end if;
                  return Line (Line'First .. Index - 1)
                    & Line (Index + 4 .. Line'Last);
               elsif Index + 2 <= Line'Last
                 and then Line (Index .. Index + 2) = "// "
               then
                  if Clean then
                     Index_Start := Index + 3;
                     Skip_Blanks (Line, Index_Start);
                     return Line (Index_Start .. Line'Last);
                  end if;
                  return Line (Line'First .. Index - 1)
                    & Line (Index + 3 .. Line'Last);
               else
                  return Line (Index + 2 .. Line'Last);
               end if;
            elsif Line (Index .. Index + 1) = "/*" then
               --  Multiline comment

               Index_Last := Index;
               Skip_To_String (Line, Index_Last, "*/");

               if Index_Last < Line'Last
                 and then Line (Index_Last .. Index_Last + 1) = "*/"
               then
                  --  The line contains "*/"

                  Index_Start := Index;
                  After_Start := Index_Start + 2;
                  Before_Last := Index_Last - 1;

                  if Is_Blank (Line (After_Start))
                    and then After_Start + 1 < Index_Last
                  then
                     After_Start := After_Start + 1;
                  end if;

                  if Is_Blank (Line (Before_Last))
                    and then Before_Last - 1 >= After_Start
                  then
                     Before_Last := Before_Last - 1;
                  end if;

                  if Clean then
                     Skip_Blanks (Line, After_Start);

                     return Line (After_Start .. Before_Last);
                  end if;

                  if Index_Last + 1 < Line'Last then
                     return Line (Line'First .. Index_Start - 1)
                       & Line (After_Start .. Before_Last)
                       & Line (Index_Last + 2 .. Line'Last);
                  else
                     return Line (Line'First .. Index_Start - 1)
                       & Line (After_Start .. Before_Last);
                  end if;
               else
                  --  There is no "*/" in the line
                  Index_Start := Index + 2;
                  if Clean then
                     Skip_Blanks (Line, Index_Start);
                  end if;

                  if Index_Start >= Line'Last then
                     --  There is only blanks after "/*". We keep them
                     --  in order to avoid missing end-of-line characters.
                     return Line (Index + 2 .. Line'Last);
                  else
                     return Line (Index_Start .. Line'Last);
                  end if;
               end if;
            elsif Line (Index .. Index + 1) = "*/" then
               --  Characters before the "*/" mark are part of the comment.
               --  Extract the part after the mark.

               Index_Start := Line'First;

               if Clean then
                  Skip_Blanks (Line, Index_Start);
               end if;

               return Line (Index_Start .. Index - 1)
                 & Line (Index + 2 .. Line'Last);
            end if;
         end loop;

         Index_Start := Line'First;

         if Clean then
            Skip_Blanks (Line, Index_Start);
         end if;

         return Line (Index_Start .. Line'Last);
      end if;
   end Comment_Line;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   overriding function Get_Project_Fields
     (Lang : access C_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return C_Project_Fields;
   end Get_Project_Fields;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name (Lang : access C_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "c";
   end Get_Name;

end Language.C;
