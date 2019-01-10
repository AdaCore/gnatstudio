------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GNAT.Regpat; use GNAT.Regpat;
with Language.C;  use Language.C;

with GNAT.Strings; use GNAT.Strings;

package body Language.Java is

   Keywords_Regexp : aliased String :=
     --  The C subset
     "^(auto|break|c(ase|on(st|tinue)|har)|d(efault|o|ouble)|e(lse|num|xtern)"
     & "|f(loat|or)|goto|i(f|n(t|line))|long|re(gister|strict|turn)"
     & "|s(hort|i(gned|zeof)|t(atic|ruct)|witch)|un(ion|signed)|vo(id|latile)"
     & "|while|typedef"

     --  Java
     & "|finally|synchronized|implements|extends"
     & "|t(h(rows|readsafe)|ransient)|native)\b";

   Keywords_List : aliased Pattern_Matcher :=
                     Compile ("^(" & Keywords_Regexp & ")\W");

   The_Keywords : constant GNAT.Strings.String_List :=
                    GNAT.Strings.String_List'(Language.Keywords (C_Lang)) &
                    (1  => new String'("finally"),
                     2  => new String'("synchronized"),
                     3  => new String'("implements"),
                     4  => new String'("extends"),
                     5  => new String'("throws"),
                     6  => new String'("threadsafe"),
                     7  => new String'("transient"),
                     8  => new String'("native"));

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Java_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return False;
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access Java_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name;
      --  This notion does not exist in Java, since all types
      --  are references.
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access Java_Language;
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
     (Lang  : access Java_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name & '.' & Field;
   end Record_Field_Name;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Java_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_Regexp'Access;
   end Keywords;

   overriding function Keywords
     (Lang : access Java_Language) return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   overriding function Keywords
     (Lang : access Java_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return The_Keywords;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   Java_Context  : aliased Language_Context :=
     (Syntax =>
        (Comment_Start                 => new String'("/*"),
         Comment_End                   => new String'("*/"),
         New_Line_Comment_Start        => new String'("//"),
         New_Line_Comment_Start_Regexp => null),
      String_Delimiter              => '"',
      Quote_Character               => '\',
      Constant_Character            => ''',
      Can_Indent                    => True,
      Syntax_Highlighting           => True,
      Case_Sensitive                => True,
      Accurate_Xref                 => False,
      Use_Semicolon                 => True);

   overriding function Get_Language_Context
     (Lang : access Java_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Java_Context'Access;
   end Get_Language_Context;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name (Lang : access Java_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "Java";
   end Get_Name;

end Language.Java;
