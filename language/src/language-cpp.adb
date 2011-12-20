------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GNAT.Regpat; use GNAT.Regpat;
with Language.C;  use Language, Language.C;
with C_Analyzer;  use C_Analyzer;

package body Language.Cpp is

   Keywords_Regexp : aliased constant String :=
                       Language.Keywords (C_Lang).all &
                       "|a(bstract|sm)|bool|c(atch|lass|onst_cast)" &
                       "|d(elete|ynamic_cast)|explicit|f(alse|inal|riend)" &
                       "|interface|mutable|n(amespace|ew)|operator" &
                       "|p(r(ivate|otected)|ublic)|reinterpret_cast" &
                       "|s(tatic_cast|ynchronized)" &
                       "|t(emplate|h(is|hrow)|r(ue|y)|ype(id|name))" &
                       "|using|virtual|wchar_t";

   Keywords_List : aliased Pattern_Matcher :=
                     Compile ("^(" & Keywords_Regexp & ")\W");

   Classes_RE : aliased Pattern_Matcher :=
     Compile ("^\s*(class|struct)\s+([\w_]+)\s*(:[^{]+)?\{", Multiple_Lines);

   Methods_RE : aliased Pattern_Matcher :=
     Compile                  --  Based on Language.C.Subprogram_RE
       ("^(\w+\s+)?"
        & "([\w_*]+\s+)?"
        & "([\w_*]+\s+)?"
        & "([*&]+\s*)?"
        & "([\w_*]+(::[\w_*]+)+)\s*"  -- method name
        & "\([^(]",
        Multiple_Lines);

   function Make_Entry_Class
     (Str     : String;
      Matched : Match_Array) return String;
   --  Function used to create an entry in the explorer, for classes.
   --  See the description of Explorer_Categories for more information.

   Cpp_Explorer_Categories : constant Explorer_Categories (1 .. 2) :=
     (1 => (Category       => Cat_Class,
            Category_Name  => GNATCOLL.Symbols.No_Symbol,
            Regexp         => Classes_RE'Access,
            Position_Index => 2,
            End_Index      => 0,
            Make_Entry     => Make_Entry_Class'Access),
      2 => (Category       => Cat_Method,
            Category_Name  => GNATCOLL.Symbols.No_Symbol,
            Regexp         => Methods_RE'Access,
            Position_Index => 5,
            End_Index      => 0,
            Make_Entry     => null));

   ----------------------
   -- Make_Entry_Class --
   ----------------------

   function Make_Entry_Class
     (Str      : String;
      Matched  : Match_Array) return String is
   begin
      return Str (Matched (1).First .. Matched (1).Last)
        & " " & Str (Matched (2).First .. Matched (2).Last);
   end Make_Entry_Class;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access Cpp_Language) return Explorer_Categories is
   begin
      return Explorer_Regexps (C_Language (Lang.all)'Access)
        & Cpp_Explorer_Categories;
   end Explorer_Regexps;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Cpp_Language) return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   Cpp_Context             : aliased Language_Context :=
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
      Case_Sensitive                => True,
      Accurate_Xref                 => False);

   overriding function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Cpp_Context'Access;
   end Get_Language_Context;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
     (Lang     : access Cpp_Language;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      pragma Suppress (All_Checks);
      --  See comment in Language.C.Parse_Entities

   begin
      Analyze_C_Source
        (Buffer        => Buffer,
         Symbols       => Lang.Symbols,
         Indent_Params => Default_Indent_Parameters,
         Format        => False,
         Callback      => Callback,
         Enable_Cpp    => True);
   end Parse_Entities;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name (Lang : access Cpp_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "c++";
   end Get_Name;

   ----------------------
   -- Entities_Indexed --
   ----------------------

   overriding function Entities_Indexed (Self : Cpp_Language) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Entities_Indexed;

end Language.Cpp;
