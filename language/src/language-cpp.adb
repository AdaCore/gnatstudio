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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with C_Analyzer;              use C_Analyzer;
with GNAT.Regpat;             use GNAT.Regpat;
with Language;                use Language;
with Language.C;              use Language.C;

package body Language.Cpp is

   function Is_Keyword (Word : String) return Boolean;
   --  Return true if Word is a CPP keyword

   Keywords_Regexp : aliased constant String :=
     --  the C subset
     "^(auto|break|c(ase|on(st|tinue)|har)|d(efault|o|ouble)|e(lse|num|xtern)"
     & "|f(loat|or)|goto|i(f|n(t|line))|long|re(gister|strict|turn)"
     & "|s(hort|i(gned|zeof)|t(atic|ruct)|witch)|un(ion|signed)|vo(id|latile)"
     & "|while|typedef"

     --  C++
     & "|a(bstract|sm)|bool|c(atch|lass|onst_cast)"
     & "|d(elete|ynamic_cast)|explicit|f(alse|inal|riend)"
     & "|interface|mutable|n(amespace|ew)|operator"
     & "|p(r(ivate|otected)|ublic)|reinterpret_cast"
     & "|s(tatic_cast|ynchronized)"
     & "|t(emplate|h(is|hrow)|r(ue|y)|ype(id|name))"
     & "|using|virtual|wchar_t)\b";

   Keywords_List : aliased Pattern_Matcher := Compile (Keywords_Regexp);

   The_Keywords : constant GNAT.Strings.String_List :=
     (1  => new String'("and"),
      2  => new String'("and_eq"),
      3  => new String'("asm"),
      4  => new String'("auto"),
      5  => new String'("bitand"),
      6  => new String'("bitor"),
      7  => new String'("bool"),
      8  => new String'("break"),
      9  => new String'("case"),
      10 => new String'("catch"),
      11 => new String'("char"),
      12 => new String'("class"),
      13 => new String'("compl"),
      14 => new String'("const"),
      15 => new String'("continue"),
      16 => new String'("default"),
      17 => new String'("delete"),
      18 => new String'("double"),
      19 => new String'("dynamic_cast"),
      20 => new String'("else"),
      21 => new String'("enum"),
      22 => new String'("explicit"),
      23 => new String'("export"),
      24 => new String'("extern"),
      25 => new String'("false"),
      26 => new String'("float"),
      27 => new String'("for"),
      28 => new String'("friend"),
      29 => new String'("goto"),
      30 => new String'("if"),
      31 => new String'("inline"),
      32 => new String'("int"),
      33 => new String'("long"),
      34 => new String'("mutable"),
      35 => new String'("namespace"),
      36 => new String'("new"),
      37 => new String'("not"),
      38 => new String'("not_eq"),
      39 => new String'("operator"),
      40 => new String'("or"),
      41 => new String'("or_eq"),
      42 => new String'("private"),
      43 => new String'("protected"),
      44 => new String'("public"),
      45 => new String'("register"),
      46 => new String'("reinterpret_cast"),
      47 => new String'("return"),
      48 => new String'("short"),
      49 => new String'("signed"),
      50 => new String'("sizeof"),
      51 => new String'("static"),
      52 => new String'("struct"),
      53 => new String'("switch"),
      54 => new String'("template"),
      55 => new String'("this"),
      56 => new String'("throw"),
      57 => new String'("true"),
      58 => new String'("try"),
      59 => new String'("typedef"),
      60 => new String'("typeid"),
      61 => new String'("typename"),
      62 => new String'("union"),
      63 => new String'("unsigned"),
      64 => new String'("using"),
      65 => new String'("virtual"),
      66 => new String'("void"),
      67 => new String'("volatile"),
      68 => new String'("wchar_t"),
      69 => new String'("while"),
      70 => new String'("xor"),
      71 => new String'("xor_eq"));

   --  C++ 2011 keywords
   --    alignas
   --    alignof
   --    char16_t
   --    char32_t
   --    constexpr
   --    const_cast
   --    decltype
   --    noexcept
   --    nullptr
   --    static_assert
   --    thread_local

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
     (Syntax => (Comment_Start                 => new String'("/*"),
                 Comment_End                   => new String'("*/"),
                 New_Line_Comment_Start        => new String'("//"),
                 New_Line_Comment_Start_Regexp => null),
      String_Delimiter              => '"',
      Quote_Character               => '\',
      Constant_Character            => ''',
      Can_Indent                    => True,
      Syntax_Highlighting           => not Active (Language.Clang_Support),
      Case_Sensitive                => True,
      Accurate_Xref                 => False,
      Use_Semicolon                 => True);

   overriding function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Cpp_Context'Access;
   end Get_Language_Context;

   ----------------
   -- Is_Keyword --
   ----------------

   function Is_Keyword (Word : String) return Boolean is
   begin
      for J in The_Keywords'Range loop
         if The_Keywords (J).all = Word then
            return True;
         end if;
      end loop;

      return False;
   end Is_Keyword;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
     (Lang     : access Cpp_Language;
      Buffer   : String;
      Callback : Entity_Callback) is
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

   ----------------------------
   -- Parse_Tokens_Backwards --
   ----------------------------

   overriding
   procedure Parse_Tokens_Backwards
     (Lang         : access Cpp_Language;
      Buffer       : UTF8_String;
      Start_Offset : String_Index_Type;
      End_Offset   : String_Index_Type := 0;
      Callback     : access procedure (Token : Token_Record;
                                       Stop : in out Boolean))
   is
      pragma Unreferenced (Lang);

      Lowest : constant String_Index_Type :=
                 String_Index_Type'Max
                   (End_Offset, String_Index_Type (Buffer'First));
      Index  : String_Index_Type := Start_Offset;
      Ch     : Character;
      Line   : Natural;
      Token  : Token_Type;
      Stop   : Boolean := False;

      Tok_Begin  : String_Index_Type;
      Tok_End    : String_Index_Type;
      Word_Begin : String_Index_Type;

      procedure Prev_Char (Skip_Comment_Lines : Boolean := True);
      --  Update Ch with the previous character. If Skip_Comment_Lines is
      --  true and we are located at the beginning of the line then skip
      --  comment lines delimited by "//" found in previous lines.

      procedure Scan;
      --  Scan the previous token

      procedure Skip_Comment_Line;
      --  Skip a comment line (delimited by "//")

      ---------------
      -- Prev_Char --
      ---------------

      procedure Prev_Char (Skip_Comment_Lines : Boolean := True) is
      begin
         if Index = Lowest or else Stop then
            if Index = Lowest then
               Index := String_Index_Type'Pred (Lowest);
               Word_Begin := Index;
            end if;

            Ch   := ' ';
            Stop := True;
            return;
         end if;

         Index := Index - 1;
         Ch    := Buffer (Natural (Index));
         Word_Begin := Index;

         if Ch = ASCII.LF then
            Line := Line + 1;
            Ch   := ' ';

            if Skip_Comment_Lines then
               Skip_Comment_Line;
            end if;

         elsif Ch = ASCII.CR then
            Ch := ' ';
         end if;
      end Prev_Char;

      -----------------------
      -- Skip_Comment_Line --
      -----------------------

      procedure Skip_Comment_Line is
         Saved_Stop       : constant Boolean           := Stop;
         Saved_Index      : constant String_Index_Type := Index;
         Saved_Ch         : constant Character         := Ch;
         Saved_Line       : constant Natural           := Line;
         Saved_Word_Begin : constant String_Index_Type := Word_Begin;

      begin
         while not Stop
           and then Line = Saved_Line
           and then Ch /= '/'
         loop
            --  At this stage we don't want to skip consecutive comment lines
            --  Otherwise we displace the scanning cursor too much.

            Prev_Char (Skip_Comment_Lines => False);
         end loop;

         if not Stop
           and then Line = Saved_Line
           and then Ch = '/'
         then
            Prev_Char;

            if Ch = '/' then
               Prev_Char;
               Word_Begin := Saved_Word_Begin;
               return;
            end if;
         end if;

         Stop       := Saved_Stop;
         Index      := Saved_Index;
         Ch         := Saved_Ch;
         Line       := Saved_Line;
         Word_Begin := Saved_Word_Begin;
      end Skip_Comment_Line;

      ----------
      -- Scan --
      ----------

      procedure Scan is
      begin
         loop
            --  Skip spaces

            while Ch = ' ' and then not Stop loop
               Prev_Char;
            end loop;

            Tok_End := Index;

            --  Handle delimited comments

            while Ch = '/' loop
               Prev_Char;

               if Ch /= '*' then
                  Token := Tok_Slash;
                  Tok_Begin := Index + 1;
                  return;
               end if;

               Prev_Char;
               loop
                  while Ch /= '*' and then not Stop loop
                     Prev_Char;
                  end loop;

                  Prev_Char;

                  exit when Ch = '/' or else Stop;
               end loop;

               Prev_Char;
            end loop;

            if Stop then
               Token := Tok_Unknown;
               return;
            end if;

            exit when Ch /= ' ';
         end loop;

         Tok_End := Index;

         case Ch is
            when '+' =>
               Prev_Char;

               case Ch is
                  when '+' =>
                     Token := Tok_Increment;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Plus;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when '-' =>
               Prev_Char;

               case Ch is
                  when '-' =>
                     Token := Tok_Decrement;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Minus;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when '*' =>
               Prev_Char;
               Token := Tok_Asterisk;
               Tok_Begin := Word_Begin + 1;
               return;

            when '/' =>
               Prev_Char;
               Token := Tok_Slash;
               Tok_Begin := Word_Begin + 1;
               return;

            when '%' =>
               Prev_Char;
               Token := Tok_Modulus;
               Tok_Begin := Word_Begin + 1;
               return;

            when '=' =>
               Prev_Char;

               case Ch is
                  when '=' =>
                     Token := Tok_Equal;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '!' =>
                     Token := Tok_Not_Equal;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '>' =>
                     Prev_Char;

                     case Ch is
                     when '>' =>
                        Token := Tok_Bit_Right_Assign;
                        Tok_Begin := Word_Begin;
                        Prev_Char;
                        return;

                     when others =>
                        Token := Tok_Greater_Or_Eq;
                        Tok_Begin := Word_Begin;
                        Prev_Char;
                        return;
                     end case;

                  when '<' =>
                     Prev_Char;

                     case Ch is
                     when '<' =>
                        Token := Tok_Bit_Less_Assign;
                        Tok_Begin := Word_Begin;
                        Prev_Char;
                        return;

                     when others =>
                        Token := Tok_Less_Or_Eq;
                        Tok_Begin := Word_Begin;
                        Prev_Char;
                        return;
                     end case;

                  when '+' =>
                     Token := Tok_Add_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '-' =>
                     Token := Tok_Sub_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '*' =>
                     Token := Tok_Mul_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '/' =>
                     Token := Tok_Div_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '%' =>
                     Token := Tok_Mod_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '&' =>
                     Token := Tok_Bit_And_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '|' =>
                     Token := Tok_Bit_Or_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '^' =>
                     Token := Tok_Bit_Xor_Assign;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Assign;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when '&' =>
               Prev_Char;

               case Ch is
                  when '&' =>
                     Token := Tok_And;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Bitwise_And;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when '|' =>
               Prev_Char;

               case Ch is
                  when '|' =>
                     Token := Tok_Or;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Bitwise_Or;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when '.' =>
               Token := Tok_Dot;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when ',' =>
               Token := Tok_Comma;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when '?' =>
               Token := Tok_Question;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when '(' =>
               Token := Tok_Left_Paren;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when ')' =>
               Token := Tok_Right_Paren;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when '[' =>
               Token := Tok_Left_Sq_Bracket;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when ']' =>
               Token := Tok_Right_Sq_Bracket;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when '{' =>
               Token := Tok_Block_Begin;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when '}' =>
               Token := Tok_Block_End;
               Tok_Begin := Word_Begin;
               Prev_Char;
               return;

            when '"' =>
               Prev_Char;
               while not Stop and then Ch /= '"' loop
                  Prev_Char;
               end loop;

               Token := Tok_String_Literal;
               Tok_Begin := Word_Begin;

               Prev_Char;
               return;

            when '<' =>
               Prev_Char;

               case Ch is
                  when '<' =>
                     Token := Tok_Bitwise_Left;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Less_Than;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when '>' =>
               Prev_Char;

               case Ch is
                  when '-' =>
                     Token := Tok_Dereference;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when '>' =>
                     Token := Tok_Bitwise_Right;
                     Tok_Begin := Word_Begin;
                     Prev_Char;
                     return;

                  when others =>
                     Token := Tok_Greater_Than;
                     Tok_Begin := Word_Begin + 1;
                     return;
               end case;

            when ':' =>
               Prev_Char;

               if Ch = ':' then
                  Token := Tok_Scope;
                  Tok_Begin := Word_Begin;
                  Prev_Char;
                  return;
               else
                  Token := Tok_Colon;
                  Tok_Begin := Word_Begin + 1;
                  return;
               end if;

            when '!' =>
               Prev_Char;
               Token := Tok_Negation;
               Tok_Begin := Word_Begin + 1;
               return;

            when '~' =>
               Prev_Char;
               Token := Tok_Bitwise_Not;
               Tok_Begin := Word_Begin + 1;
               return;

            when '^' =>
               Prev_Char;
               Token := Tok_Bitwise_Xor;
               Tok_Begin := Word_Begin + 1;
               return;

            when others =>
               null;
         end case;

         declare
            Suffix       : constant Character := To_Upper (Ch);
            Suffix_Found : Boolean := False;

         begin
            if Suffix = 'L'
              or else Suffix = 'U'
              or else Suffix = 'F'
            then
               Suffix_Found := True;
               Prev_Char;
            end if;

            if Is_Hexadecimal_Digit (Ch) then
               while not Stop
                 and then (Is_Hexadecimal_Digit (Ch) or else Ch = '.')
               loop
                  Prev_Char;
               end loop;

               if To_Upper (Ch) = 'X' then
                  Prev_Char;

                  if Ch = '0' then
                     Prev_Char;

                     Tok_Begin := Word_Begin + 1;
                     Token := Tok_Literal_Number;
                     return;
                  end if;

               elsif not Is_Alphanumeric (Ch)
                 and then Ch /= '_'
               then
                  Tok_Begin := Word_Begin + 1;
                  Token := Tok_Literal_Number;
                  return;
               end if;
            end if;

            if Suffix_Found
              or else Is_Alphanumeric (Ch)
              or else Ch = '_'
            then
               while not Stop
                 and then
                   (Is_Alphanumeric (Ch) or else Ch = '_' or else Ch = '#')
               loop
                  Prev_Char;
               end loop;

               Tok_Begin := Word_Begin + 1;

               declare
                  Text : constant String :=
                    Buffer (Natural (Tok_Begin) .. Natural (Tok_End));
               begin
                  if Is_Keyword (Text) then
                     Token := Tok_Keyword;
                  else
                     Token := Tok_Identifier;
                  end if;
               end;

               return;
            end if;
         end;

         Token := Tok_Unknown;
         Tok_Begin := Word_Begin;
         Prev_Char;
      end Scan;

   --  Start of processing for Parse_Tokens_Backwards

   begin
      if Index not in Lowest .. String_Index_Type (Buffer'Last) then
         return;
      end if;

      --  Initialize the low level

      Index      := Start_Offset + 1;
      Ch         := ' ';
      Line       := 1;
      Word_Begin := Index;

      --  Initialize the automaton

      Skip_Comment_Line;
      Prev_Char;

      while not Stop loop
         Scan;

         Callback
           ((Tok_Type    => Token,
             Token_First => Tok_Begin,
             Token_Last  => Tok_End),
            Stop);
      end loop;
   end Parse_Tokens_Backwards;

end Language.Cpp;
