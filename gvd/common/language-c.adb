-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

with GNAT.Regpat;       use GNAT.Regpat;
with Pixmaps_IDE;       use Pixmaps_IDE;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with String_Utils;      use String_Utils;

package body Language.C is

   Keywords_List : constant Pattern_Matcher := Compile
     ("^(" & C_Keywords_Regexp & ")\W");
   --  for java: ("finally" "synchronized" "implements" "extends" "throws"
   --  "threadsafe" "transient" "native" "volatile"

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
            Regexp         => Subprogram_RE'Access,
            Position_Index => 5,
            Icon           => subprogram_xpm'Access,
            Make_Entry     => null));

   -----------------
   -- Local types --
   -----------------

   type Token_Type is
     (Tok_Identifier,

      --  Reserved words for C:

      --  Type specifiers:
      Tok_Char,
      Tok_Float,
      Tok_Int,
      Tok_Long,
      Tok_Short,
      Tok_Signed,
      Tok_Unsigned,
      Tok_Void,

      --  Storage-class specifiers:
      Tok_Auto,
      Tok_Extern,
      Tok_Static,
      Tok_Register,
      Tok_Restrict,
      Tok_Volatile,

      --  Construct keywords:
      Tok_Break,
      Tok_Case,
      Tok_Const,
      Tok_Continue,
      Tok_Default,
      Tok_Do,
      Tok_Double,
      Tok_Else,
      Tok_Enum,
      Tok_For,
      Tok_Goto,
      Tok_If,
      Tok_Inline,
      Tok_Return,
      Tok_Sizeof,
      Tok_Struct,
      Tok_Switch,
      Tok_Typedef,
      Tok_Union,
      Tok_While,

      --  Reserved words for C++:

      Tok_Abstract,
      Tok_Asm,
      Tok_Bool,
      Tok_Catch,
      Tok_Class,
      Tok_Const_Cast,
      Tok_Delete,
      Tok_Dynamic_Cast,
      Tok_Explicit,
      Tok_False,
      Tok_Final,
      Tok_Friend,
      Tok_Interface,
      Tok_Mutable,
      Tok_Namespace,
      Tok_New,
      Tok_Operator,
      Tok_Private,
      Tok_Protected,
      Tok_Public,
      Tok_Reinterpret_Cast,
      Tok_Static_Cast,
      Tok_Synchronized,
      Tok_Template,
      Tok_This,
      Tok_Throw,
      Tok_True,
      Tok_Try,
      Tok_Typeid,
      Tok_Typename,
      Tok_Using,
      Tok_Virtual,
      Tok_Wchar_t);

   subtype Cpp_Token is Token_Type range Tok_Abstract .. Tok_Wchar_t;

   ----------------------
   -- Local procedures --
   ----------------------

   function Get_Token (S : String) return Token_Type;
   --  Return a Token_Type given a string.

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (S : String) return Token_Type is
      pragma Suppress (All_Checks);
      --  For efficiency

      Second : Integer;
   begin
      if S'Length = 1 then
         return Tok_Identifier;
      end if;

      Second := S'First + 1;

      --  Use a case statement instead of a loop for efficiency

      case S (S'First) is
         when 'a' =>
            if S (Second .. S'Last) = "uto" then
               return Tok_Auto;
            elsif S (Second .. S'Last) = "bstract" then
               return Tok_Abstract;
            elsif S (Second .. S'Last) = "sm" then
               return Tok_Asm;
            end if;

         when 'b' =>
            if S (Second .. S'Last) = "reak" then
               return Tok_Break;
            elsif S (Second .. S'Last) = "ool" then
               return Tok_Bool;
            end if;

         when 'c' =>
            if S (Second .. S'Last) = "ase" then
               return Tok_Case;
            elsif S (Second .. S'Last) = "atch" then
               return Tok_Catch;
            elsif S (Second .. S'Last) = "lass" then
               return Tok_Class;
            elsif S'Length > 4 and then S (Second .. Second + 3) = "onst" then
               if S'Length = 5 then
                  return Tok_Const;
               elsif S (Second  + 4 .. S'Last) = "_cast" then
                  return Tok_Const_Cast;
               end if;

            elsif S (Second .. S'Last) = "ontinue" then
               return Tok_Continue;
            elsif S (Second .. S'Last) = "har" then
               return Tok_Char;
            end if;

         when 'd' =>
            if S (Second .. S'Last) = "efault" then
               return Tok_Default;
            elsif S (Second .. S'Last) = "elete" then
               return Tok_Delete;
            elsif S (Second .. S'Last) = "o" then
               return Tok_Do;
            elsif S (Second .. S'Last) = "ouble" then
               return Tok_Double;
            elsif S (Second .. S'Last) = "ynamic_cast" then
               return Tok_Dynamic_Cast;
            end if;

         when 'e' =>
            if S (Second .. S'Last) = "lse" then
               return Tok_Else;
            elsif S (Second .. S'Last) = "num" then
               return Tok_Enum;
            elsif S (Second .. S'Last) = "xtern" then
               return Tok_Extern;
            elsif S (Second .. S'Last) = "xplicit" then
               return Tok_Explicit;
            end if;

         when 'f' =>
            if S (Second .. S'Last) = "alse" then
               return Tok_False;
            elsif S (Second .. S'Last) = "or" then
               return Tok_For;
            elsif S (Second .. S'Last) = "inal" then
               return Tok_Final;
            elsif S (Second .. S'Last) = "loat" then
               return Tok_Float;
            elsif S (Second .. S'Last) = "riend" then
               return Tok_Friend;
            end if;

         when 'g' =>
            if S (Second .. S'Last) = "oto" then
               return Tok_Goto;
            end if;

         when 'i' =>
            if S (Second .. S'Last) = "f" then
               return Tok_If;
            elsif S (Second) = 'n' then
               if S (Second + 1 .. S'Last) = "t" then
                  return Tok_Int;
               elsif S (Second + 1 .. S'Last) = "line" then
                  return Tok_Inline;
               elsif S (Second + 1 .. S'Last) = "terface" then
                  return Tok_Interface;
               end if;
            end if;

         when 'l' =>
            if S (Second .. S'Last) = "ong" then
               return Tok_Long;
            end if;

         when 'm' =>
            if S (Second .. S'Last) = "utable" then
               return Tok_Mutable;
            end if;

         when 'n' =>
            if S (Second .. S'Last) = "amespace" then
               return Tok_Namespace;
            elsif S (Second .. S'Last) = "ew" then
               return Tok_New;
            end if;

         when 'o' =>
            if S (Second .. S'Last) = "perator" then
               return Tok_Operator;
            end if;

         when 'p' =>
            if S (Second) = 'r' then
               if S (Second + 1 .. S'Last) = "ivate" then
                  return Tok_Private;
               elsif S (Second + 1 .. S'Last) = "otected" then
                  return Tok_Protected;
               end if;

            elsif S (Second .. S'Last) = "ublic" then
               return Tok_Public;
            end if;

         when 'r' =>
            if S (Second) = 'e' then
               if S (Second + 1 .. S'Last) = "gister" then
                  return Tok_Register;
               elsif S (Second + 1 .. S'Last) = "interpret_cast" then
                  return Tok_Reinterpret_Cast;
               elsif S (Second + 1 .. S'Last) = "strict" then
                  return Tok_Restrict;
               elsif S (Second + 1 .. S'Last) = "turn" then
                  return Tok_Return;
               end if;
            end if;

         when 's' =>
            if S (Second) = 't' then
               if S'Length > 5
                 and then S (Second + 1 .. Second + 4) = "atic"
               then
                  if S'Length = 6 then
                     return Tok_Static;
                  elsif S (Second + 5 .. S'Last) = "_cast" then
                     return Tok_Static_Cast;
                  end if;

               elsif S (Second + 1 .. S'Last) = "ruct" then
                  return Tok_Struct;
               end if;

            elsif S (Second) = 'i' then
               if S (Second + 1 .. S'Last) = "gned" then
                  return Tok_Signed;
               elsif S (Second + 1 .. S'Last) = "zeof" then
                  return Tok_Sizeof;
               end if;

            elsif S (Second .. S'Last) = "hort" then
               return Tok_Short;
            elsif S (Second .. S'Last) = "ynchronized" then
               return Tok_Synchronized;
            elsif S (Second .. S'Last) = "witch" then
               return Tok_Switch;
            end if;

         when 't' =>
            if S (Second .. S'Last) = "emplate" then
               return Tok_Template;
            elsif S (Second .. S'Last) = "his" then
               return Tok_This;
            elsif S (Second .. S'Last) = "hrow" then
               return Tok_Throw;
            elsif S (Second .. S'Last) = "ry" then
               return Tok_Try;
            elsif S (Second .. S'Last) = "rue" then
               return Tok_True;
            elsif S'Length > 6 and then S (Second .. Second + 2) = "ype" then
               if S (Second + 3 .. S'Last) = "def" then
                  return Tok_Typedef;
               elsif S (Second + 3 .. S'Last) = "id" then
                  return Tok_Typeid;
               elsif S (Second + 3 .. S'Last) = "name" then
                  return Tok_Typename;
               end if;
            end if;

         when 'u' =>
            if S (Second .. S'Last) = "nion" then
               return Tok_Union;
            elsif S (Second .. S'Last) = "sing" then
               return Tok_Using;
            elsif S (Second .. S'Last) = "nsigned" then
               return Tok_Unsigned;
            end if;

         when 'v' =>
            if S (Second) = 'o' then
               if S (Second + 1 .. S'Last) = "id" then
                  return Tok_Void;
               elsif S (Second + 1 .. S'Last) = "latile" then
                  return Tok_Volatile;
               end if;
            elsif S (Second .. S'Last) = "irtual" then
               return Tok_Virtual;
            end if;

         when 'w' =>
            if S (Second .. S'Last) = "hile" then
               return Tok_While;
            elsif S (Second .. S'Last) = "char_t" then
               return Tok_Wchar_t;
            end if;

         when others =>
            return Tok_Identifier;
      end case;

      return Tok_Identifier;
   end Get_Token;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
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

   function Dereference_Name
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

   function Array_Item_Name
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

   function Record_Field_Name
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

   function Explorer_Regexps
     (Lang : access C_Language) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return C_Explorer_Categories;
   end Explorer_Regexps;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access C_Language) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access C_Language) return Language_Context
   is
      pragma Unreferenced (Lang);
   begin
      return
        (Comment_Start_Length          => 2,
         Comment_End_Length            => 2,
         New_Line_Comment_Start_Length => 2,
         Comment_Start                 => "/*",
         Comment_End                   => "*/",
         New_Line_Comment_Start        => "//",
         String_Delimiter              => '"',
         Quote_Character               => '\',
         Constant_Character            => ''',
         Can_Indent                    => True,
         Syntax_Highlighting           => True);
   end Get_Language_Context;

   ----------------------
   -- Analyze_C_Source --
   ----------------------

   procedure Analyze_C_Source
     (Buffer        : String;
      Indent        : out Integer;
      Indent_Params : Indent_Parameters;
      No_Contents   : out Boolean;
      Callback      : Entity_Callback := null;
      Enable_Cpp    : Boolean := False)
   is
      pragma Suppress (All_Checks);
      --  For efficiency

      First       : Natural;
      Index       : Natural := Buffer'First;
      Token       : Token_Type;
      Curly_Level : Integer := 0;
      Paren_Level : Integer := 0;

   begin
      No_Contents := True;
      Indent := 0;

      while Index < Buffer'Last loop
         case Buffer (Index) is
            when ASCII.NUL .. ' ' =>
               if Buffer (Index) = ASCII.LF then
                  No_Contents := True;
               end if;

            when '{' =>
               Curly_Level := Curly_Level + 1;
               Indent := Indent + Indent_Params.Indent_Level;

            when '(' =>
               Paren_Level := Paren_Level + 1;
               Indent := Indent + Indent_Params.Indent_Level;

            when '}' =>
               Curly_Level := Curly_Level - 1;
               Indent := Indent - Indent_Params.Indent_Level;

            when ')' =>
               Paren_Level := Paren_Level - 1;
               Indent := Indent - Indent_Params.Indent_Level;

            when '"' =>
               No_Contents := False;

               --  Skip string

               First := Index;
               Index := Index + 1;

               while Index < Buffer'Last
                 and then (Buffer (Index) /= '"'
                           or else Buffer (Index - 1) = '\')
                 and then Buffer (Index) /= ASCII.LF
               loop
                  Index := Index + 1;
               end loop;

               if Callback /= null then
                  exit when Callback
                    (String_Text, (0, 0, First), (0, 0, Index), False);
               end if;

            when ''' =>
               No_Contents := False;

               --  Skip character

               First := Index;
               Index := Index + 1;

               while Index < Buffer'Last
                 and then (Buffer (Index) /= '''
                           or else Buffer (Index - 1) = '\')
                 and then Buffer (Index) /= ASCII.LF
               loop
                  Index := Index + 1;
               end loop;

               if Callback /= null then
                  exit when Callback
                    (Character_Text, (0, 0, First), (0, 0, Index), False);
               end if;

            when '/' =>
               No_Contents := False;

               --  Comment ?

               First := Index;

               if Buffer (Index + 1) = '/' then
                  --  C++ style comment, skip whole line

                  Index := Index + 1;

                  while Index <= Buffer'Last
                    and then Buffer (Index + 1) /= ASCII.LF
                  loop
                     Index := Index + 1;
                  end loop;

                  if Callback /= null then
                     exit when Callback
                       (Comment_Text, (0, 0, First), (0, 0, Index), False);
                  end if;

               elsif Buffer (Index + 1) = '*' then
                  --  Skip comment

                  Index := Index + 3;

                  while Index < Buffer'Last
                    and then (Buffer (Index - 1) /= '*'
                      or else Buffer (Index) /= '/')
                  loop
                     Index := Index + 1;
                  end loop;

                  if Callback /= null then
                     exit when Callback
                       (Comment_Text,
                        (0, 0, First), (0, 0, Index),
                        Buffer (Index) /= '/');
                  end if;
               end if;

            when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' =>
               No_Contents := False;

               --  Skip identifier or reserved word

               First := Index;

               while Index <= Buffer'Last
                 and then Is_Entity_Letter (Buffer (Index + 1))
               loop
                  Index := Index + 1;
               end loop;

               Token := Get_Token (Buffer (First .. Index));

               if Callback /= null then
                  if Token = Tok_Identifier then
                     exit when Callback
                       (Identifier_Text,
                        (0, 0, First), (0, 0, Index), False);
                  elsif Enable_Cpp
                    or else Token not in Cpp_Token
                  then
                     exit when Callback
                       (Keyword_Text, (0, 0, First), (0, 0, Index), False);
                  end if;
               end if;

            when others =>
               No_Contents := False;
         end case;

         Index := Index + 1;
      end loop;

   exception
      when others =>
         null;
   end Analyze_C_Source;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access C_Language;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      pragma Unreferenced (Lang);
      pragma Suppress (All_Checks);
      --  ??? For some reason we're sometimes getting a CE in this procedure
      --  with no apparent reason if checks are enabled.

      Ignored     : Natural;
      No_Contents : Boolean;

   begin
      Analyze_C_Source
        (Buffer        => Buffer,
         Indent        => Ignored,
         Indent_Params => Default_Indent_Parameters,
         No_Contents   => No_Contents,
         Callback      => Callback);
   end Parse_Entities;

   ----------------------
   -- Next_Indentation --
   ----------------------

   procedure Next_Indentation
     (Lang          : access C_Language;
      Buffer        : String;
      Indent        : out Natural;
      Next_Indent   : out Natural;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters)
   is
      pragma Unreferenced (Lang);

      Tab_Width   : Natural renames Indent_Params.Tab_Width;
      First       : Natural := Buffer'Last - 1;
      Index       : Natural;
      Offset      : Integer := 0;
      No_Contents : Boolean := True;

   begin
      --  Go to beginning of line

      if First < Buffer'First then
         Indent := 0;
         Next_Indent := 0;
         return;
      end if;

      while First /= Buffer'First and then Buffer (First - 1) /= ASCII.LF loop
         First := First - 1;
      end loop;

      Index  := First;
      Indent := 0;

      loop
         if Buffer (Index) = ' ' then
            Indent := Indent + 1;
         elsif Buffer (Index) = ASCII.HT then
            Indent := Indent + Tab_Width - (Indent mod Tab_Width);
         else
            exit;
         end if;

         exit when Index = Buffer'Last;

         Index := Index + 1;
      end loop;

      Analyze_C_Source
        (Buffer        => Buffer (Index .. Buffer'Last),
         Indent        => Offset,
         Indent_Params => Indent_Params,
         No_Contents   => No_Contents,
         Callback      => null);

      if -Offset > Indent then
         Next_Indent := 0;

         if No_Contents then
            Indent := 0;
         end if;

      else
         Next_Indent := Indent + Offset;

         if Offset < 0 and then No_Contents then
            Indent := Next_Indent;
         end if;
      end if;
   end Next_Indentation;

   -----------------------
   -- Is_Case_Sensitive --
   -----------------------

   function Is_Case_Sensitive (Lang : access C_Language) return Boolean is
      pragma Unreferenced (Lang);
   begin
      return True;
   end Is_Case_Sensitive;

   ------------------
   -- Comment_Line --
   ------------------

   function Comment_Line
     (Lang : access C_Language;
      Line : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "/* " & Line & " */";
   end Comment_Line;

   --------------------
   -- Uncomment_Line --
   --------------------

   function Uncomment_Line
     (Lang : access C_Language;
      Line : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      if Line'Length > 6
        and then Line (Line'First .. Line'First + 2) = "/* "
        and then Line (Line'Last - 2 .. Line'Last) = " */"
      then
         return Line (Line'First + 3 .. Line'Last - 3);
      end if;

      return Line;
   end Uncomment_Line;

end Language.C;
