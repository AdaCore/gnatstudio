-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with String_Utils; use String_Utils;
with Glib.Unicode; use Glib.Unicode;
with Indent_Stack; use Indent_Stack;
with Generic_Stack;

package body C_Analyzer is

   -----------------
   -- Local types --
   -----------------

   type Token_Type is
     (Tok_String_Literal,        -- "xxx"
      Tok_Char_Literal,          -- 'x'
      Tok_Pound,                 -- Preprocessor directive

      Tok_Identifier,

      Tok_Minus,                 -- -
      Tok_Plus,                  -- +

      Tok_Star,                  -- *
      Tok_Star_Assign,           -- *=
      Tok_Slash,                 -- /
      Tok_Slash_Assign,          -- /=

      Tok_Dot,                   -- .
      Tok_Semicolon,             -- ;

      Tok_Left_Paren,            -- (
      Tok_Right_Paren,           -- )
      Tok_Left_Bracket,          -- {
      Tok_Right_Bracket,         -- }
      Tok_Left_Square_Bracket,   -- [
      Tok_Right_Square_Bracket,  -- ]
      Tok_Comma,                 -- ,

      Tok_Less,                  -- <
      Tok_Assign,                -- =
      Tok_Equal,                 -- ==
      Tok_Greater,               -- >
      Tok_Not,                   -- !
      Tok_Not_Equal,             -- !=
      Tok_Greater_Equal,         -- >=
      Tok_Less_Equal,            -- <=

      Tok_Plus_Assign,           -- +=
      Tok_Minus_Assign,          -- -=
      Tok_Tilde,                 -- ~
      Tok_Tilde_Assign,          -- ~=
      Tok_Percent,               -- %
      Tok_Percent_Assign,        -- %=
      Tok_Xor,                   -- ^
      Tok_Xor_Assign,            -- ^=
      Tok_And,                   -- &
      Tok_Logical_And,           -- &&
      Tok_And_Assign,            -- &=
      Tok_Or,                    -- |
      Tok_Logical_Or,            -- ||
      Tok_Or_Assign,             -- |=

      --  ??? mising: << >> <<= >>=

      Tok_Arrow,                 -- =>

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
      Tok_Wchar_t,

      No_Token);

   subtype Cpp_Token is Token_Type range Tok_Abstract .. Tok_Wchar_t;

   type Extended_Token is record
      Token          : Token_Type := No_Token;
      --  Enclosing token

      Start_New_Line : Boolean := False;
      --  Set to True if the first '{' for the current construct was found
      --  its own line (only used internally).

      Paren_Level    : Natural := 0;
      Curly_Level    : Natural := 0;
      --  Saved paren and curly levels for the current construct.

      First          : Boolean := True;
      --  Flag used to mark the first use of this construct, e.g. the first
      --  'case' of a 'switch'.

      Sloc           : Source_Location;
      --  Source location for this entity

      Sloc_Name      : Source_Location;
      --  Source location for the name of this entity, if relevant

      Name_Start     : Natural := 0;
      Name_End       : Natural := 0;
      --  Location in the buffer of the entity name, if any.
   end record;
   --  Extended information for a token

   package Token_Stack is new Generic_Stack (Extended_Token);
   use Token_Stack;

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

   ----------------------
   -- Analyze_C_Source --
   ----------------------

   procedure Analyze_C_Source
     (Buffer           : String;
      Indent_Params    : Indent_Parameters;
      Format           : Boolean               := True;
      From, To         : Natural               := 0;
      Replace          : Replace_Text_Callback := null;
      Constructs       : Construct_List_Access := null;
      Callback         : Entity_Callback       := null;
      Enable_Cpp       : Boolean               := False)
   is
      pragma Suppress (All_Checks);
      --  For efficiency

      None              : constant := -1;

      Default_Extended  : Extended_Token;
      pragma Warnings (Off, Default_Extended);
      --  Use default values to initialize this pseudo constant.

      Indent_Level      : Natural renames Indent_Params.Indent_Level;
      Indent_Continue   : Natural renames Indent_Params.Indent_Level;
      Use_Tabs          : Boolean renames Indent_Params.Use_Tabs;
      Tab_Width         : Natural renames Indent_Params.Tab_Width;

      First             : Natural;
      Index             : Natural := Buffer'First;
      Indent            : Natural := 0;
      Indent_Done       : Boolean := False;
      Token             : Token_Type := No_Token;
      Prev_Token        : Token_Type := No_Token;
      Curly_Level       : Integer := 0;
      Continuation_Val  : Integer := 0;
      Paren_Level       : Integer := 0;
      Line              : Natural := 1;
      Padding           : Integer := 0;
      Char_In_Line      : Natural := 1;
      Start_Char        : Natural;
      Num_Ifdef         : Natural;
      Last_Replace_Line : Natural := 0;
      Top_Token         : Token_Stack.Generic_Type_Access;
      Tokens            : Token_Stack.Simple_Stack;
      Indents           : Indent_Stack.Simple_Stack;

      procedure Do_Indent
        (P            : Natural;
         Num_Spaces   : Integer;
         Continuation : Boolean := False);
      --  Perform indentation by inserting spaces in the buffer at position P.
      --  If Continuation is True, Indent_Continue extra spaces are added.

      procedure New_Line;
      --  Increment line counter and line-related states.
      --  Should be called each time a new line is detected.

      function Identifier_Keyword return Boolean;
      --  Handle next identifier or keyword.
      --  Assume that Buffer (Index) is an identifier character.
      --  Return whether processing should be stopped.

      function Preprocessor_Directive return Boolean;
      --  Handle preprocessor directive.
      --  Assume that Buffer (Index) = '#'
      --  For now, only handle #if 0 as a multiple-line comment
      --  Return whether processing should be stopped.

      procedure Replace_Text
        (First : Natural;
         Last  : Natural;
         Str   : String);
      --  Wrapper for Replace.all, taking (From, To) into account.

      function Skip_Character return Boolean;
      --  Skip character starting at Buffer (Index)
      --  Return whether processing should be stopped.

      function Skip_Comment return Boolean;
      --  Skip possible comment starting at Buffer (Index)
      --  Assume that Buffer (Index) = '/'
      --  Do nothing if there is no comment.
      --  Return whether processing should be stopped.

      function Skip_String return Boolean;
      --  Skip string starting at Buffer (Index)
      --  Return whether processing should be stopped.

      --------------------
      -- Stack Routines --
      --------------------

      procedure Pop
        (Stack : in out Token_Stack.Simple_Stack; Value : out Extended_Token);
      --  Pop Value on top of Stack.

      procedure Pop (Stack : in out Token_Stack.Simple_Stack);
      --  Pop Value on top of Stack. Ignore returned value.

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent
        (P            : Natural;
         Num_Spaces   : Integer;
         Continuation : Boolean := False)
      is
         Start       : Natural;
         Indentation : Integer;
         Index       : Natural;

      begin
         if Indent_Done or not Format then
            return;
         end if;

         Start := Line_Start (Buffer, P);
         Index := Start;

         loop
            exit when Buffer (Index) /= ' '
              and then Buffer (Index) /= ASCII.HT;

            Index := Index + 1;
         end loop;

         if Top (Indents).all = None then
            Indentation := Num_Spaces;
         else
            Indentation := Top (Indents).all;
         end if;

         if Continuation then
            Continuation_Val := Continuation_Val + Indent_Continue;
            Indentation := Indentation + Continuation_Val;
         else
            Continuation_Val := 0;
         end if;

         Replace_Text
           (Start, Index, Blank_Slice (Indentation, Use_Tabs, Tab_Width));
         Indent_Done := True;
      end Do_Indent;

      --------------
      -- New_Line --
      --------------

      procedure New_Line is
      begin
         Indent_Done := False;
         Line := Line + 1;
         Char_In_Line := 0;
      end New_Line;

      ---------
      -- Pop --
      ---------

      procedure Pop
        (Stack : in out Token_Stack.Simple_Stack;
         Value : out Extended_Token)
      is
         Column : Natural;
         Info   : Construct_Access;

      begin
         --  Never pop the initial value

         if Top (Stack).Token = No_Token then
            Value := Top (Stack).all;
            return;
         end if;

         Token_Stack.Pop (Stack, Value);

         --  Build next entry of Constructs if needed.

         if Constructs /= null
           and then Value.Token /= Tok_Assign
         then
            Column             := Char_In_Line;
            Info               := Constructs.Current;
            Constructs.Current := new Construct_Information;

            if Constructs.First = null then
               Constructs.First := Constructs.Current;
            else
               Constructs.Current.Prev := Info;
               Constructs.Current.Next := Info.Next;
               Info.Next               := Constructs.Current;
            end if;

            Constructs.Last := Constructs.Current;

            case Value.Token is
               when Tok_Pound =>
                  --  Tok_Pound is used for #include

                  Constructs.Current.Category := Cat_Include;
                  Constructs.Current.Name := new String'(Buffer
                    (Value.Name_Start .. Value.Name_End));
                  Constructs.Current.Sloc_Entity := Value.Sloc_Name;

               when Tok_Do | Tok_For | Tok_While =>
                  Constructs.Current.Category := Cat_Loop_Statement;
               when Tok_If | Tok_Else =>
                  Constructs.Current.Category := Cat_If_Statement;
               when Tok_Switch =>
                  Constructs.Current.Category := Cat_Case_Statement;
               when Tok_Void =>
                  --  Tok_Void is used for blocks: {}

                  if Value.Curly_Level = 0 then
                     --  ??? Would be nice to be able to find the name
                     --  and parameters of this function

                     Constructs.Current.Category := Cat_Function;

                  else
                     Constructs.Current.Category := Cat_Simple_Block;
                  end if;

               when Tok_Struct =>
                  Constructs.Current.Category := Cat_Structure;
               when Tok_Union =>
                  Constructs.Current.Category := Cat_Union;
               when Tok_Class =>
                  Constructs.Current.Category := Cat_Class;
               when Tok_Enum =>
                  Constructs.Current.Category := Cat_Type;
               when others =>
                  Constructs.Current.Category := Cat_Unknown;
            end case;

            Constructs.Current.Sloc_Start := Value.Sloc;
            Constructs.Current.Sloc_End := (Line, Column, Index);
            Constructs.Current.Is_Declaration := False;
         end if;
      end Pop;

      procedure Pop (Stack : in out Token_Stack.Simple_Stack) is
         Value : Extended_Token;
      begin
         Pop (Stack, Value);
      end Pop;

      ------------------
      -- Replace_Text --
      ------------------

      procedure Replace_Text
        (First : Natural;
         Last  : Natural;
         Str   : String)
      is
         Start : Natural;
      begin
         if Replace /= null
           and then (To = 0 or else Line in From .. To)
         then
            if Last_Replace_Line /= Line then
               Last_Replace_Line := Line;
               Padding := 0;
            end if;

            Start := Line_Start (Buffer, First);
            Replace
              (Line,
               Padding + First - Start + 1,
               Padding + Last - Start + 1, Str);

            Padding := Padding + Str'Length - (Last - First);
         end if;
      end Replace_Text;

      --------------------
      -- Skip_Character --
      --------------------

      function Skip_Character return Boolean is
      begin
         First := Index;
         Index := Index + 1;
         Start_Char   := Char_In_Line;
         Char_In_Line := Char_In_Line + 1;

         while Index < Buffer'Last
           and then (Buffer (Index) /= '''
                     or else Buffer (Index - 1) = '\')
           and then Buffer (Index) /= ASCII.LF
         loop
            Index := UTF8_Find_Next_Char (Buffer, Index);
            Char_In_Line := Char_In_Line + 1;
         end loop;

         if Callback = null then
            return False;
         else
            return Callback
              (Character_Text, (Line, Start_Char, First),
               (Line, Char_In_Line, Index), False);
         end if;
      end Skip_Character;

      -----------------
      -- Skip_String --
      -----------------

      function Skip_String return Boolean is
      begin
         First := Index;
         Start_Char := Char_In_Line;
         Index := Index + 1;
         Char_In_Line := Char_In_Line + 1;

         while Index < Buffer'Last
           and then (Buffer (Index) /= '"'
                     or else Buffer (Index - 1) = '\')
           and then Buffer (Index) /= ASCII.LF
         loop
            Index := UTF8_Find_Next_Char (Buffer, Index);
            Char_In_Line := Char_In_Line + 1;
         end loop;

         if Callback = null then
            return False;
         else
            return Callback
              (String_Text, (Line, Start_Char, First),
               (Line, Char_In_Line, Index), False);
         end if;
      end Skip_String;

      ------------------
      -- Skip_Comment --
      ------------------

      function Skip_Comment return Boolean is
         Start_Line : Natural;
      begin
         First := Index;

         if Buffer (Index + 1) = '/' then
            --  C++ style comment, skip whole line

            Index := Index + 1;
            Start_Char := Char_In_Line;
            Char_In_Line := Char_In_Line + 1;

            while Index <= Buffer'Last
              and then Buffer (Index + 1) /= ASCII.LF
            loop
               Index := UTF8_Find_Next_Char (Buffer, Index);
               Char_In_Line := Char_In_Line + 1;
            end loop;

            if Callback = null then
               return False;
            else
               return Callback
                 (Comment_Text, (Line, Start_Char, First),
                  (Line, Char_In_Line, Index), False);
            end if;

         elsif Buffer (Index + 1) = '*' then
            --  Skip comment

            Start_Char := Char_In_Line;
            Start_Line := Line;

            Index := Index + 2;
            Char_In_Line := Char_In_Line + 2;

            if Buffer (Index) = ASCII.LF then
               New_Line;
            end if;

            --  Skip the first character, so that the following test that
            --  a '*' doesn't see the one after the opening of the
            --  comment

            Index := Index + 1;
            Char_In_Line := Char_In_Line + 1;

            while Index < Buffer'Last
              and then (Buffer (Index - 1) /= '*'
                        or else Buffer (Index) /= '/')
            loop
               if Buffer (Index) = ASCII.LF then
                  New_Line;
               end if;

               Index := UTF8_Find_Next_Char (Buffer, Index);
               Char_In_Line := Char_In_Line + 1;
            end loop;

            if Callback = null then
               return False;
            else
               return Callback
                 (Comment_Text,
                  (Start_Line, Start_Char, First),
                  (Line, Char_In_Line, Index),
                  Buffer (Index) /= '/');
            end if;
         end if;

         return False;
      end Skip_Comment;

      ----------------------------
      -- Preprocessor_Directive --
      ----------------------------

      function Preprocessor_Directive return Boolean is
         Start_Line                : Natural;
         First_Column, First_Index : Natural;
         Name_Column, Name_Index   : Natural;
         Char                      : Character;

      begin
         if Buffer'Last > Index + 4
           and then Buffer (Index + 1 .. Index + 4) = "if 0"
           and then Is_Blank (Buffer (Index + 5))
         then
            First      := Index;
            Start_Char := Char_In_Line;
            Start_Line := Line;

            Index := Index + 5;
            Char_In_Line := Char_In_Line + 5;
            Num_Ifdef := 1;

            while Index < Buffer'Last loop
               if Buffer (Index) = ASCII.LF then
                  New_Line;

               --  ??? Need to handle comments and strings
               elsif Buffer (Index) = '#' then
                  if Buffer'Last >= Index + 2
                    and then Buffer (Index + 1 .. Index + 2) = "if"
                  then
                     Char_In_Line := Char_In_Line + 2;
                     Index := Index + 2;
                     Num_Ifdef := Num_Ifdef + 1;

                  elsif Buffer'Last >= Index + 4
                    and then (Buffer (Index + 1 .. Index + 4) = "else"
                              or else Buffer (Index + 1 .. Index + 4) = "elif")
                  then
                     Char_In_Line := Char_In_Line + 4;
                     Index := Index + 4;

                     exit when Num_Ifdef = 1;

                  elsif Buffer'Last >= Index + 5
                    and then Buffer (Index + 1 .. Index + 5) = "endif"
                  then
                     Char_In_Line := Char_In_Line + 5;
                     Index := Index + 5;
                     Num_Ifdef := Num_Ifdef - 1;

                     exit when Num_Ifdef = 0;
                  end if;
               end if;

               Index := UTF8_Find_Next_Char (Buffer, Index);
               Char_In_Line := Char_In_Line + 1;
            end loop;

            if Callback = null then
               return False;
            else
               return Callback
                 (Comment_Text,
                  (Start_Line, Start_Char, First),
                  (Line, Char_In_Line, Index),
                  Num_Ifdef > 0);
            end if;

         else
            if Buffer'Last > Index + 7
              and then Buffer (Index + 1 .. Index + 7) = "include"
            then
               First_Index  := Index;
               First_Column := Char_In_Line;
               Index        := Index + 8;
               Char_In_Line := Char_In_Line + 8;

               --  ??? Need to ignore comments

               while Index < Buffer'Last
                 and then Buffer (Index) /= '"'
                 and then Buffer (Index) /= '<'
                 and then Buffer (Index) /= ASCII.LF
               loop
                  Index := UTF8_Find_Next_Char (Buffer, Index);
                  Char_In_Line := Char_In_Line + 1;
               end loop;

               if Index < Buffer'Last and then Buffer (Index) /= ASCII.LF then
                  Name_Index  := Index;
                  Name_Column := Char_In_Line;
                  Index  := Index + 1;
                  Char_In_Line := Char_In_Line + 1;

                  if Buffer (Name_Index) = '"' then
                     Char := '"';
                  else
                     Char := '>';
                  end if;

                  while Index < Buffer'Last
                    and then Buffer (Index) /= Char
                  loop
                     Index := UTF8_Find_Next_Char (Buffer, Index);
                     Char_In_Line := Char_In_Line + 1;
                  end loop;

                  if Constructs /= null then
                     declare
                        Val : Extended_Token;
                     begin
                        --  Dummy push/pop in order to register this #include
                        --  directive in the list of constructs

                        Val.Token       := Tok_Pound;
                        Val.Sloc        := (Line, First_Column, First_Index);
                        Val.Sloc_Name   := (Line, Name_Column, Name_Index);
                        Val.Name_Start  := Name_Index;
                        Val.Name_End    := Index;
                        Push (Tokens, Val);
                        Pop (Tokens);
                     end;
                  end if;
               end if;

            end if;

            --  Skip line

            while Index < Buffer'Last
              and then Buffer (Index) /= ASCII.LF
            loop
               Index := Index + 1;
            end loop;

            --  Mark this line as indented, so that the current indentation is
            --  kept.

            Indent_Done := True;
         end if;

         return False;
      end Preprocessor_Directive;

      ------------------------
      -- Identifier_Keyword --
      ------------------------

      function Identifier_Keyword return Boolean is
         Prev      : Natural := Index;
         Temp      : Extended_Token;
         Top_Token : Token_Stack.Generic_Type_Access := Top (Tokens);

      begin
         First := Index;
         Start_Char := Char_In_Line;

         while Index < Buffer'Last
           and then Is_Entity_Letter
             (UTF8_Get_Char (Buffer (Index .. Buffer'Last)))
         loop
            Prev  := Index;
            Index := UTF8_Find_Next_Char (Buffer, Index);
            Char_In_Line := Char_In_Line + 1;
         end loop;

         Index := Prev;
         Char_In_Line := Char_In_Line - 1;
         Token := Get_Token (Buffer (First .. Index));

         Temp.Token       := Token;
         Temp.Curly_Level := Curly_Level;
         Temp.Paren_Level := Paren_Level;
         Temp.Sloc.Line   := Line;
         Temp.Sloc.Column := Start_Char;
         Temp.Sloc.Index  := First;

         case Token is
            when Tok_For | Tok_Do | Tok_Switch | Tok_Else =>
               Push (Tokens, Temp);
               Do_Indent (Index, Indent);
               Indent := Indent + Indent_Level;

            when Tok_While =>
               if not (Top_Token.Token = Tok_Do
                       and then Top_Token.Curly_Level = Curly_Level
                       and then Top_Token.Paren_Level = Paren_Level)
               then
                  Push (Tokens, Temp);
                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               else
                  Do_Indent (Index, Indent);
               end if;

            when Tok_If =>
               if Prev_Token /= Tok_Else then
                  Push (Tokens, Temp);
                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               end if;

            when Tok_Case | Tok_Default =>
               if Top_Token.First then
                  Top_Token.First := False;
               else
                  Indent := Indent - Indent_Level;
               end if;

               Do_Indent (Index, Indent);
               Indent := Indent + Indent_Level;

            when Tok_Struct | Tok_Class | Tok_Enum | Tok_Union =>
               if Paren_Level = 0 then
                  Push (Tokens, Temp);
                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               end if;

            when Tok_Typedef =>
               if Constructs /= null then
                  --  ??? Register type definition
                  null;
               end if;

               Do_Indent (Index, Indent);

            when others =>
               Do_Indent (Index, Indent);
         end case;

         if Callback /= null then
            if Token = Tok_Identifier then
               return Callback
                 (Identifier_Text,
                  Temp.Sloc, (Line, Char_In_Line, Index), False);

            elsif Enable_Cpp or else Token not in Cpp_Token then
               return Callback
                 (Keyword_Text, Temp.Sloc, (Line, Char_In_Line, Index), False);
            end if;
         end if;

         return False;
      end Identifier_Keyword;

   begin  -- Analyze_C_Source
      --  Push a dummy token so that stack will never be empty.
      Push (Tokens, Default_Extended);

      --  Push a dummy indentation so that stack will never be empty.
      Push (Indents, None);

      while Index <= Buffer'Last loop
         case Buffer (Index) is
            when '{' =>
               Token := Tok_Left_Bracket;
               Do_Indent (Index, Indent);
               Top_Token := Top (Tokens);

               if Top_Token.Token = No_Token
                 or else Top_Token.Curly_Level /= Curly_Level
                 or else Top_Token.Paren_Level /= Paren_Level
               then
                  --  Record a simple block or assignment.

                  declare
                     Val : Extended_Token;
                  begin
                     if Prev_Token = Tok_Assign then
                        --  An assignment statement, e.g. int x [] = {1,2}

                        Val.Token := Tok_Assign;
                     else
                        --  An simple block, e.g: { foo (); }

                        Val.Token := Tok_Void;
                     end if;

                     Val.Curly_Level := Curly_Level;
                     Val.Paren_Level := Paren_Level;
                     Val.Sloc.Line   := Line;
                     Val.Sloc.Column :=
                       Index - Line_Start (Buffer, Index);
                     Val.Sloc.Index  := Index;
                     Indent := Indent + Indent_Level;
                     Push (Tokens, Val);
                  end;

               elsif Top_Token.Sloc.Line /= Line then
                  Top_Token.Start_New_Line := True;
                  Indent := Indent + Indent_Level;
               end if;

               Curly_Level := Curly_Level + 1;

            when '}' =>
               Token := Tok_Right_Bracket;
               Curly_Level := Curly_Level - 1;

               Top_Token := Top (Tokens);
               Indent := Indent - Indent_Level;

               if Top_Token.Token = Tok_Switch
                 and then not Top_Token.First
               then
                  Indent := Indent - Indent_Level;
               end if;

               Do_Indent (Index, Indent);

               if Top_Token.Token /= No_Token
                 and then Top_Token.Curly_Level = Curly_Level
                 and then Top_Token.Paren_Level = Paren_Level
               then
                  if Top_Token.Start_New_Line then
                     Indent := Indent - Indent_Level;
                  end if;

                  if Top_Token.Token /= Tok_Do then
                     Pop (Tokens);
                  end if;
               end if;

            when ';' =>
               Token := Tok_Semicolon;
               Top_Token := Top (Tokens);

               if Top_Token.Token /= No_Token
                 and then Top_Token.Curly_Level = Curly_Level
                 and then Top_Token.Paren_Level = Paren_Level
               then
                  if Top_Token.Token = Tok_Do then
                     Do_Indent (Index, Indent);
                  else
                     Indent := Indent - Indent_Level;
                     Do_Indent (Index, Indent);

                     if Top_Token.Start_New_Line then
                        Indent := Indent - Indent_Level;
                     end if;
                  end if;

                  Pop (Tokens);
               end if;

            when '(' =>
               Token := Tok_Left_Paren;

               --  ??? Could optimize by caching Line_Start
               Push
                 (Indents, Index - Line_Start (Buffer, Index) + Padding + 1);
               Paren_Level := Paren_Level + 1;

            when ')' =>
               Token := Tok_Right_Paren;
               Pop (Indents);
               Paren_Level := Paren_Level - 1;

            when '=' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Equal;
                     when '>' =>
                        Token := Tok_Arrow;
                     when others =>
                        Token := Tok_Assign;
                  end case;
               else
                  Token := Tok_Assign;
               end if;

            when ',' =>
               Token := Tok_Comma;

            when '.' =>
               Token := Tok_Dot;

            when '[' =>
               Token := Tok_Left_Square_Bracket;

            when ']' =>
               Token := Tok_Right_Square_Bracket;

            when '-' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Minus_Assign;
               else
                  Token := Tok_Minus;
               end if;

            when '+' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Plus_Assign;
               else
                  Token := Tok_Plus;
               end if;

            when '*' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Star_Assign;
               else
                  Token := Tok_Star;
               end if;

            when '>' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Greater_Equal;
               else
                  Token := Tok_Greater;
               end if;

            when '<' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Less_Equal;
               else
                  Token := Tok_Less;
               end if;

            when '~' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Tilde_Assign;
               else
                  Token := Tok_Tilde;
               end if;

            when '!' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Not_Equal;
               else
                  Token := Tok_Not;
               end if;

            when '%' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Percent;
               else
                  Token := Tok_Percent_Assign;
               end if;

            when '^' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Xor_Assign;
               else
                  Token := Tok_Xor;
               end if;

            when '&' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_And_Assign;
                     when '&' =>
                        Token := Tok_Logical_And;
                     when others =>
                        Token := Tok_And;
                  end case;
               else
                  Token := Tok_And;
               end if;

            when '|' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Or_Assign;
                     when '|' =>
                        Token := Tok_Logical_Or;
                     when others =>
                        Token := Tok_Or;
                  end case;
               else
                  Token := Tok_Or;
               end if;

            when '"' =>
               Token := Tok_String_Literal;
               exit when Skip_String;

            when ''' =>
               Token := Tok_Char_Literal;
               exit when Skip_Character;

            when '/' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Slash_Assign;
                     when '/' | '*' =>
                        exit when Skip_Comment;
                     when others =>
                        Token := Tok_Slash;
                  end case;
               else
                  Token := Tok_Slash;
               end if;

            when '#' =>
               Token := Tok_Pound;
               exit when Preprocessor_Directive;

            when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' =>
               exit when Identifier_Keyword;

            when others =>
               null;
         end case;

         if Is_Blank (Buffer (Index)) then
            if Buffer (Index) = ASCII.LF then
               New_Line;
            end if;
         else
            Do_Indent (Index, Indent);
         end if;

         Prev_Token := Token;
         Index := Index + 1;
         Char_In_Line := Char_In_Line + 1;
      end loop;

      if Buffer (Index) = ASCII.LF then
         New_Line;
         Do_Indent (Index, Indent);
      end if;

   exception
      when others =>
         null;
   end Analyze_C_Source;

end C_Analyzer;
