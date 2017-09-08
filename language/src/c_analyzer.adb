------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;
with String_Utils; use String_Utils;
with Indent_Stack;
with Generic_Stack;
with GNATCOLL.Utils; use GNATCOLL.Utils;
with UTF8_Utils;     use UTF8_Utils;

package body C_Analyzer is

   use Indent_Stack.Stack;

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

      Tok_Minus_Minus,           -- --
      Tok_Plus_Plus,             -- ++

      Tok_Star,                  -- *
      Tok_Star_Assign,           -- *=
      Tok_Slash,                 -- /
      Tok_Slash_Assign,          -- /=

      Tok_Dot,                   -- .
      Tok_Deref_Select,          -- ->
      Tok_Semicolon,             -- ;
      Tok_Colon,                 -- :

      Tok_Question_Mark,         -- ?
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

      Tok_Righ_Shift,            -- >>
      Tok_Righ_Shift_Assign,     -- >>=
      Tok_Left_Shift,            -- <<
      Tok_Left_Shift_Assign,     -- <<=

      --  Reserved words for C:

      --  Type specifiers:
      Tok_Char,
      Tok_Double,
      Tok_Float,
      Tok_Int,
      Tok_Long,
      Tok_Short,
      Tok_Signed,
      Tok_Unsigned,
      Tok_Void,
      Tok_Typedef,

      --  Storage-class specifiers:
      Tok_Auto,
      Tok_Const,
      Tok_Extern,
      Tok_Static,
      Tok_Register,
      Tok_Restrict,
      Tok_Volatile,

      --  Construct keywords:
      Tok_Break,
      Tok_Case,
      Tok_Continue,
      Tok_Default,
      Tok_Do,
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

   subtype Type_Token is Token_Type range Tok_Char .. Tok_Unsigned;

   subtype Storage_Token is Token_Type range Tok_Auto .. Tok_Volatile;

   type Token_Set is array (Token_Type) of Boolean;
   pragma Pack (Token_Set);

   Indent_Separate_Line : Token_Set :=
     (Tok_Struct | Tok_Class | Tok_Union | Tok_Namespace => False,
      others                                             => True);
   --  True when a token construct uses extra indentation level when '{'
   --  is on a separate line.

   type Extended_Token is record
      Token          : Token_Type := No_Token;
      --  Enclosing token

      Start_New_Line : Boolean := False;
      --  Set to True if the first '{' for the current construct was found
      --  its own line (only used internally).

      Paren_Level    : Natural := 0;
      Curly_Level    : Natural := 0;
      --  Saved paren and curly levels for the current construct

      First          : Boolean := True;
      --  Flag used to mark the first use of this construct, e.g. the first
      --  'case' of a 'switch'.

      Sloc           : Source_Location;
      --  Source location for this entity

      Sloc_Name      : Source_Location;
      --  Source location for the name of this entity, if relevant

      Name_Start     : Natural := 0;
      Name_End       : Natural := 0;
      --  Location in the buffer of the entity name, if any
   end record;
   --  Extended information for a token

   package Token_Stack is new Generic_Stack (Extended_Token);
   use Token_Stack;

   ----------------------
   -- Local procedures --
   ----------------------

   procedure Pop_To_Construct
     (Stack : in out Token_Stack.Simple_Stack;
      Item  : out Token_Stack.Generic_Type_Access);
   --  Return first token from Stack that is not an identifier nor a
   --  type or storage token kind. This Item is not removed from the stack.
   --  Items before the item found are removed from the stack.

   function Get_Token (S : String) return Token_Type;
   --  Return a Token_Type given a string

   ----------------------
   -- Pop_To_Construct --
   ----------------------

   procedure Pop_To_Construct
     (Stack : in out Token_Stack.Simple_Stack;
      Item  : out Token_Stack.Generic_Type_Access) is
   begin
      loop
         Item := Top (Stack);
         exit when Item.Token = No_Token
           or else (Item.Token /= Tok_Identifier
                    and then Item.Token not in Type_Token
                    and then Item.Token not in Storage_Token);
         Pop (Stack);
      end loop;
   end Pop_To_Construct;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (S : String) return Token_Type is
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
      Symbols          : GNATCOLL.Symbols.Symbol_Table_Access;
      Indent_Params    : Indent_Parameters;
      Format           : Boolean               := True;
      From, To         : Natural               := 0;
      Replace          : Replace_Text_Callback := null;
      Constructs       : Construct_List_Access := null;
      Callback         : Entity_Callback       := null;
      Enable_Cpp       : Boolean               := False)
   is
      None              : constant := -1;

      Default_Extended  : Extended_Token;
      pragma Warnings (Off, Default_Extended);
      --  Use default values to initialize this pseudo constant.

      Indent_Level      : Natural renames Indent_Params.Indent_Level;
      Indent_Continue   : Natural renames Indent_Params.Indent_Level;
      Indent_Comments   : Boolean renames Indent_Params.Indent_Comments;
      Indent_Extra      : Boolean renames Indent_Params.Align_On_Colons;
      Use_Tabs          : Boolean renames Indent_Params.Use_Tabs;

      First             : Natural;
      Index             : Natural := Buffer'First;
      Paren_Index       : Natural;
      Indent            : Natural := 0;
      Indent_Done       : Boolean := False;
      Indentation       : Natural := 0;
      Token             : Token_Type := No_Token;
      Prev_Token        : Token_Type := No_Token;
      Curly_Level       : Integer := 0;
      Continuation_Val  : Integer := 0;
      Paren_Level       : Integer := 0;
      Line              : Natural := 1;
      Padding           : Integer := 0;
      Char_In_Line      : Natural := 1;
      --  Current byte index in current line, different from logical column
      --  which is computed by the callers directly. In other words, callers
      --  expect byte counts for the column information.

      Start_Char        : Natural;
      Start_Index       : Natural;
      Num_Ifdef         : Natural;
      Last_Replace_Line : Natural := 0;
      Top_Token         : Token_Stack.Generic_Type_Access;
      Enclosing         : Token_Stack.Generic_Type_Access;
      Tok_Ident         : Extended_Token;
      Tokens            : Token_Stack.Simple_Stack;
      Indents           : Indent_Stack.Stack.Simple_Stack;
      Main_File         : String_Access;

      function Do_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Centralized routine used to perform the callback. It remembers
      --  in the local variable Callback_Done if the call has been done
      --  (and thus avoid performing the call twice with the same token).

      procedure Do_Indent
        (P            : Natural;
         Num_Spaces   : Integer;
         Continuation : Boolean := False);
      --  Perform indentation by inserting spaces in the buffer at position P.
      --  If Continuation is True, Indent_Continue extra spaces are added.

      procedure New_Line;
      --  Increment line counter and line-related states.
      --  Should be called each time a new line is detected.

      procedure Next_Char;
      --  Increment the scanner index

      function Identifier_Keyword return Boolean;
      --  Handle next identifier or keyword.
      --  Assume that Buffer (Index) is an identifier character.
      --  Return whether processing should be stopped.

      procedure Pop_Constructs_And_Indent;
      --  Pop non construct items in the stack and indent current line.
      --  This procedure will typically be called after a ';' or '='.

      function Preprocessor_Directive return Boolean;
      --  Handle preprocessor directive.
      --  Assume that Buffer (Index) = '#'
      --  For now, handle #if 0 as a multiple-line comment, recognize
      --  #include directives and # <number> <file> directives.
      --  Return whether processing should be stopped.

      procedure Replace_Text
        (First : Natural;
         Last  : Natural;
         Str   : String);
      --  Wrapper for Replace.all, taking (From, To) into account.

      procedure Set_Paren_Index;
      --  Set Paren_Index according to the current indentation level and
      --  Idents.

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

      -----------------
      -- Do_Callback --
      -----------------

      Callback_Done : Boolean;

      function Do_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is
      begin
         if Callback = null then
            return False;
         else
            Callback_Done := True;
            return Callback
              (Entity, Sloc_Start, Sloc_End, Partial_Entity);
         end if;
      end Do_Callback;

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent
        (P            : Natural;
         Num_Spaces   : Integer;
         Continuation : Boolean := False)
      is
         Start : Natural;
         Index : Natural;
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

         if Top (Indents).Level = None then
            Indentation := Num_Spaces;
         else
            Indentation := Top (Indents).Level;
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

      ---------------
      -- Next_Char --
      ---------------

      procedure Next_Char is
         P : constant Natural := Index;
      begin
         Index := UTF8_Next_Char (Buffer, Index);
         Char_In_Line := Char_In_Line + Index - P;
      end Next_Char;

      ---------
      -- Pop --
      ---------

      procedure Pop
        (Stack : in out Token_Stack.Simple_Stack;
         Value : out Extended_Token)
      is
         Column : Natural;
         Info   : Construct_Access;
         Item   : Token_Stack.Generic_Type_Access;

      begin
         Pop_To_Construct (Stack, Item);

         if Item.Token = No_Token then
            --  No more token on the stack
            Value := Item.all;
            return;
         else
            --  There is an item on the stack, read it now
            Token_Stack.Pop (Stack, Value);
         end if;

         --  Build next entry of Constructs if needed.

         if Constructs /= null then
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

            Constructs.Size := Constructs.Size + 1;
            Constructs.Last := Constructs.Current;

            case Value.Token is
               when Tok_Pound =>
                  --  Tok_Pound is used for #include

                  Constructs.Current.Info.Category := Cat_Include;
                  Constructs.Current.Info.Name := Symbols.Find
                    (Buffer (Value.Name_Start .. Value.Name_End));
                  Constructs.Current.Info.Sloc_Entity := Value.Sloc_Name;

               when Tok_Do | Tok_For | Tok_While =>
                  Constructs.Current.Info.Category := Cat_Loop_Statement;
               when Tok_If | Tok_Else =>
                  Constructs.Current.Info.Category := Cat_If_Statement;
               when Tok_Switch =>
                  Constructs.Current.Info.Category := Cat_Case_Statement;
               when Tok_Void =>
                  --  Tok_Void is used for blocks: {}

                  if Value.Name_Start /= 0 then
                     --  ??? Would be nice to be able to find the parameters
                     --  of this function

                     Constructs.Current.Info.Category := Cat_Function;
                     Constructs.Current.Info.Name := Symbols.Find
                       (Buffer (Value.Name_Start .. Value.Name_End));

                  else
                     Constructs.Current.Info.Category := Cat_Simple_Block;
                  end if;

               when Tok_Struct =>
                  Constructs.Current.Info.Category := Cat_Structure;
                  Constructs.Current.Info.Name := Symbols.Find
                    (Buffer (Value.Name_Start .. Value.Name_End));
               when Tok_Union =>
                  Constructs.Current.Info.Category := Cat_Union;
                  Constructs.Current.Info.Name := Symbols.Find
                    (Buffer (Value.Name_Start .. Value.Name_End));
               when Tok_Class =>
                  Constructs.Current.Info.Category := Cat_Class;
                  Constructs.Current.Info.Name := Symbols.Find
                    (Buffer (Value.Name_Start .. Value.Name_End));
               when Tok_Enum =>
                  Constructs.Current.Info.Category := Cat_Type;
               when others =>
                  Constructs.Current.Info.Category := Cat_Unknown;
            end case;

            Constructs.Current.Info.Sloc_Start     := Value.Sloc;
            Constructs.Current.Info.Sloc_End       := (Line, Column, Index);
            Constructs.Current.Info.Is_Declaration := False;
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
         Escape : Boolean := False;
      begin
         First := Index;
         Start_Char   := Char_In_Line;
         Next_Char;

         while Index < Buffer'Last loop
            case Buffer (Index) is
               when ASCII.LF =>
                  exit when not Escape;
                  Escape := False;
                  New_Line;

               when ''' =>
                  exit when not Escape;
                  Escape := False;

               when '\' =>
                  Escape := not Escape;

               when others =>
                  Escape := False;
            end case;

            Next_Char;
         end loop;

         return Do_Callback
           (Character_Text, (Line, Start_Char, First),
            (Line, Char_In_Line, Index), False);
      end Skip_Character;

      -----------------
      -- Skip_String --
      -----------------

      function Skip_String return Boolean is
         Escape : Boolean := False;
      begin
         First := Index;
         Start_Char := Char_In_Line;
         Next_Char;

         while Index < Buffer'Last loop
            case Buffer (Index) is
               when ASCII.LF =>
                  exit when not Escape;
                  Escape := False;
                  New_Line;

               when '"' =>
                  exit when not Escape;
                  Escape := False;

               when '\' =>
                  Escape := not Escape;

               when others =>
                  Escape := False;
            end case;

            Next_Char;
         end loop;

         return Do_Callback
           (String_Text, (Line, Start_Char, First),
            (Line, Char_In_Line, Index), False);
      end Skip_String;

      ------------------
      -- Skip_Comment --
      ------------------

      function Skip_Comment return Boolean is
         Start_Line    : Natural;  --  Line starting the comment
         Ref_Column    : Natural;  --  First column with non blank char
         Indent_Normal : Integer := 0;    --  Number of spaces to use when
                                          --  indenting comments
         Indent_Star   : Integer := 0;    --  Ditto, when the first non blank
                                          --  char is a '*'
         Line_Reset    : Boolean;         --  Whether we have a newline that
                                          --  needs indentation.
         Column        : Natural;         --  Current column, taking into
                                          --  account TAB characters.
         Last_Blank    : Natural;         --  Last blank column on the first
                                          --  comment line.
         All_Stars     : Boolean;
         Tmp           : Natural;

         procedure Add_Tab (Value : in out Natural);
         --  Increment Value by a TAB character (up to 8 chars).
         pragma Inline (Add_Tab);

         -------------
         -- Add_Tab --
         -------------

         procedure Add_Tab (Value : in out Natural) is
         begin
            Value := 8 * (Value / 8 + 1);
         end Add_Tab;

      begin
         First := Index;

         if Buffer (Index + 1) = '/' then
            --  C++ style comment, skip whole line

            Start_Char := Char_In_Line;
            Next_Char;

            while Index <= Buffer'Last
              and then Buffer (UTF8_Next_Char (Buffer, Index)) /= ASCII.LF
            loop
               Next_Char;
            end loop;

            if Indent_Comments then
               Do_Indent (Index, Indent);
            end if;

            return Do_Callback
              (Comment_Text, (Line, Start_Char, First),
               (Line, Char_In_Line, Index), False);

         elsif Buffer (Index + 1) = '*' then
            --  Skip and indent C-style multi-line comment

            Start_Char := Char_In_Line;
            Start_Line := Line;
            Column     := Char_In_Line;
            Last_Blank := 0;

            if Indent_Comments then
               Do_Indent (Index, Indent);
               Tmp := Line_Start (Buffer, Index);

               loop
                  case Buffer (Tmp) is
                     when ' ' =>
                        Last_Blank := Last_Blank + 1;
                     when ASCII.HT =>
                        Add_Tab (Last_Blank);
                     when others =>
                        exit;
                  end case;

                  Tmp := Tmp + 1;
               end loop;

               Column := Last_Blank;

               while Tmp <= Index loop
                  if Buffer (Tmp) = ASCII.HT then
                     Add_Tab (Column);
                  else
                     Column := Column + 1;
                  end if;

                  Tmp := Tmp + 1;
               end loop;

               Indent_Star := Column + Indentation - Last_Blank;
            end if;

            Index := Index + 2;
            Char_In_Line := Char_In_Line + 2;
            Column := Column + 2;

            if Buffer (Index) = ASCII.LF then
               Line_Reset := True;
               Ref_Column := Column + 1;
               Indent_Normal := Indent_Star + 2;
               New_Line;
               Index := Index + 1;
               Char_In_Line := 1;
               Column := 1;

            else
               while Index < Buffer'Last loop
                  case Buffer (Index) is
                     when ' ' =>
                        Column := Column + 1;
                     when ASCII.HT =>
                        Add_Tab (Column);
                     when others =>
                        exit;
                  end case;

                  Next_Char;
               end loop;

               Ref_Column := Column;
               Indent_Normal := Ref_Column + Indentation - Last_Blank - 1;
               Line_Reset := False;

               if Char_In_Line = Start_Char + 2 then
                  --  Skip the first character, so that the following test that
                  --  a '*' doesn't see the one after the opening of the
                  --  comment

                  Next_Char;
                  Column := Column + 1;
               end if;
            end if;

            All_Stars := True;

            while Index < Buffer'Last
              and then (Buffer (Index - 1) /= '*'
                        or else Buffer (Index) /= '/')
            loop
               if Buffer (Index) /= ' '
                 and then Buffer (Index) /= ASCII.HT
               then
                  if Buffer (Index) = ASCII.LF then
                     New_Line;
                     Line_Reset := True;
                     Column := 0;

                  elsif Line_Reset and Indent_Comments then
                     --  Keep all '*' chars aligned

                     if Buffer (Index) = '*'
                       and then (All_Stars or else Buffer (Index + 1) = '/')
                     then
                        Do_Indent (Index, Indent_Star);
                     else
                        All_Stars := False;

                        if Column > Ref_Column then
                           Do_Indent
                             (Index, Indent_Normal + Column - Ref_Column);
                        else
                           Do_Indent (Index, Indent_Normal);
                        end if;
                     end if;

                     Line_Reset := False;
                  end if;
               end if;

               if Buffer (Index) = ASCII.HT then
                  Add_Tab (Column);
               end if;

               Column := Column + 1;
               Next_Char;
            end loop;

            return Do_Callback
              (Comment_Text,
               (Start_Line, Start_Char, First),
               (Line, Char_In_Line, Index),
               Buffer (Index) /= '/');
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
         First_Token_In_Line       : Boolean;

      begin
         if Buffer'Last > Index + 4
           and then Buffer (Index + 1 .. Index + 4) = "if 0"
           and then Is_Blank (Buffer (Index + 5))
         then
            --  Handle #if 0 ... #endif block as a comment

            First      := Index;
            Start_Char := Char_In_Line;
            Start_Line := Line;

            Index := Index + 5;
            Char_In_Line := Char_In_Line + 5;
            Num_Ifdef := 1;
            First_Token_In_Line := False;

            while Index < Buffer'Last loop
               if Buffer (Index) = ASCII.LF then
                  New_Line;
                  First_Token_In_Line := True;

               --  ??? Need to handle comments and strings
               elsif First_Token_In_Line then
                  if Buffer (Index) = '#' then
                     First_Token_In_Line := False;

                     if Buffer'Last >= Index + 2
                       and then Buffer (Index + 1 .. Index + 2) = "if"
                     then
                        Char_In_Line := Char_In_Line + 2;
                        Index := Index + 2;
                        Num_Ifdef := Num_Ifdef + 1;

                     elsif Buffer'Last >= Index + 4
                       and then (Buffer (Index + 1 .. Index + 4) = "else"
                                 or else Buffer (Index + 1 .. Index + 4)
                                   = "elif")
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

                  elsif not Is_Blank (Buffer (Index)) then
                     First_Token_In_Line := False;
                  end if;
               end if;

               Next_Char;
            end loop;

            return Do_Callback
              (Comment_Text,
               (Start_Line, Start_Char, First),
               (Line, Char_In_Line, Index),
               Num_Ifdef > 0);

         else
            if Buffer'Last > Index + 7
              and then Buffer (Index + 1 .. Index + 7) = "include"
            then
               --  #include directive

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
                  Next_Char;
               end loop;

               if Index < Buffer'Last and then Buffer (Index) /= ASCII.LF then
                  Name_Index  := Index;
                  Name_Column := Char_In_Line;
                  Next_Char;

                  if Buffer (Name_Index) = '"' then
                     Char := '"';
                  else
                     Char := '>';
                  end if;

                  while Index < Buffer'Last
                    and then Buffer (Index) /= Char
                  loop
                     Next_Char;
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
            elsif Buffer'Last > Index + 3
              and then Buffer (Index + 1) = ' '
              and then Buffer (Index + 2) in '1' .. '9'
            then
               loop
                  --  # <num> <file> directive
                  Index := Index + 2;
                  Start_Line :=
                    Character'Pos (Buffer (Index)) - Character'Pos ('0');
                  Index := Index + 1;

                  while Index <= Buffer'Last
                    and then Buffer (Index) in '0' .. '9'
                  loop
                     Index := Index + 1;
                     Start_Line := Start_Line * 10 +
                       Character'Pos (Buffer (Index)) - Character'Pos ('0');
                  end loop;

                  Skip_Blanks (Buffer, Index);
                  pragma Assert (Buffer (Index) = '"');
                  Index := Index + 1;
                  First_Index := Index;

                  while Index <= Buffer'Last
                    and then Buffer (Index) /= '"'
                  loop
                     Index := Index + 1;
                  end loop;

                  if Main_File = null then
                     Main_File :=
                       new String'(Buffer (First_Index .. Index - 1));
                  elsif Buffer (First_Index .. Index - 1) = Main_File.all then
                     Indent_Done := False;
                     Line := Start_Line;
                     Char_In_Line := 0;
                     Index := Index + 1;
                     Skip_To_Char (Buffer, Index, ASCII.LF);

                     if Index < Buffer'Last then
                        Index := Index + 1;
                     end if;

                     return False;
                  end if;

                  loop
                     --  Skip until reaching a # <num> <file> directive
                     Skip_To_Char (Buffer, Index, '#');

                     if Index + 4 > Buffer'Last then
                        return False;
                     end if;

                     exit when  Buffer (Index - 1) = ASCII.LF
                       and then Buffer (Index + 1) = ' '
                       and then Buffer (Index + 2) in '1' .. '9';

                     Index := Index + 1;
                  end loop;
               end loop;
            end if;

            --  Skip line and possible continuation lines (when using
            --  backslash before and end-of-line char)

            while Index < Buffer'Last loop
               if Buffer (Index) = ASCII.LF then
                  if Buffer (Index - 1) = '\' then
                     New_Line;
                  else
                     exit;
                  end if;
               end if;

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
         Top_Token : Token_Stack.Generic_Type_Access;
         C         : Wide_Wide_Character;

      begin
         Pop_To_Construct (Tokens, Top_Token);
         First := Index;
         Start_Char := Char_In_Line;

         while Index < Buffer'Last loop
            C := UTF8_Get_Char (Buffer (Index .. Buffer'Last));
            exit when C /= '$' and then not Is_Entity_Letter (C);

            Prev := Index;
            Next_Char;
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
         Temp.Name_Start  := First;
         Temp.Name_End    := Index;

         case Token is
            when Tok_For | Tok_Do | Tok_Switch | Tok_Else =>
               Push (Tokens, Temp);
               Do_Indent (Index, Indent);
               Indent := Indent + Indent_Level;

            when Tok_While =>
               if Top_Token.Token = Tok_Do
                 and then Top_Token.Curly_Level = Curly_Level
                 and then Top_Token.Paren_Level = Paren_Level
               then
                  Do_Indent (Index, Indent);
               else
                  Push (Tokens, Temp);
                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               end if;

            when Tok_If =>
               if Prev_Token /= Tok_Else then
                  Push (Tokens, Temp);
                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               end if;

            when Tok_Case | Tok_Default =>
               if Prev_Token /= Tok_Left_Paren then
                  if Top_Token.First then
                     Top_Token.First := False;
                  else
                     Indent := Indent - Indent_Level;
                  end if;

                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               end if;

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

            when Type_Token | Storage_Token =>
               if Curly_Level = 0 then
                  Push (Tokens, Temp);
               end if;

               Do_Indent (Index, Indent);

            when Tok_Identifier =>
               if Curly_Level = 0
                 or else Top_Token.Token in Tok_Class | Tok_Struct | Tok_Union
               then
                  --  Only record identifier outside function body, we only
                  --  record them to be able to retrieve subprogram name.

                  Push (Tokens, Temp);

                  --  save the name of the struct in the Struct token itself
                  if Curly_Level = 0
                    and then Top_Token.Token
                      in Tok_Struct | Tok_Class | Tok_Union
                  then
                     Top_Token.Name_Start := Temp.Name_Start;
                     Top_Token.Name_End   := Temp.Name_End;
                  end if;
               end if;

               Do_Indent (Index, Indent);

            when Tok_Public | Tok_Protected | Tok_Private =>
               --  Need to unindent temporarily:
               --  class t
               --  {
               --    int foo;
               --  protected:  <--
               --    int bar;
               --  }

               if Curly_Level = 1 then
                  Indent := Indent - Indent_Level;
                  Do_Indent (Index, Indent);
                  Indent := Indent + Indent_Level;
               else
                  Do_Indent (Index, Indent);
               end if;

            when others =>
               Do_Indent (Index, Indent);
         end case;

         if Callback /= null then
            if Token = Tok_Identifier then
               return Do_Callback
                 (Identifier_Text,
                  Temp.Sloc, (Line, Char_In_Line, Index), False);

            elsif Enable_Cpp or else Token not in Cpp_Token then
               return Do_Callback
                 (Keyword_Text, Temp.Sloc, (Line, Char_In_Line, Index), False);
            end if;
         end if;

         return False;
      end Identifier_Keyword;

      ---------------------
      -- Set_Paren_Index --
      ---------------------

      procedure Set_Paren_Index is
         First_Non_Blank : Natural := Line_Start (Buffer, Index);
         Local_Indent    : Natural;
         Indents_Level   : constant Integer := Top (Indents).Level;
      begin
         if Indents_Level = None
           or else Top (Indents).Line = Line
         then
            Local_Indent := Indentation;
         else
            Local_Indent := Indents_Level;
         end if;

         Skip_Blanks (Buffer, First_Non_Blank);
         Paren_Index := Index - First_Non_Blank + Local_Indent + 1;
      end Set_Paren_Index;

      -------------------------------
      -- Pop_Constructs_And_Indent --
      -------------------------------

      procedure Pop_Constructs_And_Indent is
      begin
         loop
            Pop_To_Construct (Tokens, Top_Token);

            exit when Top_Token.Token = No_Token
              or else Top_Token.Curly_Level /= Curly_Level
              or else Top_Token.Paren_Level /= Paren_Level;

            if Top_Token.Token = Tok_Do then
               Indent := Indent - Indent_Level;
               Do_Indent (Index, Indent);
            else
               Indent := Indent - Indent_Level;
               Do_Indent (Index, Indent);

               if Top_Token.Start_New_Line then
                  Indent := Indent - Indent_Level;
               end if;
            end if;

            Pop (Tokens);
         end loop;
      end Pop_Constructs_And_Indent;

      --  Local variables

      Some_Token_Found : Boolean;

   --  Start of processing for Analyze_C_Source

   begin  -- Analyze_C_Source
      Indent_Separate_Line (Tok_If)     := Indent_Extra;
      Indent_Separate_Line (Tok_Else)   := Indent_Extra;
      Indent_Separate_Line (Tok_For)    := Indent_Extra;
      Indent_Separate_Line (Tok_While)  := Indent_Extra;
      Indent_Separate_Line (Tok_Do)     := Indent_Extra;
      Indent_Separate_Line (Tok_Switch) := Indent_Extra;

      --  Push a dummy token so that stack will never be empty.
      Push (Tokens, Default_Extended);

      --  Push a dummy indentation so that stack will never be empty.
      Push (Indents, (None, 0, 0, 0));

      while Index <= Buffer'Last loop
         Start_Char  := Char_In_Line;
         Start_Index := Index;

         Some_Token_Found := True;
         Callback_Done := False;

         case Buffer (Index) is
            when '{' =>
               Token := Tok_Left_Bracket;

               Enclosing := Top (Tokens);

               if Enclosing.Token = Tok_Identifier then
                  Enclosing := Next (Tokens);
               end if;

               if not Indent_Separate_Line (Enclosing.Token)
                 and then Enclosing.Curly_Level = Curly_Level
               then
                  Indent := Indent - Indent_Level;
               end if;

               Do_Indent (Index, Indent);
               Pop_To_Construct (Tokens, Top_Token);

               if Top_Token.Token = No_Token
                 or else Top_Token.Curly_Level /= Curly_Level
                 or else Top_Token.Paren_Level /= Paren_Level
               then
                  --  Record a simple block or assignment.

                  if Prev_Token = Tok_Assign
                    or else Top (Indents).Level /= None
                  then
                     --  An assignment statement, e.g. int x [] = {1,2}

                     if Indent_Done then
                        Set_Paren_Index;
                     else
                        Paren_Index := Indent + Indent_Continue;
                     end if;

                     Push (Indents, (Paren_Index, 0, Line, 0));
                     Do_Indent (Index, Indent);

                  else
                     --  A simple block, e.g: { foo (); }

                     declare
                        Val : Extended_Token;
                     begin
                        Val.Token := Tok_Void;

                        if Curly_Level = 0
                          or else Top_Token.Token = Tok_Class
                          or else Top_Token.Token = Tok_Struct
                        then
                           --  This is a top level curly brace, it defines a
                           --  function, use Tok_Ident as the function name.
                           --  Tok_Ident has been set while parsing a '('
                           --  see below.

                           if Tok_Ident.Token = Tok_Identifier then
                              Val.Name_Start  := Tok_Ident.Name_Start;
                              Val.Name_End    := Tok_Ident.Name_End;
                              --  We do not want to reuse this value
                              Tok_Ident.Token := Tok_Void;
                           end if;
                        end if;

                        Val.Curly_Level := Curly_Level;
                        Val.Paren_Level := Paren_Level;
                        Val.Sloc.Line   := Line;
                        Val.Sloc.Column :=
                          Index - Line_Start (Buffer, Index) + 1;
                        Val.Sloc.Index  := Index;
                        Indent := Indent + Indent_Level;
                        Push (Tokens, Val);
                     end;
                  end if;

               elsif not Indent_Separate_Line (Enclosing.Token) then
                  Indent := Indent + Indent_Level;

               elsif Top_Token.Sloc.Line /= Line then
                  Top_Token.Start_New_Line := True;
                  Indent := Indent + Indent_Level;
               end if;

               Curly_Level := Curly_Level + 1;

            when '}' =>
               Token := Tok_Right_Bracket;

               --  Ignore extra curlys (may happen with syntax errors or
               --  with preprocessor directives, as in:
               --  struct {
               --  #ifdef FOO
               --  }
               --  #else
               --  }
               --  #endif

               if Curly_Level > 0 then
                  Curly_Level := Curly_Level - 1;

                  if Top (Indents).Level /= None then
                     Pop (Indents);
                  else
                     Pop_To_Construct (Tokens, Top_Token);
                     Indent := Indent - Indent_Level;

                     if Top_Token.Token = Tok_Switch
                       and then not Top_Token.First
                     then
                        Indent := Indent - Indent_Level;
                     end if;

                     Do_Indent (Index, Indent);

                     declare
                        First : Boolean := True;
                     begin
                        while Top_Token.Token /= No_Token
                          and then Top_Token.Curly_Level = Curly_Level
                          and then Top_Token.Paren_Level = Paren_Level
                        loop
                           if First then
                              First := False;
                           else
                              Indent := Indent - Indent_Level;
                           end if;

                           if Top_Token.Start_New_Line then
                              Indent := Indent - Indent_Level;
                           end if;

                           if Top_Token.Token = Tok_Do then
                              Indent := Indent + Indent_Level;
                              exit;
                           end if;

                           Pop (Tokens);
                           Pop_To_Construct (Tokens, Top_Token);
                        end loop;
                     end;
                  end if;
               end if;

            when ';' =>
               Token := Tok_Semicolon;
               Pop_Constructs_And_Indent;

            when '(' =>
               Token := Tok_Left_Paren;

               --  Get identifier just before the parenthesis, this is usually
               --  the name of the function (if this is a function definition)
               --  ??? This will not handle macros such as:
               --  int foo PARAMS ((void*)) {}

               if Paren_Level = 0 then
                  Tok_Ident := Token_Stack.Top (Tokens).all;
               end if;

               Pop_To_Construct (Tokens, Top_Token);

               if (Top_Token.Token = Tok_Enum
                   or else Top_Token.Token = Tok_Struct
                   or else Top_Token.Token = Tok_Class
                   or else Top_Token.Token = Tok_Union)
                 and then Top_Token.Curly_Level = Curly_Level
                 and then Top_Token.Paren_Level = Paren_Level
               then
                  Pop (Tokens);
                  Indent := Indent - Indent_Level;
               end if;

               if Top (Indents).Level /= None then
                  Do_Indent (Index, Indent);
               end if;

               if Indent_Done then
                  Set_Paren_Index;
               else
                  if Prev_Token = Tok_Identifier then
                     --  Open parenthesis on start of line, e.g:
                     --  foo
                     --    (bar);

                     Paren_Index := Indent + Indent_Continue;
                  else
                     Paren_Index := Indent;
                  end if;
               end if;

               --  ??? Could optimize by caching Line_Start

               Push (Indents, (Paren_Index, 0, Line, 0));
               Do_Indent (Index, Indent);
               Paren_Level := Paren_Level + 1;

            when ')' =>
               Token := Tok_Right_Paren;

               if Paren_Level > 0 then
                  Pop (Indents);
                  Paren_Level := Paren_Level - 1;
               end if;

            when '=' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Equal;
                        Next_Char;
                     when others =>
                        Token := Tok_Assign;
                  end case;
               else
                  Token := Tok_Assign;
               end if;

               if Token = Tok_Assign
                 and then Curly_Level = 0
                 and then Top (Tokens).Token = Tok_Identifier
               then
                  Pop_Constructs_And_Indent;
               end if;

            when ':' =>
               Token := Tok_Colon;

            when '?' =>
               Token := Tok_Question_Mark;

            when ',' =>
               Token := Tok_Comma;

            when '.' =>
               Token := Tok_Dot;

            when '[' =>
               Token := Tok_Left_Square_Bracket;

            when ']' =>
               Token := Tok_Right_Square_Bracket;

            when '-' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Minus_Assign;
                        Next_Char;
                     when '>' =>
                        Token := Tok_Deref_Select;
                        Next_Char;
                     when '-' =>
                        Token := Tok_Minus_Minus;
                        Next_Char;
                     when others =>
                        Token := Tok_Minus;
                  end case;
               else
                  Token := Tok_Minus;
               end if;

            when '+' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Plus_Assign;
                        Next_Char;
                     when '+' =>
                        Token := Tok_Plus_Plus;
                        Next_Char;
                     when others =>
                        Token := Tok_Plus;
                  end case;
               else
                  Token := Tok_Plus;
               end if;

            when '*' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Star_Assign;
                  Next_Char;
               else
                  Token := Tok_Star;
               end if;

            when '>' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Greater_Equal;
                        Next_Char;
                     when '>' =>
                        if (Index + 1) < Buffer'Last
                          and then Buffer (Index + 2) = '='
                        then
                           Token := Tok_Righ_Shift_Assign;
                           Next_Char;
                        else
                           Token := Tok_Righ_Shift;
                        end if;
                        Next_Char;

                     when others =>
                        Token := Tok_Greater;
                  end case;
               else
                  Token := Tok_Greater;
               end if;

            when '<' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_Less_Equal;
                        Next_Char;
                     when '<' =>
                        if (Index + 1) > Buffer'Last
                          and then Buffer (Index + 2) = '='
                        then
                           Token := Tok_Left_Shift_Assign;
                           Next_Char;
                        else
                           Token := Tok_Left_Shift;
                        end if;
                        Next_Char;

                     when others =>
                        Token := Tok_Less;
                  end case;
               else
                  Token := Tok_Less;
               end if;

            when '~' =>
               Token := Tok_Tilde;

            when '!' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Not_Equal;
                  Next_Char;
               else
                  Token := Tok_Not;
               end if;

            when '%' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Percent;
                  Next_Char;
               else
                  Token := Tok_Percent_Assign;
               end if;

            when '^' =>
               if Index < Buffer'Last and then Buffer (Index + 1) = '=' then
                  Token := Tok_Xor_Assign;
                  Next_Char;
               else
                  Token := Tok_Xor;
               end if;

            when '&' =>
               if Index < Buffer'Last then
                  case Buffer (Index + 1) is
                     when '=' =>
                        Token := Tok_And_Assign;
                        Next_Char;
                     when '&' =>
                        Token := Tok_Logical_And;
                        Next_Char;
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
                        Next_Char;
                     when '|' =>
                        Token := Tok_Logical_Or;
                        Next_Char;
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
                        Next_Char;
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
               Some_Token_Found := False;
         end case;

         if not Callback_Done
           and then Some_Token_Found
           and then Token in Tok_Minus .. Tok_Left_Shift_Assign
         then
            exit when Do_Callback
              (Entity         => Operator_Text,
               Sloc_Start     => (Line, Start_Char, Start_Index),
               Sloc_End       => (Line, Char_In_Line, Index),
               Partial_Entity => False);
         end if;

         if Is_Blank (Buffer (Index)) then
            if Buffer (Index) = ASCII.LF then
               if Index = Buffer'Last then
                  Do_Indent (Index, Indent);
               end if;

               New_Line;
            end if;
         else
            Do_Indent (Index, Indent);
         end if;

         Prev_Token := Token;
         Next_Char;
      end loop;

      Free (Main_File);
      Clear (Tokens);
      Clear (Indents);

   exception
      when others =>
         Free (Main_File);
         Clear (Tokens);
         Clear (Indents);
   end Analyze_C_Source;

end C_Analyzer;
