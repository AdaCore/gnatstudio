-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                              AdaCore                              --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Glib.Unicode;            use Glib.Unicode;

with Generic_Stack;
with Indent_Stack;
with String_Utils;            use String_Utils;

package body Ada_Analyzer is

   use Indent_Stack.Stack;

   pragma Suppress (All_Checks);
   --  For efficiency

   -----------------
   -- Local types --
   -----------------

   type Token_Type is
     (
      --  Token name          Token type   Class(es)

      Tok_Integer_Literal, -- numeric lit  Literal, Lit_Or_Name

      Tok_String_Literal,  -- string lit   Literal. Lit_Or_Name

      Tok_Char_Literal,    -- char lit     Name, Literal. Lit_Or_Name

      Tok_Operator_Symbol, -- op symbol    Name, Literal, Lit_Or_Name, Desig

      Tok_Identifier,      -- identifer    Name, Lit_Or_Name, Desig

      Tok_Double_Asterisk, -- **

      Tok_Minus,           -- -            Binary_Addop, Unary_Addop
      Tok_Plus,            -- +            Binary_Addop, Unary_Addop

      Tok_Asterisk,        -- *            Mulop
      Tok_Slash,           -- /            Mulop

      Tok_Dot,             -- .            Namext
      Tok_Apostrophe,      -- '            Namext

      Tok_Right_Paren,     -- )            Sterm
      Tok_Comma,           -- ,            Sterm

      Tok_Less,            -- <            Relop, Sterm
      Tok_Equal,           -- =            Relop, Sterm
      Tok_Greater,         -- >            Relop, Sterm
      Tok_Not_Equal,       -- /=           Relop, Sterm
      Tok_Greater_Equal,   -- >=           Relop, Sterm
      Tok_Less_Equal,      -- <=           Relop, Sterm

      Tok_Box,             -- <>           Relop, Eterm, Sterm
      Tok_Colon_Equal,     -- :=           Eterm, Sterm

      Tok_Abstract,        -- ABSTRACT     Eterm, Sterm
      Tok_Access,          -- ACCESS       Eterm, Sterm
      Tok_Aliased,         -- ALIASED      Eterm, Sterm
      Tok_All,             -- ALL          Eterm, Sterm
      Tok_Array,           -- ARRAY        Eterm, Sterm
      Tok_At,              -- AT           Eterm, Sterm

      Tok_Mod,             -- MOD          Mulop
      Tok_Rem,             -- REM          Mulop

      Tok_And,             -- AND          Logop, Sterm

      Tok_Delta,           -- DELTA        Atkwd, Sterm, Consk
      Tok_Digits,          -- DIGITS       Atkwd, Sterm, Consk
      Tok_Range,           -- RANGE        Atkwd, Sterm, Consk

      Tok_Abs,             -- ABS
      Tok_New,             -- NEW
      Tok_Null,            -- NULL
      Tok_Others,          -- OTHERS

      Tok_In,              -- IN           Relop, Sterm
      Tok_Not,             -- NOT          Relop, Sterm

      Tok_Or,              -- OR           Logop, Sterm
      Tok_Xor,             -- XOR          Logop, Sterm

      Tok_Body,            -- BODY         Eterm, Sterm
      Tok_Constant,        -- CONSTANT     Eterm, Sterm
      Tok_Limited,         -- LIMITED      Eterm, Sterm
      Tok_Of,              -- OF           Eterm, Sterm
      Tok_Out,             -- OUT          Eterm, Sterm
      Tok_Renames,         -- RENAMES      Eterm, Sterm
      Tok_Reverse,         -- REVERSE      Eterm, Sterm
      Tok_Tagged,          -- TAGGED       Eterm, Sterm

      Tok_Case,            -- CASE         Eterm, Sterm, After_SM
      Tok_Delay,           -- DELAY        Eterm, Sterm, After_SM

      Tok_Accept,          -- ACCEPT       Eterm, Sterm, After_SM
      Tok_Elsif,           -- ELSIF        Eterm, Sterm, After_SM
      Tok_End,             -- END          Eterm, Sterm, After_SM
      Tok_Exit,            -- EXIT         Eterm, Sterm, After_SM
      Tok_Goto,            -- GOTO         Eterm, Sterm, After_SM
      Tok_If,              -- IF           Eterm, Sterm, After_SM
      Tok_Pragma,          -- PRAGMA       Eterm, Sterm, After_SM
      Tok_Raise,           -- RAISE        Eterm, Sterm, After_SM
      Tok_Requeue,         -- REQUEUE      Eterm, Sterm, After_SM
      Tok_Return,          -- RETURN       Eterm, Sterm, After_SM
      Tok_Terminate,       -- TERMINATE    Eterm, Sterm, After_SM
      Tok_Until,           -- UNTIL        Eterm, Sterm, After_SM
      Tok_When,            -- WHEN         Eterm, Sterm, After_SM

      Tok_For,             -- FOR          Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_While,           -- WHILE        Eterm, Sterm, After_SM, Labeled_Stmt

      Tok_Separate,        -- SEPARATE     Eterm, Sterm, Cunit, After_SM

      Tok_Entry,           -- ENTRY        Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Protected,       -- PROTECTED    Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Task,            -- TASK         Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Type,            -- TYPE         Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Subtype,         -- SUBTYPE      Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Overriding,      -- OVERRIDING   Eterm, Sterm, Declk, Declk, After_SM
      Tok_Synchronized,    -- SYNCHRONIZED Eterm, Sterm, Declk, Deckn, After_SM
      Tok_Use,             -- USE          Eterm, Sterm, Declk, Deckn, After_SM

      Tok_Generic,         -- GENERIC      Eterm, Sterm, Cunit, Declk, After_SM

      Tok_Function,        -- FUNCTION     Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Package,         -- PACKAGE      Eterm, Sterm, Cunit, Declk, After_SM
      Tok_Procedure,       -- PROCEDURE    Eterm, Sterm, Cunit, Declk, After_SM

      Tok_Do,              -- DO           Eterm, Sterm
      Tok_Is,              -- IS           Eterm, Sterm
      Tok_Interface,       -- INTERFACE    Eterm, Sterm
      Tok_Record,          -- RECORD       Eterm, Sterm
      Tok_Then,            -- THEN         Eterm, Sterm

      Tok_Abort,           -- ABORT        Eterm, Sterm, After_SM
      Tok_Else,            -- ELSE         Eterm, Sterm, After_SM
      Tok_Exception,       -- EXCEPTION    Eterm, Sterm, After_SM

      Tok_Select,          -- SELECT       Eterm, Sterm, After_SM

      Tok_Begin,           -- BEGIN        Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_Declare,         -- DECLARE      Eterm, Sterm, After_SM, Labeled_Stmt
      Tok_Loop,            -- LOOP         Eterm, Sterm, After_SM, Labeled_Stmt

      Tok_Private,         -- PRIVATE      Eterm, Sterm, Cunit, After_SM
      Tok_With,            -- WITH         Eterm, Sterm, Cunit, After_SM

      Tok_Semicolon,       -- ;            Eterm, Sterm, Cterm

      Tok_Left_Paren,      -- (            Namext, Consk

      Tok_Ampersand,       -- &            Binary_Addop

      Tok_Vertical_Bar,    -- |            Cterm, Sterm, Chtok

      Tok_Less_Less,       -- <<           Eterm, Sterm, After_SM
      Tok_Greater_Greater, -- >>           Eterm, Sterm

      Tok_Pound,           -- # sign, used by the preprocessor

      Tok_Colon,           -- :            Eterm, Sterm

      Tok_Arrow,           -- =>           Sterm, Cterm, Chtok

      Tok_Dot_Dot,         -- ..           Sterm, Chtok

      No_Token);
      --  No_Token is used for initializing Token values to indicate that
      --  no value has been set yet.

   type Token_Set is array (Token_Type) of Boolean;
   pragma Pack (Token_Set);

   subtype Reserved_Token_Type is Token_Type range Tok_Abstract .. Tok_With;

   subtype Token_Class_Literal is
     Token_Type range Tok_Integer_Literal .. Tok_Operator_Symbol;
   --  Literal

   subtype Token_Class_Declk is
     Token_Type range Tok_Entry .. Tok_Procedure;
   --  Keywords which start a declaration

   subtype Token_Class_No_Cont is Token_Type range Tok_Generic .. Tok_Colon;
   --  Do not allow a following continuation line

   Is_Operator : constant Token_Set :=
     (Tok_Double_Asterisk |
      Tok_Minus           |
      Tok_Plus            |
      Tok_Asterisk        |
      Tok_Slash           |
      Tok_Less            |
      Tok_Equal           |
      Tok_Greater         |
      Tok_Not_Equal       |
      Tok_Greater_Equal   |
      Tok_Less_Equal      |
      Tok_Ampersand       => True,
      others              => False);

   Is_Extended_Operator : constant Token_Set :=
     (Tok_Double_Asterisk .. Tok_Slash     |
      Tok_Comma .. Tok_Colon_Equal         |
      Tok_Semicolon                        |
      Tok_Ampersand .. Tok_Greater_Greater |
      Tok_Colon .. Tok_Dot_Dot             => True,
      others                               => False);

   Max_Identifier : constant := 256;
   --  Maximum length of an identifier.

   No_Attribute : constant Construct_Attribute_Map := (others => False);

   type Extended_Token is record
      Token         : Token_Type := No_Token;
      --  Enclosing token

      Declaration   : Boolean := False;
      --  Are we inside a declarative part ?

      Extra_Indent : Boolean := False;
      --  Used for various kinds of constructs, when detecting different
      --  ways of indenting code (only used internally).
      --  For a Tok_Record, set to True if the keyword "record" was found on
      --  its own line.
      --  For a Tok_Case, set to True if 'when' constructs should be indented
      --  an extra level, as for the RM style.

      Type_Declaration : Boolean := False;
      --  Is it a type declaration ?

      Package_Declaration : Boolean := False;
      --  Is it a package declaration ?

      --  ??? It would be nice to merge the fields Declaration,
      --  Type_Declaration and Package_Declaration at some point.

      Identifier          : String (1 .. Max_Identifier);
      --  Name of the enclosing token
      --  The actual name is Identifier (1 .. Ident_Len)

      Ident_Len           : Natural := 0;
      --  Actual length of Indentifier

      Profile_Start       : Natural := 0;
      --  Position in the buffer where the profile of the current subprogram
      --  starts.

      Profile_End         : Natural := 0;
      --  Position in the buffer where the profile of the current subprogram
      --  ends.

      Align_Colon         : Natural := 0;
      --  The column on which to align declarations.

      Colon_Col           : Natural := 0;
      --  The column where the last ':' was found.
      --  Only relevant for Tok_Colon tokens

      Sloc                : Source_Location;
      --  Source location for this entity

      Sloc_Name           : Source_Location;
      --  Source location for the name of this entity, if relevant

      Visibility          : Construct_Visibility := Visibility_Public;
      --  Is the token public or private ?

      Visibility_Section  : Construct_Visibility := Visibility_Public;
      --  Are we on the public or on the private section of the token ?

      Is_Parameter   : Boolean := False;
      --  Return True if this token is in the profile of its parent

      Type_Definition_Section : Boolean := False;
      --  Are we currently processing the type definition of the token ?

      Is_In_Type_Definition : Boolean := False;
      --  Is this token found in the type definition section of its parent ?

      Attributes            : Construct_Attribute_Map := No_Attribute;
      --  Defines the attributes that have been found for the given token
   end record;
   --  Extended information for a token

   package Token_Stack is new Generic_Stack (Extended_Token);
   use Token_Stack;

   type Construct_Type is
     (Conditional,
      Type_Declaration,
      Function_Call,
      Aggregate);
   --  Type used to differentiate different constructs to refine indentation

   package Construct_Stack is new Generic_Stack (Construct_Type);
   use Construct_Stack;

   ----------------------
   -- Local procedures --
   ----------------------

   function Get_Token
     (Str : String; Prev_Token : Token_Type) return Token_Type;
   --  Return a token_Type given a string.
   --  For efficiency, S is assumed to start at index 1.
   --  Prev_Token is the previous token scanned, if any. This is needed
   --  to e.g. differenciate pragma Interface vs interface keyword.

   function Is_Library_Level (Stack : Token_Stack.Simple_Stack) return Boolean;
   --  Return True if the current scope in Stack is a library level package.

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token
     (Str : String; Prev_Token : Token_Type) return Token_Type
   is
      S : String (Str'Range);
   begin
      --  First convert Str to lower case for the token parser below

      for K in Str'Range loop
         S (K) := To_Lower (Str (K));
      end loop;

      if S'Length = 0 then
         return No_Token;
      elsif S'Length = 1 then
         if Is_Control (S (S'First)) then
            return No_Token;
         else
            return Tok_Identifier;
         end if;
      end if;

      --  Use a case statement instead of a loop for efficiency

      case S (1) is
         when 'a' =>
            case S (2) is
               when 'b' =>
                  if S (3 .. S'Last) = "ort" then
                     return Tok_Abort;
                  elsif S (3 .. S'Last) = "s" then
                     return Tok_Abs;
                  elsif S (3 .. S'Last) = "stract" then
                     return Tok_Abstract;
                  end if;

               when 'c' =>
                  if S (3 .. S'Last) = "cept" then
                     return Tok_Accept;
                  elsif S (3 .. S'Last) = "cess" then
                     return Tok_Access;
                  end if;

               when 'l' =>
                  if S (3 .. S'Last) = "l" then
                     return Tok_All;
                  elsif S (3 .. S'Last) = "iased" then
                     return Tok_Aliased;
                  end if;

               when 'n' =>
                  if S (3 .. S'Last) = "d" then
                     return Tok_And;
                  end if;

               when 'r' =>
                  if S (3 .. S'Last) = "ray" then
                     return Tok_Array;
                  end if;

               when 't' =>
                  if S'Length = 2 then
                     return Tok_At;
                  end if;

               when others =>
                  return Tok_Identifier;
            end case;

         when 'b' =>
            if S (2 .. S'Last) = "egin" then
               return Tok_Begin;
            elsif S (2 .. S'Last) = "ody" then
               return Tok_Body;
            end if;

         when 'c' =>
            if S (2 .. S'Last) = "ase" then
               return Tok_Case;
            elsif S (2 .. S'Last) = "onstant" then
               return Tok_Constant;
            end if;

         when 'd' =>
            if S (2) = 'e' then
               if S (3 .. S'Last) = "clare" then
                  return Tok_Declare;
               elsif S (3 .. S'Last) = "lay" then
                  return Tok_Delay;
               elsif S (3 .. S'Last) = "lta" then
                  return Tok_Delta;
               end if;

            elsif S (2 .. S'Last) = "igits" then
               return Tok_Digits;
            elsif S (2 .. S'Last) = "o" then
               return Tok_Do;
            end if;

         when 'e' =>
            if S (2 .. S'Last) = "lse" then
               return Tok_Else;
            elsif S (2 .. S'Last) = "lsif" then
               return Tok_Elsif;
            elsif S (2 .. S'Last) = "nd" then
               return Tok_End;
            elsif S (2 .. S'Last) = "ntry" then
               return Tok_Entry;
            elsif S (2 .. S'Last) = "xception" then
               return Tok_Exception;
            elsif S (2 .. S'Last) = "xit" then
               return Tok_Exit;
            end if;

         when 'f' =>
            if S (2 .. S'Last) = "or" then
               return Tok_For;
            elsif S (2 .. S'Last) = "unction" then
               return Tok_Function;
            end if;

         when 'g' =>
            if S (2 .. S'Last) = "eneric" then
               return Tok_Generic;
            elsif S (2 .. S'Last) = "oto" then
               return Tok_Goto;
            end if;

         when 'i' =>
            if S (2 .. S'Last) = "f" then
               return Tok_If;
            elsif S (2 .. S'Last) = "n" then
               return Tok_In;
            elsif S (2 .. S'Last) = "nterface" then
               if Prev_Token = Tok_Pragma then
                  return Tok_Identifier;
               else
                  return Tok_Interface;
               end if;

            elsif S (2 .. S'Last) = "s" then
               return Tok_Is;
            end if;

         when 'l' =>
            if S (2 .. S'Last) = "imited" then
               return Tok_Limited;
            elsif S (2 .. S'Last) = "oop" then
               return Tok_Loop;
            end if;

         when 'm' =>
            if S (2 .. S'Last) = "od" then
               return Tok_Mod;
            end if;

         when 'n' =>
            if S (2 .. S'Last) = "ew" then
               return Tok_New;
            elsif S (2 .. S'Last) = "ot" then
               return Tok_Not;
            elsif S (2 .. S'Last) = "ull" then
               return Tok_Null;
            end if;

         when 'o' =>
            if S (2 .. S'Last) = "thers" then
               return Tok_Others;
            elsif S (2 .. S'Last) = "ut" then
               return Tok_Out;
            elsif S (2 .. S'Last) = "f" then
               return Tok_Of;
            elsif S (2 .. S'Last) = "r" then
               return Tok_Or;
            elsif S (2 .. S'Last) = "verriding" then
               return Tok_Overriding;
            end if;

         when 'p' =>
            if S (2) = 'r' then
               if S (3 .. S'Last) = "agma" then
                  return Tok_Pragma;
               elsif S (3 .. S'Last) = "ivate" then
                  return Tok_Private;
               elsif S (3 .. S'Last) = "ocedure" then
                  return Tok_Procedure;
               elsif S (3 .. S'Last) = "otected" then
                  return Tok_Protected;
               end if;

            elsif S (2 .. S'Last) = "ackage" then
               return Tok_Package;
            end if;

         when 'r' =>
            if S (2) = 'a' then
               if S (3 .. S'Last) = "ise" then
                  return Tok_Raise;
               elsif S (3 .. S'Last) = "nge" then
                  return Tok_Range;
               end if;

            elsif S (2) = 'e' then
               if S (3 .. S'Last) = "cord" then
                  return Tok_Record;
               elsif S (3 .. S'Last) = "m" then
                  return Tok_Rem;
               elsif S (3 .. S'Last) = "names" then
                  return Tok_Renames;
               elsif S (3 .. S'Last) = "queue" then
                  return Tok_Requeue;
               elsif S (3 .. S'Last) = "turn" then
                  return Tok_Return;
               elsif S (3 .. S'Last) = "verse" then
                  return Tok_Reverse;
               end if;
            end if;

         when 's' =>
            if S (2 .. S'Last) = "elect" then
               return Tok_Select;
            elsif S (2 .. S'Last) = "eparate" then
               return Tok_Separate;
            elsif S (2 .. S'Last) = "ubtype" then
               return Tok_Subtype;
            elsif S (2 .. S'Last) = "ynchronized" then
               return Tok_Synchronized;
            end if;

         when 't' =>
            if S (2 .. S'Last) = "agged" then
               return Tok_Tagged;
            elsif S (2 .. S'Last) = "ask" then
               return Tok_Task;
            elsif S (2 .. S'Last) = "erminate" then
               return Tok_Terminate;
            elsif S (2 .. S'Last) = "hen" then
               return Tok_Then;
            elsif S (2 .. S'Last) = "ype" then
               return Tok_Type;
            end if;

         when 'u' =>
            if S (2 .. S'Last) = "ntil" then
               return Tok_Until;
            elsif S (2 .. S'Last) = "se" then
               return Tok_Use;
            end if;

         when 'w' =>
            if S (2 .. S'Last) = "hen" then
               return Tok_When;
            elsif S (2 .. S'Last) = "hile" then
               return Tok_While;
            elsif S (2 .. S'Last) = "ith" then
               return Tok_With;
            end if;

         when 'x' =>
            if S (2 .. S'Last) = "or" then
               return Tok_Xor;
            end if;

         when others =>
            return Tok_Identifier;
      end case;

      return Tok_Identifier;
   end Get_Token;

   ----------------------
   -- Is_Library_Level --
   ----------------------

   function Is_Library_Level
     (Stack : Token_Stack.Simple_Stack) return Boolean
   is
      Tmp : Token_Stack.Simple_Stack;
   begin
      Tmp := Stack;

      while Tmp /= null and then Tmp.Val.Token /= No_Token loop
         if Tmp.Val.Token /= Tok_Package then
            return False;
         end if;

         Tmp := Tmp.Next;
      end loop;

      return True;
   end Is_Library_Level;

   ------------------------
   -- Analyze_Ada_Source --
   ------------------------

   procedure Analyze_Ada_Source
     (Buffer           : String;
      Indent_Params    : Indent_Parameters;
      Format           : Boolean               := True;
      From, To         : Natural               := 0;
      Replace          : Replace_Text_Callback := null;
      Constructs       : Construct_List_Access := null;
      Callback         : Entity_Callback       := null;
      Indent_Offset    : Natural               := 0;
      Case_Exceptions  : Casing_Exceptions     := No_Casing_Exception)
   is
      ---------------
      -- Constants --
      ---------------

      None   : constant := -1;

      Default_Extended : Extended_Token;
      pragma Warnings (Off, Default_Extended);
      --  Use default values to initialize this pseudo constant.

      Indent_Level        : Natural renames Indent_Params.Indent_Level;
      Indent_Continue     : Natural renames Indent_Params.Indent_Continue;
      Indent_Decl         : Natural renames Indent_Params.Indent_Decl;
      Indent_With         : constant := 5;
      Indent_Use          : constant := 4;
      Indent_When         : constant := 5;
      Indent_Comments     : Boolean renames Indent_Params.Indent_Comments;
      Stick_Comments      : Boolean renames Indent_Params.Stick_Comments;
      Stop_On_Blank_Line  : constant Boolean := True;
      Indent_Record       : Natural renames Indent_Params.Indent_Record;
      Indent_Case_Extra   : Indent_Style renames
        Indent_Params.Indent_Case_Extra;
      Indent_Conditional  : Natural renames Indent_Params.Indent_Conditional;
      Reserved_Casing     : Casing_Type renames Indent_Params.Reserved_Casing;
      Ident_Casing        : Casing_Type renames Indent_Params.Ident_Casing;
      Use_Tabs            : Boolean renames Indent_Params.Use_Tabs;
      Tab_Width           : Natural renames Indent_Params.Tab_Width;
      Format_Operators    : constant Boolean :=
                              Format and then Indent_Params.Format_Operators;
      Align_On_Colons     : constant Boolean :=
                              Format and then Indent_Params.Align_On_Colons;
      Align_On_Arrows     : constant Boolean :=
                              Format and then Indent_Params.Align_On_Arrows;
      Align_Decl_On_Colon : constant Boolean :=
                              Format
                                and then Indent_Params.Align_Decl_On_Colon;

      Buffer_Last         : constant Natural := Buffer'Last;

      ---------------
      -- Variables --
      ---------------

      Line_Count          : Integer           := 1;
      Str                 : String (1 .. 1024);
      Str_Len             : Natural           := 0;
      Current             : Natural;
      Prec                : Natural           := Buffer'First;
      Start_Of_Line       : Natural;
      Prev_Line           : Natural;
      Num_Spaces          : Integer           := 0;
      Continuation_Val    : Integer           := 0;
      Indent_Done         : Boolean           := False;
      Num_Parens          : Integer           := 0;
      Index_Ident         : Natural;
      In_Generic          : Boolean           := False;
      Subprogram_Decl     : Boolean           := False;
      Syntax_Error        : Boolean           := False;
      --  Not used for now, but may be useful in the future.
      pragma Unreferenced (Syntax_Error);

      Comments_Skipped    : Boolean           := False;
      Token               : Token_Type;
      Prev_Token          : Token_Type        := No_Token;
      Prev_Prev_Token     : Token_Type        := No_Token;
      Tokens              : Token_Stack.Simple_Stack;
      Paren_Stack         : Construct_Stack.Simple_Stack;
      Indents             : Indent_Stack.Stack.Simple_Stack;
      Top_Token           : Token_Stack.Generic_Type_Access;
      Casing              : Casing_Type;
      Terminated          : Boolean := False;
      End_Reached         : Boolean := False;
      Last_Replace_Line   : Natural := 0;
      Padding             : Integer := 0;
      Paren_In_Middle     : Boolean := False;

      Is_Parameter        : Boolean := False;
      --  This variable is true if the identifiers picked are parameters, false
      --  otherwise.

      Right_Assignment    : Boolean := False;
      --  When this is true, we are in a left assignment section.

      function Handle_Reserved_Word (Reserved : Token_Type) return Boolean;
      --  Handle reserved words.
      --  Return whether parsing should be terminated.

      procedure Next_Word
        (P           : in out Natural;
         Terminated  : out Boolean;
         End_Reached : out Boolean);
      --  Starting at Buffer (P), find the location of the next word
      --  and set P accordingly.
      --  Formatting of operators is performed by this procedure.
      --  The following variables are accessed read-only:
      --    Buffer, Tokens, Num_Spaces, Indent_Continue
      --  The following variables are read and modified:
      --    New_Buffer, Num_Parens, Line_Count, Indents, Indent_Done,
      --    Prev_Token.
      --  If the end of the buffer has been reached, set End_Reached to True.
      --  If parsing should be terminated, set Terminated to True.

      function End_Of_Word (P : Natural) return Natural;
      --  Return the end of the word pointed by P.

      function End_Of_Identifier (P : Natural) return Natural;
      --  Starting from P, scan for the end of the identifier.
      --  P should be at the first non word character, which means that
      --  if the identifier does not contain any dot, P - 1 will be returned.

      procedure Look_For_End_Of_Data_Declaration
        (Sloc : in out Source_Location);
      --  Search the first ; character, or the first closing parenthesis,
      --  skipping nested ones. Intended to go at the end of a variable or
      --  parameter declaration.

      procedure Look_For (Sloc : in out Source_Location; Char : Character);
      --  Search Char in Buffer starting from Sloc.
      --  Sloc is updated to the first occurrence of Char in Buffer, or
      --  Buffer_Last if not found.

      function Look_For (Index : Natural; S : String) return Boolean;
      --  Return True if Buffer (Index) contains the word S

      procedure New_Line (Count : in out Natural);
      pragma Inline (New_Line);
      --  Increment Count and poll if needed (e.g for graphic events)

      procedure Do_Indent
        (Prec         : Natural;
         Num_Spaces   : Integer;
         Continuation : Boolean := False);
      --  Perform indentation by inserting spaces in the buffer.
      --  If Continuation is True, Indent_Continue extra spaces are added.

      procedure Indent_Function_Return (Prec : Natural);
      --  Perform special indentation for function return/rename statements

      procedure Compute_Indentation
        (Token      : Token_Type;
         Prev_Token : Token_Type;
         Prec       : Natural;
         Num_Spaces : Integer);
      --  Compute proper indentation, taking into account various cases
      --  of simple/continuation/declaration/... lines.

      function Next_Char (P : Natural) return Natural;
      --  Return the next char in buffer. P is the current character
      pragma Inline (Next_Char);

      function Prev_Char (P : Natural) return Natural;
      --  Return the previous char in buffer. P is the current character
      pragma Inline (Prev_Char);

      function Compute_Alignment
        (P                  : Natural;
         Stop_On_Blank_Line : Boolean    := False;
         Skip_First_Line    : Boolean    := True;
         Align_On           : Token_Type := Tok_Colon) return Natural;
      --  Compute the column number for an alignment on Align_On, starting at P
      --  Align_On can take one of the following values:
      --  - Tok_Colon
      --  - Tok_Arrow
      --  - Tok_Colon_Equal (not supported yet)

      function Find_Multi_Line_Paren (P : Natural) return Natural;
      --  Return the location of the first left parenthesis which is not
      --  closed on the same line.
      --  If a right parenthesis is found before, return 0.

      function Find_Arrow (P : Natural) return Natural;
      --  Return the location of the first '=>' token on the current line,
      --  0 if none.

      procedure Replace_Text
        (First : Natural;
         Last  : Natural;
         Str   : String);
      --  Wrapper for Replace.all, taking (From, To) into account

      --------------------
      -- Stack Routines --
      --------------------

      procedure Pop
        (Stack : in out Token_Stack.Simple_Stack; Value : out Extended_Token);
      --  Pop Value on top of Stack

      procedure Pop (Stack : in out Token_Stack.Simple_Stack);
      --  Pop Value on top of Stack. Ignore returned value

      ---------------
      -- Next_Char --
      ---------------

      function Next_Char (P : Natural) return Natural is
      begin
         return UTF8_Next_Char (Buffer, P);
      end Next_Char;

      ---------------
      -- Prev_Char --
      ---------------

      function Prev_Char (P : Natural) return Natural is
      begin
         return UTF8_Find_Prev_Char (Buffer, P);
      end Prev_Char;

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent
        (Prec         : Natural;
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

         Paren_In_Middle := False;
         Start := Line_Start (Buffer, Prec);
         Index := Start;

         loop
            --  Manual unrolling for efficiency

            exit when Buffer (Index) /= ' '
              and then Buffer (Index) /= ASCII.HT;

            Index := Index + 1;

            exit when Buffer (Index) /= ' '
              and then Buffer (Index) /= ASCII.HT;

            Index := Index + 1;

            exit when Buffer (Index) /= ' '
              and then Buffer (Index) /= ASCII.HT;

            Index := Index + 1;
         end loop;

         if Top (Indents).Level = None then
            Indentation := Num_Spaces + Indent_Offset;
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
           (Start, Index,
            Blank_Slice (Indentation, Use_Tabs, Tab_Width));
         Indent_Done := True;
      end Do_Indent;

      ----------------------------
      -- Indent_Function_Return --
      ----------------------------

      procedure Indent_Function_Return (Prec : Natural) is
         Top_Token : constant Token_Stack.Generic_Type_Access := Top (Tokens);
      begin
         --  function A
         --    return B;   --  from Indent_Continue
         --  function A (....)
         --              return B;
         --  function A
         --    (...)
         --     return B;

         if Top_Token.Profile_Start = 0 then
            Do_Indent (Prec, Num_Spaces + Indent_Continue);
         else
            Do_Indent
              (Prec,
               Top_Token.Profile_Start -
                 Line_Start (Buffer, Top_Token.Profile_Start) + 1);
         end if;
      end Indent_Function_Return;

      ----------------
      -- Find_Arrow --
      ----------------

      function Find_Arrow (P : Natural) return Natural is
         J : Natural;
      begin
         J := P;

         loop
            exit when J >= Buffer'Last;

            case Buffer (J) is
               when '>' =>
                  if Buffer (J - 1) = '=' then
                     return J - 1;
                  end if;

               when '"' =>
                  if Buffer (J - 1) /= ''' then
                     J := J + 1;
                     Skip_To_Char (Buffer, J, '"');
                  end if;

               when '-' =>
                  if Buffer (J + 1) = '-' then
                     --  A comment, return
                     return 0;
                  end if;

               when '(' =>
                  if Buffer (J - 1) /= '''
                    or else Buffer (J + 1) /= '''
                  then
                     return 0;
                  end if;

               when ';' =>
                  if Buffer (J - 1) /= ''' then
                     return 0;
                  end if;

               when ASCII.LF =>
                  return 0;

               when others =>
                  null;
            end case;

            J := J + 1;
         end loop;

         return 0;
      end Find_Arrow;

      ---------------------------
      -- Find_Multi_Line_Paren --
      ---------------------------

      function Find_Multi_Line_Paren (P : Natural) return Natural is
         Found            : Natural;
         J                : Natural := P;
         Local_Num_Parens : Natural := 0;

      begin
         Main_Loop :
         loop
            exit Main_Loop when J >= Buffer'Last;

            case Buffer (J) is
               when '"' =>
                  if Buffer (J - 1) /= ''' then
                     J := J + 1;
                     Skip_To_Char (Buffer, J, '"');
                  end if;

               when '-' =>
                  if Buffer (J + 1) = '-' then
                     if Local_Num_Parens > 0 then
                        return Found;
                     end if;

                     --  Skip comment

                     J := Next_Line (Buffer, J) - 1;
                  end if;

               when '(' =>
                  if Buffer (J - 1) /= '''
                    or else Buffer (J + 1) /= '''
                  then
                     Local_Num_Parens := Local_Num_Parens + 1;
                     Found := J;
                  end if;

               when ')' =>
                  if Buffer (J - 1) /= ''' then
                     if Local_Num_Parens = 0 then
                        return 0;
                     end if;

                     Local_Num_Parens := Local_Num_Parens - 1;
                  end if;

               when ';' =>
                  if Buffer (J - 1) /= ''' then
                     return 0;
                  end if;

               when ASCII.LF =>
                  if Local_Num_Parens > 0 then
                     return Found;
                  end if;

               when others =>
                  null;
            end case;

            J := J + 1;
         end loop Main_Loop;

         if Local_Num_Parens > 0 then
            if J >= Buffer'Last and then Local_Num_Parens = 1 then
               return 0;
            end if;

            return Found;
         else
            return 0;
         end if;
      end Find_Multi_Line_Paren;

      -----------------------
      -- Compute_Alignment --
      -----------------------

      function Compute_Alignment
        (P                  : Natural;
         Stop_On_Blank_Line : Boolean    := False;
         Skip_First_Line    : Boolean    := True;
         Align_On           : Token_Type := Tok_Colon) return Natural
      is
         Alignment        : Natural := 0;
         New_Align        : Natural;
         Found_Align      : Boolean := False;
         J                : Natural;
         Non_Blank        : Natural := 0;
         Length_Ident     : Natural := 0;
         Local_Num_Parens : Natural := 0;

      begin
         if Align_On /= Tok_Colon
           and then Align_On /= Tok_Arrow
         --  and then Align_On /= Tok_Colon_Equal  ??? not supported yet
         then
            return 0;
         end if;

         --  ??? Need to call Compute_Alignment again after blank line and
         --  Stop_On_Blank_Line is True

         if Skip_First_Line then
            J := Next_Line (Buffer, P);
         else
            J := P;
         end if;

         Main_Loop :
         loop
            exit Main_Loop when J >= Buffer'Last;

            if Non_Blank = 0 then
               if not Is_Blank (Buffer (J)) then
                  Non_Blank := J;
                  Length_Ident := 1;
               end if;

            else
               Length_Ident := Length_Ident + 1;
            end if;

            exit Main_Loop when Look_For (J, "begin")
              or else Look_For (J, "case")
              or else Look_For (J, "end")
              or else Look_For (J, "package")
              or else Look_For (J, "protected")
              or else Look_For (J, "task")
              or else Look_For (J, "type");

            case Buffer (J) is
               when '"' =>
                  if Buffer (J - 1) /= ''' then
                     J := J + 1;
                     Skip_To_Char (Buffer, J, '"');
                  end if;

               when '-' =>
                  if Buffer (J + 1) = '-' then
                     --  Skip comment

                     J := Next_Line (Buffer, J) - 1;
                     Found_Align := False;
                     Non_Blank := 0;
                  end if;

               when '(' =>
                  if Buffer (J - 1) /= '''
                    or else Buffer (J + 1) /= '''
                  then
                     Local_Num_Parens := Local_Num_Parens + 1;
                  end if;

               when ')' =>
                  if Buffer (J - 1) /= ''' then
                     exit Main_Loop when Local_Num_Parens = 0;

                     Local_Num_Parens := Local_Num_Parens - 1;
                  end if;

               when ':' =>
                  if Align_On = Tok_Colon
                    and then Local_Num_Parens = 0
                    and then Buffer (J - 1) /= '''
                    and then not Found_Align
                  then
                     Found_Align := True;
                     New_Align   := Length_Ident;

                     if Format_Operators
                       and then not Is_Blank (Buffer (J - 1))
                     then
                        New_Align := New_Align + 1;
                     end if;

                     Alignment := Natural'Max (Alignment, New_Align);
                  end if;

               when '=' =>
                  if Align_On = Tok_Arrow
                    and then Buffer (J + 1) = '>'
                    and then Local_Num_Parens = 0
                    and then not Found_Align
                  then
                     Found_Align := True;
                     New_Align   := Length_Ident + 1;

                     if Format_Operators
                       and then not Is_Blank (Buffer (J - 1))
                     then
                        --  ??? Format_Operators may add more than a single
                        --  blank inside the expression.

                        New_Align := New_Align + 1;
                     end if;

                     Alignment := Natural'Max (Alignment, New_Align);
                  end if;

               when ASCII.LF =>
                  exit Main_Loop when Stop_On_Blank_Line
                    and then Non_Blank = 0;

                  Found_Align := False;
                  Non_Blank  := 0;

               when others =>
                  null;
            end case;

            J := Next_Char (J);
         end loop Main_Loop;

         return Alignment;
      end Compute_Alignment;

      -------------------------
      -- Compute_Indentation --
      -------------------------

      procedure Compute_Indentation
        (Token      : Token_Type;
         Prev_Token : Token_Type;
         Prec       : Natural;
         Num_Spaces : Integer)
      is
         Top_Tok : constant Token_Type := Top (Tokens).Token;

         function Is_Continuation_Line return Boolean;
         --  Return True is we are indenting a continuation line.

         --------------------------
         -- Is_Continuation_Line --
         --------------------------

         function Is_Continuation_Line return Boolean is
         begin
            return (Prev_Token = Tok_Is and then In_Generic)
              or else
                (Top_Tok /= No_Token
                 and then
                   ((Token not in Reserved_Token_Type
                     and then Prev_Token not in Token_Class_No_Cont
                     and then
                       (Prev_Token /= Tok_Arrow
                        or else (Top_Tok /= Tok_Case
                                 and then not Top (Tokens).Declaration
                                 and then Top_Tok /= Tok_When
                                 and then Top_Tok /= Tok_Select
                                 and then Top_Tok /= Tok_Exception)))
                    or else (Prev_Token = Tok_Is
                             and then (Token = Tok_New
                                       or else Token = Tok_Access
                                       or else Token = Tok_Separate
                                       or else
                                         (Top_Tok = Tok_Subtype
                                          and then Token /= Tok_Subtype)))
                    or else Token = Tok_Array
                    or else Prev_Token = Tok_Colon_Equal
                    or else Prev_Token = Tok_Access
                    or else Prev_Token = Tok_Of
                    or else (Prev_Token = Tok_Exit and then Token = Tok_When)
                    or else (Prev_Token = Tok_With
                             and then Token = Tok_Private)
                    or else (Prev_Token = Tok_Null and then Token = Tok_Record)
                    or else (Prev_Prev_Token = Tok_Raise
                             and then Token = Tok_With)
                    or else
                      (Top_Tok = Tok_If
                       and then Token /= Tok_If
                       and then
                         (Prev_Token = Tok_Then or else Prev_Token = Tok_Else))
                    or else
                      (Top_Tok = Tok_Type
                       and then (Token = Tok_Null or else Token = Tok_Tagged))
                    or else (Token = Tok_When and then Top_Tok = Tok_Entry)));
         end Is_Continuation_Line;

      begin
         if Indent_Done or not Format then
            return;
         end if;

         if Prev_Token = Tok_Vertical_Bar and then Top_Tok = Tok_When then
            --  Indent multiple-line when statements specially:
            --  case Foo is
            --     when A |
            --          B =>  --  from Indent_When

            Do_Indent (Prec, Num_Spaces - Indent_Level + Indent_When);

         elsif Prev_Token = Tok_Comma then
            if Top_Tok = Tok_Declare
              or else Top_Tok = Tok_Identifier
              or else Top_Tok = Tok_Record
              or else Top_Tok = Tok_Case
              or else Top_Tok = Tok_When
            then
               --  Inside a declare block, indent broken lines specially
               --  declare
               --     A,
               --         B : Integer;  --  from Indent_Decl

               Do_Indent (Prec, Num_Spaces + Indent_Decl);

            elsif Top_Tok = Tok_With then
               --  Indent continuation lines in with clauses:
               --  with Package1,
               --     Package2;  --  from Indent_With

               Do_Indent (Prec, Num_Spaces + Indent_With);

            elsif Top_Tok = Tok_Use then
               --  Ditto for use clauses:
               --  use Package1,
               --    Package2;  --  from Indent_Use

               Do_Indent (Prec, Num_Spaces + Indent_Use);

            else
               --  Default case, simply use Num_Spaces

               Do_Indent (Prec, Num_Spaces);
            end if;

         elsif Prev_Token = Tok_Colon_Equal
           and then Top (Tokens).Colon_Col /= 0
           and then Continuation_Val = 0
         then
            Continuation_Val := Top (Tokens).Colon_Col + 4 - Indent_Continue;
            Do_Indent (Prec, Num_Spaces, Continuation => True);

         elsif Is_Continuation_Line then
            Do_Indent (Prec, Num_Spaces, Continuation => True);

            if Is_Operator (Token) then
               Continuation_Val := 0;
            end if;

         else
            Do_Indent (Prec, Num_Spaces);
         end if;
      end Compute_Indentation;

      -----------------
      -- End_Of_Word --
      -----------------

      function End_Of_Word (P : Natural) return Natural is
         Tmp  : Natural := P;
         Next : Natural;
      begin
         while Tmp <= Buffer_Last loop
            Next := Next_Char (Tmp);
            exit when Next > Buffer_Last
              or else not Is_Entity_Letter
                (UTF8_Get_Char (Buffer (Next .. Buffer_Last)));
            Tmp := Next;
         end loop;

         if Next > Buffer_Last then
            return Buffer_Last;
         else
            return Tmp;
         end if;
      end End_Of_Word;

      -----------------------
      -- End_Of_Identifier --
      -----------------------

      function End_Of_Identifier (P : Natural) return Natural is
         Tmp       : Natural := P;
         Prev      : Natural := P - 1;
         Start     : Natural;
         New_Lines : Natural;

      begin
         --  Do not try to go past '.' and line breaks when reformatting,
         --  this is casuing too much trouble for no gain.
         --  Getting full identifiers is only useful when parsing, not
         --  when reformatting.

         if Format then
            return P - 1;
         end if;

         loop
            New_Lines := 0;

            while Tmp < Buffer_Last and then Is_Blank (Buffer (Tmp)) loop
               if Buffer (Tmp) = ASCII.LF then
                  New_Lines := New_Lines + 1;
               end if;

               Tmp := Tmp + 1;
            end loop;

            if Tmp >= Buffer_Last
              or else Buffer (Tmp) /= '.'
              or else Buffer (Tmp + 1) = '.'
            then
               return Prev;
            end if;

            while Tmp < Buffer_Last loop
               Tmp := Tmp + 1;

               if Buffer (Tmp) = ASCII.LF then
                  Compute_Indentation (Token, Prev_Token, Tmp - 1, Num_Spaces);
                  New_Lines := New_Lines + 1;
               end if;

               exit when not Is_Blank (Buffer (Tmp));
            end loop;

            if Buffer (Tmp) = '"' then
               --  Case of an operator, e.g System."="

               while Tmp < Buffer_Last loop
                  Tmp := Tmp + 1;

                  exit when Buffer (Tmp) = '"' or else Buffer (Tmp) = ASCII.LF;
               end loop;

               if Buffer (Tmp) = ASCII.LF then
                  Tmp := Tmp - 1;
               end if;

               Line_Count := Line_Count + New_Lines;
               return Tmp;

            else
               Start := Tmp;
               Tmp   := End_Of_Word (Tmp);

               if Buffer (Start .. Tmp) = "all" then
                  return Prev;
               end if;
            end if;

            Line_Count := Line_Count + New_Lines;
            Prev := Tmp;
            Tmp  := Tmp + 1;
         end loop;

         return Prev;
      end End_Of_Identifier;

      --------------------------------------
      -- Look_For_End_Of_Data_Declaration --
      --------------------------------------

      procedure Look_For_End_Of_Data_Declaration
        (Sloc : in out Source_Location)
      is
         C           : Character;
         In_Comments : Boolean := False;
         Paren_Depth : Integer := 0;
      begin
         for J in Sloc.Index .. Buffer_Last loop
            C := Buffer (J);

            if not In_Comments
              and then Paren_Depth = 0
              and then (C = ';' or else C = ')')
            then
               Sloc.Index := J;
               return;

            elsif C = '-' and then Buffer (J - 1) = '-' then
               In_Comments := True;

            elsif C = ASCII.LF then
               In_Comments := False;
               Sloc.Line := Sloc.Line + 1;
               Sloc.Column := 1;

            elsif C = '(' then
               Paren_Depth := Paren_Depth + 1;
               Sloc.Column := Sloc.Column + 1;

            elsif C = ')' then
               Paren_Depth := Paren_Depth - 1;
               Sloc.Column := Sloc.Column + 1;

            elsif C /= ASCII.CR then
               Sloc.Column := Sloc.Column + 1;

            end if;
         end loop;
      end Look_For_End_Of_Data_Declaration;

      --------------
      -- Look_For --
      --------------

      procedure Look_For (Sloc : in out Source_Location; Char : Character) is
         C           : Character;
         In_Comments : Boolean := False;
      begin
         for J in Sloc.Index .. Buffer_Last loop
            C := Buffer (J);

            if not In_Comments and then C = Char then
               Sloc.Index := J;
               return;

            elsif C = '-' and then Buffer (J - 1) = '-' then
               In_Comments := True;

            elsif C = ASCII.LF then
               In_Comments := False;
               Sloc.Line := Sloc.Line + 1;
               Sloc.Column := 1;

            elsif C /= ASCII.CR then
               Sloc.Column := Sloc.Column + 1;
            end if;
         end loop;
      end Look_For;

      function Look_For (Index : Natural; S : String) return Boolean is
      begin
         return Is_Blank (Buffer (Index - 1))
           and then Index + S'Length < Buffer'Last
           and then To_Lower (Buffer (Index .. Index + S'Length - 1)) = S
           and then (Buffer (Index + S'Length) = ';'
                     or else Buffer (Index + S'Length) = '-'
                     or else Is_Blank (Buffer (Index + S'Length)));
      end Look_For;

      --------------
      -- New_Line --
      --------------

      procedure New_Line (Count : in out Natural) is
      begin
         Count := Count + 1;
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

         --  Tok_Record will be taken into account by Tok_Type if needed.
         --  Tok_Case inside a type definition should also not be recorded.
         --  Build next entry of Constructs

         if Value.Token = Tok_Colon and then Constructs /= null then
            --  If we are on a Tok_Colon, then we want to assign attibutes to
            --  all the data that are related to this declaration.

            declare
               Current : Construct_Access := Constructs.Current;
            begin
               while Current /= null
                 and then Current.Category in Data_Category
                 and then Current.Sloc_Start.Index <= Prec
                 and then Current.Sloc_End.Index >= Prec
               loop
                  Current.Attributes := Value.Attributes;
                  Current := Current.Prev;
               end loop;
            end;
         elsif Value.Token /= Tok_Record
           and then Value.Token /= Tok_When
           and then Constructs /= null
           and then
             (Value.Token /= Tok_Case or else Top (Stack).Token /= Tok_Record)
           and then (Value.Token /= Tok_Type or else not In_Generic)
         then
            Column             := Prec - Line_Start (Buffer, Prec) + 1;
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

            Constructs.Current.Visibility := Value.Visibility;
            Constructs.Current.Attributes := Value.Attributes;

            if Value.Attributes (Ada_Tagged_Attribute) then
               Constructs.Current.Category := Cat_Class;
            elsif Value.Attributes (Ada_Record_Attribute) then
               Constructs.Current.Category := Cat_Structure;
            else
               case Value.Token is
                  when Tok_Package =>
                     Constructs.Current.Category := Cat_Package;
                  when Tok_Procedure =>
                     Constructs.Current.Category := Cat_Procedure;
                  when Tok_Function =>
                     Constructs.Current.Category := Cat_Function;
                  when Tok_Task =>
                     Constructs.Current.Category := Cat_Task;
                  when Tok_Protected =>
                     Constructs.Current.Category := Cat_Protected;
                  when Tok_Entry =>
                     Constructs.Current.Category := Cat_Entry;

                  when Tok_Type =>
                     Constructs.Current.Category := Cat_Type;
                  when Tok_Subtype =>
                     Constructs.Current.Category := Cat_Subtype;
                  when Tok_For =>
                     Constructs.Current.Category := Cat_Representation_Clause;
                  when Tok_Identifier =>
                     if Is_Library_Level (Stack) then
                        Constructs.Current.Category := Cat_Variable;
                     elsif Value.Is_Parameter then
                        Constructs.Current.Category := Cat_Parameter;
                     elsif Value.Is_In_Type_Definition then
                        if Top (Stack).Type_Declaration
                          or else Top (Stack).Attributes (Ada_Record_Attribute)
                        then
                           Constructs.Current.Category := Cat_Field;
                        else
                           Constructs.Current.Category := Cat_Literal;
                        end if;
                     else
                        Constructs.Current.Category := Cat_Local_Variable;
                     end if;
                  when Tok_With =>
                     Constructs.Current.Category := Cat_With;
                  when Tok_Use =>
                     Constructs.Current.Category := Cat_Use;

                  when Tok_Loop =>
                     Constructs.Current.Category := Cat_Loop_Statement;
                  when Tok_Then =>
                     Constructs.Current.Category := Cat_If_Statement;
                  when Tok_Case =>
                     Constructs.Current.Category := Cat_Case_Statement;
                  when Tok_Select =>
                     Constructs.Current.Category := Cat_Select_Statement;
                  when Tok_Accept | Tok_Do =>
                     Constructs.Current.Category := Cat_Accept_Statement;
                  when Tok_Declare =>
                     Constructs.Current.Category := Cat_Declare_Block;
                  when Tok_Begin =>
                     Constructs.Current.Category := Cat_Simple_Block;
                  when Tok_Return =>
                     Constructs.Current.Category := Cat_Return_Block;

                  when Tok_Exception =>
                     Constructs.Current.Category := Cat_Exception_Handler;
                  when others =>
                     Constructs.Current.Category := Cat_Unknown;
               end case;
            end if;

            if Value.Ident_Len > 0 then
               Constructs.Current.Name :=
                 new String'(Value.Identifier (1 .. Value.Ident_Len));
               Constructs.Current.Sloc_Entity := Value.Sloc_Name;
            end if;

            if Value.Profile_Start /= 0 then
               Constructs.Current.Profile :=
                 new String'
                   (Buffer (Value.Profile_Start .. Value.Profile_End));
            end if;

            Constructs.Current.Sloc_Start := Value.Sloc;

            if Comments_Skipped then
               Constructs.Current.Sloc_End := (Prev_Line, Column, Prec);
            else
               Constructs.Current.Sloc_End := (Line_Count, Column, Prec);
            end if;

            case Constructs.Current.Category is
               when Cat_Parameter =>
                  --  Adjust the Sloc_End to the next semicolon for enclosing
                  --  entities and variable declarations, or next closing
                  --  parenthesis

                  Look_For_End_Of_Data_Declaration
                    (Constructs.Current.Sloc_End);

               when Cat_Variable | Cat_Local_Variable | Cat_Field |
                    Cat_Declare_Block | Cat_Simple_Block |
                    Cat_Type | Cat_Subtype | Namespace_Category |
                    Subprogram_Category
               =>
                  --  Adjust the Sloc_End to the next semicolon for enclosing
                  --  entities and variable declarations.

                  Look_For (Constructs.Current.Sloc_End, ';');

               when others =>
                  null;
            end case;

            Constructs.Current.Is_Declaration :=
              Subprogram_Decl
                or else Value.Type_Declaration
                or else Value.Package_Declaration;
         end if;
      end Pop;

      procedure Pop (Stack : in out Token_Stack.Simple_Stack) is
         Value : Extended_Token;
      begin
         Pop (Stack, Value);
      end Pop;

      --------------------------
      -- Handle_Reserved_Word --
      --------------------------

      function Handle_Reserved_Word (Reserved : Token_Type) return Boolean is
         Temp          : Extended_Token;
         Top_Token     : Token_Stack.Generic_Type_Access := Top (Tokens);
         Start_Of_Line : Natural;
         Index_Next    : Natural;

      begin
         Temp.Token       := Reserved;
         Start_Of_Line    := Line_Start (Buffer, Prec);
         Temp.Sloc.Line   := Line_Count;
         Temp.Sloc.Column := Prec - Start_Of_Line + 1;
         Temp.Sloc.Index  := Prec;
         Temp.Visibility  := Top_Token.Visibility_Section;

         if Callback /= null then
            if Callback
              (Keyword_Text,
               Temp.Sloc,
               (Line_Count, Current - Start_Of_Line + 1, Current),
               False)
            then
               return True;
            end if;
         end if;

         --  Computes Tok_Token.Type_Att

         if not Right_Assignment then
            case Reserved is
               when Tok_Abstract =>
                  Top_Token.Attributes (Ada_Abstract_Attribute) := True;
               when Tok_Access =>
                  Top_Token.Attributes (Ada_Access_Attribute) := True;
               when Tok_Aliased =>
                  Top_Token.Attributes (Ada_Aliased_Attribute) := True;
               when Tok_Array =>
                  Top_Token.Attributes (Ada_Array_Attribute) := True;
               when Tok_Colon_Equal =>
                  Top_Token.Attributes (Ada_Assign_Attribute) := True;
               when Tok_Constant =>
                  Top_Token.Attributes (Ada_Constant_Attribute) := True;
               when Tok_Delta =>
                  Top_Token.Attributes (Ada_Delta_Attribute) := True;
               when Tok_Digits =>
                  Top_Token.Attributes (Ada_Digits_Attribute) := True;
               when Tok_Range =>
                  Top_Token.Attributes (Ada_Range_Attribute) := True;
               when Tok_Mod =>
                  Top_Token.Attributes (Ada_Mod_Attribute) := True;
               when Tok_New =>
                  Top_Token.Attributes (Ada_New_Attribute) := True;
               when Tok_Not =>
                  Top_Token.Attributes (Ada_Not_Attribute) := True;
               when Tok_Null =>
                  Top_Token.Attributes (Ada_Null_Attribute) := True;
               when Tok_Out =>
                  Top_Token.Attributes (Ada_Out_Attribute) := True;
               when Tok_Tagged =>
                  Top_Token.Attributes (Ada_Tagged_Attribute) := True;
               when Tok_In =>
                  Top_Token.Attributes (Ada_In_Attribute) := True;
               when Tok_Interface =>
                  Top_Token.Attributes (Ada_Interface_Attribute) := True;
               when Tok_Record =>
                  Top_Token.Attributes (Ada_Record_Attribute) := True;
               when others =>
                  null;
            end case;
         end if;

         --  Note: the order of the following conditions is important

         if Reserved = Tok_Body then
            Subprogram_Decl := False;

            if Top_Token.Token = Tok_Package then
               Top_Token.Package_Declaration := False;
            end if;

         elsif Reserved = Tok_Tagged then
            null;

         elsif Prev_Token /= Tok_End and then Reserved = Tok_Case then
            if Align_On_Colons
              and then Top_Token.Token = Tok_Record
            then
               Temp.Align_Colon := Compute_Alignment (Prec);
            end if;

            Temp.Visibility_Section := Top_Token.Visibility_Section;

            --  If the case is in a type declaration (e.g. protected), then
            --  mark itself as a type declaration so we will extract the
            --  corresponding fields.
            if Top_Token.Type_Declaration then
               Temp.Type_Declaration := True;
            end if;

            --  If the case is in a record, then mark itself as a record so we
            --  will extract the corresponding fields.
            if Top_Token.Attributes (Ada_Record_Attribute) then
               Temp.Attributes (Ada_Record_Attribute) := True;
            end if;

            Do_Indent (Prec, Num_Spaces);

            if Indent_Case_Extra = RM_Style then
               Temp.Extra_Indent := True;
               Num_Spaces := Num_Spaces + Indent_Level;
            end if;

            Push (Tokens, Temp);

         elsif Prev_Token /= Tok_End and then
           (Reserved = Tok_If
             or else Reserved = Tok_For
             or else Reserved = Tok_While
             or else Reserved = Tok_Accept)
         then
            Push (Tokens, Temp);

         elsif Reserved = Tok_Abort then
            if Top_Token.Token = Tok_Select and then Prev_Token = Tok_Then then
               --  Temporarily unindent if we have a 'then abort' construct,
               --  with 'abort' on its own line, e.g:
               --  select
               --     Foo;
               --  then
               --    abort
               --     Bar;

               Do_Indent
                 (Prec, Num_Spaces - Indent_Level, Continuation => True);
            end if;

         elsif Reserved = Tok_Renames then
            if Subprogram_Decl then
               --  function A (....)
               --    renames B;

               Indent_Function_Return (Prec);
            end if;

            if not Top_Token.Declaration
              and then (Top_Token.Token = Tok_Function
                or else Top_Token.Token = Tok_Procedure
                or else Top_Token.Token = Tok_Package)
            then
               --  Terminate current subprogram declaration, e.g:
               --  procedure ... renames ...;

               Subprogram_Decl := False;
               Pop (Tokens);
            end if;

         elsif Prev_Token = Tok_Is
           and then not In_Generic
           and then Top_Token.Token /= Tok_Type
           and then Top_Token.Token /= Tok_Task
           and then Top_Token.Token /= Tok_Protected
           and then Top_Token.Token /= Tok_Subtype
           and then (Reserved = Tok_New
                     or else Reserved = Tok_Abstract
                     or else Reserved = Tok_Separate
                     or else (Reserved = Tok_Null
                              and then Top_Token.Token = Tok_Procedure))
         then
            --  Handle indentation of e.g.
            --
            --  function Abstract_Func
            --    return String
            --    is abstract;

            if Top_Token.Token = Tok_Function
              and then Reserved /= Tok_New
              and then
                (Reserved = Tok_Abstract
                 or else Reserved = Tok_Separate
                 or else
                   Start_Of_Line /= Line_Start (Buffer, Top_Token.Sloc.Index))
            then
               Indent_Function_Return (Prec);
               Num_Spaces := Num_Spaces + Indent_Level;
            end if;

            Pop (Tokens);

            --  Unindent since this is a declaration, e.g:
            --  package ... is new ...;
            --  function ... is abstract;
            --  function ... is separate;
            --  procedure ... is null;

            Num_Spaces := Num_Spaces - Indent_Level;

            if Num_Spaces < 0 then
               Num_Spaces := 0;
               Syntax_Error := True;
            end if;

         elsif Reserved = Tok_Function
           or else Reserved = Tok_Procedure
           or else Reserved = Tok_Package
           or else Reserved = Tok_Task
           or else Reserved = Tok_Protected
           or else Reserved = Tok_Entry
         then
            if Reserved = Tok_Package then
               Temp.Package_Declaration := True;

            elsif Reserved /= Tok_Protected
              and then ((Top_Token.Token = Tok_Type
                         and then (Prev_Token /= Tok_Access
                                   or else Prev_Prev_Token = Tok_Is))
                        or else (Prev_Token /= Tok_Access
                                 and then Prev_Token /= Tok_Protected
                                 and then Prev_Token /= Tok_Constant))
            then
               --  take into account the following:
               --  type P is access procedure;
               --
               --  and ignore the following cases:
               --  type P is array () of access procedure;
               --  procedure P (X : access procedure);

               Subprogram_Decl := True;
               Num_Parens      := 0;
            end if;

            if Prev_Token = Tok_Access or else Prev_Token = Tok_Protected then
               --  Ada 2005 anonymous access subprogram parameter:
               --  procedure P (F : access [protected] procedure);

               null;

            elsif Top_Token.Token = Tok_Type then
               null;

            elsif not In_Generic then
               if not Top_Token.Declaration
                 and then (Top_Token.Token = Tok_Function
                           or else Top_Token.Token = Tok_Procedure)
               then
                  --  There was a function declaration, e.g:
                  --
                  --  procedure xxx ();
                  --  procedure ...
                  Pop (Tokens);
               end if;

               Push (Tokens, Temp);

            elsif Prev_Token /= Tok_With then
               --  unindent after a generic declaration, e.g:
               --
               --  generic
               --     with procedure xxx;
               --     with function xxx;
               --     with package xxx;
               --  package xxx is

               Num_Spaces := Num_Spaces - Indent_Level;

               if Num_Spaces < 0 then
                  Num_Spaces := 0;
                  Syntax_Error := True;
               end if;

               In_Generic := False;

               Push (Tokens, Temp);
            end if;

         elsif Reserved = Tok_Return
           and then Subprogram_Decl
         then
            Indent_Function_Return (Prec);

         elsif Reserved = Tok_End
           or else Reserved = Tok_Elsif
           or else (Reserved = Tok_Null
                    and then Prev_Token = Tok_Is
                    and then Top_Token.Token = Tok_Procedure)
         then
            --  unindent after end of elsif, e.g:
            --
            --  if xxx then
            --     xxx
            --  elsif xxx then
            --     xxx
            --  end if;

            if Reserved = Tok_End then
               case Top_Token.Token is
                  when Tok_When | Tok_Exception =>
                     --  Undo additional level of indentation, as in:
                     --     ...
                     --  exception
                     --     when others =>
                     --        null;
                     --  end;

                     Num_Spaces := Num_Spaces - Indent_Level;

                     --  End of subprogram
                     Pop (Tokens);

                  when Tok_Case =>
                     if Top_Token.Extra_Indent then
                        Num_Spaces := Num_Spaces - Indent_Level;
                     end if;

                  when Tok_Record =>
                     --  If the "record" keyword was on its own line

                     if Top_Token.Extra_Indent then
                        Do_Indent (Prec, Num_Spaces - Indent_Level);
                        Num_Spaces := Num_Spaces - Indent_Record;
                     end if;

                  when others =>
                     null;
               end case;

               Pop (Tokens);

            elsif Top_Token.Token = Tok_Then
              and then Reserved = Tok_Elsif
            then
               Top_Token.Token := Tok_If;
            end if;

            Num_Spaces := Num_Spaces - Indent_Level;

            if Num_Spaces < 0 then
               Num_Spaces   := 0;
               Syntax_Error := True;
            end if;

         elsif Reserved = Tok_With then
            if not In_Generic then
               if Top_Token.Token = No_Token then
                  Push (Tokens, Temp);

               elsif Top_Token.Token = Tok_Type then
                  --  ??? is this correct for Ada 05 constructs:
                  --  task type foo is new A with entry A; end task;

                  Top_Token.Attributes (Ada_Tagged_Attribute) := True;
               end if;
            end if;

         elsif Reserved = Tok_Use and then
           (Top_Token.Token = No_Token or else
             (Top_Token.Token /= Tok_For
               and then Top_Token.Token /= Tok_Record))
         then
            Push (Tokens, Temp);

         elsif    (Reserved = Tok_Is and then not In_Generic)
           or else Reserved = Tok_Declare
           or else Reserved = Tok_Begin
           or else Reserved = Tok_Do
           or else (Prev_Token /= Tok_Or  and then Reserved = Tok_Else)
           or else (Prev_Token /= Tok_And and then Reserved = Tok_Then)
           or else (Prev_Token /= Tok_End and then Reserved = Tok_Select)
           or else (Reserved = Tok_Or
                    and then (Top_Token.Token = Tok_Select
                              or else Top_Token.Token = Tok_When))
           or else (Prev_Token /= Tok_End and then Reserved = Tok_Loop)
           or else (Prev_Token /= Tok_End and then Prev_Token /= Tok_Null
                    and then Reserved = Tok_Record)
           or else ((Top_Token.Token = Tok_Exception
                     or else Top_Token.Token = Tok_Case
                     or else Top_Token.Token = Tok_Select)
                    and then Reserved = Tok_When
                    and then Prev_Token /= Tok_Exit
                    and then Prev_Prev_Token /= Tok_Exit)
           or else (Top_Token.Declaration
                    and then Reserved = Tok_Private
                    and then
                      (Prev_Token /= Tok_Is
                       or else Top_Token.Token = Tok_Package)
                    and then Prev_Token /= Tok_Limited
                    and then Prev_Token /= Tok_With)
         then
            --  unindent for this reserved word, and then indent again, e.g:
            --
            --  procedure xxx is
            --     ...
            --  begin    <--
            --     ...

            if Reserved = Tok_Do then
               if Top_Token.Token = Tok_Accept then
                  Top_Token.Token := Tok_Do;
               else
                  --  Extended return statement:
                  --  return X : xxx do
                  --     ...
                  --  end;

                  Temp.Token := Tok_Return;
                  Push (Tokens, Temp);
               end if;
            end if;

            if Reserved = Tok_Select then
               Push (Tokens, Temp);

            elsif Top_Token.Token = Tok_If
              and then Reserved = Tok_Then
            then
               --  Notify that we're past the 'if' condition

               Top_Token.Token := Tok_Then;

            elsif Reserved = Tok_Loop then
               if Top_Token.Token = Tok_While
                 or else Top_Token.Token = Tok_For
               then
                  --  Replace token since this is a loop construct
                  --  but keep the original source location.

                  Top_Token.Token := Tok_Loop;

               else
                  Push (Tokens, Temp);
               end if;

            elsif Reserved = Tok_Declare then
               if Align_On_Colons then
                  Temp.Align_Colon :=
                    Compute_Alignment
                      (Prec, Stop_On_Blank_Line => Stop_On_Blank_Line);
               end if;

               Temp.Declaration := True;
               Push (Tokens, Temp);

            elsif Reserved = Tok_Is then
               if not In_Generic then
                  case Top_Token.Token is
                     when Tok_Case | Tok_When | Tok_Type | Tok_Subtype =>
                        Top_Token.Type_Definition_Section := True;

                     when Tok_Task | Tok_Protected =>
                        if Top_Token.Token = Tok_Protected then
                           Top_Token.Type_Definition_Section := True;
                        end if;

                        Subprogram_Decl := False;

                        if Align_On_Colons then
                           Top_Token.Align_Colon := Compute_Alignment
                             (Prec, Stop_On_Blank_Line => Stop_On_Blank_Line);
                        end if;

                        Top_Token.Declaration := True;

                     when others =>
                        Subprogram_Decl := False;

                        if Top_Token.Token = Tok_Function then
                           Index_Next := Current + 1;

                           --  Skip blanks on current line

                           while Index_Next < Buffer'Last
                             and then Buffer (Index_Next) /= ASCII.LF
                             and then (Buffer (Index_Next) = ' '
                                       or else Buffer (Index_Next) = ASCII.HT)
                           loop
                              Index_Next := Index_Next + 1;
                           end loop;
                        end if;

                        if Align_On_Colons then
                           Top_Token.Align_Colon := Compute_Alignment
                             (Prec, Stop_On_Blank_Line => Stop_On_Blank_Line);
                        end if;

                        Top_Token.Declaration := True;
                  end case;
               end if;

            elsif Reserved = Tok_Begin then
               if Top_Token.Declaration then
                  Num_Spaces := Num_Spaces - Indent_Level;
                  Top_Token.Align_Colon := 0;
                  Top_Token.Declaration := False;

               else
                  Push (Tokens, Temp);
               end if;

            elsif Reserved = Tok_Record then
               --  Is "record" the first keyword on the line ?
               --  If True, we are in a case like:
               --     type A is
               --        record    --  from Indent_Record
               --           null;
               --        end record;

               if not Indent_Done then
                  Temp.Extra_Indent := True;
                  Num_Spaces := Num_Spaces + Indent_Record;
                  Do_Indent (Prec, Num_Spaces);
               end if;

               if Top_Token.Token = Tok_Type then
                  Top_Token.Attributes (Ada_Record_Attribute) := True;
                  Temp.Attributes (Ada_Record_Attribute) := True;
                  Temp.Type_Definition_Section :=
                    Top_Token.Type_Definition_Section;
                  Num_Spaces := Num_Spaces + Indent_Level;

                  if Align_On_Colons then
                     Temp.Align_Colon := Compute_Alignment (Prec);
                  end if;
               end if;

               Push (Tokens, Temp);

            elsif Reserved = Tok_Else
              or else (Top_Token.Token = Tok_Select
                       and then Reserved = Tok_Then)
              or else Reserved = Tok_When
              or else Reserved = Tok_Or
              or else Reserved = Tok_Private
            then
               if (Reserved = Tok_Or or else Reserved = Tok_Else)
                 and then Top_Token.Token = Tok_When
               then
                  Num_Spaces := Num_Spaces - Indent_Level;
                  Pop (Tokens);
                  Top_Token := Top (Tokens);
               end if;

               if Reserved /= Tok_When
                 or else Top_Token.Token /= Tok_Select
               then
                  Num_Spaces := Num_Spaces - Indent_Level;
               end if;

               if Reserved = Tok_Private then
                  Top_Token.Visibility_Section := Visibility_Private;
               end if;

               if Reserved = Tok_When then
                  if Top_Token.Token = Tok_Case then
                     if Indent_Case_Extra = Automatic
                       and then not Top_Token.Extra_Indent
                       and then Prec - Start_Of_Line > Num_Spaces
                     then
                        Top_Token.Extra_Indent := True;
                        Num_Spaces := Num_Spaces + Indent_Level;
                     end if;
                  end if;

                  Push (Tokens, Temp);
               end if;
            end if;

            if Num_Spaces < 0 then
               Num_Spaces   := 0;
               Syntax_Error := True;
            end if;

            if Top_Token.Token /= Tok_Type
              and then Top_Token.Token /= Tok_Subtype
              and then (Token /= Tok_Is
                        or else In_Generic
                        or else Top_Token.Token /= Tok_Function
                        or else not
                          (Look_For (Index_Next, "abstract")
                           or else Look_For (Index_Next, "separate")))
               --  'is abstract|separate' will be indented when handling
               --  'abstract|separate' for functions
            then
               Do_Indent (Prec, Num_Spaces);
               Num_Spaces := Num_Spaces + Indent_Level;
            end if;

         elsif Reserved = Tok_Or
           or else Reserved = Tok_And
         then
            --  "and then", "or else", "and" and "or" should get an extra
            --  indentation on line start, e.g:
            --  if ...
            --    and then ...

            if Top (Indents).Level = None then
               if Continuation_Val > 0 then
                  Continuation_Val := Continuation_Val - Indent_Continue;
               end if;

               Do_Indent (Prec, Num_Spaces, Continuation => True);
            else
               Do_Indent (Prec, Num_Spaces);
            end if;

         elsif Reserved = Tok_Generic then
            --  Indent before a generic entity, e.g:
            --
            --  generic
            --     type ...;

            Do_Indent (Prec, Num_Spaces);
            Num_Spaces := Num_Spaces + Indent_Level;
            In_Generic := True;

         elsif (Reserved = Tok_Type
                and then Prev_Token /= Tok_With    --  with type
                and then Prev_Token /= Tok_Use)    --  use type
           or else Reserved = Tok_Subtype
         then
            --  Entering a type declaration/definition.

            if Prev_Token = Tok_Task               --  task type
              or else Prev_Token = Tok_Protected   --  protected type
            then
               Top_Token.Type_Declaration := True;
            else
               Push (Tokens, Temp);
            end if;

         elsif Reserved = Tok_Exception then
            if Top_Token.Token /= Tok_Colon then
               Num_Spaces := Num_Spaces - Indent_Level;
               Do_Indent (Prec, Num_Spaces);
               Num_Spaces := Num_Spaces + 2 * Indent_Level;
               Push (Tokens, Temp);
            end if;
         end if;

         return False;

      exception
         when Token_Stack.Stack_Empty =>
            Syntax_Error := True;
            return False;
      end Handle_Reserved_Word;

      ---------------
      -- Next_Word --
      ---------------

      procedure Next_Word
        (P           : in out Natural;
         Terminated  : out Boolean;
         End_Reached : out Boolean)
      is
         Comma           : String := ", ";
         Spaces          : String := "    ";
         End_Of_Line     : Natural;
         Start_Of_Line   : Natural;
         Long            : Natural;
         First           : Natural;
         Last            : Natural;
         Offs            : Natural;
         Align           : Natural;
         Adjust          : Natural;
         Insert_Spaces   : Boolean;
         Char            : Character;
         Prev_Prev_Token : Token_Type;
         Top_Token       : Token_Stack.Generic_Type_Access;
         Tmp             : Boolean;
         Token_Found     : Boolean;

         procedure Close_Parenthesis;
         --  Current buffer contents is a closed parenthesis,
         --  reset stacks and states accordingly.

         procedure Handle_Arrow;
         --  Current buffer contents is an arrow colon, handle it.
         --  In particular, align arrows in statements.

         procedure Handle_Colon;
         --  Current buffer contents is a colon, handle it.
         --  In particular, align colons in declarations.

         procedure Handle_Two_Chars (Second_Char : Character);
         --  Handle a two char operator, whose second char is Second_Char.

         procedure Preprocessor_Directive;
         --  Handle preprocessor directive.
         --  Assume that Buffer (P) = '#'

         procedure Skip_Blank_Lines;
         --  Skip empty lines

         procedure Skip_Comments;
         --  Skip comment & blank lines.

         -----------------------
         -- Close_Parenthesis --
         -----------------------

         procedure Close_Parenthesis is
         begin
            if not Is_Empty (Paren_Stack) then
               Pop (Paren_Stack);
            end if;

            if Indents = null or else Top (Indents).Level = None then
               --  Syntax error
               null;
            else
               if not Indent_Done then
                  --  Adjust indentation level for closing parenthesis at the
                  --  beginning of a line, e.g:
                  --
                  --    (x,
                  --     y
                  --    );  <-

                  Top (Indents).Level := Top (Indents).Level - 1;
                  Do_Indent (P, Num_Spaces);
               end if;

               Pop (Indents);
               Num_Parens := Num_Parens - 1;

               Top_Token := Top (Tokens);

               if Num_Parens = 0 then
                  Right_Assignment := False;
                  Paren_In_Middle := False;

                  if Top_Token.Token in Token_Class_Declk
                    and then Top_Token.Profile_End = 0
                    and then Subprogram_Decl
                  then
                     Top_Token.Profile_End := P;
                     Top_Token.Align_Colon := 0;
                  end if;
               end if;
            end if;
         end Close_Parenthesis;

         ------------------
         -- Handle_Colon --
         ------------------

         procedure Handle_Colon is
            Align_Colon  : Natural := 0;
            Non_Blank    : Natural;
            Offset_Align : Natural;
            First_Paren  : Natural;
            Colon_Token  : Token_Stack.Generic_Type_Access;

         begin
            Prev_Token := Tok_Colon;
            Non_Blank := Start_Of_Line;
            Is_Parameter := False;

            if Format then
               Skip_Blanks (Buffer, Non_Blank);
            end if;

            if Top_Token.Declaration
              and then Top_Token.Token = Tok_Identifier
            then
               Pop (Tokens);
               Top_Token := Top (Tokens);

               declare
                  Val : Extended_Token;
               begin
                  --  Create a dummy token to separate variables
                  --  declaration from their type.

                  Val.Token := Tok_Colon;
                  Val.Is_Parameter := Subprogram_Decl;

                  if Align_Decl_On_Colon then
                     Val.Colon_Col := P - Non_Blank;
                  end if;

                  Push (Tokens, Val);
                  Colon_Token := Top (Tokens);
               end;
            end if;

            --  Auto align colons in declarations (parameters, variables, ...)

            if Top_Token.Align_Colon = 0
              or else Num_Parens > 1
            then
               return;
            end if;

            Align_Colon := Top_Token.Align_Colon;

            if Format_Operators then
               if Buffer (Next_Char (P)) = ' '
                 or else Last - 1 = End_Of_Line
               then
                  Long := 2;
               else
                  Long := 3;
               end if;

               if Buffer (Prev_Char (P)) = ' ' then
                  Offs := 2;
                  Long := Long - 1;
               else
                  Align_Colon := Align_Colon - 1;
               end if;
            else
               Offs := 2;
               Long := 1;
            end if;

            if Num_Parens /= 0 then
               --  Handle properly alignment of first parameter in the
               --  following case:
               --
               --  procedure F (X : Integer;    <--
               --               Foo : Integer);

               First_Paren := Non_Blank;

               while First_Paren < P
                 and then Buffer (First_Paren) /= '('
               loop
                  First_Paren := First_Paren + 1;
               end loop;

               if Buffer (First_Paren) = '(' then
                  Align_Colon := Align_Colon + First_Paren - Non_Blank + 1;
               end if;
            end if;

            --  In case Align_Colon is too small, avoid truncating non blank
            --  characters by never using a negative offset.

            Offset_Align :=
              Integer'Max (0, Align_Colon - (P - Non_Blank + 1));

            if Align_Decl_On_Colon
              and then Colon_Token /= null
            then
               Colon_Token.Colon_Col := Colon_Token.Colon_Col + Offset_Align;
            end if;

            Replace_Text
              (First, Last,
               (1 .. Offset_Align => ' ')
                & Spaces (Offs .. Offs + Long - 1));
            Insert_Spaces := False;
         end Handle_Colon;

         ------------------
         -- Handle_Arrow --
         ------------------

         procedure Handle_Arrow is
            Align_Arrow  : Natural := Top (Indents).Align_Arrow;
            Non_Blank    : Natural;
            Offset_Align : Natural;
            J            : Natural;
            First_Paren  : Natural;

         begin
            Prev_Token := Tok_Arrow;

            if Top_Token.Token = Tok_When
              and then (Tokens.Next = null
                        or else
                          Tokens.Next.Val.Token /= Tok_Select)
            then
               Pop (Tokens);
               Top_Token := Top (Tokens);
            end if;

            Handle_Two_Chars ('>');

            if Align_Arrow = 0 then
               return;
            end if;

            if not Is_Blank (Buffer (P - 2)) then
               Align_Arrow := Align_Arrow - 1;
            end if;

            Non_Blank := Start_Of_Line;
            Skip_Blanks (Buffer, Non_Blank);

            --  Handle properly alignment of first parameter in the
            --  following case:
            --
            --  Foo (X => 1,    <--
            --       Foo => 2);

            First_Paren := 0;
            J := Non_Blank;

            while J < P - 1 loop
               if Buffer (J) = '('
                 and then (Buffer (J - 1) /= '''
                           or else Buffer (J + 1) /= ''')
                 and then First_Paren = 0
               then
                  First_Paren := J;
               end if;

               if Buffer (J) = ')'
                 and then (Buffer (J - 1) /= '''
                           or else Buffer (J + 1) /= ''')
               then
                  First_Paren := 0;
               end if;

               J := J + 1;
            end loop;

            if First_Paren /= 0 and then Buffer (First_Paren) = '(' then
               Align_Arrow := Align_Arrow + First_Paren - Non_Blank + 1;
            end if;

            --  In case Align_Arrow is too small, avoid truncating non blank
            --  characters by never using a negative offset.

            Offset_Align := Integer'Max (0, Align_Arrow - (P - Non_Blank + 1));

            Replace_Text
              (First, Last,
               (1 .. Offset_Align => ' ')
                & Spaces (Offs .. Offs + Long - 1));
            Insert_Spaces := False;
         end Handle_Arrow;

         ----------------------
         -- Handle_Two_Chars --
         ----------------------

         procedure Handle_Two_Chars (Second_Char : Character) is
         begin
            Last := P + 2;

            if Is_Blank (Buffer (Prev_Char (P))) then
               Offs := 2;
               Long := 2;

            else
               Long := 3;
            end if;

            P := Next_Char (P);

            if P < Buffer_Last
              and then not Is_Blank (Buffer (Next_Char (P)))
            then
               Long := Long + 1;
            end if;

            Spaces (3) := Second_Char;
         end Handle_Two_Chars;

         ----------------------
         -- Skip_Blank_Lines --
         ----------------------

         procedure Skip_Blank_Lines is
         begin
            if P > Buffer_Last then
               return;
            end if;

            if Buffer (P) = ASCII.LF or else Buffer (P) = ASCII.CR then
               while P < Buffer_Last and then
                 (Buffer (P) = ASCII.LF or else Buffer (P) = ASCII.CR)
               loop
                  if Buffer (P) = ASCII.LF then
                     New_Line (Line_Count);
                     Indent_Done := False;
                  end if;

                  P := Next_Char (P);
               end loop;

               if P < Buffer_Last then
                  Start_Of_Line := P;
                  End_Of_Line   := Line_End (Buffer, Start_Of_Line);
               end if;

            end if;
         end Skip_Blank_Lines;

         -------------------
         -- Skip_Comments --
         -------------------

         procedure Skip_Comments is
            Prev_Start_Line : Natural;
            Last            : Natural;
            Ref_Indent      : Natural;
            Success         : Boolean;
         begin
            Ref_Indent := Num_Spaces;

            if not Indent_Done
              and then Stick_Comments
              and then (Prev_Token = Tok_Is or else Prev_Token = Tok_Record)
              and then Start_Of_Line > Buffer'First + 2
              and then Buffer (Start_Of_Line - 2) /= ASCII.LF
            then
               Ref_Indent := Ref_Indent - Indent_Level;
            end if;

            while P < Buffer_Last
              and then Buffer (P) = '-'
              and then Buffer (P + 1) = '-'
            loop
               Prev_Start_Line := Start_Of_Line;
               Prev_Line := Line_Count;
               First := P;
               Comments_Skipped := True;

               --  If we do not indent here, then automatic indentation
               --  won't work for comments right after 'is' and 'begin',
               --  e.g:
               --  procedure Foo is
               --  begin
               --     --  comment

               if Indent_Comments then
                  Do_Indent (P, Ref_Indent);
               end if;

               --  Keep track of the indentation of the first comment line,
               --  in case we're doing incremental reformatting: in this case,
               --  we want to follow the indentation (possibly manual) of this
               --  first line.

               if Ref_Indent = Num_Spaces
                 and then To /= 0
                 and then Line_Count not in From .. To
               then
                  Ref_Indent := P - Start_Of_Line - Indent_Offset;
               end if;

               Next_Line (Buffer, P + 1, P, Success);
               Last := P;

               if Success then
                  New_Line (Line_Count);
                  Last := Prev_Char (Last);
                  Indent_Done := False;
               end if;

               loop
                  --  Skip blank lines
                  --  ??? need to handle ASCII.CR as well, although we now
                  --  only use LF separators internally in GPS.

                  while P < Buffer_Last and then Buffer (P) = ASCII.LF loop
                     Ref_Indent := Num_Spaces;
                     New_Line (Line_Count);
                     P := P + 1;
                  end loop;

                  Start_Of_Line := P;

                  if P /= Buffer_Last then
                     while Buffer (P) = ' ' or else Buffer (P) = ASCII.HT loop
                        P := P + 1;

                        if P = Buffer_Last then
                           exit;
                        end if;
                     end loop;
                  end if;

                  exit when P = Buffer_Last or else Buffer (P) /= ASCII.LF;
               end loop;

               End_Of_Line := Line_End (Buffer, P);
               Padding     := 0;

               if Indent_Comments and then Buffer (P) = ASCII.LF then
                  --  Indent last buffer line before exiting, to position
                  --  the cursor at the right location

                  pragma Assert (P = Buffer_Last);
                  Do_Indent (P, Ref_Indent);
               end if;

               if Callback /= null then
                  if Callback
                    (Comment_Text,
                       (Prev_Line, First - Prev_Start_Line + 1, First),
                       (Prev_Line, Last - Prev_Start_Line + 1, Last),
                     False)
                  then
                     Terminated := True;
                     return;
                  end if;
               end if;
            end loop;

            if P >= Buffer_Last then
               End_Reached := True;
            end if;
         end Skip_Comments;

         ----------------------------
         -- Preprocessor_Directive --
         ----------------------------

         procedure Preprocessor_Directive is
         begin
            --  Skip line

            while P < Buffer'Last
              and then Buffer (P + 1) /= ASCII.LF
            loop
               P := P + 1;
            end loop;

            --  Mark this line as indented, so that the current indentation is
            --  kept.

            Indent_Done := True;
         end Preprocessor_Directive;

      begin  --  Next_Word
         Start_Of_Line := Line_Start (Buffer, P);
         End_Of_Line   := Line_End (Buffer, Start_Of_Line);
         Terminated    := False;
         End_Reached   := False;

         loop
            Skip_Blank_Lines;
            Skip_Comments;

            if (End_Reached and then Buffer (Buffer_Last) /= ASCII.LF)
              or else Terminated
            then
               return;
            end if;

            exit when P >= Buffer_Last
              or else Is_Entity_Letter
                (UTF8_Get_Char (Buffer (P .. Buffer_Last)));

            --  WARNING: any call to Pop (Token) during the case statement
            --  below should be followed by a recomputation of Top_Token.
            --  See e.g. Handle_Arrow where this is done.
            --  Only if Top_Token is not accessed further down this procedure
            --  can the recomputation be omitted.

            Top_Token := Top (Tokens);
            Prev_Prev_Token := Prev_Token;
            Token_Found := True;

            case Buffer (P) is
               when '#' =>
                  First := P;
                  Prev_Token := Tok_Pound;

                  if (P = Buffer'First
                      or else not Is_Alphanumeric (Buffer (P - 1)))
                    and then Is_Letter (Buffer (P + 1))
                  then
                     Preprocessor_Directive;
                  end if;

               when '(' =>
                  First := P;
                  Prev_Token := Tok_Left_Paren;

                  if Subprogram_Decl and then Num_Parens = 0 then
                     Is_Parameter := True;
                  end if;

                  if not Is_Empty (Paren_Stack) then
                     Push (Paren_Stack, Top (Paren_Stack).all);
                  elsif Top_Token.Token = Tok_Type then
                     Push (Paren_Stack, Type_Declaration);
                  elsif Prev_Prev_Token = Tok_Return then
                     Push (Paren_Stack, Aggregate);
                  elsif Prev_Prev_Token in Reserved_Token_Type then
                     Push (Paren_Stack, Conditional);
                  elsif Prev_Prev_Token = Tok_Identifier then
                     Push (Paren_Stack, Function_Call);
                  else
                     Push (Paren_Stack, Aggregate);
                  end if;

                  if Continuation_Val > Indent_Continue
                    and then Top (Indents).Level /= None
                  then
                     Continuation_Val := 0;
                  end if;

                  if P > Buffer'First then
                     Char := Buffer (Prev_Char (P));
                  else
                     Char := ' ';
                  end if;

                  if Indent_Done then
                     Adjust := Indent_Continue + Continuation_Val;
                     Paren_In_Middle := True;

                     --  If Prev_Prev_Token is an operator, it means that
                     --  spaces have already been inserted.

                     if Format_Operators
                       and then not Is_Blank (Char)
                       and then Char /= '('
                       and then Char /= '''
                       and then not Is_Extended_Operator (Prev_Prev_Token)
                     then
                        Spaces (2) := Buffer (P);
                        Replace_Text (P, P + 1, Spaces (1 .. 2));
                     end if;

                  else
                     --  Indent with extra spaces if the '(' is the first
                     --  non blank character on the line

                     if Prev_Prev_Token = Tok_Comma then
                        Adjust := 1;
                     else
                        Adjust := Indent_Continue + 1;
                     end if;

                     if Prev_Prev_Token = Tok_Comma
                       or else Prev_Prev_Token = Tok_Ampersand
                     then
                        Do_Indent (P, Num_Spaces);
                     else
                        if Prev_Prev_Token = Tok_Colon_Equal
                          and then Top_Token.Colon_Col /= 0
                          and then Continuation_Val = 0
                        then
                           Continuation_Val :=
                             Top (Tokens).Colon_Col + 4 - Indent_Continue;
                        end if;

                        Adjust := Adjust + Continuation_Val;
                        Tmp := Paren_In_Middle;
                        Do_Indent
                          (P, Num_Spaces,
                           Continuation =>
                             Prev_Prev_Token = Tok_Apostrophe
                             or else Prev_Prev_Token = Tok_Arrow
                             or else not Paren_In_Middle
                             or else Prev_Prev_Token in Reserved_Token_Type);
                        Paren_In_Middle := Tmp;
                     end if;
                  end if;

                  Num_Parens := Num_Parens + 1;
                  Align      := 0;

                  if Num_Parens = 1
                    and then Top_Token.Token in Token_Class_Declk
                    and then Top_Token.Profile_Start = 0
                  then
                     if Subprogram_Decl then
                        Top_Token.Profile_Start := P;
                     end if;

                     if Align_On_Colons then
                        Top_Token.Align_Colon := Compute_Alignment
                          (P + 1, Skip_First_Line => False);
                     end if;

                  else
                     if Align_On_Arrows then
                        Align := Compute_Alignment
                          (P + 1,
                           Skip_First_Line => False,
                           Align_On        => Tok_Arrow);
                     end if;
                  end if;

                  --  Indent on the left parenthesis for subprogram & type
                  --  declarations, and for subprogram calls/aggregates with no
                  --  nested parenthesis except on the same line and for
                  --  arrows+paren as in:
                  --     X := (Foo => (Y,
                  --                   Z));
                  --  In other cases (complex subprogram call),
                  --  indent as for continuation lines.

                  declare
                     Level : Integer;
                  begin
                     if Top (Paren_Stack).all = Conditional then
                        Level := P - Start_Of_Line + Padding
                                 + Indent_Conditional;

                     elsif Subprogram_Decl
                       or else Top (Paren_Stack).all = Type_Declaration
                       or else Prev_Prev_Token = Tok_Arrow
                       or else (Format
                                and then Num_Parens = 1
                                and then Find_Multi_Line_Paren (P + 1) = 0)
                       or else (Format and then Find_Arrow (P + 1) /= 0)
                     then
                        Level := P - Start_Of_Line + Padding + 1;
                     else
                        if Top (Indents).Level = None then
                           Level := Num_Spaces + Adjust;
                        elsif Prev_Prev_Token = Tok_Left_Paren
                          or else Top (Indents).Line = Line_Count
                        then
                           Level := Top (Indents).Level;
                        else
                           Level := Top (Indents).Level + Adjust;
                        end if;
                     end if;

                     Push (Indents, (Level, Align, Line_Count));
                  end;

                  if Continuation_Val > 0 then
                     Continuation_Val := Continuation_Val - Indent_Continue;
                  end if;

               when ')' =>
                  if (Top_Token.Token = Tok_Colon
                      or else Top_Token.Token = Tok_Identifier)
                    and then
                      (Top_Token.Is_Parameter
                       or else
                         (Top_Token.Is_In_Type_Definition
                          and then not
                            Top_Token.Attributes (Ada_Record_Attribute)
                          and then not Top_Token.Type_Declaration))
                  then
                     if Top_Token.Token = Tok_Identifier
                       and then not (Top_Token.Is_In_Type_Definition
                                     and then not Top_Token.Attributes
                                       (Ada_Record_Attribute)
                                     and then not Top_Token.Type_Declaration)
                     then
                        --  This handles cases where we have a family entry.
                        --  For example:
                        --    entry E (Integer);
                        --  here Integer is a type, so we don't want it in the
                        --  constructs. But is has already been pushed. The
                        --  code below disactivate its addition to the
                        --  constructs

                        Top_Token.Token := Tok_Colon;
                     end if;

                     Pop (Tokens);
                  end if;

                  First := P;
                  Prev_Token := Tok_Right_Paren;
                  Close_Parenthesis;

               when '"' =>
                  declare
                     Len    : Natural;
                     Entity : Language_Entity;
                  begin
                     First := P;

                     while P < End_Of_Line loop
                        P := Next_Char (P);

                        exit when Buffer (P) = '"';
                     end loop;

                     if Buffer (P) /= '"' then
                        --  Syntax error: the string was not terminated
                        --  Try to recover properly, and in particular, try
                        --  to reset the parentheses stack.

                        if Num_Parens > 0 then
                           Close_Parenthesis;
                        end if;
                     end if;

                     if (Top_Token.Token in Token_Class_Declk
                         and then Top_Token.Ident_Len = 0)
                       or else Prev_Token = Tok_End
                     then
                        --  This is an operator symbol, e.g function ">=" (...)

                        if Prev_Token /= Tok_End then
                           Len := P - First + 1;
                           Top_Token.Identifier (1 .. Len) :=
                             Buffer (First .. P);
                           Top_Token.Ident_Len := Len;
                           Top_Token.Sloc_Name.Line := Line_Count;
                           Top_Token.Sloc_Name.Column :=
                             First - Start_Of_Line + 1;
                           Top_Token.Sloc_Name.Index := First;
                        end if;

                        Prev_Token := Tok_Operator_Symbol;
                        Entity     := Identifier_Text;
                     else
                        Prev_Token := Tok_String_Literal;
                        Entity     := String_Text;
                     end if;

                     Compute_Indentation
                       (Prev_Token, Prev_Prev_Token, P, Num_Spaces);

                     if Callback /= null then
                        if Callback
                          (Entity,
                           (Line_Count, First - Start_Of_Line + 1,
                            First),
                           (Line_Count, P - Start_Of_Line + 1, P),
                           False)
                        then
                           Terminated := True;
                           return;
                        end if;
                     end if;
                  end;

               when '&' | '+' | '-' | '*' | '/' | ':' | '<' | '>' | '=' |
                    '|' | '.'
               =>
                  Spaces (2) := Buffer (P);
                  Spaces (3) := ' ';
                  First := P;
                  Last  := P + 1;
                  Offs  := 1;

                  case Buffer (P) is
                     when '+' | '-' =>
                        if Buffer (P) = '-' then
                           Prev_Token := Tok_Minus;
                        else
                           Prev_Token := Tok_Plus;
                        end if;

                        if Prev_Prev_Token = Tok_Identifier
                          and then
                            (P <= Buffer'First + 1
                             or else To_Upper (Buffer (P - 1)) /= 'E'
                             or else
                               (Buffer (P - 2) /= '#'
                                and then Buffer (P - 2) not in '0' .. '9'))
                          and then
                            (P = Buffer'Last
                             or else (Buffer (P + 1) /= '"'
                                      and then Buffer (P + 1) /= '('))
                        then
                           Insert_Spaces := True;
                        else
                           Insert_Spaces := False;
                        end if;

                     when '&' | '|' =>
                        if Buffer (P) = '&' then
                           Prev_Token := Tok_Ampersand;
                        else
                           Prev_Token := Tok_Vertical_Bar;
                        end if;

                        Insert_Spaces := True;

                     when '/' | ':' =>
                        Insert_Spaces := True;

                        if P < Buffer'Last
                          and then Buffer (P + 1) = '='
                        then
                           if Buffer (P) = ':'
                             and then Top_Token.Token = Tok_Colon
                           then
                              Right_Assignment := True;
                              Top_Token.Attributes
                                (Ada_Assign_Attribute) := True;

                              if Top_Token.Is_Parameter then
                                 Pop (Tokens);
                              end if;
                           end if;

                           Handle_Two_Chars ('=');

                           if Buffer (P) = '/' then
                              Prev_Token := Tok_Not_Equal;
                           else
                              Prev_Token := Tok_Colon_Equal;
                           end if;

                        elsif Buffer (P) = '/' then
                           Prev_Token := Tok_Slash;
                        else
                           Handle_Colon;
                        end if;

                     when '*' =>
                        Insert_Spaces := Buffer (Prev_Char (P)) /= '*';

                        if Buffer (Next_Char (P)) = '*' then
                           Handle_Two_Chars ('*');
                           Prev_Token := Tok_Double_Asterisk;
                        else
                           Prev_Token := Tok_Asterisk;
                        end if;

                     when '.' =>
                        Insert_Spaces := Buffer (Next_Char (P)) = '.';

                        if Insert_Spaces then
                           Handle_Two_Chars ('.');
                           Prev_Token := Tok_Dot_Dot;
                        else
                           Prev_Token := Tok_Dot;
                        end if;

                     when '<' =>
                        case Buffer (Next_Char (P)) is
                           when '=' =>
                              Insert_Spaces := True;
                              Prev_Token    := Tok_Less_Equal;
                              Handle_Two_Chars ('=');

                           when '<' =>
                              Prev_Token    := Tok_Less_Less;
                              Insert_Spaces := False;
                              Handle_Two_Chars ('<');

                           when '>' =>
                              Prev_Token    := Tok_Box;
                              Insert_Spaces := False;
                              Handle_Two_Chars ('>');

                           when others =>
                              Prev_Token    := Tok_Less;
                              Insert_Spaces := True;
                        end case;

                     when '>' =>
                        case Buffer (Next_Char (P)) is
                           when '=' =>
                              Insert_Spaces := True;
                              Prev_Token    := Tok_Greater_Equal;
                              Handle_Two_Chars ('=');

                           when '>' =>
                              Prev_Token    := Tok_Greater_Greater;
                              Insert_Spaces := False;
                              Handle_Two_Chars ('>');

                           when others =>
                              Prev_Token    := Tok_Greater;
                              Insert_Spaces := True;
                        end case;

                     when '=' =>
                        Insert_Spaces := True;

                        if Buffer (P + 1) = '>' then
                           Handle_Arrow;
                        else
                           Prev_Token := Tok_Equal;
                        end if;

                     when others =>
                        null;
                  end case;

                  if Spaces (3) = ' ' then
                     if Is_Blank (Buffer (P + 1))
                       or else Last - 1 = End_Of_Line
                     then
                        Long := 2;
                     else
                        Long := 3;
                     end if;
                  end if;

                  if P > Buffer'First and then Is_Blank (Buffer (P - 1)) then
                     Offs := 2;
                     Long := Long - 1;
                  end if;

                  if Num_Parens = 0
                    and then Top_Token.Token /= Tok_When
                  then
                     --  If we're not inside parens or a when statement,
                     --  then handle continuation lines here. Otherwise,
                     --  continuation lines are already handled separately.

                     Compute_Indentation
                       (Prev_Token, Prev_Prev_Token, P, Num_Spaces);
                  else
                     Do_Indent (P, Num_Spaces);
                  end if;

                  if Format_Operators and then Insert_Spaces then
                     Replace_Text
                       (First, Last, Spaces (Offs .. Offs + Long - 1));
                  end if;

               when ',' | ';' =>
                  First := P;

                  if Buffer (P) = ';' then
                     Prev_Token := Tok_Semicolon;
                     Right_Assignment := False;

                     if Top_Token.Token = Tok_Colon then
                        Pop (Tokens);

                     elsif Num_Parens = 0 then
                        if Subprogram_Decl
                          or else Top_Token.Token = Tok_Subtype
                          or else Top_Token.Token = Tok_For
                        then
                           if not In_Generic then
                              --  subprogram spec or type decl or repr. clause,
                              --  e.g:
                              --  procedure xxx (...);
                              --  type ... is ...;
                              --  for ... use ...;

                              Pop (Tokens);
                           end if;

                           Subprogram_Decl := False;

                        elsif Top_Token.Token = Tok_With
                          or else Top_Token.Token = Tok_Use
                          or else Top_Token.Token = Tok_Identifier
                          or else Top_Token.Token = Tok_Type
                          or else Top_Token.Token = Tok_Accept
                        then
                           Pop (Tokens);
                        end if;
                     end if;

                     if Subprogram_Decl then
                        Is_Parameter := True;
                     end if;

                  else
                     Prev_Token := Tok_Comma;

                     if Top_Token.Declaration
                       and then Top_Token.Token = Tok_Identifier
                     then
                        Pop (Tokens);

                     elsif Top_Token.Token = Tok_With
                       or else Top_Token.Token = Tok_Use
                     then
                        declare
                           Val : Extended_Token;
                        begin
                           --  Create a separate entry for each with clause:
                           --  with a, b;
                           --  will get two entries: one for a, one for b.

                           Val.Token := Top_Token.Token;
                           Pop (Tokens);
                           Val.Sloc.Line   := Line_Count;
                           Val.Sloc.Column :=
                             Prec - Line_Start (Buffer, Prec) + 2;
                           Val.Sloc.Index  := Prec + 1;
                           Val.Ident_Len := 0;
                           Push (Tokens, Val);
                        end;
                     end if;
                  end if;

                  Char := Buffer (P + 1);

                  if Format_Operators
                    and then Char /= ' ' and then P /= End_Of_Line
                  then
                     Comma (1) := Buffer (P);
                     Replace_Text (P, P + 1, Comma (1 .. 2));
                  end if;

               when ''' =>
                  --  Apostrophe. This can either be the start of a character
                  --  literal, an isolated apostrophe used in a qualified
                  --  expression or an attribute. We treat it as a character
                  --  literal if it does not follow a right parenthesis,
                  --  identifier, the keyword ALL or a literal. This means that
                  --  we correctly treat constructs like:
                  --    A := Character'('A');

                  First := P;

                  if Prev_Token = Tok_Identifier
                     or else Prev_Token = Tok_Right_Paren
                     or else Prev_Token = Tok_All
                     or else Prev_Token in Token_Class_Literal
                     or else P = End_Of_Line
                  then
                     Prev_Token := Tok_Apostrophe;
                  else
                     if P = End_Of_Line - 1 then
                        P := P + 1;
                     else
                        P := P + 2;
                     end if;

                     while P < End_Of_Line
                       and then Buffer (P) /= '''
                     loop
                        P := Next_Char (P);
                     end loop;

                     Prev_Token := Tok_Char_Literal;

                     if Callback /= null then
                        if Callback
                          (Character_Text,
                           (Line_Count, First - Start_Of_Line + 1, First),
                           (Line_Count, P - Start_Of_Line + 1, P),
                           False)
                        then
                           Terminated := True;
                           Comments_Skipped := False;
                           return;
                        end if;
                     end if;
                  end if;

               when others =>
                  Token_Found := False;
            end case;

            if Buffer (P) /= ' ' and then not Is_Control (Buffer (P)) then
               Comments_Skipped := False;
            end if;

            if Token_Found
              and then (Prev_Token in Tok_Double_Asterisk .. Tok_Colon_Equal
                        or else Prev_Token in Tok_Semicolon .. Tok_Dot_Dot)
            then
               if Callback /= null then
                  if Callback
                    (Operator_Text,
                     (Line_Count, First - Start_Of_Line + 1, First),
                     (Line_Count, P - Start_Of_Line + 1, P),
                     False)
                  then
                     Terminated := True;
                     Comments_Skipped := False;
                     return;
                  end if;
               end if;
            end if;

            P := Next_Char (P);
         end loop;

         Comments_Skipped := False;
         Terminated := False;
      end Next_Word;

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
           and then (To = 0 or else Line_Count in From .. To)
         then
            if Last_Replace_Line /= Line_Count then
               Last_Replace_Line := Line_Count;
               Padding := 0;
            end if;

            Start := Line_Start (Buffer, First);
            Replace
              (Line_Count,
               Padding + First - Start + 1,
               Padding + Last - Start + 1, Str);

            Padding := Padding + Str'Length - (Last - First);
         end if;
      end Replace_Text;

   begin  --  Analyze_Ada_Source
      if Buffer'Length = 0 then
         return;
      end if;

      --  Push a dummy token so that stack will never be empty

      Push (Tokens, Default_Extended);

      --  Push a dummy indentation so that stack will never be empty

      Push (Indents, (None, 0, 0));

      Next_Word (Prec, Terminated, End_Reached);

      --  If there is only one character in the buffer and the end of the
      --  buffer has been reached, enter the main loop anyway. Otherwise,
      --  the on-the-fly auto-casing will not work when adding a space after
      --  single character identifier.

      if (End_Reached
          and then Buffer_Last > 1
          and then Buffer (Buffer_Last) /= ASCII.LF)
        or else Terminated
      then
         return;
      end if;

      Current := End_Of_Word (Prec);

      Main_Loop :
      loop
         Str_Len := Current - Prec + 1;

         Str (1 .. Str_Len) := Buffer (Prec .. Current);

         Token := Get_Token (Str (1 .. Str_Len), Prev_Token);

         if Token = Tok_Identifier then
            --  Handle dotted names, e.g Foo.Bar.X

            Prev_Line := Line_Count;

            if Current < Buffer_Last then
               Index_Ident := End_Of_Identifier (Current + 1);

               if Index_Ident /= Current then
                  --  We have a dotted name, update indexes

                  Str_Len := Index_Ident - Prec + 1;

                  Str (Current - Prec + 2 .. Index_Ident - Prec + 1) :=
                    Buffer (Current + 1 .. Index_Ident);

                  Current := Index_Ident;
               end if;
            end if;

            Top_Token := Top (Tokens);
            Start_Of_Line := Line_Start (Buffer, Prec);

            if Top_Token.Ident_Len = 0
              and then (Top_Token.Token in Token_Class_Declk
                        or else Top_Token.Token = Tok_With)
            then
               --  Store enclosing entity name

               Top_Token.Identifier (1 .. Str_Len) := Buffer (Prec .. Current);
               Top_Token.Ident_Len := Str_Len;
               Top_Token.Sloc_Name.Line   := Prev_Line;
               Top_Token.Sloc_Name.Column := Prec - Start_Of_Line + 1;
               Top_Token.Sloc_Name.Index  := Prec;
            end if;

            if (Top_Token.Declaration
                or else Top_Token.Type_Declaration
                or else Top_Token.Attributes (Ada_Record_Attribute)
                or else Is_Parameter
                or else
                  (Top_Token.Type_Definition_Section
                   and then Top_Token.Token = Tok_Type
                   and then Top_Token.Attributes = No_Attribute))
              and then not In_Generic
              and then (Num_Parens = 0
                        or else Is_Parameter
                        or else Top_Token.Type_Definition_Section)
              and then (Prev_Token not in Reserved_Token_Type
                        or else Prev_Token = Tok_Declare
                        or else Prev_Token = Tok_Is
                        or else Prev_Token = Tok_Private
                        or else Prev_Token = Tok_Record)
            then
               --  This is a variable, a field declaration or a enumeration
               --  literal

               declare
                  Val : Extended_Token;
               begin
                  Val.Token       := Tok_Identifier;
                  Val.Sloc.Line   := Prev_Line;
                  Val.Sloc.Column := Prec - Start_Of_Line + 1;
                  Val.Sloc.Index  := Prec;
                  Val.Identifier (1 .. Str_Len) := Str (1 .. Str_Len);
                  Val.Ident_Len   := Str_Len;
                  Val.Sloc_Name   := Val.Sloc;
                  Val.Declaration := True;
                  Val.Visibility  := Top_Token.Visibility_Section;
                  Val.Is_Parameter := Is_Parameter;
                  Val.Is_In_Type_Definition :=
                    Top_Token.Type_Definition_Section;
                  Push (Tokens, Val);
               end;
            end if;

            Casing := Ident_Casing;

            if Prev_Token = Tok_Apostrophe
              and then To_Upper (Str (1 .. Str_Len)) = "CLASS"
            then
               Top_Token.Attributes (Ada_Class_Attribute) := True;
            end if;

            if Callback /= null then
               exit Main_Loop when Callback
                 (Identifier_Text,
                  (Prev_Line, Prec - Start_Of_Line + 1, Prec),
                  (Line_Count,
                   Current - Line_Start (Buffer, Current) + 1,
                   Current),
                  False);
            end if;

         elsif Prev_Token = Tok_Apostrophe
           and then (Token = Tok_Delta or else Token = Tok_Digits
                     or else Token = Tok_Mod or else Token = Tok_Range
                     or else Token = Tok_Access)
         then
            --  This token should not be considered as a reserved word

            Casing := Ident_Casing;

            if Callback /= null then
               Start_Of_Line := Line_Start (Buffer, Prec);

               exit Main_Loop when Callback
                 (Identifier_Text,
                  (Line_Count, Prec - Start_Of_Line + 1, Prec),
                  (Line_Count, Current - Start_Of_Line + 1, Current),
                  False);
            end if;

         elsif Token = No_Token then
            Casing := Unchanged;
         else
            Casing := Reserved_Casing;

            exit Main_Loop when Handle_Reserved_Word (Token);
         end if;

         if Indent_Params.Casing_Policy /= Disabled then
            case Casing is
               when Unchanged =>
                  null;

               when Upper | Lower | Mixed | Smart_Mixed  =>
                  Set_Case (Case_Exceptions, Str (1 .. Str_Len), Casing);
                  Replace_Text (Prec, Current + 1, Str (1 .. Str_Len));
            end case;
         end if;

         --  'is' is handled specially, so nothing is needed here

         if Token /= Tok_Is then
            Compute_Indentation (Token, Prev_Token, Prec, Num_Spaces);
         end if;

         Prec            := Current + 1;
         Prev_Prev_Token := Prev_Token;
         Prev_Token      := Token;

         exit Main_Loop when Prec > Buffer_Last;

         Next_Word (Prec, Terminated, End_Reached);

         exit Main_Loop when
           (End_Reached and then Buffer (Buffer_Last) /= ASCII.LF)
           or else Terminated;

         Current := End_Of_Word (Prec);
      end loop Main_Loop;

      --  Try to register partial constructs, friendlier

      Prec := Integer'Min (Prec, Buffer'Last);

      if Constructs /= null then
         while Top (Tokens).Token /= No_Token loop
            Pop (Tokens);
         end loop;
      end if;

      Clear (Tokens);
      Clear (Indents);

   exception
      when others =>
         --  Do not put calls to Put_Line here, these are undesirable, in
         --  particular under Windows when compiled with -mwindows where this
         --  will raise Device_Error.
         --  Also, this is not an unexpected exception: it is somewhat
         --  expected that exceptions (e.g. Constraint_Error) may be raised,
         --  and we do not want to behave unexpectedly in such cases.

         Clear (Tokens);
         Clear (Indents);
   end Analyze_Ada_Source;

end Ada_Analyzer;
