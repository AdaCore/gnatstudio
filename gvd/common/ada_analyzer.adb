with String_Utils;            use String_Utils;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Scans;                   use Scans;
with Ada.Unchecked_Deallocation;
with Generic_Stack;

package body Source_Analyzer is

   ---------------
   -- Constants --
   ---------------

   None   : constant := -1;
   Spaces : constant String (1 .. 256) := (others => ' ');
   --  Use to handle indentation in procedure Do_Indent below.

   -----------
   -- Types --
   -----------

   type Extended_Token is record
      Token       : Token_Type;
      Declaration : Boolean;
   end record;

   package Token_Stack is new Generic_Stack (Extended_Token);
   use Token_Stack;

   package Indent_Stack is new Generic_Stack (Integer);
   use Indent_Stack;

   subtype Word is Natural;

   --------------------------
   -- Line Buffer Handling --
   --------------------------

   --  The line buffer represents a buffer (e.g contents of a file) line
   --  by line. Line separators (LF or CR/LF) are kept at the end of the buffer
   --  It is recommended to take advantage of the bound information that comes
   --  with a String_Access so that there can be a direct mapping between
   --  the original raw buffer and a line buffer.
   --  Len is used to keep the length of the original line stored. Since this
   --  type is intended for making changes in buffers at a minimal cost
   --  (e.g avoiding copies of complete buffers when inserting a few
   --  characters), being able to convert from the original buffer's position
   --  information to the line buffer is critical and is achieved using the Len
   --  field.

   type Line_Buffer_Record;
   type Line_Buffer is access Line_Buffer_Record;
   type Line_Buffer_Record is record
      Line : String_Access;
      Len  : Natural;
      Next : Line_Buffer;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Line_Buffer_Record, Line_Buffer);

   type Extended_Line_Buffer is record
      First   : Line_Buffer;
      Current : Line_Buffer;
   end record;

   function To_Line_Buffer (Buffer : String) return Extended_Line_Buffer;
   --  Convert a string to a line buffer.
   --  CR/LF and LF are treated as end of lines.

   procedure Print (Buffer : Extended_Line_Buffer);
   --  Output the contents of Buffer on standard output.

   procedure Free (Buffer : in out Extended_Line_Buffer);
   --  Free the contents of buffer.

   ----------------------
   -- Parsing Routines --
   ----------------------

   function End_Of_Word (Buffer : String; P : Word) return Word;
   --  Return the end of the word pointed by P.

   function Get_Token (S : String) return Token_Type;
   --  Return a token_Type given a string.
   --  For efficiency, S is assumed to start at index 1.

   function Is_Word_Char (C : Character) return Boolean;
   --  Return whether C is a word character (alphanumeric or underscore).
   pragma Inline (Is_Word_Char);

   function Line_Start (Buffer : String; P : Word) return Word;
   --  Return the start of the line pointed by P.

   function Line_End   (Buffer : String; P : Word) return Word;
   --  Return the end of the line pointed by P.

   function Next_Line  (Buffer : String; P : Word) return Word;
   --  Return the start of the next line.

   function Next_Char  (P : Word) return Word;
   --  Return the next char in buffer. P is the current character.
   pragma Inline (Next_Char);

   procedure Next_Word
     (Buffer          : String;
      New_Buffer      : in out Extended_Line_Buffer;
      P               : in out Word;
      Num_Parens      : in out Integer;
      Semicolon       : in out Boolean;
      Line_Count      : in out Word;
      Indents         : in out Indent_Stack.Simple_Stack;
      Num_Spaces      : Integer;
      Indent_Continue : Natural;
      Indent_Done     : in out Boolean;
      Prev_Token      : in out Token_Type);
   --  Starting at Buffer (P), find the location of the next word
   --  and set P accordingly.
   --  Formatting of operators is performed by this procedure.

   function Prev_Char (P : Word) return Word;
   --  Return the previous char in buffer. P is the current character.
   pragma Inline (Prev_Char);

   procedure Replace_Text
     (Buffer  : in out Extended_Line_Buffer;
      First   : Word;
      Last    : Word;
      Replace : String);
   --  Replace the slice First .. Last - 1 in Buffer by Replace.

   procedure Do_Indent
     (Buffer      : String;
      New_Buffer  : in out Extended_Line_Buffer;
      Prec        : Word;
      Indents     : Indent_Stack.Simple_Stack;
      Num_Spaces  : Integer;
      Indent_Done : in out Boolean);
   --  Perform indentation by inserting spaces in the buffer.

   ------------------
   -- Is_Word_Char --
   ------------------

   function Is_Word_Char (C : Character) return Boolean is
   begin
      return C = '_' or else Is_Alphanumeric (C);
   end Is_Word_Char;

   ---------------
   -- Next_Char --
   ---------------

   function Next_Char (P : Word) return Word is
   begin
      return P + 1;
   end Next_Char;

   -----------------
   -- End_Of_Word --
   -----------------

   function End_Of_Word (Buffer : String; P : Word) return Word is
      Tmp : Word := P;
   begin
      while Tmp < Buffer'Last
        and then Is_Word_Char (Buffer (Next_Char (Tmp)))
      loop
         Tmp := Next_Char (Tmp);
      end loop;

      return Tmp;
   end End_Of_Word;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (S : String) return Token_Type is
   begin
      if S'Length = 1 then
         return Tok_Identifier;
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

            elsif S (2 .. S'Last) = "idgits" then
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

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (Buffer : String; P : Word) return Word is
   begin
      for J in reverse Buffer'First .. P loop
         if Buffer (J) = ASCII.LF or else Buffer (J) = ASCII.CR then
            return J + 1;
         end if;
      end loop;

      return Buffer'First;
   end Line_Start;

   --------------
   -- Line_End --
   --------------

   function Line_End (Buffer : String; P : Word) return Word is
   begin
      for J in P .. Buffer'Last loop
         if Buffer (J) = ASCII.LF or else Buffer (J) = ASCII.CR then
            return J - 1;
         end if;
      end loop;

      return Buffer'Last;
   end Line_End;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (Buffer : String; P : Word) return Word is
   begin
      for J in P .. Buffer'Last - 1 loop
         if Buffer (J) = ASCII.LF then
            return J + 1;
         end if;
      end loop;

      return Buffer'Last;
   end Next_Line;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (Buffer          : String;
      New_Buffer      : in out Extended_Line_Buffer;
      P               : in out Word;
      Num_Parens      : in out Integer;
      Semicolon       : in out Boolean;
      Line_Count      : in out Word;
      Indents         : in out Indent_Stack.Simple_Stack;
      Num_Spaces      : Integer;
      Indent_Continue : Natural;
      Indent_Done     : in out Boolean;
      Prev_Token      : in out Token_Type)
   is
      Comma         : String := ", ";
      Spaces        : String := "    ";
      End_Of_Line   : Word;
      Start_Of_Line : Word;
      Long          : Word;
      First         : Word;
      Last          : Word;
      Offs          : Word;
      Insert_Spaces : Boolean;
      Char          : Character;
      Padding       : Integer;

      procedure Handle_Two_Chars (Second_Char : Character);
      --  Handle a two char operator, whose second char is Second_Char.

      procedure Handle_Two_Chars (Second_Char : Character) is
      begin
         Last := P + 2;

         if Buffer (Prev_Char (P)) = ' ' then
            Offs := 2;
            Long := 2;

         else
            Long := 3;
         end if;

         P := Next_Char (P);

         if Buffer (Next_Char (P)) /= ' ' then
            Long := Long + 1;
         end if;

         Spaces (3) := Second_Char;
      end Handle_Two_Chars;

   begin
      End_Of_Line     := Line_End (Buffer, P);
      Start_Of_Line   := Line_Start (Buffer, P);

      if New_Buffer.Current.Line'First = Start_Of_Line then
         Padding := New_Buffer.Current.Line'Length - New_Buffer.Current.Len;
      else
         Padding := 0;
         Indent_Done := False;
      end if;

      loop
         if P > End_Of_Line then
            End_Of_Line   := Line_End (Buffer, P);
            Start_Of_Line := Line_Start (Buffer, P);
            Line_Count    := Line_Count + 1;
            Padding       := 0;
            Indent_Done := False;
         end if;

         --  Skip comments

         while Buffer (P) = '-'
           and then Buffer (Next_Char (P)) = '-'
         loop
            P             := Next_Line (Buffer, P);
            Start_Of_Line := P;
            End_Of_Line   := Line_End (Buffer, P);
            Line_Count    := Line_Count + 1;
            Padding       := 0;
            Indent_Done := False;
         end loop;

         exit when P = Buffer'Last or else Is_Word_Char (Buffer (P));

         case Buffer (P) is
            when '(' =>
               Prev_Token := Tok_Left_Paren;
               Char := Buffer (Prev_Char (P));

               if Indent_Done then
                  if Char /= ' ' and then Char /= '(' then
                     Spaces (2) := Buffer (P);
                     Replace_Text (New_Buffer, P, P + 1, Spaces (1 .. 2));
                     Padding :=
                       New_Buffer.Current.Line'Length - New_Buffer.Current.Len;
                  end if;

               else
                  --  Indent with 2 extra spaces if the '(' is the first
                  --  non blank character on the line

                  Do_Indent
                    (Buffer, New_Buffer, P, Indents,
                     Num_Spaces + Indent_Continue, Indent_Done);
                  Padding :=
                    New_Buffer.Current.Line'Length - New_Buffer.Current.Len;
               end if;

               Push (Indents, P - Start_Of_Line + Padding + 1);
               Num_Parens := Num_Parens + 1;

            when ')' =>
               Prev_Token := Tok_Right_Paren;
               Pop (Indents);
               Num_Parens := Num_Parens - 1;

            when '"' =>
               Prev_Token := Tok_String_Literal;
               P := Next_Char (P);

               while P <= End_Of_Line
                 and then Buffer (P) /= '"'
               loop
                  P := Next_Char (P);
               end loop;

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
                     if Buffer (P) = '+' then
                        Prev_Token := Tok_Minus;
                     else
                        Prev_Token := Tok_Plus;
                     end if;

                     if
                       To_Upper (Buffer (Prev_Char (P))) /= 'E'
                         or else Buffer (Prev_Char (Prev_Char (P)))
                           not in '0' .. '9'
                     then
                        Prev_Token    := Tok_Integer_Literal;
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

                     if Buffer (Next_Char (P)) = '=' then
                        Handle_Two_Chars ('=');

                        if Buffer (P) = '/' then
                           Prev_Token := Tok_Not_Equal;
                        else
                           Prev_Token := Tok_Colon_Equal;
                        end if;

                     elsif Buffer (P) = '/' then
                        Prev_Token := Tok_Slash;
                     else
                        Prev_Token := Tok_Colon;
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

                     if Buffer (Next_Char (P)) = '>' then
                        Prev_Token := Tok_Arrow;
                        Handle_Two_Chars ('>');
                     else
                        Prev_Token := Tok_Equal;
                     end if;

                  when others =>
                     null;
               end case;

               if Buffer (Prev_Char (P)) = ' ' then
                  First := First - 1;
               end if;

               if Spaces (3) = ' ' then
                  if Buffer (Next_Char (P)) = ' '
                    or else Last - 1 = End_Of_Line
                  then
                     Long := 2;
                  else
                     Long := 3;
                  end if;
               end if;

               if Insert_Spaces and then
                 (Buffer (Prev_Char (P)) /= ' '
                   or else Long /= Last - P + 1)
               then
                  Replace_Text
                    (New_Buffer, First, Last,
                     Spaces (Offs .. Offs + Long - 1));
               end if;

            when ',' | ';' =>
               if Buffer (P) = ';' then
                  Prev_Token := Tok_Semicolon;
                  Semicolon  := True;
               else
                  Prev_Token := Tok_Comma;
               end if;

               Char := Buffer (Next_Char (P));

               if Char /= ' ' and then P /= End_Of_Line then
                  Comma (1) := Buffer (P);
                  Replace_Text (New_Buffer, P, P + 1, Comma (1 .. 2));
               end if;

            when ''' =>
               --  Apostrophe. This can either be the start of a character
               --  literal, an isolated apostrophe used in a qualified
               --  expression or an attribute. We treat it as a character
               --  literal if it does not follow a right parenthesis,
               --  identifier, the keyword ALL or a literal. This means that we
               --  correctly treat constructs like:
               --    A := Character'('A');

               if Prev_Token = Tok_Identifier
                  or else Prev_Token = Tok_Right_Paren
                  or else Prev_Token = Tok_All
                  or else Prev_Token in Token_Class_Literal
               then
                  Prev_Token := Tok_Apostrophe;
               else
                  P := Next_Char (Next_Char (P));

                  while P <= End_Of_Line
                    and then Buffer (P) /= '''
                  loop
                     P := Next_Char (P);
                  end loop;

                  Prev_Token := Tok_Char_Literal;
               end if;

            when others =>
               null;
         end case;

         P := Next_Char (P);
      end loop;
   end Next_Word;

   ---------------
   -- Prev_Char --
   ---------------

   function Prev_Char (P : Word) return Word is
   begin
      return P - 1;
   end Prev_Char;

   ---------------
   -- Do_Indent --
   ---------------

   procedure Do_Indent
     (Buffer      : String;
      New_Buffer  : in out Extended_Line_Buffer;
      Prec        : Word;
      Indents     : Indent_Stack.Simple_Stack;
      Num_Spaces  : Integer;
      Indent_Done : in out Boolean)
   is
      Start       : Word;
      Indentation : Integer;
      Index       : Word;

   begin
      if not Indent_Done then
         Start := Line_Start (Buffer, Prec);
         Index := Start;

         while Buffer (Index) = ' ' or else Buffer (Index) = ASCII.HT loop
            Index := Index + 1;
         end loop;

         if Top (Indents) = None then
            Indentation := Num_Spaces;
         else
            Indentation := Top (Indents);
         end if;

         Replace_Text (New_Buffer, Start, Index, Spaces (1 .. Indentation));
         --  Current := End_Of_Word (Buffer, Prec);
         Indent_Done := True;
      end if;
   end Do_Indent;

   ----------------
   -- Format_Ada --
   ----------------

   procedure Format_Ada
     (Buffer          : String;
      Indent_Level    : Natural := 3;
      Indent_Continue : Natural := 2;
      Reserved_Casing : Casing_Type := Lower;
      Ident_Casing    : Casing_Type := Mixed)
   is
      New_Buffer          : Extended_Line_Buffer;
      Word_Count          : Integer           := 0;
      Line_Count          : Integer           := 0;
      Str                 : String (1 .. 1024);
      Str_Len             : Natural           := 0;
      Current             : Word;
      Prec, Prec_Last     : Word              := 1;
      Num_Spaces          : Integer           := 0;
      Indent_Done         : Boolean           := False;
      Num_Parens          : Integer           := 0;
      Prev_Num_Parens     : Integer           := 0;
      In_Generic          : Boolean           := False;
      Semicolon           : Boolean           := False;
      Type_Decl           : Boolean           := False;
      Was_Type_Decl       : Boolean           := False;
      End_Token           : Boolean           := False;
      Or_Token            : Boolean           := False;
      And_Token           : Boolean           := False;
      Subprogram_Decl     : Boolean           := False;
      Syntax_Error        : Boolean           := False;
      Started             : Boolean           := False;
      Prev_Reserved       : Token_Type        := Tok_Identifier;
      Token               : Token_Type;
      Prev_Token          : Token_Type := No_Token;
      Tokens              : Token_Stack.Simple_Stack;
      Indents             : Indent_Stack.Simple_Stack;
      Val                 : Extended_Token;
      Casing              : Casing_Type;

      procedure Handle_Reserved_Word (Word : Token_Type);
      --  Handle reserved words.

      --------------------------
      -- Handle_Reserved_Word --
      --------------------------

      procedure Handle_Reserved_Word (Word : Token_Type) is
         Temp : Extended_Token := (Word, False);
      begin
         --  Note: the order of the following conditions is important

         if Word = Tok_Body then
            Subprogram_Decl := False;
         elsif not End_Token and then Word = Tok_If then
            Push (Tokens, Temp);

         elsif Prev_Reserved = Tok_Is and then not Was_Type_Decl
           and then
             (Word = Tok_New or else Word = Tok_Abstract
               or else Word = Tok_Separate)
         then
            --  unindent since this is a declaration, e.g:
            --  package ... is new ...;
            --  function ... is abstract;
            --  function ... is separate;

            Num_Spaces := Num_Spaces - Indent_Level;

            if Num_Spaces < 0 then
               Num_Spaces := 0;
               Syntax_Error := True;
            end if;

            Pop (Tokens);

         elsif Word = Tok_Function
           or else Word = Tok_Procedure
           or else Word = Tok_Package
           or else Word = Tok_Task
           or else Word = Tok_Protected
           or else Word = Tok_Entry
         then
            Type_Decl     := False;
            Was_Type_Decl := False;

            if Word /= Tok_Package then
               Subprogram_Decl := True;
               Num_Parens      := 0;
            end if;

            if not In_Generic then
               Val := Top (Tokens);

               if not Val.Declaration and then
                 (Val.Token = Tok_Procedure or else Val.Token = Tok_Function)
               then
                  --  There was a function declaration, e.g:
                  --
                  --  procedure xxx ();
                  --  procedure ...
                  Pop (Tokens);
               end if;

               Push (Tokens, Temp);

            elsif Prev_Reserved /= Tok_With then
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

         elsif Word = Tok_End or else Word = Tok_Elsif then
            --  unindent after end of elsif, e.g:
            --
            --  if xxx then
            --     xxx
            --  elsif xxx then
            --     xxx
            --  end if;

            if Word = Tok_End then
               Pop (Tokens, Val);

               case Val.Token is
                  when Tok_Exception =>
                     --  Undo additional level of indentation, as in:
                     --     ...
                     --  exception
                     --     when =>
                     --        null;
                     --  end;

                     Num_Spaces := Num_Spaces - Indent_Level;

                     --  End of subprogram
                     Pop (Tokens);

                  when Tok_Case =>
                     Num_Spaces := Num_Spaces - Indent_Level;

                  when others =>
                     null;
               end case;
            end if;

            Num_Spaces := Num_Spaces - Indent_Level;

            if Num_Spaces < 0 then
               Num_Spaces   := 0;
               Syntax_Error := True;
            end if;

         elsif     Word = Tok_Is
           or else Word = Tok_Declare
           or else Word = Tok_Begin
           or else Word = Tok_Do
           or else (not Or_Token  and then Word = Tok_Else)
           or else (not And_Token and then Word = Tok_Then)
           or else (not End_Token and then Word = Tok_Select)
           or else (Top (Tokens).Token = Tok_Select and then Word = Tok_Or)
           or else (not End_Token and then Word = Tok_Loop)
           or else (not End_Token and then Prev_Reserved /= Tok_Null
                      and then Word = Tok_Record)
           or else ((Top (Tokens).Token = Tok_Exception
                       or else Top (Tokens).Token = Tok_Case)
                     and then Word = Tok_When)
           or else (Top (Tokens).Declaration
                      and then Word = Tok_Private
                      and then Prev_Reserved /= Tok_Is
                      and then Prev_Reserved /= Tok_With)
         then
            --  unindent for this reserved word, and then indent again, e.g:
            --
            --  procedure xxx is
            --     ...
            --  begin    <--
            --     ...

            if not Type_Decl then
               if Word = Tok_Select then
                  --  Start of a select statement
                  Push (Tokens, Temp);
               end if;

               if Word = Tok_Else
                 or else (Top (Tokens).Token = Tok_Select
                          and then Word = Tok_Then)
                 or else Word = Tok_Begin
                 or else Word = Tok_Record
                 or else Word = Tok_When
                 or else Word = Tok_Or
                 or else Word = Tok_Private
               then
                  if Word = Tok_Begin then
                     Val := Top (Tokens);

                     if Val.Declaration then
                        Num_Spaces := Num_Spaces - Indent_Level;
                        Pop (Tokens);
                        Val.Declaration := False;
                        Push (Tokens, Val);
                     else
                        Push (Tokens, Temp);
                     end if;

                  elsif Word = Tok_Record then
                     Push (Tokens, Temp);
                  else
                     Num_Spaces := Num_Spaces - Indent_Level;
                  end if;

                  if Num_Spaces < 0 then
                     Num_Spaces   := 0;
                     Syntax_Error := True;
                  end if;
               end if;

               Do_Indent
                 (Buffer, New_Buffer, Prec, Indents, Num_Spaces, Indent_Done);
               Num_Spaces := Num_Spaces + Indent_Level;
            end if;

            if Word = Tok_Do
              or else Word = Tok_Loop
            then
               Push (Tokens, Temp);
            elsif Word = Tok_Declare then
               Temp.Declaration := True;
               Push (Tokens, Temp);
            end if;

            if Word = Tok_Is then
               if Prev_Reserved = Tok_Case then
                  Temp.Token := Tok_Case;
                  Push (Tokens, Temp);
                  Num_Spaces := Num_Spaces + Indent_Level;

               elsif Type_Decl then
                  Was_Type_Decl := True;
                  Type_Decl     := False;

               else
                  Pop (Tokens, Val);
                  Val.Declaration := True;
                  Push (Tokens, Val);
                  Subprogram_Decl := False;
               end if;
            end if;

         elsif Word = Tok_Generic then
            --  Indent before a generic entity, e.g:
            --
            --  generic
            --     type ...;

            Do_Indent
              (Buffer, New_Buffer, Prec, Indents, Num_Spaces, Indent_Done);
            Num_Spaces := Num_Spaces + Indent_Level;
            In_Generic := True;

         elsif (Word = Tok_Type
                and then Prev_Reserved /= Tok_Task
                and then Prev_Reserved /= Tok_Protected)
           or else Word = Tok_Subtype
         then
            --  Entering a type declaration/definition.
            --  ??? Should use the stack instead

            Type_Decl := True;

         elsif Word = Tok_Exception then
            Val := Top (Tokens);

            if not Val.Declaration then
               Num_Spaces := Num_Spaces - Indent_Level;
               Do_Indent
                 (Buffer, New_Buffer, Prec, Indents, Num_Spaces, Indent_Done);
               Num_Spaces := Num_Spaces + 2 * Indent_Level;
               Push (Tokens, Temp);
            end if;
         end if;

         Prev_Reserved := Word;
         End_Token     := Word = Tok_End;
         Or_Token      := Word = Tok_Or;
         And_Token     := Word = Tok_And;
      end Handle_Reserved_Word;

   begin  --  Format_Ada
      New_Buffer := To_Line_Buffer (Buffer);

      --  Push a dummy token so that stack will never be empty.
      Push (Tokens, (No_Token, False));

      --  Push a dummy indentation so that stack will never be empty.
      Push (Indents, None);

      Next_Word
        (Buffer, New_Buffer, Prec, Num_Parens, Semicolon,
         Line_Count, Indents, Indent_Continue, Num_Spaces,
         Indent_Done, Prev_Token);
      Current := End_Of_Word (Buffer, Prec);

      while Current < Buffer'Last loop
         Str_Len := Current - Prec + 1;

         if Str_Len > 1 or else Buffer (Prec - 1) /= ''' then
            for J in Prec .. Current loop
               Str (J - Prec + 1) := To_Lower (Buffer (J));
            end loop;

            Token := Get_Token (Str (1 .. Str_Len));

            if End_Token and then
              (Token = Tok_Identifier or else Semicolon)
            then
               End_Token := False;
            elsif Subprogram_Decl then
               if Num_Parens = 0 then
                  if Semicolon or else Prev_Num_Parens > 0 then
                     Subprogram_Decl := False;

                     if Semicolon and then not In_Generic then
                        --  subprogram decl with no following reserved word,
                        --  e.g:
                        --  procedure ... ();

                        Pop (Tokens);
                     end if;
                  end if;
               end if;
            end if;

            if Token = Tok_Identifier
              or else
                ((Token = Tok_Delta or else Token = Tok_Digits
                   or else Token = Tok_Range or else Token = Tok_Access)
                 and then Prev_Token = Tok_Apostrophe)
            then
               Casing    := Ident_Casing;
               Or_Token  := False;
               And_Token := False;

            else
               Casing := Reserved_Casing;
               Handle_Reserved_Word (Token);
            end if;

            case Casing is
               when Unchanged =>
                  null;
               when Upper =>
                  for J in 1 .. Str_Len loop
                     Str (J) := To_Upper (Str (J));
                  end loop;

                  Replace_Text
                    (New_Buffer, Prec, Current + 1, Str (1 .. Str_Len));

               when Lower =>
                  Replace_Text
                    (New_Buffer, Prec, Current + 1, Str (1 .. Str_Len));

               when Mixed =>
                  Mixed_Case (Str (1 .. Str_Len));
                  Replace_Text
                    (New_Buffer, Prec, Current + 1, Str (1 .. Str_Len));
            end case;
         end if;

         if Started then
            Do_Indent
              (Buffer, New_Buffer, Prec, Indents, Num_Spaces, Indent_Done);
         else
            Started := True;
         end if;

         Prec_Last       := Prec;
         Prev_Num_Parens := Num_Parens;
         Semicolon       := False;
         Prec            := Current + 1;
         Prev_Token      := Token;
         Next_Word
           (Buffer, New_Buffer, Prec, Num_Parens, Semicolon,
            Line_Count, Indents, Indent_Continue, Num_Spaces,
            Indent_Done, Prev_Token);

         Syntax_Error :=
           Syntax_Error or else (Prec = Buffer'Last and then Num_Spaces > 0);

         if Syntax_Error then
            Put_Line
              (">>> Syntax Error at line" & Line_Count'Img &
               ", around character" & Current'Img);
         end if;

         Current    := End_Of_Word (Buffer, Prec);
         Word_Count := Word_Count + 1;

         --  A new line, reset flags.

         if Line_Start (Buffer, Prec) /= Line_Start (Buffer, Prec_Last) then
            null;
            --  Indent_Done := False;
            --  ??? Handle events, update progress bar, ...
         end if;
      end loop;

      Print (New_Buffer);
      Free (New_Buffer);
   end Format_Ada;

   --------------------
   -- To_Line_Buffer --
   --------------------

   function To_Line_Buffer (Buffer : String) return Extended_Line_Buffer is
      B     : Extended_Line_Buffer;
      Index : Natural := Buffer'First;
      First : Natural;
      Tmp   : Line_Buffer;
      Prev  : Line_Buffer;
      pragma Warnings (Off, Prev);
      --  GNAT will issue a "warning: "Prev" may be null" which cannot occur
      --  since Prev is set to Tmp at the end of each iteration.

   begin
      loop
         exit when Index >= Buffer'Last;

         First := Index;
         Skip_To_Char (Buffer, Index, ASCII.LF);
         Tmp := new Line_Buffer_Record;

         if First = Buffer'First then
            B.First   := Tmp;
            B.Current := B.First;

         else
            Prev.Next := Tmp;
         end if;

         if Index < Buffer'Last and then Buffer (Index + 1) = ASCII.CR then
            Index := Index + 1;
         end if;

         Tmp.Line := new String' (Buffer (First .. Index));
         Tmp.Len  := Tmp.Line'Length;

         Index := Index + 1;
         Prev := Tmp;
      end loop;

      return B;
   end To_Line_Buffer;

   -----------
   -- Print --
   -----------

   procedure Print (Buffer : Extended_Line_Buffer) is
      Tmp  : Line_Buffer := Buffer.First;
   begin
      loop
         exit when Tmp = null;
         Put (Tmp.Line.all);
         Tmp := Tmp.Next;
      end loop;
   end Print;

   ----------
   -- Free --
   ----------

   procedure Free (Buffer : in out Extended_Line_Buffer) is
      Tmp  : Line_Buffer := Buffer.First;
      Prev : Line_Buffer;

   begin
      loop
         exit when Tmp = null;
         Prev := Tmp;
         Tmp := Tmp.Next;
         Free (Prev.Line);
         Free (Prev);
      end loop;
   end Free;

   ------------------
   -- Replace_Text --
   ------------------

   procedure Replace_Text
     (Buffer  : in out Extended_Line_Buffer;
      First   : Word;
      Last    : Word;
      Replace : String)
   is
      S          : String_Access;
      F, L       : Word;
      Line_First : Natural;
      Line_Last  : Natural;
      Padding    : Integer;

   begin
      if Buffer.Current.Line'First + Buffer.Current.Len - 1 < First then
         loop
            Buffer.Current := Buffer.Current.Next;

            exit when Buffer.Current.Line'First + Buffer.Current.Len > First;
         end loop;
      end if;

      Padding := Buffer.Current.Line'Length - Buffer.Current.Len;
      F       := First + Padding;
      L       := Last  + Padding;

      if Last - First = Replace'Length then
         --  Simple case, no need to reallocate buffer

         Buffer.Current.Line (F .. L - 1) := Replace;

      else
         Line_First := Buffer.Current.Line'First;
         Line_Last  := Buffer.Current.Line'Last;

         S := new String
           (Line_First .. Line_Last - ((Last - First) - Replace'Length));
         S (Line_First .. F - 1) := Buffer.Current.Line (Line_First .. F - 1);
         S (F .. F + Replace'Length - 1) := Replace;
         S (F + Replace'Length .. S'Last) :=
           Buffer.Current.Line (L .. Buffer.Current.Line'Last);

         Free (Buffer.Current.Line);
         Buffer.Current.Line := S;
      end if;
   end Replace_Text;

end Source_Analyzer;
