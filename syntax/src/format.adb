with String_Utils;            use String_Utils;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

package body Format is

   type Token_Type is
     (Tok_Unknown,
      Tok_Abort,
      Tok_Abs,
      Tok_Abstract,
      Tok_Accept,
      Tok_Access,
      Tok_Aliased,
      Tok_All,
      Tok_And,
      Tok_Array,
      Tok_At,
      Tok_Begin,
      Tok_Body,
      Tok_Case,
      Tok_Constant,
      Tok_Declare,
      Tok_Delay,
      Tok_Delta,
      Tok_Digits,
      Tok_Do,
      Tok_Else,
      Tok_Elsif,
      Tok_End,
      Tok_Entry,
      Tok_Exception,
      Tok_Exit,
      Tok_For,
      Tok_Function,
      Tok_Generic,
      Tok_Goto,
      Tok_If,
      Tok_In,
      Tok_Is,
      Tok_Limited,
      Tok_Loop,
      Tok_Mod,
      Tok_New,
      Tok_Not,
      Tok_Null,
      Tok_Of,
      Tok_Or,
      Tok_Others,
      Tok_Out,
      Tok_Package,
      Tok_Pragma,
      Tok_Private,
      Tok_Procedure,
      Tok_Protected,
      Tok_Raise,
      Tok_Range,
      Tok_Record,
      Tok_Rem,
      Tok_Renames,
      Tok_Requeue,
      Tok_Return,
      Tok_Reverse,
      Tok_Select,
      Tok_Separate,
      Tok_Subtype,
      Tok_Tagged,
      Tok_Task,
      Tok_Terminate,
      Tok_Then,
      Tok_Type,
      Tok_Until,
      Tok_Use,
      Tok_When,
      Tok_While,
      Tok_With,
      Tok_Xor);

   subtype Word is Natural;

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
      Padding : Integer := 0;
   end record;

   function To_Line_Buffer (Buffer : String) return Extended_Line_Buffer;
   --  Convert a string to a line buffer.
   --  CR/LF and LF are treated as end of lines.

   procedure Print (Buffer : Extended_Line_Buffer);
   --  Output the contents of Buffer on standard output.

   procedure Free (Buffer : in out Extended_Line_Buffer);
   --  Free the contents of buffer.

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
      In_Comments     : in out Boolean;
      Num_Parens      : in out Integer;
      String_Mismatch : in out Boolean;
      Semicolon       : in out Boolean;
      Line_Count      : in out Word);
   --  Starting at Buffer (P), find the location of the next word
   --  and set P accordingly.
   --  Formatting of operators is performed by this procedure.

   function Prev_Char (P : Word) return Word;
   --  Return the previous char in buffer. P is the current character.
   pragma Inline (Prev_Char);

   function Prev_Non_Blank (Buffer : String; P : Word) return Character;
   --  Return the first non blank character before Buffer (P).

   procedure Replace_Text
     (Buffer  : in out Extended_Line_Buffer;
      First   : Word;
      Last    : Word;
      Replace : String);
   --  Replace the slice First .. Last - 1 in Buffer by Replace.

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
      while Tmp < Buffer'Last and then Is_Word_Char (Buffer (Next_Char (Tmp)))
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
         return Tok_Unknown;
      end if;

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
                  return Tok_Unknown;
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
            return Tok_Unknown;
      end case;

      return Tok_Unknown;
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
      In_Comments     : in out Boolean;
      Num_Parens      : in out Integer;
      String_Mismatch : in out Boolean;
      Semicolon       : in out Boolean;
      Line_Count      : in out Word)
   is
      Comma         : String := ", ";
      Spaces        : String := "    ";
      End_Of_Line   : Word;
      Long          : Word;
      First         : Word;
      Last          : Word;
      Offs          : Word;
      Insert_Spaces : Boolean;
      Char          : Character;
      PChar         : Character;
  
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
      if In_Comments then
         P := Next_Line (Buffer, P);
         Line_Count := Line_Count + 1;
      end if;

      End_Of_Line     := Line_End (Buffer, P);
      In_Comments     := False;
      String_Mismatch := False;

      loop
         if P > End_Of_Line then
            End_Of_Line := Line_End (Buffer, P);
            Line_Count  := Line_Count + 1;
         end if;

         --  Skip comments

         while Buffer (P) = '-'
           and then Buffer (Next_Char (P)) = '-'
         loop
            P           := Next_Line (Buffer, P);
            End_Of_Line := Line_End (Buffer, P);
            Line_Count  := Line_Count + 1;
         end loop;

         exit when P = Buffer'Last or else Is_Word_Char (Buffer (P));

         case Buffer (P) is
            when '(' =>
               Num_Parens := Num_Parens + 1;
               Char := Buffer (Prev_Char (P));

               if Char /= ' ' and then Char /= ''' then
                  Spaces (2) := Buffer (P);
                  Replace_Text (New_Buffer, P, P + 1, Spaces (1 .. 2));
               end if;

            when ')' =>
               Num_Parens := Num_Parens - 1;

            when '"' =>
               if Buffer (Prev_Char (P)) /= '''
                 or else Buffer (Next_Char (P)) /= '''
               then
                  P := Next_Char (P);

                  while P < Buffer'Last
                    and then Buffer (P) /= '"'
                    and then P <= End_Of_Line
                  loop
                     P := Next_Char (P);
                  end loop;

                  if Buffer (P) /= '"' then
                     String_Mismatch := True;
                  end if;
               end if;

            when '&' | '+' | '-' | '*' | '/' | ':' | '<' | '>' | '=' |
                 '|' | '.'
            =>
               if Buffer (Prev_Char (P)) /= '''
                 or else Buffer (Next_Char (P)) /= '''
               then
                  Spaces (2) := Buffer (P);
                  Spaces (3) := ' ';
                  First := P;
                  Last  := P + 1;
                  Offs  := 1;

                  case Buffer (P) is
                     when '+' | '-' =>
                        Insert_Spaces :=
                          To_Upper (Buffer (Prev_Char (P))) /= 'E'
                            or else Buffer (Prev_Char (Prev_Char (P)))
                              not in '0' .. '9';

                     when '&' | '|' =>
                        Insert_Spaces := True;

                     when '/' | ':' =>
                        Insert_Spaces := True;

                        if Buffer (Next_Char (P)) = '=' then
                           Handle_Two_Chars ('=');
                        end if;

                     when '*' =>
                        Insert_Spaces := Buffer (Prev_Char (P)) /= '*';

                        if Buffer (Next_Char (P)) = '*' then
                           Handle_Two_Chars ('*');
                        end if;

                     when '.' =>
                        Insert_Spaces := Buffer (Next_Char (P)) = '.';

                        if Insert_Spaces then
                           Handle_Two_Chars ('.');
                        end if;

                     when '<' =>
                        Char := Buffer (Next_Char (P));
                        Insert_Spaces := Char /= '=' and then Char /= '<'
                          and then Char /= '>'
                          and then Buffer (Prev_Char (P)) /= '<';

                     when '>' =>
                        Char := Buffer (Next_Char (P));
                        PChar := Buffer (Prev_Char (P));
                        Insert_Spaces :=
                          Char /= '=' and then Char /= '>'
                            and then PChar /= '='
                            and then PChar /= '<'
                            and then PChar /= '>';

                     when '=' =>
                        Char := Buffer (Prev_Char (P));
                        Insert_Spaces :=
                          Char /= '/' and then Char /= ':'
                          and then Char /= '>' and then Char /= '<';

                        if Buffer (Next_Char (P)) = '>' then
                           Handle_Two_Chars ('>');
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
               end if;

            when ',' | ';' =>
               if Buffer (P) = ';' then
                  Semicolon := True;
               end if;

               Char := Buffer (Next_Char (P));

               if Char /= ' ' and then Char /= '''
                 and then P /= End_Of_Line
               then
                  Comma (1) := Buffer (P);
                  Replace_Text (New_Buffer, P, P + 1, Comma (1 .. 2));
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

   --------------------
   -- Prev_Non_Blank --
   --------------------

   function Prev_Non_Blank (Buffer : String; P : Word) return Character is
   begin
      for J in reverse Buffer'First .. P - 1 loop
         if Buffer (J) /= ' ' and then Buffer (J) /= ASCII.HT then
            return Buffer (J);
         end if;
      end loop;

      return Buffer (Buffer'First);
   end Prev_Non_Blank;

   ----------------
   -- Format_Ada --
   ----------------

   procedure Format_Ada (Buffer : String) is
      None         : constant := -1;
      Indent_Level : constant := 3;

      type Token_Stack_Record;
      type Token_Stack is access Token_Stack_Record;
      type Token_Stack_Record is record
         Val  : Token_Type;
         Next : Token_Stack;
      end record;

      procedure Free is new
        Ada.Unchecked_Deallocation (Token_Stack_Record, Token_Stack);

      New_Buffer          : Extended_Line_Buffer;
      Index               : Word;
      Word_Count          : Integer           := 0;
      Line_Count          : Integer           := 0;
      Str                 : String (1 .. 1024);
      Str_Len             : Natural           := 0;
      Current             : Word;
      Prec, Prec_Last     : Word              := 1;
      Num_Spaces          : Integer           := 0;
      Param_Indent        : Integer           := None;
      Spaces              : String (1 .. 150) := (others => ' ');
      In_Comments         : Boolean           := False;
      Indent_Done         : Boolean           := False;
      Num_Parens          : Integer           := 0;
      Prev_Num_Parens     : Integer           := 0;
      In_Generic          : Boolean           := False;
      Semicolon           : Boolean           := False;
      Type_Decl           : Boolean           := False;
      Was_Type_Decl       : Boolean           := False;
      Select_Token        : Natural           := 0;
      End_Token           : Boolean           := False;
      Or_Token            : Boolean           := False;
      And_Token           : Boolean           := False;
      Subprogram_Decl     : Boolean           := False;
      In_String           : Boolean           := False;
      String_Mismatch     : Boolean           := False;
      Syntax_Error        : Boolean           := False;
      Started             : Boolean           := False;
      Prev_Reserved       : Token_Type        := Tok_Unknown;
      Token               : Token_Type;
      Stack               : Token_Stack;
      Val                 : Token_Type;

      procedure Do_Indent;
      --  Perform indentation by inserting spaces in the buffer.

      procedure Handle_Reserved_Word (Word : Token_Type);
      --  Handle reserved words.

      procedure Push (Token : Token_Type);
      --  If Token = Tok_Declare, it means that we are entering
      --  a declaration part.
      --  If Token = Tok_Unknown, this is the beginning of a subprogram/
      --  begin block.
      --  Otherwise, this is the beginning of a constructs (e.g select,
      --  case, record, ...)
      pragma Inline (Push);

      function Pop return Token_Type;
      procedure Pop;
      pragma Inline (Pop);

      function Top return Token_Type;
      --  Return the top of the stack without modifying it.
      pragma Inline (Top);

      ----------
      -- Push --
      ----------

      procedure Push (Token : Token_Type) is
      begin
         Stack := new Token_Stack_Record' (Token, Stack);
         pragma Debug (Put_Line ("pushed " & Token'Img));
      end;

      ---------
      -- Pop --
      ---------

      function Pop return Token_Type is
         P   : Token_Stack;
         Val : Token_Type;

      begin
         if Stack = null or else Stack.Val = Tok_Unknown then
            Syntax_Error := True;
            return Tok_Unknown;
         else
            Val   := Stack.Val;
            P     := Stack;
            Stack := Stack.Next;
            Free (P);
            pragma Debug (Put_Line ("popped " & Val'Img));
            return Val;
         end if;
      end;

      procedure Pop is
         Val : Token_Type;
      begin
         Val := Pop;
      end Pop;

      ---------
      -- Top --
      ---------

      function Top return Token_Type is
      begin
         if Stack = null then
            Syntax_Error := True;
            return Tok_Unknown;
         else
            return Stack.Val;
         end if;
      end;

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent is
         Start       : Word;
         Indentation : Integer;

      begin   
         if not Indent_Done then
            Start := Line_Start (Buffer, Prec);
            Index := Start;

            while Buffer (Index) = ' ' or else Buffer (Index) = ASCII.HT loop
               Index := Index + 1;
            end loop;

            if Param_Indent = None then
               Indentation := Num_Spaces;
            else
               Indentation := Param_Indent;
            end if;

            Replace_Text (New_Buffer, Start, Index, Spaces (1 .. Indentation));
            Current := End_Of_Word (Buffer, Prec);
            Indent_Done := True;
         end if;
      end Do_Indent;

      procedure Handle_Reserved_Word (Word : Token_Type) is
      begin
         --  Note: the order of the following conditions is important

         if Word = Tok_Body then
            Subprogram_Decl := False;
         elsif not End_Token and then Word = Tok_If then
            Push (Word);

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

            Pop;

         elsif Word = Tok_Function
           or else Word = Tok_Procedure
           or else Word = Tok_Package
           or else Word = Tok_Task
           or else Word = Tok_Protected
         then
            Type_Decl     := False;
            Was_Type_Decl := False;

            if Word /= Tok_Package then
               Subprogram_Decl := True;
               Num_Parens      := 0;
            end if;

            if not In_Generic then
               Val := Top;

               if Val = Tok_Procedure or else Val = Tok_Function then
                  --  There was a function declaration, e.g:
                  --
                  --  procedure xxx ();
                  --  procedure ...
                  Pop;
               end if;

               Push (Word);

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
               Push (Word);
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
               Val := Pop;

               case Val is
                  when Tok_Select =>
                     Select_Token := Select_Token - 1;
                  when Tok_Exception | Tok_Case =>
                     --  Additional level of indentation, as in:
                     --     ...
                     --  exception
                     --     when =>
                     --        null;
                     --  end;

                     Num_Spaces := Num_Spaces - Indent_Level;

                  when others =>
                     null;
               end case;

               pragma Debug (Put_Line ("end " & Val'IMG));
            end if;

            Num_Spaces := Num_Spaces - Indent_Level;

            if Num_Spaces < 0 then
               Num_Spaces   := 0;
               Syntax_Error := True;
            end if;

         elsif     Word = Tok_Is
           or else Word = Tok_Declare
           or else Word = Tok_Begin
           or else Word = Tok_When
           or else Word = Tok_Do
           or else (not Or_Token  and then Word = Tok_Else)
           or else (not And_Token and then Word = Tok_Then)
           or else (not End_Token and then Word = Tok_Select)
           or else (Top = Tok_Select and then Word = Tok_Or)
           or else (not End_Token and then Word = Tok_Loop)
           or else (not End_Token and then Prev_Reserved /= Tok_Null
                      and then Word = Tok_Record)
           or else ((Top = Tok_Declare or else Top = Tok_Package)
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
                  Push (Word);
                  Select_Token := Select_Token + 1;
               end if;

               if Word = Tok_Else
                 or else (Top = Tok_Select and then Word = Tok_Then)
                 or else Word = Tok_Begin
                 or else Word = Tok_Record
                 or else Word = Tok_When
                 or else Word = Tok_Or
                 or else Word = Tok_Private
               then
                  if Word = Tok_Begin then
                     Val := Top;

                     if Val = Tok_Declare then
                        Num_Spaces := Num_Spaces - Indent_Level;
                        Pop;
                        Push (Word);

                     elsif Val = Tok_Package then
                        Num_Spaces := Num_Spaces - Indent_Level;
                     else
                        Push (Word);
                     end if;

                  elsif Word = Tok_Record then
                     Push (Tok_Record);
                  else
                     Num_Spaces := Num_Spaces - Indent_Level;
                  end if;

                  if Num_Spaces < 0 then
                     Num_Spaces   := 0;
                     Syntax_Error := True;
                  end if;
               end if;

               Do_Indent;
               Num_Spaces := Num_Spaces + Indent_Level;
            end if;

            if Word = Tok_Do
              or else Word = Tok_Loop
              or else Word = Tok_Declare
            then
               Push (Word);
            end if;

            if Word = Tok_Is then
               if Prev_Reserved = Tok_Case then
                  Push (Tok_Case);
                  Num_Spaces := Num_Spaces + Indent_Level;

               elsif Type_Decl then
                  Was_Type_Decl := True;
                  Type_Decl     := False;

               else
                  Val := Top;

                  --  Should have a more complete case statement here ???

                  if Val /= Tok_Package then
                     Pop;
                     Push (Tok_Declare);
                  end if;

                  Subprogram_Decl := False;
               end if;
            end if;

         elsif Word = Tok_Generic then
            --  Indent before a generic entity, e.g:
            --
            --  generic
            --     type ...;

            Do_Indent;
            Num_Spaces := Num_Spaces + Indent_Level;
            In_Generic := True;

         elsif (Word = Tok_Type
                and then Prev_Reserved /= Tok_Task
                and then Prev_Reserved /= Tok_Protected)
           or else Word = Tok_Subtype
         then
            --  Entering a type declaration/definition.

            Type_Decl := True;

         elsif Word = Tok_Exception then
            Val := Top;

            if Val /= Tok_Declare then
               Num_Spaces := Num_Spaces - Indent_Level;
               Do_Indent;
               Num_Spaces := Num_Spaces + 2 * Indent_Level;
               Pop;
               Push (Tok_Exception);
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
      Push (Tok_Unknown);

      while Prec < Buffer'Last and then not (Is_Word_Char (Buffer (Prec))) loop
         if Buffer (Prec) = '"' then
            In_String := not In_String;
         elsif Buffer (Prec) = '-'
           and then Buffer (Next_Char (Prec)) = '-'
         then
            In_Comments := True;
            Next_Word
              (Buffer, New_Buffer, Prec,
               In_Comments, Num_Parens, String_Mismatch,
               Semicolon, Line_Count);

         else
            Prec := Next_Char (Prec);
         end if;
      end loop;

      Current := End_Of_Word (Buffer, Prec);

      while Current < Buffer'Last loop
         Str_Len := Current - Prec + 1;

         if not (Str_Len = 1 and then Buffer (Prec - 1) = ''') then
            for J in Prec .. Current loop
               Str (J - Prec + 1) := To_Lower (Buffer (J));
            end loop;

            Token := Get_Token (Str (1 .. Str_Len));

            if End_Token and then
              (Token = Tok_Unknown or else Semicolon)
            then
               End_Token := False;
            elsif Subprogram_Decl then
               if Num_Parens = 1 and then Prev_Num_Parens = 0 then
                  Param_Indent := Prec - Line_Start (Buffer, Prec);
               elsif Num_Parens = 0 then
                  if Semicolon or else Prev_Num_Parens = 1 then
                     Subprogram_Decl := False;
                     Param_Indent    := None;

                     if Semicolon and then not In_Generic then
                        --  subprogram decl with no following reserved word,
                        --  e.g:
                        --  procedure ... ();

                        Pop;
                     end if;
                  end if;
               end if;
            end if;

            if Token = Tok_Unknown
              or else
                ((Token = Tok_Delta or else Token = Tok_Digits
                   or else Token = Tok_Range or else Token = Tok_Access)
                 and then Prev_Non_Blank (Buffer, Prec) = ''')
            then
               --  In this context, an unknown token can only be an identifier
               --  since operators have already been handled.

               Mixed_Case (Str (1 .. Str_Len));
               Or_Token  := False;
               And_Token := False;

            else
               Handle_Reserved_Word (Token);
            end if;

            Replace_Text (New_Buffer, Prec, Current + 1, Str (1 .. Str_Len));
         end if;

         if Started then
            Do_Indent;
         else
            Started := True;
         end if;

         Prec_Last       := Prec;
         Prev_Num_Parens := Num_Parens;
         Semicolon       := False;
         Prec            := Current + 1;
         Next_Word
           (Buffer, New_Buffer, Prec, In_Comments,
            Num_Parens, String_Mismatch, Semicolon, Line_Count);

         Syntax_Error :=
           Syntax_Error or else (Prec = Buffer'Last and then Num_Spaces > 0);

         if String_Mismatch then
            Put_Line
              (">>> String Mismatch at line" & Line_Count'Img &
               ", around character" & Current'Img);
         end if;

         if Syntax_Error then
            Put_Line
              (">>> Syntax Error at line" & Line_Count'Img &
               ", around character" & Current'Img);
         end if;

         Current    := End_Of_Word (Buffer, Prec);
         Word_Count := Word_Count + 1;

         --  A new line, reset flags.

         if Line_Start (Buffer, Prec) /= Line_Start (Buffer, Prec_Last) then
            Indent_Done := False;
            In_String   := False;

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

   begin
      if Buffer.Current.Line'First + Buffer.Current.Len - 1 < First then
         loop
            Buffer.Current := Buffer.Current.Next;

            exit when Buffer.Current.Line'First + Buffer.Current.Len > First;
         end loop;

         Buffer.Padding := 0;
      end if;

      F := First + Buffer.Padding;
      L := Last  + Buffer.Padding;

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
         Buffer.Padding := Buffer.Padding + Replace'Length - (Last - First);
      end if;
   end Replace_Text;

end Format;
