------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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
with Ada.Finalization;        use Ada.Finalization;
with Ada.Streams;             use Ada.Streams;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

package body MI.Lexer is

   -------------------------------------------------------
   -- Token_Type comparison function "=" implementation --
   -------------------------------------------------------

   function "=" (Left, Right : Token_Type) return Boolean is
   begin
      if Left.Code /= Right.Code or else Left.Line /= Right.Line or else
        Left.Column /= Right.Column then
         return False;
      end if;

      case Left.Code is
         when Token_No =>
            return Left.Value = Right.Value;
         when Identifier | C_String =>
            return Left.Text = Right.Text;
         when others =>
            return True;
      end case;
   end "=";

   -----------------
   -- Clear_Token --
   -----------------

   procedure Clear_Token (Token : in out Token_Type)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (String, String_Access);
   begin
      case Token.Code is
         when Identifier | C_String =>
            if Token.Text /= null then
               Unchecked_Free (Token.Text);
               Token.Text := null;
            end if;

         when others =>
            null;  --  Nothing to do here.
      end case;
   end Clear_Token;

   ---------------------
   -- Clear_Token_List --
   ---------------------

   procedure Clear_Token_List (Tokens : in out Token_List)
   is
      Cursor : Token_Lists.Cursor := Token_Lists.First (Tokens);
      Token  : Token_Type;
   begin
      while Token_Lists.Has_Element (Cursor) loop
         Token := Token_Lists.Element (Cursor);
         Clear_Token (Token);
         Cursor := Token_Lists.Next (Cursor);
      end loop;
   end Clear_Token_List;

   ------------------------------------------
   -- Function Build_Tokens implementation --
   ------------------------------------------

   function Build_Tokens (Input : Stream_Access) return Token_List is

      ---------------------------------
      -- Package Handler declaration --
      ---------------------------------

      package Handler is
         --  This package is meant to simplify operations on the input stream
         --  by providing a Stream_Handler structure and its associated
         --  methods:
         --    * Look_Ahead return Character
         --    * Eat
         --    * Read_Number return Integer
         --    * Read_Identifier return String_Access
         --    * Read_C_String return String_Access
         --  This package is defined in the declarative part of the
         --  Lexer.Get_Tokens function to reduce at a maximum its scope:
         --  Since Get_Tokens is the only function which use this package, we
         --  don't want to declare it elsewhere.

         type Buffer_Offset is new Natural range 1 .. 1024;
         --  Buffer index type ranged (1 .. 1024).
         --  Index is used as the range type of the Buffer_Type type declare
         --  below.

         type Buffer_Type is array (Buffer_Offset) of Character;
         --  Buffer type definition.
         --  It's a buffer of character using the Index type as range value
         --  type.

         type Stream_Handler is new Controlled with record
            Buffer : Buffer_Type   := (others => ASCII.NUL);
            Offset : Buffer_Offset := Buffer_Offset'First;
            Line   : Natural       := 1;
            Column : Natural       := 1;
         end record;
         --  Tagged type stream handler.
         --  It's composed of a buffer containing the recently flushed data
         --  from the string, and an index indicated the current read cusror
         --  in the buffer.

         overriding
         procedure Initialize (Sh : in out Stream_Handler);
         --  Ctor of the Stream_Handler object.

         function Look_Ahead (Sh : Stream_Handler) return Character;
         --  Return the next token in the stream, without consuming it.

         procedure Eat (Sh : in out Stream_Handler);
         --  Consume the current token in the stream.

         procedure Read_Number
           (Sh     : in out Handler.Stream_Handler;
            Number : in out Integer);
         --  Read the longest number from the stream and return it.

         procedure Read_Identifier
           (Sh         : in out Handler.Stream_Handler;
            Identifier : in out String_Access);
         --  Read the longest identifier from the stream and return it.

         procedure Read_C_String
           (Sh       : in out Handler.Stream_Handler;
            C_String : in out String_Access);
         --  Read the longest C string from the stream and return it.

      private
         procedure Flush (Sh : in out Stream_Handler);
         --  Update the handler by flushing current buffer and rebuffering
         --  new data.

         function Is_Valid_Identifier_Character (C : Character) return Boolean;
         pragma Inline (Is_Valid_Identifier_Character);
         --  Return whether or not the given character is eligible to be part
         --  of an identifier.

      end Handler;

      -----------------------------------------
      -- Package Handler body implementation --
      -----------------------------------------

      package body Handler is

         ----------------
         -- Initialize --
         ----------------

         procedure Initialize (Sh : in out Stream_Handler) is
         begin
            Sh.Flush;
         end Initialize;

         ----------------
         -- Look_Ahead --
         ----------------

         function Look_Ahead (Sh : Stream_Handler) return Character is
         begin
            return Sh.Buffer (Sh.Offset);
         end Look_Ahead;

         ---------
         -- Eat --
         ---------

         procedure Eat (Sh : in out Stream_Handler) is
         begin
            if Sh.Offset = Buffer_Offset'Last then
               Sh.Flush;
            else
               Sh.Offset := Sh.Offset + 1;
            end if;

            Sh.Column := Sh.Column + 1;
         end Eat;

         -----------
         -- Flush --
         -----------

         procedure Flush (Sh : in out Stream_Handler)
         is
            Buffer_Size : constant Stream_Element_Offset :=
              Buffer_Type'Object_Size / Stream_Element'Size;

            type SEA_Pointer is
              access all Stream_Element_Array (1 .. Buffer_Size);

            function As_SEA_Pointer is
              new Ada.Unchecked_Conversion (System.Address, SEA_Pointer);

            Offset        : Stream_Element_Offset;
            Buffer_Access : constant SEA_Pointer :=
                              As_SEA_Pointer (Sh.Buffer'Address);

         begin
            Ada.Streams.Read (Input.all, Buffer_Access.all, Offset);

            if Offset < Buffer_Access.all'Last then
               for J in Buffer_Offset (Offset) + 1 .. Sh.Buffer'Last loop
                  Sh.Buffer (J) := ASCII.NUL;
               end loop;
            end if;

            Sh.Offset := Buffer_Offset'First;
         end Flush;

         -----------------
         -- Read_Number --
         -----------------

         procedure Read_Number
           (Sh     : in out Stream_Handler;
            Number : in out Integer)
         is
            C      : Character;
         begin
            Number := 0;

            loop
               C := Sh.Look_Ahead;
               exit when not Is_Digit (C);
               Sh.Eat;
               Number := Number * 10 + (Character'Pos (C)
                 - Character'Pos ('0'));
            end loop;
         end Read_Number;

         -----------------------------------
         -- Is_Valid_Identifier_Character --
         -----------------------------------

         function Is_Valid_Identifier_Character
           (C : Character) return Boolean is
         begin
            return Is_Alphanumeric (C) or else C = '-' or else C = '_';
         end Is_Valid_Identifier_Character;

         ---------------------
         -- Read_Identifier --
         ---------------------

         procedure Read_Identifier
           (Sh         : in out Stream_Handler;
            Identifier : in out String_Access)
         is
            C          : Character;
            Str        : Unbounded_String;
         begin
            loop
               C := Sh.Look_Ahead;
               exit when not Is_Valid_Identifier_Character (C);
               Sh.Eat;
               Append (Str, C);
            end loop;

            Identifier := new String'(To_String (Str));
         end Read_Identifier;

         --------------------
         --  Read_C_String --
         --------------------

         procedure Read_C_String
           (Sh            : in out Stream_Handler;
            C_String      : in out String_Access)
         is
            Previous_Char : Character := ASCII.NUL;
            Current_Char  : Character := ASCII.NUL;
            Str           : Unbounded_String;

         begin
            pragma Assert (Sh.Look_Ahead = '"');
            Sh.Eat;  -- Eats the starting quotation char.

            loop
               Previous_Char := Current_Char;
               Current_Char := Sh.Look_Ahead;
               exit when Current_Char = '"'
                         and then Previous_Char /= '\';
               Sh.Eat;
               Append (Str, Current_Char);
            end loop;

            pragma Assert (Sh.Look_Ahead = '"');
            Sh.Eat;  -- Eats the terminating quotation char.

            C_String := new String'(To_String (Str));
         end Read_C_String;

      end Handler;

      ---------------------------
      -- Variables declaration --
      ---------------------------

      C               : Character := ASCII.NUL;  --  The current character
      C_String_Access : String_Access := null;   --  The current lexed string
      List            : Token_List;              --  The resulting token list
      Number          : Integer := 0;            --  The extracted token value
      Sh              : Handler.Stream_Handler;  --  The input stream
      Word            : Unbounded_String;        --  A single word

   begin
      Main_Loop : loop
         C := Sh.Look_Ahead;

         exit Main_Loop when C = ASCII.NUL;

         --  Reads characters one by one and try to figure out a token list.
         --  If the token is composed of only one character, then append it
         --  directly to the list and loop.  Else, if it is a number, then read
         --  the longest number possible ; if it is a c-string (i.e. a doubly
         --  quoted string) then read until the terminating quotation mark ;
         --  else, read the longest identifier possible.

         case C is
            when ASCII.LF =>
               List.Append (Token_Type'(Code => Newline, Line => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
               Sh.Column := 1;
               Sh.Line := Sh.Line + 1;
            when '&' =>
               List.Append (Token_Type'(Code   => Ampersand,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '@' =>
               List.Append (Token_Type'(Code   => At_Sign,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '~' =>
               List.Append (Token_Type'(Code   => Tilde,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when ',' =>
               List.Append (Token_Type'(Code   => Comma,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '*' =>
               List.Append (Token_Type'(Code   => Asterisk,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '+' =>
               List.Append (Token_Type'(Code   => Plus_Sign,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '=' =>
               List.Append (Token_Type'(Code   => Equal_Sign,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '^' =>
               List.Append (Token_Type'(Code   => Caret,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '[' =>
               List.Append (Token_Type'(Code   => L_Bracket,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when ']' =>
               List.Append (Token_Type'(Code   => R_Bracket,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '{' =>
               List.Append (Token_Type'(Code   => L_Brace,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '}' =>
               List.Append (Token_Type'(Code   => R_Brace,
                                        Line => Sh.Line,
                                        Column => Sh.Column));
               Sh.Eat;
            when '"' =>
               Sh.Read_C_String (C_String_Access);
               List.Append (Token_Type'(Code   => C_String,
                                        Text   => C_String_Access,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
            when '(' =>
               Sh.Eat;
               Word := To_Unbounded_String ("");

               Inner_Loop : loop
                  C := Sh.Look_Ahead;

                  exit Inner_Loop when C = ')';
                  exit Main_Loop when C = ASCII.NUL;

                  Sh.Eat;
                  Word := Word & C;
               end loop Inner_Loop;

               pragma Assert (C = ')');
               Sh.Eat;

               --  `(gdb)' is the only token using parentheses...
               if Word = "gdb" then
                  List.Append (Token_Type'(Code   => Gdb_Prompt,
                                           Line   => Sh.Line,
                                           Column => Sh.Column));
               else  -- ...anything else is an invalid token.
                  Clear_Token_List (List);
                  raise Lexer_Error with ("Unexpected token `("
                                          & To_String (Word)
                                          & ")', expected (gdb)");
               end if;
            when '0' .. '9' =>
               Sh.Read_Number (Number);
               List.Append (Token_Type'(Code   => Token_No,
                                        Value  => Number,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
            when others =>
               Sh.Read_Identifier (C_String_Access);

               if C_String_Access.all = "" then
                  Clear_Token_List (List);
                  raise Lexer_Error with ("Invalid token `" & C & "' at "
                                          & "line"
                                          & Positive'Image (Sh.Line)
                                          & " and column"
                                          & Positive'Image (Sh.Column));
               end if;

               List.Append (Token_Type'(Code   => Identifier,
                                        Text   => C_String_Access,
                                        Line   => Sh.Line,
                                        Column => Sh.Column));
         end case;
      end loop Main_Loop;

      --  Finally append the End_Of_File token to the list.

      List.Append (Token_Type'(Code   => End_Of_File,
                               Line   => Sh.Line,
                               Column => Sh.Column));

      return List;
   end Build_Tokens;

end MI.Lexer;
