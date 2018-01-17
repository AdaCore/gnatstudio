------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

--  This package defines the GDB/MI output lexer.  Based on the grammar and the
--  possible tokens, this lexer split its input (the MI output) into a list of
--  smaller pieces (tokens) to preprocess the RAW input before forwarding it to
--  the parser.  This step prepare and format the data so that the parser task
--  is simplified.  For example, the lexer is in charge of splitting the input
--  into basic tokens such as special characters/operators (e.g. '*', '+',
--  ...), numbers and/or identifiers.  The output of the lexer is a list of
--  tokens to be directly forwarded to the parser.

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Finalization;                   use Ada.Finalization;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;           use Ada.Text_IO.Text_Streams;

package MI.Lexer is

   Lexer_Error : exception;
   --  The lexer error type exception which is raised on fatal errors during
   --  the lexing phase.

   type Token_Code is (Token_No, Identifier, Newline, Ampersand, At_Sign,
                       Tilde, Comma, Asterisk, Plus_Sign, Equal_Sign, Caret,
                       L_Bracket, R_Bracket, L_Brace, R_Brace, C_String,
                       Gdb_Prompt, End_Of_File, Unknown);

   --  Token code type enumeration.
   --  List all the possible tokens one can encounter when parsing GDB/MI
   --  output.

   type Token_Type (Code : Token_Code := Unknown) is record
      Line   : Natural := 1;
      Column : Natural := 1;

      case Code is
         when Token_No =>
            Value : Integer := 0;

         when Identifier | C_String =>
            Text : String_Access := null;

         when others =>
            null;
      end case;
   end record;

   --  A token structure, which can vary depending on the token value.
   --
   --  A Token_Type has a (integer) value if it holds a GDB/MI grammar token,
   --  or a String is it holds an Identifier or a C_String. In any other cases,
   --  nothing is added to the structure.
   --
   --  In the Identifier and C_String case, the structure contains a pointer to
   --  a String.

   overriding function "=" (Left, Right : Token_Type) return Boolean;
   --  Equality operator on Token_Type. Needed by the Token_List declared
   --  further.

   package Token_Lists is new Doubly_Linked_Lists (Token_Type);
   --  List of Token_Type returned by the lexer.

   subtype Token_List is Token_Lists.List;

   procedure Clear_Token (Token : in out Token_Type);
   --  Release memory potentially allocated for the given token.

   procedure Clear_Token_List (Tokens : in out Token_List);
   --  Iterate over the token list, release memory allocated for each token,
   --  and clear the list.

   function Build_Tokens (Input : Stream_Access) return Token_List;
   function Build_Tokens (Input : String) return Token_List;
   --  The main function of the lexer, which is fed with a stream or string,
   --  and return a list of tokens.

   function Image (Item : Token_Type) return String;

   type Token_List_Controller is
     new Ada.Finalization.Limited_Controlled with record
      List : Token_List;
   end record;

   overriding procedure Finalize (This : in out Token_List_Controller);

end MI.Lexer;
