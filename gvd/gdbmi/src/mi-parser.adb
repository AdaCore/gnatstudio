------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
--                                                                          --
-- GPS is free software;  you can  redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;  -- For Count_Type
with Ada.Text_IO;    use Ada.Text_IO;

package body MI.Parser is

   -----------------
   -- Get_Records --
   -----------------

   procedure Build_Records
     (Tokens : in out Token_List;
      Result : in out Record_List) is

      ----------------------------------
      -- First & Followers definition --
      ----------------------------------

      --  The following part declares and implements a set of inlined functions
      --  corresponding to the firsts and followers of each rule from the
      --  GDB/MI output grammar.

      function Is_Unit_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Unit_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `output' rule, i.e. Token in (#, '*', '+', '=', '~', '@',
      --  '&', '^').

      function Is_Unit_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_Unit_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `output' rule, i.e. Token in (EOF).

      function Is_Output_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Output_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `output' rule, i.e. Token in (#, '*', '+', '=', '~', '@',
      --  '&', '^').

      function Is_Output_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_Output_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `output' rule, i.e. Token in (#, '*', '+', '=',
      --  '~', '@', '&', '^', EOF).

      function Is_Out_Of_Band_Record_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Out_Of_Band_Record_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `out-of-band-record' rule, i.e. Token in (#, '*', '+', '=',
      --  '~', '@', '&').

      function Is_Out_Of_Band_Record_Follower
        (Token : Token_Type) return Boolean;
      pragma Inline (Is_Out_Of_Band_Record_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `out-of-band-record' rule, i.e. Token in (#, '*',
      --  '+', '=', '~', '@', '&', '^', '(gdb)').

      function Is_Result_Record_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Result_Record_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `result-record' rule, i.e. Token in (#, '^').

      function Is_Result_Record_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_Result_Record_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `result-record' rule, i.e. Token in ('(gdb)').

      function Is_Async_Output_Record_First
        (Token : Token_Type) return Boolean;
      pragma Inline (Is_Async_Output_Record_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `async-output-record' rule, i.e. Token in (#, '*', '+', '=').

      function Is_Async_Output_Record_Follower
        (Token : Token_Type) return Boolean;
      pragma Inline (Is_Async_Output_Record_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `async-output-record' rule, i.e. Token in ('~',
      --  '@', '&', '*', '+', '=', '#', '^', '(gdb)').

      function Is_Stream_Output_Record_First
        (Token : Token_Type) return Boolean;
      pragma Inline (Is_Stream_Output_Record_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `stream-output-record' rule, i.e. Token in ('~', '@', '&').

      function Is_Stream_Output_Record_Follower
        (Token : Token_Type) return Boolean;
      pragma Inline (Is_Stream_Output_Record_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `stream-output-record' rule, i.e. Token in ('~',
      --  '@', '&', '*', '+', '=', '#', '^', '(gdb)').

      function Is_Result_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Result_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `result' rule, i.e. Token in (Identifier).

      function Is_Result_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_Result_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `result' rule, i.e. Token in (',', NL, ']', '}').

      function Is_Value_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Value_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `value' rule, i.e. Token in (c-string, '[', '{').

      function Is_Value_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_Value_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `value' rule, i.e. Token in (',', NL, ']', '}').

      function Is_Tuple_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_Tuple_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `tuple' rule, i.e. Token in ('{').

      function Is_Tuple_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_Tuple_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `tuple' rule, i.e. Token in (',', NL, ']', '}').

      function Is_List_First (Token : Token_Type) return Boolean;
      pragma Inline (Is_List_First);
      --  This function checks whether or not a given token is a valid "first"
      --  for the `list' rule, i.e. Token in ('[').

      function Is_List_Follower (Token : Token_Type) return Boolean;
      pragma Inline (Is_List_Follower);
      --  This function checks whether or not a given token is a valid
      --  "follower" for the `list' rule, i.e. Token in (',', NL, ']', '}').

      function Is_Known_Result_Class (Token : String) return Boolean;
      pragma Inline (Is_Known_Result_Class);
      --  This function checks whether or not the given token is one of the
      --  known `result-class' possible string, i.e.
      --   * "done"
      --   * "running"
      --   * "connected"
      --   * "error"
      --   * "exit"

      function Is_Known_Async_Class (Token : String) return Boolean;
      pragma Inline (Is_Known_Async_Class);
      --  This function checks whether or not the given token is one of the
      --  known `async-class' possible string, i.e.

      -------------------
      -- Is_Unit_First --
      -------------------

      function Is_Unit_First (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Asterisk | Plus_Sign | Equal_Sign | Tilde | At_Sign
                 | Ampersand | Caret =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Unit_First;

      ----------------------
      -- Is_Unit_Follower --
      ----------------------

      function Is_Unit_Follower (Token : Token_Type) return Boolean is
      begin
         return Token.Code = End_Of_File;
      end Is_Unit_Follower;

      ---------------------
      -- Is_Output_First --
      ---------------------

      function Is_Output_First (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Asterisk | Plus_Sign | Equal_Sign | Tilde | At_Sign
                 | Ampersand | Caret =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Output_First;

      ------------------------
      -- Is_Output_Follower --
      ------------------------

      function Is_Output_Follower (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Asterisk | Plus_Sign | Equal_Sign | Tilde | At_Sign
                 | Ampersand | Caret | End_Of_File =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Output_Follower;

      ---------------------------------
      -- Is_Out_Of_Band_Record_First --
      ---------------------------------

      function Is_Out_Of_Band_Record_First
        (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Asterisk | Plus_Sign | Equal_Sign | Tilde | At_Sign
                 | Ampersand =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Out_Of_Band_Record_First;

      ------------------------------------
      -- Is_Out_Of_Band_Record_Follower --
      ------------------------------------

      function Is_Out_Of_Band_Record_Follower
        (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Asterisk | Plus_Sign | Equal_Sign | Tilde | At_Sign
                 | Ampersand | Caret | Gdb_Prompt =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Out_Of_Band_Record_Follower;

      ----------------------------
      -- Is_Result_Record_First --
      ----------------------------

      function Is_Result_Record_First (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Caret =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Result_Record_First;

      -------------------------------
      -- Is_Result_Record_Follower --
      -------------------------------

      function Is_Result_Record_Follower (Token : Token_Type) return Boolean is
      begin
         return Token.Code = Gdb_Prompt;
      end Is_Result_Record_Follower;

      ----------------------------------
      -- Is_Async_Output_Record_First --
      ----------------------------------

      function Is_Async_Output_Record_First
        (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Token_No | Asterisk | Plus_Sign | Equal_Sign =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Async_Output_Record_First;

      -------------------------------------
      -- Is_Async_Output_Record_Follower --
      -------------------------------------

      function Is_Async_Output_Record_Follower
        (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Tilde | At_Sign | Ampersand | Asterisk | Plus_Sign
                 | Equal_Sign | Token_No | Caret | Gdb_Prompt =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Async_Output_Record_Follower;

      -----------------------------------
      -- Is_Stream_Output_Record_First --
      -----------------------------------

      function Is_Stream_Output_Record_First
        (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Tilde | At_Sign | Ampersand =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Stream_Output_Record_First;

      --------------------------------------
      -- Is_Stream_Output_Record_Follower --
      --------------------------------------

      function Is_Stream_Output_Record_Follower
        (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Tilde | At_Sign | Ampersand | Asterisk | Plus_Sign
                 | Equal_Sign | Token_No | Caret | Gdb_Prompt =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Stream_Output_Record_Follower;

      ---------------------
      -- Is_Result_First --
      ---------------------

      function Is_Result_First (Token : Token_Type) return Boolean is
      begin
         return Token.Code = Identifier;
      end Is_Result_First;

      ------------------------
      -- Is_Result_Follower --
      ------------------------

      function Is_Result_Follower (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Comma | Newline | R_Bracket | R_Brace =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Result_Follower;

      --------------------
      -- Is_Value_First --
      --------------------

      function Is_Value_First (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when C_String | L_Bracket | L_Brace =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Value_First;

      -----------------------
      -- Is_Value_Follower --
      -----------------------

      function Is_Value_Follower (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Comma | Newline | R_Bracket | R_Brace =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Value_Follower;

      --------------------
      -- Is_Tuple_First --
      --------------------

      function Is_Tuple_First (Token : Token_Type) return Boolean is
      begin
         return Token.Code = L_Brace;
      end Is_Tuple_First;

      -----------------------
      -- Is_Tuple_Follower --
      -----------------------

      function Is_Tuple_Follower (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Comma | Newline | R_Bracket | R_Brace =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Tuple_Follower;

      -------------------
      -- Is_List_First --
      -------------------

      function Is_List_First (Token : Token_Type) return Boolean is
      begin
         return Token.Code = L_Bracket;
      end Is_List_First;

      ----------------------
      -- Is_List_Follower --
      ----------------------

      function Is_List_Follower (Token : Token_Type) return Boolean is
      begin
         case Token.Code is
            when Comma | Newline | R_Bracket | R_Brace =>
               return True;
            when others =>
               return False;
         end case;
      end Is_List_Follower;

      ----------------------------
      -- Result & Async Classes --
      ----------------------------

      --  result-class and async-class terminals from the GDB/MI output grammar
      --  are more or less well defined.  For example, result-class can be of 5
      --  different types (done, running, connected, error, exit), which are
      --  declared in the grammar, but async-class is "stopped | others (where
      --  others will be added depending on the needs -- this is still in
      --  development).".  For this reason, the lexer is a bit more laxist, and
      --  consider them as Identifier (which does not insert any conflict in
      --  the grammar).  Since, by precaution, we provide two expression
      --  functions, Is_Known_Result_Class and Is_Known_Async_Class to ensure
      --  that the identifier is a know class.  If not, a warning can be spawn
      --  to eventually ask for that unknown class to be officially
      --  incorporated into the grammar.

      ---------------------------
      -- Is_Known_Result_Class --
      ---------------------------

      function Is_Known_Result_Class (Token : String) return Boolean is
      begin
         return Token = "done" or else Token = "running"
                  or else Token = "connected" or else Token = "error"
                  or else Token = "exit";
      end Is_Known_Result_Class;
      --  This function checks whether or not the given token is one of the
      --  known `result-class' possible string, i.e.
      --   * "done"
      --   * "running"
      --   * "connected"
      --   * "error"
      --   * "exit"

      --------------------------
      -- Is_Known_Async_Class --
      --------------------------

      function Is_Known_Async_Class (Token : String) return Boolean is
      begin
         return Token = "stopped";
      end Is_Known_Async_Class;

      ---------------------------------
      -- Parse functions declaration --
      ---------------------------------

      function Look_Ahead (Tokens : Token_List) return Token_Type;
      --  Returns the first token of the list

      procedure Eat (Tokens : in out Token_List);
      --  Consumes a token from the list

      procedure Step (Tokens : in out Token_List; Next : in out Token_Type);
      --  Has the effect of both Eat and Look_Ahead: consumes a token and
      --  return the following.

      function Is_Result_Record (Tokens : Token_List) return Boolean;
      --  Look two tokens ahead (since the grammar is LL(2)) to determine
      --  whether the current expression is a result-record or an
      --  out-of-band-record.

      procedure Parse_Unit
        (Tokens : in out Token_List;
         Result : in out Record_List);
      --  unit =>
      --   (output)+

      procedure Parse_Output
        (Tokens : in out Token_List;
         Result : in out Record_List);
      --  output =>
      --   (out-of-band-record)* [result-record] "(gdb)" nl

      procedure Parse_Out_Of_Band_Record
        (Tokens : in out Token_List;
         Result : out MI_Record_Access);
      --  out-of-band-record =>
      --   async-output-record | stream-output-record

      procedure Parse_Result_Record
        (Tokens : in out Token_List;
         Result : out Result_Record_Access);
      --  result-record =>
      --   [Token] "^" result-class ("," result)* nl

      procedure Parse_Async_Output_Record
        (Tokens : in out Token_List;
         Result : out Result_Record_Access);
      --  async-output-record =>
      --   [Token] ("*" | "+" | "=") async-class  ("," result)* nl

      procedure Parse_Stream_Output_Record
        (Tokens : in out Token_List;
         Result : out Stream_Output_Record_Access);
      --  stream-output-record =>
      --   ("~" | "@" | "&") C-String nl

      procedure Parse_Result
        (Tokens : in out Token_List;
         Result : out Result_Pair);
      --  result =>
      --   variable "=" value

      procedure Parse_Value
        (Tokens : in out Token_List;
         Value  : out MI_Value_Access);
      --  value =>
      --   C-String | tuple | list

      procedure Parse_Tuple
        (Tokens : in out Token_List;
         Value  : out MI_Value_Access);
      --  tuple =>
      --   "{}" | "{" result ("," result)* "}"

      procedure Parse_List
        (Tokens : in out Token_List;
         Value  : out MI_Value_Access);
      --  list =>
      --   "[]" | "[" result ("," result)* "]" | "[" value ("," value)* "]"

      ----------------
      -- Look_Ahead --
      ----------------

      function Look_Ahead (Tokens : Token_List) return Token_Type
      is
         Cursor : constant Token_Lists.Cursor := Tokens.First;
      begin
         --  End with a End_Of_File token
         pragma Assert (not Tokens.Is_Empty);
         --  Equivalent assert
         pragma Assert (Token_Lists.Has_Element (Cursor));
         --  End_Of_File MUST be the last token
         pragma Assert (Token_Lists.Element (Cursor).Code /= End_Of_File
                        or else Tokens.Length = 1);

         return Token_Lists.Element (Cursor);
      end Look_Ahead;

      ---------
      -- Eat --
      ---------

      procedure Eat (Tokens : in out Token_List) is
      begin
         pragma Assert (not Tokens.Is_Empty);
         Tokens.Delete_First;
      end Eat;

      ----------
      -- Step --
      ----------

      procedure Step (Tokens : in out Token_List; Next : in out Token_Type) is
      begin
         Eat (Tokens);
         Next := Look_Ahead (Tokens);
      end Step;

      ----------------------
      -- Is_Result_Record --
      ----------------------

      function Is_Result_Record (Tokens : Token_List) return Boolean
      is
         function Look_Twice_Ahead (Tokens : Token_List) return Token_Type;
         --  Gdb/MI output grammar is LL(2), which means that at some point, we
         --  need to look two tokens ahead to make a decision on which rule to
         --  follow and invoke.  The only incertitude spot which make this
         --  grammar LL(2) is when the parser must evaluate an sequence of
         --  tokens to be either part of an out-of-band-record rule or part of
         --  a result-record rule, because both can start by a token number.

         function Look_Twice_Ahead (Tokens : Token_List) return Token_Type
         is
            pragma Assert (Tokens.Length >= 2);
            Cursor : Token_Lists.Cursor := Tokens.First;
         begin
            Cursor := Token_Lists.Next (Cursor);

            pragma Assert (Token_Lists.Has_Element (Cursor));
            pragma Assert (Token_Lists.Element (Cursor).Code /= End_Of_File
                           or else Tokens.Length = 1);

            return Token_Lists.Element (Cursor);
         end Look_Twice_Ahead;

         First  : constant Token_Type := Look_Ahead (Tokens);
         Second : constant Token_Type := Look_Twice_Ahead (Tokens);

      begin
         return (Is_Result_Record_First (First)
                 and then First.Code /= Token_No)
                  or else
                (First.Code = Token_No and then Second.Code = Caret);
      end Is_Result_Record;

      ----------------
      -- Parse_Unit --
      ----------------

      --  unit =>
      --   (output)+
      procedure Parse_Unit
        (Tokens : in out Token_List;
         Result : in out Record_List)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Unit_First (Token));
      begin
         while Is_Output_First (Token) loop
            Parse_Output (Tokens, Result);
            Token := Look_Ahead (Tokens);

            if not Is_Output_Follower (Token) then
               --  ??? try to do some error recovery
               raise Parser_Error with ("syntax error, expected output "
                                        & "follower");
            end if;
         end loop;

         if Token.Code /= End_Of_File then
            --  ??? try to do some error recovery
            raise Parser_Error with "garbage at the end of expression";
         end if;
      end Parse_Unit;

      ------------------
      -- Parse_Output --
      ------------------

      --  output =>
      --   (out-of-band-record)* [result-record] "(gdb)" nl
      procedure Parse_Output
        (Tokens : in out Token_List;
         Result : in out Record_List)
      is
         MIR    : MI_Record_Access;
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Output_First (Token));

      begin
         while Is_Out_Of_Band_Record_First (Token) loop

            --  The grammar is LL(2) at this point.  We need to check two
            --  tokens ahead that the current token is not part of a
            --  `result-record' rule.

            exit when Is_Result_Record (Tokens);

            Parse_Out_Of_Band_Record (Tokens, MIR);
            Result.Append (MIR);
            Token := Look_Ahead (Tokens);

            if not Is_Out_Of_Band_Record_Follower (Token) then
               --  ??? try to do some error recovery
               raise Parser_Error with ("syntax error, expected "
                                        & "out-of-band-record follower");
            end if;
         end loop;

         --  Rest of expression: [result-record] "(gdb)" nl

         Token := Look_Ahead (Tokens);

         if Is_Result_Record_First (Token) then
            Parse_Result_Record (Tokens, Result_Record_Access (MIR));
            Result.Append (MIR);
            Token := Look_Ahead (Tokens);

            if not Is_Result_Record_Follower (Token) then
               --  ??? try to do some error recovery
               raise Parser_Error with ("syntax error, expected "
                                        & "result-record follower");
            end if;
         end if;

         if Token.Code /= Gdb_Prompt then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected '(gdb)'";
         end if;

         Step (Tokens, Token);

         if Token.Code /= Newline then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected newline";
         end if;

         Eat (Tokens);
      end Parse_Output;

      ------------------------------
      -- Parse_Out_Of_Band_Record --
      ------------------------------

      --  out-of-band-record =>
      --   async-output-record | stream-output-record
      procedure Parse_Out_Of_Band_Record
        (Tokens : in out Token_List;
         Result : out MI_Record_Access)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Out_Of_Band_Record_First (Token));
      begin
         if Is_Async_Output_Record_First (Token) then
            Parse_Async_Output_Record (Tokens, Result_Record_Access (Result));
            Token := Look_Ahead (Tokens);

            if not Is_Async_Output_Record_Follower (Token) then
               --  ??? try to do some error recovery
               raise Parser_Error with ("syntax error, expected "
                                        & "async-output-record follower");
            end if;
         else  --  Token MUST be first of Stream_Output_Record
            pragma Assert (Is_Stream_Output_Record_First (Token));
            Parse_Stream_Output_Record (Tokens,
                                        Stream_Output_Record_Access (Result));
            Token := Look_Ahead (Tokens);

            if not Is_Stream_Output_Record_Follower (Token) then
               --  ??? try to do some error recovery
               raise Parser_Error with ("syntax error, expected "
                                        & "stream-output-record follower");
            end if;
         end if;
      end Parse_Out_Of_Band_Record;

      -------------------------
      -- Parse_Result_Record --
      -------------------------

      --  result-record =>
      --   [Token] "^" result-class ("," result)* nl
      procedure Parse_Result_Record
        (Tokens : in out Token_List;
         Result : out Result_Record_Access)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Result_Record_First (Token));
      begin
         Result := new Result_Record;

         --  [Token] "^" result-class ("," result)* nl

         if Token.Code = Token_No then
            Result.all.Token := Token.Value;
            Step (Tokens, Token);
         end if;

         --  "^" result-class ("," result)* nl

         if Token.Code /= Caret then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected '^'";
         end if;

         Result.all.R_Type := Sync_Result;
         Step (Tokens, Token);

         --  result-class ("," result)* nl

         if Token.Code /= Identifier then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected result-class";
         end if;

         if not Is_Known_Result_Class (Token.Text.all) then
            Put_Line ("Warning: unknown result-class `" & Token.Text.all
                      & "'");
         end if;

         Result.all.Class := Token.Text;
         Eat (Tokens);

         --  ("," result)* nl

         loop
            Token := Look_Ahead (Tokens);
            exit when Token.Code /= Comma;
            Step (Tokens, Token);

            Parse_Result_Pair : declare
               Pair : Result_Pair;
            begin
               if not Is_Result_First (Token) then
                  --  ??? try to do some error recovery
                  raise Parser_Error with ("unexpected token, expected result "
                                           & "first");
               end if;

               Parse_Result (Tokens, Pair);
               Token := Look_Ahead (Tokens);

               if not Is_Result_Follower (Token) then
                  --  ??? try to do some error recovery
                  raise Parser_Error with ("unexpected token, expected result"
                                           & "follower");
               end if;

               Result.all.Results.Append (Pair);
            end Parse_Result_Pair;
         end loop;

         if Token.Code /= Newline then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected newline";
         end if;

         Eat (Tokens);
      end Parse_Result_Record;

      -------------------------------
      -- Parse_Async_Output_Record --
      -------------------------------

      --  async-output-record =>
      --   [Token] ("*" | "+" | "=") async-class ("," result)* nl
      procedure Parse_Async_Output_Record
        (Tokens : in out Token_List;
         Result : out Result_Record_Access)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Async_Output_Record_First (Token));
      begin
         Result := new Result_Record;

         --  [Token] ("*" | "+" | "=") async-class ("," result)* nl

         if Token.Code = Token_No then
            Result.all.Token := Token.Value;
            Step (Tokens, Token);
         end if;

         --  ("*" | "+" | "=") async-class ("," result)* nl

         if Token.Code = Asterisk then
            Result.all.R_Type := Async_Exec;
         elsif Token.Code = Plus_Sign then
            Result.all.R_Type := Async_Status;
         elsif Token.Code = Equal_Sign then
            Result.all.R_Type := Async_Notify;
         else
            --  ??? try to do some error recovery
            raise Parser_Error with ("unexpected token, expected either "
                                     & "'*', '+' or '='");
         end if;

         Step (Tokens, Token);

         --  async-class ("," result)* nl

         if Token.Code /= Identifier then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected async-class";
         end if;

         if not Is_Known_Async_Class (Token.Text.all) then
            Put_Line ("Warning: unknown async-class `" & Token.Text.all & "'");
         end if;

         Result.all.Class := Token.Text;
         Eat (Tokens);

         --  ??? split the following section to an independent function
         --  since this is used by both Parse_Result_Record and
         --  Parse_Async_Output_Record.  Say, Parse_Results_Pair.

         loop
            Token := Look_Ahead (Tokens);
            exit when Token.Code /= Comma;
            Step (Tokens, Token);

            Parse_Result_Pair : declare
               Pair : Result_Pair;
            begin
               if not Is_Result_First (Token) then
                  --  ??? try to do some error recovery
                  raise Parser_Error with ("unexpected token, expected result "
                                           & "first");
               end if;

               Parse_Result (Tokens, Pair);
               Token := Look_Ahead (Tokens);

               if not Is_Result_Follower (Token) then
                  --  ??? try to do some error recovery
                  raise Parser_Error with ("unexpected token, expected result "
                                           & "follower");
               end if;

               Result.all.Results.Append (Pair);
            end Parse_Result_Pair;
         end loop;

         if Token.Code /= Newline then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected newline";
         end if;

         Eat (Tokens);
      end Parse_Async_Output_Record;

      --------------------------------
      -- Parse_Stream_Output_Record --
      --------------------------------

      --  stream-output-record =>
      --   ("~" | "@" | "&") C-String nl
      procedure Parse_Stream_Output_Record
        (Tokens : in out Token_List;
         Result : out Stream_Output_Record_Access)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Stream_Output_Record_First (Token));
      begin
         Result := new Stream_Output_Record;

         if Token.Code = Tilde then
            Result.all.Output_Type := Console;
         elsif Token.Code = At_Sign then
            Result.all.Output_Type := Target;
         else  -- Token.Code MUST be Ampersand
            pragma Assert (Token.Code = Ampersand);
            Result.all.Output_Type := Log;
         end if;

         Step (Tokens, Token);

         if Token.Code /= C_String then
            --  ??? try to do some error recovery
            raise Parser_Error with "syntax error, expected c-string";
         end if;

         Result.all.Content := Token.Text;
         Step (Tokens, Token);

         if Token.Code /= Newline then
            --  ??? try to do some error recovery
            raise Parser_Error with "syntax error, expected newline";
         end if;

         Eat (Tokens);
      end Parse_Stream_Output_Record;

      ------------------
      -- Parse_Result --
      ------------------

      --  result =>
      --   variable "=" value
      procedure Parse_Result
        (Tokens : in out Token_List;
         Result : out Result_Pair)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Result_First (Token));  -- Assert is identifier
         pragma Assert (Token.Code = Identifier);  -- Equivalent

      begin
         Result.Variable := Token.Text;
         Step (Tokens, Token);

         if Token.Code /= Equal_Sign then
            raise Parser_Error with "unexpected token. expected `='";
         end if;

         Step (Tokens, Token);

         if not Is_Value_First (Token) then
            --  ??? try to do some error recovery
            --  For example, read until find a valid value first
            raise Parser_Error with "unexpected token, expected value first";
         end if;

         Parse_Value (Tokens, Result.Value);
         Token := Look_Ahead (Tokens);

         if not Is_Value_Follower (Token) then
            --  ??? try to do some error recovery
            raise Parser_Error with "syntax error, expected value follower";
         end if;
      end Parse_Result;

      -----------------
      -- Parse_Value --
      -----------------

      --  value =>
      --   C-String | tuple | list
      procedure Parse_Value
        (Tokens : in out Token_List;
         Value  : out MI_Value_Access)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Value_First (Token));
      begin
         if Token.Code = C_String then
            Value := new String_Value'(Value => Token.Text);
            Eat (Tokens);
         elsif Token.Code = L_Bracket then
            Parse_List (Tokens, Value);
            Token := Look_Ahead (Tokens);

            if not Is_List_Follower (Look_Ahead (Tokens)) then
               --  ??? try to do some error recovery
               raise Parser_Error with "syntax error, expected list follower";
            end if;
         else  -- Token.Code MUST be L_Brace
            pragma Assert (Token.Code = L_Brace);
            Parse_Tuple (Tokens, Value);
            Token := Look_Ahead (Tokens);

            if not Is_Tuple_Follower (Look_Ahead (Tokens)) then
               --  ??? try to do some error recovery
               raise Parser_Error with "syntax error, expected tuple follower";
            end if;
         end if;
      end Parse_Value;

      -----------------
      -- Parse_Tuple --
      -----------------

      --  tuple =>
      --   "{}" | "{" result ("," result)* "}"
      procedure Parse_Tuple
        (Tokens : in out Token_List;
         Value  : out MI_Value_Access)
      is
         V_List : Result_List_Value_Access := null;
         Result : Result_Pair;
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_Tuple_First (Token));
         pragma Assert (Token.Code = L_Brace);  -- Equivalent

      begin
         Value := null;
         Step (Tokens, Token);

         if Token.Code = R_Brace then
            Eat (Tokens);
            return;
         end if;

         if not Is_Result_First (Token) then
            --  ??? try to do some error recovery
            raise Parser_Error with "syntax error, expected result first";
         end if;

         V_List := new Result_List_Value;

         loop
            Parse_Result (Tokens, Result);
            Token := Look_Ahead (Tokens);

            if not Is_Result_Follower (Token) then
               --  ??? try to do some error recovery
               raise Parser_Error with ("syntax error, expected result "
                                        & "follower");
            end if;

            V_List.all.Value.Append (Result);

            exit when Token.Code /= Comma;

            Eat (Tokens);
         end loop;

         if Token.Code /= R_Brace then
            --  ??? try to do some error recovery
            raise Parser_Error with "unexpected token, expected '}'";
         end if;

         Eat (Tokens);
         Value := MI_Value_Access (V_List);
      end Parse_Tuple;

      ----------------
      -- Parse_List --
      ----------------

      --  list =>
      --   "[]" | "[" result ("," result)* "]" | "[" value ("," value)* "]"
      procedure Parse_List
        (Tokens : in out Token_List;
         Value  : out MI_Value_Access)
      is
         Token  : Token_Type := Look_Ahead (Tokens);
         pragma Assert (Is_List_First (Token));
         pragma Assert (Token.Code = L_Bracket);  -- Equivalent

      begin
         Value := null;
         Step (Tokens, Token);

         if Token.Code = R_Bracket then
            Eat (Tokens);
            return;
         end if;

         if Is_Result_First (Token) then
            declare
               Result : Result_Pair;
               V_List : constant Result_List_Value_Access
                          := new Result_List_Value;

            begin
               loop
                  Parse_Result (Tokens, Result);
                  Token := Look_Ahead (Tokens);

                  if not Is_Result_Follower (Token) then
                     --  ??? try to do some error recovery
                     raise Parser_Error with ("syntax error, expected result "
                                              & "follower");
                  end if;

                  V_List.all.Value.Append (Result);

                  exit when Token.Code /= Comma;

                  Eat (Tokens);
               end loop;

               Value := MI_Value_Access (V_List);
            end;
         elsif Is_Value_First (Token) then
            declare
               Val    : MI_Value_Access := null;
               V_List : constant Value_List_Value_Access
                          := new Value_List_Value;

            begin
               loop
                  Parse_Value (Tokens, Val);
                  Token := Look_Ahead (Tokens);

                  if not Is_Value_Follower (Token) then
                     --  ??? try to do some error recovery
                     raise Parser_Error with ("syntax error, expected value "
                                              & "follower");
                  end if;

                  V_List.all.Value.Append (Val);

                  exit when Token.Code /= Comma;

                  Eat (Tokens);
               end loop;

               Value := MI_Value_Access (V_List);
            end;
         else
            --  ??? try to do some error recovery
            raise Parser_Error with ("syntax error, expected result or value "
                                     & "first");
         end if;

         if Token.Code /= R_Bracket then
            --  ??? try to do some error recovery
            --  ??? need to free Value's content
            Value := null;
            raise Parser_Error with "unexpected token, expected ']'";
         end if;

         Eat (Tokens);
      end Parse_List;

      ----------
      -- Body --
      ----------

   begin
      Parse_Unit (Tokens, Result);
      pragma Assert (Is_Unit_Follower (Look_Ahead (Tokens)));
   end Build_Records;

end MI.Parser;
