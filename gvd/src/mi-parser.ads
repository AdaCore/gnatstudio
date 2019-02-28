------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

--  The MI.Parser package provides the declaration of the GDB/MI output parser.
--  The input of this parser is the output of the lexer from this API, i.e. a
--  list of predefined and preprocessed tokens.  This parser progressively eats
--  each token accordingly to the MI output grammar rules (see below).  The
--  output of this parser is an AST-like structure built using the types
--  defined in the MI.Ast package.  One can use the Visitor pattern and its
--  implementation from this API to walk through and manipulate this AST (see
--  MI.Ast).

with MI.Ast;   use MI.Ast;
with MI.Lexer; use MI.Lexer;

package MI.Parser is

   -------------------------
   -- MI original grammar --
   -------------------------

   --  The MI Grammar is pretty simple. Here the raw grammar one can find on
   --  GDB MI official website:
   --
   --  output =>
   --   (out-of-band-record)* [result-record] "(gdb)" nl
   --
   --  result-record =>
   --   [Token] "^" result-class ("," result)* nl
   --
   --  out-of-band-record =>
   --   async-record | stream-record
   --
   --  async-record =>
   --   exec-async-output | status-async-output | notify-async-output
   --
   --  exec-async-output =>
   --   [Token] "*" async-output
   --
   --  status-async-output =>
   --   [Token] "+" async-output
   --
   --  notify-async-output =>
   --   [Token] "=" async-output
   --
   --  async-output =>
   --   async-class ("," result)* nl
   --
   --  result-class =>
   --   "done" | "running" | "connected" | "error" | "exit"
   --
   --  async-class =>
   --   "stopped" | ...
   --
   --  result =>
   --   variable "=" value
   --
   --  variable =>
   --   String
   --
   --  value =>
   --   const | tuple | list
   --
   --  const =>
   --   C-String
   --
   --  tuple =>
   --   "{}" | "{" result ("," result)* "}"
   --
   --  list =>
   --   "[]" | "[" result ("," result)* "]" | "[" value ("," value)* "]"
   --
   --  stream-record =>
   --   console-stream-output | target-stream-output | log-stream-output
   --
   --  console-stream-output =>
   --   "~" C-String
   --
   --  target-stream-output =>
   --   "@" C-String
   --
   --  log-stream-output =>
   --   "&" C-String
   --
   --  nl =>
   --   CR | CR-LF
   --
   --  Token => sequence of digits

   ----------------------------
   -- MI implemented grammar --
   ----------------------------

   --  Here is how the grammar is implemented in this API:
   --
   --  unit =>
   --   (output)+
   --
   --  output =>
   --   (out-of-band-record)* [result-record] "(gdb)" nl
   --
   --  out-of-band-record =>
   --   async-output-record | stream-output-record
   --
   --  result-record =>
   --   [Token] "^" result-class ("," result)* nl
   --
   --  async-output-record =>
   --   [Token] ("*" | "+" | "=") async-class  ("," result)* nl
   --
   --  stream-output-record =>
   --   ("~" | "@" | "&") C-String nl
   --
   --  result =>
   --   variable "=" value
   --
   --  value =>
   --   C-String | tuple | list
   --
   --  tuple =>
   --   "{}" | "{" result ("," result)* "}"
   --
   --  list =>
   --   "[]" | "[" result ("," result)* "]" | "[" value ("," value)* "]"

   Parser_Error : exception;
   --  The parser error type exception which is raised on fatal errors.  If an
   --  error can be recovered, a simple warning message is outputed in the
   --  standard error stream.

   procedure Build_Records
     (Tokens : in out Token_List;
      Result : in out Record_List);
   --  This function is the parser main one.  It is fed with a Token_List,
   --  which is the result of the first phase of the parsing, i.e. the lexing,
   --  done by the function MI.Lexer.Build_Tokens. Build_Records return a list
   --  of MI_Record ready to be exploited by the application.

end MI.Parser;
