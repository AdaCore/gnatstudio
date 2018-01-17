------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

----------------------------------------
--  How to add a new parser in Codefix ?
----------------------------------------

--  An error message treatment in Codefix is divided into two parts: the
--  structure of the message and its semantic meaning. Those two parts are
--  separated into two different packages: Codefix.Errors_Parsers and
--  Codefix.Formal_Errors. Ideally, no information concerning the message
--  caption should appear in Formal_Errors, in order to minimize problems
--  following changes in GNAT error system.

--  Adding an error parser is quite easy. An error parser is a tagged type
--  derived from 'Error_Parser'. The two values of the discriminant should be
--  defined at the derivation. 'Category' is used to know which category of
--  error the parser fixes. It is used to know if the user wants or not to fix
--  this error. If you want to create a new category, just type your own
--  value for the discriminant. The genius programmers of GPS should
--  have managed a way to update automatically the structure of the
--  preferences. The second parameter of 'Error_Parser'is 'Nb_Parser'. It is
--  used to know how many regular expressions you want to check in your parser.
--  In fact, you can have some messages that can't be described in one
--  expression, but close enough to make a bit redundant the creation of a new
--  'Error_Parser'. That's why you can create more than one regular expression
--  for a parser.

--  After having chosen the two discriminants, you must at least implement the
--  two abstract functions. The first one is 'Initialize':

--  procedure Initialize (This : in out Error_Parser)

--  In this procedure, you have to initialize the field 'Matcher', an array
--  constrained between 1 and Nb_Parsers (the second discriminant value), in
--  which you have to create all the pattern matchers. Each pattern matcher is
--  in fact an access to Pattern_Matcher (automatically freed in the Free
--  function of 'Error_Parser'). Basically, the creation of a new regular
--  expression is ' := new Pattern_Matcher'(Compile (...))'.

--  The second and most important procedure is Fix:

--  procedure Fix
--    (This         : Error_Parser;
--     Current_Text : Text_Navigator_Abstr'Class;
--     Message_It   : Error_Message_Iterator;
--     Options      : Fix_Options;
--     Solutions    : out Solution_List;
--     Matches      : Match_Array)

--  This procedure is called when a regular expression of 'This' matches.
--  Usually, a regular expression matching stops the search of other matches.
--  If you want to continue the search, you have to raise
--  'Uncorrectable_Message'. Then, Codefix will consider that the message does
--  not match and the search will continue.

--  Current_Text is the message that matches a parser. The columns that it
--  contains are a little modified (a tabulation is equal to a character, not a
--  position mod 8).

--  Message_It is an iterator which allows you to extract new error captions
--  when a message has more than one line. The problem is that if you use the
--  function 'Get_Message' and discover that the message is not matching with
--  ones you need, this message will never be parsed by other parsers. So, you
--  have to call first the function 'Preview', check if you really want to
--  treat this message and then call 'Skip_Message' to suppress the message
--  from the list. This function assumes that you won't need to get nonadjacent
--  message for the same treatment. If you need such of thing, add a complain
--  at the beginning of the document.

--  A possible fix is represented in an object derived from Text_Command.
--  Most of times, you won't have to care about the internal representation
--  of this object. In the body of Fix, you have to call one or more functions
--  from Formal_Errors to generate at least one Text_Command and add it to the
--  out-mode parameter Solutions.

--  Matches is the Match_Array resulting from the execution of the pattern
--  matcher. It is constrained between 0 .. n, where n is the number of
--  couple of parenthesis in the regular expression.

--  After having created your own 'Error_Parser', you have to add it in the
--  parsers list. Nothing easier, just call the function Add_Parser with an
--  instantiation of your parser. Just a thing to care about: the order of the
--  instantiations is the order of the call. You may want to analyze a
--  possibility before the other ones (to make structures like 'this one is
--  possible only if the previous isn't). That's the only case where the order
--  matters.

--  After having created a new parser, you probably have to create a new
--  function in formal error. There is no real rule, just look at examples and
--  'Text_Manager' packages to know which possibilities you have. The only
--  thing important is to avoid using the error message, but variables that
--  have been initialized from it in the Errors_Parser, because the error
--  message can change and 'Formal_Errors' should not be disturbed by these
--  changes.

with Ada.Unchecked_Deallocation;
with GNAT.Regpat;            use GNAT.Regpat;

with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
with Codefix.Error_Lists;    use Codefix.Error_Lists;
with Codefix.Text_Manager;   use Codefix.Text_Manager;

private with GPS_Vectors;

package Codefix.Errors_Parser is

   Uncorrectable_Message : exception;

   type Ptr_Matcher is access all Pattern_Matcher;
   type Arr_Matcher is array (Integer range <>) of Ptr_Matcher;
   procedure Free is new
      Ada.Unchecked_Deallocation (Pattern_Matcher, Ptr_Matcher);

   type Error_Parser (Nb_Parsers : Natural) is abstract new Root_Error_Parser
   with record
       Matcher    : Arr_Matcher (1 .. Nb_Parsers);
   end record;
   --  The Error_Parser is used to parse a message and call the right
   --  funtions in the formal_errors package

   procedure Fix
     (This         : Error_Parser'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Success      : out Boolean);
   --  Analyse the error message and, if it matches, transmit it to the
   --  abstract version of Fix. At the end, Solutions contains the possible
   --  corrections. If no possible correction is found Success is False,
   --  otherwise it is True.

   procedure Free (This : in out Error_Parser);
   --  Free the memory associated with an Error_Parser

   procedure Fix
     (This         : Error_Parser;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array) is abstract;
   --  Get informations parsed from the message and call functions in
   --  Formal_Errors in order to find possible corrections. At the end,
   --  Solutions contains the possible corrections, if no possible correction
   --  Success is False, otherwise it is True.

   procedure Initialize (This : in out Error_Parser) is abstract;
   --  Initialize each field needed by Error_Parser, in particular the matcher.

   type Ptr_Parser is access all Error_Parser'Class;

   procedure Free (Data : in out Ptr_Parser);
   --  Free the Data associated with a Ptr_Parser.

   type Fix_Processor is private;

   procedure Add_Parser
     (Processor : in out Fix_Processor; New_Parser : Ptr_Parser);

   procedure Get_Solutions
     (Processor    : Fix_Processor;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : in out Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List);
   --  Analyze the message given in parameter, and return possible fixes. If no
   --  fixes can be found, then the resulting solution list is empty.

   procedure Free (Processor : in out Fix_Processor);

   procedure Initialize_Parsers (Processor : in out Fix_Processor);
   --  Initialize all Parsers, must be called before the first call of Fix.

private

   package Parser_List is new GPS_Vectors (Ptr_Parser);

   type Fix_Processor is record
      Parse_List : Parser_List.Vector;
   end record;

end Codefix.Errors_Parser;
