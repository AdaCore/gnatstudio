-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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
--  this error. If you want to create a new category, just type an indedite
--  indedite value for the discriminant. The genius programmers of GPS should
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


--  The second procedure, the most important one is the procedure Fix:

--  procedure Fix
--    (This         : Error_Parser;
--     Errors_List  : in out Errors_Interface'Class;
--     Current_Text : Text_Navigator_Abstr'Class;
--     Message      : Error_Message;
--     Solutions    : out Solution_List;
--     Matches      : Match_Array)

--  This procedure is called when a regular expression of 'This' matches.
--  Usually, a regular expression matching stops the search of other matches.
--  If you want to continue the search, you have to raise
--  'Uncorrectable_Message'. Then, Codefix will consider that the message does
--  not match and the search will continue.

--  A possible fix is represented in an object derived from Text_Command.
--  Most of times, you won't have to care about the internal representation
--  of this object. You just have to get the solution list given from the
--  formal error function, and then return it with the parameter 'Solution'
--  of Fix function.

--  The object Message is the message that matches a parser. The cols that it
--  contains are a little modified (a tabulation is equal to a character, not a
--  position mod 8).

--  The object Matches is the Match_Array resulting from the execution of the
--  pattern matcher. It is constrained between 0 .. n, where n is the number of
--  couple of parenthesis in the regular expression.

--  The object Error_List is the main Error_Interface, the object with whom you
--  can extract new errors captions. It is necessary to use this object when a
--  message is on more than one line. The problem is that if you use the
--  function 'Get_Message' and discover that the message is not matching with
--  ones you need, this message will never be parsed by other parsers. So, you
--  have to call first the function 'Preview', check if you really want to
--  treat this message and then call 'Skip_Message' to suppress the message
--  from the list. This function assumes that you won't need to get nonadjacent
--  message for the same treatment. If you need such of thing, add a complain
--  at the beginning of the document.

--  In the body of Fix, you have to call one or more functions from
--  Formal_Errors to generate at least one Text_Command and add it in
--  'Solutions'.


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
with Generic_List;

with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;

use Codefix.Formal_Errors.Command_List;
use Codefix.Formal_Errors.Cursor_Lists;

package Codefix.Errors_Parser is

   procedure Get_Solutions
     (Current_Text : Text_Navigator_Abstr'Class;
      Errors_List  : in out Errors_Interface'Class;
      Message      : Error_Message;
      Category     : out Dynamic_String;
      Solutions    : out Solution_List);
   --  Here is the big function that analyses a message and return the
   --  possible solutions.

   procedure Free_Parsers;
   --  Free the memory used by the parsers

   Uncorrectable_Message : exception;

   type Ptr_Matcher is access Pattern_Matcher;
   type Arr_Matcher is array (Integer range <>) of Ptr_Matcher;
   procedure Free is new
      Ada.Unchecked_Deallocation (Pattern_Matcher, Ptr_Matcher);

   type Ptr_Natural is access Natural;
   procedure Free is new
      Ada.Unchecked_Deallocation (Natural, Ptr_Natural);

   type Error_Parser
     (Category : Dynamic_String; Nb_Parsers : Natural)
   is abstract tagged record
       Matcher    : Arr_Matcher (1 .. Nb_Parsers);
       Current_It : Ptr_Natural := new Natural;
   end record;
   --  The Error_Parser is used to parse a message and call the rigth
   --  funtions in the formal_errors package

   procedure Fix
     (This         : Error_Parser'Class;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Success      : out Boolean);
   --  Analyse the error message and, if it matches, trasmit it to the abstract
   --  version of Fix. At the end, Solutions contains the possible corrections,
   --  if no possible correction Success is False, otherwise it is True.

   procedure Free (This : in out Error_Parser);
   --  Free the memory associated with an Error_Parser

   procedure Fix
     (This         : Error_Parser;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array) is abstract;
   --  Get informations parsed from the message and call functions in
   --  Formal_Errors in order to find possible corredctions. At the end,
   --  Solutions contains the possible corrections, if no possible correction
   --  Success is False, otherwise it is True.

   procedure Initialize (This : in out Error_Parser) is abstract;
   --  Initialize each field needed by Error_Parser, in particular the matcher.

   type Ptr_Parser is access Error_Parser'Class;

   procedure Free (Data : in out Ptr_Parser);
   --  Free the Data associated with a Ptr_Parser.

   package Parser_List is new Generic_List (Ptr_Parser, Free);
   use Parser_List;

   procedure Add_Parser (New_Parser : Ptr_Parser);
   --  Add a parser in the general parse list.

   General_Parse_List : Parser_List.List := Parser_List.Null_List;
   General_Preferences_List : State_List;

   procedure Initialize_Parsers;
   --  Initialize all Parsers, must be called before the first call of Fix.

   type Agregate_Misspelling is new Error_Parser
     (new String'("Misspelling"), 1)
   with null record;

   procedure Initialize (This : in out Agregate_Misspelling);

   procedure Fix
     (This         : Agregate_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'possible mispelling of "=>"'

   type Ligth_Misspelling is new Error_Parser
     (new String'("Misspelling"), 1)
   with null record;

   procedure Initialize (This : in out Ligth_Misspelling);

   procedure Fix
     (This         : Ligth_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix the most 'possible mispelling of sth'

   type Double_Misspelling is new Error_Parser
     (new String'("Misspelling"), 1)
   with null record;

   procedure Initialize (This : in out Double_Misspelling);

   procedure Fix
     (This         : Double_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix the case where there is an alternative for the correction

   type Goto_Misspelling is new Error_Parser
     (new String'("Misspelling"), 1)
   with null record;

   procedure Initialize (This : in out Goto_Misspelling);

   procedure Fix
     (This         : Goto_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix expressions like 'go  to Label;'

   type Library_Misspelling is new Error_Parser
     (new String'("Misspelling"), 1)
   with record
      Misspelling_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("possible misspelling of ""([^""]+)"""));
   end record;

   procedure Initialize (This : in out Library_Misspelling);

   procedure Free (This : in out Library_Misspelling);

   procedure Fix
     (This         : Library_Misspelling;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Problems of misspelling of packages

   type Sth_Should_Be_Sth is new Error_Parser
     (new String'("Wrong_Keyword"), 2)
   with null record;

   procedure Initialize (This : in out Sth_Should_Be_Sth);

   procedure Fix
     (This         : Sth_Should_Be_Sth;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix messages like 'sth should be sth'

   type Should_Be_Semicolon is new Error_Parser
     (new String'("Wrong_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Should_Be_Semicolon);

   procedure Fix
     (This         : Should_Be_Semicolon;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'perdiod should probably be semicolon'

   type And_Meant is new Error_Parser
     (new String'("Wrong_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out And_Meant);

   procedure Fix
     (This         : And_Meant;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where '&' stands for 'and'

   type Or_Meant is new Error_Parser
     (new String'("Wrong_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Or_Meant);

   procedure Fix
     (This         : Or_Meant;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where '|' stands for 'or'

   type Bad_End_Block is new Error_Parser
     (new String'("Wrong_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Bad_End_Block);

   procedure Fix
     (This         : Bad_End_Block;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix "end sth" expected in column 7 for "sth"

   type Unqualified_Expression is new Error_Parser
     (new String'("Qualified_Expression"), 1)
   with null record;

   procedure Initialize (This : in out Unqualified_Expression);

   procedure Fix
     (This         : Unqualified_Expression;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where ' is missing

   type Goes_Before is new Error_Parser
     (new String'("Wrong_Declaration_Order"), 4)
   with null record;

   procedure Initialize (This : in out Goes_Before);

   procedure Fix
     (This         : Goes_Before;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'so must be before sth'

   type Sth_Expected_3 is new Error_Parser
     (new String'("Keyword_Missing"), 1)
   with null record;

   procedure Initialize (This : in out Sth_Expected_3);

   procedure Fix
     (This         : Sth_Expected_3;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'function, procedure or package expected'

   type Sth_Expected_2 is new Error_Parser
     (new String'("Keyword_Missing"), 1)
   with null record;

   procedure Initialize (This : in out Sth_Expected_2);

   procedure Fix
     (This         : Sth_Expected_2;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix error messages where function or procedure is expected.

   type Sth_Expected is new Error_Parser
     (new String'("Keyword_Missing"), 1)
   with null record;

   procedure Initialize (This : in out Sth_Expected);

   procedure Fix
     (This         : Sth_Expected;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix error messages where a keyword is expected at a position.

   type Missing_Kw is new Error_Parser
     (new String'("Keyword_Missing"), 1)
   with null record;

   procedure Initialize (This : in out Missing_Kw);

   procedure Fix
     (This         : Missing_Kw;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'kw missing'.

   type Missing_Sep is new Error_Parser
     (new String'("Separator_Missing"), 1)
   with record
      Wrong_Form : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("([\w|\s]+;)"));
   end record;

   procedure Free (This : in out Missing_Sep);

   procedure Initialize (This : in out Missing_Sep);

   procedure Fix
     (This         : Missing_Sep;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'sth missing'.

   type Missing_All is new Error_Parser
     (new String'("Semantic_Incoherence"), 2)
   with record
      Col_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("type[\s]+[\w]+[\s]+is[\s]+(access)"));
   end record;

   procedure Free (This : in out Missing_All);

   procedure Initialize (This : in out Missing_All);

   procedure Fix
     (This         : Missing_All;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'add All to'.

   type Statement_Missing is new Error_Parser
     (new String'("Statement_Expected"), 1)
   with null record;

   procedure Initialize (This : in out Statement_Missing);

   procedure Fix
     (This         : Statement_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'statement missing'.

   type Space_Missing is new Error_Parser
     (new String'("Space_Required"), 1)
   with null record;

   procedure Initialize (This : in out Space_Missing);

   procedure Fix
     (This         : Space_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'space required'.

   type Name_Missing is new Error_Parser
     (new String'("Block_Name_Expected"), 3)
   with record
      Matcher_Aux : Arr_Matcher (1 .. 3) :=
        (new Pattern_Matcher'
         (Compile ("(end)[\s]*;", Case_Insensitive)),
         new Pattern_Matcher'
         (Compile ("(exit)", Case_Insensitive)),
         new Pattern_Matcher'
         (Compile ("(end[\s]+record);", Case_Insensitive)));
   end record;

   procedure Free (This : in out Name_Missing);

   procedure Initialize (This : in out Name_Missing);

   procedure Fix
     (This         : Name_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'end sth expected'.

   type Double_Keyword is new Error_Parser
     (new String'("Extra_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Double_Keyword);

   procedure Fix
     (This         : Double_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Extra_Paren is new Error_Parser
     (new String'("Extra_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Extra_Paren);

   procedure Fix
     (This         : Extra_Paren;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Redundant_Keyword is new Error_Parser
     (new String'("Extra_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Redundant_Keyword);

   procedure Fix
     (This         : Redundant_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'retudant sth'.

   type Unexpected_Sep is new Error_Parser
     (new String'("Unexpected_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Unexpected_Sep);

   procedure Fix
     (This         : Unexpected_Sep;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Unexpected_Word is new Error_Parser
     (new String'("Unexpected_Keyword"), 1)
   with null record;

   procedure Initialize (This : in out Unexpected_Word);

   procedure Fix
     (This         : Unexpected_Word;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'unexpected sth ignored'.

   type Kw_Not_Allowed is new Error_Parser
     (new String'("Unallowed_Keyword"), 3)
   with null record;

   procedure Initialize (This : in out Kw_Not_Allowed);

   procedure Fix
     (This         : Kw_Not_Allowed;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'kw not allowed' etc.

   type Sep_Not_Allowed is new Error_Parser
     (new String'("Unallowed_Style_Character"), 4)
   with null record;

   procedure Initialize (This : in out Sep_Not_Allowed);

   procedure Fix
     (This         : Sep_Not_Allowed;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'kw not allowed' etc.

   type Should_Be_In is new Error_Parser
     (new String'("Wrong_Indentation"), 1)
   with null record;

   procedure Initialize (This : in out Should_Be_In);

   procedure Fix
     (This         : Should_Be_In;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'sth should be in column'.

   type Bad_Column is new Error_Parser
     (new String'("Wrong_Indentation"), 3)
   with null record;

   procedure Initialize (This : in out Bad_Column);

   procedure Fix
     (This         : Bad_Column;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix messages about bad indentation.

   type Main_With_Missing is new Error_Parser
     (new String'("Missing_With"), 3)
   with null record;

   procedure Initialize (This : in out Main_With_Missing);

   procedure Fix
     (This         : Main_With_Missing;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix messages that guess a missing with.

   type Bad_Casing_Standard is new Error_Parser
     (new String'("Wrong_Case"), 2)
   with null record;

   procedure Initialize (This : in out Bad_Casing_Standard);

   procedure Fix
     (This         : Bad_Casing_Standard;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix standard case errors.

   type Bad_Casing_Declared is new Error_Parser
     (new String'("Wrong_Case"), 1)
   with null record;

   procedure Initialize (This : in out Bad_Casing_Declared);

   procedure Fix
     (This         : Bad_Casing_Declared;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix case problems with declared words etc.

   type Bad_Casing_Keyword is new Error_Parser
     (new String'("Wrong_Case"), 1)
   with null record;

   procedure Initialize (This : in out Bad_Casing_Keyword);

   procedure Fix
     (This         : Bad_Casing_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix case problems with keywords.

   type Object_Not_Referenced is new Error_Parser
     (new String'("Unit_Not_Referenced"), 6)
   with null record;

   procedure Initialize (This : in out Object_Not_Referenced);

   procedure Fix
     (This         : Object_Not_Referenced;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'sth is not referenced'.

   type Pkg_Not_Referenced is new Error_Parser
     (new String'("Unit_Not_Referenced"), 2)
   with null record;

   procedure Initialize (This : in out Pkg_Not_Referenced);

   procedure Fix
     (This         : Pkg_Not_Referenced;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'no entities of sth are referenced'.

   type Pragma_Missplaced is new Error_Parser
     (new String'("Pragma_Should_Begin"), 1)
   with null record;

   procedure Initialize (This : in out Pragma_Missplaced);

   procedure Fix
     (This         : Pragma_Missplaced;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'pragma must be first line of'.

   type Constant_Expected is new Error_Parser
     (new String'("Var_Not_Modified"), 1)
   with null record;

   procedure Initialize (This : in out Constant_Expected);

   procedure Fix
     (This         : Constant_Expected;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'could be declared constant'.

   type Possible_Interpretation is new Error_Parser
     (new String'("Ambiguous_Expression"), 1)
   with record
      Local_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("possible interpretation at line ([\d]+)"));
      Source_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("possible interpretation at ([^:]+):([\d]+)"));
   end record;

   procedure Initialize (This : in out Possible_Interpretation);

   procedure Free (This : in out Possible_Interpretation);

   procedure Fix
     (This         : Possible_Interpretation;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'ambiguous expression (cannot resolve "Sth")'

   type Hidden_Declaration is new Error_Parser
     (new String'("Ambiguous_Expression"), 1)
   with record
      Check_Possible : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("multiple use clauses cause hiding"));
      Get_From_Current_File : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("hidden declaration at line ([\d]+)"));
      Get_From_Other_File : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("hidden declaration at ([^:]+):([\d]+)"));
   end record;

   procedure Initialize (This : in out Hidden_Declaration);

   procedure Free (This : in out Hidden_Declaration);

   procedure Fix
     (This         : Hidden_Declaration;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'multiple use clause hiding'

   type Redundant_Conversion is new Error_Parser
     (new String'("Useless_Conversion"), 1)
   with null record;

   procedure Initialize (This : in out Redundant_Conversion);

   procedure Fix
     (This         : Redundant_Conversion;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'useless conversion'

   type Missplaced_With is new Error_Parser
     (new String'("Movable_With_Clause"), 2)
     with null record;

   procedure Initialize (This : in out Missplaced_With);

   procedure Fix
     (This         : Missplaced_With;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'with clause can be moved to body'


end Codefix.Errors_Parser;
