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

with Ada.Unchecked_Deallocation;

with GNAT.Regpat;            use GNAT.Regpat;
with Generic_List;

with Codefix.Text_Manager;   use Codefix.Text_Manager;
with Codefix.Formal_Errors;  use Codefix.Formal_Errors;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;

use Codefix.Formal_Errors.Extract_List;

package Codefix.Errors_Parser is

   type Error_Subcategorie is
     (Misspelling,
      Wrong_Keyword,
      Unallowed_Tabulation,
      Illegal_Character,
      Qualified_Expression,
      Wrong_Declaration_Order,
      Wrong_Loop_Position,
      Keyword_Missing,
      Separator_Missing,
      Semantic_Incoherence,
      Statement_Expected,
      Space_Required,
      Implicit_Deference,
      Block_Name_Expected,
      Extra_Keyword,
      Unexpected_Keyword,
      Unallowed_Keyword,
      Unallowed_Style_Character,
      Unefficient_Pragma,
      Wrong_Indentation,
      Missing_With,
      Wrong_Case,
      Unit_Not_Referenced,
      Pragma_Should_Begin,
      Var_Not_Modified,
      Ambiguous_Expression,
      Useless_Conversion,
      Movable_With_Clause);
   --  Those subcatgeroies are the reals categories of errors that an user can
   --  choose to correct or not.

   type Error_State is (Enabled, Disabled);
   --  The two states possible for an error.

   procedure Set_Error_State (Error : Error_Subcategorie; State : Error_State);
   --  Modify the current error state.

   function Get_Error_State (Error : Error_Subcategorie) return Error_State;
   --  Return the current error state.

   procedure Get_Solutions
     (Current_Text : Text_Navigator_Abstr'Class;
      Errors_List  : in out Errors_Interface'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List);
   --  Here is the big function that analyses a message and return the
   --  possible solutions.

   procedure Free_Parsers;
   --  Free the memory used by the parsers

   Uncorrectible_Message : exception;

   type Ptr_Matcher is access Pattern_Matcher;
   type Arr_Matcher is array (Integer range <>) of Ptr_Matcher;
   procedure Free is new
      Ada.Unchecked_Deallocation (Pattern_Matcher, Ptr_Matcher);

   type Ptr_Natural is access Natural;
   procedure Free is new
      Ada.Unchecked_Deallocation (Natural, Ptr_Natural);

   type Error_Parser (Subcategorie : Error_Subcategorie; Nb_Parsers : Natural)
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
   General_Errors_Array : array (Error_Subcategorie'Range) of Error_State :=
     (others => Enabled);

   procedure Initialize_Parsers;
   --  Initialize all Parsers, must be called before the first call of Fix.

   type Agregate_Misspelling is new Error_Parser (Misspelling, 1)
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

   type Ligth_Misspelling is new Error_Parser (Misspelling, 1)
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

   type Double_Misspelling is new Error_Parser (Misspelling, 1)
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

   type Goto_Misspelling is new Error_Parser (Misspelling, 1)
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

   type Library_Misspelling is new Error_Parser (Misspelling, 1) with record
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

   type Sth_Should_Be_Sth is new Error_Parser (Wrong_Keyword, 1)
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

   type Should_Be_Semicolon is new Error_Parser (Wrong_Keyword, 1)
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

   type And_Meant is new Error_Parser (Wrong_Keyword, 1)
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

   type Or_Meant is new Error_Parser (Wrong_Keyword, 1) with null record;

   procedure Initialize (This : in out Or_Meant);

   procedure Fix
     (This         : Or_Meant;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where '|' stands for 'or'

   type Unqualified_Expression is new Error_Parser (Qualified_Expression, 1)
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

   type Goes_Before is new Error_Parser (Wrong_Declaration_Order, 4)
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

   type Sth_Expected_3 is new Error_Parser (Keyword_Missing, 1)
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

   type Sth_Expected_2 is new Error_Parser (Keyword_Missing, 1)
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

   type Sth_Expected is new Error_Parser (Keyword_Missing, 1) with null record;

   procedure Initialize (This : in out Sth_Expected);

   procedure Fix
     (This         : Sth_Expected;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix error messages where a keyword is expected at a position.

   type Missing_Begin is new Error_Parser (Keyword_Missing, 2) with
     null record;

   procedure Initialize (This : in out Missing_Begin);

   procedure Fix
     (This         : Missing_Begin;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'missing "begin" '.

   type Missing_Kw is new Error_Parser (Keyword_Missing, 1) with null record;

   procedure Initialize (This : in out Missing_Kw);

   procedure Fix
     (This         : Missing_Kw;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'kw missing'.

   type Missing_Sep is new Error_Parser (Separator_Missing, 1) with record
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

   type Missing_All is new Error_Parser (Semantic_Incoherence, 2) with record
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

   type Statement_Missing is new Error_Parser (Statement_Expected, 1)
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

   type Space_Missing is new Error_Parser (Space_Required, 1)
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

   type Name_Missing is new Error_Parser (Block_Name_Expected, 3) with record
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

   type Double_Keyword is new Error_Parser (Extra_Keyword, 1) with null record;

   procedure Initialize (This : in out Double_Keyword);

   procedure Fix
     (This         : Double_Keyword;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Extra_Paren is new Error_Parser (Extra_Keyword, 1) with null record;

   procedure Initialize (This : in out Extra_Paren);

   procedure Fix
     (This         : Extra_Paren;
      Errors_List  : in out Errors_Interface'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Redundant_Keyword is new Error_Parser (Extra_Keyword, 1)
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

   type Unexpected_Sep is new Error_Parser (Unexpected_Keyword, 1)
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

   type Unexpected_Word is new Error_Parser (Unexpected_Keyword, 1)
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

   type Kw_Not_Allowed is new Error_Parser (Unallowed_Keyword, 3)
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

   type Sep_Not_Allowed is new Error_Parser (Unallowed_Style_Character, 4)
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

   type Should_Be_In is new Error_Parser (Wrong_Indentation, 1)
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

   type Bad_Column is new Error_Parser (Wrong_Indentation, 3)
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

   type Main_With_Missing is new Error_Parser (Missing_With, 3)
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

   type Bad_Casing_Standard is new Error_Parser (Wrong_Case, 2)
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

   type Bad_Casing_Declared is new Error_Parser (Wrong_Case, 1)
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

   type Bad_Casing_Keyword is new Error_Parser (Wrong_Case, 1)
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

   type Object_Not_Referenced is new Error_Parser (Unit_Not_Referenced, 6)
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

   type Pkg_Not_Referenced is new Error_Parser (Unit_Not_Referenced, 2)
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

   type Pragma_Missplaced is new Error_Parser (Pragma_Should_Begin, 1)
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

   type Constant_Expected is new Error_Parser (Var_Not_Modified, 1)
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

   type Possible_Interpretation is new Error_Parser (Ambiguous_Expression, 1)
   with record
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

   type Hidden_Declaration is new Error_Parser (Ambiguous_Expression, 1)
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

   type Redundant_Conversion is new Error_Parser (Useless_Conversion, 1)
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

   type Missplaced_With is new Error_Parser (Movable_With_Clause, 1)
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
