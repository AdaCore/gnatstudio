with Ada.Unchecked_Deallocation;

with GNAT.Regpat; use GNAT.Regpat;
with Generic_List;

with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;

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
      Pragma_Should_Begin);
   --  Those subcatgeroies are the reals categories of errors that an user can
   --  choose to correct or not.

   type Error_State is (Enabled, Disabled);
   --  The two states possible for an error.

   procedure Set_Error_State (Error : Error_Subcategorie; State : Error_State);
   --  Modify the current error state.

   function Get_Error_State (Error : Error_Subcategorie) return Error_State;
   --  Return the current error state.

   function Get_Solutions
     (Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message) return Solution_List;
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

   procedure Correct
     (This         : Error_Parser'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Success      : out Boolean);

   procedure Free (This : in out Error_Parser);

   procedure Correct
     (This         : Error_Parser;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array) is abstract;

   procedure Initialize (This : in out Error_Parser) is abstract;

   type Ptr_Parser is access Error_Parser'Class;

   procedure Free (Data : in out Ptr_Parser);

   package Parser_List is new Generic_List (Ptr_Parser, Free);
   use Parser_List;

   procedure Add_Parser (New_Parser : Ptr_Parser);

   General_Parse_List : Parser_List.List := Parser_List.Null_List;
   General_Errors_Array : array (Error_Subcategorie'Range) of Error_State
      := (others => Enabled);

   procedure Initialize_Parsers;

   type Agregate_Misspelling is new Error_Parser (Misspelling, 1)
      with null record;

   procedure Initialize (This : in out Agregate_Misspelling);

   procedure Correct
     (This         : Agregate_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'possible mispelling of "=>"'

   type Ligth_Misspelling is new Error_Parser (Misspelling, 1)
      with null record;

   procedure Initialize (This : in out Ligth_Misspelling);

   procedure Correct
     (This         : Ligth_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct the most 'possible mispelling of sth'

   type Double_Misspelling is new Error_Parser (Misspelling, 1)
      with null record;

   procedure Initialize (This : in out Double_Misspelling);

   procedure Correct
     (This         : Double_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct the case where there is an alternative for the correction

   type Goto_Misspelling is new Error_Parser (Misspelling, 1)
      with null record;

   procedure Initialize (This : in out Goto_Misspelling);

   procedure Correct
     (This         : Goto_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct expressions like 'go  to Label;'

   type Sth_Should_Be_Sth is new Error_Parser (Wrong_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Sth_Should_Be_Sth);

   procedure Correct
     (This         : Sth_Should_Be_Sth;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct messages like 'sth should be sth'

   type Should_Be_Semicolon is new Error_Parser (Wrong_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Should_Be_Semicolon);

   procedure Correct
     (This         : Should_Be_Semicolon;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'perdiod should probably be semicolon'

   type And_Meant is new Error_Parser (Wrong_Keyword, 1)
      with null record;

   procedure Initialize (This : in out And_Meant);

   procedure Correct
     (This         : And_Meant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct instruction where '&' stands for 'and'

   type Or_Meant is new Error_Parser (Wrong_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Or_Meant);

   procedure Correct
     (This         : Or_Meant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct instruction where '|' stands for 'or'

   type Unqualified_Expression is new Error_Parser (Qualified_Expression, 1)
      with null record;

   procedure Initialize (This : in out Unqualified_Expression);

   procedure Correct
     (This         : Unqualified_Expression;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct instruction where ' is missing

   type Goes_Before is new Error_Parser (Wrong_Declaration_Order, 4)
      with null record;

   procedure Initialize (This : in out Goes_Before);

   procedure Correct
     (This         : Goes_Before;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'so must be before sth'

   type Sth_Expected_3 is new Error_Parser (Keyword_Missing, 1)
      with null record;

   procedure Initialize (This : in out Sth_Expected_3);

   procedure Correct
     (This         : Sth_Expected_3;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'function, procedure or package expected'

   type Sth_Expected_2 is new Error_Parser (Keyword_Missing, 1)
      with null record;

   procedure Initialize (This : in out Sth_Expected_2);

   procedure Correct
     (This         : Sth_Expected_2;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct error messages where function or procedure is expected.

   type Sth_Expected is new Error_Parser (Keyword_Missing, 1)
      with null record;

   procedure Initialize (This : in out Sth_Expected);

   procedure Correct
     (This         : Sth_Expected;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct error messages where a keyword is expected at a position.

   type Missing_Kw is new Error_Parser (Keyword_Missing, 1)
      with null record;

   procedure Initialize (This : in out Missing_Kw);

   procedure Correct
     (This         : Missing_Kw;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'kw missing'.

   type Missing_Sep is new Error_Parser (Separator_Missing, 1) with record
      Wrong_Form : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("([\w|\s]+;)"));
   end record;

   procedure Free (This : in out Missing_Sep);

   procedure Initialize (This : in out Missing_Sep);

   procedure Correct
     (This         : Missing_Sep;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'sth missing'.

   type Missing_All is new Error_Parser (Semantic_Incoherence, 1) with record
      Col_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("type[\s]+[\w]+[\s]+is[\s]+(access)"));
   end record;

   procedure Free (This : in out Missing_All);

   procedure Initialize (This : in out Missing_All);

   procedure Correct
     (This         : Missing_All;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'add All to'.

   type Statement_Missing is new Error_Parser (Statement_Expected, 1)
      with null record;

   procedure Initialize (This : in out Statement_Missing);

   procedure Correct
     (This         : Statement_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'statement missing'.

   type Space_Missing is new Error_Parser (Space_Required, 1)
      with null record;

   procedure Initialize (This : in out Space_Missing);

   procedure Correct
     (This         : Space_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'statement missing'.

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

   procedure Correct
     (This         : Name_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'statement missing'.

   type Double_Keyword is new Error_Parser (Extra_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Double_Keyword);

   procedure Correct
     (This         : Double_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'extra sth ignored'.

   type Extra_Paren is new Error_Parser (Extra_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Extra_Paren);

   procedure Correct
     (This         : Extra_Paren;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'extra sth ignored'.

   type Redundant_Keyword is new Error_Parser (Extra_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Redundant_Keyword);

   procedure Correct
     (This         : Redundant_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'extra sth ignored'.

   type Unexpected_Sep is new Error_Parser (Unexpected_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Unexpected_Sep);

   procedure Correct
     (This         : Unexpected_Sep;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'extra sth ignored'.

   type Unexpected_Word is new Error_Parser (Unexpected_Keyword, 1)
      with null record;

   procedure Initialize (This : in out Unexpected_Word);

   procedure Correct
     (This         : Unexpected_Word;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct  'extra sth ignored'.

   type Kw_Not_Allowed is new Error_Parser (Unallowed_Keyword, 3)
      with null record;

   procedure Initialize (This : in out Kw_Not_Allowed);

   procedure Correct
     (This         : Kw_Not_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'kw not allowed' etc.


   type Sep_Not_Allowed is new Error_Parser (Unallowed_Style_Character, 4)
      with null record;

   procedure Initialize (This : in out Sep_Not_Allowed);

   procedure Correct
     (This         : Sep_Not_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'kw not allowed' etc.

   type Should_Be_In is new Error_Parser (Wrong_Indentation, 1)
      with null record;

   procedure Initialize (This : in out Should_Be_In);

   procedure Correct
     (This         : Should_Be_In;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct 'sth should be in column'.

   type Bad_Column is new Error_Parser (Wrong_Indentation, 3)
      with null record;

   procedure Initialize (This : in out Bad_Column);

   procedure Correct
     (This         : Bad_Column;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct messages about bad indentation.

   type Main_With_Missing is new Error_Parser (Missing_With, 3)
      with null record;


   procedure Initialize (This : in out Main_With_Missing);

   procedure Correct
     (This         : Main_With_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct messages that guess a missing with.

   type Bad_Casing_Standard is new Error_Parser (Wrong_Case, 2)
      with null record;

   procedure Initialize (This : in out Bad_Casing_Standard);

   procedure Correct
     (This         : Bad_Casing_Standard;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct standard case errors.

   type Bad_Casing_Declared is new Error_Parser (Wrong_Case, 1)
      with null record;

   procedure Initialize (This : in out Bad_Casing_Declared);

   procedure Correct
     (This         : Bad_Casing_Declared;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct case problems with declared words etc.

   type Bad_Casing_Keyword is new Error_Parser (Wrong_Case, 1)
      with null record;

   procedure Initialize (This : in out Bad_Casing_Keyword);

   procedure Correct
     (This         : Bad_Casing_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct case problems with declared words etc.

   type Object_Not_Referenced is new Error_Parser (Unit_Not_Referenced, 6)
      with null record;

   procedure Initialize (This : in out Object_Not_Referenced);

   procedure Correct
     (This         : Object_Not_Referenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct problems like 'sth is not referenced'.

   type Pragma_Missplaced is new Error_Parser (Pragma_Should_Begin, 1)
      with null record;

   procedure Initialize (This : in out Pragma_Missplaced);

   procedure Correct
     (This         : Pragma_Missplaced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message      : Error_Message;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Correct problem 'pragma must be first line of'.


end Codefix.Errors_Parser;
