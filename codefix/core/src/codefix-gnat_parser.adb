------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

with Ada_Semantic_Tree;              use Ada_Semantic_Tree;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Parts;        use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Generics;     use Ada_Semantic_Tree.Generics;
with Codefix.Error_Lists;            use Codefix.Error_Lists;
with Codefix.Text_Manager;           use Codefix.Text_Manager;
with Codefix.Formal_Errors;          use Codefix.Formal_Errors;
with GNAT.Strings;                   use GNAT.Strings;
with GNAT.Regpat;                    use GNAT.Regpat;
with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.VFS;                   use GNATCOLL.VFS;
with GNATCOLL.Xref;
with Language;                       use Language;
with Language.Tree;                  use Language.Tree;
with Language.Tree.Database;         use Language.Tree.Database;

package body Codefix.GNAT_Parser is
   use Cursor_Lists;
   use type GNATCOLL.Xref.Visible_Column;

   ---------------------------
   -- Parsers for GNATCHECK --
   ---------------------------

   type GnatCheck_Missing_Storage_Order is
     new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out GnatCheck_Missing_Storage_Order);

   overriding
   procedure Fix
     (This         : GnatCheck_Missing_Storage_Order;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix missing representation clause Scalar_Storage_Order

   ----------------------
   -- Parsers for GNAT --
   ----------------------

   type Agregate_Misspelling is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Agregate_Misspelling);

   overriding
   procedure Fix
     (This         : Agregate_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'possible mispelling of "=>"'

   type Light_Misspelling is new Error_Parser (3) with null record;

   overriding
   procedure Initialize (This : in out Light_Misspelling);

   overriding
   procedure Fix
     (This         : Light_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix the most 'possible mispelling of sth'

   type Double_Misspelling is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Double_Misspelling);

   overriding
   procedure Fix
     (This         : Double_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix the case where there is an alternative for the correction

   type Goto_Misspelling is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Goto_Misspelling);

   overriding
   procedure Fix
     (This         : Goto_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix expressions like 'go  to Label;'

   type Library_Misspelling is new Error_Parser (1) with record
      Misspelling_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("possible misspelling of ""([^""]+)"""));
   end record;

   overriding
   procedure Initialize (This : in out Library_Misspelling);

   overriding
   procedure Free (This : in out Library_Misspelling);

   overriding
   procedure Fix
     (This         : Library_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Problems of misspelling of packages

   type Sth_Should_Be_Sth is new Error_Parser (4) with null record;

   overriding
   procedure Initialize (This : in out Sth_Should_Be_Sth);

   overriding
   procedure Fix
     (This         : Sth_Should_Be_Sth;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix messages like 'sth should be sth'

   type Should_Be_Semicolon is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Should_Be_Semicolon);

   overriding
   procedure Fix
     (This         : Should_Be_Semicolon;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'perdiod should probably be semicolon'

   type And_Meant is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out And_Meant);

   overriding
   procedure Fix
     (This         : And_Meant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where '&' stands for 'and'

   type Or_Meant is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Or_Meant);

   overriding
   procedure Fix
     (This         : Or_Meant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where '|' stands for 'or'

   type Short_Circuit_Required is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Short_Circuit_Required);

   overriding
   procedure Fix
     (This         : Short_Circuit_Required;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where '|' stands for 'or'

   type Bad_End_Block is new Error_Parser (2) with null record;

   overriding
   procedure Initialize (This : in out Bad_End_Block);

   overriding
   procedure Fix
     (This         : Bad_End_Block;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix "end sth" expected in column 7 for "sth"

   type Unqualified_Expression is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Unqualified_Expression);

   overriding
   procedure Fix
     (This         : Unqualified_Expression;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix instruction where ' is missing

   type Goes_Before is new Error_Parser (4) with null record;

   overriding
   procedure Initialize (This : in out Goes_Before);

   overriding
   procedure Fix
     (This         : Goes_Before;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'so must be before sth'

   type Sth_Expected_3 is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Sth_Expected_3);

   overriding
   procedure Fix
     (This         : Sth_Expected_3;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'function, procedure or package expected'

   type Sth_Expected_2 is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Sth_Expected_2);

   overriding
   procedure Fix
     (This         : Sth_Expected_2;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix error messages where function or procedure is expected.

   type Sth_Expected is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Sth_Expected);

   overriding
   procedure Fix
     (This         : Sth_Expected;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix error messages where a keyword is expected at a position.

   type Missing_Kw is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Missing_Kw);

   overriding
   procedure Fix
     (This         : Missing_Kw;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'kw missing'.

   type Missing_Sep is new Error_Parser (2) with record
      Wrong_Form : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("([\w|\s]+;)"));
   end record;

   overriding
   procedure Free (This : in out Missing_Sep);

   overriding
   procedure Initialize (This : in out Missing_Sep);

   overriding
   procedure Fix
     (This         : Missing_Sep;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'sth missing'.

   type Missing_All is new Error_Parser (2) with record
      null;
   end record;

   overriding
   procedure Free (This : in out Missing_All);

   overriding
   procedure Initialize (This : in out Missing_All);

   overriding
   procedure Fix
     (This         : Missing_All;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'add All to'.

   type Implicit_Dereference is new Error_Parser (1) with null record;

   overriding procedure Initialize (This : in out Implicit_Dereference);

   overriding procedure Fix
     (This         : Implicit_Dereference;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'implicit dereference'.

   type Statement_Missing is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Statement_Missing);

   overriding
   procedure Fix
     (This         : Statement_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'statement missing'.

   type Useless_Assignment is new Error_Parser (1) with null record;

   overriding procedure Initialize (This : in out Useless_Assignment);

   overriding procedure Fix
     (This         : Useless_Assignment;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'useless assignment'.

   type Space_Missing is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Space_Missing);

   overriding
   procedure Fix
     (This         : Space_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'space required'.

   type Two_Spaces_Missing is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Two_Spaces_Missing);

   overriding
   procedure Fix
     (This         : Two_Spaces_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'space required'.

   type Name_Missing is new Error_Parser (3) with record
      Matcher_Aux : GNAT.Strings.String_List (1 .. 3) :=
        (new String'("(end)[\s]*;"),
         new String'("(exit)"),
         new String'("(end[\s]+record);"));
   end record;

   overriding
   procedure Free (This : in out Name_Missing);

   overriding
   procedure Initialize (This : in out Name_Missing);

   overriding
   procedure Fix
     (This         : Name_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'end sth expected'.

   type Double_Keyword is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Double_Keyword);

   overriding
   procedure Fix
     (This         : Double_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Extra_Paren is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Extra_Paren);

   overriding
   procedure Fix
     (This         : Extra_Paren;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Redudant_Paren is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Redudant_Paren);

   overriding
   procedure Fix
     (This         : Redudant_Paren;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'redundant parenthesis'.

   type Redundant_Keyword is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Redundant_Keyword);

   overriding
   procedure Fix
     (This         : Redundant_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'redundant sth'.

   type Redundant_Attribute is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Redundant_Attribute);

   overriding
   procedure Fix
     (This         : Redundant_Attribute;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'redundant attribute'.

   type Redundant_Comparison is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Redundant_Comparison);

   overriding
   procedure Fix
     (This         : Redundant_Comparison;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'comparison with True is redundant'.

   type No_Space_Allowed is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out No_Space_Allowed);

   overriding
   procedure Fix
     (This         : No_Space_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'no space allowed there'.

   type Unexpected_Sep is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Unexpected_Sep);

   overriding
   procedure Fix
     (This         : Unexpected_Sep;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'extra sth ignored'.

   type Unexpected_Word is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Unexpected_Word);

   overriding
   procedure Fix
     (This         : Unexpected_Word;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'unexpected sth ignored'.

   type Kw_Not_Allowed is new Error_Parser (3) with null record;

   overriding
   procedure Initialize (This : in out Kw_Not_Allowed);

   overriding
   procedure Fix
     (This         : Kw_Not_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'kw not allowed' etc.

   type Sep_Not_Allowed is new Error_Parser (5) with null record;

   overriding
   procedure Initialize (This : in out Sep_Not_Allowed);

   overriding
   procedure Fix
     (This         : Sep_Not_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'sep not allowed' etc.

   type In_Should_Be_Omitted is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out In_Should_Be_Omitted);

   overriding
   procedure Fix
     (This         : In_Should_Be_Omitted;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix "in" should be omitted.

   type Out_Should_Be_Omitted is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Out_Should_Be_Omitted);

   overriding
   procedure Fix
     (This         : Out_Should_Be_Omitted;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix "out" should be omitted.

   type Already_Use_Visible is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Already_Use_Visible);

   overriding
   procedure Fix
     (This         : Already_Use_Visible;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'pckg is already use_visible'

   type Redundant_With is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Redundant_With);

   overriding
   procedure Fix
     (This         : Redundant_With;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix for 'redundant with clause'

   type Redundant_With_In_Body is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Redundant_With_In_Body);

   overriding
   procedure Fix
     (This         : Redundant_With_In_Body;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix for 'redundant with clause in body'

   type Use_Valid_Instead is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Use_Valid_Instead);

   overriding
   procedure Fix
     (This         : Use_Valid_Instead;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'use 'Valid attribute instead"

   type Should_Be_In is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Should_Be_In);

   overriding
   procedure Fix
     (This         : Should_Be_In;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix 'sth should be in column'.

   type Bad_Column is new Error_Parser (3) with null record;

   overriding
   procedure Initialize (This : in out Bad_Column);

   overriding
   procedure Fix
     (This         : Bad_Column;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix messages about bad indentation.

   type Main_With_Missing is new Error_Parser (6) with null record;

   overriding
   procedure Initialize (This : in out Main_With_Missing);

   overriding
   procedure Fix
     (This         : Main_With_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix messages that guess a missing with.

   type Bad_Casing_Standard is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Bad_Casing_Standard);

   overriding
   procedure Fix
     (This         : Bad_Casing_Standard;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix standard case errors.

   type Bad_Casing_Declared is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Bad_Casing_Declared);

   overriding
   procedure Fix
     (This         : Bad_Casing_Declared;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix case problems with declared words etc.

   type Bad_Casing_Keyword is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Bad_Casing_Keyword);

   overriding
   procedure Fix
     (This         : Bad_Casing_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix case problems with keywords.

   type Object_Not_Referenced is new Error_Parser (2) with null record;

   overriding
   procedure Initialize (This : in out Object_Not_Referenced);

   overriding
   procedure Fix
     (This         : Object_Not_Referenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'sth is not referenced'.

   type Pkg_Not_Referenced is new Error_Parser (3) with null record;

   overriding
   procedure Initialize (This : in out Pkg_Not_Referenced);

   overriding
   procedure Fix
     (This         : Pkg_Not_Referenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'no entities of sth are referenced'.

   type Never_Read is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Never_Read);

   overriding
   procedure Fix
     (This         : Never_Read;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like '"bla" is assigned but never read'.

   type Never_Assigned is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Never_Assigned);

   overriding
   procedure Fix
     (This         : Never_Assigned;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like '"bla" is assigned but never read'.

   type Pragma_Missplaced is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Pragma_Missplaced);

   overriding
   procedure Fix
     (This         : Pragma_Missplaced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'pragma must be first line of'.

   type Useless_Pragma_Pack is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Useless_Pragma_Pack);

   overriding
   procedure Fix
     (This         : Useless_Pragma_Pack;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'pragma Pack has no effect'.

   type Constant_Expected is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Constant_Expected);

   overriding
   procedure Fix
     (This         : Constant_Expected;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'could be declared constant'.

   type Suspicious_Renaming is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Suspicious_Renaming);

   overriding
   procedure Fix
     (This         : Suspicious_Renaming;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'suggest using an initialized constant object instead'

   type Possible_Interpretation is new Error_Parser (1) with record
      Local_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("possible interpretation at line ([\d]+)"));
      Source_Matcher : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("possible interpretation at ([^:]+):([\d]+)"));
   end record;

   overriding
   procedure Initialize (This : in out Possible_Interpretation);

   overriding
   procedure Free (This : in out Possible_Interpretation);

   overriding
   procedure Fix
     (This         : Possible_Interpretation;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'ambiguous expression (cannot resolve "Sth")'

   type Hidden_Declaration is new Error_Parser (1) with record
      Check_Possible : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("multiple use clauses cause hiding"));
      Get_From_Current_File : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("hidden declaration at line ([\d]+)"));
      Get_From_Other_File : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("hidden declaration at ([^:]+):([\d]+)"));
   end record;

   overriding
   procedure Initialize (This : in out Hidden_Declaration);

   overriding
   procedure Free (This : in out Hidden_Declaration);

   overriding
   procedure Fix
     (This         : Hidden_Declaration;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'multiple use clause hiding'

   type Use_Missing is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Use_Missing);

   overriding
   procedure Fix
     (This         : Use_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'use would make operation legal'

   type Redundant_Conversion is new Error_Parser (3) with null record;

   overriding
   procedure Initialize (This : in out Redundant_Conversion);

   overriding
   procedure Fix
     (This         : Redundant_Conversion;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'useless conversion'

   type Useless_Abs is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Useless_Abs);

   overriding
   procedure Fix
     (This         : Useless_Abs;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'abs applied to known non-negative value has no effect'

   type Missplaced_With is new Error_Parser (2) with null record;

   overriding
   procedure Initialize (This : in out Missplaced_With);

   overriding
   procedure Fix
     (This         : Missplaced_With;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'with clause can be moved to body'

   type Not_Fully_Conformant is new Error_Parser (4) with null record;

   overriding
   procedure Initialize (This : in out Not_Fully_Conformant);

   overriding
   procedure Fix
     (This         : Not_Fully_Conformant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problem 'not fully conformant with declaration'

   type Generic_Use_Unallowed is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Generic_Use_Unallowed);

   overriding
   procedure Fix
     (This         : Generic_Use_Unallowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'a generic package is not allowed in a use clause'.

   type Non_Visible_Declaration is new Error_Parser (1) with record
      Get_From_Current_File : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("non-visible declaration at (line) ([\d]+)"));
      Get_From_Other_File : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("non-visible declaration at ([^\:]+):([\d]+)"));
   end record;

   overriding
   procedure Initialize (This : in out Non_Visible_Declaration);

   overriding
   procedure Fix
     (This         : Non_Visible_Declaration;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'non visible declaration at'.

   type Consecutive_Underlines is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Consecutive_Underlines);

   overriding
   procedure Fix
     (This         : Consecutive_Underlines;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'two consecutives underlines not permitted'.

   type Multiple_Blank_Lines is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Multiple_Blank_Lines);

   overriding
   procedure Fix
     (This         : Multiple_Blank_Lines;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'multiple blank lines'.

   type EOF_Blank_Lines is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out EOF_Blank_Lines);

   overriding
   procedure Fix
     (This         : EOF_Blank_Lines;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'blank line not allowed at end of file'.

   type Suggested_Replacement is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Suggested_Replacement);

   overriding
   procedure Fix
     (This         : Suggested_Replacement;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'suggested replacement:'.

   type Pragma_Pack is new Error_Parser (1) with record
      Use_Pragma : Ptr_Matcher := new Pattern_Matcher'
        (Compile ("use explicit pragma Pack"));
   end record;

   overriding
   procedure Initialize (This : in out Pragma_Pack);

   overriding
   procedure Free (This : in out Pragma_Pack);

   overriding
   procedure Fix
     (This         : Pragma_Pack;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'size given is too small, use pragma Pack'.

   type Undefined_Entity is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Undefined_Entity);

   overriding
   procedure Fix
     (This         : Undefined_Entity;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'bla is undefined' if no more information is given
   --  on that line.

   type Unwanted_Pragma_Unreferenced is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Unwanted_Pragma_Unreferenced);

   overriding
   procedure Fix
     (This         : Unwanted_Pragma_Unreferenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'warning: pragma unreferenced given for...'

   type Wrong_Index_Usage is new Error_Parser (2) with null record;

   overriding
   procedure Initialize (This : in out Wrong_Index_Usage);

   overriding
   procedure Fix
     (This         : Wrong_Index_Usage;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'index number not allowed for one dimensional array'
   --  or 'index number required for multi-dimensional array'

   type Wrong_Sb_Order is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Wrong_Sb_Order);

   overriding
   procedure Fix
     (This         : Wrong_Sb_Order;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like 'subprogram body "bla" not in alphabetical order'

   type No_Statement_Following_Then is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out No_Statement_Following_Then);

   overriding
   procedure Fix
     (This         : No_Statement_Following_Then;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like no statements may follow "then" on same line

   type Elaborate_All_Required is new Error_Parser (1) with null record;

   overriding
   procedure Initialize (This : in out Elaborate_All_Required);

   overriding
   procedure Fix
     (This         : Elaborate_All_Required;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array);
   --  Fix problems like Elaborate_All pragma required for "Procs2"

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : in out GnatCheck_Missing_Storage_Order) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("check: Scalar_Storage_Order is not specified")));
   end Initialize;

   overriding procedure Fix
     (This         : GnatCheck_Missing_Storage_Order;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message      : constant Error_Message := Get_Message (Message_It);
      Message_Str  : constant String := Get_Message (Message);
      Msg_Suffix_1 : constant String := "(Bit_Order is Low_Order_First)";
      Msg_Suffix_2 : constant String := "(Bit_Order is High_Order_First)";

   begin
      --  Case 1: Bit order is Low_Order_First

      if Message_Str (Message_Str'Last - Msg_Suffix_1'Length + 1
                      .. Message_Str'Last)
        = Msg_Suffix_1
      then
         Solutions :=
           Add_Record_Rep_Clause
             (Current_Text  => Current_Text,
              Cursor        => Message,
              Caption       => To_Unbounded_String ("Low_Order_First"),
              First_Clause  =>
                "'Scalar_Storage_Order use System.Low_Order_First;");

      --  Case 2: Bit order is High_Order_First

      elsif Message_Str (Message_Str'Last - Msg_Suffix_2'Length + 1
                      .. Message_Str'Last)
        = Msg_Suffix_2
      then
         Solutions :=
           Add_Record_Rep_Clause
             (Current_Text  => Current_Text,
              Cursor        => Message,
              Caption       => To_Unbounded_String ("Low_Order_First"),
              First_Clause  =>
                "'Scalar_Storage_Order use System.High_Order_First;");

      --  Case 3: Bit order has not been specified

      else
         Solutions :=
           Add_Record_Rep_Clause
             (Current_Text  => Current_Text,
              Cursor        => Message,
              Caption       => To_Unbounded_String ("Low_Order_First"),
              First_Clause  =>
                "'Bit_Order use System.Low_Order_First;",
              Second_Clause =>
                "'Scalar_Storage_Order use System.Low_Order_First;",
              With_Clause   => "System");

         Concat (Solutions,
           Add_Record_Rep_Clause
             (Current_Text  => Current_Text,
              Cursor        => Message,
              Caption       => To_Unbounded_String ("High_Order_First"),
              First_Clause  =>
                "'Bit_Order use System.High_Order_First;",
              Second_Clause =>
                "'Scalar_Storage_Order use System.High_Order_First;"));
      end if;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Agregate_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("misspelling of ""=>""")));
   end Initialize;

   overriding procedure Fix
     (This         : Agregate_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message,
           To_Unbounded_String ("=>"),
           To_Unbounded_String ("="));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Light_Misspelling) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("misspelling of ""([^""]+)""$")),
         new Pattern_Matcher'
           (Compile ("incorrect spelling of keyword ""([^""]+)""$")),
         new Pattern_Matcher'
           (Compile (" expected ""([^""]+)""$")));
   end Initialize;

   overriding procedure Fix
     (This         : Light_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Double_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("misspelling of ""([^""]+)"" or ""([^""]+)""")));
   end Initialize;

   overriding procedure Fix
     (This         : Double_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));

      Concat
        (Solutions,
         Should_Be
           (Current_Text,
            Message,
            To_Unbounded_String
              (Get_Message (Message)
               (Matches (2).First .. Matches (2).Last))));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Goto_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("goto is one word")));
   end Initialize;

   overriding procedure Fix
     (This         : Goto_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message,
           To_Unbounded_String ("goto"),
           To_Unbounded_String ("(go[\s]+to)"),
           Regular_Expression);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Library_Misspelling) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""([^""]+)"" is not a predefined library unit")));
   end Initialize;

   overriding procedure Free (This : in out Library_Misspelling) is
   begin
      Free (Error_Parser (This));
      Free (This.Misspelling_Matcher);
   end Free;

   overriding procedure Fix
     (This         : Library_Misspelling;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options);
      Preview     : Error_Message;
      Fix_Matches : Match_Array (0 .. 1);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      if At_End (Next (Message_It)) then
         raise Uncorrectable_Message;
      end if;

      Preview := Get_Message (Next (Message_It));
      Match (This.Misspelling_Matcher.all, Get_Message (Preview), Fix_Matches);

      if Fix_Matches (0) = No_Match then
         raise Uncorrectable_Message;
      end if;

      Solutions := Should_Be
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Preview)
            (Fix_Matches (1).First .. Fix_Matches (1).Last)),
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         Text_Ascii);

      Cancel_Message (Next (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Sth_Should_Be_Sth) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("""([^""]+)"" should be ""([^""]+)""")),
         new Pattern_Matcher'
           (Compile ("([^\w\s][^\s][^,]*) should be ""([^""]+)""")),
         new Pattern_Matcher'
           (Compile ("([^\w\s][^\s][^,]*) should be ([^\w\s][^\s]*)")),
         new Pattern_Matcher'
           (Compile ("""([^""])+"" illegal here, replaced by ""([^""])+""")));
   end Initialize;

   overriding procedure Fix
     (This         : Sth_Should_Be_Sth;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (2).First .. Matches (2).Last)),
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         Text_Ascii);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Should_Be_Semicolon) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("period should probably be semicolon")));
   end Initialize;

   overriding procedure Fix
     (This         : Should_Be_Semicolon;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message,
           To_Unbounded_String (";"),
           To_Unbounded_String ("."),
           Text_Ascii);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out And_Meant) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("maybe ""and"" was meant")));
   end Initialize;

   overriding procedure Fix
     (This         : And_Meant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message, To_Unbounded_String ("and"),
           To_Unbounded_String ("&"));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Or_Meant) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("did you mean ""or""")));
   end Initialize;

   overriding procedure Fix
     (This         : Or_Meant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message,
           To_Unbounded_String ("or"),
           To_Unbounded_String ("\|"));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Short_Circuit_Required) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""((or else)|(and then))"" required")));
   end Initialize;

   overriding procedure Fix
     (This         : Short_Circuit_Required;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Get_Message (Message_It),
           To_Unbounded_String
             (Get_Message (Message_It).Get_Message
              (Matches (1).First .. Matches (1).Last)),
           To_Unbounded_String ("([\w]+)"),
           Regular_Expression);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Bad_End_Block) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("""(end [\w.]+;)"" expected", Case_Insensitive)),
         new Pattern_Matcher'
           (Compile ("""(end loop [\w]+;)"" expected", Case_Insensitive)));
   end Initialize;

   overriding procedure Fix
     (This         : Bad_End_Block;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message,
           To_Unbounded_String
             (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
           To_Unbounded_String ("([^;]+;)"),
           Regular_Expression);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Unqualified_Expression) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("if qualified expression was meant")));
   end Initialize;

   overriding procedure Fix
     (This         : Unqualified_Expression;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Should_Be
          (Current_Text,
           Message,
           To_Unbounded_String ("'("),
           To_Unbounded_String ("("),
           Text_Ascii,
           To_Unbounded_String ("Replace conversion by qualification"));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Goes_Before) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("""([\w]+)"" goes before ""([\w]+)""")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" must come before ""([\w]+)""")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" should be before ""([\w]+)""")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" must preceed ""([\w]+)""")));
   end Initialize;

   overriding procedure Fix
     (This         : Goes_Before;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Wrong_Order
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         To_Unbounded_String
           (Get_Message (Message) (Matches (2).First .. Matches (2).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Sth_Expected_3) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""function"", ""procedure"" or ""package"" expected")));
   end Initialize;

   overriding procedure Fix
     (This         : Sth_Expected_3;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Expected (Current_Text, Message, To_Unbounded_String ("function"));
      Concat
        (Solutions,
         Expected (Current_Text, Message, To_Unbounded_String ("procedure")));
      Concat
        (Solutions,
         Expected (Current_Text, Message, To_Unbounded_String ("package")));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Sth_Expected_2) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""function"" or ""procedure"" expected")));
   end Initialize;

   overriding procedure Fix
     (This         : Sth_Expected_2;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Expected (Current_Text, Message, To_Unbounded_String ("function"));
      Concat
        (Solutions,
         Expected (Current_Text, Message, To_Unbounded_String ("procedure")));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Sth_Expected) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""([\w]+)"" expected")));
   end Initialize;

   overriding procedure Fix
     (This         : Sth_Expected;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Expected
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Missing_Kw) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("missing ""(\w+)""")));
   end Initialize;

   overriding procedure Fix
     (This         : Missing_Kw;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);

      Str_Red : constant String :=
        Get_Message (Message) (Matches (1).First .. Matches (1).Last);
   begin
      if Str_Red = "return" or else Str_Red = "RETURN"
        or else Str_Red = "begin" or else Str_Red = "BEGIN"
      then
         raise Uncorrectable_Message;
      end if;

      Solutions :=
        Expected (Current_Text, Message, To_Unbounded_String (Str_Red));
   end Fix;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Missing_Sep) is
   begin
      Free (This.Wrong_Form);
      Free (Error_Parser (This));
   end Free;

   overriding procedure Initialize (This : in out Missing_Sep) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("missing ""([^""\w]+)""")),
         2 => new Pattern_Matcher'
           (Compile ("missing (string quote)")));
   end Initialize;

   overriding procedure Fix
     (This         : Missing_Sep;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options);

      Message : constant Error_Message := Get_Message (Message_It);
      Wrong_Matches : Match_Array (0 .. 1);
      Str_Red : constant String :=
        Get_Message (Message) (Matches (1).First .. Matches (1).Last);
      Add_Spaces : Boolean;
   begin
      if Str_Red = "string quote" then
         Solutions := Expected
           (Current_Text,
            Message,
            To_Unbounded_String (""""),
            Add_Spaces => False);
      else
         Match
           (This.Wrong_Form.all,
            Str_Red,
            Wrong_Matches);

         if Wrong_Matches (0) /= No_Match then
            raise Uncorrectable_Message;
         end if;

         if Str_Red = "=>" or else Str_Red = ":=" then
            Add_Spaces := True;
         else
            Add_Spaces := False;
         end if;

         Solutions := Expected
           (Current_Text,
            Message,
            To_Unbounded_String (Str_Red),
            Add_Spaces => Add_Spaces);
      end if;
   end Fix;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Missing_All) is
   begin
      Free (Error_Parser (This));
   end Free;

   overriding procedure Initialize (This : in out Missing_All) is
   begin
      This.Matcher := (new Pattern_Matcher'
        (Compile ("add ""all"" to type ""[\w]+"" defined at (line) ([0-9]+)")),
       new Pattern_Matcher'(Compile
        ("add ""all"" to type ""[\w]+"" defined at ([^:]+):([0-9]+)")));
   end Initialize;

   overriding procedure Fix
     (This         : Missing_All;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Declaration_Cursor : File_Cursor;
      Message : constant Error_Message := Get_Message (Message_It);

   begin
      if Get_Message (Message) (Matches (1).First .. Matches (1).Last) /=
        "line"
      then
         --  ??? Doesn't seem really clean to use the message as a file name ?
         Set_File
           (Declaration_Cursor,
            Get_Registry (Current_Text).Tree.Create
             (+Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
      else
         Set_File (Declaration_Cursor, Get_File (Message));
      end if;

      Set_Location
        (Declaration_Cursor,
         Integer'Value
           (Get_Message (Message) (Matches (2).First .. Matches (2).Last)),
         1);

      Solutions := Expected
        (Current_Text,
         Declaration_Cursor,
         To_Unbounded_String ("all"),
         After_Pattern => "type[\s]+[\w]+[\s]+is[\s]+(access)");
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Implicit_Dereference) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("implicit dereference")));
   end Initialize;

   overriding procedure Fix
     (This         : Implicit_Dereference;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions :=
        Expected
          (Current_Text,
           Get_Message (Message_It),
           To_Unbounded_String (".all"),
           Add_Spaces => False);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Statement_Missing) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("statement expected")));
   end Initialize;

   overriding procedure Fix
     (This         : Statement_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Expected
          (Current_Text,
           Message,
           To_Unbounded_String ("null;"),
           Position => Before);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Useless_Assignment) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("useless assignment")));
   end Initialize;

   overriding procedure Fix
     (This         : Useless_Assignment;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Remove_Statement (Current_Text, Get_Message (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Space_Missing) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'(Compile ("space required")));
   end Initialize;

   overriding procedure Fix
     (This         : Space_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Expected
          (Current_Text,
           Message,
           To_Unbounded_String (" "),
           Add_Spaces => False);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Two_Spaces_Missing) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'(Compile ("two spaces required")));
   end Initialize;

   overriding procedure Fix
     (This         : Two_Spaces_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Expected
          (Current_Text,
           Message,
           To_Unbounded_String ("  "),
           Add_Spaces => False);
   end Fix;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Name_Missing) is
   begin
      for J in This.Matcher_Aux'Range loop
         Free (This.Matcher_Aux (J));
      end loop;

      Free (Error_Parser (This));
   end Free;

   overriding procedure Initialize (This : in out Name_Missing) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("\(style\) ""(end) ([\w.]+)"" required")),
         new Pattern_Matcher'
           (Compile ("\(style\) ""(exit) ([\w]+)"" required")),
         new Pattern_Matcher'
           (Compile ("\(style\) ""(end record) ([\w]+)"" required")));
   end Initialize;

   overriding procedure Fix
     (This         : Name_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options);

      Message : constant Error_Message := Get_Message (Message_It);
      Line_Cursor : File_Cursor := File_Cursor (Message);
      Message_Kind : constant String :=
        Get_Message (Message) (Matches (1).First .. Matches (1).Last);
      Match_Number : Integer := 1;

   begin
      Set_Location (Line_Cursor, Get_Line (Line_Cursor), 1);

      if Message_Kind = "end" then
         Match_Number := 1;
      elsif Message_Kind = "exit" then
         Match_Number := 2;
      elsif Message_Kind = "end record" then
         Match_Number := 3;
      else
         raise Codefix_Panic;
      end if;

      Solutions := Expected
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (2).First .. Matches (2).Last)),
         After_Pattern => This.Matcher_Aux (Match_Number).all);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Double_Keyword) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("extra ""([^""])"" ignored")));
   end Initialize;

   overriding procedure Fix
     (This         : Double_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);

   begin
      Solutions :=
        Unexpected
          (Current_Text,
           Message,
           To_Unbounded_String
             (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Extra_Paren) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("extra right paren")));
   end Initialize;

   overriding procedure Fix
     (This         : Extra_Paren;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Unexpected (Current_Text, Message, To_Unbounded_String (")"));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redudant_Paren) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
       (Compile ("redundant parentheses")));
   end Initialize;

   overriding
   procedure Fix
     (This         : Redudant_Paren;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Parenthesis_Couple (Current_Text, Message);
   end Fix;

   ----------------------
   -- No_Space_Allowed --
   ----------------------

   --  Warning: Spaces are also handled by Sep_Not_Allowed

   overriding
   procedure Initialize (This : in out No_Space_Allowed) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'(Compile ("no space allowed")));
   end Initialize;

   overriding
   procedure Fix
     (This         : No_Space_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Unexpected
          (Current_Text      => Current_Text,
           Message           => Message,
           String_Unexpected => To_Unbounded_String (" "),
           All_Occurrences   => True);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redundant_Keyword) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("redundant (["" \w]+)")));
   end Initialize;

   overriding procedure Fix
     (This         : Redundant_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
      Str_Red : constant String := Get_Message (Message)
        (Matches (1).First .. Matches (1).Last);
   begin
      if Str_Red = "colon" then
         Solutions :=
           Unexpected (Current_Text, Message, To_Unbounded_String (":"));
      elsif Str_Red = """then""" then
         Solutions :=
           Unexpected (Current_Text, Message, To_Unbounded_String ("then"));
      else
         raise Uncorrectable_Message;
      end if;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redundant_Attribute) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("redundant attribute")));
   end Initialize;

   overriding procedure Fix
     (This         : Redundant_Attribute;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Remove_Attribute (Current_Text, Get_Message (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redundant_Comparison) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("comparison with True is redundant")));
   end Initialize;

   overriding procedure Fix
     (This         : Redundant_Comparison;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Remove_Comparison (Current_Text, Get_Message (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Unexpected_Sep) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("unexpected ""([^""]+)"" ignored")));
   end Initialize;

   overriding procedure Fix
     (This         : Unexpected_Sep;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Unexpected
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Unexpected_Word) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("unexpected ([\w]+) ([\w]+)")));
   end Initialize;

   overriding procedure Fix
     (This         : Unexpected_Word;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message   : constant Error_Message := Get_Message (Message_It);
      Str_Red_1 : constant String := Get_Message (Message)
        (Matches (1).First .. Matches (1).Last);
      Str_Red_2 : constant String := Get_Message (Message)
        (Matches (2).First .. Matches (2).Last);
   begin
      if Str_Red_1 = "semicolon" and then Str_Red_2 = "ignored" then
         Solutions :=
           Unexpected (Current_Text, Message, To_Unbounded_String (";"));
      elsif Str_Red_1 = "right" and then Str_Red_2 = "parenthesis" then
         Solutions :=
           Unexpected (Current_Text, Message, To_Unbounded_String (")"));
      else
         raise Uncorrectable_Message;
      end if;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Kw_Not_Allowed) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'(Compile ("""([\w]+)"" not allowed")),
         new Pattern_Matcher'(Compile ("""([\w]+)"" keyword not allowed")),
         new Pattern_Matcher'
           (Compile ("""([\w]+)"" ignored \(only allowed in")));
   end Initialize;

   overriding procedure Fix
     (This         : Kw_Not_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Unexpected
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ---------------------
   -- Sep_Not_Allowed --
   ---------------------

   --  Warning: Spaces are also handled by Space_Not_Allowed

   overriding procedure Initialize (This : in out Sep_Not_Allowed) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("(form feed) not allowed")),
         new Pattern_Matcher'
           (Compile ("(vertical tab) not allowed")),
         new Pattern_Matcher'
           (Compile ("(trailing spaces) not permitted")),
         new Pattern_Matcher'
           (Compile ("(space) not allowed")),
         new Pattern_Matcher'
           (Compile ("\(style\) (horizontal tab) not allowed")));
   end Initialize;

   overriding procedure Fix
     (This         : Sep_Not_Allowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);

      All_Occurrences      : Boolean := False;
      Word_Read            : Unbounded_String;
      Unallowed_Characters : Unbounded_String;
      Format_Str           : String_Mode := Text_Ascii;

   begin
      Word_Read :=
        To_Unbounded_String
          (Get_Message (Message) (Matches (1).First .. Matches (1).Last));

      if Word_Read = "form feed" then
         Unallowed_Characters := To_Unbounded_String ((1 => ASCII.FF));
         Format_Str := Text_Ascii;
      elsif Word_Read = "vertical tab" then
         Unallowed_Characters := To_Unbounded_String ((1 => ASCII.VT));
         Format_Str := Text_Ascii;
      elsif Word_Read = "trailing spaces" then
         Unallowed_Characters := To_Unbounded_String ("([\s]+)");
         Format_Str := Regular_Expression;
      elsif Word_Read = "space" then
         All_Occurrences := True;
         Unallowed_Characters := To_Unbounded_String (" ");
         Format_Str := Text_Ascii;
      elsif Word_Read = "horizontal tab" then
         Solutions := Expand_Tabs (Current_Text, Message);
         return;
      else
         raise Codefix_Panic;
      end if;

      Solutions :=
        Unexpected
          (Current_Text      => Current_Text,
           Message           => Message,
           String_Unexpected => Unallowed_Characters,
           Mode              => Format_Str,
           All_Occurrences   => All_Occurrences);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out In_Should_Be_Omitted) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'(Compile ("""in"" should be omitted")));
   end Initialize;

   overriding procedure Fix
     (This         : In_Should_Be_Omitted;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Unexpected
        (Current_Text      => Current_Text,
         Message           => Get_Message (Message_It),
         String_Unexpected => To_Unbounded_String ("(in[\s]*)"),
         Mode              => Regular_Expression);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Out_Should_Be_Omitted) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
                   (Compile ("mode could be ""in"" instead of ""in out""")));
   end Initialize;

   overriding procedure Fix
     (This         : Out_Should_Be_Omitted;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions :=
        Unexpected
          (Current_Text      => Current_Text,
           Message           => Get_Message (Message_It),
           String_Unexpected => To_Unbounded_String ("([\s]+out)"),
           Mode              => Regular_Expression,
           Search_Forward    => True);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Already_Use_Visible) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("""([\w])+"" is already use.visible")));
   end Initialize;

   overriding procedure Fix
     (This         : Already_Use_Visible;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Dependency_Clause
        (Current_Text, Message, Cat_Use, After);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redundant_With) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("redundant with clause")));
   end Initialize;

   overriding procedure Fix
     (This         : Redundant_With;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Dependency_Clause
        (Current_Text, Message, Cat_With, Before, Look_For_Use => False);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redundant_With_In_Body) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("redundant with clause in body")));
   end Initialize;

   overriding procedure Fix
     (This         : Redundant_With_In_Body;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Dependency_Clause
        (Current_Text, Message, Cat_With, Before, Look_For_Use => False);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Use_Valid_Instead) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("use 'Valid attribute instead")));
   end Initialize;

   overriding procedure Fix
     (This         : Use_Valid_Instead;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Change_To_Tick_Valid (Current_Text, Message);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Should_Be_In) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("should be in column ([0-9]+)")));
   end Initialize;

   overriding procedure Fix
     (This         : Should_Be_In;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Wrong_Column
        (Current_Text,
         Message,
         Visible_Column_Type'Value (Get_Message (Message)
                          (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Bad_Column) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'(Compile ("bad column")),
         new Pattern_Matcher'(Compile ("incorrect layout")),
         new Pattern_Matcher'(Compile ("bad indentation")));
   end Initialize;

   overriding procedure Fix
     (This         : Bad_Column;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Wrong_Column (Current_Text, Message);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Main_With_Missing) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("missing (with) for ""([^""]+)""()()")),
         new Pattern_Matcher'
           (Compile ("possible missing (with) of ([\w]+)()()")),
         new Pattern_Matcher'
           (Compile ("missing (with)_clause on ""([^""])""()()")),
         new Pattern_Matcher'
           (Compile ("missing ""(with) ([^;]+)(;)""()")),
         new Pattern_Matcher'
           (Compile ("possible missing ""(with) ([^;]+)(;)""()")),
         new Pattern_Matcher'
           (Compile ("possible missing ""(with) ([^;]+)(;) (use) [^;]+""")));
   end Initialize;

   overriding procedure Fix
     (This         : Main_With_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);

      With_Str : constant String :=
        Get_Message (Message) (Matches (1).First .. Matches (1).Last);
      Pckg_Str : constant Unbounded_String :=
        To_Unbounded_String
          (Get_Message (Message) (Matches (2).First .. Matches (2).Last));
      Column_Str : constant String :=
        Get_Message (Message) (Matches (3).First .. Matches (3).Last);
      Use_Str : constant String :=
        Get_Message (Message) (Matches (4).First .. Matches (4).Last);

      --  Messages that are coming with a semicolo are new messages, other ones
      --  should systematically add use & with clauses.
   begin
      Solutions := Clause_Missing
        (Current_Text,
         Message,
         Pckg_Str,
         With_Str = "with",
         Column_Str /= ";" or else Use_Str = "use");
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Bad_Casing_Standard) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("bad capitalization, mixed case required")));
   end Initialize;

   overriding procedure Fix
     (This         : Bad_Casing_Standard;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Bad_Casing (Current_Text, Message);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Bad_Casing_Declared) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("bad casing of ""([^""]+)"" declared")));
   end Initialize;

   overriding procedure Fix
     (This         : Bad_Casing_Declared;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Bad_Casing
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Bad_Casing_Keyword) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("reserved words must be all lower case")));
   end Initialize;

   overriding procedure Fix
     (This         : Bad_Casing_Keyword;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions :=
        Bad_Casing (Current_Text, Message, Null_Unbounded_String, Lower);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Object_Not_Referenced) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("(procedure|variable|constant|parameter|type|literal|" &
                     "named number|unit|discriminant|) ?""([\w]+)""" &
                     " is not referenced")),
         new Pattern_Matcher'
           (Compile ("(function) ""(""?[^""]+""?)"" is not referenced")));
   end Initialize;

   overriding procedure Fix
     (This         : Object_Not_Referenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This);

      Message : constant Error_Message := Get_Message (Message_It);
      First_Word     : constant String := Get_Message (Message)
        (Matches (1).First .. Matches (1).Last);
      Category       : Language_Category;
      Operation_Mask : Useless_Entity_Operations;

   begin
      if First_Word = "procedure" then
         Category := Cat_Procedure;
         Operation_Mask := Options.Remove_Policy or Add_Pragma_Unreferenced;
      elsif First_Word = "function" then
         Category := Cat_Function;
         Operation_Mask := Options.Remove_Policy or Add_Pragma_Unreferenced;
      elsif First_Word = "variable" then
         Category := Cat_Variable;
         Operation_Mask := Options.Remove_Policy;
      elsif First_Word = "constant" then
         Category := Cat_Variable;
         Operation_Mask := Options.Remove_Policy or Add_Pragma_Unreferenced;
      elsif First_Word = "named number" then
         Category := Cat_Variable;
         Operation_Mask := Options.Remove_Policy;
      elsif First_Word = "parameter" then
         Category := Cat_Parameter;
         Operation_Mask := Add_Pragma_Unreferenced;
      elsif First_Word = "discriminant" then
         Category := Cat_Discriminant;
         Operation_Mask := Add_Pragma_Unreferenced;
      elsif First_Word = "literal" then
         Category := Cat_Literal;
         Operation_Mask := Add_Pragma_Unreferenced;
      elsif First_Word = "type" then
         Category := Cat_Type;
         Operation_Mask := Options.Remove_Policy or Add_Pragma_Unreferenced;
      elsif First_Word = "unit" then
         Category := Cat_With;
         Operation_Mask := Options.Remove_Policy;
      else
         Category := Cat_Unknown;
         Operation_Mask := Options.Remove_Policy or Add_Pragma_Unreferenced;
      end if;

      begin
         Solutions := Not_Referenced
           (Current_Text,
            Message,
            Category,
            To_Unbounded_String
              (Get_Message (Message) (Matches (2).First .. Matches (2).Last)),
            Operation_Mask);
      exception
         when Codefix_Panic =>
            --  This could happen on some error messages, for example when
            --  a generic parameter is not referenced, since GPS is not yet
            --  able to parse such entities.

            raise Uncorrectable_Message;
      end;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Pkg_Not_Referenced) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("unit ""([^""]+)"" is not referenced")),
         new Pattern_Matcher'
           (Compile ("no entities of ""([^""]+)"" are referenced")),
         new Pattern_Matcher'
           (Compile ("unit ""([^""]+)"" is never instantiated")));
   end Initialize;

   overriding procedure Fix
     (This         : Pkg_Not_Referenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Not_Referenced
        (Current_Text,
         Message,
         Cat_Unknown, -- could be Cat_Package or Cat_With
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         Options.Remove_Policy);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Never_Read) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("variable ""([^""]+)"" is assigned but never read")));
   end Initialize;

   overriding procedure Fix
     (This         : Never_Read;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Not_Referenced
        (Current_Text,
         Message,
         Cat_Variable,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         Add_Pragma_Unreferenced);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Never_Assigned) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile
              ("variable ""([^""]+)"" is never read and never assigned")));
   end Initialize;

   overriding procedure Fix
     (This         : Never_Assigned;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Not_Referenced
        (Current_Text,
         Message,
         Cat_Variable,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         Options.Remove_Policy);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Pragma_Missplaced) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("pragma must be first line of file")));
   end Initialize;

   overriding procedure Fix
     (This         : Pragma_Missplaced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := First_Line_Pragma (Current_Text, Message);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Useless_Pragma_Pack) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("pragma Pack has no effect")));
   end Initialize;

   overriding procedure Fix
     (This         : Useless_Pragma_Pack;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Remove_Statement (Current_Text, Get_Message (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Constant_Expected) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("""([\w]+)"" is not modified, could be declared const")));
   end Initialize;

   overriding procedure Fix
     (This         : Constant_Expected;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Not_Modified
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Suspicious_Renaming) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("suggest using an initialized constant object instead")));
   end Initialize;

   overriding procedure Fix
     (This         : Suspicious_Renaming;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Renames_To_Constant
        (Current_Text, Get_Message (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Possible_Interpretation) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile
              ("ambiguous expression \(cannot resolve """"?([^""]+)""""?")));
   end Initialize;

   overriding procedure Free (This : in out Possible_Interpretation) is
   begin
      Free (Error_Parser (This));
      Free (This.Source_Matcher);
      Free (This.Local_Matcher);
   end Free;

   overriding procedure Fix
     (This         : Possible_Interpretation;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options);

      Message : constant Error_Message := Get_Message (Message_It);
      Matches_Prev    : Match_Array (0 .. 2);
      Preview         : Error_Message;
      Cursor_List     : Cursor_Lists.Vector;
      Next_Message    : Error_Message_Iterator := Message_It;
   begin
      loop
         declare
            Solution_Cursor : File_Cursor;
         begin
            Next_Message := Next (Next_Message);

            exit when At_End (Next_Message);
            Preview := Get_Message (Next_Message);
            Match
              (This.Source_Matcher.all,
               Get_Message (Preview),
               Matches_Prev);

            if Matches_Prev (0) = No_Match then
               Match
                 (This.Local_Matcher.all, Get_Message (Preview), Matches_Prev);

               exit when Matches_Prev (0) = No_Match;

               Set_File (Solution_Cursor, Get_File (Message));
               Set_Location
                 (Solution_Cursor,
                  Line => Integer'Value
                    (Get_Message (Preview)
                       (Matches_Prev (1).First .. Matches_Prev (1).Last)),
                  Column => 1);
            else
               Set_File
                 (Solution_Cursor,
                  Get_Registry (Current_Text).Tree.Create
                    (+Get_Message (Preview)
                       (Matches_Prev (1).First .. Matches_Prev (1).Last)));
               Set_Location
                 (Solution_Cursor,
                  Line => Integer'Value
                    (Get_Message (Preview)
                       (Matches_Prev (2).First .. Matches_Prev (2).Last)),
                  Column => 1);
            end if;

            Append (Cursor_List, Solution_Cursor);
            Cancel_Message (Next_Message);
         end;

      end loop;

      Solutions := Resolve_Ambiguity
        (Current_Text,
         Message,
         Cursor_List,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));

      Cursor_List.Clear;

      if Length (Solutions) = 0 then
         raise Uncorrectable_Message;
      end if;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Hidden_Declaration) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
         (Compile ("""""?([^""]+)""""? is not visible")));
   end Initialize;

   overriding procedure Free (This : in out Hidden_Declaration) is
   begin
      Free (Error_Parser (This));
      Free (This.Check_Possible);
      Free (This.Get_From_Current_File);
      Free (This.Get_From_Other_File);
   end Free;

   overriding procedure Fix
     (This         : Hidden_Declaration;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options);
      Matches_Check : Match_Array (0 .. 0);
      Matches_Loc   : Match_Array (0 .. 2);
      Preview       : Error_Message;
      Cursor_List   : Cursor_Lists.Vector;
      Next_Message  : Error_Message_Iterator;
      Message       : constant Error_Message := Get_Message (Message_It);

   begin
      Next_Message := Next (Message_It);

      if At_End (Next_Message) then
         raise Uncorrectable_Message;
      end if;

      Preview := Get_Message (Next_Message);

      Match (This.Check_Possible.all, Get_Message (Preview), Matches_Check);

      if Matches_Check (0) = No_Match then
         raise Uncorrectable_Message;
      end if;

      Cancel_Message (Next_Message);
      Free (Preview);

      loop
         declare
            Solution_Cursor : File_Cursor;
         begin
            Next_Message := Next (Next_Message);

            exit when At_End (Next_Message);

            Preview := Get_Message (Next_Message);

            Match
              (This.Get_From_Other_File.all,
               Get_Message (Preview),
               Matches_Loc);

            if Matches_Loc (0) = No_Match then
               Match
                 (This.Get_From_Current_File.all,
                  Get_Message (Preview),
                  Matches_Loc);

               exit when Matches_Loc (0) = No_Match;

               Set_File (Solution_Cursor, Get_File (Message));
               Set_Location
                 (Solution_Cursor,
                  Line => Integer'Value
                    (Get_Message (Preview)
                       (Matches_Loc (1).First .. Matches_Loc (1).Last)),
                  Column => 1);
            else
               Set_File
                 (Solution_Cursor,
                  Get_Registry (Current_Text).Tree.Create
                    (+Get_Message (Preview)
                       (Matches_Loc (1).First .. Matches_Loc (1).Last)));
               Set_Location
                 (Solution_Cursor,
                  Line => Integer'Value
                    (Get_Message (Preview)
                       (Matches_Loc (2).First .. Matches_Loc (2).Last)),
                  Column => 1);
            end if;

            Append (Cursor_List, Solution_Cursor);

            Cancel_Message (Next_Message);
         end;
      end loop;

      Solutions := Resolve_Ambiguity
        (Current_Text,
         Message,
         Cursor_List,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)));

      if Length (Solutions) = 0 then
         raise Uncorrectable_Message;
      end if;

   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Use_Missing) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("operator for type ""[^""]*"" defined at "
            & "([^\:]+):([0-9]+) is not directly visible")));
   end Initialize;

   overriding procedure Fix
     (This         : Use_Missing;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Solutions);

      Message : constant Error_Message := Get_Message (Message_It);
      Preview  : Error_Message;
      Decl_Cur : File_Cursor;

      Next_Message : Error_Message_Iterator;
   begin
      Next_Message := Next (Message_It);

      if At_End (Next_Message) then
         raise Uncorrectable_Message;
      end if;

      Preview := Get_Message (Next_Message);

      if Get_Message (Preview) /= "use clause would make operation legal" then
         raise Uncorrectable_Message;
      end if;

      Cancel_Message (Next_Message);
      Free (Preview);

      Set_File
        (Decl_Cur,
         Get_Registry (Current_Text).Tree.Create
           (+Get_Message (Message) (Matches (1).First .. Matches (1).Last)));

      Set_Line (Decl_Cur, Integer'Value (Get_Message (Message)
        (Matches (2).First .. Matches (2).Last)));

      Set_Column (Decl_Cur, 1);

      Solutions :=
         Clause_Missing
           (Current_Text   => Current_Text,
            Cursor         => Message,
            Missing_Clause =>
              To_Unbounded_String (Get_Full_Prefix (Current_Text, Decl_Cur)),
            Add_With       => False,
            Add_Use        => True);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Redundant_Conversion) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("useless conversion, ""([^""])"" has this type")),
         2 => new Pattern_Matcher'
           (Compile ("redundant conversion, ""([^""])"" is of type")),
         3 => new Pattern_Matcher'
           (Compile
             ("redundant conversion, expression is of type ""([^""]+)""")));
   end Initialize;

   overriding procedure Fix
     (This         : Redundant_Conversion;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Conversion
        (Current_Text,
         Message,
         To_Unbounded_String
           ("Remove useless conversion of """
            & Get_Message (Message)
            (Matches (1).First .. Matches (1).Last) & """"));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Useless_Abs) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile
              ("abs applied to known non-negative value has no effect")));
   end Initialize;

   overriding
   procedure Fix
     (This         : Useless_Abs;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Conversion
        (Current_Text,
         Message,
         To_Unbounded_String ("Remove abs operator"));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Missplaced_With) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("with clause can be moved to body")),
         new Pattern_Matcher'
           (Compile ("with clause might be moved to body")));
   end Initialize;

   overriding procedure Fix
     (This         : Missplaced_With;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Move_With_To_Body (Current_Text, Message);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Not_Fully_Conformant) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("not fully conformant with declaration at " &
                     "([^\:]+):([\d]+)")),
         new Pattern_Matcher'
           (Compile ("not fully conformant with declaration at " &
                     "(line) ([\d]+)")),
         new Pattern_Matcher'
           (Compile ("not type conformant with declaration at " &
                     "([^\:]+):([\d]+)")),
         new Pattern_Matcher'
           (Compile ("not type conformant with declaration at " &
                     "(line) ([\d]+)")));
   end Initialize;

   overriding procedure Fix
     (This         : Not_Fully_Conformant;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Spec_Cursor : File_Cursor;
      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Set_Location
        (Spec_Cursor,
         Line => Natural'Value
           (Get_Message (Message)
              (Matches (2).First .. Matches (2).Last)),
         Column => 1);

      if Get_Message (Message)
        (Matches (1).First .. Matches (1).Last) = "line"
      then
         Set_File (Spec_Cursor, Get_File (Message));
      else
         Set_File
           (Spec_Cursor,
            Get_Registry (Current_Text).Tree.Create
             (+Get_Message (Message) (Matches (1).First .. Matches (1).Last)));
      end if;

      Solutions := Make_Conformant (Current_Text, Message, Spec_Cursor);

      Free (Spec_Cursor);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Generic_Use_Unallowed) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("a generic package is not allowed in a use clause")));
   end Initialize;

   overriding procedure Fix
     (This         : Generic_Use_Unallowed;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Matches, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Dependency_Clause
        (Current_Text, Message, Cat_Use, Before);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Non_Visible_Declaration) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("""""?([^""]+)""""? is not visible")));
   end Initialize;

   overriding procedure Fix
     (This         : Non_Visible_Declaration;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options, Matches);

      Next_Message  : Error_Message_Iterator := Message_It;
      Message       : constant Error_Message := Get_Message (Message_It);

   begin
      Solutions := Null_Solution_List;

      loop
         declare
            Preview         : Error_Message;
            Matches_Loc     : Match_Array (0 .. 2);
            Solution_Cursor : File_Cursor;
            Seek_With       : Boolean;
            Resolve_List    : Solution_List;
         begin
            Next_Message := Next (Next_Message);

            exit when At_End (Next_Message);

            Preview := Get_Message (Next_Message);

            Match
              (This.Get_From_Other_File.all,
               Get_Message (Preview),
               Matches_Loc);

            if Matches_Loc (0) = No_Match then
               Match
                 (This.Get_From_Current_File.all,
                  Get_Message (Preview),
                  Matches_Loc);

               exit when Matches_Loc (0) = No_Match;

               Seek_With := False;

               Set_File (Solution_Cursor, Get_File (Message));
            else
               Seek_With := True;

               Set_File
                 (Solution_Cursor,
                  Get_Registry (Current_Text).Tree.Create
                    (+Get_Message (Preview)
                       (Matches_Loc (1).First .. Matches_Loc (1).Last)));
            end if;

            Set_Location
              (Solution_Cursor,
               Line => Integer'Value
                 (Get_Message (Preview)
                    (Matches_Loc (2).First .. Matches_Loc (2).Last)),
               Column => 1);

            Resolve_List := Resolve_Unvisible_Declaration
              (Current_Text, Message, Solution_Cursor, Seek_With);

            Unique_Concat (Solutions, Resolve_List);

            Cancel_Message (Next_Message);
            Free (Solution_Cursor);
            Free_List (Resolve_List);
         end;
      end loop;

      if Length (Solutions) = 0 then
         raise Uncorrectable_Message;
      end if;

   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Consecutive_Underlines) is
   begin
      This.Matcher := (1 => new Pattern_Matcher'
        (Compile ("two consecutive underlines not permitted")));
   end Initialize;

   overriding procedure Fix
     (This         : Consecutive_Underlines;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Remove_Extra_Underlines (Current_Text, Message);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Multiple_Blank_Lines) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'(Compile ("multiple blank lines")));
   end Initialize;

   overriding procedure Fix
     (This         : Multiple_Blank_Lines;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);

      First_Line_To_Remove : File_Cursor :=
        File_Cursor (Get_Message (Message_It));
   begin
      First_Line_To_Remove.Set_Location
        (First_Line_To_Remove.Get_Line + 1,
         1);

      Solutions := Remove_Blank_Lines
        (Current_Text, First_Line_To_Remove);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out EOF_Blank_Lines) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("blank lines not allowed at end of file")));
   end Initialize;

   overriding procedure Fix
     (This         : EOF_Blank_Lines;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);

      First_Line_To_Remove : File_Cursor :=
        File_Cursor (Get_Message (Message_It));
   begin
      First_Line_To_Remove.Set_Location
        (First_Line_To_Remove.Get_Line + 1,
         1);

      Solutions := Remove_Blank_Lines
        (Current_Text, First_Line_To_Remove);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Suggested_Replacement) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("suggested replacement: ""([^""]*)""")));
   end Initialize;

   overriding procedure Fix
     (This         : Suggested_Replacement;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);
      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Should_Be
        (Current_Text,
         Message,
         To_Unbounded_String
           (Get_Message (Message) (Matches (1).First .. Matches (1).Last)),
         To_Unbounded_String ("([\w]+)"),
         Regular_Expression);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Pragma_Pack) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("size given for ""([^""]+)"" too small")));
   end Initialize;

   overriding
   procedure Free (This : in out Pragma_Pack) is
   begin
      Free (This.Use_Pragma);
      Free (Error_Parser (This));
   end Free;

   overriding
   procedure Fix
     (This         : Pragma_Pack;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (Options);
      Message : constant Error_Message := Get_Message (Message_It);

      Use_Pragma_Match : Match_Array (0 .. 0);
   begin
      if not At_End (Next (Message_It)) then
         Match
           (This.Use_Pragma.all,
            Get_Message (Get_Message (Next (Message_It))),
            Use_Pragma_Match);

         if Use_Pragma_Match (0) /= No_Match then
            Solutions := Expected
              (Current_Text    => Current_Text,
               Message         => Message,
               String_Expected =>
                 To_Unbounded_String
                   ("pragma Pack ("
                    & Get_Message (Message)
                    (Matches (1).First .. Matches (1).Last) & ");"),
               Position        => After);
         end if;
      end if;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Undefined_Entity) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("""([^""]+)"" is undefined")));
   end Initialize;

   ---------
   -- Fix --
   ---------

   overriding procedure Fix
     (This         : Undefined_Entity;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Solutions);

      Message        : Error_Message renames Get_Message (Message_It);
      Entity_Name    : GNAT.Strings.String_Access := new String'
        (Get_Message (Message_It).Get_Message
        (Matches (1).First .. Matches (1).Last));
      Expression     : Parsed_Expression;
      List           : Entity_List;
      It             : Entity_Iterator;
      Entity         : Entity_Access;
      Correct_Entity : Entity_Access := Null_Entity_Access;

      File   : constant Structured_File_Access :=
        Current_Text.Get_Structured_File (Message.Get_File);
      Entity_Declaration : File_Cursor;
   begin
      if not At_End (Next (Message_It)) then
         declare
            Next_Message : constant Error_Message :=
              Get_Message (Next (Message_It));
         begin
            if Next_Message.Get_File = Message.Get_File
              and then Next_Message.Get_Line = Message.Get_Line
              and then Next_Message.Get_Column = Message.Get_Column
            then
               --  In this case, the compiler has additional information for
               --  that location, and potentially proposal for fixes. Don't
               --  analyse the database in that case.

               return;
            end if;
         end;
      end if;

      Expression := Parse_Expression_Backward
        (Entity_Name, String_Index_Type (Entity_Name'Last));

      List := Find_Declarations
        (Context    =>
           (From_File,
            Null_Instance_Info,
            File,
            --  Retreive the offset where to start looking from. Line offset
            --  should be precise enough, no need to retreive the exact
            --  location of the error message.
            Get_Offset_Of_Line (File, Message.Get_Line)),
         Expression => Expression,
         Is_Partial => False);

      It := First (List);

      while not At_End (It) loop
         Entity := Get_Entity (It);

         if Get_First_Occurence (Entity) = Entity then
            if Correct_Entity = Null_Entity_Access then
               Correct_Entity := Entity;
            else
               Correct_Entity := Null_Entity_Access;
               exit;
            end if;
         end if;

         Next (It);
      end loop;
      Free (It);

      if Correct_Entity /= Null_Entity_Access then
         Set_File
           (Entity_Declaration,
            Get_File_Path (Get_File (Correct_Entity)));
         Set_Line
           (Entity_Declaration,
            Get_Construct (Correct_Entity).Sloc_Start.Line);
         Set_Column
           (Entity_Declaration,
            --  This is not exact - we're actually retreiving a char index
            --  here. But for the purpose of retreiving the enclosing package
            --  later on, that's good enough.
            Visible_Column_Type
              (Get_Construct (Correct_Entity).Sloc_Start.Column));

         Solutions := Resolve_Unvisible_Declaration
           (Current_Text,
            Message,
            Entity_Declaration,
            True);
      end if;

      Free (Expression);
      Free (Entity_Name);
      Free (List);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Unwanted_Pragma_Unreferenced) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("pragma Unreferenced given for ""([^""]+)""")));
   end Initialize;

   overriding
   procedure Fix
     (This         : Unwanted_Pragma_Unreferenced;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);
   begin
      Solutions := Remove_Element_From_Unreferenced_Pragma
        (Current_Text  => Current_Text,
         Object_Cursor => Get_Message (Message_It),
         Object_Name   => Get_Message
           (Message_It).Get_Message (Matches (1).First .. Matches (1).Last));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Wrong_Index_Usage) is
   begin
      This.Matcher :=
        (new Pattern_Matcher'
           (Compile ("index number (not allowed) for one dimensional array")),
         new Pattern_Matcher'
           (Compile ("index number (required) for multi-dimensional array")));
   end Initialize;

   overriding
   procedure Fix
     (This         : Wrong_Index_Usage;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Mode : constant String := Get_Message
        (Message_It).Get_Message (Matches (1).First .. Matches (1).Last);
   begin
      if Mode = "not allowed" then
         Solutions := Fix_Index_Number
           (Current_Text, Get_Message (Message_It), True);
      else
         Solutions := Fix_Index_Number
           (Current_Text, Get_Message (Message_It), False);
      end if;
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Wrong_Sb_Order) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("subprogram body ""[^""]+"" not in alphabetical order")));
   end Initialize;

   overriding procedure Fix
     (This         : Wrong_Sb_Order;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions := Reorder_Subprogram
        (Current_Text, Get_Message (Message_It));
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : in out No_Statement_Following_Then)
   is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("no statements may follow ""then"" on same line")));
   end Initialize;

   overriding procedure Fix
     (This         : No_Statement_Following_Then;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options, Matches);
   begin
      Solutions :=
        Add_Line
          (Current_Text,
           Get_Message (Message_It),
           Null_Unbounded_String,
           True);
   end Fix;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Elaborate_All_Required) is
   begin
      This.Matcher :=
        (1 => new Pattern_Matcher'
           (Compile ("Elaborate_All pragma required for ""([^""]+)""")));
   end Initialize;

   ---------
   -- Fix --
   ---------

   overriding procedure Fix
     (This         : Elaborate_All_Required;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_It   : Error_Message_Iterator;
      Options      : Fix_Options;
      Solutions    : out Solution_List;
      Matches      : Match_Array)
   is
      pragma Unreferenced (This, Options);

      Message : constant Error_Message := Get_Message (Message_It);
   begin
      Solutions := Add_Elaborate_All
        (Current_Text,
         Message,
         Get_Message (Message) (Matches (1).First .. Matches (1).Last));
   end Fix;

   ----------------------
   -- Register_Parsers --
   ----------------------

   procedure Register_Parsers (Processor : in out Fix_Processor) is
   begin
      Add_Parser (Processor, new Agregate_Misspelling);
      Add_Parser (Processor, new Double_Misspelling);
      Add_Parser (Processor, new Light_Misspelling);
      Add_Parser (Processor, new Goto_Misspelling);
      Add_Parser (Processor, new Library_Misspelling);
      Add_Parser (Processor, new Sth_Should_Be_Sth);
      Add_Parser (Processor, new Should_Be_Semicolon);
      Add_Parser (Processor, new And_Meant);
      Add_Parser (Processor, new Or_Meant);
      Add_Parser (Processor, new Short_Circuit_Required);
      Add_Parser (Processor, new Bad_End_Block);
      Add_Parser (Processor, new Unqualified_Expression);
      Add_Parser (Processor, new Goes_Before);
      Add_Parser (Processor, new Sth_Expected_3);
      Add_Parser (Processor, new Sth_Expected_2);
      Add_Parser (Processor, new Sth_Expected);
      Add_Parser (Processor, new Missing_Kw);
      Add_Parser (Processor, new Missing_Sep);
      Add_Parser (Processor, new Missing_All);
      Add_Parser (Processor, new Implicit_Dereference);
      Add_Parser (Processor, new Statement_Missing);
      Add_Parser (Processor, new Useless_Assignment);
      Add_Parser (Processor, new Space_Missing);
      Add_Parser (Processor, new Two_Spaces_Missing);
      Add_Parser (Processor, new Name_Missing);
      Add_Parser (Processor, new Double_Keyword);
      Add_Parser (Processor, new Extra_Paren);
      Add_Parser (Processor, new Redudant_Paren);
      Add_Parser (Processor, new No_Space_Allowed);
      Add_Parser (Processor, new Redundant_Keyword);
      Add_Parser (Processor, new Redundant_Attribute);
      Add_Parser (Processor, new Redundant_Comparison);
      Add_Parser (Processor, new Unexpected_Sep);
      Add_Parser (Processor, new Unexpected_Word);
      Add_Parser (Processor, new Kw_Not_Allowed);
      Add_Parser (Processor, new Sep_Not_Allowed);
      Add_Parser (Processor, new In_Should_Be_Omitted);
      Add_Parser (Processor, new Out_Should_Be_Omitted);
      Add_Parser (Processor, new Already_Use_Visible);
      Add_Parser (Processor, new Use_Valid_Instead);
      Add_Parser (Processor, new Should_Be_In);
      Add_Parser (Processor, new Bad_Column);
      Add_Parser (Processor, new Main_With_Missing);
      Add_Parser (Processor, new Bad_Casing_Standard);
      Add_Parser (Processor, new Bad_Casing_Declared);
      Add_Parser (Processor, new Bad_Casing_Keyword);
      Add_Parser (Processor, new Object_Not_Referenced);
      Add_Parser (Processor, new Pkg_Not_Referenced);
      Add_Parser (Processor, new Never_Read);
      Add_Parser (Processor, new Never_Assigned);
      Add_Parser (Processor, new Pragma_Missplaced);
      Add_Parser (Processor, new Useless_Pragma_Pack);
      Add_Parser (Processor, new Constant_Expected);
      Add_Parser (Processor, new Suspicious_Renaming);
      Add_Parser (Processor, new Possible_Interpretation);
      Add_Parser (Processor, new Hidden_Declaration);
      Add_Parser (Processor, new Redundant_Conversion);
      Add_Parser (Processor, new Useless_Abs);
      Add_Parser (Processor, new Missplaced_With);
      Add_Parser (Processor, new Not_Fully_Conformant);
      Add_Parser (Processor, new Generic_Use_Unallowed);
      Add_Parser (Processor, new Use_Missing);
      Add_Parser (Processor, new Non_Visible_Declaration);
      Add_Parser (Processor, new Redundant_With_In_Body);
      Add_Parser (Processor, new Redundant_With);
      Add_Parser (Processor, new Consecutive_Underlines);
      Add_Parser (Processor, new Multiple_Blank_Lines);
      Add_Parser (Processor, new EOF_Blank_Lines);
      Add_Parser (Processor, new Suggested_Replacement);
      Add_Parser (Processor, new Pragma_Pack);
      Add_Parser (Processor, new Undefined_Entity);
      Add_Parser (Processor, new Unwanted_Pragma_Unreferenced);
      Add_Parser (Processor, new Wrong_Index_Usage);
      Add_Parser (Processor, new Wrong_Sb_Order);
      Add_Parser (Processor, new No_Statement_Following_Then);
      Add_Parser (Processor, new Elaborate_All_Required);

      --  GNATCheck parsers

      Add_Parser (Processor, new GnatCheck_Missing_Storage_Order);
   end Register_Parsers;

end Codefix.GNAT_Parser;
