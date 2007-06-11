-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006-2007                    --
--                                AdaCore                            --
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

with Codefix.Errors_Parser; use Codefix.Errors_Parser;

package body Codefix.Helpers is

   --------------------
   -- Create_Parsers --
   --------------------

   procedure Create_Parsers is
   begin
      Add_Parser (new Agregate_Misspelling);
      Add_Parser (new Double_Misspelling);
      Add_Parser (new Light_Misspelling);
      Add_Parser (new Goto_Misspelling);
      Add_Parser (new Library_Misspelling);
      Add_Parser (new Sth_Should_Be_Sth);
      Add_Parser (new Should_Be_Semicolon);
      Add_Parser (new And_Meant);
      Add_Parser (new Or_Meant);
      Add_Parser (new Bad_End_Block);
      Add_Parser (new Unqualified_Expression);
      Add_Parser (new Goes_Before);
      Add_Parser (new Sth_Expected_3);
      Add_Parser (new Sth_Expected_2);
      Add_Parser (new Sth_Expected);
      Add_Parser (new Missing_Kw);
      Add_Parser (new Missing_Sep);
      Add_Parser (new Missing_All);
      Add_Parser (new Statement_Missing);
      Add_Parser (new Space_Missing);
      Add_Parser (new Two_Spaces_Missing);
      Add_Parser (new Name_Missing);
      Add_Parser (new Double_Keyword);
      Add_Parser (new Extra_Paren);
      Add_Parser (new Redundant_Keyword);
      Add_Parser (new Unexpected_Sep);
      Add_Parser (new Unexpected_Word);
      Add_Parser (new Kw_Not_Allowed);
      Add_Parser (new Sep_Not_Allowed);
      Add_Parser (new Already_Use_Visible);
      Add_Parser (new Use_Valid_Instead);
      Add_Parser (new Should_Be_In);
      Add_Parser (new Bad_Column);
      Add_Parser (new Main_With_Missing);
      Add_Parser (new Bad_Casing_Standard);
      Add_Parser (new Bad_Casing_Declared);
      Add_Parser (new Bad_Casing_Keyword);
      Add_Parser (new Object_Not_Referenced);
      Add_Parser (new Pkg_Not_Referenced);
      Add_Parser (new Never_Read);
      Add_Parser (new Never_Assigned);
      Add_Parser (new Pragma_Missplaced);
      Add_Parser (new Constant_Expected);
      Add_Parser (new Possible_Interpretation);
      Add_Parser (new Hidden_Declaration);
      Add_Parser (new Redundant_Conversion);
      Add_Parser (new Missplaced_With);
      Add_Parser (new Not_Fully_Conformant);
      Add_Parser (new Generic_Use_Unallowed);
      Add_Parser (new Use_Missing);
      Add_Parser (new Non_Visible_Declaration);
      Add_Parser (new Redundant_With_In_Body);
      Add_Parser (new Consecutive_Underlines);
      Initialize_Parsers;
   end Create_Parsers;

end Codefix.Helpers;
