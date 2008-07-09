-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

--  This package provides a backwards expression parser

with Glib;         use Glib;

with Generic_List;

package Ada_Semantic_Tree.Expression_Parser is

   type Token_Type is
     (No_Token,
      Tok_Dot,
      Tok_Open_Parenthesis,
      Tok_Close_Parenthesis,
      Tok_Identifier,
      Tok_Expression,
      Tok_With,
      Tok_Use,
      Tok_All,
      Tok_Arrow);
   --  Types of tokens that are found on the expression

   type Token_Record is record
      Tok_Type    : Token_Type := No_Token;
      Token_First : Natural := 0;
      Token_Last  : Natural := 0;
   end record;
   --  This record holds the informations concerning one very token

   procedure Free (This : in out Token_Record);
   --  Used to instantiate the generic list, does not actually do anything

   Null_Token : constant Token_Record;

   package Token_List is new Generic_List (Token_Record);
   use Token_List;

   type UTF8_String_Access is access all UTF8_String;

   type Parsed_Expression is record
      Original_Buffer : UTF8_String_Access;
      Tokens : Token_List.List := Token_List.Null_List;
   end record;

   Null_Parsed_Expression : constant Parsed_Expression :=
     (null, Token_List.Null_List);

   function Parse_Current_List
     (Buffer       : UTF8_String_Access;
      Start_Offset : Natural;
      End_Offset   : Natural := 0)
      return Parsed_Expression;
   --  This function looks backwards from the offset given in parameter and
   --  parses the relevant completion expression.
   --  Start_Offset is the offset (in byte) of where we have to look. The
   --  caller is responsible for calling Free on Tokens.
   --  The buffer given in parameter much as a liftime superior or equal to
   --  the resulting parser expression, as it gets referenced by this
   --  expression.

   function Parse_Current_List
     (Buffer : UTF8_String_Access) return Parsed_Expression;
   --  Same as before, but analyzes the whole buffer as an expression (ie
   --  Start_Offset = Buffer'Last.

   function Get_Name
     (Expression : Parsed_Expression; Token : Token_Record) return String;
   --  Return the name of the element pointed by the token

private

   Null_Token : constant Token_Record :=
     (Tok_Type             => No_Token,
      Token_First          => 0,
      Token_Last           => 0);

end Ada_Semantic_Tree.Expression_Parser;
