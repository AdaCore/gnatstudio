-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

--  This package is used to provide an easy way to analyze expression that have
--  to be completed for Ada

with Generic_List;

package Completion.Expression_Parser is

   type Token_Type is
     (No_Token,
      Tok_Dot,
      Tok_Open_Parenthesis,
      Tok_Identifier,
      Tok_Expression,
      Tok_With,
      Tok_Use,
      Tok_All,
      Tok_List_Item);
   --  Types of tokens that are found on the exporession to be completed

   type Token_Record is record
      Tok_Type             : Token_Type := No_Token;

      Number_Of_Parameters : Natural := 0;
      --  Only needed if the identifier of the token is a subprogram call.
      --  Otherwise, it will always be equal to 0.

      Token_Name_First     : Natural := 0;
      Token_Name_Last      : Natural := 0;
   end record;
   --  This record holds the informations concerning one very token

   procedure Free (This : in out Token_Record);
   --  Used to instantiate the generic list, does not actually do anything

   Null_Token : constant Token_Record;

   package Token_List is new Generic_List (Token_Record);
   use Token_List;

   function Parse_Current_List
     (Buffer : UTF8_String; Start_Offset : Natural) return Token_List.List;
   --  This function looks backwards from the offset given in parameter and
   --  parses the relevant completion expression.
   --  Start_Offset is the offset (in byte) of where we have to look

   function Get_Name (Buffer : String; Token : Token_Record) return String;
   --  Return the name of the element pointed by the token

private

   Null_Token : constant Token_Record :=
     (Tok_Type             => No_Token,
      Number_Of_Parameters => 0,
      Token_Name_First     => 0,
      Token_Name_Last      => 0);

end Completion.Expression_Parser;
