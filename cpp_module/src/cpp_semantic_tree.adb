------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Language.Cpp; use Language.Cpp;

package body Cpp_Semantic_Tree is

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   function Parse_Expression_Backward
     (Buffer       : access constant Glib.UTF8_String;
      Start_Offset : String_Index_Type;
      End_Offset   : String_Index_Type := 0)
      return Parsed_Expression
   is
      use Token_List;

      Result           : Parsed_Expression;
      Expression_Depth : Natural := 0;
      Is_First_Token   : Boolean := True;

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean);

      ------------------
      -- Handle_Token --
      ------------------

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean) is
         Tok_Typ : constant Token_Type := Token.Tok_Type;

      begin
         case Tok_Typ is
            when Language.Cpp.Tok_Identifier  |
                 Language.Cpp.Tok_Dot         |
                 Language.Cpp.Tok_Scope       |
                 Language.Cpp.Tok_Dereference =>
               Prepend (Result.Tokens, Token);

            when Language.Cpp.Tok_Left_Paren =>
               if Is_First_Token then
                  Prepend (Result.Tokens, Token);
               else
                  Stop := True;
               end if;

            when Language.Cpp.Tok_Right_Sq_Bracket =>
               Prepend (Result.Tokens, Token);
               Expression_Depth := Expression_Depth + 1;

            when Language.Cpp.Tok_Left_Sq_Bracket =>
               Prepend (Result.Tokens, Token);
               Expression_Depth := Expression_Depth - 1;

            when others =>
               if Expression_Depth > 0 then
                  Prepend (Result.Tokens, Token);
               else
                  Stop := True;
               end if;

         end case;

         Is_First_Token := False;
      end Handle_Token;

   begin
      Cpp_Lang.Parse_Tokens_Backwards
         (Buffer      => Buffer.all,
         Start_Offset => Start_Offset,
         End_Offset   => End_Offset,
         Callback     => Handle_Token'Access);

      return Result;
   end Parse_Expression_Backward;

   ----------
   -- Free --
   ----------

   procedure Free (Expression : in out Parsed_Expression) is
   begin
      Expression.Tokens.Clear;
   end Free;

end Cpp_Semantic_Tree;
