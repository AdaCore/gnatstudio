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

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Completion.Expression_Parser is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Token_Record) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Buffer : String; Token : Token_Record) return String is
   begin
      return Buffer (Token.Token_Name_First .. Token.Token_Name_Last);
   end Get_Name;

   ------------------------
   -- Parse_Current_List --
   ------------------------

   function Parse_Current_List
     (Buffer : String; Start_Offset : Natural) return Token_List.List
   is
      Offset : Natural := Start_Offset;
      Token  : Token_Record;
      Result : Token_List.List;

      procedure Skip_Expression (Offset : in out Natural);
      procedure Skip_String (Offset : in out Natural);
      procedure Skip_Comment_Line (Offset : in out Natural);
      procedure Push_Pckg (Offset : in out Natural);

      procedure Push (Token : in out Token_Record);
      procedure Pop;

      ---------------------
      -- Skip_Expression --
      ---------------------

      procedure Skip_Expression (Offset : in out Natural) is
         Local_Token : Token_Record := Token;
      begin
         Local_Token.Number_Of_Parameters := 1;

         while Offset > 0 loop
            case Buffer (Offset) is
               when ')' =>
                  Offset := Offset - 1;
                  Skip_Expression (Offset);

                  --  We don't want to store sub expressions
                  Pop;

               when ',' =>
                  Local_Token.Number_Of_Parameters :=
                    Local_Token.Number_Of_Parameters + 1;

               when '"' =>
                  Offset := Offset - 1;
                  Skip_String (Offset);

               when ''' =>
                  if Offset - 2 > 0 then
                     Offset := Offset - 2;
                  else
                     raise Constraint_Error;
                  end if;

               when '(' =>
                  Local_Token.Tok_Type := Tok_Expression;
                  Push (Local_Token);
                  Offset := Offset - 1;
                  exit;

               when ASCII.LF =>
                  Skip_Comment_Line (Offset);

               when others =>
                  null;
            end case;

            Offset := Offset - 1;

         end loop;
      end Skip_Expression;

      -----------------
      -- Skip_String --
      -----------------

      procedure Skip_String (Offset : in out Natural) is
      begin
         while Offset > 0 loop
            case Buffer (Offset) is
               when '"' =>
                  if Offset > 0 then

                     if Buffer (Offset - 1) /= '"' then
                        exit;
                     else
                        Offset := Offset - 1;
                     end if;
                  end if;

               when others =>
                  null;
            end case;

            Offset := Offset - 1;

         end loop;
      end Skip_String;

      -----------------------
      -- Skip_Comment_Line --
      -----------------------

      procedure Skip_Comment_Line (Offset : in out Natural) is
         Local_Offset : Natural := 0;
      begin
         while Local_Offset > 1 loop
            case Buffer (Local_Offset) is
               when '"' =>
                  Offset := Offset - 1;
                  Skip_String (Local_Offset);

               when '-' =>
                  if Buffer (Local_Offset - 1) = '-' then
                     Local_Offset := Local_Offset - 1;

                     Skip_Comment_Line (Local_Offset);
                     Offset := Local_Offset;
                     exit;
                  end if;

               when others =>
                  null;

            end case;

         end loop;
      end Skip_Comment_Line;

      ---------------
      -- Push_Pckg --
      ---------------

      procedure Push_Pckg (Offset : in out Natural) is
      begin
         if Offset - 3 > 0
           and then To_Lower (Buffer (Offset - 3 .. Offset)) = "with"
         then
            if Offset - 4 <= 0
              or else Buffer (Offset - 4) = ' '
              or else Buffer (Offset - 4) = ASCII.LF
              or else Buffer (Offset - 4) = ASCII.HT
            then
               Token.Tok_Type := Tok_With;
               Push (Token);
            end if;
         elsif Offset - 2 > 0
           and then To_Lower (Buffer (Offset - 3 .. Offset)) = "use"
         then
            if Offset - 3 <= 0
              or else Buffer (Offset - 3) = ' '
              or else Buffer (Offset - 3) = ASCII.LF
              or else Buffer (Offset - 3) = ASCII.HT
            then
               Token.Tok_Type := Tok_Use;
               Push (Token);
            end if;
         end if;
      end Push_Pckg;

      ----------
      -- Push --
      ----------

      procedure Push (Token : in out Token_Record) is
      begin
         if Token /= Null_Token then
            Prepend (Result, Token);
            Token := Null_Token;
         end if;
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop is
      begin
         Remove_Nodes (Result, Token_List.Null_Node, First (Result));
      end Pop;

   begin
      while Offset > 0 loop
         case Buffer (Offset) is
            when ')' =>
               Push (Token);
               Offset := Offset - 1;
               Skip_Expression (Offset);

            when '(' =>
               Push (Token);

            when '.' =>
               Push (Token);
               Token.Tok_Type := Tok_Dot;
               Push (Token);

            when ',' =>
               Push (Token);

               if Length (Result) = 0 then
                  Offset := Offset - 1;
                  Skip_Expression (Offset);
               else
                  exit;
               end if;

            when ' ' | ASCII.HT | ASCII.CR =>
               Push (Token);

            when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' =>
               Token.Tok_Type := Tok_Identifier;
               Token.Token_Name_First := Offset;

               if Token.Token_Name_Last = 0 then
                  Token.Token_Name_Last := Offset;

                  if Length (Result) > 0
                    and then Head (Result).Tok_Type = Tok_Identifier
                  then
                     if To_Lower (Get_Name (Buffer, Token)) = "all" then
                        Token.Tok_Type := Tok_All;
                     end if;

                     Push_Pckg (Offset);
                     Token := Null_Token;
                     exit;
                  end if;
               end if;

            when ASCII.LF =>
               Skip_Comment_Line (Offset);

            when others =>
               Push (Token);
               exit;

         end case;

         Offset := Offset - 1;
      end loop;

      Push (Token);
      return Result;
   end Parse_Current_List;

end Completion.Expression_Parser;
