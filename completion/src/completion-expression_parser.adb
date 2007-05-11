-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

with Glib.Unicode;            use Glib.Unicode;

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
      procedure Handle_List_Items (Offset : in out Natural);
      procedure Skip_String (Offset : in out Natural);
      procedure Skip_Comment_Line (Offset : in out Natural);
      procedure Push_Pckg (Offset : in out Natural);
      function Check_Prev_Word (Offset : Positive; Word : String)
         return Boolean;

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
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Skip_Expression (Offset);

                  --  We don't want to store sub expressions
                  Pop;

               when ',' =>
                  Local_Token.Number_Of_Parameters :=
                    Local_Token.Number_Of_Parameters + 1;

               when '"' =>
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Skip_String (Offset);

               when ''' =>
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);

               when '(' =>
                  Local_Token.Tok_Type := Tok_Expression;
                  Push (Local_Token);
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  exit;

               when ASCII.LF =>
                  Skip_Comment_Line (Offset);

               when others =>
                  null;
            end case;

            Offset := UTF8_Find_Prev_Char (Buffer, Offset);

         end loop;
      end Skip_Expression;

      -----------------------
      -- Handle_List_Items --
      -----------------------

      procedure Handle_List_Items (Offset : in out Natural) is
         Local_Token    : Token_Record := Token;
         Possible_Arrow : Boolean := False;
         In_Name        : Boolean := False;
      begin
         Local_Token.Number_Of_Parameters := 1;

         while Offset > 0 loop
            case Buffer (Offset) is
               when ')' =>
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Skip_Expression (Offset);

                  --  We don't want to store sub expressions
                  Pop;

               when '"' =>
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Skip_String (Offset);

               when ''' =>
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);

               when ',' =>
                  Local_Token.Tok_Type := Tok_List_Item;
                  Push (Local_Token);

               when '(' =>
                  Local_Token.Tok_Type := Tok_List_Item;
                  Push (Local_Token);
                  Local_Token.Tok_Type := Tok_Open_Parenthesis;
                  Push (Local_Token);
                  exit;

               when '>' =>
                  Possible_Arrow := True;

               when '=' =>
                  if Possible_Arrow then
                     In_Name := True;
                  end if;

               when ASCII.LF =>
                  Skip_Comment_Line (Offset);

               when others =>
                  null;
            end case;

            exit when Offset = 0;

            if Buffer (Offset) /= '>' then
               Possible_Arrow := False;
            end if;

            if Is_Alnum
              (UTF8_Get_Char
                 (Buffer (Offset .. UTF8_Next_Char (Buffer, Offset))))
              or else Buffer (Offset) = '_'
            then
               if In_Name then
                  Local_Token.Token_Name_First := Offset;

                  if Local_Token.Token_Name_Last = 0 then
                     Local_Token.Token_Name_Last := Offset;
                  end if;
               end if;
            elsif Buffer (Offset) /= ' '
              and then Buffer (Offset) /= ASCII.HT
              and then Buffer (Offset) /= ASCII.CR
              and then Buffer (Offset) /= '='
            then
               In_Name := False;
            end if;

            Offset := UTF8_Find_Prev_Char (Buffer, Offset);

         end loop;
      end Handle_List_Items;

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
                        Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                     end if;
                  end if;

               when others =>
                  null;
            end case;

            Offset := UTF8_Find_Prev_Char (Buffer, Offset);

         end loop;
      end Skip_String;

      -----------------------
      -- Skip_Comment_Line --
      -----------------------

      procedure Skip_Comment_Line (Offset : in out Natural) is
         Local_Offset : Natural := Offset;
      begin
         Local_Offset := UTF8_Find_Prev_Char (Buffer, Local_Offset);

         while Local_Offset > 1 loop
            case Buffer (Local_Offset) is
               when '"' =>
                  Local_Offset := UTF8_Find_Prev_Char (Buffer, Local_Offset);
                  Skip_String (Local_Offset);

               when '-' =>
                  Local_Offset := UTF8_Find_Prev_Char (Buffer, Local_Offset);

                  if Buffer (Local_Offset) = '-' then
                     Local_Offset := UTF8_Find_Prev_Char
                       (Buffer, Local_Offset);

                     Skip_Comment_Line (Local_Offset);
                     Offset := Local_Offset;

                     exit;
                  end if;

               when ASCII.LF =>
                  exit;

               when others =>
                  Local_Offset := UTF8_Find_Prev_Char (Buffer, Local_Offset);

            end case;
         end loop;
      end Skip_Comment_Line;

      ---------------
      -- Push_Pckg --
      ---------------

      procedure Push_Pckg (Offset : in out Natural) is
      begin
         if Check_Prev_Word (Offset, "with") then
            Token.Tok_Type := Tok_With;
            Token.Token_Name_Last := Offset;
            Offset := Offset - 4;
            Token.Token_Name_First := Offset;
            Push (Token);
         elsif Check_Prev_Word (Offset, "use") then
            Token.Tok_Type := Tok_Use;
            Token.Token_Name_Last := Offset;
            Offset := Offset - 3;
            Token.Token_Name_First := Offset;
            Push (Token);
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

      ---------------------
      -- Check_Prev_Word --
      ---------------------

      function Check_Prev_Word (Offset : Positive; Word : String)
         return Boolean is
      begin
         return Offset - (Word'Length - 1) > 0
           and then To_Lower (Buffer (Offset - (Word'Length - 1) .. Offset))
             = To_Lower (Word)
           and then
             (Offset - Word'Length <= 0
              or else Buffer (Offset - Word'Length) = ' '
              or else Buffer (Offset - Word'Length) = ASCII.LF
              or else Buffer (Offset - Word'Length) = ASCII.HT);
      end Check_Prev_Word;

      Blank_Here, Blank_Before : Boolean := False;
      Next_Ind                 : Natural;

   begin
      Skip_Comment_Line (Offset);

      if Offset /= Start_Offset then
         --  In this case, we are on a comment line. So the expression is
         --  empty.

         return Result;
      end if;

      while Offset > 0 loop

         Blank_Here := False;

         case Buffer (Offset) is
            when ')' =>
               Push (Token);
               Offset := UTF8_Find_Prev_Char (Buffer, Offset);
               Skip_Expression (Offset);

            when '(' =>
               Push (Token);

               if Length (Result) = 0 then
                  Token.Tok_Type := Tok_Open_Parenthesis;
                  Token.Number_Of_Parameters := 0;
                  Push (Token);
               end if;

            when '.' =>
               Push (Token);

               if Length (Result) > 0
                 and then Head (Result).Tok_Type = Tok_Dot
               then
                  --  In this case, we have a range construct, like A .. B.
                  --  It's the end of the expression.

                  Pop;

                  exit;
               end if;

               Token.Tok_Type := Tok_Dot;
               Push (Token);

            when ',' =>
               Push (Token);

               if Length (Result) = 0 then
                  Offset := UTF8_Find_Prev_Char (Buffer, Offset);
                  Handle_List_Items (Offset);
               else
                  exit;
               end if;

            when ' ' | ASCII.HT | ASCII.CR =>
               Push (Token);
               Blank_Here := True;

            when ASCII.LF =>
               Skip_Comment_Line (Offset);

            when others =>
               Next_Ind := UTF8_Next_Char (Buffer, Offset);

               if Next_Ind in Buffer'Range
                 and then
                   (Is_Alnum
                        (UTF8_Get_Char (Buffer (Offset .. Next_Ind)))
                    or else Buffer (Offset) = '_')
               then

                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_Name_First := Offset;

                  if Token.Token_Name_Last = 0 then
                     Token.Token_Name_Last := Offset;

                     if Length (Result) = 0 and then Blank_Before then
                        Token := Null_Token;
                        Push_Pckg (Offset);
                        exit;
                     end if;

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
               else
                  Push (Token);
                  exit;
               end if;
         end case;

         Offset := UTF8_Find_Prev_Char (Buffer, Offset);
         Blank_Before := Blank_Here;
      end loop;

      Push (Token);

      return Result;
   end Parse_Current_List;

end Completion.Expression_Parser;
