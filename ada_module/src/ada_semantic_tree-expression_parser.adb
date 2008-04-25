-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007-2008, AdaCore               --
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

with Language.Tree;              use Language.Tree;
with Language.Tree.Database;     use Language.Tree.Database;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Glib.Unicode;               use Glib.Unicode;

package body Ada_Semantic_Tree.Expression_Parser is

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

   function Get_Name
     (Expression : Parsed_Expression; Token : Token_Record) return String
   is
   begin
      if Token.Token_First /= 0 and then Token.Token_Last /= 0 then
         return Expression.Original_Buffer
           (Token.Token_First .. Token.Token_Last);
      else
         return "";
      end if;
   end Get_Name;

   ------------------------
   -- Parse_Current_List --
   ------------------------

   function Parse_Current_List
     (Buffer       : UTF8_String_Access;
      Start_Offset : Natural;
      End_Offset   : Natural := 0)
      return Parsed_Expression
   is
      Offset             : Natural := Start_Offset;
      Offset_Limit       : Natural;
      Token              : Token_Record;
      Result             : Parsed_Expression;
      Last_Token_On_Line : Token_List.List_Node;

      procedure Handle_Expression (Offset : in out Natural; Skip : Boolean);
      procedure Skip_String (Offset : in out Natural);
      procedure Skip_Comment_Line (Offset : in out Natural);
      procedure Push_Pckg (Offset : in out Natural);
      function Check_Prev_Word
        (Offset : Positive; Word : String)
         return Boolean;

      procedure Push (Token : in out Token_Record);
      procedure Pop;

      ---------------------
      -- Skip_Expression --
      ---------------------

      procedure Handle_Expression (Offset : in out Natural; Skip : Boolean) is
         Local_Token : Token_Record := Token;
      begin
         Local_Token.Token_Last := Offset;

         while Offset > Offset_Limit loop
            case Buffer (Offset) is
               when ')' =>
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Handle_Expression (Offset, True);

               when ',' =>
                  if not Skip then
                     Local_Token.Tok_Type := Tok_Expression;
                     Local_Token.Token_First := Offset + 1;
                     Push (Local_Token);
                     Local_Token.Token_Last := Offset - 1;
                  end if;

               when '"' =>
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Skip_String (Offset);

               when ''' =>
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

               when '(' =>
                  if not Skip then
                     Local_Token.Tok_Type := Tok_Expression;
                     Local_Token.Token_First := Offset + 1;
                     Push (Local_Token);

                     Local_Token.Tok_Type := Tok_Open_Parenthesis;
                     Push (Local_Token);
                  end if;

                  exit;

               when ASCII.LF =>
                  Skip_Comment_Line (Offset);

               when others =>
                  null;
            end case;

            Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

         end loop;
      end Handle_Expression;

      -----------------
      -- Skip_String --
      -----------------

      procedure Skip_String (Offset : in out Natural) is
      begin
         while Offset > Offset_Limit + 1 loop
            case Buffer (Offset) is
               when '"' =>
                  if Buffer (Offset - 1) /= '"' then
                     exit;
                  else
                     Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  end if;

               when ASCII.LF =>
                  --  Strings can't be on spread across multiple lines - this
                  --  probably means that the string is broken.
                  exit;

               when others =>
                  null;
            end case;

            Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

         end loop;
      end Skip_String;

      -----------------------
      -- Skip_Comment_Line --
      -----------------------

      procedure Skip_Comment_Line (Offset : in out Natural) is
         Local_Offset : Natural := Offset;
         Prev_Offset  : Natural;
      begin
         Local_Offset := UTF8_Find_Prev_Char (Buffer.all, Local_Offset);

         while Local_Offset > 1 loop
            case Buffer (Local_Offset) is
               when '"' =>
                  Local_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);
                  Skip_String (Local_Offset);

               when ''' =>
                  Local_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);
                  Local_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);

               when '-' =>
                  Prev_Offset := UTF8_Find_Prev_Char
                    (Buffer.all, Local_Offset);

                  if Buffer (Prev_Offset) = '-' then
                     Local_Offset := UTF8_Find_Prev_Char
                       (Buffer.all, Prev_Offset);

                     Skip_Comment_Line (Local_Offset);
                     Offset := Local_Offset + 1;

                     exit;
                  end if;

               when ASCII.LF =>
                  exit;

               when others =>
                  null;
            end case;

            Local_Offset := UTF8_Find_Prev_Char
              (Buffer.all, Local_Offset);
         end loop;
      end Skip_Comment_Line;

      ---------------
      -- Push_Pckg --
      ---------------

      procedure Push_Pckg (Offset : in out Natural) is
      begin
         if Check_Prev_Word (Offset, "with") then
            Token.Tok_Type := Tok_With;
            Token.Token_Last := Offset;
            Offset := Offset - 3;
            Token.Token_First := Offset;
            Push (Token);
         elsif Check_Prev_Word (Offset, "use") then
            Token.Tok_Type := Tok_Use;
            Token.Token_Last := Offset;
            Offset := Offset - 2;
            Token.Token_First := Offset;
            Push (Token);
         end if;
      end Push_Pckg;

      ----------
      -- Push --
      ----------

      procedure Push (Token : in out Token_Record) is
         Name : constant String := Get_Name (Result, Token);
      begin
         --  Check if we're on a special keyword

         if Equal (Name, "all", False) then
            Token.Tok_Type := Tok_All;
         end if;

         if Token /= Null_Token then
            if Token.Token_First = 0 then
               Token.Token_First := Offset;
            end if;

            if Token.Token_Last = 0 then
               Token.Token_Last := Offset;
            end if;

            Prepend (Result.Tokens, Token);
            Token := Null_Token;
         end if;

         if Last_Token_On_Line = Token_List.Null_Node then
            Last_Token_On_Line := First (Result.Tokens);
         end if;
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop is
      begin
         Remove_Nodes
           (Result.Tokens, Token_List.Null_Node, First (Result.Tokens));
      end Pop;

      ---------------------
      -- Check_Prev_Word --
      ---------------------

      function Check_Prev_Word (Offset : Positive; Word : String)
                                return Boolean is
      begin
         return Offset - (Word'Length - 1) > Offset_Limit
           and then To_Lower (Buffer (Offset - (Word'Length - 1) .. Offset))
           = To_Lower (Word)
           and then
             (Offset - Word'Length <= Offset_Limit
              or else Buffer (Offset - Word'Length) = ' '
              or else Buffer (Offset - Word'Length) = ASCII.LF
              or else Buffer (Offset - Word'Length) = ASCII.HT);
      end Check_Prev_Word;

      Blank_Here, Blank_Before : Boolean := False;
      Next_Ind                 : Natural;
      Possible_Arrow           : Boolean := False;

   begin
      if End_Offset < Buffer'First then
         Offset_Limit := Buffer'First - 1;
      else
         Offset_Limit := End_Offset - 1;
      end if;

      Skip_Comment_Line (Offset);
      Result.Original_Buffer := Buffer;

      if Offset /= Start_Offset then
         --  In this case, we are on a comment line. So the expression is
         --  empty.

         return Result;
      end if;

      while Offset > Offset_Limit loop

         Blank_Here := False;

         if Possible_Arrow and then Buffer (Offset) /= '=' then
            exit;
         end if;

         case Buffer (Offset) is
            when ',' =>
               Push (Token);

               if Length (Result.Tokens) = 0 then
                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
                  Handle_Expression (Offset, False);
               else
                  exit;
               end if;

            when ')' =>
               Push (Token);

               Token.Tok_Type := Tok_Close_Parenthesis;
               Push (Token);

               Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
               Handle_Expression (Offset, False);

            when '(' =>
               Push (Token);

               if Length (Result.Tokens) = 0 then
                  Token.Tok_Type := Tok_Open_Parenthesis;
                  Token.Token_First := Offset;
                  Token.Token_Last := Offset;
                  Push (Token);
               end if;

            when '.' =>
               Push (Token);

               if Length (Result.Tokens) > 0
                 and then Head (Result.Tokens).Tok_Type = Tok_Dot
               then
                  --  In this case, we have a range construct, like A .. B.
                  --  It's the end of the expression.

                  Pop;

                  exit;
               end if;

               Token.Tok_Type := Tok_Dot;
               Push (Token);

            when ' ' | ASCII.HT | ASCII.CR =>
               Push (Token);
               Blank_Here := True;

            when '"' =>
               if First (Result.Tokens) /= Token_List.Null_Node
                 and then
                   (Data (First (Result.Tokens)).Tok_Type
                    = Tok_Open_Parenthesis
                    or else Data (First (Result.Tokens)).Tok_Type
                    = Tok_Expression)
               then
                  --  We are in an operator symbol case

                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_Last := Offset;

                  Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);

                  Skip_String (Offset);

                  Token.Token_First := Offset;

                  Push (Token);
               else
                  exit;
               end if;

            when '>' =>
               Possible_Arrow := True;

            when '=' =>
               if Possible_Arrow then
                  Token.Tok_Type := Tok_Arrow;

                  Push (Token);

                  Possible_Arrow := False;
               else
                  exit;
               end if;

            when ASCII.LF =>
               Push (Token);
               Blank_Here := True;
               Skip_Comment_Line (Offset);

            when others =>
               Next_Ind := UTF8_Next_Char (Buffer.all, Offset);

               if (Next_Ind in Buffer'Range
                   and then
                     (Is_Alnum
                        (UTF8_Get_Char (Buffer (Offset .. Next_Ind)))))
                 or else
                   (Next_Ind not in Buffer'Range
                    and then Is_Alphanumeric (Buffer (Offset)))
                 or else Buffer (Offset) = '_'
               then
                  Token.Tok_Type := Tok_Identifier;
                  Token.Token_First := Offset;

                  if Token.Token_Last = 0 then
                     Token.Token_Last := Offset;

                     if Length (Result.Tokens) = 0 and then Blank_Before then
                        Token := Null_Token;
                        Push_Pckg (Offset);
                        exit;
                     end if;

                     if Length (Result.Tokens) > 0
                       and then Head (Result.Tokens).Tok_Type = Tok_Identifier
                     then
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

         Offset := UTF8_Find_Prev_Char (Buffer.all, Offset);
         Blank_Before := Blank_Here;
      end loop;

      Push (Token);

      return Result;
   end Parse_Current_List;

   ------------------------
   -- Parse_Current_List --
   ------------------------

   function Parse_Current_List
     (Buffer : UTF8_String_Access) return Parsed_Expression is
   begin
      return Parse_Current_List (Buffer, Buffer'Last);
   end Parse_Current_List;

end Ada_Semantic_Tree.Expression_Parser;
