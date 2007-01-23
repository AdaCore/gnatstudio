-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006, 2007                   --
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

with Completion.Expression_Parser; use Completion.Expression_Parser;

package body Completion.Ada is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   function Get_Initial_Completion_List
     (Manager        : Ada_Completion_Manager;
      Context        : Completion_Context;
      End_Is_Partial : Boolean := True) return Completion_List
   is
      use Token_List;

      Completing_Expression : Token_List.List;

      procedure Analyze_Token
        (Token             : Token_List.List_Node;
         Previous_Proposal : Completion_Proposal'Class;
         Result            : in out Completion_List);

      -------------------
      -- Analyze_Token --
      -------------------

      Filter : Possibilities_Filter := All_Visible_Entities;

      procedure Analyze_Token
        (Token             : Token_List.List_Node;
         Previous_Proposal : Completion_Proposal'Class;
         Result            : in out Completion_List)
      is

         procedure Handle_Identifier (Id : String);

         -----------------------
         -- Handle_Identifier --
         -----------------------

         procedure Handle_Identifier (Id : String) is
            Tmp    : Completion_List;
            Tmp_It : Completion_Iterator;
         begin
            if Token = First (Completing_Expression)
              or else (Filter and All_Accessible_Units) /= 0
            then
               declare
                  It : Completion_Resolver_List_Pckg.List_Node;
               begin
                  It := First (Manager.Resolvers);

                  while It /= Completion_Resolver_List_Pckg.Null_Node loop
                     Get_Possibilities
                       (Data (It),
                        Id,
                        Next (Token) = Token_List.Null_Node
                           and then End_Is_Partial,
                        Context,
                        Data (Token).Token_Name_First - 1,
                        Filter,
                        Tmp);

                     It := Next (It);
                  end loop;
               end;

               Filter := All_Visible_Entities;
            else
               Get_Composition
                 (Previous_Proposal,
                  Get_Name (Context.Buffer.all, Data (Token)),
                  Data (Token).Token_Name_First - 1,
                  Next (Token) = Token_List.Null_Node
                     and then End_Is_Partial,
                  Tmp);

               Tmp.Searched_Identifier := new String'
                 (Get_Name (Context.Buffer.all, Data (Token)));
            end if;

            Tmp_It := First (Tmp);

            if Next (Token) = Token_List.Null_Node then
               Completion_List_Pckg.Concat (Result.List, Tmp.List);

               if Result.Searched_Identifier = null then
                  Result.Searched_Identifier := Tmp.Searched_Identifier;
               else
                  Free (Tmp.Searched_Identifier);
               end if;
            else
               while not At_End (Tmp_It) loop
                  Analyze_Token (Next (Token), Get_Proposal (Tmp_It), Result);

                  Next (Tmp_It);
               end loop;

               Free (Tmp);
            end if;
         end Handle_Identifier;

      begin
         case Data (Token).Tok_Type is
            when Tok_Dot =>
               if Next (Token) = Token_List.Null_Node then
                  Get_Composition
                    (Previous_Proposal,
                     "",
                     Context.Offset,
                     True,
                     Result);
               else
                  Analyze_Token
                    (Next (Token), Previous_Proposal, Result);
               end if;

            when Tok_All =>
               if Next (Token) = Token_List.Null_Node then
                  --  If it's the last token, it might not be the keyword all,
                  --  see if we have something here
                  Handle_Identifier ("all");
               else
                  --  If not, we are really on the 'all' keyword, and we need
                  --  to dereference it
                  null;
               end if;

            when Tok_Identifier =>
               Handle_Identifier
                 (Get_Name (Context.Buffer.all, Data (Token)));

            when Tok_Expression =>
               if Next (Token) /= Token_List.Null_Node then
                  declare
                     Local_Proposal : Completion_Proposal'Class :=
                       Previous_Proposal;
                  begin
                     Append_Expression
                       (Local_Proposal,
                        Data (Token).Number_Of_Parameters);

                     Analyze_Token (Next (Token), Local_Proposal, Result);
                  end;
               end if;

            when Tok_Open_Parenthesis =>
               if Previous_Proposal.Profile = null then
                  --  There is no possible profile completion here, drop the
                  --  proposal

                  return;
               end if;

               declare
                  Current_Token : Token_List.List_Node := Next (Token);
                  Success : Boolean;
               begin
                  while Current_Token /= Token_List.Null_Node loop
                     if Data (Current_Token).Tok_Type = Tok_List_Item then
                        if Data (Current_Token).Token_Name_First = 0 then
                           Set_Next_Param_Written
                             (Previous_Proposal.Profile, Success);
                        else
                           Set_Param_Written
                             (Previous_Proposal.Profile,
                              Context.Buffer
                                (Data (Current_Token).Token_Name_First
                                 .. Data (Current_Token).Token_Name_Last),
                              Success);
                        end if;

                        if not Success then
                           return;
                        end if;
                     end if;

                     Current_Token := Next (Current_Token);
                  end loop;
               end;

               Previous_Proposal.Profile.Is_In_Profile := True;

            when Tok_With | Tok_Use =>
               pragma Assert (Token = First (Completing_Expression));

               Filter := All_Accessible_Units;

               if Next (Token) /= Token_List.Null_Node then
                  Analyze_Token (Next (Token), Previous_Proposal, Result);
               else
                  declare
                     It : Completion_Resolver_List_Pckg.List_Node;
                  begin
                     It := First (Manager.Resolvers);

                     while It /= Completion_Resolver_List_Pckg.Null_Node loop
                        Get_Possibilities
                          (Data (It),
                           "",
                           True,
                           Context,
                           Data (Token).Token_Name_First - 1,
                           All_Accessible_Units,
                           Result);

                        It := Next (It);
                     end loop;
                  end;
               end if;

            when others =>
               null;
         end case;

         if Previous_Proposal.Profile /= null
           and then Previous_Proposal.Profile.Is_In_Profile
         then
            Get_Composition
              (Previous_Proposal,
               "",
               Context.Offset,
               True,
               Result);
         end if;
      end Analyze_Token;

      Result : Completion_List;
   begin
      Completing_Expression := Parse_Current_List
        (Context.Buffer.all, Context.Offset);

      if First (Completing_Expression) /= Token_List.Null_Node then
         Analyze_Token
           (First (Completing_Expression), Null_Completion_Proposal, Result);
      end if;

      return Result;
   end Get_Initial_Completion_List;

end Completion.Ada;
