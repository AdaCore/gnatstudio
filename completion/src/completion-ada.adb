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

with Completion.Expression_Parser; use Completion.Expression_Parser;

package body Completion.Ada is

   ---------------------------------
   -- Get_Initial_Completion_List --
   ---------------------------------

   function Get_Initial_Completion_List
     (Manager      : Ada_Completion_Manager;
      Start_Offset : Natural) return Completion_List
   is
      use Token_List;

      Completing_Expression : Token_List.List;

      procedure Analyze_Token
        (Token       : Token_List.List_Node;
         Previous_It : Completion_Iterator;
         Result      : in out Completion_List);

      -------------------
      -- Analyze_Token --
      -------------------

      Filter : Possibilities_Filter := Everything;

      procedure Analyze_Token
        (Token       : Token_List.List_Node;
         Previous_It : Completion_Iterator;
         Result      : in out Completion_List)
      is
         Tmp           : Completion_List;
         Tmp_It        : Completion_Iterator;

         procedure Handle_Identifier (Id : String);

         -----------------------
         -- Handle_Identifier --
         -----------------------

         procedure Handle_Identifier (Id : String) is
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
                        Next (Token) = Token_List.Null_Node,
                        Data (Token).Token_Name_First - 1,
                        Filter,
                        Tmp);

                     It := Next (It);
                  end loop;
               end;

               Filter := All_Visible_Entities;
            else
               Get_Composition
                 (Get_Proposal (Previous_It),
                  Get_Name (Get_Buffer (Manager).all, Data (Token)),
                  Data (Token).Token_Name_First - 1,
                  Next (Token) = Token_List.Null_Node,
                  Tmp);

               --  ??? See how these two could be handled in a more smooth way.
               --  We duplicate information with the call above.
               Tmp.Searched_Identifier := new String'
                 (Get_Name (Get_Buffer (Manager).all, Data (Token)));
               Tmp.Is_Partial := Next (Token)
                 = Token_List.Null_Node;
            end if;

            Tmp_It := First (Tmp);

            if Next (Token) = Token_List.Null_Node then
               Result := Tmp;
            else
               Free (Result);

               while not At_End (Tmp_It) loop
                  Analyze_Token (Next (Token), Tmp_It, Result);

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
                    (Get_Proposal (Previous_It),
                     "",
                     Start_Offset,
                     True,
                     Result);
               else
                  Analyze_Token (Next (Token), Previous_It, Result);
               end if;

            when Tok_Comma =>
               pragma Assert (Next (Token) = Token_List.Null_Node);

               --  ???  Temporary deactivated this code, since it's not
               --  implemented anyway
--                 declare
--                    Real_Proposal : Completion_Proposal'Class :=
--                      Get_Proposal (Previous_It);
--                 begin
--                    Set_Mode (Real_Proposal, Show_Parameters);
--                    Append (Returned_List, Real_Proposal);
--
--                    return Returned_List;
--                 end;
               null;

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
                 (Get_Name (Get_Buffer (Manager).all, Data (Token)));

               return;

            when Tok_Expression =>
               if Get_Number_Of_Parameters (Get_Proposal (Previous_It))
                 >= Data (Token).Number_Of_Parameters
                 and then Next (Token) /= Token_List.Null_Node
               then
                  Analyze_Token (Next (Token), Previous_It, Result);
               end if;

            when Tok_With =>
               pragma Assert (Token = First (Completing_Expression));

               Filter := All_Accessible_Units;

               Analyze_Token (Next (Token), Previous_It, Result);

            when Tok_Use =>
               null;

            when others =>
               null;
         end case;
      end Analyze_Token;

      Result : Completion_List;
   begin
      Completing_Expression := Parse_Current_List
        (Get_Buffer (Manager).all, Start_Offset);

      Analyze_Token
        (First (Completing_Expression), Null_Completion_Iterator, Result);

      return Result;
   end Get_Initial_Completion_List;

end Completion.Ada;
