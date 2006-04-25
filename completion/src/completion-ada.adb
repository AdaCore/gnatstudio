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

      function Analyze_Token
        (Token : Token_List.List_Node; Previous_It : Completion_Iterator)
         return Completion_List;

      -------------------
      -- Analyze_Token --
      -------------------

      Filter : Possibilities_Filter := All_Visible_Entities;

      function Analyze_Token
        (Token : Token_List.List_Node; Previous_It : Completion_Iterator)
         return Completion_List
      is
         Tmp           : Completion_List;
         Tmp_It        : Completion_Iterator;
         Returned_List : Completion_List;

         function Handle_Identifier (Id : String) return Completion_List;

         -----------------------
         -- Handle_Identifier --
         -----------------------

         function Handle_Identifier (Id : String) return Completion_List is
            Returned_List : Completion_List;
         begin
            if Token = First (Completing_Expression)
              or else (Filter and All_Accessible_Units) /= 0
            then
               declare
                  It : Completion_Resolver_List_Pckg.List_Node;
               begin
                  It := First (Manager.Resolvers);

                  while It /= Completion_Resolver_List_Pckg.Null_Node loop
                     Concat
                       (Tmp,
                        Get_Possibilities
                          (Data (It),
                           Id,
                           Next (Token) = Token_List.Null_Node,
                           Start_Offset,
                           Filter));

                     It := Next (It);
                  end loop;
               end;

               Filter := All_Visible_Entities;
            else
               Tmp := Get_Composition
                 (Data (Previous_It),
                  Get_Name (Get_Buffer (Manager).all, Data (Token)),
                  Start_Offset,
                  Next (Token) = Token_List.Null_Node);
            end if;

            Tmp_It := First (Tmp);

            if Next (Token) = Token_List.Null_Node then
               return Tmp;
            else
               while Tmp_It /= Null_Completion_Iterator loop
                  Concat
                    (Returned_List,
                     Analyze_Token (Next (Token), Tmp_It));

                  Tmp_It := Next (Tmp_It);
               end loop;
            end if;

            return Returned_List;
         end Handle_Identifier;

      begin
         case Data (Token).Tok_Type is
            when Tok_Dot =>
               if Next (Token) = Token_List.Null_Node then
                  return Get_Composition (Data (Previous_It), Start_Offset);
               else
                  return Analyze_Token (Next (Token), Previous_It);
               end if;

            when Tok_Comma =>
               pragma Assert (Next (Token) = Token_List.Null_Node);

               declare
                  Real_Proposal : Completion_Proposal'Class :=
                    Get_Proposal (Previous_It);
               begin
                  Set_Mode (Real_Proposal, Show_Parameters);
                  Append (Returned_List, Real_Proposal);

                  return Returned_List;
               end;

            when Tok_All =>
               if Next (Token) = Token_List.Null_Node then
                  --  If it's the last token, it might not be the keyword all,
                  --  see if we have something here
                  return Handle_Identifier ("all");
               else
                  --  If not, we are really on the 'all' keyword, and we need
                  --  to dereference it
                  null;
               end if;

            when Tok_Identifier =>
               return Handle_Identifier
                 (Get_Name (Get_Buffer (Manager).all, Data (Token)));

            when Tok_Expression =>
               if Get_Number_Of_Parameters (Get_Proposal (Previous_It))
                 < Data (Token).Number_Of_Parameters
               then
                  return Null_Completion_List;
               end if;

               return Analyze_Token (Next (Token), Previous_It);

            when Tok_With =>
               pragma Assert (Token = First (Completing_Expression));

               Filter := All_Accessible_Units;

               return Analyze_Token (Next (Token), Previous_It);

            when Tok_Use =>
               null;

            when others =>
               null;
         end case;

         return Null_Completion_List;
      end Analyze_Token;

   begin
      Completing_Expression := Parse_Current_List
        (Get_Buffer (Manager).all, Start_Offset);

      return Analyze_Token
        (First (Completing_Expression), Null_Completion_Iterator);
   end Get_Initial_Completion_List;

end Completion.Ada;
