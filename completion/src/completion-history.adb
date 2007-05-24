-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2007                         --
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

package body Completion.History is

   -----------------------
   -- Get_Possibilities --
   -----------------------

   procedure Get_Possibilities
     (Resolver   : access Completion_History;
      Identifier : String;
      Is_Partial : Boolean;
      Context    : Completion_Context;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List)
   is
      It : Proposal_Stack.Cursor := First (Resolver.Stack);

      List : Completion_List_Extensive_Pckg.Extensive_List_Pckg.List;
   begin
      It := First (Resolver.Stack);

      while It /= Proposal_Stack.No_Element loop
         declare
            Proposal : Completion_Proposal_Access :=
              From_Completion_Id
                (Resolver.Manager, Element (It), Context, Filter);
         begin
            if Proposal /= null and then Match
              (Proposal.all, Identifier, Is_Partial, Context, Offset, Filter)
            then
               Completion_List_Extensive_Pckg.Extensive_List_Pckg.Append
                 (List, Proposal.all);
            end if;

            Free (Proposal);
         end;

         It := Next (It);
      end loop;

      Completion_List_Pckg.Append
        (Result.List, Completion_List_Extensive_Pckg.To_Extensive_List (List));
   end Get_Possibilities;

   ------------------------
   -- From_Completion_Id --
   ------------------------

   function From_Completion_Id
     (Resolver : access Completion_History;
      Id       : Completion_Id;
      Context  : Completion_Context;
      Filter   : Possibilities_Filter)
      return Completion_Proposal_Access
   is
      pragma Unreferenced (Resolver, Id, Context, Filter);
   begin
      return null;
   end From_Completion_Id;

   ----------
   -- Free --
   ----------

   procedure Free (Resolver : in out Completion_History) is
   begin
      Clear (Resolver.Stack);
   end Free;

   ----------------------
   -- Prepend_Proposal --
   ----------------------

   procedure Prepend_Proposal
     (Resolver : access Completion_History;
      Proposal : Completion_Proposal'Class)
   is
      It : Proposal_Stack.Cursor := First (Resolver.Stack);
      Id : constant Completion_Id :=  To_Completion_Id (Proposal);
   begin
      while It /= Proposal_Stack.No_Element loop
         if Element (It) = Id then
            Delete (Resolver.Stack, It);

            exit;
         end if;

         It := Next (It);
      end loop;

      Prepend (Resolver.Stack, Id);

      if Natural (Length (Resolver.Stack)) > Resolver.Size then
         Delete_Last
           (Resolver.Stack,
            Length (Resolver.Stack) - Count_Type (Resolver.Size));
      end if;
   end Prepend_Proposal;

   ----------------------
   -- Set_History_Size --
   ----------------------

   procedure Set_History_Size
     (History : access Completion_History; Size : Natural) is
   begin
      History.Size := Size;
   end Set_History_Size;

end Completion.History;
