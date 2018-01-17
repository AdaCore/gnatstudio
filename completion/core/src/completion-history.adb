------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body Completion.History is

   Resolver_ID : constant String := "CNST_HIS";

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver   : access Completion_History;
      Offset     : String_Index_Type;
      Context    : Completion_Context;
      Result     : in out Completion_List)
   is
      It : Proposal_Stack.Cursor := First (Resolver.Stack);
      It_Garbage : Proposal_Stack.Cursor := First (Resolver.Stack);

      List : Completion_List_Extensive_Pckg.Extensive_List_Pckg.Vector;
      Garbage_Element : Stored_Proposal_Access;

      procedure Free is new Ada.Unchecked_Deallocation
        (Stored_Proposal'Class, Stored_Proposal_Access);
   begin
      It := First (Resolver.Stack);

      while It /= Proposal_Stack.No_Element loop
         if Is_Valid (Element (It).all) then
            declare
               Proposal : Completion_Proposal_Access :=
                 From_Stored_Proposal
                   (Element (It).all, Resolver.Manager, Context);
            begin
               if Proposal /= null
                 and then Match (Proposal.all, Context, Offset)
               then
                  Completion_List_Extensive_Pckg.Extensive_List_Pckg.Append
                    (List, Proposal.all);
               end if;

               Free (Proposal);
            end;

            It := Next (It);
         else
            Garbage_Element := Element (It);
            It_Garbage := It;
            It := Next (It);

            Delete (Resolver.Stack, It_Garbage);
            Free (Garbage_Element.all);
            Free (Garbage_Element);
         end if;
      end loop;

      Completion_List_Pckg.Append
        (Result.List, Completion_List_Extensive_Pckg.To_Extensive_List (List));
   end Get_Completion_Root;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id (Resolver : Completion_History) return String is
      pragma Unreferenced (Resolver);
   begin
      return Resolver_ID;
   end Get_Id;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Resolver : in out Completion_History) is
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
      Stored     : Stored_Proposal_Access;
      It         : Proposal_Stack.Cursor;
      It_Element : Stored_Proposal_Access;

      procedure Free is new Ada.Unchecked_Deallocation
        (Stored_Proposal'Class, Stored_Proposal_Access);
   begin
      if Proposal not in Storable_Proposal'Class then
         return;
      end if;

      Stored := To_Stored_Proposal (Storable_Proposal'Class (Proposal));

      if Stored = null then
         --  If we can't create a stored proposal out of the proposal, then
         --  don't store it.

         return;
      end if;

      It     := First (Resolver.Stack);

      --  Free all elements equals to the one given in parameter form the
      --  history - we only keep one instance of each choice.
      while It /= Proposal_Stack.No_Element loop
         if Equal (Element (It).all, Stored.all) then
            It_Element := Element (It);
            Delete (Resolver.Stack, It);
            Free (It_Element.all);
            Free (It_Element);

            exit;
         end if;

         It := Next (It);
      end loop;

      Prepend (Resolver.Stack, Stored);

      while Natural (Length (Resolver.Stack)) > Resolver.Size loop
         It_Element := Element (Last (Resolver.Stack));

         Delete_Last (Resolver.Stack, 1);

         Free (It_Element.all);
         Free (It_Element);
      end loop;
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
