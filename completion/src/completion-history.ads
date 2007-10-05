-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2007, AdaCore                   --
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

--  This package offers an history capability to the completion engine. This is
--  an optional capability, enabled by implementing the interface
--  Storable_Proposal to the Completion_Proposal.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
use Ada.Containers;

package Completion.History is

   type Completion_History is new Completion_Resolver with private;
   --  A completion history has to be populated by user chosen completion.
   --  Later, the most recent proposals will be displayed in top of the list.
   --  This resolver has to be the first one referenced in the manager. It's
   --  implemented as a stack of unique stored proposals.

   type Completion_History_Access is access all Completion_History'Class;

   type Storable_Proposal is abstract new Completion_Proposal with null record;
   --  This is the root type of any completion proposal that's supposed to
   --  be able to manage history
   --  ??? This should really be an interface.

   type Stored_Proposal is abstract tagged null record;
   --  This is the base type for a proposal stored in the history. Such a
   --  proposal must have the capability of surviving during the whole
   --  lifecycle of a tool session, beyond the usual completion resolver and
   --  manager lifetime. The user is also responsible to check if the
   --  completion is still possible or not.

   type Stored_Proposal_Access is access all Stored_Proposal'Class;

   function To_Stored_Proposal
     (Proposal : Storable_Proposal) return Stored_Proposal_Access is abstract;
   --  Extract the stored proposal from a storable proposal.

   function Equal
     (Left : Stored_Proposal; Right : Stored_Proposal'Class)
      return Boolean is abstract;
   --  Return true if the two proposals are equals - the completion history
   --  will only store unique proposals.

   function From_Stored_Proposal
     (Stored  : Stored_Proposal;
      Manager : Completion_Manager_Access;
      Context : Completion_Context)
      return Completion_Proposal_Access is abstract;
   --  Recreates a completion proposal out of a stored proposal. If the
   --  proposal cannot be retreived anymore, the implementer may return a null
   --  value.

   procedure Free (Stored : in out Stored_Proposal) is abstract;
   --  Free the data associated to this stored proposal;

   overriding
   procedure Get_Completion_Root
     (Resolver   : access Completion_History;
      Offset     : Integer;
      Context    : Completion_Context;
      Result     : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Completion_History) return String;
   --  See inherited documentation

   overriding
   procedure Free (Resolver : in out Completion_History);
   --  See inherited documentation

   procedure Prepend_Proposal
     (Resolver : access Completion_History;
      Proposal : Completion_Proposal'Class
      --  This proposal has to implement the interface Storable_Proposal in
      --  order to be stored. If not, the proposal won't be stored.
     );
   --  This function has to be called each time a completion is applied by
   --  the user. The proposal will be added in the front of the history list.
   --  If the proposal given in parameter is already in the list, then it will
   --  be moved to the front.

   procedure Set_History_Size
     (History : access Completion_History; Size : Natural);
   --  Sets the history maximum size. By default, it has 50 elements.

private

   package Proposal_Stack is new
     Indefinite_Doubly_Linked_Lists (Stored_Proposal_Access);

   use Proposal_Stack;

   type Completion_History is new Completion_Resolver with record
      Stack : Proposal_Stack.List;
      Size  : Natural := 50;
   end record;

end Completion.History;
