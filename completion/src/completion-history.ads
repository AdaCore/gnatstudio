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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
use Ada.Containers;

package Completion.History is

   type Completion_History is new Completion_Resolver with private;
   --  A completion history has to be populated by user chosen completion.
   --  Later, the most recent proposals will be displayed in top of the list.
   --  This resolver has to be the first one referenced in the manager.

   type Completion_History_Access is access all Completion_History'Class;

   procedure Get_Possibilities
     (Resolver   : access Completion_History;
      Identifier : String;
      Is_Partial : Boolean;
      Context    : Completion_Context;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List);
   --  See inherited documentation

   function From_Completion_Id
     (Resolver : access Completion_History;
      Id       : Completion_Id;
      Context  : Completion_Context;
      Filter   : Possibilities_Filter)
      return Completion_Proposal_Access;
   --  See inherited documentation

   procedure Free (Resolver : in out Completion_History);
   --  See inherited documentation

   procedure Prepend_Proposal
     (Resolver : access Completion_History;
      Proposal : Completion_Proposal'Class);
   --  This function has to be called each time a completion is applied by
   --  the user. The proposal will be added in the front of the history list.
   --  If the proposal given in parameter is already in the list, then it will
   --  be moved to the front.

   procedure Set_History_Size
     (History : access Completion_History; Size : Natural);
   --  Sets the history maximum size. By default, it has 50 elements.

private

   package Proposal_Stack is new
     Indefinite_Doubly_Linked_Lists (Completion_Id);

   use Proposal_Stack;

   type Completion_History is new Completion_Resolver with record
      Stack : Proposal_Stack.List;
      Size  : Natural := 50;
   end record;

end Completion.History;
