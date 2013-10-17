------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

package body Commands.Controls is

   type Queue_Change_Command is new Root_Command with record
      The_Queue : Command_Queue;
      UR        : Undo_Redo;
   end record;
   type Queue_Change_Access is access all Queue_Change_Command;

   overriding function Execute
     (Command : access Queue_Change_Command) return Command_Return_Type;

   overriding procedure Free (X : in out Queue_Change_Command) is null;
   --  Free memory associated to X.

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Queue_Change_Command) return Command_Return_Type is
   begin
      --  Is this a change in the current queue ?
      if Command.The_Queue = Command.UR.Queue then
         if Command.UR.Undo_Button /= null then
            Command.UR.Undo_Button.Set_Sensitive
              (Command.The_Queue /= Null_Command_Queue
               and then not Undo_Queue_Empty (Command.The_Queue));
         end if;

         if Command.UR.Redo_Button /= null then
            Command.UR.Redo_Button.Set_Sensitive
              (Command.The_Queue /= Null_Command_Queue
               and then not Redo_Queue_Empty (Command.The_Queue));
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------------------
   -- Set_Undo_Redo_Queue --
   -------------------------

   procedure Set_Undo_Redo_Queue (Queue  : Command_Queue; UR : Undo_Redo) is
      Command : Queue_Change_Access;
   begin
      if Queue /= UR.Queue then
         UR.Queue := Queue;

         Command := new Queue_Change_Command;
         Command.UR := UR;
         Command.The_Queue := Queue;
         UR.Command := Command_Access (Command);

         Execute (Command);

         --  This frees the previous command associated with that queue.
         Add_Queue_Change_Hook (Queue, Command_Access (Command), "Controls");
      end if;
   end Set_Undo_Redo_Queue;

   ---------------------------
   -- Unset_Undo_Redo_Queue --
   ---------------------------

   procedure Unset_Undo_Redo_Queue (UR : Undo_Redo) is
   begin
      UR.Queue := Null_Command_Queue;

      if UR.Undo_Button /= null then
         UR.Undo_Button.Set_Sensitive (False);
      end if;

      if UR.Redo_Button /= null then
         UR.Redo_Button.Set_Sensitive (False);
      end if;

      --  UR.Commands still monitors the old queue, but its execution has no
      --  effect.
   end Unset_Undo_Redo_Queue;

end Commands.Controls;
