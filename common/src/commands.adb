-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glide_Intl; use Glide_Intl;

package body Commands is

   use Command_Queues;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Execute_Next_Action (Queue : Command_Queue);
   --  Execute the next action in the queue, or do nothing if there is none.

   ---------------
   -- New_Queue --
   ---------------

   function New_Queue return Command_Queue is
      Q : Command_Queue := new Command_Queue_Record;
   begin
      return Q;
   end New_Queue;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Access) is
      pragma Unreferenced (X);
   begin
      --  ??? must correctly free memory.
      null;
   end Free;

   ----------
   -- Name --
   ----------

   function Name (Command : access Root_Command) return String is
      pragma Unreferenced (Command);
   begin
      return -"Generic command";
   end Name;

   ----------
   -- Undo --
   ----------

   function Undo (Command : access Root_Command) return Boolean is
      pragma Unreferenced (Command);
   begin
      return False;
   end Undo;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command;
      High_Priority : Boolean := False) is
   begin
      Action.Queue := Queue;

      if High_Priority then
         if Queue.Queue_Node = Null_Node then
            Append
              (Queue.The_Queue, Queue.Queue_Node, Command_Access (Action));
            Queue.Queue_Node := Last (Queue.The_Queue);
         else
            Prepend
              (Queue.The_Queue, Queue.Queue_Node, Command_Access (Action));
            Queue.Queue_Node := Prev (Queue.The_Queue, Queue.Queue_Node);
         end if;
      else
         Append (Queue.The_Queue, Command_Access (Action));

         if Queue.Queue_Node = Null_Node then
            Queue.Queue_Node := Last (Queue.The_Queue);
         end if;
      end if;

      if not Queue.Command_In_Progress then
         Execute_Next_Action (Queue);
      end if;
   end Enqueue;

   -------------------------
   -- Execute_Next_Action --
   -------------------------

   procedure Execute_Next_Action (Queue : Command_Queue) is
   begin
      --  If there is already a command running, then do nothing.

      if Queue.Command_In_Progress = True then
         return;
      end if;

      --  If the queue is empty, set its state accordingly.

      if Queue.Queue_Node = Null_Node then
         Queue.Command_In_Progress := False;
         return;
      end if;

      Queue.Command_In_Progress := True;

      declare
         Action  : Command_Access := Data (Queue.Queue_Node);
         Success : Boolean;
      begin
         Queue.Queue_Node := Next (Queue.Queue_Node);
         Success := Execute (Action);

         --  ??? Where is Action freed ? at the end of execution ?
      end;
   end Execute_Next_Action;

   ----------------------
   -- Command_Finished --
   ----------------------

   procedure Command_Finished
     (Action  : access Root_Command;
      Success : Boolean)
   is
      Queue             : Command_Queue renames Action.Queue;
      Next_Command_Temp : List_Node;

   begin
      Queue.Command_In_Progress := False;

      if Success then
         Next_Command_Temp := First (Action.Next_Commands);

         while Next_Command_Temp /= Null_Node loop
            Enqueue (Queue, Data (Next_Command_Temp), True);
            Next_Command_Temp := Next (Next_Command_Temp);
         end loop;

         --  ??? There is a memory leak here.
         --  Pointers allocated to Action.Next_Commands are dangling.
         Action.Next_Commands := Null_List;
      else
         Free (Action.Next_Commands);
      end if;

      Execute_Next_Action (Action.Queue);
   end Command_Finished;

   -------------
   -- Execute --
   -------------

   procedure Execute (Command : access Root_Command) is
      Success : Boolean;
   begin
      Success := Execute (Command_Access (Command));
   end Execute;

   ----------------------------
   -- Add_Consequence_Action --
   ----------------------------

   procedure Add_Consequence_Action
     (Item   : Command_Access;
      Action : Command_Access) is
   begin
      Prepend (Item.Next_Commands, Action);
   end Add_Consequence_Action;

end Commands;
