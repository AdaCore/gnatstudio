-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Glide_Intl; use Glide_Intl;
with Unchecked_Deallocation;

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
   begin
      return new Command_Queue_Record;
   end New_Queue;

   ----------------
   -- Free_Queue --
   ----------------

   procedure Free_Queue (Q : in out Command_Queue)
   is
      procedure Free_Queue_Access is
         new Unchecked_Deallocation (Command_Queue_Record, Command_Queue);
   begin
      if Q /= null then
         --  Those lists contain only copies of commands that are in
         --  Q.The_Queue, therefore their contents do not need to be
         --  destroyed explicitely.
         Free (Q.Undo_Queue, Free_Data => False);
         Free (Q.Redo_Queue, Free_Data => False);

         Free (Q.The_Queue);
         Free (Q.Queue_Change_Hook);
         Free_Queue_Access (Q);
      end if;
   end Free_Queue;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Access) is
   begin
      if X /= null then
         Free (X.Next_Commands);
         Destroy (X);
      end if;
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
   begin
      Command_Finished (Command, False);
      return False;
   end Undo;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (X : access Root_Command) is
      pragma Unreferenced (X);
   begin
      null;
   end Destroy;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command;
      High_Priority : Boolean := False) is
   begin
      if Queue = Null_Command_Queue then
         return;
      end if;

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
         Action  : constant Command_Access := Data (Queue.Queue_Node);
         Success : Boolean;
      begin
         Queue.Queue_Node := Next (Queue.Queue_Node);

         case Action.Mode is
            when Normal | Undone =>
               Success := Execute (Action);

            when Done =>
               Success := Undo (Action);
         end case;

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
      Queue : Command_Queue renames Action.Queue;
      Node  : List_Node;

   begin
      if Queue = null then
         return;
      end if;

      Queue.Command_In_Progress := False;

      if Success then
         Node := First (Action.Next_Commands);

         while Node /= Null_Node loop
            Prepend (Queue.The_Queue, Queue.Queue_Node, Data (Node));
            Queue.Queue_Node := Prev (Queue.The_Queue, Queue.Queue_Node);
            Data (Node).Queue := Queue;
            Node := Next (Node);
         end loop;

         --  ??? There is a memory leak here.
         --  Pointers allocated to Action.Next_Commands are dangling.
         Action.Next_Commands := Null_List;

      else
         Free (Action.Next_Commands);
      end if;

      case Action.Mode is
         when Normal =>
            Action.Mode := Done;
            Prepend (Queue.Undo_Queue, Command_Access (Action));
            --  When a normal command is finished, purge the redo
            --  queue.
            Free (Queue.Redo_Queue, Free_Data => False);

         when Done =>
            Action.Mode := Undone;
            Prepend (Queue.Redo_Queue, Command_Access (Action));

         when Undone =>
            Action.Mode := Done;
            Prepend (Queue.Undo_Queue, Command_Access (Action));
      end case;

      if Queue.Queue_Change_Hook /= null then
         Execute (Queue.Queue_Change_Hook);
      end if;

      Execute_Next_Action (Action.Queue);
   end Command_Finished;

   ---------------------------
   -- Get_Queue_Change_Hook --
   ---------------------------

   function Get_Queue_Change_Hook
     (Queue : Command_Queue)
     return Command_Access
   is
   begin
      return Queue.Queue_Change_Hook;
   end Get_Queue_Change_Hook;

   ---------------------------
   -- Add_Queue_Change_Hook --
   ---------------------------

   procedure Add_Queue_Change_Hook
     (Queue   : Command_Queue;
      Command : Command_Access)
   is
   begin
      Queue.Queue_Change_Hook := Command;
      Command.Queue := null;
   end Add_Queue_Change_Hook;

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
     (Item   : access Root_Command'Class;
      Action : access Root_Command'Class) is
   begin
      Prepend (Item.Next_Commands, Command_Access (Action));
   end Add_Consequence_Action;

   --------------------------
   -- Get_Previous_Command --
   --------------------------

   function Get_Previous_Command (Queue : Command_Queue)
     return Command_Access
   is
   begin
      if not Is_Empty (Queue.Undo_Queue) then
         return Head (Queue.Undo_Queue);
      else
         return null;
      end if;
   end Get_Previous_Command;

   ----------------------
   -- Get_Next_Command --
   ----------------------

   function Get_Next_Command (Queue : Command_Queue)
     return Command_Access
   is
   begin
      if not Is_Empty (Queue.Redo_Queue) then
         return Head (Queue.Redo_Queue);
      else
         return null;
      end if;
   end Get_Next_Command;

   ----------
   -- Undo --
   ----------

   procedure Undo (Queue : Command_Queue) is
      Action : Command_Access;
   begin
      if not Is_Empty (Queue.Undo_Queue) then
         Action := Head (Queue.Undo_Queue);
         Next (Queue.Undo_Queue);
         Enqueue (Queue, Action);
      end if;
   end Undo;

   ----------
   -- Redo --
   ----------

   procedure Redo (Queue : Command_Queue) is
      Action : Command_Access;
   begin
      if not Is_Empty (Queue.Redo_Queue) then
         Action := Head (Queue.Redo_Queue);
         Next (Queue.Redo_Queue);
         Enqueue (Queue, Action);
      end if;
   end Redo;

   ----------------------
   -- Undo_Queue_Empty --
   ----------------------

   function Undo_Queue_Empty (Queue : Command_Queue) return Boolean is
   begin
      return Is_Empty (Queue.Undo_Queue);
   end Undo_Queue_Empty;

   ----------------------
   -- Redo_Queue_Empty --
   ----------------------

   function Redo_Queue_Empty (Queue : Command_Queue) return Boolean is
   begin
      return Is_Empty (Queue.Redo_Queue);
   end Redo_Queue_Empty;

end Commands;
