-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001 - 2002                     --
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
         Prepend (Queue.The_Queue, Command_Access (Action));
      else
         Append (Queue.The_Queue, Command_Access (Action));
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
      if Is_Empty (Queue.The_Queue) then
         Queue.Command_In_Progress := False;
         return;
      end if;

      Queue.Command_In_Progress := True;

      declare
         Action  : Command_Access := Head (Queue.The_Queue);
         Success : Boolean;
      begin
         Queue.The_Queue := Next (Queue.The_Queue);
         Success := Execute (Action);

         --  ??? Where is Action freed ? at the end of execution ?
      end;
   end Execute_Next_Action;

   ----------------------
   -- Command_Finished --
   ----------------------

   procedure Command_Finished (Queue : Command_Queue) is
   begin
      Queue.Command_In_Progress := False;
      Execute_Next_Action (Queue);
   end Command_Finished;

   -------------
   -- Execute --
   -------------

   procedure Execute (Command : access Root_Command) is
      Success : Boolean;
   begin
      Success := Execute (Command_Access (Command));
   end Execute;

end Commands;
