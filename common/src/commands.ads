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

--  This package offers a mechanism for defining simple commands and
--  executing them sequentially.
--
--  In order to define new commands, you must override Root_Command
--  and provide implementation for the function Execute (and for the
--  function Undo if you want your command to be undoable).
--
--  Once the command is created, you can then add it to the command
--  queue using Enqueue. If a command is already being run, then the
--  action will be enqueued. When an action finishes, the next action
--  pending in the queue will be executed.
--
--  One very important point : the implementation of the function
--  Execute must call Command_Finished when the action is completed,
--  since this is what triggers the execution of the next action in
--  queue.
--
--  Commands can be asynchronous, ie Execute may return even though
--  the action is not finished.

with Generic_List;

package Commands is

   type Root_Command is abstract tagged limited private;
   type Command_Access is access all Root_Command'Class;

   function Name (Command : access Root_Command) return String;
   --  Gives a description of the command.

   function Execute (Command : access Root_Command) return Boolean is abstract;
   --  Executes Command. Return value indicates whether the operation was
   --  successful.
   --  IMPORTANT: every implementation for Execute must guarantee
   --  that Command_Finished will be called when the action is
   --  completed. See comments above.

   function Undo (Command : access Root_Command) return Boolean;
   --  Undo a Command. Return value indicates whether the operation was
   --  successful.
   --  IMPORTANT: at the end of undoing, Command_Finished must be
   --  called. See above for details.

   type Command_Queue is private;

   function New_Queue return Command_Queue;
   --  Create a new empty Command_Queue.

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command;
      High_Priority : Boolean := False);
   --  Adds Action to the Queue.
   --  If High_Priority is True, the only thing that is guaranteed is
   --  that this Action will be executed before all the actions that
   --  were enqueued with High_Priority set to False.

   procedure Undo (Queue : Command_Queue);
   --  Undo one action from the queue.

   procedure Redo (Queue : Command_Queue);
   --  Redo one action from the queue.

   function Undo_Queue_Empty (Queue : Command_Queue) return Boolean;
   function Redo_Queue_Empty (Queue : Command_Queue) return Boolean;
   --  These function indicate whether the undo and redo queues for
   --  the Queue are empty.

   procedure Add_Consequence_Action
     (Item   : Command_Access;
      Action : Command_Access);
   --  Add an action that will be enqueued if Item executes successfully.

private
   procedure Execute (Command : access Root_Command);
   --  Convenience subprogram : same as function Execute, but does not
   --  return any value.

   procedure Command_Finished
     (Action  : access Root_Command;
      Success : Boolean);
   --  This procedure should be called every time the execution of a Command
   --  ends. This starts the execution of the next Command in Action.Queue.
   --  Action is the Action that has just finished. Success indicates
   --  the success of Action.

   procedure Free (X : in out Command_Access);
   package Command_Queues is new Generic_List (Command_Access);

   type Command_Queue_Record is record
      Command_In_Progress : Boolean := False;
      The_Queue           : Command_Queues.List;
      Queue_Node          : Command_Queues.List_Node;

      Undo_Queue          : Command_Queues.List;
      --  This contains the actions that have already been done,
      --  in reverse chronological order (ie, most ancient actions are
      --  at the end of the queue, recent actions are prepended at the
      --  beginning.)

      Redo_Queue          : Command_Queues.List;
      --  This contains the actions that have been done and undone.
      --  (Again, most recent additions to this queue are at its
      --  beginning.)
   end record;
   type Command_Queue is access Command_Queue_Record;

   type Command_Mode is (Normal, Done, Undone);
   --  Normal actions are enqueued normally. When they end, they are
   --  enqueued at the end of the Undo_Queue and their mode becomes
   --  "Done".
   --  When a "Done" action is enqueued, it will be undone when it's
   --  time for its execution. It will then be added to the Redo_Queue
   --  and its mode will be Undone. When executing an Undone action,
   --  it will go to the Undo_Queue and become Done again, and so on.

   type Root_Command is abstract tagged limited record
      Queue         : Command_Queue;
      Next_Commands : Command_Queues.List;
      Mode          : Command_Mode := Normal;
   end record;

   pragma Inline (Undo_Queue_Empty);
   pragma Inline (Redo_Queue_Empty);

end Commands;
