-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
--  One very important point: the implementation of the function
--  Execute must call Command_Finished when the action is completed,
--  since this is what triggers the execution of the next action in
--  queue.
--
--  Commands can be asynchronous, ie Execute may return even though
--  the action is not finished.

with Generic_List;
with String_List_Utils;

package Commands is

   type Root_Command is abstract tagged limited private;
   type Command_Access is access all Root_Command'Class;

   type Progress_Activity is (Running, Stalled, Unknown);

   type Progress_Record is record
      Activity : Progress_Activity := Unknown;
      --  The current activity type.

      Current  : Natural := 0;
      --  The current progress indicator.

      Total    : Natural := 1;
      --  The total progress indicator.
   end record;

   function Name (Command : access Root_Command) return String;
   --  Gives a description of the command.

   function Progress (Command : access Root_Command) return Progress_Record;
   --  Return the current progress of the command.

   procedure Set_Progress
     (Command  : access Root_Command;
      Progress : Progress_Record);
   --  Set the progress of Command.

   type Command_Return_Type is
     (Success,
      --  The command terminated with success.

      Failure,
      --  The command terminated and failed.

      Execute_Again,
      --  The command should be executed again as soon as possible.

      Lower_Priority,
      --  Same as Execute_Again, and lower the priority of the command.

      Raise_Priority
      --  Same as Execute_Again, and raise the priority of the command.
      );

   function Execute
     (Command : access Root_Command) return Command_Return_Type is abstract;
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

   procedure Free (X : in out Root_Command) is abstract;
   --  Free memory associated to X.

   type Command_Queue is private;

   Null_Command_Queue : constant Command_Queue;

   function New_Queue return Command_Queue;
   --  Create a new empty Command_Queue.

   procedure Free_Queue (Q : in out Command_Queue);
   --  Free memory associated with Q.

   procedure Empty_Queue (Q : Command_Queue);
   --  Free all done, undone and pending actions in Q.

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command;
      High_Priority : Boolean := False);
   --  Adds Action to the Queue.
   --  If High_Priority is True, the only thing that is guaranteed is
   --  that this Action will be executed before all the actions that
   --  were enqueued with High_Priority set to False.

   function Get_Position (Queue : Command_Queue) return Integer;
   --  Return the position in the queue (see comments for
   --  Command_Queue_Record, below).

   procedure Undo (Queue : Command_Queue);
   --  Undo one action from the queue.

   procedure Redo (Queue : Command_Queue);
   --  Redo one action from the queue.

   function Undo_Queue_Empty (Queue : Command_Queue) return Boolean;
   function Redo_Queue_Empty (Queue : Command_Queue) return Boolean;
   --  These function indicate whether the undo and redo queues for
   --  the Queue are empty.

   procedure Add_Consequence_Action
     (Item   : access Root_Command'Class;
      Action : access Root_Command'Class);
   --  Add an action that will be enqueued if Item executes successfully.
   --  See comments for Add_Alternate_Action.

   procedure Add_Alternate_Action
     (Item   : access Root_Command'Class;
      Action : access Root_Command'Class);
   --  Add an action that will be enqueued if execution of Item is
   --  unsuccessfull.
   --
   --  Commands associated to a command using Add_Consequence_Action
   --  and Add_Alternate_Action are dealt with in the followin way:
   --   - if they are actually executed, then they are enqueued to the
   --  queue as normal commands, and are executed immediately after
   --  Item finishes. They are accessible through Undo/Redo, and memory
   --  associated to them is freed when the queue is freed.
   --   - if they are not executed, then the memory associated to them
   --  is freed when Item finishes.

   procedure Destroy (X : in out Command_Access);
   --  Free memory associated with X.

   package Command_Queues is
     new Generic_List (Command_Access, Free => Destroy);

   function Get_Consequence_Actions
     (Item : access Root_Command'Class) return Command_Queues.List;
   --  Get the consequence actions for Item.
   --  Result must not be freed by the user.

   function Get_Alternate_Actions
     (Item : access Root_Command'Class) return Command_Queues.List;
   --  Get the alternate actions for Item.
   --  Result must not be freed by the user.

   procedure Free_Consequence_Actions
     (Item      : access Root_Command'Class;
      Free_Data : Boolean);
   procedure Free_Alternate_Actions
     (Item      : access Root_Command'Class;
      Free_Data : Boolean);
   --  Free memory associated to the consequence/alternate commands of Item.
   --  If Free_Data is False, the command will be marked as having no
   --  consequence/alternate actions, but the actual commands will not be
   --  freed.

   procedure Add_Queue_Change_Hook
     (Queue      : Command_Queue;
      Command    : Command_Access;
      Identifier : String);
   --  Set a command that will be executed every time that the state of the
   --  queue changes.
   --  Command queue change hooks are associated to an identifier.
   --  If a command associated to Identifier already exists, it is freed
   --  and replaced by Command.
   --  The caller is responsible for freeing Command when it is no longer
   --  used.

   procedure Launch_Synchronous
     (Command : Command_Access;
      Wait    : Duration := 0.0);
   --  Launch Command and all the consequent/alternate actions recursively,
   --  then return.
   --  If Wait is non-null, delay Wait milliseconds between each execution.
   --  Launching a command this way does not free memory associated to Command.

private

   function Get_Previous_Command (Queue : Command_Queue)
     return Command_Access;
   --  Return the previous command that was executed.

   function Get_Next_Command (Queue : Command_Queue)
     return Command_Access;
   --  Return the next command to be executed.

   procedure Execute (Command : access Root_Command);
   --  Convenience subprogram: same as function Execute, but does not
   --  return any value.

   procedure Command_Finished
     (Action  : access Root_Command;
      Success : Boolean);
   --  This procedure should be called every time the execution of a Command
   --  ends. This starts the execution of the next Command in Action.Queue.
   --  Action is the Action that has just finished. Success indicates
   --  the success of Action.

   function Get_Queue_Change_Hook
     (Queue : Command_Queue)
     return Command_Queues.List;
   --  Return the queue change hook.

   type Command_Queue_Record is record
      Command_In_Progress : Boolean := False;
      The_Queue           : Command_Queues.List;

      Undo_Queue          : Command_Queues.List;
      --  This contains the actions that have already been done,
      --  in reverse chronological order (ie, most ancient actions are
      --  at the end of the queue, recent actions are prepended at the
      --  beginning.)

      Redo_Queue          : Command_Queues.List;
      --  This contains the actions that have been done and undone.
      --  (Again, most recent additions to this queue are at its
      --  beginning.)

      Queue_Change_Hook   : Command_Queues.List;
      --  These are the actions that will be executed every time the state
      --  of the queue changes.

      Hook_Identifiers    : String_List_Utils.String_List.List;
      --  A list of identifiers corresponding to elements in Queue_Change_Hook.

      Position            : Integer := 0;
      --  The position in the queue.
      --  Equal to 0 when the queue is empty, 1 is added every time a command
      --  from this queue is executed, and 1 is substracted every time a
      --  command from this queue is undone.
   end record;
   type Command_Queue is access Command_Queue_Record;

   Null_Command_Queue : constant Command_Queue := null;

   type Command_Mode is (Normal, Done, Undone);
   --  Normal actions are enqueued normally. When they end, they are
   --  enqueued at the end of the Undo_Queue and their mode becomes
   --  "Done".
   --  When a "Done" action is enqueued, it will be undone when it's
   --  time for its execution. It will then be added to the Redo_Queue
   --  and its mode will be Undone. When executing an Undone action,
   --  it will go to the Undo_Queue and become Done again, and so on.

   type Root_Command is abstract tagged limited record
      Queue              : Command_Queue;
      Next_Commands      : Command_Queues.List;
      Alternate_Commands : Command_Queues.List;
      Mode               : Command_Mode := Normal;

      Progress           : Progress_Record;
      --  The current progress of the command.

      --  The following booleans are used to avoid cases when execution of
      --  a command might cause this command to be destroyed in the process.

      Do_Not_Free : Boolean := False;
      --  Used to indicate that the command is currently being used
      --  and should not be freed.

      To_Be_Freed : Boolean := False;
      --  Indicates that an attempt to free the command was made
      --  when Do_Not_Free was set, and that the command should be freed
      --  as soon as possible.
   end record;

   pragma Inline (Undo_Queue_Empty);
   pragma Inline (Redo_Queue_Empty);

end Commands;
