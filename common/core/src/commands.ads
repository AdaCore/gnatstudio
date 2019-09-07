------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This package offers a mechanism for defining simple commands and
--  executing them sequentially.
--
--  In order to define new commands, you must override Root_Command
--  and provide implementation for the function Execute (and for the
--  function Undo if you want your command to be undoable).
--
--  Commands are executed either directly (see Launch_Synchronous), or
--  through a Command_Queue, or through the Task_Manager.
--  See details below.
--
--  One very important point: the implementation of the function
--  Execute must call Command_Finished when the action is completed,
--  since this is what triggers the execution of the next action in
--  queue.
--
--  Commands can be asynchronous, ie Execute may return even though
--  the action is not finished.

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.Refcount;

private with Ada.Finalization;

package Commands is

   type Root_Command is abstract tagged limited private;
   type Command_Access is access all Root_Command'Class;

   type Progress_Activity is (Running, Stalled, Unknown);

   type Progress_Record is record
      Activity : Progress_Activity := Unknown;
      --  The current activity type

      Current  : Natural := 0;
      --  The current progress indicator. When Current = Total, the command
      --  is assumed to be finished.

      Total    : Natural := 1;
      --  The total progress indicator
   end record;

   function Name (Command : access Root_Command) return String;
   --  Gives a description of the command

   function Progress (Command : access Root_Command) return Progress_Record;
   --  Return the current progress of the command

   procedure Set_Progress
     (Command  : access Root_Command;
      Progress : Progress_Record);
   --  Set the progress of Command

   procedure Interrupt (Command : in out Root_Command) is null;
   --  Called when the Command is explicitely interrupted

   type Command_Return_Type is
     (Success,
      --  The command terminated with success

      Failure,
      --  The command terminated and failed

      Execute_Again
      --  The command should be executed again as soon as possible. This value
      --  might also means that the command has never been executed.
      );

   function Execute
     (Command : access Root_Command) return Command_Return_Type is abstract;
   --  Executes Command. Return value indicates whether the operation was
   --  successful.
   --  IMPORTANT: every implementation for Execute must guarantee
   --  that Command_Finished will be called when the action is
   --  completed. See comments above.
   --
   --  Do not execute this function directly. You should instead use
   --  Launch_Synchronous or the task manager, or a command queue,
   --  to execute a command.

   function Undo (Command : access Root_Command) return Boolean;
   --  Undo a Command. Return value indicates whether the operation was
   --  successful.
   --  IMPORTANT: at the end of undoing, Command_Finished must be
   --  called. See above for details.

   procedure Primitive_Free (X : in out Root_Command) is null;
   --  Free memory associated to X.
   --  Do not call directly.

   procedure Ref   (Command : access Root_Command'Class);
   procedure Unref (Command : in out Command_Access);
   --  This temporarily prevent freeing the command because it might still be
   --  in use (in particular this is used for global commands used in actions
   --  and menus). When Unref is called, if the refcount reaches 0 then the
   --  command will be freed automatically (this depends on whether we tried
   --  to free it while a refcount was not null).

   package Command_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Command_Access);

   procedure Free (List : in out Command_Lists.List);
   --  Call Unref on all elements in List and then clears List

   procedure Next (List : in out Command_Lists.List);
   --  Call Unref on the first element in List and then deletes it

   -------------------------
   -- Executing a command --
   -------------------------

   procedure Launch_Synchronous
     (Command : access Root_Command'Class;
      Wait    : Duration := 0.0);
   --  Launch Command then return.
   --  If Wait is non-null, delay Wait milliseconds between each execution.
   --  This can be used when the command executes an external process, to give
   --  it more CPU time.
   --
   --  Launching a command this way does not free memory associated to Command.
   --  WARNING: It is not possible to execute through Launch_Synchronous a
   --  command that causes its own destruction.
   --
   --  The command and its consequence actions are not modified. As a result,
   --  you can execute the same command multiple times through calls to
   --  Launch_Synchronous.

   -------------------
   -- Command_Queue --
   -------------------
   --  Command queues are used to store lists of commands, mostly for
   --  Undo/Redo purposes.
   --  They are also used if you need to execute several commands one after
   --  the other, when these commands do not have strong links one to another.

   type Command_Queue is private;
   Null_Command_Queue : constant Command_Queue;

   function New_Queue return Command_Queue;
   --  Create a new empty Command_Queue

   procedure Empty_Queue (Q : Command_Queue);
   --  Free all done, undone and pending actions in Q

   ----------------------
   -- Command grouping --
   ----------------------

   --  A Queue contains a series of commands. Sometimes it is useful to
   --  "group" a number of commands that follow each other, so that they
   --  are all executed together when performing undo/redo.
   --
   --  For instance in this queue:
   --
   --       Command      C1 ->  C2 ->  C3 ->  C4
   --       group         A      B      B      C
   --
   --  commands C2 and C3 are grouped together for the purposes of undo/redo.
   --
   --  By default, each command which is enqueued is given its own group.
   --
   --  There are two mechanisms for controlling how commands get grouped.
   --  You can either
   --       - manually create a new group, and enqueue a number of commands
   --         in this group, knowing that these commands will be guaranteed
   --         to be in a different group than commands before and commands
   --         after the group. This is done by calling New_Group.
   --
   --       - enqueue a number of commands to the current group. This is done
   --         by calling Current_Group.

   procedure Start_Group (Q : Command_Queue);
   --  Start a group of commands. The commands appended to the queue will be
   --  considered as part of a same group, until End_Group is called.
   --  One call to End_Group must be made for each call to Start_Group.
   --  This has no effect on Null_Command_Queue.

   procedure End_Group (Q : Command_Queue);
   --  Ends grouping of commands

   type Group_Block is limited private
     with Warnings => Off;
   --  Deactivate warnings so we can use it as a declare block guard without
   --  using it.

   function New_Group (Q : Command_Queue) return Group_Block;
   --  Creates a new undo/redo group controlled by a Group_Block.
   --  The group is left when the Group_Block is finalized, so this can be
   --  used this way:
   --      declare
   --          G : constant Group_Block := Group (Queue);
   --      begin
   --          (some actions appended to the queue, belonging to the group)
   --      end; --  the block is released here

   function Current_Group (Q : Command_Queue) return Group_Block;
   --  Creates a new undo/redo group controlled by a Group_Block, similarly
   --  to New_Group, except that, in this case, commands will be added to
   --  the current group.

   procedure Change_Group (Queue : Command_Queue);
   --  Change the current group, so that following actions are not grouped
   --  in the same undo/redo group. This has no effect if the queue is inside
   --  an undo/redo group (ie, if there is currently a Group_Block created by
   --  New_Group or Current_Group.

   procedure Enqueue
     (Queue         : Command_Queue;
      Action        : access Root_Command'Class);
   --  Adds Action to the Queue, and start executing the command immediately
   --  if the queue is empty, or after all commands already in the queue.
   --  The execution is by default synchronous, ie this call will only return
   --  when the command has finished executing. However, it can be made
   --  asynchronous if the command registers an Idle callback.
   --
   --  As a side effect, when the command is executed, its consequence actions
   --  are modified, so you cannot execute the same command exactly the same
   --  way a second time.
   --
   --  This command steals a reference to Action, so that when the queue
   --  finishes its execution the command is freed. If you need to keep a
   --  reference to the command, you should call Ref.

   function Get_Position (Queue : Command_Queue) return Integer;
   --  Return the position in the queue (see comments for
   --  Command_Queue_Record, below).

   procedure Undo (Queue : Command_Queue);
   --  Undo one action from the queue

   procedure Redo (Queue : Command_Queue);
   --  Redo one action from the queue

   function Undo_Queue_Empty (Queue : Command_Queue) return Boolean;
   function Redo_Queue_Empty (Queue : Command_Queue) return Boolean;
   --  These function indicate whether the undo and redo queues for
   --  the Queue are empty.

   procedure Add_Queue_Change_Hook
     (Queue      : Command_Queue;
      Command    : Command_Access;
      Identifier : String);
   --  Set a command that will be executed every time the state of the queue
   --  changes.
   --  Command queue change hooks are associated to an identifier.
   --  If a command associated to Identifier already exists, it is freed
   --  and replaced by Command.
   --  The caller is responsible for freeing Command when it is no longer
   --  used.
   --  This is useful for implementing Undo/Redo buttons in a graphical
   --  interface, for instance.

   ---------------------
   -- Debug functions --
   ---------------------

   --  These functions are useful for debugging purposes, but should not be
   --  called in production.

   function Debug_Get_Undo_Queue
     (Q : Command_Queue) return Command_Lists.List;
   --  Return the undo queue

   function Debug_Get_Redo_Queue
     (Q : Command_Queue) return Command_Lists.List;
   --  Return the redo queue

   function Debug_Get_Group
     (C : Command_Access) return Natural;
   --  Return the command's action group

private

   function Get_Previous_Command (Queue : Command_Queue)
     return Command_Access;
   --  Return the previous command that was executed

   function Get_Next_Command (Queue : Command_Queue)
     return Command_Access;
   --  Return the next command to be executed

   procedure Execute (Command : access Root_Command);
   --  Convenience subprogram: same as function Execute, but does not
   --  return any value.

   procedure Command_Finished_Status
     (Action  : access Root_Command'Class;
      Success : in out Boolean);
   procedure Command_Finished
     (Action  : access Root_Command'Class;
      Success : Boolean);
   --  This procedure should be called every time the execution of a Command
   --  ends. This starts the execution of the next Command in Action.Queue.
   --  Action is the Action that has just finished. Success indicates
   --  the success of Action.

   type Identifier_And_Command is record
      Identifier : Ada.Strings.Unbounded.Unbounded_String;
      Command    : Command_Access;
   end record;

   package Identifier_And_Command_List is new
     Ada.Containers.Doubly_Linked_Lists (Identifier_And_Command);

   type Queue_Internal is record
      Command_In_Progress : Boolean := False;
      Stored_Status       : Boolean := True;
      --  Status stored for a group fail set of actions

      The_Queue           : Command_Lists.List;

      Undo_Queue          : Command_Lists.List;
      --  This contains the actions that have already been done,
      --  in reverse chronological order (ie, most ancient actions are
      --  at the end of the queue, recent actions are prepended at the
      --  beginning).

      Redo_Queue          : Command_Lists.List;
      --  This contains the actions that have been done and undone.
      --  (Again, most recent additions to this queue are at its
      --  beginning).

      Queue_Change_Hook   : Identifier_And_Command_List.List;
      --  These are the actions that will be executed every time the state
      --  of the queue changes.

      Position            : Integer := 0;
      --  The position in the queue.
      --  Equal to 0 when the queue is empty, 1 is added every time a command
      --  from this queue is executed, and 1 is substracted every time a
      --  command from this queue is undone.

      Group_Level         : Natural := 0;
      --  Indicates the current group level. 0 indicates that the commands
      --  are independant, >0 indicates that the commands being added to the
      --  queue are part of a group.

      Current_Group_Number     : Natural := 0;
      --  Indicates the number of the current group. This is used to
      --  distinguish between possible consecutive groups.
   end record;

   procedure Free_Queue (Q : in out Queue_Internal);
   --  Free memory associated to Q

   package Command_Queues is new GNATCOLL.Refcount.Shared_Pointers
     (Element_Type => Queue_Internal,
      Release      => Free_Queue);

   type Command_Queue is record
      Ref : Command_Queues.Ref;
   end record;

   Null_Command_Queue : constant Command_Queue :=
     (Ref => Command_Queues.Null_Ref);

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
      Mode               : Command_Mode := Normal;
      Group_Fail         : Boolean := False;

      Progress           : Progress_Record;
      --  The current progress of the command

      Ref_Count          : Natural := 1;
      --  Used to indicate that the command is currently being used
      --  and should not be freed. A command can only be freed when this is 0.
      --  Some global commands used in menus and actions have this increased
      --  by one, and some uses of commands might temporarily increase this
      --  count.

      Group              : Natural := 0;
      --  The group the command belongs to. 0 indicates that the command does
      --  not correspond to a group.
   end record;

   pragma Inline (Undo_Queue_Empty);
   pragma Inline (Redo_Queue_Empty);

   type Group_Block is new Ada.Finalization.Limited_Controlled with record
      Queue              : Command_Queue;
      Increments_Counter : Boolean := False;
      --  Whether to increment the group counter at creation and finalization
      --  of this block.

      Already_Left       : Boolean := False;
      --  Whether we have already finalized this block
   end record;
   overriding procedure Finalize (Self : in out Group_Block);

end Commands;
