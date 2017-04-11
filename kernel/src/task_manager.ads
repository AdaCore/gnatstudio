------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

--  This package provides a task manager with a scheduler
--  to handle asynchronous or background tasks run by GPS.
--
--  The task manager is created at GPS startup.
--
--  By default, the task manager does nothing.
--  When it is given a command (through a call to Add_Command), it will begin
--  the processing of the commands, and will run in the background until all
--  the commands are finished.

with Commands;             use Commands;
with Glib.Main;            use Glib.Main;
with GPS.Scripts.Commands; use GPS.Scripts.Commands;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Scripts;     use GNATCOLL.Scripts;

package Task_Manager is

   type Task_Manager_Record is tagged private;
   type Task_Manager_Access is access all Task_Manager_Record'Class;
   No_Task_Manager : constant Task_Manager_Access;

   procedure Add_Command
     (Manager           : not null access Task_Manager_Record;
      Command           : not null access Scheduled_Command'Class;
      Active            : Boolean;
      Show_Bar          : Boolean;
      Queue_Id          : String := "";
      Block_Exit        : Boolean := True;
      Start_Immediately : Boolean := False);
   --  Add a command to be handled by the task manager.
   --  The command will be executed at once.
   --  The progress bar will be shown for this command if Show_Bar is True.
   --  If Block_Exit is True, GPS should display a confirmation dialog if
   --  exiting while this command is running.

   procedure Interrupt_Queue
     (Manager : not null access Task_Manager_Record;
      Command : not null access Scheduled_Command'Class);
   --  Interrupt the Queue that contains Command.
   --  This interrupts the currently running command (if any); none of the
   --  subsequent commands will be executed, and all commands in the queue
   --  will be Free'd.
   --  Do nothing if there is no such queue.

   procedure Interrupt_Queue
     (Manager  : not null access Task_Manager_Record;
      Queue_Id : String);
   --  Interrupts the queue that has the given id, if any. If there is no
   --  such queue, do nothing.

   procedure Interrupt_Command
     (Manager : not null access Task_Manager_Record;
      Index   : Integer);
   --  Interrupt command referenced by Index

   function Head
     (Manager : not null access Task_Manager_Record; Id : String)
     return Scheduled_Command_Access;
   --  Return the Head command from the Queue of the given id, null if none

   procedure Interrupt_Latest_Task
      (Manager : not null access Task_Manager_Record);
   --  Interrupt the task that was started last

   procedure Interrupt_All_Tasks
      (Manager : not null access Task_Manager_Record);
   --  Interrupt all tasks

   procedure Pause_Command
     (Manager : not null access Task_Manager_Record;
      Index   : Integer);
   --  Pause command referenced by Index

   procedure Resume_Command
     (Manager : not null access Task_Manager_Record;
      Index   : Integer);
   --  Resume paused command referenced by Index

   procedure Destroy
     (Manager : Task_Manager_Access);
   --  Free all memory associated to the task manager

   type Command_Array is array (Integer range <>) of Scheduled_Command_Access;

   function Get_Scheduled_Commands
     (Manager : not null access Task_Manager_Record) return Command_Array;
   --  Return all the commands currently stored in the task manager

   function Scheduled_Command_From_Command
     (Manager : not null access Task_Manager_Record;
      Command : access Root_Command'Class)
      return Scheduled_Command_Access;
   --  Return the scheduled command that wraps the given low-level command.
   --  The task manager can only deal with commands of type Scheduled_Command.
   --  In general, this is transparent for other subprograms, but it is
   --  sometimes useful to find the scheduled command itself so that it can
   --  be interrupted, paused, or to get access to the python instances.
   --
   --  The result should not be unref-ed, and might be null.

   function Has_Queue
     (Manager  : not null access Task_Manager_Record;
      Queue_Id : String) return Boolean;
   --  Return True if a queue identified by Queue_Id is currently running or
   --  paused in the task manager.

private

   type Queue_Status is (Not_Started, Running, Paused, Completed);
   --  Not_Started: the queue has not started
   --  Running: the queue is running
   --  Paused: the queue is paused
   --  Completed: the queue is considered completed: no commands should be
   --    added to it, and the commands in the queue will be freed at the next
   --    iteration of the task manager.

   type Task_Queue_Record is record
      Status   : Queue_Status := Running;

      Queue         : Command_Lists.List;
      --  Each element is a Scheduled_Command

      Stored_Status : Command_Return_Type := Success;

      Total    : Integer := 0;
      --  The total number of items inserted so far in Queue

      Done     : Integer := 0;
      --  The number of items done in queue

      Id       : Unbounded_String := Null_Unbounded_String;

      Current_Priority : Integer := 0;

      Show_Bar     : Boolean := False;

      Block_Exit   : Boolean := True;

      Inst : Class_Instance := No_Class_Instance;
      --  The scripting instance currently wrapping this task

      To_Refresh : Boolean := False;
      --  Whether we should refresh the GUI for this queue
   end record;
   type Task_Queue_Access is access Task_Queue_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Task_Queue_Record, Task_Queue_Access);

   type Task_Queue_Array is array (Natural range <>) of Task_Queue_Access;
   type Task_Queue_Array_Access is access Task_Queue_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Task_Queue_Array, Task_Queue_Array_Access);

   type Task_Manager_Record is tagged record
      Queues               : Task_Queue_Array_Access;

      Passive_Index        : Integer := 0;
      --  Index of the first passive queue in Queues

      Minimal_Active_Priority  : Integer := 0;
      Minimal_Passive_Priority : Integer := 0;

      Prevent_Active_Reentry : Boolean := False;
      --  Flag to prevent reentry

      Active_Handler_Id        : Glib.Main.G_Source_Id := No_Source_Id;
      --  The id of the active idle callback.

      Passive_Handler_Id       : Glib.Main.G_Source_Id := No_Source_Id;
      --  The id of the passive timeout callback
   end record;

   No_Task_Manager : constant Task_Manager_Access := null;

end Task_Manager;
