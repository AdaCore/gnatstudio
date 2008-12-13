-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

--  This package provides a task manager with a scheduler
--  to handle asynchronous or background tasks run by GPS.
--
--  The task manager is created upon GPS startup.
--
--  By default, the task manager does nothing.
--  When it is given a command (through a call to Add_Command), it will begin
--  the processing of the commands, and will run in the background until all
--  the commands are finished.

with Gtk.Box;
with Gtk.Progress_Bar;
with Gtk.Widget;  use Gtk.Widget;

with Commands;    use Commands;
with GNAT.Strings;

with Ada.Unchecked_Deallocation;

package Task_Manager is

   type Task_Manager_Record is private;
   type Task_Manager_Access is access all Task_Manager_Record;

   procedure Add_Command
     (Manager    : Task_Manager_Access;
      Command    : Command_Access;
      Active     : Boolean;
      Show_Bar   : Boolean;
      Queue_Id   : String := "";
      Block_Exit : Boolean := True);
   --  Add a command to be handled by the task manager.
   --  The command will be executed at once.
   --  The progress bar will be shown for this command if Show_Bar is True.
   --  If Block_Exit is True, GPS should display a confirmation dialog if
   --  exiting while this command is running.

   procedure Interrupt_Queue
     (Manager : Task_Manager_Access;
      Command : Command_Access);
   --  Interrupt the Queue that contains Command.
   --  Do nothing if there is no such queue.

   function Head
     (Manager : Task_Manager_Access; Id : String) return Command_Access;
   --  Return the Head command from the Queue of the given id, null if none

   procedure Interrupt_Latest_Task (Manager : Task_Manager_Access);
   --  Interrupt the task that was started last

   procedure Interrupt_All_Tasks (Manager : Task_Manager_Access);
   --  Interrput all tasks

   procedure Set_Progress_Area
     (Manager : Task_Manager_Access;
      Area    : Gtk.Box.Gtk_Hbox);
   --  Indicate an area in which progress bars can be displayed

   procedure Set_Busy_Commands
     (Manager      : Task_Manager_Access;
      Push_Command : Command_Access;
      Pop_Command  : Command_Access);
   --  Set the commands used to push/pop the busy state

   procedure Destroy
     (Manager : Task_Manager_Access);
   --  Free all memory associated to the task manager

   function Get_GUI (Manager : Task_Manager_Access) return Gtk_Widget;
   procedure Set_GUI
     (Manager : Task_Manager_Access;
      GUI     : Gtk_Widget);
   --  Get and set the active graphical interface for Manager

   function Has_Running_Commands
     (Manager         : Task_Manager_Access;
      Consider_Silent : Boolean) return Boolean;
   --  Return True if there is one or more command that is Running or Paused.
   --  If Consider_Silent is True, then all tasks will be considered,
   --  otherwise tasks that are considered as silent (ie, that have no
   --  associated progress bar) are not taken into account.

   type Command_Array is array (Integer range <>) of Command_Access;

   function Get_Scheduled_Commands
     (Manager : Task_Manager_Access) return Command_Array;
   --  Return all the commands currently stored in the task manager

private

   procedure Run
     (Manager : Task_Manager_Access;
      Active  : Boolean);
   --  Runs the task manager, if it is not already running

   type Queue_Status is (Not_Started, Running, Paused, Completed, Interrupted);
   --  ??? The Interrupted status should probably be removed since interrupted
   --  tasks are not to b e displayed in the task manager.

   type Task_Queue_Record is record
      Status   : Queue_Status := Running;

      Queue         : Command_Queues.List;
      Stored_Status : Command_Return_Type := Success;

      Total    : Integer := 0;
      --  The total number of items inserted so far in Queue

      Done     : Integer := 0;
      --  The number of items done in queue

      Id       : GNAT.Strings.String_Access;

      Priority : Natural := 2;
      --  A priority of 1 is high priority,
      --                2 is normal priority,
      --                3 is low priority

      Current_Priority : Integer := 0;

      Need_Refresh : Boolean := False;

      Show_Bar     : Boolean := False;

      Block_Exit   : Boolean := True;

      Bar          : Gtk.Progress_Bar.Gtk_Progress_Bar := null;
   end record;
   type Task_Queue_Access is access Task_Queue_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Task_Queue_Record, Task_Queue_Access);

   type Task_Queue_Array is array (Natural range <>) of Task_Queue_Access;
   type Task_Queue_Array_Access is access Task_Queue_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Task_Queue_Array, Task_Queue_Array_Access);

   type Task_Manager_Record is record
      Queues               : Task_Queue_Array_Access;

      Passive_Index        : Integer := 0;
      --  Index of the first passive queue in Queues

      Running_Active       : Boolean := False;
      --  Whether the active loop is running

      Running_Passive      : Boolean := False;

      Minimal_Active_Priority  : Integer := 0;
      Minimal_Passive_Priority : Integer := 0;

      Need_Global_Refresh  : Boolean := True;

      --  Graphical elements

      GUI                  : Gtk_Widget       := null;
      Progress_Area        : Gtk.Box.Gtk_Hbox := null;
      Referenced_Command   : Integer := -1;
      --  The command currently referenced by the contextual menu.
      --  This is an index in Queues, set to <0 when the command is not valid
      --  or the menu not created.

      Push_Command, Pop_Command : Command_Access;
      --  Commands used to push/pop the "busy" state
   end record;

end Task_Manager;
