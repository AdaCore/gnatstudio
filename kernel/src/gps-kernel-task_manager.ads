-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2005                      --
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

--  This package initializes the GPS Task Manager, and provides menus
--  to display the Task Manager Interface.

with Commands;           use Commands;
with Task_Manager;       use Task_Manager;
with Generic_List;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;

package GPS.Kernel.Task_Manager is

   use Commands.Command_Queues;

   type Scheduled_Command is new Root_Command with private;
   --  This command encloses any command scheduled or running in the task
   --  manager

   type Scheduled_Command_Access is access all Scheduled_Command'Class;

   function Name (Command : access Scheduled_Command) return String;

   procedure Interrupt (Command : in out Scheduled_Command);

   procedure Set_Progress
     (Command : access Scheduled_Command; Progress : Progress_Record);

   function Progress
     (Command : access Scheduled_Command) return Progress_Record;

   function Execute
     (Command : access Scheduled_Command) return Command_Return_Type;

   function Undo (This : access Scheduled_Command) return Boolean;

   procedure Free (Command : in out Scheduled_Command);
   --  See inhertited documentation

   function Get_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class)
      return Class_Instance;
   --  Returns the class instance associated to this command for the given
   --  scripting language. Create one when needed.

   procedure Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True);
   --  Add a command to the Task_Manager.
   --  If Queue_Id is not empty, the queue will be appended at the end of the
   --  queue corresponding to the Id if it exists, or a new queue with this
   --  Id will be created.
   --  If Active is True, the command will be launched in active mode,
   --  otherwise it will be launched in background mode.
   --  If Show_Bar is True, a progress bar will be displayed for this command,
   --  otherwise it will only be visible in the Task Manager window.
   --  Memory associated to Command will be freed by the Task Manager
   --  after execution, unless Destroy_On_Exit is false.
   --  See comments in task_manager.ads for details.

   procedure Interrupt_Queue
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Command_Access);
   --  Interrupt the Queue that contains Command.
   --  Do nothing if there is no such queue.

   procedure Interrupt_Latest_Task
     (Kernel : access Kernel_Handle_Record'Class);
   --  Interrupt the last command that was started in the task manager

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the task manager module graphical entry points

   function Get_Task_Manager
     (Kernel : access Kernel_Handle_Record'Class) return Task_Manager_Access;
   --  Return the GPS task manager.

private

   type Instance_Item is record
      Script   : GPS.Kernel.Scripts.Scripting_Language;
      Instance : GPS.Kernel.Scripts.Class_Instance;
   end record;
   --  This type is used to store the association between a instance item and
   --  a script, for a given command.

   procedure Free (Item : in out Instance_Item);
   --  Free the memory of the class instance in this class item.

   package Instance_List is new Generic_List (Instance_Item);
   use Instance_List;

   type Scheduled_Command is new Root_Command with record
      Command         : Command_Access;
      Destroy_On_Exit : Boolean;

      Instances       : Instance_List.List;
      --  List of instances for the given command, created only when needed,
      --  one per script language.
   end record;

end GPS.Kernel.Task_Manager;
