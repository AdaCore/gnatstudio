------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

--  This package initializes the GPS Task Manager, and provides menus
--  to display the Task Manager Interface.

with Commands;             use Commands;
with Task_Manager;         use Task_Manager;
with GPS.Scripts.Commands; use GPS.Scripts.Commands;

package GPS.Kernel.Task_Manager is

   procedure Launch_Background_Command
     (Kernel            : access Kernel_Handle_Record'Class;
      Command           : access Root_Command'Class;
      Active            : Boolean;
      Show_Bar          : Boolean;
      Queue_Id          : String := "";
      Block_Exit        : Boolean := True;
      Start_Immediately : Boolean := False);
   --  Add a command to the Task_Manager.
   --  If Queue_Id is not empty, the queue will be appended at the end of the
   --  queue corresponding to the Id if it exists, or a new queue with this
   --  Id will be created.
   --
   --  If Active is True, the command will be launched in active mode, ie
   --  executed as fast as possible in an idle loop. Otherwise, it is launched
   --  in background mode, ie executed more slowly in a timeout. If a queue is
   --  specified, the command is put in front of the queue if Active is false,
   --  and at the back otherwise.
   --
   --  If Active or Start_Immediately, the command is started immediately.
   --  Otherwise, it will be executed only by the background loop (idle or
   --  timeout).
   --
   --  If Show_Bar is True, a progress bar will be displayed for this command,
   --  otherwise it will only be visible in the Task Manager window.
   --
   --  If Block_Exit is True, GPS will show a confirmation dialog if user exits
   --  while this command is running.
   --  See comments in task_manager.ads for details.
   --
   --  Memory associated to Command will be freed by the Task Manager
   --  after execution.

   function Launch_Background_Command
     (Kernel            : access Kernel_Handle_Record'Class;
      Command           : access Root_Command'Class;
      Active            : Boolean;
      Show_Bar          : Boolean;
      Queue_Id          : String := "";
      Block_Exit        : Boolean := True;
      Start_Immediately : Boolean := False) return Scheduled_Command_Access;
   --  Same as above, but returns the command actually inserted in the task
   --  manager.

   function Has_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String) return Boolean;
   --  Return True if a queue identified by Queue_Id is currently running or
   --  paused in the task manager.

   procedure Interrupt_Queue
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : Scheduled_Command_Access);
   procedure Interrupt_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String);
   --  Interrupt the Queue that contains Command.
   --  Do nothing if there is no such queue.

   procedure Interrupt_Latest_Task
     (Kernel : access Kernel_Handle_Record'Class);
   --  Interrupt the last command that was started in the task manager

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the task manager module graphical entry points

   procedure Set_Task_Manager
     (Kernel  : access Kernel_Handle_Record'Class;
      Manager : Task_Manager_Access);
   function Get_Task_Manager
     (Kernel : access Kernel_Handle_Record'Class) return Task_Manager_Access;
   --  Return the GPS task manager

end GPS.Kernel.Task_Manager;
