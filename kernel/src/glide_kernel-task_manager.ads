-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This package initializes the GPS Task Manager, and provides menus
--  to display the Task Manager Interface.

with Commands; use Commands;

package Glide_Kernel.Task_Manager is

   use Commands.Command_Queues;

   procedure Launch_Background_Command
     (Kernel   : access Kernel_Handle_Record'Class;
      Command  : access Root_Command'Class;
      Active   : Boolean;
      Queue_Id : String := "";
      Destroy_On_Exit : Boolean := True);
   --  Add a command to the Task_Manager.
   --  If Queue_Id is not empty, the queue will be appended at the end of the
   --  queue corresponding to the Id if it exists, or a new queue with this
   --  Id will be created.
   --  If Active is True, the command will be launched in active mode,
   --  otherwise it will be launched in background mode.
   --  Memory associated to Command will be freed by the Task Manager
   --  after execution, unless Destroy_On_Exit is false.
   --  See comments in task_manager.ads for details.

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the task manager module graphical entry points.

end Glide_Kernel.Task_Manager;
