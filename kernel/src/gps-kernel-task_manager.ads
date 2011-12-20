------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Commands;     use Commands;
with Task_Manager; use Task_Manager;
with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package GPS.Kernel.Task_Manager is

   type Scheduled_Command is new Root_Command with private;
   --  This command encloses any command scheduled or running in the task
   --  manager

   type Scheduled_Command_Access is access all Scheduled_Command'Class;

   overriding function Name (Command : access Scheduled_Command) return String;

   overriding procedure Interrupt (Command : in out Scheduled_Command);

   overriding procedure Set_Progress
     (Command : access Scheduled_Command; Progress : Progress_Record);

   overriding function Progress
     (Command : access Scheduled_Command) return Progress_Record;

   overriding function Execute
     (Command : access Scheduled_Command) return Command_Return_Type;

   overriding function Undo (This : access Scheduled_Command) return Boolean;

   overriding procedure Free (Command : in out Scheduled_Command);
   --  See inhertited documentation

   procedure Launch_Foreground_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Destroy_On_Exit : Boolean := True);
   --  Executes a command, blocking the whole GPS interface while doing so.
   --  It is recommended instead to use Launch_Background_Command, but this one
   --  is sometimes used in user's python scripts.

   procedure Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True;
      Block_Exit      : Boolean := True);
   --  Add a command to the Task_Manager.
   --  If Queue_Id is not empty, the queue will be appended at the end of the
   --  queue corresponding to the Id if it exists, or a new queue with this
   --  Id will be created.
   --  If Active is True, the command will be launched in active mode, ie
   --  executed as fast as possible in an idle loop. Otherwise, it is launched
   --  in background mode, ie executed more slowly in a timeout. If a queue is
   --  specified, the command is put in front of the queue if Active is false,
   --  and at the back otherwise.
   --  If Show_Bar is True, a progress bar will be displayed for this command,
   --  otherwise it will only be visible in the Task Manager window.
   --  Memory associated to Command will be freed by the Task Manager
   --  after execution, unless Destroy_On_Exit is false.
   --  If Block_Exit is True, GPS will show a confirmation dialog if user exits
   --  while this command is running.
   --  See comments in task_manager.ads for details.

   function Launch_Background_Command
     (Kernel          : access Kernel_Handle_Record'Class;
      Command         : access Root_Command'Class;
      Active          : Boolean;
      Show_Bar        : Boolean;
      Queue_Id        : String := "";
      Destroy_On_Exit : Boolean := True;
      Block_Exit      : Boolean := True) return Scheduled_Command_Access;
   --  Same as above, but returns the command actually inserted in the task
   --  manager.

   function Has_Queue
     (Kernel   : access Kernel_Handle_Record'Class;
      Queue_Id : String) return Boolean;
   --  Return True if a queue identified by Queue_Id is currently running or
   --  paused in the task manager.

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
   --  Return the GPS task manager

   procedure Show_Task_Manager (Kernel : Kernel_Handle);
   --  Show the GPS task manager

   function Get_Command (Command : access Scheduled_Command'Class)
      return Command_Access;
   --  Return the command associated to this scheduled command

   -------------------------------------
   -- Support for scripting languages --
   -------------------------------------

   function Get_Instance
     (Command       : access Scheduled_Command'Class;
      Language      : access Scripting_Language_Record'Class;
      Command_Class : Class_Type := No_Class)
      return Class_Instance;
   --  Returns the class instance associated to this command for the given
   --  scripting language. Create one when needed. The instance is from the
   --  class GPS.Command.
   --  Command_Class specify the actual type of the command that have to be
   --  created if needed. It has to be a child of type Command. If its type is
   --  No_Class, then default command class will be used.

   procedure Set_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class;
      Instance : Class_Instance);
   --  Set the instance corresponding to the given language to the given
   --  command. This assumes that no instance has previously been set for the
   --  given language.

   procedure Remove_Instance
     (Command  : access Scheduled_Command'Class;
      Language : access Scripting_Language_Record'Class);
   --  Removes the instance corresponding to the given language from the list

   function Get_Data
     (Instance : GNATCOLL.Scripts.Class_Instance)
      return Scheduled_Command_Access;
   --  Return the command stored in Instance. Instance must be of the class
   --  GPS.Command.

private

   type Scheduled_Command is new Root_Command with record
      Command         : Command_Access;
      Destroy_On_Exit : Boolean;
      Instances       : GNATCOLL.Scripts.Instance_List;
      Is_Dead         : Boolean := False;
   end record;

end GPS.Kernel.Task_Manager;
