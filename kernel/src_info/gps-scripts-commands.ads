------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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
--  Implementation of GPS.Command class

with Commands;         use Commands;
with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package GPS.Scripts.Commands is

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
   --  See inherited documentation

   function Create_Wrapper
     (Command         : access Root_Command'Class;
      Destroy_On_Exit : Boolean) return Scheduled_Command_Access;
   --  Create a new wrapper

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

   function Get_Command_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type;
   --  Return Class_Type for GPS.Command class

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class);
   --  Add script commands for GPS.Command class.

private

   type Scheduled_Command is new Root_Command with record
      Command         : Command_Access;
      Destroy_On_Exit : Boolean;
      Instances       : GNATCOLL.Scripts.Instance_List;
      Is_Dead         : Boolean := False;
   end record;

end GPS.Scripts.Commands;
