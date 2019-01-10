------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

package GPS.Scripts.Commands is

   type Scheduled_Command is new Root_Command with private;
   type Scheduled_Command_Access is access all Scheduled_Command'Class;
   --  This command encloses any command scheduled or running in the task
   --  manager

   overriding function Name (Command : access Scheduled_Command) return String;
   overriding procedure Interrupt (Command : in out Scheduled_Command);
   overriding procedure Set_Progress
     (Command : access Scheduled_Command; Progress : Progress_Record);
   overriding function Progress
     (Command : access Scheduled_Command) return Progress_Record;
   overriding function Execute
     (Command : access Scheduled_Command) return Command_Return_Type;
   overriding function Undo (This : access Scheduled_Command) return Boolean;
   overriding procedure Primitive_Free (Command : in out Scheduled_Command);
   --  See inherited documentation

   function Create_Wrapper
     (Command : access Root_Command'Class)
      return Scheduled_Command_Access;
   --  Create a new wrapper

   function Get_Command (Command : access Scheduled_Command'Class)
      return Command_Access;
   --  Return the command associated to this scheduled command

   -------------------------------------
   -- Support for scripting languages --
   -------------------------------------

   function Get_Command
      (Data       : Callback_Data'Class;
       Nth        : Positive;
       Allow_Null : Boolean := False)
      return Scheduled_Command_Access;
   --  Return the command given as the nth argument to data

   procedure Set_Command
      (Inst    : Class_Instance;
       Command : not null access Scheduled_Command'Class);
   function Get_Instance
      (Command : not null access Scheduled_Command'Class;
       Script  : not null access Scripting_Language_Record'Class;
       Class_To_Create : String := "")
      return Class_Instance;
   --  Wrap the command in a python instance. The same instance is
   --  always reused for a given command. Its class can be
   --  overridden via the Class_To_Create parameter (it defaults to
   --  "Command").

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class);
   --  Add script commands for GPS.Command class.

private

   Command_Class_Name : constant String := "Command";

   type Command_Script_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : Command_Script_Proxy) return String
      is (Command_Class_Name) with Inline;

   type Scheduled_Command is new Root_Command with record
      Command         : Command_Access;
      Instances       : Command_Script_Proxy;
   end record;

end GPS.Scripts.Commands;
