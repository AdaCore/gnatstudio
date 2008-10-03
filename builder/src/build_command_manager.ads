-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Provides a high-level interface to launch commands.
--  This interface is the one that should be shared by scripts, hooks,
--  menus, toolbar buttons, etc.
--
--  See spec of Builder_Facility_Module for an overview of the build system.

with GPS.Kernel;
with Build_Configurations; use Build_Configurations;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;

package Build_Command_Manager is

   procedure Launch_Target
     (Kernel       : GPS.Kernel.Kernel_Handle;
      Registry     : Build_Config_Registry_Access;
      Target_Name  : String;
      Force_Dialog : Boolean);
   --  Launch a build of target named Target_Name

   -------------------
   -- Build_Command --
   -------------------

   --  The Build_Command acts simply as a wrapper around Launch_Target.
   --  Used for defining build actions.

   type Build_Command is new Interactive_Command with record
      Target_Name  : Unbounded_String;
      Registry     : Build_Config_Registry_Access;
      Kernel       : GPS.Kernel.Kernel_Handle;
      Force_Dialog : Boolean;
   end record;
   type Build_Command_Access is access all Build_Command'Class;

   overriding
   function Execute
     (Command : access Build_Command;
      Context : Interactive_Command_Context)
      return Command_Return_Type;
   --  See inherited documentation

   procedure Create
     (Item         : out Build_Command_Access;
      Kernel       : GPS.Kernel.Kernel_Handle;
      Registry     : Build_Config_Registry_Access;
      Target_Name  : String;
      Force_Dialog : Boolean);
   --  Create a build command
   --  Force_Dialog indicates that the command should always be launched
   --  through the interactive dialog.

end Build_Command_Manager;
