------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

--  This package defines a type of command that can have 'consequences' and/or
--  'alternates': these are lists of commands that get launched in the
--  background when the execution of the command is a success or a failure
--  respectively.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Commands;              use Commands;

package VCS.Branching_Commands is

   package Command_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Command_Access);
   use Command_Lists;

   type Branching_Command_Record is new Root_Command with private;
   type Branching_Command is access all Branching_Command_Record;

   function Create
     (Kernel   : access Kernel_Handle_Record'Class;
      Command  : Command_Access;
      Queue_ID : String) return Branching_Command;
   --  Create a Branching command wrapping Command

   overriding function Execute
     (Command : access Branching_Command_Record) return Command_Return_Type;

   procedure Add_Consequence_Action
     (Command : Branching_Command;
      Action  : Command_Access);
   --  Add Action to the list of consequences of Command

   procedure Add_Alternate_Action
     (Command : Branching_Command;
      Action  : Command_Access);
   --  Add Action to the list of alternates of Command

private

   type Branching_Command_Record is new Root_Command with record
      Kernel              : access Kernel_Handle_Record'Class;
      Command             : Command_Access;

      Queue_Id            : Unbounded_String;
      Consequence_Actions : List;
      --  Commands that will be executed if Command succeeds

      Alternate_Actions   : List;
      --  Commands that will be executed if Command fails
   end record;
end VCS.Branching_Commands;
