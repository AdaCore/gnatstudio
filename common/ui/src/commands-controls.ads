------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

--  This package enables the user to connect Undo / Redo buttons to a
--  queue of commands

with Gtk.Widget;   use Gtk.Widget;

package Commands.Controls is

   type Undo_Redo_Information is private;
   type Undo_Redo is access Undo_Redo_Information;

   procedure Set_Undo_Redo_Queue (UR : Undo_Redo; Queue : Command_Queue);
   --  Associate the state of Queue to the buttons:
   --  The sensitivity of Undo/Redo widgets (named "controls")
   --  indicate the presence of actions in the corresponding Undo/Redo Queues.
   --  Activating the controls executes the first action in the corresponding
   --  queue or the global command.

   procedure Unset_Undo_Redo_Queue (UR : Undo_Redo);
   --  Disconnect any controls that are connected to Command.

   function Get_Undo_Redo_Queue (UR : Undo_Redo) return Command_Queue;
   --  Getter for the current monitored queue

   procedure Set_Global_Command (UR : Undo_Redo; Command : Command_Access);
   --  Set a global command to be able to undo it, if null then invalidate
   --  the current global command
   --  The command will be automatically ref/decref.
   --  Will do nothing while executing the current global command.

   function Execute_Global_Command (UR : Undo_Redo) return Command_Return_Type;
   --  Safe way to execute the current global command.

   procedure Free (UR : in out Undo_Redo);
   --  Use to clean the internal data structure

   procedure Undo (UR : Undo_Redo);
   --  Undo the last command

   procedure Redo (UR : Undo_Redo);
   --  Redo the previous command

   function Can_Undo (UR : Undo_Redo) return Boolean;
   --  True if the last command can be undone

   function Can_Redo (UR : Undo_Redo) return Boolean;
   --  True if the previous command can be redone

private
   type Undo_Redo_Information is record
      Queue            : Commands.Command_Queue;
      --  The queue from which undo and redo information is read

      Global_Command   : Command_Access := null;
      --  Last global command

      Redo_Global      : Boolean := False;
      --  Can we undo or redo the last global command

      Executing_Global : Boolean := False;
      --  True when executing a global command
   end record;

end Commands.Controls;
