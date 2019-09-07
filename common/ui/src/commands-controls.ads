------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

   type Undo_Redo_Information is record
      Queue : Commands.Command_Queue;
      --  The queue from which undo and redo information is read
   end record;

   type Undo_Redo is access Undo_Redo_Information;

   procedure Set_Undo_Redo_Queue (Queue  : Command_Queue; UR : Undo_Redo);
   --  Associate the state of Queue to the buttons:
   --  The sensitivity of Undo/Redo widgets (named "controls")
   --  indicate the presence of actions in the corresponding Undo/Redo Queues.
   --  Activating the controls executes the first action in the corresponding
   --  queue.

   procedure Unset_Undo_Redo_Queue (UR : Undo_Redo);
   --  Disconnect any controls that are connected to Command.

end Commands.Controls;
