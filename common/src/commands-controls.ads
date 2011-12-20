------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Tool_Button;         use Gtk.Tool_Button;

package Commands.Controls is

   type Undo_Redo_Information is record
      Undo_Button    : Gtk.Tool_Button.Gtk_Tool_Button;
      Redo_Button    : Gtk.Tool_Button.Gtk_Tool_Button;

      Undo_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Redo_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;

      Undo_Button_Handler_ID    : Handler_Id;
      Redo_Button_Handler_ID    : Handler_Id;
      Undo_Menu_Item_Handler_ID : Handler_Id;
      Redo_Menu_Item_Handler_ID : Handler_Id;
   end record;

   type Undo_Redo is access Undo_Redo_Information;

   function Set_Controls
     (Queue  : Command_Queue;
      UR     : Undo_Redo) return Command_Access;
   --  Associate the state of Queue to the buttons:
   --  The sensitivity of Undo/Redo widgets (named "controls")
   --  indicate the presence of actions in the corresponding Undo/Redo Queues.
   --  Activating the controls executes the first action in the corresponding
   --  queue.

   procedure Unset_Controls (Command : Command_Access);
   --  Disconnect any controls that are connected to Command.
   --  Command must be a Queue_Change_Access.

end Commands.Controls;
