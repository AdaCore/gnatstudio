-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                             AdaCore                               --
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

   procedure Set_Controls
     (Queue  : Command_Queue;
      UR     : Undo_Redo);
   --  Associate the state of Queue to the buttons:
   --  The sensitivity of Undo/Redo widgets (named "controls")
   --  indicate the presence of actions in the corresponding Undo/Redo Queues.
   --  Activating the controls executes the first action in the corresponding
   --  queue.

   procedure Unset_Controls (Queue : Command_Queue);
   --  Disconnect any controls that are connected to Queue.

end Commands.Controls;
