-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001 - 2002                     --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  This package enables the user to connect Undo / Redo buttons to a
--  queue of commands

with Gtk.Button;    use Gtk.Button;
with Gtk.Menu_Item; use Gtk.Menu_Item;

package Commands.Controls is

   procedure Set_Controls (Queue       : Command_Queue;
                           Undo_Button : Gtk_Button;
                           Redo_Button : Gtk_Button;
                           Undo_Menu_Item : Gtk_Menu_Item;
                           Redo_Menu_Item : Gtk_Menu_Item);

   procedure Unset_Controls (Queue : Command_Queue);

end Commands.Controls;
