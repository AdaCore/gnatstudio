-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
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

--  This package handles the GUI part of the task manager.

with Gtk.Box;                  use Gtk.Box;
with Gtkada.Tree_View;         use Gtkada.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;

with Ada.Unchecked_Deallocation;

package Task_Manager.GUI is

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with private;
   type Task_Manager_Interface is access all
     Task_Manager_Interface_Record'Class;

   procedure Gtk_New
     (View    : out Task_Manager_Interface;
      Manager : Task_Manager_Access);
   --  Create a new Task_Manager_Interface;

   procedure Initialize
     (View    : access Task_Manager_Interface_Record'Class;
      Manager : Task_Manager_Access);
   --  Internal initialization procedure.

   procedure Refresh (View : access Task_Manager_Interface_Record'Class);
   --  Refresh the information in View from the Task_Manager.

   procedure Refresh_Command
     (View  : access Task_Manager_Interface_Record'Class;
      Index : Integer);
   --  Refresh only one command line.
   --  Index corresponds to the index of the command in View.Manager.Queues

private

   type Iter_Array is array (Natural range <>) of Gtk_Tree_Iter;
   type Iter_Array_Access is access Iter_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Iter_Array, Iter_Array_Access);

   type Task_Manager_Interface_Record is new Gtk_Hbox_Record with record
      Tree    : Tree_View;
      Manager : Task_Manager_Access;

      Lines   : Iter_Array_Access;
   end record;

end Task_Manager.GUI;
