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

--  This package provides a task manager with a scheduler
--  to handle asynchronous or background tasks run by GPS.
--
--  The task manager is created upon GPS startup.
--
--  By default, the task manager does nothing.
--  When it is given a command (through a call to Add_Command), it will begin
--  the processing of the commands, and will run in the background until all
--  the commands are finished.

with Gtk.Box;
with Gtk.Progress_Bar;
with Gtk.Widget;  use Gtk.Widget;

with Commands;    use Commands;
with Basic_Types; use Basic_Types;

with Ada.Unchecked_Deallocation;

package Task_Manager is

   type Task_Manager_Record is private;
   type Task_Manager_Access is access Task_Manager_Record;

   procedure Add_Command
     (Manager  : Task_Manager_Access;
      Command  : Command_Access;
      Active   : Boolean;
      Queue_Id : String := "");
   --  Add a command to be handled by the task manager.
   --  The command will be executed at once.

   procedure Set_Progress_Area
     (Manager : Task_Manager_Access;
      Area    : Gtk.Box.Gtk_Hbox);
   --  Indicate an area in which progress bars can be displayed.

private
   type Queue_Status is (Not_Started, Running, Paused, Completed);

   type Task_Queue_Record is record
      Status   : Queue_Status := Running;

      Queue    : Command_Queues.List;

      Total    : Integer := 0;
      --  The total number of items inserted so far in Queue.

      Done     : Integer := 0;
      --  The number of items done in queue.

      Id       : String_Access;

      Priority : Natural := 2;
      --  A priority of 1 is high priority,
      --                2 is normal priority,
      --                3 is low priority

      Current_Priority : Integer := 0;

      Need_Refresh : Boolean := False;

      Bar          : Gtk.Progress_Bar.Gtk_Progress_Bar := null;
   end record;

   type Task_Queue_Array is array (Natural range <>) of Task_Queue_Record;
   type Task_Queue_Array_Access is access Task_Queue_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Task_Queue_Array, Task_Queue_Array_Access);

   type Task_Manager_Record is record
      Queues               : Task_Queue_Array_Access;

      Passive_Index        : Integer := 0;
      --  Index of the first passive queue in Queues.

      Running_Active       : Boolean := False;
      --  Whether the active loop is running.

      Running_Passive      : Boolean := False;

      Minimal_Active_Priority  : Integer := 0;
      Minimal_Passive_Priority : Integer := 0;

      Need_Global_Refresh  : Boolean := True;

      --  Graphical elements.

      GUI                  : Gtk_Widget := null;
      Progress_Area        : Gtk.Box.Gtk_Hbox   := null;
   end record;

end Task_Manager;
