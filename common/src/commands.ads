-----------------------------------------------------------------------
--                          G L I D E  I I                           --
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

with Generic_List;

package Commands is

   type Root_Command is abstract tagged limited private;
   type Command_Access is access all Root_Command'Class;

   function Name (Command : access Root_Command) return String;
   --  Gives a description of the command.

   function Execute (Command : access Root_Command) return Boolean is abstract;
   --  Executes Command. Return value indicates whether the operation was
   --  successful.

   function Undo (Command : access Root_Command) return Boolean;
   --  Undo a Command. Return value indicates whether the operation was
   --  successful.

   type Command_Queue is private;

   function New_Queue return Command_Queue;

   procedure Enqueue
     (Queue  : Command_Queue;
      Action : access Root_Command);
   --  Appends Action at the end of the Queue.

   procedure Execute_Next_Action (Queue : Command_Queue);
   --  Execute the next action in the queue, or do nothing if there is none.

private

   procedure Command_Finished (Queue : Command_Queue);
   --  This procedure should be called every time the execution of a Command
   --  ends. This starts the execution of the next Command in the Queue.

   procedure Free (X : in out Command_Access);
   package Command_Queues is new Generic_List (Command_Access);

   type Command_Queue_Record is record
      Command_In_Progress : Boolean := False;
      The_Queue           : Command_Queues.List;
   end record;
   type Command_Queue is access Command_Queue_Record;

   type Root_Command is abstract tagged limited record
      Queue : Command_Queue;
   end record;

end Commands;
