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

with Ada.Unchecked_Deallocation;

generic
   type Data_Type (<>) is private;

   Description : String;

   with procedure Free (Data : in out Data_Type) is <>;
   --  Free memory allocated to Data.

   with procedure Iterate
     (Data    : in out Data_Type;
      Command : Command_Access;
      Result  : out Command_Return_Type) is <>;
   --  Do one iteration. Initialize Data at first iteration if needed.
   --  Command points to the current command. Progress fields can be set
   --  accordingly

package Commands.Generic_Asynchronous is

   type Generic_Asynchronous_Command is new Root_Command with private;
   type Generic_Asynchronous_Command_Access is
     access all Generic_Asynchronous_Command;

   procedure Create
     (Command : out Generic_Asynchronous_Command_Access;
      Data    : in Data_Type);

   procedure Free (D : in out Generic_Asynchronous_Command);
   --  Free memory associated to D.

   function Execute
     (Command : access Generic_Asynchronous_Command)
      return Command_Return_Type;
   --  Execute Command. Will fail if Command has not been created using Create.

   function Name (Command : access Generic_Asynchronous_Command) return String;
   --  Return the name of the command.

private

   type Data_Access is access Data_Type;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Data_Type, Data_Access);

   type Generic_Asynchronous_Command is new Root_Command with record
      Data : Data_Access;
   end record;

end Commands.Generic_Asynchronous;
