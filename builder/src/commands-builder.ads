-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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

--  This package handles build commands.

with Glide_Kernel.Timeout; use Glide_Kernel.Timeout;

package Commands.Builder is

   type Build_Command is new Root_Command with private;
   type Build_Command_Access is access all Build_Command;

   procedure Create
     (Item : out Build_Command_Access;
      Data : Process_Data);
   --  Create a new Build_Command.

   procedure Free (D : in out Build_Command);
   --  Free memory associated to D.

   function Execute
     (Command : access Build_Command) return Command_Return_Type;
   --  Execute Command, and launch the associated Handler.
   --  See comments for Create.

   function Name (Command : access Build_Command) return String;
   --  Return a description of the command.

private

   type Build_Command is new Root_Command with record
      Data : Process_Data;
   end record;

end Commands.Builder;
