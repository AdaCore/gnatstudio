-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2005                       --
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

with GPS.Kernel;         use GPS.Kernel;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNAT.Sockets;       use GNAT.Sockets;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;

package Commands.Socket is

   type Socket_Command is new Root_Command with private;
   type Socket_Command_Access is access all Socket_Command;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Socket_Command_Access;
      Kernel  : Kernel_Handle;
      Command : String;
      Shell   : String := GPS_Shell_Name;
      Stream  : Stream_Access);
   --  Create a new custom command

   procedure Free (X : in out Socket_Command);
   --  Free memory associated to X

   function Execute
     (Command : access Socket_Command) return Command_Return_Type;
   --  Execute Command, and return Success if the command could be launched
   --  successfully.

private

   type Socket_Command is new Root_Command with record
      Kernel  : Kernel_Handle;
      Command : String_Access;
      Shell   : String_Access;
      Stream  : Stream_Access;
   end record;

end Commands.Socket;
