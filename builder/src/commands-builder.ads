-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
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

with GPS.Kernel.Timeout; use GPS.Kernel.Timeout;
with VFS;                  use VFS;
with GNAT.OS_Lib;

package Commands.Builder is

   Error_Category   : constant String := "Builder results";
   --  -"Builder results"
   Warning_Category : constant String := "Builder warnings";
   --  -"Builder warnings"
   Style_Category   : constant String := "Style errors";
   --  -"Style errors"
   Shadow_Category  : constant String := "Syntax check";
   --  -"Syntax check"

   type Build_Command is new Root_Command with private;
   type Build_Command_Access is access all Build_Command;

   procedure Create
     (Item  : out Build_Command_Access;
      Data  : Process_Data;
      Quiet : Boolean := False;
      Files : File_Array_Access := null);
   --  Create a new Build_Command.
   --  Files contain an array of files that are to be deleted when the
   --  command is destroyed. User must not free Files.

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
      Quiet : Boolean := False;
      Data  : Process_Data;
      Files : File_Array_Access := null;
      Main_Error_Category : GNAT.OS_Lib.String_Access;
      Style_Category      : GNAT.OS_Lib.String_Access;
      Warning_Category    : GNAT.OS_Lib.String_Access;
   end record;

end Commands.Builder;
