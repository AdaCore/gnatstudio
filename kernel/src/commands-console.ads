-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Basic_Types;          use Basic_Types;

package Commands.Console is

   type Console_Command is new Root_Command with private;
   type Console_Command_Access is access all Console_Command;

   procedure Create
     (Item           : out Console_Command_Access;
      Kernel         : Kernel_Handle;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info);
   --  Create a new console command.

   function Copy
     (Item : Console_Command_Access)
     return Console_Command_Access;
   --  Create a new Console_Command_Access as a copy of Item.

   function Execute (Command : access Console_Command) return Boolean;
   --  Execute Command.

   function Undo (Command : access Console_Command) return Boolean
     renames Execute;

   procedure Free (Command : in out Console_Command);
   --  Free memory associated to Command.

private

   type Console_Command is new Root_Command with record
      Kernel         : Kernel_Handle;
      Text           : String_Access;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Insert_Mode    : Message_Type := Info;
   end record;

end Commands.Console;
