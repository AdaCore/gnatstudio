-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

--  This package contains commands related to the debugger.

with Glide_Kernel;          use Glide_Kernel;
with Basic_Types;           use Basic_Types;
with Debugger;              use Debugger;
with GVD.Types;             use GVD.Types;

package Commands.Debugger is

   Breakpoints_Column_Id : constant String := "Debugger/Breakpoints";

   type Set_Breakpoint_Command is new Root_Command with private;
   type Set_Breakpoint_Command_Access is access all Set_Breakpoint_Command;

   type Breakpoint_Command_Mode is (Set, Unset);

   procedure Create
     (Item           : out Set_Breakpoint_Command_Access;
      Kernel         : Kernel_Handle;
      Debugger       : Debugger_Access;
      Mode           : Breakpoint_Command_Mode;
      File           : String;
      Line           : Positive;
      Identifier     : Breakpoint_Identifier := 0);

   function Execute
     (Command : access Set_Breakpoint_Command) return Command_Return_Type;

   procedure Free (Command : in out Set_Breakpoint_Command);
   --  Free memory associated to Command.

private

   type Set_Breakpoint_Command is new Root_Command with record
      File     : String_Access;
      Kernel   : Kernel_Handle;
      Line     : Positive;
      BMode    : Breakpoint_Command_Mode;
      Debugger : Debugger_Access;
      BP       : GVD.Types.Breakpoint_Identifier;
   end record;

end Commands.Debugger;
