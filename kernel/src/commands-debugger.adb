----------------------------------------------------------------------
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

with GVD.Types;            use GVD.Types;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Intl;           use Glide_Intl;
with Process_Proxies;      use Process_Proxies;

package body Commands.Debugger is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out Set_Breakpoint_Command_Access;
      Kernel         : Kernel_Handle;
      Debugger       : Debugger_Access;
      Mode           : Breakpoint_Command_Mode;
      File           : String;
      Line           : Positive;
      Identifier     : Breakpoint_Identifier := 0) is
   begin
      Item := new Set_Breakpoint_Command;
      Item.Kernel := Kernel;
      Item.BMode := Mode;
      Item.File := new String' (File);
      Item.Line := Line;
      Item.Debugger := Debugger;
      Item.BP := Identifier;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Set_Breakpoint_Command) return Boolean is
   begin
      if Command_In_Process (Get_Process (Command.Debugger)) then
         Insert (Command.Kernel,
                 -"The debugger is busy processing a command",
                 Highlight_Sloc => False,
                 Add_LF         => True,
                 Mode           => Error);

         Command_Finished (Command, False);
         return False;
      end if;

      case Command.BMode is
         when Set =>
            Command.BP := Break_Source
              (Command.Debugger,
               Command.File.all,
               Command.Line,
               Mode => Visible);

         when Unset =>
            Remove_Breakpoint
              (Command.Debugger,
               Command.BP,
               Mode => Visible);
      end case;

      --  ??? There is a tricky case here: when Command is executed and
      --  Break_Source or Remove_Breakpoint is called, the debugger updates
      --  the breakpoint information accordingly. If the corresponding file
      --  is currently displayed, this will cause a new action item to be
      --  inserted in the side column information, and this causes the
      --  previous action for this column to be freed. Therefore, if the
      --  command was launched through a click on the column, the variable
      --  Column is not valid anymore at this point.
      --  For now, I'll just comment the following line, since it has no
      --  impact on the behaviour now, until I implement a general way
      --  to avoid that sort of problem.

      --  Command_Finished (Command, True);
      return True;
   end Execute;

   -------------
   -- Destroy --
   -------------

   procedure Free (Command : in out Set_Breakpoint_Command) is
   begin
      Free (Command.File);
   end Free;

end Commands.Debugger;
