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

with Debugger;             use Debugger;
with GVD.Types;            use GVD.Types;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Intl;           use Glide_Intl;
with Process_Proxies;      use Process_Proxies;

package body Commands.Debugger is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item       : out Set_Breakpoint_Command_Access;
      Kernel     : Kernel_Handle;
      Debugger   : Visual_Debugger;
      Mode       : Breakpoint_Command_Mode;
      File       : String;
      Line       : Positive) is
   begin
      Item          := new Set_Breakpoint_Command;
      Item.Kernel   := Kernel;
      Item.BMode    := Mode;
      Item.File     := new String'(File);
      Item.Line     := Line;
      Item.Debugger := Debugger;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Set_Breakpoint_Command) return Command_Return_Type
   is
      C : Command_Access;
   begin
      if Command_In_Process (Get_Process (Command.Debugger.Debugger)) then
         Insert
           (Command.Kernel,
            -"The debugger is busy processing a command",
            Mode => Error);
         Command_Finished (Command, False);

         return Failure;
      end if;

      Command.Do_Not_Free := True;

      case Command.BMode is
         when Set =>
            Break_Source
              (Command.Debugger.Debugger,
               Command.File.all,
               Command.Line,
               Mode => Visible);

         when Unset =>
            if Command.Debugger.Breakpoints /= null then
               for J in Command.Debugger.Breakpoints'Range loop
                  if Command.Debugger.Breakpoints (J).Line = Command.Line
                    and then Command.Debugger.Breakpoints (J).File /= null
                    and then Command.Debugger.Breakpoints (J).File.all =
                      Command.File.all
                  then
                     Remove_Breakpoint
                       (Command.Debugger.Debugger,
                        Command.Debugger.Breakpoints (J).Num,
                        Mode => Visible);
                  end if;
               end loop;
            end if;
      end case;

      Command.Do_Not_Free := False;

      if Command.To_Be_Freed then
         C := Command_Access (Command);
         Destroy (C);

         return Failure;

      else
         Command_Finished (Command, True);
      end if;

      return Success;
   end Execute;

   -------------
   -- Destroy --
   -------------

   procedure Free (Command : in out Set_Breakpoint_Command) is
   begin
      Free (Command.File);
   end Free;

end Commands.Debugger;
