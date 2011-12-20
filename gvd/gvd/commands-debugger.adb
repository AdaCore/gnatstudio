------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Debugger;           use Debugger;
with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Console; use GPS.Kernel.Console;
with GVD.Types;          use GVD.Types;
with Process_Proxies;    use Process_Proxies;

package body Commands.Debugger is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item       : out Set_Breakpoint_Command_Access;
      Kernel     : Kernel_Handle;
      Debugger   : Visual_Debugger;
      Mode       : Breakpoint_Command_Mode;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Positive) is
   begin
      Item          := new Set_Breakpoint_Command;
      Item.Kernel   := Kernel;
      Item.BMode    := Mode;
      Item.File     := File;
      Item.Line     := Line;
      Item.Debugger := Debugger;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
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

      Ref (Command);

      case Command.BMode is
         when Set =>
            Break_Source
              (Command.Debugger.Debugger,
               Command.File,
               Command.Line,
               Mode => Visible);

         when Unset =>
            if Command.Debugger.Breakpoints /= null then
               for J in Command.Debugger.Breakpoints'Range loop
                  if Command.Debugger.Breakpoints (J).Line = Command.Line
                    and then Command.Debugger.Breakpoints (J).File =
                      Command.File
                  then
                     Remove_Breakpoint
                       (Command.Debugger.Debugger,
                        Command.Debugger.Breakpoints (J).Num,
                        Mode => Visible);
                  end if;
               end loop;
            end if;
      end case;

      C := Command_Access (Command);
      Unref (C);
      if C = null then
         --  Was freed
         return Failure;
      else
         Command_Finished (Command, True);
      end if;

      return Success;
   end Execute;

end Commands.Debugger;
