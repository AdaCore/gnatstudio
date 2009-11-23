-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Debugger;             use Debugger;
with GPS.Intl;             use GPS.Intl;
with GPS.Kernel;           use GPS.Kernel;
with GPS.Main_Window;      use GPS.Main_Window;
with GVD.Process;          use GVD.Process;
with GVD;                  use GVD;
with Histories;            use Histories;
with Process_Proxies;      use Process_Proxies;
with Std_Dialogs;          use Std_Dialogs;

package body GVD.Menu is

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Debugger : Visual_Debugger) is
   begin
      if Debugger = null
        or else Debugger.Debugger = null
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return;
      end if;

      declare
         Is_Func : aliased Boolean;
         Expression : constant String := Display_Entry_Dialog
           (Parent  => Debugger.Window,
            Title   => -"Expression Selection",
            Message => -"Enter an expression to display:",
            Key     => "gvd_display_expression_dialog",
            Check_Msg => -"Expression is a subprogram call",
            History   => Get_History (GPS_Window (Debugger.Window).Kernel),
            Key_Check => "expression_subprogram_debugger",
            Button_Active => Is_Func'Unchecked_Access);

      begin
         if Expression /= ""
           and then Expression (Expression'First) /= ASCII.NUL
         then
            if Is_Func then
               Process_User_Command
                 (Debugger,
                  "graph print `" & Expression & '`',
                  Output_Command => True);
            else
               Process_User_Command
                 (Debugger,
                  "graph display " & Expression,
                  Output_Command => True);
            end if;
         end if;
      end;
   end Display_Expression;

end GVD.Menu;
