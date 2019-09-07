------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Debugger;             use Debugger;
with GPS.Intl;             use GPS.Intl;
with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with GVD.Process;          use GVD.Process;
with GVD;                  use GVD;
with Histories;            use Histories;
with Process_Proxies;      use Process_Proxies;
with GPS.Dialogs;          use GPS.Dialogs;

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
         Is_Func    : aliased Boolean;
         Expression : constant String := Display_Text_Input_Dialog
           (Kernel        => Debugger.Kernel,
            Title         => -"Expression Selection",
            Message       => -"Enter an expression to display:",
            Key           => "gvd_display_expression_dialog",
            Check_Msg     => -"Expression is a subprogram call",
            Key_Check     => "expression_subprogram_debugger",
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
