-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Gtk.Menu;              use Gtk.Menu;

with Glide_Kernel.Console;  use Glide_Kernel.Console;
with Glide_Kernel.Modules;  use Glide_Kernel.Modules;

with Codefix.Formal_Errors; use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;

with Codefix_Module;        use Codefix_Module;

package body Commands.Codefix is

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Codefix_Command) return Boolean is
      Success_Execute : Boolean;
      Result          : Extract;
      Menu            : Gtk_Menu;
   begin
      if Is_Fixed (Command.Error) then
         return True;
      end if;

      if Get_Number_Of_Fixes (Command.Error) = 1 then
         Secured_Execute
           (Data (First (Get_Solutions (Command.Error))),
            Command.Current_Text.all,
            Result,
            Success_Execute);
      else
         Menu := Create_Submenu (Command.Error);
         Show_All (Menu);
         Popup (Menu);
         return True;
      end if;

      if Success_Execute then
         Validate_And_Commit
           (Command.Corrector.all,
            Command.Current_Text.all,
            Command.Error,
            Data (First (Get_Solutions (Command.Error))));
      else
         Insert
            (Command.Kernel,
             "No more sense for " & Get_Message
              (Get_Error_Message (Command.Error)));
      end if;

      Remove_Location_Action
        (Kernel        => Command.Kernel,
         Identifier    => "--  ???",
         Category      => "Builder Results",
         File          => Get_Error_Message (Command.Error).File_Name.all,
         Line          => Get_Error_Message (Command.Error).Line,
         Column        => Get_Error_Message (Command.Error).Col,
         Message       =>
             Cut_Message (Get_Message (Get_Error_Message (Command.Error))));

      return True;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Command : in out Codefix_Command) is
      pragma Unreferenced (Command);
   begin
      null;
   end Free;

end Commands.Codefix;
