-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Codefix.Formal_Errors; use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;

with Codefix_Module;        use Codefix_Module;

package body Commands.Codefix is

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Codefix_Command) return Command_Return_Type
   is
      Menu : Gtk_Menu;
   begin
      if Is_Fixed (Command.Error) then
         return Success;
      end if;

      if Get_Number_Of_Fixes (Command.Error) > 1 then
         Menu := Create_Submenu
           (Command.Kernel, Command.Session, Command.Error);
         Show_All (Menu);
         Popup (Menu);
         return Success;
      end if;

      Validate_And_Commit
        (Command.Session.Corrector.all,
         Command.Session.Current_Text.all,
         Command.Error,
         Data (First (Get_Solutions (Command.Error))));

      Remove_Pixmap
        (Command.Kernel,
         Command.Session,
         Command.Error);

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   function Undo (Command : access Codefix_Command) return Boolean is
   begin
      Undo (Command.Error, Command.Session.Current_Text.all);
      Create_Pixmap_And_Category
        (Command.Kernel,
         Command.Session,
         Command.Error);

      return True;
   end Undo;

   ----------
   -- Free --
   ----------

   procedure Free (Command : in out Codefix_Command) is
      pragma Unreferenced (Command);
   begin
      null;
   end Free;

end Commands.Codefix;
