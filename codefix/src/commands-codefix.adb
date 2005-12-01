-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                            AdaCore                                --
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
with Traces;                use Traces;
with GPS.Intl;              use GPS.Intl;
with VFS;                   use VFS;

package body Commands.Codefix is

   Me : constant Debug_Handle := Create ("Commands.Codefix", Off);

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
         Gtk_New (Menu);
         Create_Submenu
           (Command.Kernel, Menu, Command.Session, Command.Error);
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

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Codefix_Add_Command) return Command_Return_Type is
   begin
      if Command.Current_Error /= Null_Error_Id then
         Trace
           (Me, "Activate_Codefix: Error found at "
            & Full_Name
              (Get_File (Get_Error_Message (Command.Current_Error))).all
            & Get_Line (Get_Error_Message (Command.Current_Error))'Img
            & Get_Column (Get_Error_Message (Command.Current_Error))'Img);

         Create_Pixmap_And_Category
           (Command.Kernel, Command.Session, Command.Current_Error);

         Command.Current_Error := Next (Command.Current_Error);
         Command.Errors_Fixed  := Command.Errors_Fixed + 1;

         return Execute_Again;
      else
         return Success;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Command : in out Codefix_Add_Command) is
      pragma Unreferenced (Command);
   begin
      null;
   end Free;

   --------------
   -- Progress --
   --------------

   function Progress
     (Command : access Codefix_Add_Command) return Progress_Record is
   begin
      return (Running, Command.Errors_Fixed, Command.Errors_Num);
   end Progress;

   ----------
   -- Name --
   ----------

   function Name (Command : access Codefix_Add_Command) return String is
      pragma Unreferenced (Command);
   begin
      return -"Code fixing";
   end Name;

end Commands.Codefix;
