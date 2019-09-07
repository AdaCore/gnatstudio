------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Gtk.Menu;              use Gtk.Menu;

with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with GPS.Intl;              use GPS.Intl;

with Codefix.Formal_Errors; use Codefix.Formal_Errors;
with Codefix_Module;        use Codefix_Module;
with Codefix.Text_Manager;  use Codefix.Text_Manager;

package body Commands.Codefix is

   Me : constant Trace_Handle := Create ("GPS.CODEFIX.COMMANDS", Off);

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Codefix_Command) return Command_Return_Type
   is
      Menu        : Gtk_Menu;
      Sub_Command : Ptr_Command;
   begin
      if Command.Session_Timestamp /= Command.Session.Timestamp then
         return Success;
      end if;

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

      Sub_Command := Get_Command (First (Get_Solutions (Command.Error)));

      if Sub_Command.Is_Writable then
         Validate_And_Commit
           (Command.Session.Corrector.all,
            Command.Session.Current_Text.all,
            Command.Error,
            Sub_Command.all);

         Remove_Pixmap
           (Command.Kernel,
            Command.Session,
            Command.Error);
      else
         Command.Kernel.Insert
           (-"cannot fix readonly file",
            Mode => Error);
      end if;

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Codefix_Command) return Boolean is
   begin
      Undo (Command.Error, Command.Session.Current_Text.all);
      Create_Pixmap_And_Category
        (Command.Kernel,
         Command.Session,
         Command.Error);

      return True;
   end Undo;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Codefix_Add_Command) return Command_Return_Type is
   begin
      if Command.Session_Timestamp /= Command.Session.Timestamp then
         return Success;
      end if;

      if Command.Current_Error /= Null_Error_Id then
         if Active (Me) then
            Trace
              (Me, "Activate_Codefix: Error found at "
               & Display_Full_Name
                 (Get_File (Get_Error_Message (Command.Current_Error)))
               & Get_Line (Get_Error_Message (Command.Current_Error))'Img
               & Get_Column (Get_Error_Message (Command.Current_Error))'Img);
         end if;

         Create_Pixmap_And_Category
           (Command.Kernel, Command.Session, Command.Current_Error);

         Command.Current_Error := Next (Command.Current_Error);
         Command.Errors_Fixed  := Command.Errors_Fixed + 1;

         return Execute_Again;
      else
         return Success;
      end if;
   end Execute;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access Codefix_Add_Command) return Progress_Record is
   begin
      return (Running, Command.Errors_Fixed, Command.Errors_Num);
   end Progress;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Codefix_Add_Command) return String
   is
      pragma Unreferenced (Command);
   begin
      return -"Code fixing";
   end Name;

end Commands.Codefix;
