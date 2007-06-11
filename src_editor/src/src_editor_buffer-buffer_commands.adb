-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                    Copyright (C) 2003-2006                        --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Tags;        use Ada.Tags;

with Gtk;             use Gtk;
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gtk.Widget;      use Gtk.Widget;

with GPS.Kernel;      use GPS.Kernel;
with Src_Editor_View; use Src_Editor_View;

package body Src_Editor_Buffer.Buffer_Commands is

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Jump_To_Delimiter_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

      View                 : Source_View;
      Widget               : constant Gtk_Widget :=
                               Get_Current_Focus_Widget (Command.Kernel);
      Buffer               : Source_Buffer;
      On_Cursor_Iter       : Gtk_Text_Iter;
      First_Highlight_Iter : Gtk_Text_Iter;
      Last_Highlight_Iter  : Gtk_Text_Iter;
      Found                : Integer;

   begin
      if Widget /= null
        and then Widget.all'Tag = Source_View_Record'Tag
      then
         View := Source_View (Widget);
         Buffer := Source_Buffer (Get_Buffer (View));
      else
         return Commands.Failure;
      end if;

      Get_Iter_At_Mark (Buffer, On_Cursor_Iter, Buffer.Insert_Mark);

      Get_Delimiters
        (Buffer,
         On_Cursor_Iter,
         First_Highlight_Iter, Last_Highlight_Iter,
         Found);

      if Found > 0 then
         if Equal (First_Highlight_Iter, On_Cursor_Iter) then
            Place_Cursor (Buffer, Last_Highlight_Iter);
         else
            Place_Cursor (Buffer, First_Highlight_Iter);
         end if;
      end if;

      return Commands.Success;
   end Execute;

end Src_Editor_Buffer.Buffer_Commands;
