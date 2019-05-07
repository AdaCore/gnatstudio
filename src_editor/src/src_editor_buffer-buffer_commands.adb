------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Tags;                  use Ada.Tags;

with Gtk;                       use Gtk;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Widget;                use Gtk.Widget;

with Src_Editor_View;           use Src_Editor_View;
with Src_Editor_Buffer.Cursors; use Src_Editor_Buffer.Cursors;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;

package body Src_Editor_Buffer.Buffer_Commands is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Jump_To_Delimiter_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View                 : Source_View;
      Widget               : constant Gtk_Widget :=
                               Get_Current_Focus_Widget (Kernel);
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
         Found,
         Counter_Max => Natural'Last);

      if Found > 0 then
         Move (Get_Main_Cursor (Buffer),
               (if Equal (First_Highlight_Iter, On_Cursor_Iter)
                then Last_Highlight_Iter
                else First_Highlight_Iter), False);
         View.Scroll_To_Cursor_Location;
      end if;

      return Commands.Success;
   end Execute;

end Src_Editor_Buffer.Buffer_Commands;
