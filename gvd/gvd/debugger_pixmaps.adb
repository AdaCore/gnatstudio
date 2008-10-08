-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Enums;      use Gtk.Enums;

package body Debugger_Pixmaps is

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics (Widget : Gtk_Widget) is
   begin
      if Line_Has_Code_Pixbuf = Null_Pixbuf then
         Line_Has_Code_Pixbuf := Render_Icon
           (Widget, "gps-debugger-line-has-code", Icon_Size_Menu);
         Line_Might_Have_Code_Pixbuf := Render_Icon
           (Widget, "gps-debugger-line-might-have-code", Icon_Size_Menu);
         Line_Has_Breakpoint_Pixbuf := Render_Icon
           (Widget, "gps-debugger-break", Icon_Size_Menu);
         Current_Line_Pixbuf := Render_Icon
           (Widget, "gps-debugger-current", Icon_Size_Menu);
      end if;
   end Init_Graphics;

end Debugger_Pixmaps;
