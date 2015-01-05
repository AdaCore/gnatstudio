------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
