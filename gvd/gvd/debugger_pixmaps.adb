-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                             ACT-Europe                            --
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

with Pixmaps_IDE;    use Pixmaps_IDE;

package body Debugger_Pixmaps is

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics is
   begin
      if Line_Has_Code_Pixbuf = Null_Pixbuf then
         Line_Has_Code_Pixbuf := Gdk_New_From_Xpm_Data (dot_xpm);
         Line_Might_Have_Code_Pixbuf := Gdk_New_From_Xpm_Data (grey_dot_xpm);
         Line_Has_Breakpoint_Pixbuf := Gdk_New_From_Xpm_Data (stop_xpm);
         Current_Line_Pixbuf := Gdk_New_From_Xpm_Data (arrow_xpm);
      end if;
   end Init_Graphics;

end Debugger_Pixmaps;
