-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                               AdaCore                             --
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

with Gtk.Widget;      use Gtk.Widget;
with Gdk.Pixbuf;      use Gdk.Pixbuf;

package Debugger_Pixmaps is

   Line_Has_Code_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Line_Might_Have_Code_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;
   Line_Has_Breakpoint_Pixbuf  : Gdk_Pixbuf := Null_Pixbuf;
   Current_Line_Pixbuf         : Gdk_Pixbuf := Null_Pixbuf;

   procedure Init_Graphics (Widget : Gtk_Widget);
   --  Initialize the module-wide pixbufs.
   --  This procedure will use the theme engine associated with Widget to look
   --  up any stock ids. (Therefore this widget can be any visible widget in
   --  the standard style).

end Debugger_Pixmaps;
