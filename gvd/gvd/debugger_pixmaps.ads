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

with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Basic_Types;     use Basic_Types;

package Debugger_Pixmaps is

   Line_Has_Code_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;
   Line_Has_Breakpoint_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;

   procedure Init_Graphics;
   --  Initialize the pixbufs.

private

   Line_Has_Code_Xpm : aliased Pixmap_Array;
   pragma Import (C, Line_Has_Code_Xpm, "dot_xpm");

   Line_Has_Breakpoint_Xpm : aliased Pixmap_Array;
   pragma Import (C, Line_Has_Breakpoint_Xpm, "stop_xpm");

end Debugger_Pixmaps;
