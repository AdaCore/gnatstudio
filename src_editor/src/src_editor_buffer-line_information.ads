-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                       Copyright (C) 2003                          --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles the customizable information in the buffer,
--  such as information added to the sides of lines, or VCS information.

with Pango.Layout; use Pango.Layout;

with Glib; use Glib;

with Gdk.Pixmap;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text_View; use Gtk.Text_View;

package Src_Editor_Buffer.Line_Information is

   procedure Create_Line_Information_Column
     (Buffer        : access Source_Buffer_Record'Class;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean);
   --  Add a column corresponding to Identifier in Buffer.

   procedure Remove_Line_Information_Column
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String);
   --  Remove a column from the side information in Buffer.

   procedure Add_File_Information
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Box        : Gtk_Widget;
      Info       : Glide_Kernel.Modules.Line_Information_Data);
   --  Add the line information to the Buffer.
   --  User must not free Info.

   procedure Draw_Line_Info
     (Editor      : access Source_Buffer_Record'Class;
      Top_Line    : Buffer_Line_Type;
      Bottom_Line : Buffer_Line_Type;
      View        : Gtk_Text_View;
      GC          : Gdk.GC.Gdk_GC;
      Layout      : in out Pango_Layout;
      Drawable    : in out Gdk.Pixmap.Gdk_Pixmap);
   --  Draw side info from Top_Line to Bottom_Line on Drawable.
   --  Layout should be used to draw text.

   procedure On_Click
     (Editor : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Offset : Gint);
   --  Perform a click in the side column for line Line, offset Offset.

end Src_Editor_Buffer.Line_Information;
