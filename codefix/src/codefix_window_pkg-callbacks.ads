-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Widget; use Gtk.Widget;

package Codefix_Window_Pkg.Callbacks is
   procedure On_Fix_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Prev_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Next_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Apply_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Undo_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Refresh_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Codefix_Window_Pkg.Callbacks;
