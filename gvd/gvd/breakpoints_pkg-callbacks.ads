-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
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

with Gtk.Arguments; use Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Breakpoints_Pkg.Callbacks is
   function On_Breakpoints_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Breakpoints_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Location_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Subprogam_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Address_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Regexp_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Update_Location_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Update_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Advanced_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Load_Exception_List_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Update_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Advanced_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Breakpoints_Pkg.Callbacks;
