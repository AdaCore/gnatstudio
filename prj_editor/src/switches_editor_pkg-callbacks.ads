-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Switches_Editor_Pkg.Callbacks is
   procedure Refresh_All_Switches
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure Refresh_Make_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Make_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Ada_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Ada_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_C_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_C_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Cpp_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cpp_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Binder_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Bind_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Linker_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Linker_Switches
     (Object : access Gtk_Widget_Record'Class);

end Switches_Editor_Pkg.Callbacks;
