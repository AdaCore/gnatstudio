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

with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

with Codefix.Errors_Manager; use Codefix.Errors_Manager;

package body Final_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Final_Validation_Clicked --
   ---------------------------------

   procedure On_Final_Validation_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Final_Window : Final_Window_Access := Final_Window_Access (Object);
      Success      : Boolean;
   begin
      Update
        (Final_Window.Graphic_Codefix.Corrector,
         Success,
         Final_Window.Graphic_Codefix.Current_Text,
         null);

      Quit (Final_Window.Graphic_Codefix);
   end On_Final_Validation_Clicked;

   -----------------------------
   -- On_Final_Cancel_Clicked --
   -----------------------------

   procedure On_Final_Cancel_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Final_Window : Final_Window_Access := Final_Window_Access (Object);
   begin
      Quit (Final_Window.Graphic_Codefix);
   end On_Final_Cancel_Clicked;

end Final_Window_Pkg.Callbacks;
