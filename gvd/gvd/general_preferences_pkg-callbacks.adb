-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Widget; use Gtk.Widget;

package body Odd_Preferences_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------------------
   -- On_Odd_Preferences_Delete_Event --
   -------------------------------------

   function On_Odd_Preferences_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Hide (Object);
      return False;
   end On_Odd_Preferences_Delete_Event;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Get_Toplevel (Object));
   end On_Ok_Button_Clicked;

   -----------------------------
   -- On_Reset_Button_Clicked --
   -----------------------------

   procedure On_Reset_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Reset_Button_Clicked;

   ----------------------------
   -- On_Help_Button_Clicked --
   ----------------------------

   procedure On_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Button_Clicked;

end Odd_Preferences_Pkg.Callbacks;
