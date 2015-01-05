------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with Glib.Object;
with Gdk.Event;     use Gdk.Event;
with Gtk.Arguments;
with Gtk.Widget;

package List_Select_Pkg.Callbacks is

   procedure On_Clist_Select_Row
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_Clist_Button_Press
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   procedure On_The_Entry_Activate
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class);

   function On_Delete_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;

end List_Select_Pkg.Callbacks;
