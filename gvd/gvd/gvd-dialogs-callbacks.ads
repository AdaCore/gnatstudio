-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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
with Gtk.Arguments;

package GVD.Dialogs.Callbacks is

   procedure On_Question_No_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Question_Yes_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Question_OK_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class);

end GVD.Dialogs.Callbacks;
