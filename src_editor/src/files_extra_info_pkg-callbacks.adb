-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                             ACT-Europe                            --
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

--  with System; use System;
--  with Glib; use Glib;
--  with Gdk.Event; use Gdk.Event;
--  with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
--  with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with GPS.Intl; use GPS.Intl;

package body Files_Extra_Info_Pkg.Callbacks is

   --  use Gtk.Arguments;

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Extra : constant Files_Extra_Info_Access :=
        Files_Extra_Info_Access (Object);
      S     : constant String := Select_Directory
        (-"Select a directory",
         Parent  => Gtk_Window (Get_Toplevel (Object)),
         History => null);  --  ??? No history

   begin
      if S /= "" then
         Set_Text (Extra.Directory_Entry, S);
      end if;
   end On_Browse_Button_Clicked;

end Files_Extra_Info_Pkg.Callbacks;
