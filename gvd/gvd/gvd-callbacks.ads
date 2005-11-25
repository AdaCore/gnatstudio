-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides instantiations of Gtk.Handlers.Callback that
--  are shared among GVD packages.

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Text_View;    use Gtk.Text_View;

package GVD.Callbacks is

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   package Entry_Callback is new
     Gtk.Handlers.Callback (Gtk_Entry_Record);

   package Entry_Return_Callback is new
     Gtk.Handlers.Return_Callback (Gtk_Entry_Record, Boolean);

   package Check_Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Check_Button_Record);

   package Text_Callback is new
     Gtk.Handlers.Callback (Gtk_Text_View_Record);

end GVD.Callbacks;
