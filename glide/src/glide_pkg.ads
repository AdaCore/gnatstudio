-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtkada.MDI; use Gtkada.MDI;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;

package Glide_Pkg is

   type Glide_Record is new Gtk_Window_Record with record
      Vbox : Gtk_Vbox;
      Factory : Gtk_Item_Factory;
      Mdi : MDI_Window;
      Console_Sw : Gtk_Scrolled_Window;
      Console : Gtk_Text;
      Statusbar : Gtk_Statusbar;
   end record;
   type Glide_Access is access all Glide_Record'Class;

   procedure Gtk_New (Glide : out Glide_Access);
   procedure Initialize (Glide : access Glide_Record'Class);

end Glide_Pkg;
