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

with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Color;       use Gdk.Color;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Menu_Bar;    use Gtk.Menu_Bar;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Style;       use Gtk.Style;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Glide; use Callbacks_Glide;
with Glide_Menu;      use Glide_Menu;
with Glide_Intl;      use Glide_Intl;
with Factory_Data;

package body Glide_Pkg is

MDI_Background_Color : constant String := "#666666";
--  <preference> Background color to use for the MDI window

procedure Gtk_New (Glide : out Glide_Access) is
begin
   Glide := new Glide_Record;
   Glide_Pkg.Initialize (Glide);
end Gtk_New;

procedure Initialize (Glide : access Glide_Record'Class) is
   pragma Suppress (All_Checks);
   The_Accel_Group : Gtk_Accel_Group;
   Menu_Item : Gtk_Menu_Item;
   Child : MDI_Child;
   Color : Gdk_Color;
   Style : Gtk_Style;

begin
   Gtk.Window.Initialize (Glide, Window_Toplevel);
   Set_Title (Glide, "GLIDE");
   Set_Policy (Glide, False, True, False);
   Set_Position (Glide, Win_Pos_None);
   Set_Modal (Glide, False);
   Set_Default_Size (Glide, 640, 480);

   Gtk_New_Vbox (Glide.Vbox, False, 0);
   Add (Glide, Glide.Vbox);

   Gtk_New (The_Accel_Group);
   Add_Accel_Group (Glide, The_Accel_Group);
   Gtk_New (Glide.Factory, Gtk.Menu_Bar.Get_Type,
            "<glide>", The_Accel_Group);
   Factory_Data.Create_Items
     (Glide.Factory, Glide_Menu_Items.all, Glide.all'Access);
   Pack_Start
     (Glide.Vbox,
      Get_Widget (Glide.Factory, "<glide>"), False, False, 0);

   Gtk_New (Glide.Mdi);
   Color := Parse (MDI_Background_Color);
   Alloc (Get_Default_Colormap, Color);
   Style := Copy (Get_Style (Glide.Mdi));
   Set_Background (Style, State_Normal, Color);
   Set_Style (Glide.Mdi, Style);
   Set_Priorities (Glide.Mdi, (Left => 4, Right => 3, Top => 1, Bottom => 2));
   Pack_Start (Glide.Vbox, Glide.Mdi, True, True, 0);

   Menu_Item := Gtk_Menu_Item (Get_Widget (Glide.Factory, -"/Window"));
   Set_Submenu (Menu_Item, Create_Menu (Glide.Mdi));

   Gtk_New (Glide.Console_Sw);
   Set_Policy (Glide.Console_Sw, Policy_Never, Policy_Always);
   Child := Put (Glide.Mdi, Glide.Console_Sw);
   Set_Title (Child, "Console");
   Set_Dock_Side (Child, Bottom);
   Dock_Child (Child);

   Gtk_New (Glide.Console);
   Set_Editable (Glide.Console, False);
   Add (Glide.Console_Sw, Glide.Console);

   Gtk_New (Glide.Statusbar);
   Pack_Start (Glide.Vbox, Glide.Statusbar, False, False, 0);

end Initialize;

end Glide_Pkg;
