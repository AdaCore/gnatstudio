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

with Gtk.Extra.Combo_Box;  use Gtk.Extra.Combo_Box;
with Gdk.Color;
with Gtk.Color_Selection;

package GVD.Color_Combo is

   type Gvd_Color_Combo_Record is new Gtk_Combo_Box_Record with private;
   type Gvd_Color_Combo is access all Gvd_Color_Combo_Record'Class;

   procedure Gtk_New (Combo : out Gvd_Color_Combo);
   procedure Initialize (Combo : access Gvd_Color_Combo_Record'Class);

   procedure Set_Color
     (Combo : access Gvd_Color_Combo_Record;
      Color : Gdk.Color.Gdk_Color);

   function Get_Color (Combo : access Gvd_Color_Combo_Record)
      return Gdk.Color.Gdk_Color;

private
   type Gvd_Color_Combo_Record is new Gtk_Combo_Box_Record with record
      Color : Gdk.Color.Gdk_Color;
      Selection : Gtk.Color_Selection.Gtk_Color_Selection;
   end record;
end GVD.Color_Combo;
