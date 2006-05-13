-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2006                       --
--                             AdaCore                               --
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

with Gtk.Extra.Combo_Button;  use Gtk.Extra.Combo_Button;
with Gdk.Color;
with Gtk.Color_Selection;

package Gtkada.Color_Combo is

   type Gtk_Color_Combo_Record is new Gtk_Combo_Button_Record with private;
   type Gtk_Color_Combo is access all Gtk_Color_Combo_Record'Class;

   procedure Gtk_New (Combo : out Gtk_Color_Combo);
   procedure Initialize (Combo : access Gtk_Color_Combo_Record'Class);

   procedure Set_Color
     (Combo : access Gtk_Color_Combo_Record;
      Color : Gdk.Color.Gdk_Color);

   function Get_Color (Combo : access Gtk_Color_Combo_Record)
      return Gdk.Color.Gdk_Color;
   --  Return the selected color as a preallocated color.

   function Get_Color (Combo : access Gtk_Color_Combo_Record)
      return String;
   --  Return the selected color as a string

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "color_changed"
   --    procedure Handler (Combo : access Gtk_Color_Combo_Record'Class);
   --
   --    Emitted when the selected color has changed.
   --  </signals>

   procedure Color_Changed (Combo : access Gtk_Color_Combo_Record'Class);
   --  Emit the "color_changed" signal.

private
   type Gtk_Color_Combo_Record is new Gtk_Combo_Button_Record with record
      Color     : Gdk.Color.Gdk_Color;
      Selection : Gtk.Color_Selection.Gtk_Color_Selection;
   end record;
end Gtkada.Color_Combo;
