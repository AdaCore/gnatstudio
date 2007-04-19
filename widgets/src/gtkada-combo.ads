-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                    Copyright (C) 2002-2007                        --
--                            AdaCore                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;      use Glib;
with Gtk.Combo;

package Gtkada.Combo is

   type Gtkada_Combo_Record is new Gtk.Combo.Gtk_Combo_Record with private;
   type Gtkada_Combo is access all Gtkada_Combo_Record'Class;
   --  A new combo, same as Gtk_Combo, but with a special signal to indicate
   --  when its contents has changed.
   --  The entry is set to non-editable, for a proper handling.

   procedure Gtk_New (Combo : out Gtkada_Combo);
   --  Create a new Gtk_Combo

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Combo : access Gtkada_Combo_Record'Class);
   --
   --    Emitted when the contents of the combo has changed
   --  </signals>

   Signal_Changed : constant Signal_Name := "changed";

private
   type Gtkada_Combo_Record is new Gtk.Combo.Gtk_Combo_Record with null record;

end Gtkada.Combo;
