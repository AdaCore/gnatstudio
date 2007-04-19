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

with Glib.Object;          use Glib.Object;
with Gtk.Combo;            use Gtk.Combo;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtk.GEntry;           use Gtk.GEntry;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtkada.Combo is

   Class_Record : GObject_Class := Uninitialized_Class;

   Signals : constant chars_ptr_array :=
     (1 => New_String ("changed"));

   procedure Selected (Combo : access Gtk_Widget_Record'Class);
   --  Called when a new value has been selected

   --------------
   -- Selected --
   --------------

   procedure Selected (Combo : access Gtk_Widget_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (Combo, "changed");
   end Selected;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo : out Gtkada_Combo) is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));

   begin
      Combo := new Gtkada_Combo_Record;
      Gtk.Combo.Initialize (Combo);
      Initialize_Class_Record
        (Combo, Signals, Class_Record, "GtkadaCombo", Signal_Parameters);
      Set_Editable (Get_Entry (Combo), False);

      Widget_Callback.Object_Connect
        (Get_Popup_Window (Combo), Signal_Hide, Selected'Access, Combo);
   end Gtk_New;

end Gtkada.Combo;
