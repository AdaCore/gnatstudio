
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

private
   type Gtkada_Combo_Record is new Gtk.Combo.Gtk_Combo_Record with null record;

end Gtkada.Combo;
