
with Glib.Properties.Creation;
with Gtk.Box;
with Gtk.GEntry;
with Gtk.Toggle_Button;
with Gtk.Window;

package Flags_Combos is

   type Flags_Combo_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Flags_Combo is access all Flags_Combo_Record'Class;

   procedure Gtk_New
     (Combo : out Flags_Combo;
      Klass : Glib.Properties.Creation.Flags_Class);
   --  Create a new flags editor, that edits a given flags class.

   procedure Initialize
     (Combo : access Flags_Combo_Record'Class;
      Klass : Glib.Properties.Creation.Flags_Class);
   --  Internal function for creating new widgets

   procedure Set_Value
     (Combo : access Flags_Combo_Record;
      Val : Glib.Properties.Creation.Flags_Int_Value);
   --  Set the current value in the combo. This is reflected both in
   --  the entry and in the popup menu.

   function Get_Value (Combo : access Flags_Combo_Record)
      return Glib.Properties.Creation.Flags_Int_Value;
   --  Return the value currently edited in the combo box

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Combo : access Flags_Combo_Record'Class);
   --
   --    Emitted every time the value of the flags has changed
   --  </signals>

private
   type Flags_Combo_Record is new Gtk.Box.Gtk_Box_Record with record
      Toggle : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Ent    : Gtk.GEntry.Gtk_Entry;
      Klass  : Glib.Properties.Creation.Flags_Class;
      Value  : Glib.Properties.Creation.Flags_Int_Value;
      Win    : Gtk.Window.Gtk_Window := null;
   end record;
end Flags_Combos;
