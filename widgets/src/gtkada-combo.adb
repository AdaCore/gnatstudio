with Glib;                 use Glib;
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
        (Get_Popup_Window (Combo), "hide",
         Widget_Callback.To_Marshaller (Selected'Access), Combo);
   end Gtk_New;

end Gtkada.Combo;
