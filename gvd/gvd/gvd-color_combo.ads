
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
