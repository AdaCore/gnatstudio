with Gtk.Widget; use Gtk.Widget;

package RAD.Editor is

   procedure Add_Draw_Signals (Widget : access Gtk_Widget_Record'Class);

   procedure Add_Key_Signals (Widget : access Gtk_Widget_Record'Class);

   procedure Add_Mouse_Signals (Widget : access Gtk_Widget_Record'Class);

end RAD.Editor;
