with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Print_Dialog_Pkg.Callbacks is
   procedure On_Print_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Print_Dialog_Pkg.Callbacks;
