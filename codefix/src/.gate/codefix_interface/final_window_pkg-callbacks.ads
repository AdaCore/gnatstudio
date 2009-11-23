with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Final_Window_Pkg.Callbacks is
   procedure On_Final_Validation_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Final_Cancel_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Final_Window_Pkg.Callbacks;
