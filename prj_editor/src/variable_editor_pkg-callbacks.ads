--  with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Variable_Editor_Pkg.Callbacks is
   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Close_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Variable_Editor_Pkg.Callbacks;
