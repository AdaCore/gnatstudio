with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Inspector_Pkg.Callbacks is
   function On_Properties_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

end Inspector_Pkg.Callbacks;
