with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Process_Tab_Pkg.Callbacks is
   function On_Process_Tab_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Editor_Text_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

end Process_Tab_Pkg.Callbacks;
