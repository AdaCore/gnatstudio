with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Builder_Pkg.Callbacks is
   function On_Builder_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Build_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Quit_Clicked
     (Object : access Gtk_Button_Record'Class);

end Builder_Pkg.Callbacks;
