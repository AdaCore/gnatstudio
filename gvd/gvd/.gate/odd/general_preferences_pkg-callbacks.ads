with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package General_Preferences_Pkg.Callbacks is
   function On_Gvd_Preferences_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Test_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

end General_Preferences_Pkg.Callbacks;
