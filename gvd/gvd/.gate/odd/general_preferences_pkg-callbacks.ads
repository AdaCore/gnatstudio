with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Odd_Preferences_Pkg.Callbacks is
   function On_Odd_Preferences_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Reset_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Odd_Preferences_Pkg.Callbacks;
