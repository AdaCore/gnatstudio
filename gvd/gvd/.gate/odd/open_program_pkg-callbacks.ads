with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Open_Program_Pkg.Callbacks is
   procedure On_Radio_Button_Toggled
     (Object : access Gtk_Radio_Button_Record'Class);

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Ok_Open_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Open_Clicked
     (Object : access Gtk_Button_Record'Class);

end Open_Program_Pkg.Callbacks;
