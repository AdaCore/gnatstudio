with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Advanced_Breakpoint_Pkg.Callbacks is
   procedure On_Start_Record_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Stop_Record_Clicked
     (Object : access Gtk_Button_Record'Class);

end Advanced_Breakpoint_Pkg.Callbacks;
