with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Switches_Editor_Pkg.Callbacks is
   procedure On_Set_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Switches_Editor_Pkg.Callbacks;
