with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Hyper_Grep_Gui_Pkg.Callbacks is
   function On_Hyper_Grep_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Only_Project_Check_Toggled
     (Object : access Gtk_Check_Button_Record'Class);

   procedure On_Start_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Stop_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Hyper_Grep_Gui_Pkg.Callbacks;
