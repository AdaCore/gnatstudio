with Gtk.Widget; use Gtk.Widget;
with Gtk.Arguments;

package Task_Dialog_Pkg.Callbacks is
   procedure On_Task_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Task_Dialog_Pkg.Callbacks;
