with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Make_Suite_Window_Pkg.Callbacks is
   function On_Make_Suite_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Add_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Remove_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class);

end Make_Suite_Window_Pkg.Callbacks;
