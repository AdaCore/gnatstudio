with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Foreign_Naming_Scheme_Editor_Pkg.Callbacks is
   procedure On_Exception_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_Exception_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Update_Clicked
     (Object : access Gtk_Widget_Record'Class);

   function On_Filename_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

end Foreign_Naming_Scheme_Editor_Pkg.Callbacks;
