with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Process_Tab_Pkg.Callbacks is
   procedure On_Thread_Notebook_Switch_Page
     (Object : access Gtk_Notebook_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Debugger_Text_Insert_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Debugger_Text_Delete_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_Debugger_Text_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

end Process_Tab_Pkg.Callbacks;
