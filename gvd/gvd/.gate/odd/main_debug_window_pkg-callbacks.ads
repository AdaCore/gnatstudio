with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Main_Debug_Window_Pkg.Callbacks is
   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

end Main_Debug_Window_Pkg.Callbacks;
