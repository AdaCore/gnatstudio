with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Process_Tab_Pkg.Callbacks is
   function On_Process_Tab_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Data_Paned_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Stack_List_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_Stack_List_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Editor_Text_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Command_Scrolledwindow_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Debugger_Text_Insert_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Debugger_Text_Delete_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_Debugger_Text_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Debugger_Text_Grab_Focus
     (Object : access Gtk_Widget_Record'Class);

end Process_Tab_Pkg.Callbacks;
