with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Main_Debug_Window_Pkg.Callbacks is
   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Run1_Toolbar_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Interrupt1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

end Main_Debug_Window_Pkg.Callbacks;
