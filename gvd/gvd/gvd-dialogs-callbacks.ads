with Gtk.Widget; use Gtk.Widget;
with Gtk.Arguments;

package Odd.Dialogs.Callbacks is
   procedure On_Backtrace_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Task_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Question_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class);
end Odd.Dialogs.Callbacks;
