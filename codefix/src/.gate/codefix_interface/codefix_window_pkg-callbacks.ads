with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Codefix_Window_Pkg.Callbacks is
   procedure On_Fix_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Prev_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Next_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Apply_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Undo_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Refresh_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Codefix_Window_Pkg.Callbacks;
