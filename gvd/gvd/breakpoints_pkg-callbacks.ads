with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Breakpoints_Pkg.Callbacks is
   procedure On_Location_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Subprogam_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Address_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Regexp_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Advanced_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Advanced_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure Update_Breakpoint_List
     (Editor : access Breakpoints_Pkg.Breakpoints_Record'Class);
   --  Update the list of breakpoints in the dialog.
   --  The list is taken from the one stored in the current debugger session.

end Breakpoints_Pkg.Callbacks;
