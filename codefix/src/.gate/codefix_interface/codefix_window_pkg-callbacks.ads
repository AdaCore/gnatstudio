with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Codefix_Window_Pkg.Callbacks is
   function On_Codefix_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Fix_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Skip_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Accept_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Skip_All_Corrections_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Accept_All_Corrections_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Undo_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Refresh_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Changes_Clicked
     (Object : access Gtk_Widget_Record'Class);

end Codefix_Window_Pkg.Callbacks;
