with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Breakpoints_Pkg.Callbacks is
   procedure On_Advanced_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Add_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Remove_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Remove_All_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

end Breakpoints_Pkg.Callbacks;
