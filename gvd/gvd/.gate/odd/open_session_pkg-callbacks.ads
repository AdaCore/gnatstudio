with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Open_Session_Pkg.Callbacks is
   procedure On_List_Select_Child
     (Object : access Gtk_List_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Select_All_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Unselect_All_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

end Open_Session_Pkg.Callbacks;
