
package Open_Session_Pkg.Callbacks is

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Ok_Open_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Open_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Open_Clicked
     (Object : access Gtk_Button_Record'Class);

end Open_Session_Pkg.Callbacks;
