with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Memory_View_Pkg.Callbacks is
   function On_Memory_View_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Memory_View_Size_Allocate
     (Object : access Gtk_Window_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Address_View_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Size_Entry_Changed
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Data_Entry_Changed
     (Object : access Gtk_Entry_Record'Class);

   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Check_Button_Record'Class);

   procedure On_Pgup_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Pgdn_Clicked
     (Object : access Gtk_Button_Record'Class);

   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_View_Move_Cursor
     (Object : access Gtk_Text_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_View_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Reset_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Submit_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class);

end Memory_View_Pkg.Callbacks;
