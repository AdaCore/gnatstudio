with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Explorer_Window_Pkg.Callbacks is
   function On_Explorer_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Clist_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class);

end Explorer_Window_Pkg.Callbacks;
