with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Explorer_Pkg.Callbacks is
   procedure On_Clist_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Close_Clicked
     (Object : access Gtk_Button_Record'Class);

end Explorer_Pkg.Callbacks;
