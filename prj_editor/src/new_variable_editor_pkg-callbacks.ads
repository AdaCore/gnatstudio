--  with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package New_Variable_Editor_Pkg.Callbacks is
   procedure On_Get_Environment_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Typed_Variable_Toggled
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Concatenate_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Widget_Record'Class);

end New_Variable_Editor_Pkg.Callbacks;
