with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Gui_Builder_Pkg.Callbacks is
   function On_Gui_Builder_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Quit1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Edit1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Settings1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Help1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

end Gui_Builder_Pkg.Callbacks;
