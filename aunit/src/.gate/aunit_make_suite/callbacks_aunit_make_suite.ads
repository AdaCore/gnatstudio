with Gtk.Handlers;
with Gtk.Button; use Gtk.Button;
with Gtk.Clist; use Gtk.Clist;

package Callbacks_Aunit_Make_Suite is

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   package C_List_Callback is new
     Gtk.Handlers.Callback (Gtk_Clist_Record);

end Callbacks_Aunit_Make_Suite;
