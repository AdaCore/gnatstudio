with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;

package Callbacks_Hypergrep is

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   package Check_Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Check_Button_Record);

end Callbacks_Hypergrep;
