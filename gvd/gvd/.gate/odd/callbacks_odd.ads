with Gtk.Handlers;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Button; use Gtk.Button;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.List; use Gtk.List;
with Gtk.Window; use Gtk.Window;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Text; use Gtk.Text;

package Callbacks_Odd is

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Menu_Item_Record);

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

   package Radio_Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Radio_Button_Record);

   package List_Callback is new
     Gtk.Handlers.Callback (Gtk_List_Record);

   package Window_Callback is new
     Gtk.Handlers.Callback (Gtk_Window_Record);

   package Entry_Callback is new
     Gtk.Handlers.Callback (Gtk_Entry_Record);

   package Text_Callback is new
     Gtk.Handlers.Callback (Gtk_Text_Record);

end Callbacks_Odd;
