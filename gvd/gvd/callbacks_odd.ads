with Gtk.Handlers;
with Gtk.Window; use Gtk.Window;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Button; use Gtk.Button;

package Callbacks_Odd is

   package Window_Callback is new
     Gtk.Handlers.Callback (Gtk_Window_Record);

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Menu_Item_Record);

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk_Widget_Record);

   package Check_Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Check_Menu_Item_Record);

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk_Button_Record);

end Callbacks_Odd;
