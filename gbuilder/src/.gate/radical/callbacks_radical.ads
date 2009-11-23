with Gtk.Handlers;
with Gtk.Menu_Item; use Gtk.Menu_Item;

package Callbacks_Radical is

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk_Menu_Item_Record);

end Callbacks_Radical;
