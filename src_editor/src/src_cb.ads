with Gtk.Handlers;
with Gtk.Widget;      use Gtk.Widget;

package Src_Cb is

   package Window_Cb is new Gtk.Handlers.Callback (Gtk_Widget_Record);

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class);

end Src_Cb;
