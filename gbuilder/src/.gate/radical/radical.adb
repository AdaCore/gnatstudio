with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gui_Builder_Pkg; use Gui_Builder_Pkg;

procedure Radical is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Gui_Builder);
   Show_All (Gui_Builder);
   Gtk.Main.Main;
end Radical;
