with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Glide_Pkg; use Glide_Pkg;

procedure Glide is
   Glide : Glide_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Glide);
   Show_All (Glide);
   Gtk.Main.Main;
end Glide;
