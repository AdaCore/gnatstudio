with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Vdiff_Pkg; use Vdiff_Pkg;

procedure Vdiff is
   Vdiff : Vdiff_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Vdiff);
   Show_All (Vdiff);
   Gtk.Main.Main;
end Vdiff;
