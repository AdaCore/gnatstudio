with Gtk; use Gtk;
with Gtk.Main;
with Vsearch_Pkg; use Vsearch_Pkg;

procedure Vsearch is
   Vsearch : Vsearch_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Vsearch);
   Show_All (Vsearch);
   Gtk.Main.Main;
end Vsearch;
