with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Vsearch_Pkg; use Vsearch_Pkg;
with Files_Extra_Info_Pkg; use Files_Extra_Info_Pkg;

procedure Vsearch is
   Vsearch : Vsearch_Access;
   Files_Extra_Info : Files_Extra_Info_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Vsearch);
   Show_All (Vsearch);
   Gtk_New (Files_Extra_Info);
   Show_All (Files_Extra_Info);
   Gtk.Main.Main;
end Vsearch;
