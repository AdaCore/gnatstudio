with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Files_Extra_Info_Pkg; use Files_Extra_Info_Pkg;

procedure Files_Extra is
   Files_Extra_Info : Files_Extra_Info_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Files_Extra_Info);
   Show_All (Files_Extra_Info);
   Gtk.Main.Main;
end Files_Extra;
