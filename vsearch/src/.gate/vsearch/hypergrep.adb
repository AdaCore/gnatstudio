with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Hyper_Grep_Base_Pkg; use Hyper_Grep_Base_Pkg;

procedure Hypergrep is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Hyper_Grep_Base);
   Show_All (Hyper_Grep_Base);
   Gtk.Main.Main;
end Hypergrep;
