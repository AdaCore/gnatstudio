with Gtk; use Gtk;
with Gtk.Main;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Gtkada.Intl; use Gtkada.Intl;

procedure Odd is
begin
   Bind_Text_Domain ("GtkAda", "/usr/local/share/locale");
   Bind_Text_Domain ("Odd", "/usr/local/share/locale");
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Main_Debug_Window);
   Show_All (Main_Debug_Window);
   Gtk.Main.Main;
end Odd;
