with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Codefix_Window_Pkg; use Codefix_Window_Pkg;
with Codefix.Graphic_Codefix_Pkg; use Codefix.Graphic_Codefix_Pkg;

procedure Codefix_Interface is
   Graphic_Codefix : Graphic_Codefix_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Graphic_Codefix);
   Show_All (Graphic_Codefix);
   Gtk.Main.Main;
end Codefix_Interface;
