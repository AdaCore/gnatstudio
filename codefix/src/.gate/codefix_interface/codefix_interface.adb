with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Codefix_Window_Pkg; use Codefix_Window_Pkg;
with Gen_Proposition_Pkg; use Gen_Proposition_Pkg;
with Final_Window_Pkg; use Final_Window_Pkg;

procedure Codefix_Interface is
   Codefix_Window : Codefix_Window_Access;
   Gen_Proposition : Gen_Proposition_Access;
   Final_Window : Final_Window_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Codefix_Window);
   Show_All (Codefix_Window);
   Gtk_New (Gen_Proposition);
   Show_All (Gen_Proposition);
   Gtk_New (Final_Window);
   Show_All (Final_Window);
   Gtk.Main.Main;
end Codefix_Interface;
