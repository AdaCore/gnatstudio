with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Wizard_Window_Pkg; use Wizard_Window_Pkg;

procedure Prj_Edit is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Wizard_Window);
   Show_All (Wizard_Window);
   Gtk.Main.Main;
end Prj_Edit;
