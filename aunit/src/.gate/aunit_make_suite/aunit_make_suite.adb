with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Make_Suite_Window_Pkg; use Make_Suite_Window_Pkg;
with Explorer_Pkg; use Explorer_Pkg;

procedure Aunit_Make_Suite is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Make_Suite_Window);
   Show_All (Make_Suite_Window);
   Gtk_New (Explorer);
   Show_All (Explorer);
   Gtk.Main.Main;
end Aunit_Make_Suite;
