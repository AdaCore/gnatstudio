with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Make_Test_Window_Pkg; use Make_Test_Window_Pkg;

procedure Aunit_Make_Test is
   Make_Test_Window : Make_Test_Window_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Make_Test_Window);
   Show_All (Make_Test_Window);
   Gtk.Main.Main;
end Aunit_Make_Test;
