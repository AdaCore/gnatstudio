with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Make_Harness_Window_Pkg; use Make_Harness_Window_Pkg;
with Explorer_Window_Pkg; use Explorer_Window_Pkg;

procedure Aunit_Make_Harness is
   Make_Harness_Window : Make_Harness_Window_Access;
   Explorer_Window : Explorer_Window_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Make_Harness_Window);
   Show_All (Make_Harness_Window);
   Gtk_New (Explorer_Window);
   Show_All (Explorer_Window);
   Gtk.Main.Main;
end Aunit_Make_Harness;
