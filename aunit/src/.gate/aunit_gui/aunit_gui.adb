with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Make_Harness_Window_Pkg; use Make_Harness_Window_Pkg;
with Make_Suite_Window_Pkg; use Make_Suite_Window_Pkg;
with Make_Test_Window_Pkg; use Make_Test_Window_Pkg;

procedure Aunit_Gui is
   Make_Harness_Window : Make_Harness_Window_Access;
   Make_Suite_Window : Make_Suite_Window_Access;
   Make_Test_Window : Make_Test_Window_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Make_Harness_Window);
   Show_All (Make_Harness_Window);
   Gtk_New (Make_Suite_Window);
   Show_All (Make_Suite_Window);
   Gtk_New (Make_Test_Window);
   Show_All (Make_Test_Window);
   Gtk.Main.Main;
end Aunit_Gui;
