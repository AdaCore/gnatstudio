with Gtk; use Gtk;
with Gtk.Main;

with Test_File_Selector; use Test_File_Selector;
with Gtkada.File_Selector; use Gtkada.File_Selector;

procedure Run_Test_File_Selector is
   File_Selector_Window : File_Selector_Window_Access;

   Filter_A : Filter_Show_All_Access := new Filter_Show_All;
   Filter_B : Filter_Show_Txt_Access := new Filter_Show_Txt;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (File_Selector_Window, "/home/nico/");

   Register_Filter (File_Selector_Window, Filter_A);
   Register_Filter (File_Selector_Window, Filter_B);

   Show_All (File_Selector_Window);
   Gtk.Main.Main;
end Run_Test_File_Selector;
