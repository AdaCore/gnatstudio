with Gtk.Main;
with Gtk.Window;       use Gtk.Window;
with Src_Editor_Box;   use Src_Editor_Box;
with Src_Menu;         use Src_Menu;

with Language;         use Language;
with Language.Ada;     use Language.Ada;
with Language.C;       use Language.C;
with Language.Cpp;     use Language.Cpp;

procedure Src is
   My_Box      : Source_Editor_Box;
   Main_Window : Gtk_Window;
   Ignored     : Boolean;
begin

   Add_File_Extensions (Ada_Lang, ".ads;.adb;.ada");
   Add_File_Extensions (C_Lang, ".h;.c");
   Add_File_Extensions (Cpp_Lang, ".hh;.cc;.cpp");

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (My_Box);
   Create_Main_Window (Main_Window, My_Box);
   Show_All (Main_Window);
   Load_File (My_Box, "/home/brobecke/act/gnat/sem_ch3.adb",
              Success => Ignored);

   Gtk.Main.Main;
end Src;

