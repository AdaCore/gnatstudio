with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Hyper_Grep_Window_Pkg; use Hyper_Grep_Window_Pkg;

with Language;      use Language;
With Language.Ada;  use Language.Ada;
With Language.C;    use Language.C;
With Language.Cpp;  use Language.Cpp;
With Language.Java; use Language.Java;

procedure Hypergrep is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   --  Be careful with .h files: both C and C++ !
   Add_File_Extensions (Ada_Lang,  ".ads;.adb");
   Add_File_Extensions (C_Lang,    ".c;.h");
   Add_File_Extensions (Cpp_Lang,  ".cxx;.cpp;.h");
   Add_File_Extensions (Java_Lang, ".java");

   Gtk_New (Hyper_Grep_Window);
   Show_All (Hyper_Grep_Window);
   Gtk.Main.Main;
end Hypergrep;
