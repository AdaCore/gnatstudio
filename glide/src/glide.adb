with Gtk; use Gtk;
with Gtk.Main;
with Glide_Pkg; use Glide_Pkg;

with Language;      use Language;
with Language.Ada;  use Language.Ada;
with Language.C;    use Language.C;
with Language.Cpp;  use Language.Cpp;
with Language.Java; use Language.Java;

procedure Glide is
   Glide : Glide_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   --  Be careful with *.h files: both C and C++ !

   Add_File_Extensions (Ada_Lang,  ".ads;.adb");
   Add_File_Extensions (C_Lang,    ".c;.h");
   Add_File_Extensions (Cpp_Lang,  ".cxx;.cpp;.h");
   Add_File_Extensions (Java_Lang, ".java");

   Gtk_New (Glide);
   Show_All (Glide);
   Gtk.Main.Main;
end Glide;
