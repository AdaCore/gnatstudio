with Gtk.Main;
with Gtk.Window;           use Gtk.Window;
with Src_Editor_Box;       use Src_Editor_Box;
with Src_Menu;             use Src_Menu;

with Glide_Kernel.Project; use Glide_Kernel.Project;
with Language;             use Language;
with Language.Ada;         use Language.Ada;
with Language.C;           use Language.C;
with Language.Cpp;         use Language.Cpp;

procedure Src is
   My_Box      : Source_Editor_Box;
   Main_Window : Gtk_Window;
   Kernel      : Glide_Kernel.Kernel_Handle;
   --  Ignored     : Boolean;

begin
   Add_File_Extensions (Ada_Lang, ".ads;.adb;.ada");
   Add_File_Extensions (C_Lang, ".h;.c");
   Add_File_Extensions (Cpp_Lang, ".hh;.cc;.cpp");

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Glide_Kernel.Gtk_New (Kernel, null);
   Load_Project (Kernel, "../src_editor.gpr");

   Gtk_New (My_Box, Kernel);
   Create_Main_Window (Main_Window, My_Box);
   Set_Title (Main_Window, "The GLIDE Source Editor");
   Show_All (Main_Window);
   --  Load_File
   --    (My_Box, "src_editor_box.adb", Success => Ignored);

   Gtk.Main.Main;
end Src;

