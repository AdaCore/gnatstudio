with Gtk.Box;          use Gtk.Box;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtk.Main;
with Gtk.Window;       use Gtk.Window;
with Src_Cb;           use Src_Cb;
with Src_Editor_Box;   use Src_Editor_Box;
with Src_Menu;         use Src_Menu;

with Language.Ada;     use Language.Ada;

procedure Src is
   Main_Window : Gtk_Window;
   V_Box : Gtk.Box.Gtk_Box;
   My_Box : aliased Source_Editor_Box;
   Menu : Gtk_Item_Factory;
   Ignored : Boolean;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Main_Window, Window_Toplevel);
   Set_Title (Main_Window, "The GLIDE Source Editor");
   Set_Default_Size (Main_Window, 350, 400);
   Window_Cb.Connect
     (Main_Window, "destroy",
      Window_Cb.To_Marshaller (Exit_Main'Access));

   Gtk_New_Vbox (V_Box);
   Add (Main_Window, V_Box);

   Gtk_New (My_Box, Lang => Ada_Lang);

   Create_Menu (Menu, Main_Window, My_Box);
   Pack_Start (V_Box, Get_Widget (Menu, Root), False, False, 0);

   Attach (My_Box, V_Box);

   Show_All (Main_Window);
   --  Load_File (My_Box, "/home/brobecke/act/gnat/sem_ch3.adb", Ignored);
   Gtk.Main.Main;
end Src;

