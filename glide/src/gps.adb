with Gtk; use Gtk;
with Gtk.Main;
with Glide_Page;
with Glide_Menu;
with Glide_Main_Window;

procedure Glide2 is
   use Glide_Main_Window;

   Glide : Glide_Window;
   Page  : Glide_Page.Glide_Page;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Glide, "<glide>", Glide_Menu.Glide_Menu_Items.all);
   Set_Title (Glide, "Glide - New Generation");
   Maximize (Glide);
   Glide.Gvd_Home_Dir := new String' ("");
   Glide.Prefix_Directory := new String' ("");

   Glide_Page.Gtk_New (Page, Glide);
   Show_All (Glide);
   Gtk.Main.Main;
end Glide2;
