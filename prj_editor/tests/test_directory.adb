with Directory_Tree;  use Directory_Tree;
with Gtk.Main;        use Gtk.Main;
with Gtk.Window;      use Gtk.Window;
with Gtk.Enums;       use Gtk.Enums;

procedure Test_Directory is
   Win : Gtk_Window;
   Dir : Dir_Tree;
begin
   Gtk.Main.Init;
   Gtk_New (Win, Window_Toplevel);

   Gtk_New (Dir, "/");
   Show_Directory (Dir, "/home/briot/");
   Add_Directory (Dir, "/home/dummy/");
   Add_Directory (Dir, "C:/");
   Add (Win, Dir);
   Set_USize (Dir, 400, 400);

   Show_All (Win);
   Gtk.Main.Main;
end Test_Directory;
