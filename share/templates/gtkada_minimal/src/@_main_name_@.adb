with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.Label;       use Gtk.Label;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Main;
with Gtk.Window;      use Gtk.Window;
with Gtkada.Handlers; use Gtkada.Handlers;

procedure @_Main_Name_@ is

   Win   : Gtk_Window;
   Label : Gtk_Label;
   Box   : Gtk_Vbox;

begin
   --  Initialize GtkAda.
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   --  Create a window with a size of 400x400
   Gtk_New (Win);
   Win.Set_Default_Size (400, 400);

   --  Create a box to organize vertically the contents of the window
   Gtk_New_Vbox (Box);

   --  Add a label
   Gtk_New (Label, "Hello world.");
   Win.Add (Label);

   --  Show the window
   Win.Show_All;

   --  Start the Gtk+ main loop
   Gtk.Main.Main;
end @_Main_Name_@;
