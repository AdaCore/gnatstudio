with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;
package Main_Debug_Window_Pkg is

   type Main_Debug_Window_Record is new Gtk_Window_Record with record
      Vbox : Gtk_Vbox;
      Factory : Gtk_Menu_Bar;
      Toolbar_Box : Gtk_Vbox;
      Frame : Gtk_Frame;
      Process_Notebook : Gtk_Notebook;
      Statusbar : Gtk_Statusbar;
   end record;
   type Main_Debug_Window_Access is access all Main_Debug_Window_Record'Class;

   procedure Gtk_New (Main_Debug_Window : out Main_Debug_Window_Access);
   procedure Initialize (Main_Debug_Window : access Main_Debug_Window_Record'Class);

end Main_Debug_Window_Pkg;
