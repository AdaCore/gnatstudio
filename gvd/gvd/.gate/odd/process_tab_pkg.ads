with Gtk.Window; use Gtk.Window;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Text; use Gtk.Text;
with Gtk.Button; use Gtk.Button;
package Process_Tab_Pkg is

   type Process_Tab_Record is new Gtk_Window_Record with record
      Process_Paned : Gtk_Vpaned;
      Vpaned6 : Gtk_Vpaned;
      Scrolledwindow9 : Gtk_Scrolled_Window;
      Data_Canvas : Gtk_Viewport;
      File_Notebook : Gtk_Notebook;
      Frame10 : Gtk_Frame;
      Editor_Text : Gtk_Hbox;
      Label52 : Gtk_Label;
      Scrolledwindow7 : Gtk_Scrolled_Window;
      Debugger_Text : Gtk_Text;
   end record;
   type Process_Tab_Access is access all Process_Tab_Record'Class;

   procedure Gtk_New (Process_Tab : out Process_Tab_Access);
   procedure Initialize (Process_Tab : access Process_Tab_Record'Class);

   Process_Tab : Process_Tab_Access;

end Process_Tab_Pkg;
