with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Text; use Gtk.Text;
with Gtk.Button; use Gtk.Button;
package Process_Tab_Pkg is

   type Process_Tab_Record is new Gtk_Window_Record with record
      Process_Hbox : Gtk_Hbox;
      Process_Paned : Gtk_Vpaned;
      Data_Editor_Paned : Gtk_Vpaned;
      Data_Paned : Gtk_Hpaned;
      Stack_Scrolledwindow : Gtk_Scrolled_Window;
      Stack_List : Gtk_Clist;
      Label101 : Gtk_Label;
      Label201 : Gtk_Label;
      Label202 : Gtk_Label;
      Label203 : Gtk_Label;
      Label204 : Gtk_Label;
      Data_Scrolledwindow : Gtk_Scrolled_Window;
      Data_Canvas : Gtk_Viewport;
      Editor_Vbox : Gtk_Vbox;
      Label_Hbox : Gtk_Hbox;
      Explorer_Separator : Gtk_Hseparator;
      Editor_Label : Gtk_Label;
      Editor_Separator : Gtk_Hseparator;
      Editor_Text : Gtk_Hbox;
      Command_Scrolledwindow : Gtk_Scrolled_Window;
      Debugger_Text : Gtk_Text;
   end record;
   type Process_Tab_Access is access all Process_Tab_Record'Class;

   procedure Gtk_New (Process_Tab : out Process_Tab_Access);
   procedure Initialize (Process_Tab : access Process_Tab_Record'Class);

   Process_Tab : Process_Tab_Access;

end Process_Tab_Pkg;
