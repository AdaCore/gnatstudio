with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Explorer_Window_Pkg is

   type Explorer_Window_Record is new Gtk_Window_Record with record
      Vbox7 : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Clist : Gtk_Clist;
      Label4 : Gtk_Label;
      Label5 : Gtk_Label;
      Hbuttonbox2 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Cancel : Gtk_Button;
   end record;
   type Explorer_Window_Access is access all Explorer_Window_Record'Class;

   procedure Gtk_New (Explorer_Window : out Explorer_Window_Access);
   procedure Initialize (Explorer_Window : access Explorer_Window_Record'Class);

end Explorer_Window_Pkg;
