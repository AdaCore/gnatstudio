with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Explorer_Pkg is

   type Explorer_Record is new Gtk_Window_Record with record
      Vbox5 : Gtk_Vbox;
      Scrolledwindow3 : Gtk_Scrolled_Window;
      Clist : Gtk_Clist;
      Label3 : Gtk_Label;
      Label4 : Gtk_Label;
      Hbuttonbox2 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Close : Gtk_Button;
   end record;
   type Explorer_Access is access all Explorer_Record'Class;

   procedure Gtk_New (Explorer : out Explorer_Access);
   procedure Initialize (Explorer : access Explorer_Record'Class);

   Explorer : Explorer_Access;

end Explorer_Pkg;
