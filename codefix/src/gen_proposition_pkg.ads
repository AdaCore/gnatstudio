with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Button; use Gtk.Button;
package Gen_Proposition_Pkg is

   type Gen_Proposition_Record is new Gtk_Window_Record with record
      Choices_Box : Gtk_Vbox;
      Scrolledwindow4 : Gtk_Scrolled_Window;
      Old_Text : Gtk_Text;
      Scrolledwindow5 : Gtk_Scrolled_Window;
      New_Text : Gtk_Text;
   end record;
   type Gen_Proposition_Access is access all Gen_Proposition_Record'Class;

   procedure Gtk_New (Gen_Proposition : out Gen_Proposition_Access);
   procedure Initialize (Gen_Proposition : access Gen_Proposition_Record'Class);

end Gen_Proposition_Pkg;
