with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Status_Bar; use Gtk.Status_Bar;
package Builder_Pkg is

   type Builder_Record is new Gtk_Window_Record with record
      Terminated : Boolean := False;
      Vbox1 : Gtk_Vbox;
      Frame1 : Gtk_Frame;
      Hbox1 : Gtk_Hbox;
      Label1 : Gtk_Label;
      Build_Entry : Gtk_Entry;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Build : Gtk_Button;
      Quit : Gtk_Button;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Source_Editor : Gtk_Text;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Output_Text : Gtk_Text;
      Statusbar : Gtk_Statusbar;
   end record;
   type Builder_Access is access all Builder_Record'Class;

   procedure Gtk_New (Builder : out Builder_Access);
   procedure Initialize (Builder : access Builder_Record'Class);

end Builder_Pkg;
