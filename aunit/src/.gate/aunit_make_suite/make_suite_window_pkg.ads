with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
package Make_Suite_Window_Pkg is

   type Make_Suite_Window_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Hbox1 : Gtk_Hbox;
      Vbox2 : Gtk_Vbox;
      Label1 : Gtk_Label;
      Vbox3 : Gtk_Vbox;
      Name_Entry : Gtk_Entry;
      Vbox4 : Gtk_Vbox;
      Label2 : Gtk_Label;
      Hbox2 : Gtk_Hbox;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Test_List : Gtk_Clist;
      Label5 : Gtk_Label;
      Label6 : Gtk_Label;
      Label7 : Gtk_Label;
      Vbuttonbox1 : Gtk_Vbutton_Box;
      Add : Gtk_Button;
      Remove : Gtk_Button;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;
   end record;
   type Make_Suite_Window_Access is access all Make_Suite_Window_Record'Class;

   procedure Gtk_New (Make_Suite_Window : out Make_Suite_Window_Access);
   procedure Initialize (Make_Suite_Window : access Make_Suite_Window_Record'Class);

   Make_Suite_Window : Make_Suite_Window_Access;

end Make_Suite_Window_Pkg;
