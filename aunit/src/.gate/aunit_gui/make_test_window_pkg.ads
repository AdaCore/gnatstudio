with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Make_Test_Window_Pkg is

   type Make_Test_Window_Record is new Gtk_Window_Record with record
      Vbox16 : Gtk_Vbox;
      Hbox7 : Gtk_Hbox;
      Vbox17 : Gtk_Vbox;
      Label14 : Gtk_Label;
      Label15 : Gtk_Label;
      Label16 : Gtk_Label;
      Label17 : Gtk_Label;
      Vbox18 : Gtk_Vbox;
      Name_Entry : Gtk_Entry;
      Description_Entry : Gtk_Entry;
      Override_Tear_Down : Gtk_Check_Button;
      Override_Set_Up : Gtk_Check_Button;
      Hbuttonbox5 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;
   end record;
   type Make_Test_Window_Access is access all Make_Test_Window_Record'Class;

   procedure Gtk_New (Make_Test_Window : out Make_Test_Window_Access);
   procedure Initialize (Make_Test_Window : access Make_Test_Window_Record'Class);

end Make_Test_Window_Pkg;
