with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
with Gtk.Status_Bar; use Gtk.Status_Bar;
package Make_Harness_Window_Pkg is

   type Make_Harness_Window_Record is new Gtk_Window_Record with record
      Vbox3 : Gtk_Vbox;
      Hbox2 : Gtk_Hbox;
      Vbox8 : Gtk_Vbox;
      Label6 : Gtk_Label;
      Vbox6 : Gtk_Vbox;
      Label1 : Gtk_Label;
      Vbox9 : Gtk_Vbox;
      Hbox4 : Gtk_Hbox;
      Procedure_Entry : Gtk_Entry;
      Hbox3 : Gtk_Hbox;
      File_Name_Entry : Gtk_Entry;
      Vbox10 : Gtk_Vbox;
      Browse : Gtk_Button;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Ok : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;
      Statusbar : Gtk_Statusbar;
   end record;
   type Make_Harness_Window_Access is access all Make_Harness_Window_Record'Class;

   procedure Gtk_New (Make_Harness_Window : out Make_Harness_Window_Access);
   procedure Initialize (Make_Harness_Window : access Make_Harness_Window_Record'Class);

end Make_Harness_Window_Pkg;
