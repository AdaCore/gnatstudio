with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Generic_Values; use Generic_Values;
package Print_Dialog_Pkg is

   type Print_Dialog_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1 : Gtk_Vbox;
      Vbox1 : Gtk_Vbox;
      Label1 : Gtk_Label;
      Combo1 : Gtk_Combo;
      Combo_Entry1 : Gtk_Entry;
      Dialog_Action_Area1 : Gtk_Hbox;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Print_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Help_Button : Gtk_Button;
      Variable : String_Access;
   end record;
   type Print_Dialog_Access is access all Print_Dialog_Record'Class;

   procedure Gtk_New (Print_Dialog : out Print_Dialog_Access);
   procedure Initialize (Print_Dialog : access Print_Dialog_Record'Class);

   Print_Dialog : Print_Dialog_Access;

end Print_Dialog_Pkg;
