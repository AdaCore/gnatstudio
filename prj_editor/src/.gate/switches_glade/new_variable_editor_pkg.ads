with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Box; use Gtk.Box;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package New_Variable_Editor_Pkg is

   type New_Variable_Editor_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1 : Gtk_Vbox;
      Table1 : Gtk_Table;
      Label58 : Gtk_Label;
      Variable_Name : Gtk_Combo;
      Combo_Entry9 : Gtk_Entry;
      Label60 : Gtk_Label;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Viewport1 : Gtk_Viewport;
      Vbox54 : Gtk_Vbox;
      Values_List : Gtk_Clist;
      Label61 : Gtk_Label;
      Hbuttonbox4 : Gtk_Hbutton_Box;
      Delete_Variable : Gtk_Button;
      New_Variable : Gtk_Button;
      Rename_Variable : Gtk_Button;
      Dialog_Action_Area1 : Gtk_Hbox;
   end record;
   type New_Variable_Editor_Access is access all New_Variable_Editor_Record'Class;

   procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access);
   procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class);

end New_Variable_Editor_Pkg;
