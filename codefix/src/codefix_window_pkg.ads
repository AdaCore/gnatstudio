with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Codefix_Window_Pkg is

   type Codefix_Window_Record is new Gtk_Window_Record with record
      Hbox1 : Gtk_Hbox;
      Vbox6 : Gtk_Vbox;
      Error_Caption : Gtk_Label;
      Choices_Proposed : Gtk_Notebook;
      Vbox4 : Gtk_Vbox;
      Label3 : Gtk_Label;
      Fix_Caption_List : Gtk_Combo;
      Fix_Entry : Gtk_Entry;
      Vbox5 : Gtk_Vbox;
      Vbuttonbox1 : Gtk_Vbutton_Box;
      Alignment1 : Gtk_Alignment;
      Skip_Correction : Gtk_Button;
      Accept_Correction : Gtk_Button;
      Vbuttonbox2 : Gtk_Vbutton_Box;
      Cancel_Changes : Gtk_Button;
      Apply_Changes : Gtk_Button;
   end record;
   type Codefix_Window_Access is access all Codefix_Window_Record'Class;

   procedure Gtk_New (Codefix_Window : out Codefix_Window_Access);
   procedure Initialize (Codefix_Window : access Codefix_Window_Record'Class);

end Codefix_Window_Pkg;
