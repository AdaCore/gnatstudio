with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Tooltips; use Gtk.Tooltips;
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
      Vbuttonbox1 : Gtk_Vbutton_Box;
      Prev : Gtk_Button;
      Next : Gtk_Button;
      Accept_Correction : Gtk_Button;
      Undo : Gtk_Button;
      Refresh : Gtk_Button;
   end record;
   type Codefix_Window_Access is access all Codefix_Window_Record'Class;

   procedure Gtk_New (Codefix_Window : out Codefix_Window_Access);
   procedure Initialize (Codefix_Window : access Codefix_Window_Record'Class);

end Codefix_Window_Pkg;
