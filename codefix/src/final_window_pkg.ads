with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Label; use Gtk.Label;

with Codefix.Graphic_Codefix_Pkg; use Codefix.Graphic_Codefix_Pkg;

package Final_Window_Pkg is

   type Final_Window_Record is new Gtk_Dialog_Record with record
      Dialog_Vbox1 : Gtk_Vbox;
      Dialog_Action_Area1 : Gtk_Hbox;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Final_Validation : Gtk_Button;
      Final_Cancel : Gtk_Button;
      Label4 : Gtk_Label;
      Graphic_Codefix : Graphic_Codefix_Access;
   end record;
   type Final_Window_Access is access all Final_Window_Record'Class;

   procedure Gtk_New (Final_Window : out Final_Window_Access);
   procedure Initialize (Final_Window : access Final_Window_Record'Class);

end Final_Window_Pkg;
