with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label; use Gtk.Label;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Wizard_Window_Pkg is

   type Wizard_Window_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Page_Box : Gtk_Hbox;
      Eventbox1 : Gtk_Event_Box;
      Toc_Box : Gtk_Vbox;
      Notebook : Gtk_Notebook;
      Label1 : Gtk_Label;
      Hseparator1 : Gtk_Hseparator;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Previous : Gtk_Button;
      Next : Gtk_Button;
      Finish : Gtk_Button;
      Cancel : Gtk_Button;
   end record;
   type Wizard_Window_Access is access all Wizard_Window_Record'Class;

   procedure Gtk_New (Wizard_Window : out Wizard_Window_Access);
   procedure Initialize (Wizard_Window : access Wizard_Window_Record'Class);

   Wizard_Window : Wizard_Window_Access;

end Wizard_Window_Pkg;
