with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
package Memory_View_Pkg is

   type Memory_View_Record is new Gtk_Window_Record with record
      Vbox20 : Gtk_Vbox;
      Frame : Gtk_Frame;
      Hbox8 : Gtk_Hbox;
      Hbox11 : Gtk_Hbox;
      Vbox23 : Gtk_Vbox;
      Label95 : Gtk_Label;
      Label96 : Gtk_Label;
      Vbox24 : Gtk_Vbox;
      Address_Entry : Gtk_Entry;
      Value_Entry : Gtk_Entry;
      Vbuttonbox6 : Gtk_Vbutton_Box;
      Address_View : Gtk_Button;
      Value_Update : Gtk_Button;
      Hbox12 : Gtk_Hbox;
      Vbuttonbox5 : Gtk_Vbutton_Box;
      Pgup : Gtk_Button;
      Pixmap1 : Gtk_Pixmap;
      Pgdn : Gtk_Button;
      Pixmap2 : Gtk_Pixmap;
      Vseparator5 : Gtk_Vseparator;
      Label99 : Gtk_Label;
      Page_Size_Entry : Gtk_Entry;
      Label100 : Gtk_Label;
      Vseparator6 : Gtk_Vseparator;
      Label98 : Gtk_Label;
      Size : Gtk_Combo;
      Size_Entry : Gtk_Entry;
      Label97 : Gtk_Label;
      Format : Gtk_Combo;
      Data_Entry : Gtk_Entry;
      Scrolledwindow : Gtk_Scrolled_Window;
      View : Gtk_Text;
      Hbuttonbox11 : Gtk_Hbutton_Box;
      Reset : Gtk_Button;
      Submit : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;
   end record;
   type Memory_View_Access is access all Memory_View_Record'Class;

   procedure Gtk_New (Memory_View : out Memory_View_Access);
   procedure Initialize (Memory_View : access Memory_View_Record'Class);

   Memory_View : Memory_View_Access;

end Memory_View_Pkg;
