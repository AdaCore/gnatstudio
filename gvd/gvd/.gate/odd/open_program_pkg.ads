with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
package Open_Program_Pkg is

   type Open_Program_Record is new Gtk_Window_Record with record
      Vbox13 : Gtk_Vbox;
      Frame8 : Gtk_Frame;
      Table7 : Gtk_Table;
      Gdb_Button : Gtk_Radio_Button;
      Dbx_Button : Gtk_Radio_Button;
      Xdb_Button : Gtk_Radio_Button;
      Jdb_Button : Gtk_Radio_Button;
      Pydb_Button : Gtk_Radio_Button;
      Perl_Button : Gtk_Radio_Button;
      Program_Combo : Gtk_Combo;
      Program_Entry : Gtk_Entry;
      Open_Button : Gtk_Button;
      Host_Combo : Gtk_Combo;
      Host_Entry : Gtk_Entry;
      Label57 : Gtk_Label;
      Label55 : Gtk_Label;
      Label56 : Gtk_Label;
      Label58 : Gtk_Label;
      Launch_Menu : Gtk_Option_Menu;
      Combo8 : Gtk_Combo;
      Protocol_Entry : Gtk_Entry;
      Label60 : Gtk_Label;
      Combo7 : Gtk_Combo;
      Target_Entry : Gtk_Entry;
      Label59 : Gtk_Label;
      Hbuttonbox7 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Help_Button : Gtk_Button;
   end record;
   type Open_Program_Access is access all Open_Program_Record'Class;

   procedure Gtk_New (Open_Program : out Open_Program_Access);
   procedure Initialize (Open_Program : access Open_Program_Record'Class);

   Open_Program : Open_Program_Access;

end Open_Program_Pkg;
