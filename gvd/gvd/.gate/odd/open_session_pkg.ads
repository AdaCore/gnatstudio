with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Label; use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
package Open_Session_Pkg is

   type Open_Session_Record is new Gtk_Window_Record with record
      Vbox17 : Gtk_Vbox;
      Frame13 : Gtk_Frame;
      Table8 : Gtk_Table;
      Combo12 : Gtk_Combo;
      Program_Entry : Gtk_Entry;
      Open_Button : Gtk_Button;
      Launch_Menu : Gtk_Option_Menu;
      Label69 : Gtk_Label;
      Label71 : Gtk_Label;
      Hbuttonbox9 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Help_Button : Gtk_Button;
   end record;
   type Open_Session_Access is access all Open_Session_Record'Class;

   procedure Gtk_New (Open_Session : out Open_Session_Access);
   procedure Initialize (Open_Session : access Open_Session_Record'Class);

   Open_Session : Open_Session_Access;

end Open_Session_Pkg;
