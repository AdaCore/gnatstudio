with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.List; use Gtk.List;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Open_Session_Pkg is

   type Open_Session_Record is new Gtk_Window_Record with record
      Vbox17 : Gtk_Vbox;
      Hbox7 : Gtk_Hbox;
      Vbox18 : Gtk_Vbox;
      Label94 : Gtk_Label;
      Scrolledwindow10 : Gtk_Scrolled_Window;
      Viewport1 : Gtk_Viewport;
      List : Gtk_List;
      Hbox6 : Gtk_Hbox;
      Label73 : Gtk_Label;
      Entry1 : Gtk_Entry;
      Vseparator4 : Gtk_Vseparator;
      Vbox19 : Gtk_Vbox;
      Scrolledwindow11 : Gtk_Scrolled_Window;
      Viewport2 : Gtk_Viewport;
      File_Buttons : Gtk_Vbox;
      Hbuttonbox10 : Gtk_Hbutton_Box;
      Select_All : Gtk_Button;
      Unselect_All : Gtk_Button;
      Hseparator1 : Gtk_Hseparator;
      Hbuttonbox9 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type Open_Session_Access is access all Open_Session_Record'Class;

   procedure Gtk_New (Open_Session : out Open_Session_Access);
   procedure Initialize (Open_Session : access Open_Session_Record'Class);

   Open_Session : Open_Session_Access;

end Open_Session_Pkg;
