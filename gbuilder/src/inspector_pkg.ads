with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Button; use Gtk.Button;
package Inspector_Pkg is

   type Inspector_Record is new Gtk_Window_Record with record
      Vbox2 : Gtk_Vbox;
      Combo1 : Gtk_Combo;
      Combo_Entry1 : Gtk_Entry;
      Notebook4 : Gtk_Notebook;
      Scrolledwindow4 : Gtk_Scrolled_Window;
      Clist1 : Gtk_Clist;
      Label23 : Gtk_Label;
      Label24 : Gtk_Label;
      Label25 : Gtk_Label;
      Scrolledwindow5 : Gtk_Scrolled_Window;
      Clist2 : Gtk_Clist;
      Label26 : Gtk_Label;
      Label27 : Gtk_Label;
      Label28 : Gtk_Label;
   end record;
   type Inspector_Access is access all Inspector_Record'Class;

   procedure Gtk_New (Inspector : out Inspector_Access);
   procedure Initialize (Inspector : access Inspector_Record'Class);

   Inspector : Inspector_Access;

end Inspector_Pkg;
