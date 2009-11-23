with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Clist; use Gtk.Clist;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
package Foreign_Naming_Scheme_Editor_Pkg is

   type Foreign_Naming_Scheme_Editor_Record is new Gtk_Window_Record with record
      Main_Box : Gtk_Vbox;
      Frame46 : Gtk_Frame;
      Table6 : Gtk_Table;
      Label75 : Gtk_Label;
      Label76 : Gtk_Label;
      Header_File_Extension : Gtk_Combo;
      Combo_Entry10 : Gtk_Entry;
      Implementation_Extension : Gtk_Combo;
      Combo_Entry11 : Gtk_Entry;
      Frame47 : Gtk_Frame;
      Vbox57 : Gtk_Vbox;
      Scrolledwindow3 : Gtk_Scrolled_Window;
      Viewport2 : Gtk_Viewport;
      Scrolledwindow4 : Gtk_Scrolled_Window;
      Exception_List : Gtk_Clist;
      Label77 : Gtk_Label;
      Hbox10 : Gtk_Hbox;
      Filename_Entry : Gtk_Entry;
      Update : Gtk_Button;
   end record;
   type Foreign_Naming_Scheme_Editor_Access is access all Foreign_Naming_Scheme_Editor_Record'Class;

   procedure Gtk_New (Foreign_Naming_Scheme_Editor : out Foreign_Naming_Scheme_Editor_Access);
   procedure Initialize (Foreign_Naming_Scheme_Editor : access Foreign_Naming_Scheme_Editor_Record'Class);

end Foreign_Naming_Scheme_Editor_Pkg;
