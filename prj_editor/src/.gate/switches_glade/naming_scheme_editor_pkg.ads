with Gtk.Window; use Gtk.Window;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Button; use Gtk.Button;
package Naming_Scheme_Editor_Pkg is

   type Naming_Scheme_Editor_Record is new Gtk_Window_Record with record
      Alignment1 : Gtk_Alignment;
      Main_Box : Gtk_Vbox;
      Frame29 : Gtk_Frame;
      Standard_Scheme : Gtk_Combo;
      Combo_Entry3 : Gtk_Entry;
      Frame28 : Gtk_Frame;
      Table1 : Gtk_Table;
      Casing : Gtk_Combo;
      Combo_Entry2 : Gtk_Entry;
      Label23 : Gtk_Label;
      Label24 : Gtk_Label;
      Dot_Replacement : Gtk_Entry;
      Label25 : Gtk_Label;
      Label26 : Gtk_Label;
      Label27 : Gtk_Label;
      Spec_Extension : Gtk_Combo;
      Combo_Entry4 : Gtk_Entry;
      Body_Extension : Gtk_Combo;
      Combo_Entry5 : Gtk_Entry;
      Separate_Extension : Gtk_Combo;
      Combo_Entry6 : Gtk_Entry;
      Frame30 : Gtk_Frame;
      Vbox29 : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Exception_List : Gtk_Clist;
      Label28 : Gtk_Label;
      Label29 : Gtk_Label;
      Label30 : Gtk_Label;
      Hbox3 : Gtk_Hbox;
      Unit_Name_Entry : Gtk_Entry;
      Spec_Filename_Entry : Gtk_Entry;
      Body_Filename_Entry : Gtk_Entry;
      Update : Gtk_Button;
   end record;
   type Naming_Scheme_Editor_Access is access all Naming_Scheme_Editor_Record'Class;

   procedure Gtk_New (Naming_Scheme_Editor : out Naming_Scheme_Editor_Access);
   procedure Initialize (Naming_Scheme_Editor : access Naming_Scheme_Editor_Record'Class);

   Naming_Scheme_Editor : Naming_Scheme_Editor_Access;

end Naming_Scheme_Editor_Pkg;
