with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Button; use Gtk.Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
package Scenario_Editor_Pkg is

   type Scenario_Editor_Record is new Gtk_Window_Record with record
      Vbox31 : Gtk_Vbox;
      Scrolledwindow3 : Gtk_Scrolled_Window;
      Viewport2 : Gtk_Viewport;
      List_Variables : Gtk_Table;
      Label43 : Gtk_Label;
      Label45 : Gtk_Label;
      Label47 : Gtk_Label;
      Label48 : Gtk_Label;
      Combo6 : Gtk_Combo;
      Entry7 : Gtk_Entry;
      Combo7 : Gtk_Combo;
      Entry8 : Gtk_Entry;
      Entry10 : Gtk_Entry;
      Entry9 : Gtk_Entry;
      Label44 : Gtk_Label;
      Label49 : Gtk_Label;
      Label50 : Gtk_Label;
      Label51 : Gtk_Label;
      Label52 : Gtk_Label;
      Label53 : Gtk_Label;
      Label54 : Gtk_Label;
      Hseparator3 : Gtk_Hseparator;
      Button8 : Gtk_Button;
      Button12 : Gtk_Button;
      Button13 : Gtk_Button;
      Button7 : Gtk_Button;
      Button9 : Gtk_Button;
      Button10 : Gtk_Button;
      Button11 : Gtk_Button;
      Button16 : Gtk_Button;
      Hbuttonbox2 : Gtk_Hbutton_Box;
      Add_Button : Gtk_Button;
      Close_Button : Gtk_Button;
   end record;
   type Scenario_Editor_Access is access all Scenario_Editor_Record'Class;

   procedure Gtk_New (Scenario_Editor : out Scenario_Editor_Access);
   procedure Initialize (Scenario_Editor : access Scenario_Editor_Record'Class);

   Scenario_Editor : Scenario_Editor_Access;

end Scenario_Editor_Pkg;
