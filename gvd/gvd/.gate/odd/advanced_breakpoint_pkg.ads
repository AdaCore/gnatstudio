with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Advanced_Breakpoint_Pkg is

   type Advanced_Breakpoint_Record is new Gtk_Window_Record with record
      Main_Box : Gtk_Vbox;
      Condition_Frame : Gtk_Frame;
      Vbox5 : Gtk_Vbox;
      Label7 : Gtk_Label;
      Condition_Combo : Gtk_Combo;
      Combo_Entry2 : Gtk_Entry;
      Ignore_Count_Frame : Gtk_Frame;
      Vbox6 : Gtk_Vbox;
      Label8 : Gtk_Label;
      Ignore_Count_Combo : Gtk_Spin_Button;
      Command_Frame : Gtk_Frame;
      Vbox12 : Gtk_Vbox;
      Label13 : Gtk_Label;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Command_Descr : Gtk_Text;
      Hbuttonbox3 : Gtk_Hbutton_Box;
      Record_Button : Gtk_Button;
      End_Button : Gtk_Button;
   end record;
   type Advanced_Breakpoint_Access is access all Advanced_Breakpoint_Record'Class;

   procedure Gtk_New (Advanced_Breakpoint : out Advanced_Breakpoint_Access);
   procedure Initialize (Advanced_Breakpoint : access Advanced_Breakpoint_Record'Class);

   Advanced_Breakpoint : Advanced_Breakpoint_Access;

end Advanced_Breakpoint_Pkg;
