with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;
package Vdiff_Pkg is

   type Vdiff_Record is new Gtk_Window_Record with record
      Ignore_Value_Changed : Boolean := False;
      Vbox1 : Gtk_Vbox;
      Toolbar1 : Gtk_Toolbar;
      Button1 : Gtk_Widget;
      Button2 : Gtk_Widget;
      Button3 : Gtk_Widget;
      Button4 : Gtk_Widget;
      Button5 : Gtk_Widget;
      Search_Combo : Gtk_Combo;
      Combo_Entry1 : Gtk_Entry;
      Button6 : Gtk_Widget;
      Button7 : Gtk_Widget;
      Button8 : Gtk_Widget;
      Main_Frame : Gtk_Frame;
      Hbox2 : Gtk_Hbox;
      Vbox2 : Gtk_Vbox;
      Hbox4 : Gtk_Hbox;
      Label5 : Gtk_Label;
      Frame3 : Gtk_Frame;
      File_Label1 : Gtk_Label;
      File1_Box : Gtk_Hbox;
      Frame5 : Gtk_Frame;
      Drawingarea1 : Gtk_Drawing_Area;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Clist1 : Gtk_Clist;
      Label8 : Gtk_Label;
      Label9 : Gtk_Label;
      Vseparator1 : Gtk_Vseparator;
      Vbox3 : Gtk_Vbox;
      Hbox5 : Gtk_Hbox;
      Label7 : Gtk_Label;
      Frame4 : Gtk_Frame;
      File_Label2 : Gtk_Label;
      File2_Box : Gtk_Hbox;
      Frame6 : Gtk_Frame;
      Drawingarea2 : Gtk_Drawing_Area;
      Scrolledwindow2 : Gtk_Scrolled_Window;
      Clist2 : Gtk_Clist;
      Label10 : Gtk_Label;
      Label11 : Gtk_Label;
      Statusbar1 : Gtk_Statusbar;
   end record;
   type Vdiff_Access is access all Vdiff_Record'Class;

   procedure Gtk_New (Vdiff : out Vdiff_Access);
   procedure Initialize (Vdiff : access Vdiff_Record'Class);

   Vdiff : Vdiff_Access;

end Vdiff_Pkg;
