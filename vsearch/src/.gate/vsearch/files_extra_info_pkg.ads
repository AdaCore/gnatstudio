with Gtk.Window; use Gtk.Window;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Button; use Gtk.Button;
package Files_Extra_Info_Pkg is

   type Files_Extra_Info_Record is new Gtk_Window_Record with record
      Files_Frame : Gtk_Frame;
      Files_Table : Gtk_Table;
      Files_Label : Gtk_Label;
      Directory_Label : Gtk_Label;
      Files_Combo : Gtk_Combo;
      Files_Entry : Gtk_Entry;
      Directory_Combo : Gtk_Combo;
      Directory_Entry : Gtk_Entry;
      Subdirs_Check : Gtk_Check_Button;
      Browse_Button : Gtk_Button;
   end record;
   type Files_Extra_Info_Access is access all Files_Extra_Info_Record'Class;

   procedure Gtk_New (Files_Extra_Info : out Files_Extra_Info_Access);
   procedure Initialize (Files_Extra_Info : access Files_Extra_Info_Record'Class);

end Files_Extra_Info_Pkg;
