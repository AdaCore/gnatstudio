with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
package Hyper_Grep_Window_Pkg is

   type Hyper_Grep_Window_Record is new Gtk_Window_Record with record
      Main_Table : Gtk_Vbox;
      Search_Frame : Gtk_Frame;
      Search_Table : Gtk_Table;
      Statements_Check : Gtk_Check_Button;
      Strings_Check : Gtk_Check_Button;
      Comments_Check : Gtk_Check_Button;
      Case_Check : Gtk_Check_Button;
      Whole_Word_Check : Gtk_Check_Button;
      Regexp_Check : Gtk_Check_Button;
      Search_For_Label : Gtk_Label;
      Pattern_Combo : Gtk_Combo;
      Pattern_Entry : Gtk_Entry;
      Options_Label : Gtk_Label;
      Scan_In_Label : Gtk_Label;
      Files_Frame : Gtk_Frame;
      Files_Table : Gtk_Table;
      Browse_Button : Gtk_Button;
      Files_Label : Gtk_Label;
      Directory_Label : Gtk_Label;
      Directory_Combo : Gtk_Combo;
      Directory_Entry : Gtk_Entry;
      Only_Project_Check : Gtk_Check_Button;
      Subdirs_Check : Gtk_Check_Button;
      Files_Combo : Gtk_Combo;
      Files_Entry : Gtk_Entry;
      Hbuttonbox : Gtk_Hbutton_Box;
      Start_Button : Gtk_Button;
      Stop_Button : Gtk_Button;
      Close_Button : Gtk_Button;
   end record;
   type Hyper_Grep_Window_Access is access all Hyper_Grep_Window_Record'Class;

   procedure Gtk_New (Hyper_Grep_Window : out Hyper_Grep_Window_Access);
   procedure Initialize (Hyper_Grep_Window : access Hyper_Grep_Window_Record'Class);

   Hyper_Grep_Window : Hyper_Grep_Window_Access;

private

   procedure Files_Activation
     (Hyper_Grep_Window : in out Hyper_Grep_Window_Access;
      Active            : Boolean := True);
   --  (Des)activate widgets of Files_Frame (except Only_Project_Check)
   --  accordingly with Active.

   procedure Close_Window
     (Hyper_Grep_Window : in out Hyper_Grep_Window_Access);
   --  Close the window.

end Hyper_Grep_Window_Pkg;
