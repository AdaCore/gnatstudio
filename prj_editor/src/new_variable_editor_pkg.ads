with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Button; use Gtk.Button;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
package New_Variable_Editor_Pkg is

   type New_Variable_Editor_Record is new Gtk_Window_Record with record
      Vbox37 : Gtk_Vbox;
      Frame36 : Gtk_Frame;
      Variable_Name : Gtk_Entry;
      Frame33 : Gtk_Frame;
      Vbox38 : Gtk_Vbox;
      Get_Environment : Gtk_Check_Button;
      Alignment3 : Gtk_Alignment;
      List_Env_Variables : Gtk_Combo;
      Combo_Entry7 : Gtk_Entry;
      Frame34 : Gtk_Frame;
      Vbox39 : Gtk_Vbox;
      Typed_Variable : Gtk_Radio_Button;
      Untyped_Variable : Gtk_Radio_Button;
      Value_Frame : Gtk_Frame;
      Vbox40 : Gtk_Vbox;
      Scrolledwindow4 : Gtk_Scrolled_Window;
      List_Values : Gtk_Text;
      Alignment2 : Gtk_Alignment;
      Concatenate_Button : Gtk_Button;
      Hseparator4 : Gtk_Hseparator;
      Hbuttonbox3 : Gtk_Hbutton_Box;
      Add_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
   end record;
   type New_Variable_Editor_Access is access all New_Variable_Editor_Record'Class;

   procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access);
   procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class);

   New_Variable_Editor : New_Variable_Editor_Access;

end New_Variable_Editor_Pkg;
