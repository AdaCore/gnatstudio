with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;

package Log_Editor_Window_Pkg is

   type Log_Editor_Window_Record is new Gtk_Window_Record with record
      Files_Label   : Gtk_Label;
      Labels_Vbox   : Gtk_Hbox;
      Log_Text      : Gtk_Text;
      Ok_Button     : Gtk_Button;
      --  Cancel_Button : Gtk_Button;
   end record;
   type Log_Editor_Window_Access is access all Log_Editor_Window_Record'Class;

   procedure Gtk_New (Log_Editor_Window : out Log_Editor_Window_Access);
   procedure Initialize
     (Log_Editor_Window : access Log_Editor_Window_Record'Class);

   procedure Set_Text
     (Log_Editor_Window : access Log_Editor_Window_Record'Class;
      Text              : String);

   function Get_Text
     (Log_Editor_Window : access Log_Editor_Window_Record'Class)
     return String;

   procedure Add_File_Name
     (Log_Editor_Window : access Log_Editor_Window_Record'Class;
      File_Name         : String);

end Log_Editor_Window_Pkg;
