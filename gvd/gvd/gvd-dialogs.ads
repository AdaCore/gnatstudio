with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Label; use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Language; use Language;

package Task_Dialog_Pkg is

   type Task_Dialog_Record is new Gtk_Dialog_Record with record
      Main_Window : Main_Debug_Window_Access;
      Vbox1 : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Task_List : Gtk_Clist;
      Label1 : Gtk_Label;
      Label2 : Gtk_Label;
      Label3 : Gtk_Label;
      Label4 : Gtk_Label;
      Label5 : Gtk_Label;
      Label6 : Gtk_Label;
      Hbox1 : Gtk_Hbox;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Close_Button : Gtk_Button;
   end record;
   type Task_Dialog_Access is access all Task_Dialog_Record'Class;

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Main_Debug_Window_Access;
      Information : Thread_Information_Array);

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Main_Debug_Window_Access;
      Information : Thread_Information_Array);

end Task_Dialog_Pkg;
