with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Window; use Gtk.Window;
with Language; use Language;
with Debugger; use Debugger;

package Odd.Dialogs is

   type Odd_Dialog_Record is new Gtk_Dialog_Record with private;

   type Task_Dialog_Record is new Odd_Dialog_Record with private;
   type Task_Dialog_Access is access all Task_Dialog_Record'Class;

   type Backtrace_Dialog_Record is new Odd_Dialog_Record with private;
   type Backtrace_Dialog_Access is access all Backtrace_Dialog_Record'Class;

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Gtk_Window;
      Information : Thread_Information_Array);

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Gtk_Window;
      Information : Thread_Information_Array);

   procedure Update
     (Task_Dialog : access Task_Dialog_Record;
      Information : Thread_Information_Array);

   procedure Gtk_New
     (Backtrace_Dialog : out Backtrace_Dialog_Access;
      Main_Window      : Gtk_Window;
      Backtrace        : Backtrace_Array);

   procedure Initialize
     (Backtrace_Dialog : access Backtrace_Dialog_Record'Class;
      Main_Window      : Gtk_Window;
      Backtrace        : Backtrace_Array);

   procedure Update
     (Backtrace_Dialog : access Backtrace_Dialog_Record;
      Backtrace        : Backtrace_Array);

private
   type Odd_Dialog_Record is new Gtk_Dialog_Record with record
      Main_Window     : Gtk_Window;
      Vbox1           : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      List            : Gtk_Clist;
      Hbox1           : Gtk_Hbox;
      Hbuttonbox1     : Gtk_Hbutton_Box;
      Close_Button    : Gtk_Button;
   end record;

   type Task_Dialog_Record is new Odd_Dialog_Record with null record;
   type Backtrace_Dialog_Record is new Odd_Dialog_Record with null record;

end Odd.Dialogs;
