with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Window; use Gtk.Window;
with Language; use Language;
with Debugger; use Debugger;
with Generic_Values; use Generic_Values;

package Odd.Dialogs is

   type Odd_Dialog_Record is new Gtk_Dialog_Record with private;

   type Task_Dialog_Record is new Odd_Dialog_Record with private;
   type Task_Dialog_Access is access all Task_Dialog_Record'Class;

   type Backtrace_Dialog_Record is new Odd_Dialog_Record with private;
   type Backtrace_Dialog_Access is access all Backtrace_Dialog_Record'Class;

   type Question_Dialog_Record is new Odd_Dialog_Record with private;
   type Question_Dialog_Access is access all Question_Dialog_Record'Class;

   type Question_Record is record
      Choice : Generic_Values.String_Access;
      --  String that the user should enter to select that choice

      Description : Generic_Values.String_Access;
      --  Associated description
   end record;
   type Question_Array is array (Positive range <>) of Question_Record;

   procedure Free (Questions : in out Question_Array);
   --  Free all the dynamic memory associated with each question record.

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

   procedure Gtk_New
     (Question_Dialog : out Question_Dialog_Access;
      Main_Window     : Gtk_Window;
      Debugger        : Debugger_Access;
      Questions       : Question_Array);

   procedure Initialize
     (Question_Dialog : access Question_Dialog_Record'Class;
      Main_Window     : Gtk_Window;
      Debugger        : Debugger_Access;
      Questions       : Question_Array);

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
   --  ??? Why not store directly the Debugger_Process_Tab in this record,
   --  instead of having to convert in the callbacks ?

   type Task_Dialog_Record is new Odd_Dialog_Record with null record;
   type Backtrace_Dialog_Record is new Odd_Dialog_Record with null record;

   type Question_Dialog_Record is new Odd_Dialog_Record with record
      Debugger        : Debugger_Access;
   end record;
   --  We have to store the debugger for this dialog, since the user's choice
   --  should be sent to the right debugger, even if the user has switched
   --  tabs in between.

end Odd.Dialogs;
