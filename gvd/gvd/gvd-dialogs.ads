-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.List; use Gtk.List;
with Gtk.Window; use Gtk.Window;
with Gtk.Enums;
with Debugger; use Debugger;
with Odd.Types;
with Gtk.Widget;

package Odd.Dialogs is

   type Odd_Dialog_Record is new Gtk_Dialog_Record with private;
   type Odd_Dialog is access all Odd_Dialog_Record'Class;

   type Task_Dialog_Record is new Odd_Dialog_Record with private;
   type Task_Dialog_Access is access all Task_Dialog_Record'Class;

   type Backtrace_Dialog_Record is new Odd_Dialog_Record with private;
   type Backtrace_Dialog_Access is access all Backtrace_Dialog_Record'Class;

   type Question_Dialog_Record is new Odd_Dialog_Record with private;
   type Question_Dialog_Access is access all Question_Dialog_Record'Class;

   type History_Dialog_Record is new Gtk_Dialog_Record with private;
   type History_Dialog_Access is access all History_Dialog_Record'Class;

   type Question_Record is record
      Choice : Odd.Types.String_Access;
      --  String that the user should enter to select that choice

      Description : Odd.Types.String_Access;
      --  Associated description
   end record;
   type Question_Array is array (Positive range <>) of Question_Record;

   procedure Free (Questions : in out Question_Array);
   --  Free all the dynamic memory associated with each question record.

   procedure Gtk_New
     (Task_Dialog : out Task_Dialog_Access;
      Main_Window : Gtk_Window);
   --  Create an empty task dialog.
   --  No information will be displayed in it, and you need to add it through
   --  a call to Update.

   procedure Initialize
     (Task_Dialog : access Task_Dialog_Record'Class;
      Main_Window : Gtk_Window);
   --  Internal initialization function

   procedure Update
     (Task_Dialog : access Task_Dialog_Record;
      Debugger    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Update the contents of the task dialog.
   --  The information is read from Debugger (which is in fact a
   --  Debugger_Process_Tab).

   procedure Gtk_New
     (Backtrace_Dialog : out Backtrace_Dialog_Access;
      Main_Window      : Gtk_Window);
   --  Create an empty backtrace dialog.
   --  No information will be displayed until you call Update.

   procedure Initialize
     (Backtrace_Dialog : access Backtrace_Dialog_Record'Class;
      Main_Window      : Gtk_Window);
   --  Internal initialization function

   procedure Update
     (Backtrace_Dialog : access Backtrace_Dialog_Record;
      Debugger         : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Update the contents of the backtrace dialog.
   --  The information is read from Debugger (which is in fact a
   --  Debugger_Process_Tab).

   procedure Gtk_New
     (Question_Dialog            : out Question_Dialog_Access;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array);

   procedure Initialize
     (Question_Dialog            : access Question_Dialog_Record'Class;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array);

   procedure Gtk_New
     (History_Dialog : out History_Dialog_Access;
      Main_Window    : Gtk_Window);
   --  Create a new history dialog.
   --  Main_Window should be the main debug window. The information will be
   --  blank, so you should call Update to add data.

   procedure Initialize
     (History_Dialog : access History_Dialog_Record'Class);
   --  Internal inititialization function.

   procedure Update
     (History_Dialog : History_Dialog_Access;
      Debugger       : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Reads the commands history from the main debug window, and fills the
   --  list with the User and Visible commands that were sent to Debugger.

   function Simple_Entry_Dialog
     (Parent    : access Gtk.Window.Gtk_Window_Record'Class;
      Title     : String;
      Message   : String;
      Position  : Gtk.Enums.Gtk_Window_Position := Gtk.Enums.Win_Pos_Center;
      Key       : String := "") return String;
   --  Open a simple dialog, with a single entry field, and returns the
   --  contents of this field (or ASCII.NUL) if the user selected cancel).
   --  The dialog is set up as a child of Parent, so that, depending on the
   --  window manager, it isn't displayed below it.
   --  if Key is not the empty string, then the dialog is stored in Parent's
   --  user data, and reused next time. This can be used to provide an history
   --  of values entered in this dialog by the user.
   --  Position indicates where the dialog should be positionned.
   --  If Extra_Box is not null, it is inserted below the combo box. You
   --  can check its contents on exit (unless Key is "", in which case the
   --  box has been destroyed).

   function Display_Entry_Dialog
     (Parent       : access Gtk.Window.Gtk_Window_Record'Class;
      Title        : String;
      Message      : String;
      Position     : Gtk.Enums.Gtk_Window_Position := Gtk.Enums.Win_Pos_Center;
      Check_Msg    : String;
      Key          : String := "";
      Button_Active : access Boolean) return String;
   --  A dialog, like Simple_Entry_Dialog, specifically set up to enter
   --  expressions to display. Is_Func indicates whether the expression is
   --  a variable name or a generic expression (to be displayed with
   --  "graph print `...`")

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

   type History_Dialog_Record is new Gtk_Dialog_Record with record
      Vbox1 : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      List : Gtk_List;
      Hbuttonbox1 : Gtk_Hbutton_Box;
      Replay_Selection : Gtk_Button;
      Cancel : Gtk_Button;
      Help : Gtk_Button;

      Window : Gtk_Window;
      --  This is in fact the main debug window.

   end record;

end Odd.Dialogs;
