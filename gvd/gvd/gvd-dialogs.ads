-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glib.Object;
with GPS.Kernel;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Handlers;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Window; use Gtk.Window;
with GVD.Process;
with Debugger; use Debugger;
with Basic_Types;

package GVD.Dialogs is

   procedure Attach_To_Thread_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach to a thread dialog (if Create_If_Necessary is True, only attach
   --  if one exists and is not attached to a debugger)

   procedure Attach_To_Tasks_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach to a task dialog

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the functions to load and save the desktop

   type GVD_Dialog_Record is new Gtk_Dialog_Record with private;
   type GVD_Dialog is access all GVD_Dialog_Record'Class;

   type PD_Dialog_Record is new GVD_Dialog_Record with private;
   type PD_Dialog_Access is access all PD_Dialog_Record'Class;

   type Question_Dialog_Record is new GVD_Dialog_Record with private;
   type Question_Dialog_Access is access all Question_Dialog_Record'Class;

   type Question_Record is record
      Choice : Basic_Types.String_Access;
      --  String that the user should enter to select that choice

      Description : Basic_Types.String_Access;
      --  Associated description
   end record;
   type Question_Array is array (Positive range <>) of Question_Record;

   procedure Free (Questions : in out Question_Array);
   --  Free all the dynamic memory associated with each question record.

   procedure Gtk_New
     (PD_Dialog  : out PD_Dialog_Access;
      Main_Window : Gtk_Window);
   --  Create an empty protection domains dialog.
   --  No information will be displayed in it, and you need to add it through
   --  a call to Update.

   procedure Initialize
     (PD_Dialog  : access PD_Dialog_Record'Class;
      Main_Window : Gtk_Window);
   --  Internal initialization function

   procedure Update
     (PD_Dialog  : access PD_Dialog_Record;
      Debugger   : access Glib.Object.GObject_Record'Class);
   --  Update the contents of the protection domains dialog.
   --  The information is read from Debugger (which is in fact a
   --  Visual_Debugger).

   procedure On_PD_Process_Stopped
     (Widget : access Glib.Object.GObject_Record'Class);
   --  Callback function connected to the "process_stopped" signal.
   --  It will update the protection domains window associated with a given
   --  tab.

   procedure Gtk_New
     (Question_Dialog            : out Question_Dialog_Access;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "");
   --  Create a question dialog with a list of questions.
   --  If Questions consists of two choices "y" and "n" then display
   --  only a basic Yes/No dialog.

   procedure Initialize
     (Question_Dialog            : access Question_Dialog_Record'Class;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "");

private
   type GVD_Dialog_Record is new Gtk_Dialog_Record with record
      Main_Window     : Gtk_Window;
      Vbox1           : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      List            : Gtk_Clist;
      Hbox1           : Gtk_Hbox;
      Hbuttonbox1     : Gtk_Hbutton_Box;
      Close_Button    : Gtk_Button;
      Select_Row_Id   : Gtk.Handlers.Handler_Id;
   end record;
   --  ??? Why not store directly the Visual_Debugger in this record,
   --  instead of having to convert in the callbacks ?

   type PD_Dialog_Record is new GVD_Dialog_Record with null record;

   type Question_Dialog_Record is new GVD_Dialog_Record with record
      Debugger : Debugger_Access;
   end record;
   --  We have to store the debugger for this dialog, since the user's choice
   --  should be sent to the right debugger, even if the user has switched
   --  tabs in between.

   procedure Update_PD
     (Dialog   : access GVD_Dialog_Record'Class;
      Info     : in out PD_Information_Array);

end GVD.Dialogs;
