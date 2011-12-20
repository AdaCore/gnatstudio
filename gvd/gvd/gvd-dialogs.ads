------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GPS.Kernel;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Handlers;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Window; use Gtk.Window;
with GVD.Process;
with Debugger; use Debugger;
with GNAT.Strings;

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

   procedure Attach_To_PD_Dialog
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach to a protection domains dialog

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the functions to load and save the desktop

   type Question_Dialog_Record is new Gtk_Dialog_Record with private;
   type Question_Dialog_Access is access all Question_Dialog_Record'Class;

   type Dialog_Kind is (Yes_No_Dialog, Multiple_Choice_Dialog);
   --  Kind of question dialog that can be created

   type Question_Record is record
      Choice : GNAT.Strings.String_Access;
      --  String that the user should enter to select that choice

      Description : GNAT.Strings.String_Access;
      --  Associated description
   end record;
   type Question_Array is array (Positive range <>) of Question_Record;

   procedure Free (Questions : in out Question_Array);
   --  Free all the dynamic memory associated with each question record.

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
     (Dialog                     : access Question_Dialog_Record'Class;
      Main_Window                : Gtk_Window;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "");

   function Get_Dialog_Kind
     (Question_Dialog : access Question_Dialog_Record'Class)
      return Dialog_Kind;
   --  Return the kind of dialog associated with Question_Dialog

private
   type Question_Dialog_Record is new Gtk_Dialog_Record with record
      Main_Window     : Gtk_Window;
      Vbox1           : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Tree_Model      : Gtk_Tree_Store;
      Tree_View       : Gtk_Tree_View;
      Hbox1           : Gtk_Hbox;
      Hbuttonbox1     : Gtk_Hbutton_Box;
      Close_Button    : Gtk_Button;
      Select_Row_Id   : Gtk.Handlers.Handler_Id;
      Debugger        : Debugger_Access;
      Kind            : Dialog_Kind;
   end record;
   --  We have to store the debugger for this dialog, since the user's choice
   --  should be sent to the right debugger, even if the user has switched
   --  tabs in between.

end GVD.Dialogs;
