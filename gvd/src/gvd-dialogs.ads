------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GNAT.Strings;

with Gtk.Box;             use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View;       use Gtk.Tree_View;
with Gtk.Tree_Store;      use Gtk.Tree_Store;
with Gtk.Widget;          use Gtk.Widget;

with GPS.Dialogs;         use GPS.Dialogs;
with GPS.Kernel;          use GPS.Kernel;
with GPS.Kernel.MDI;      use GPS.Kernel.MDI;
with Debugger;            use Debugger;

package GVD.Dialogs is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the functions to load and save the desktop

   type Question_Dialog_Record is new GPS_Dialog_Record with private;
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
      Kernel                     : not null access Kernel_Handle_Record'Class;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "");
   procedure Initialize
     (Dialog                     : access Question_Dialog_Record'Class;
      Kernel                     : not null access Kernel_Handle_Record'Class;
      Debugger                   : Debugger_Access;
      Multiple_Selection_Allowed : Boolean;
      Questions                  : Question_Array;
      Question_Description       : String := "");
   --  Create a question dialog with a list of questions.
   --  If Questions consists of two choices "y" and "n" then display
   --  only a basic Yes/No dialog.

   Gdb_Answer_Suffix : constant String := "\" & ASCII.LF & " ";
   --  This is needed when sending an answer to Gdb's questions, one-line
   --  answers do not seem supported under Windows.

private
   type Question_Dialog_Record is new GPS_Dialog_Record with record
      Kind            : Dialog_Kind;
      Debugger        : Debugger_Access;
      Content_Area    : Gtk_Vbox;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Tree_Model      : Gtk_Tree_Store;
      Tree_View       : Gtk_Tree_View;
   end record;
   --  We have to store the debugger for this dialog, since the user's choice
   --  should be sent to the right debugger, even if the user has switched
   --  tabs in between.

end GVD.Dialogs;
