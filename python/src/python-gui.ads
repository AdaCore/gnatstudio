-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package provides a set of subprograms to facilitate the embedding of a
--  python interpreter into a GtkAda application

with Gtk.Text_View;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtk.Handlers;
with Gtk.Widget;
with GNAT.OS_Lib;
with Histories;
with Ada.Calendar;
with Interactive_Consoles;

package Python.GUI is

   type Python_Interpreter_Record is tagged private;
   type Python_Interpreter is access all Python_Interpreter_Record'Class;

   procedure Initialize
     (Interpreter : access Python_Interpreter_Record'Class;
      History     : Histories.History);
   --  Initialize the interpreter. Only one such object can be created in the
   --  application, since there is only one shared python interpreter.  Raises
   --  Interpreter_Error if the interpreter couldn't be initialized

   procedure Set_Console
     (Interpreter : access Python_Interpreter_Record'Class;
      Console     : Gtk.Text_View.Gtk_Text_View;
      Grab_Widget : Gtk.Widget.Gtk_Widget := null;
      Display_Prompt : Boolean := False);
   --  Bind the interpreter to Console, so that all input comes from Console
   --  and all output goes to it.
   --  If Console is null, no console is visible, although the interpreter can
   --  still be used.
   --  Grab_Widget is the widget which grabs the keyboard while commands are
   --  executing in the interpreter. This is necessary to prevent the user from
   --  using a menu that would in turn launch a command.
   --  If Display_Prompt is True, a prompt is displayed in the new console

   function Get_Console
     (Interpreter : access Python_Interpreter_Record'Class)
      return Gtk.Text_View.Gtk_Text_View;
   --  Return the current console of the interpreter, or null if the
   --  interpreter is not associated with a console

   function Run_Command
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Python.PyObject;
   --  Execute a command in the interpreter, and send its output to the
   --  console. Return its return value (which doesn't need to be Py_DECREF,
   --  since it is a borrowed reference).
   --  If Hide_Output is True, then nothing is printed on the console. If the
   --  command is incomplete and would require extra input (a secondary prompt
   --  in interactive mode), then it is not executed.
   --  Errors is set to True if there was an error executing the command or
   --  if the input was incomplete.

   function Run_Command
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return String;
   --  Same as above, but also return the output of the command

   procedure Insert_Text
     (Interpreter : access Python_Interpreter_Record'Class;
      Text        : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Highlight   : Boolean := False);
   --  Insert some text in the interpreter console, and scroll as necessary.
   --  The text is inserted into Console if not null, or in the default
   --  Python console otherwise.

   procedure Destroy (Interpreter : access Python_Interpreter_Record);
   --  Free the memory occupied by the interpreter

   Interpreter_Error : exception;

private

   type Python_Interpreter_Record is tagged record
      Globals : Python.PyObject;
      --  The global symbols for the python interpreter

      Use_Secondary_Prompt : Boolean := False;
      --  Which type of prompt should be displayed

      Buffer : GNAT.OS_Lib.String_Access;
      --  Buffer for the command, to be added in front of any command before
      --  executing.

      Current_Output : GNAT.OS_Lib.String_Access;
      Save_Output    : Boolean := False;
      --  The output of the current command, and whether we should save it,
      --  or discard it as soon as it has been printed in the console.

      History : Histories.History;
      History_Position : Integer := -1;

      Key_Press_Id : Gtk.Handlers.Handler_Id;
      Destroy_Id   : Gtk.Handlers.Handler_Id;

      Uneditable : Gtk.Text_Tag.Gtk_Text_Tag;
      Prompt_End_Mark : Gtk.Text_Mark.Gtk_Text_Mark;

      Console : Gtk.Text_View.Gtk_Text_View;
      --  The default python console

      Grab_Widget : Gtk.Widget.Gtk_Widget;
      --  The widget which should grab the keyboard focus while commands are
      --  executing in the python interpreter.

      Scroll_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  Mark to which the view should be scrolled

      Waiting_For_Input : Boolean := False;
      --  Set to true if the python interpreter is waiting for user input while
      --  executing a command. While this is true, the rest of GPS is frozen.

      Refresh_Timeout : Ada.Calendar.Time := Ada.Calendar.Clock;
      --  Time since we last checked the list of gtk+ events. This avoids
      --  checking too often, which slow things down too much

      In_Process : Boolean := False;
      --  True while we are processing a command. This is used to control the
      --  behavior of control-c: either interrupt, or copy

      Hide_Output : Boolean := False;
      --  True if the output of the interpreter shouldn't be displayed in the
      --  console. This is used to execute commands internally.
   end record;

end Python.GUI;
