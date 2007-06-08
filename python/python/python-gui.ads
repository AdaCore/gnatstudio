-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2006                      --
--                              AdaCore                              --
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

with Gtk.Handlers;
with GNAT.Strings;
with Ada.Calendar;
with Interactive_Consoles;

package Python.GUI is

   type Python_Interpreter_Record is tagged private;
   type Python_Interpreter is access all Python_Interpreter_Record'Class;

   procedure Initialize
     (Interpreter : access Python_Interpreter_Record'Class);
   --  Initialize the interpreter. Only one such object can be created in the
   --  application, since there is only one shared python interpreter.  Raises
   --  Interpreter_Error if the interpreter couldn't be initialized

   procedure Set_Default_Console
     (Interpreter    : access Python_Interpreter_Record'Class;
      Console        : Interactive_Consoles.Interactive_Console;
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
      return Interactive_Consoles.Interactive_Console;
   --  Return the current console of the interpreter, or null if the
   --  interpreter is not associated with a console

   function Run_Command
     (Interpreter  : access Python_Interpreter_Record'Class;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Show_Command : Boolean := False;
      Hide_Output  : Boolean := False;
      Errors       : access Boolean) return Python.PyObject;
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
      Show_Command : Boolean := False;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return String;
   --  Same as above, but also return the output of the command

   procedure Destroy (Interpreter : access Python_Interpreter_Record);
   --  Free the memory occupied by the interpreter

   function In_Process
     (Interpreter : access Python_Interpreter_Record) return Boolean;
   --  Whether the interpreter is processing a command

   Interpreter_Error : exception;

private

   type Python_Interpreter_Record is tagged record
      Globals : Python.PyObject;
      --  The global symbols for the python interpreter

      Use_Secondary_Prompt : Boolean := False;
      --  Which type of prompt should be displayed

      Buffer : GNAT.Strings.String_Access;
      --  Buffer for the command, to be added in front of any command before
      --  executing.

      Destroy_Id   : Gtk.Handlers.Handler_Id;
      Console : Interactive_Consoles.Interactive_Console;
      --  The default python console

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
