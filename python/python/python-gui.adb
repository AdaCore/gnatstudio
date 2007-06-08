-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
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

with Ada.Calendar;      use Ada.Calendar;
with Gtk.Main;          use Gtk.Main;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Object;        use Gtk.Object;
with Gtk.Widget;        use Gtk.Widget;
with Gdk.Window;        use Gdk.Window;
with Interactive_Consoles; use Interactive_Consoles;

with Python_Module;

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with System;
with Python.Ada;      use Python.Ada;
with Ada.Unchecked_Conversion;
with Traces;          use Traces;

package body Python.GUI is

   Me : constant Debug_Handle := Create ("Python.GUI");
   Me_Out : constant Debug_Handle := Create ("Python.Out", Default => Off);

   Timeout_Threshold : constant Duration := 0.2;   --  in seconds
   --  Timeout between two checks of the gtk+ event queue

   function Convert is new Standard.Ada.Unchecked_Conversion
     (System.Address, Python_Interpreter);

   package Interpreter_Callback2 is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Python_Interpreter);

   procedure Display_Prompt
     (Interpreter : access Python_Interpreter_Record'Class);
   --  Display the prompt in the shell window

   function Trace
     (User_Arg : PyObject;
      Frame    : System.Address;
      Why      : Why_Trace_Func;
      Obj      : PyObject) return Integer;
   pragma Convention (C, Trace);
   --  Suprogram called for each python instruction execution. It periodically
   --  checks the event queue, so that the interpreter can be interrupted.

   function Signal (Num : Integer; Handler : System.Address)
      return System.Address;
   pragma Import (C, Signal, "signal");

   procedure Process_Gtk_Events
     (Interpreter : access Python_Interpreter_Record'Class;
      Force       : Boolean := False);
   --  Process all pending gtk+ events

   procedure Console_Destroyed
     (Console : access Gtk_Widget_Record'Class;
      Interpreter : Python_Interpreter);
   --  Called when the console of the interpreter is destroyed.

   procedure Insert_Text
     (Interpreter : access Python_Interpreter_Record'Class;
      Text        : String;
      Console     : Interactive_Consoles.Interactive_Console := null);
   --  Insert some text in the interpreter console, and scroll as necessary.
   --  The text is inserted into Console if not null, or in the default
   --  Python console otherwise.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Interpreter : access Python_Interpreter_Record) is
   begin
      Free (Interpreter.Buffer);
   end Destroy;

   ------------------------
   -- Process_Gtk_Events --
   ------------------------

   procedure Process_Gtk_Events
     (Interpreter : access Python_Interpreter_Record'Class;
      Force       : Boolean := False)
   is
      Dead : Boolean;
      pragma Unreferenced (Dead);
   begin
      --  Process all gtk+ events, so that the text becomes visible
      --  immediately, even if the python program hasn't finished executing

      --  Note: since we have grabed the mouse and keyboards, events will only
      --  be sent to the python console, thus avoiding recursive loops inside
      --  GPS.

      --  ??? Fails if we are displaying a dialog
      if not Interpreter.Hide_Output then
         if Force
           or else Clock - Interpreter.Refresh_Timeout > Timeout_Threshold
         then
            while Gtk.Main.Events_Pending loop
               Dead := Gtk.Main.Main_Iteration;
            end loop;
            Interpreter.Refresh_Timeout := Clock;
         end if;
      end if;
   end Process_Gtk_Events;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Interpreter : access Python_Interpreter_Record'Class;
      Text        : String;
      Console     : Interactive_Consoles.Interactive_Console := null) is
   begin
      Trace (Me_Out, Text);

      if not Interpreter.Hide_Output then
         if Console /= null then
            Insert (Console, Text, Add_LF => False);

         elsif Interpreter.Console /= null
           and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
         then
            Insert (Interpreter.Console, Text, Add_LF => False);

         else
            Trace (Me, Text);
         end if;
      end if;
   end Insert_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Interpreter : access Python_Interpreter_Record'Class)
   is
      Setup_Cmd      : constant String := "import sys" & ASCII.LF;
      Main_Module    : PyObject;
      Sigint         : constant Integer := 2;
      Old_Handler    : System.Address;
      pragma Warnings (Off, Old_Handler);
      Prompt         : PyObject;
      Ignored        : Integer;
      pragma Unreferenced (Ignored);

   begin
      --  We need to set the program name, or some import commands will raise
      --  errors
      Py_SetProgramName ("Python_Interpreter");

      --  Prevent python's standard Ctrl-C handling, to leave it to the calling
      --  application.
      Old_Handler := Signal (Sigint, System.Null_Address);
      Py_Initialize;
      Old_Handler := Signal (Sigint, Old_Handler);

      if not PyRun_SimpleString (Setup_Cmd) then
         raise Interpreter_Error;
      end if;

      Main_Module := PyImport_AddModule ("__main__");
      if Main_Module = null then
         raise Interpreter_Error;
      end if;
      Interpreter.Globals := PyModule_GetDict (Main_Module);

      --  Initialize various variables

      Prompt := PySys_GetObject ("ps1");
      if Prompt = null then
         Prompt := PyString_FromString (">>> ");
         PySys_SetObject ("ps1", Prompt);
         Py_DECREF (Prompt);
      end if;

      Prompt := PySys_GetObject ("ps2");
      if Prompt = null then
         Prompt := PyString_FromString ("... ");
         PySys_SetObject ("ps2", Prompt);
         Py_DECREF (Prompt);
      end if;

      Interpreter.Buffer := new String'("");
   end Initialize;

   -------------------------
   -- Set_Default_Console --
   -------------------------

   procedure Set_Default_Console
     (Interpreter    : access Python_Interpreter_Record'Class;
      Console        : Interactive_Consoles.Interactive_Console;
      Display_Prompt : Boolean := False)
   is
      Ignored : Boolean;
      pragma Unreferenced (Ignored);
   begin
      if Console = Interpreter.Console then
         return;
      end if;

      --  If we still have a previous console, disconnect it
      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         Gtk.Handlers.Disconnect (Interpreter.Console, Interpreter.Destroy_Id);
      end if;

      Interpreter.Console := Console;

      Python_Module.Override_Default_IO (Interpreter.Console);

      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         Interpreter.Destroy_Id := Interpreter_Callback2.Connect
           (Console, Signal_Destroy,
            Interpreter_Callback2.To_Marshaller (Console_Destroyed'Access),
            Python_Interpreter (Interpreter));

         if Display_Prompt then
            Python.GUI.Display_Prompt (Interpreter);
         end if;
      end if;
   end Set_Default_Console;

   -----------------------
   -- Console_Destroyed --
   -----------------------

   procedure Console_Destroyed
     (Console     : access Gtk_Widget_Record'Class;
      Interpreter : Python_Interpreter)
   is
      pragma Unreferenced (Console);
   begin
      Set_Default_Console (Interpreter, null);
   end Console_Destroyed;

   -----------
   -- Trace --
   -----------

   function Trace
     (User_Arg : PyObject;
      Frame    : System.Address;
      Why      : Why_Trace_Func;
      Obj      : PyObject) return Integer
   is
      pragma Unreferenced (Obj, Frame, Why);
      Interpreter : constant Python_Interpreter := Convert
        (PyCObject_AsVoidPtr (User_Arg));
   begin
      Process_Gtk_Events (Interpreter);
      return 0;
   end Trace;

   -----------------
   -- Run_Command --
   -----------------

   function Run_Command
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String;
      Console     : Interactive_Console := null;
      Show_Command : Boolean := False;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return String
   is
      Result : PyObject;
      Str    : PyObject;
   begin
      Result :=
        Run_Command (Interpreter, Command, Console,
                     Show_Command, Hide_Output, Errors);

      if Result /= null and then not Errors.all then
         Str := PyObject_Str (Result);
         Py_DECREF (Result);
         declare
            S : constant String := PyString_AsString (Str);
         begin
            Py_DECREF (Str);
            return S;
         end;
      else
         Py_XDECREF (Result);
         return "";
      end if;
   end Run_Command;

   -----------------
   -- Run_Command --
   -----------------

   function Run_Command
     (Interpreter  : access Python_Interpreter_Record'Class;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Show_Command : Boolean := False;
      Hide_Output  : Boolean := False;
      Errors       : access Boolean) return PyObject
   is
      Result, Builtin : PyObject := null;
      Obj            : PyObject;
      Code           : PyCodeObject;
      Tmp            : String_Access;
      Indented_Input : constant Boolean := Command'Length > 0
        and then
          (Command (Command'First) = ASCII.HT
           or else Command (Command'First) = ' ');
      Cmd            : constant String :=
        Interpreter.Buffer.all & Command & ASCII.LF;
      Grab_Widget    : Gtk_Widget;
      use type Gdk_Window;

      Took_Grab : Boolean := False;
      Ignored : Boolean;
      pragma Unreferenced (Ignored);

      Default_Console : constant Interactive_Console := Interpreter.Console;
   begin
      --  Make sure that the output to sys.stdout is properly hidden. This is
      --  in particular required when doing completion, since the result of
      --  the command to get completions would be output in the middle of the
      --  command the user is typing.

      if Hide_Output then
         Trace (Me_Out, "Running command __gps_hide_output()");
         Ignored := PyRun_SimpleString ("__gps_hide_output()");
      end if;

      Trace (Me, "Running command: " & Cmd);

      Interpreter.Hide_Output := Hide_Output;

      if Show_Command then
         Insert_Text (Interpreter, Command & ASCII.LF, Console);
      end if;

      Errors.all := False;

      if Cmd = "" & ASCII.LF then
         if not Hide_Output and then Console = null then
            Display_Prompt (Interpreter);
         end if;
         return null;
      end if;

      if Console /= null then
         if Default_Console /= null then
            Ref (Default_Console);
         end if;

         Set_Default_Console (Interpreter, Console);
      end if;

      --  Reset previous output
      Builtin := PyImport_ImportModule ("__builtin__");
      PyObject_SetAttrString (Builtin, "_", Py_None);

      Interpreter.In_Process := True;

      Code := Py_CompileString (Cmd, "<stdin>", Py_Single_Input);

      --  If code compiled just fine
      if Code /= null and then not Indented_Input then
         --  Grab the mouse, keyboard,... so as to avoid recursive loops in
         --  GPS (user selecting a menu while python is running)
         if Interpreter.Console /= null then
            Grab_Widget := Gtk_Widget (Get_View (Interpreter.Console));
         else
            Grab_Widget := null;
         end if;

         if Grab_Widget /= null then
            Ref (Grab_Widget);

            if Get_Window (Grab_Widget) /= null then

               --  If we already have a grab (for instance when the user is
               --  displaying a menu and we are running the python command as a
               --  filter for that menu), no need to take another. In fact,
               --  taking another would break the above scenario, since in
               --  gtkmenu.c the handler for grab_notify cancels the menu when
               --  another grab is taken (G305-005)
               if Gtk.Main.Grab_Get_Current = null then
                  Gtk.Main.Grab_Add (Grab_Widget);
                  Took_Grab := True;
               end if;
            end if;
         end if;

         PyEval_SetTrace
           (Trace'Access, PyCObject_FromVoidPtr (Interpreter.all'Address));
         Obj := PyEval_EvalCode
           (Code, Interpreter.Globals, Interpreter.Globals);
         Py_DECREF (PyObject (Code));
         PyEval_SetTrace (null, null);

         --  Note: the widget might have been destroyed by the python command,
         --  we need to check that it still exists.

         if Grab_Widget /= null then
            if Took_Grab then
               Gtk.Main.Grab_Remove (Grab_Widget);
            end if;
            Unref (Grab_Widget);
         end if;

         if Obj = null then
            declare
               EType, Occurrence, Traceback : PyObject;
            begin
               if Hide_Output then
                  --  We need to preserve the current exception before
                  --  executing the next command
                  PyErr_Fetch (EType, Occurrence, Traceback);
                  Trace (Me_Out, "Running command __gps_restore_output()");
                  Ignored := PyRun_SimpleString ("__gps_restore_output()");
                  Interpreter.Hide_Output := False;
                  PyErr_Restore (EType, Occurrence, Traceback);
               end if;

               --  Always display exceptions in the python console, since it
               --  is likely they are unexpected and should be fixed.
               PyErr_Print;

               if Hide_Output then
                  Trace (Me_Out, "Running command __gps_hide_output()");
                  Ignored := PyRun_SimpleString ("__gps_hide_output()");
                  Interpreter.Hide_Output := True;
               end if;

               Trace (Me, "Got a null result, this is an error");
               Errors.all := True;
            end;
         else
            --  No other python command between this one and the previous
            --  call to PyEval_EvalCode
            if PyObject_HasAttrString (Builtin, "_") then
               Result := PyObject_GetAttrString (Builtin, "_");
            else
               Result := null;
            end if;
            Py_DECREF (Obj);
         end if;

         Interpreter.Use_Secondary_Prompt := False;
         Free (Interpreter.Buffer);
         Interpreter.Buffer := new String'("");

      --  Do we have compilation error because input was incomplete ?

      elsif not Hide_Output then
         Interpreter.Use_Secondary_Prompt := Indented_Input;

         if not Interpreter.Use_Secondary_Prompt then
            declare
               Typ, Occurrence, Traceback : PyObject;
               S : PyObject;
            begin
               if PyErr_Occurred /= null then
                  PyErr_Fetch (Typ, Occurrence, Traceback);
                  PyErr_NormalizeException (Typ, Occurrence, Traceback);

                  if PyTuple_Check (Occurrence) then
                     --  Old style exceptions
                     S := PyTuple_GetItem (Occurrence, 0);
                  else
                     --  New style: occurrence is an instance
                     --  S is null if the exception is not a syntax_error
                     S := PyObject_GetAttrString (Occurrence, "msg");
                  end if;

                  if S = null then
                     Interpreter.Use_Secondary_Prompt := False;
                  else
                     declare
                        Msg : constant String := PyString_AsString (S);
                     begin
                        --  Second message appears when typing:
                        --    >>> if 1:
                        --    ...   pass
                        --    ... else:
                        Interpreter.Use_Secondary_Prompt :=
                          Msg = "unexpected EOF while parsing"
                          or else Msg = "expected an indented block";
                     end;
                  end if;

                  if not Interpreter.Use_Secondary_Prompt then
                     PyErr_Restore (Typ, Occurrence, Traceback);
                     PyErr_Print;
                     Trace (Me, "Unexpected end of input");
                     Errors.all := True;
                  else
                     PyErr_Clear;
                  end if;
               end if;
            end;
         else
            PyErr_Clear;
         end if;

         if Interpreter.Use_Secondary_Prompt then
            Tmp := Interpreter.Buffer;
            Interpreter.Buffer := new String'
              (Interpreter.Buffer.all & Command & ASCII.LF);
            Free (Tmp);
         else
            Free (Interpreter.Buffer);
            Interpreter.Buffer := new String'("");
         end if;
      else
         PyErr_Clear;
      end if;

      if not Hide_Output and then Console = null then
         Display_Prompt (Interpreter);
      end if;

      Interpreter.In_Process := False;
      Interpreter.Hide_Output := False;

      if Console /= null then
         Set_Default_Console (Interpreter, Default_Console);

         if Default_Console /= null then
            Unref (Default_Console);
         end if;
      end if;

      if Hide_Output then
         Trace (Me_Out, "Running command __gps_restore_output()");
         Ignored := PyRun_SimpleString ("__gps_restore_output()");
      end if;

      return Result;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Interpreter.In_Process := False;
         Interpreter.Hide_Output := False;
         Errors.all := True;

         if Console /= null then
            Set_Default_Console (Interpreter, Default_Console);

            if Default_Console /= null then
               Unref (Default_Console);
            end if;
         end if;

         if Hide_Output then
            Ignored := PyRun_SimpleString ("__gps_restore_output()");
         end if;

         return Result;
   end Run_Command;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Interpreter : access Python_Interpreter_Record'Class)
   is
      Ps : PyObject;
   begin
      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
        and then Is_Editable (Interpreter.Console)
      then
         if Interpreter.Use_Secondary_Prompt then
            Ps := PySys_GetObject ("ps2");
         else
            Ps := PySys_GetObject ("ps1");
         end if;

         Set_Prompt (Interpreter.Console, PyString_AsString (Ps));
         Display_Prompt (Interpreter.Console);
         Set_Prompt (Interpreter.Console, "");
      end if;
   end Display_Prompt;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Interpreter : access Python_Interpreter_Record'Class)
      return Interactive_Consoles.Interactive_Console is
   begin
      return Interpreter.Console;
   end Get_Console;

   ----------------
   -- In_Process --
   ----------------

   function In_Process
     (Interpreter : access Python_Interpreter_Record) return Boolean is
   begin
      return Interpreter.In_Process;
   end In_Process;

end Python.GUI;
