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

with Ada.Calendar;      use Ada.Calendar;
with Ada.Exceptions;    use Ada.Exceptions;
with Glib.Object;       use Glib.Object;
with Gtk.Main;          use Gtk.Main;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Object;        use Gtk.Object;
with Gtk.Widget;        use Gtk.Widget;
with Gdk.Window;        use Gdk.Window;
with Glib;              use Glib;
with Interactive_Consoles; use Interactive_Consoles;

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with System;
with Python.Ada;      use Python.Ada;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Traces;          use Traces;
with Glide_Intl;      use Glide_Intl;

package body Python.GUI is

   Me : constant Debug_Handle := Create ("Python.GUI");
   Me_Out : constant Debug_Handle := Create ("Python.Out", Default => Off);

   Console_Class_Name : constant String := "Console";
   --  The name of the class that redirects the output of Python to one of
   --  GPS's windows

   Timeout_Threshold : constant Duration := 0.2;   --  in seconds
   --  Timeout between two checks of the gtk+ event queue

   Class_Data_Key          : constant String := "__gps_class_data__";
   Class_Instance_Data_Key : constant String := "__gps_class_inst_data__";
   --  The keys to extract the internal GPS data from a class or its instance

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

   function Write (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Write);
   --  The second is the same as Write, except the output is never saved and
   --  return to the code that executed a python command.

   function Flush (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Flush);
   function Read_Line (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Read_Line);
   function Is_A_TTY (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Is_A_TTY);
   function Output_Constructor
     (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Output_Constructor);
   --  Override the python's methods of the File class.

   procedure Process_Gtk_Events
     (Interpreter : access Python_Interpreter_Record'Class;
      Force       : Boolean := False);
   --  Process all pending gtk+ events

   procedure Console_Destroyed
     (Console : access Gtk_Widget_Record'Class;
      Interpreter : Python_Interpreter);
   --  Called when the console of the interpreter is destroyed.

   type Output_Class_Data is record
      Console      : Interactive_Console;
      Capture      : Boolean;
   end record;
   type Output_Class_Data_Access is access Output_Class_Data;
   --  Data stored in the Output class

   procedure Create_Output_Class
     (Interpreter    : access Python_Interpreter_Record'Class;
      Module_Name    : String;
      Main_Module    : PyObject);
   --  Create the GPS.Console class

   procedure Destroy_Output_Data (Data : System.Address);
   pragma Convention (C, Destroy_Output_Data);
   --  Destroy an Output_Class_Data_Access, as stored in instances of the
   --  Output class

   function Convert is new Standard.Ada.Unchecked_Conversion
     (System.Address, Output_Class_Data_Access);

   procedure Insert_Text
     (Interpreter : access Python_Interpreter_Record'Class;
      Text        : String;
      Console     : Interactive_Consoles.Interactive_Console := null);
   --  Insert some text in the interpreter console, and scroll as necessary.
   --  The text is inserted into Console if not null, or in the default
   --  Python console otherwise.

   procedure Console_Notify
     (Output_Data          : System.Address;
      Where_The_Object_Was : System.Address);
   pragma Convention (C, Console_Notify);

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

   -----------
   -- Write --
   -----------

   function Write (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self);
      S           : aliased chars_ptr;
      N           : aliased Integer;
      Stdout      : aliased PyObject;
      Interpreter : Python_Interpreter;
      Data        : PyObject;
      Old         : String_Access;
      Output_Data : Output_Class_Data_Access;

   begin
      if not PyArg_ParseTuple
        (Args, "Os#", Stdout'Address, S'Address, N'Address)
      then
         return null;
      end if;

      Data := PyObject_GetAttrString (Stdout, Class_Data_Key);
      Interpreter := Convert (PyCObject_AsVoidPtr (Data));
      Py_DECREF (Data);

      Data := PyObject_GetAttrString (Stdout, Class_Instance_Data_Key);
      Output_Data := Convert (PyCObject_AsVoidPtr (Data));
      Py_DECREF (Data);

      if Output_Data.Capture
        and then Interpreter.Save_Output
      then
         Old := Interpreter.Current_Output;
         Interpreter.Current_Output :=
           new String'(Old.all & Value (S, size_t (N)));
         Free (Old);
      end if;

      Insert_Text (Interpreter, Value (S, size_t (N)), Output_Data.Console);
      Process_Gtk_Events (Interpreter);

      Py_INCREF (Py_None);
      return Py_None;
   end Write;

   --------------------
   -- Console_Notify --
   --------------------

   procedure Console_Notify
     (Output_Data          : System.Address;
      Where_The_Object_Was : System.Address)
   is
      pragma Unreferenced (Where_The_Object_Was);
      D : constant Output_Class_Data_Access := Convert (Output_Data);
   begin
      D.Console := null;
   end Console_Notify;

   --------------
   -- Is_A_TTY --
   --------------

   function Is_A_TTY (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self, Args);
   begin
      return PyInt_FromLong (0);
   end Is_A_TTY;

   -----------
   -- Flush --
   -----------

   function Flush (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self, Args);
   begin
      Py_INCREF (Py_None);
      return Py_None;
   end Flush;

   -------------------------
   -- Destroy_Output_Data --
   -------------------------

   procedure Destroy_Output_Data (Data : System.Address) is
      procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
        (Output_Class_Data, Output_Class_Data_Access);
      D : Output_Class_Data_Access := Convert (Data);
   begin
      Weak_Unref (D.Console, Console_Notify'Access, Data);
      Unchecked_Free (D);
   end Destroy_Output_Data;

   ------------------------
   -- Output_Constructor --
   ------------------------

   function Output_Constructor
     (Self : PyObject; Args : PyObject) return PyObject
   is
      pragma Unreferenced (Self);
      Output_Data : Output_Class_Data_Access;
      User_Data   : PyObject;
      Instance    : aliased PyObject;
      Capture     : aliased Integer;
   begin
      if not PyArg_ParseTuple
        (Args, "Oi", Instance'Address, Capture'Address)
      then
         return null;
      end if;

      Output_Data := new Output_Class_Data'
        (Console      => null,
         Capture      => Capture /= 0);
      User_Data := PyCObject_FromVoidPtr
        (Output_Data.all'Address, Destroy_Output_Data'Access);
      PyObject_SetAttrString (Instance, Class_Instance_Data_Key, User_Data);
      Py_DECREF (User_Data);

      Py_INCREF (Py_None);
      return Py_None;
   end Output_Constructor;

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self);
      File          : aliased PyObject;
      Size          : aliased Integer := 0;
      Data          : PyObject;
      Interpreter   : Python_Interpreter;
      Console       : Interactive_Console;
      Output_Data   : Output_Class_Data_Access;
   begin
      if not PyArg_ParseTuple (Args, "O|i", File'Address, Size'Address) then
         return null;
      end if;

      Data := PyObject_GetAttrString (File, Class_Data_Key);
      Interpreter := Convert (PyCObject_AsVoidPtr (Data));
      Py_DECREF (Data);

      Data := PyObject_GetAttrString (File, Class_Instance_Data_Key);
      Output_Data := Convert (PyCObject_AsVoidPtr (Data));
      Py_DECREF (Data);

      if Output_Data.Console /= null then
         Console := Output_Data.Console;
      else
         Console := Interpreter.Console;
      end if;

      if Console = null
        or else Interpreter.Hide_Output
        or else Gtk.Object.Destroyed_Is_Set (Console)
      then
         --  Report EOF on stdin
         return PyString_FromString ("");
      end if;

      return PyString_FromString (Read (Console, Whole_Line => True));
   end Read_Line;

   -------------------------
   -- Create_Output_Class --
   -------------------------

   procedure Create_Output_Class
     (Interpreter : access Python_Interpreter_Record'Class;
      Module_Name : String;
      Main_Module : PyObject)
   is
      Meths   : constant PyObject := PyDict_New;
      Output  : PyClassObject;
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      PyDict_SetItemString
        (Meths, "__doc__", PyString_FromString
         (-("This class redirects its input and output to one of GPS's"
            & " consoles. The current console can be overriden by"
            & " calls to set_console()")));
      PyDict_SetItemString
        (Meths, "__module__", PyString_FromString (Module_Name));
      PyDict_SetItemString
        (Meths, Class_Data_Key,
         PyCObject_FromVoidPtr (Interpreter.all'Address));

      Output := PyClass_New
        (Bases => null,  --  could be "file"
         Dict  => Meths,
         Name  => PyString_FromString (Console_Class_Name));
      Ignored := PyModule_AddObject (Main_Module, Console_Class_Name, Output);

      Add_Method (Output, Create_Method_Def ("write",    Write'Access));
      Add_Method (Output, Create_Method_Def ("flush",    Flush'Access));
      Add_Method (Output, Create_Method_Def ("read",     Read_Line'Access));
      Add_Method (Output, Create_Method_Def ("readline", Read_Line'Access));
      Add_Method (Output, Create_Method_Def ("isatty",   Is_A_TTY'Access));
      Add_Method (Output, Create_Method_Def
        ("__init__", Output_Constructor'Access));
   end Create_Output_Class;

   -------------------
   -- Initialize_IO --
   -------------------

   procedure Initialize_IO
     (Interpreter : access Python_Interpreter_Record'Class;
      Module_Name : String;
      Module      : PyObject) is
   begin
      Create_Output_Class (Interpreter, Module_Name, Module);

      --  Note: we also set __stdout__,..., so that the user can restore them
      --  after temporarily modifying sys.stdout in their own programs
      if not PyRun_SimpleString
        ("sys.stdin="
         & Module_Name & '.' & Console_Class_Name & "(1)" & ASCII.LF
         & "sys.stdout="
         & Module_Name & '.' & Console_Class_Name & "(1)" & ASCII.LF
         & "sys.stderr="
         & Module_Name & '.' & Console_Class_Name & "(0)" & ASCII.LF
         & "sys.__stdout__=sys.stdout" & ASCII.LF
         & "sys.__stdin__=sys.stdin" & ASCII.LF
         & "sys.__stderr__=sys.stderr" & ASCII.LF)
      then
         raise Interpreter_Error;
      end if;
   end Initialize_IO;

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
      Prompt         : PyObject;
      Ignored        : Integer;
      pragma Unreferenced (Ignored);

   begin
      --  Prevent python's standard Ctrl-C handling, to leave it to the calling
      --  application.
      Old_Handler := Signal (Sigint, System.Null_Address);
      Py_Initialize;
      Old_Handler := Signal (Sigint, Old_Handler);

      --  We need to set the program name, or some import commands will raise
      --  errors
      Py_SetProgramName ("Python_Interpreter");

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

      PyEval_SetTrace
        (Trace'Access, PyCObject_FromVoidPtr (Interpreter.all'Address));
   end Initialize;

   -------------------------
   -- Set_Default_Console --
   -------------------------

   procedure Set_Default_Console
     (Interpreter : access Python_Interpreter_Record'Class;
      Console        : Interactive_Consoles.Interactive_Console;
      Display_Prompt : Boolean := False) is
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

      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         Interpreter.Destroy_Id := Interpreter_Callback2.Connect
           (Console, "destroy",
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
      pragma Unreferenced (Result);
   begin
      Interpreter.Save_Output := True;
      Interpreter.Current_Output := new String'("");

      Result :=
        Run_Command (Interpreter, Command, Console,
                     Show_Command, Hide_Output, Errors);

      declare
         Output : constant String := Interpreter.Current_Output.all;
      begin
         Free (Interpreter.Current_Output);
         Interpreter.Save_Output := False;
         return Output;
      end;
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

      Default_Console : constant Interactive_Console := Interpreter.Console;

   begin
      Trace (Me, "Running command: " & Cmd);

      if not Hide_Output and then Show_Command then
         Insert_Text (Interpreter, Command & ASCII.LF, Console);
      end if;

      Interpreter.Hide_Output := Hide_Output;
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

      Interpreter.In_Process := True;

      Code := Py_CompileString (Cmd, "<stdin>", Py_Single_Input);

      --  If code compiled just fine
      if Code /= null and then not Indented_Input then
         --  Grab the mouse, keyboard,... so as to avoid recursive loops in
         --  GPS (user selecting a menu while python is running)
         Grab_Widget := Gtk_Widget (Get_View (Interpreter.Console));

         if Grab_Widget /= null then
            Ref (Grab_Widget);

            if Get_Window (Grab_Widget) /= null then
               Gtk.Main.Grab_Add (Grab_Widget);
            end if;
         end if;

         Builtin := PyImport_ImportModule ("__builtin__");
         Obj := PyEval_EvalCode
           (Code, Interpreter.Globals, Interpreter.Globals);
         Py_DECREF (PyObject (Code));

         --  Note: the widget might have been destroyed by the python command,
         --  we need to check that it still exists.

         if Grab_Widget /= null then
            Gtk.Main.Grab_Remove (Grab_Widget);
            Unref (Grab_Widget);
         end if;

         if Obj = null then
            PyErr_Print;
            Errors.all := True;
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

                  Interpreter.Use_Secondary_Prompt :=
                    S /= null and then PyString_AsString (S) =
                    "unexpected EOF while parsing";

                  if not Interpreter.Use_Secondary_Prompt then
                     PyErr_Restore (Typ, Occurrence, Traceback);
                     PyErr_Print;
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

      return Result;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         Interpreter.In_Process := False;
         Interpreter.Hide_Output := False;
         Errors.all := True;

         if Console /= null then
            Set_Default_Console (Interpreter, Default_Console);

            if Default_Console /= null then
               Unref (Default_Console);
            end if;
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

end Python.GUI;
