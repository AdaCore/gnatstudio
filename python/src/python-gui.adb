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
with Gtk.Text_Buffer;   use Gtk.Text_Buffer;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Gtk.Text_Mark;     use Gtk.Text_Mark;
with Gtk.Text_Tag;      use Gtk.Text_Tag;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_View;     use Gtk.Text_View;
with Gtk.Main;          use Gtk.Main;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Object;        use Gtk.Object;
with Gtk.Widget;        use Gtk.Widget;
with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Window;        use Gdk.Window;
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Histories;         use Histories;
with Glide_Intl;        use Glide_Intl;

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with System;
with Python.Ada;      use Python.Ada;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with String_List_Utils;    use String_List_Utils;
with Ada.Unchecked_Conversion;
with Traces;          use Traces;
with GUI_Utils;       use GUI_Utils;

package body Python.GUI is

   Me : constant Debug_Handle := Create ("Python.GUI");

   Timeout_Threshold : constant Duration := 0.2;   --  in seconds
   --  Timeout between two checks of the gtk+ event queue

   Python_Key : constant History_Key := "python_console";

   function Convert is new Standard.Ada.Unchecked_Conversion
     (System.Address, Python_Interpreter);

   package Interpreter_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Python_Interpreter);
   package Interpreter_Callback2 is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Python_Interpreter);

   procedure Display_Prompt
     (Interpreter : access Python_Interpreter_Record'Class);
   --  Display the prompt in the shell window

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Interpreter : Python_Interpreter) return Boolean;
   --  Handle for "key_press" in the interpreter

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
   function Write_No_Capture
     (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Write_No_Capture);
   --  The second is the same as Write, except the output is never saved and
   --  return to the code that executed a python command.

   function Flush (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Flush);
   function Read_Line (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Read_Line);
   function Is_A_TTY (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Is_A_TTY);
   --  Override the python's methods of the File class.

   procedure Process_Gtk_Events
     (Interpreter : access Python_Interpreter_Record'Class;
      Force       : Boolean := False);
   --  Process all pending gtk+ events

   procedure Console_Destroyed
     (Console : access Gtk_Widget_Record'Class;
      Interpreter : Python_Interpreter);
   --  Called when the console of the interpreter is destroyed.

   procedure Update_Prompt_End_Mark
     (Interpreter : access Python_Interpreter_Record'Class);
   --  Move the prompt_end mark to the end of the console.

   function Run_Command_Get_Result
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String) return PyObject;
   --  Run a command and return its result. Nothing is printed in the console.

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

      if Force
        or else Clock - Interpreter.Refresh_Timeout > Timeout_Threshold
      then
         while Gtk.Main.Events_Pending loop
            Dead := Gtk.Main.Main_Iteration;
         end loop;
         Interpreter.Refresh_Timeout := Clock;
      end if;
   end Process_Gtk_Events;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Interpreter : access Python_Interpreter_Record'Class;
      Text        : String)
   is
      Buffer : Gtk_Text_Buffer;
      Iter   : Gtk_Text_Iter;
   begin
      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
        and then not Interpreter.Hide_Output
      then
         Buffer := Get_Buffer (Interpreter.Console);

         Get_End_Iter (Buffer, Iter);
         Insert (Buffer, Iter, Text);

         if Interpreter.Scroll_Mark = null then
            Interpreter.Scroll_Mark := Create_Mark (Buffer, Where => Iter);
         else
            Move_Mark (Buffer, Interpreter.Scroll_Mark, Iter);
         end if;

         Scroll_To_Mark (Interpreter.Console, Interpreter.Scroll_Mark);
      end if;
   end Insert_Text;

   -----------
   -- Write --
   -----------

   function Write (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self);
      S : aliased chars_ptr;
      N : aliased Integer;
      Stdout : aliased PyObject;
      Interpreter : Python_Interpreter;
      Data : PyObject;
      Old : String_Access;
   begin
      if not PyArg_ParseTuple
        (Args, "Os#", Stdout'Address, S'Address, N'Address)
      then
         return null;
      end if;

      Data := PyObject_GetAttrString (Stdout, "gpsdata");
      Interpreter := Convert (PyCObject_AsVoidPtr (Data));

      if Interpreter.Save_Output then
         Old := Interpreter.Current_Output;
         Interpreter.Current_Output := new String'(Old.all & Value (S));
         Free (Old);
      end if;

      Insert_Text (Interpreter, Value (S));
      Process_Gtk_Events (Interpreter);

      Py_INCREF (Py_None);
      return Py_None;
   end Write;

   ----------------------
   -- Write_No_Capture --
   ----------------------

   function Write_No_Capture
     (Self : PyObject; Args : PyObject) return PyObject
   is
      pragma Unreferenced (Self);
      S : aliased chars_ptr;
      N : aliased Integer;
      Stdout : aliased PyObject;
      Interpreter : Python_Interpreter;
      Data : PyObject;
   begin
      if not PyArg_ParseTuple
        (Args, "Os#", Stdout'Address, S'Address, N'Address)
      then
         return null;
      end if;

      Data := PyObject_GetAttrString (Stdout, "gpsdata");
      Interpreter := Convert (PyCObject_AsVoidPtr (Data));

      Insert_Text (Interpreter, Value (S));
      Process_Gtk_Events (Interpreter);

      Py_INCREF (Py_None);
      return Py_None;
   end Write_No_Capture;

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

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self);
      File   : aliased PyObject;
      Size   : aliased Integer := 0;
      Data   : PyObject;
      Interpreter : Python_Interpreter;
      Iter, Iter2   : Gtk_Text_Iter;
      Buffer : Gtk_Text_Buffer;
   begin
      if not PyArg_ParseTuple (Args, "O|i", File'Address, Size'Address) then
         return null;
      end if;

      Data := PyObject_GetAttrString (File, "gpsdata");
      Interpreter := Convert (PyCObject_AsVoidPtr (Data));

      if Interpreter.Console = null
        or else Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         --  Report EOF on stdin
         return PyString_FromString ("");
      end if;

      Buffer := Get_Buffer (Interpreter.Console);
      Update_Prompt_End_Mark (Interpreter);

      Interpreter.Waiting_For_Input := True;
      Gtk.Main.Main;
      Interpreter.Waiting_For_Input := False;

      Get_Iter_At_Mark (Buffer, Iter2, Interpreter.Prompt_End_Mark);
      Get_End_Iter (Buffer, Iter);

      declare
         Response : constant String := Get_Slice (Buffer, Iter2, Iter);
      begin
         Insert_Text (Interpreter, "" & ASCII.LF);
         return PyString_FromString (Response);
      end;
   end Read_Line;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Interpreter : access Python_Interpreter_Record'Class;
      History     : Histories.History)
   is
      Setup_Cmd   : constant String := "import sys" & ASCII.LF;
      Main_Module : PyObject;
      Sigint      : constant Integer := 2;
      Old_Handler : System.Address;
      Prompt      : PyObject;
      Stdout, Stderr : PyClassObject;
      Meths       : PyObject;
      Ignored     : Integer;
      pragma Unreferenced (Ignored);

   begin
      --  Prevent python's standard Ctrl-C handling, to leave it to the calling
      --  application.
      Old_Handler := Signal (Sigint, System.Null_Address);
      Py_Initialize;
      Old_Handler := Signal (Sigint, Old_Handler);

      Interpreter.History := History;

      --  Only remember the last 100 commands.
      Set_Max_Length (History.all, 100, Python_Key);
      Allow_Duplicates (History.all, Python_Key, True, True);

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

      --  Create our own stdout handler

      Meths := PyDict_New;
      PyDict_SetItemString (Meths, "__module__", PyString_FromString ("gps"));
      PyDict_SetItemString
        (Meths, "gpsdata", PyCObject_FromVoidPtr (Interpreter.all'Address));

      Stdout := PyClass_New
        (Bases => null,
         Dict  => Meths,
         Name  => PyString_FromString ("GPSStdout"));
      Ignored := PyModule_AddObject (Main_Module, "GPSStdout", Stdout);

      Add_Method (Stdout, Create_Method_Def ("write",    Write'Access));
      Add_Method (Stdout, Create_Method_Def ("flush",    Flush'Access));
      Add_Method (Stdout, Create_Method_Def ("read",     Read_Line'Access));
      Add_Method (Stdout, Create_Method_Def ("readline", Read_Line'Access));
      Add_Method (Stdout, Create_Method_Def ("isatty",   Is_A_TTY'Access));

      Meths := PyDict_New;
      PyDict_SetItemString (Meths, "__module__", PyString_FromString ("gps"));
      PyDict_SetItemString
        (Meths, "gpsdata", PyCObject_FromVoidPtr (Interpreter.all'Address));

      Stderr := PyClass_New
        (Bases => null,
         Dict  => Meths,
         Name  => PyString_FromString ("GPSStderr"));
      Ignored := PyModule_AddObject (Main_Module, "GPSStderr", Stderr);

      Add_Method
        (Stderr, Create_Method_Def ("write",  Write_No_Capture'Access));
      Add_Method (Stderr, Create_Method_Def ("flush",    Flush'Access));
      Add_Method (Stderr, Create_Method_Def ("read",     Read_Line'Access));
      Add_Method (Stderr, Create_Method_Def ("readline", Read_Line'Access));
      Add_Method (Stderr, Create_Method_Def ("isatty",   Is_A_TTY'Access));

      --  Note: we also set __stdout__,..., so that the user can restore them
      --  after temporarily modifying sys.stdout in their own programs
      if not PyRun_SimpleString
        ("sys.stdout=sys.stdin=GPSStdout ()" & ASCII.LF
         & "sys.stderr=GPSStderr()" & ASCII.LF
         & "sys.__stdout__=sys.__stdin__=sys.stdout" & ASCII.LF
         & "sys.__stderr__=sys.stderr" & ASCII.LF)
      then
         raise Interpreter_Error;
      end if;

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

   -----------------
   -- Set_Console --
   -----------------

   procedure Set_Console
     (Interpreter : access Python_Interpreter_Record'Class;
      Console     : Gtk.Text_View.Gtk_Text_View;
      Grab_Widget : Gtk.Widget.Gtk_Widget := null) is
   begin
      if Console = Interpreter.Console then
         return;
      end if;

      --  If we still have a previous console, disconnect it
      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         Insert_Text (Interpreter, -"<disconnected>");
         Gtk.Handlers.Disconnect
           (Interpreter.Console, Interpreter.Key_Press_Id);
         Gtk.Handlers.Disconnect (Interpreter.Console, Interpreter.Destroy_Id);
      end if;

      Interpreter.Console := Console;
      Interpreter.Grab_Widget := Grab_Widget;

      if Interpreter.Uneditable /= null then
         Unref (Interpreter.Uneditable);
      end if;

      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         --  Disconnect previous key_press_event signal
         Disconnect (Interpreter.Console, Interpreter.Key_Press_Id);

         Gtk_New (Interpreter.Uneditable);
         Set_Property
           (Interpreter.Uneditable, Gtk.Text_Tag.Editable_Property, False);
         Add (Get_Tag_Table (Get_Buffer (Console)), Interpreter.Uneditable);
         --  ??? Never unref-ed

         Interpreter.Key_Press_Id := Interpreter_Callback.Connect
           (Console, "key_press_event",
            Interpreter_Callback.To_Marshaller (Key_Press_Handler'Access),
            Python_Interpreter (Interpreter));
         Interpreter.Destroy_Id := Interpreter_Callback2.Connect
           (Console, "destroy",
            Interpreter_Callback2.To_Marshaller (Console_Destroyed'Access),
            Python_Interpreter (Interpreter));

         Display_Prompt (Interpreter);
      end if;
   end Set_Console;

   -----------------------
   -- Console_Destroyed --
   -----------------------

   procedure Console_Destroyed
     (Console     : access Gtk_Widget_Record'Class;
      Interpreter : Python_Interpreter)
   is
      pragma Unreferenced (Console);
   begin
      Set_Console (Interpreter, null);
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

   ----------------------------
   -- Run_Command_Get_Result --
   ----------------------------

   function Run_Command_Get_Result
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String) return PyObject
   is
      Code         : PyCodeObject;
      Obj, Builtin : PyObject;
   begin
      Trace (Me, "Running command: " & Command);
      Interpreter.In_Process := True;
      Interpreter.Hide_Output := True;

      Code := Py_CompileString (Command, "<stdin>", Py_Single_Input);
      if Code /= null then
         Builtin := PyImport_ImportModule ("__builtin__");
         Obj := PyEval_EvalCode
           (Code, Interpreter.Globals, Interpreter.Globals);

         Py_DECREF (PyObject (Code));

         if Obj /= null then
            Py_DECREF (Obj);
            return PyObject_GetAttrString (Builtin, "_");
         end if;
      end if;

      return null;
   end Run_Command_Get_Result;

   -----------------
   -- Run_Command --
   -----------------

   function Run_Command
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String;
      Hide_Output : Boolean := False) return String is
   begin
      Interpreter.Save_Output := True;
      Interpreter.Current_Output := new String'("");

      Run_Command (Interpreter, Command, Hide_Output);

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

   procedure Run_Command
     (Interpreter : access Python_Interpreter_Record'Class;
      Command     : String;
      Hide_Output : Boolean := False)
   is
      Obj            : PyObject;
      Code           : PyCodeObject;
      Tmp            : String_Access;
      Indented_Input : constant Boolean := Command'Length > 0
        and then (Command (Command'First) = ASCII.HT
                  or else Command (Command'First) = ' ');
      Cmd : constant String := Interpreter.Buffer.all & Command & ASCII.LF;
      Grab_Widget : Gtk_Widget;
      use type Gdk_Window;
   begin
      Interpreter.Hide_Output := Hide_Output;

      if Cmd = "" & ASCII.LF then
         if not Hide_Output then
            Display_Prompt (Interpreter);
         end if;
         return;
      end if;

      Trace (Me, "Running command: " & Command);
      Interpreter.In_Process := True;

      Code := Py_CompileString (Cmd, "<stdin>", Py_Single_Input);

      --  If code compiled just fine
      if Code /= null and then not Indented_Input then
         --  Grab the mouse, keyboard,... so as to avoid recursive loops in
         --  GPS (user selecting a menu while python is running)
         Grab_Widget := Interpreter.Grab_Widget;
         if Grab_Widget = null then
            Grab_Widget := Gtk_Widget (Interpreter.Console);
         end if;

         if Grab_Widget /= null then
            Ref (Grab_Widget);

            if Get_Window (Grab_Widget) /= null then
               Gtk.Main.Grab_Add (Grab_Widget);
            end if;
         end if;

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
         else
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
                  else
                     PyErr_Clear;
                  end if;
               end if;
            end;
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

      if not Hide_Output then
         Display_Prompt (Interpreter);
      end if;

      Interpreter.In_Process := False;
      Interpreter.Hide_Output := False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception "
                & Exception_Information (E));
         Interpreter.In_Process := False;
         Interpreter.Hide_Output := False;
   end Run_Command;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Interpreter : access Python_Interpreter_Record'Class)
   is
      Buffer : Gtk_Text_Buffer;
      Iter, First_Iter : Gtk_Text_Iter;
      Ps     : PyObject;
   begin
      if Interpreter.Console /= null
        and then not Gtk.Object.Destroyed_Is_Set (Interpreter.Console)
      then
         Buffer := Get_Buffer (Interpreter.Console);

         if Interpreter.Use_Secondary_Prompt then
            Ps := PySys_GetObject ("ps2");
         else
            Ps := PySys_GetObject ("ps1");
         end if;

         Insert_Text (Interpreter, PyString_AsString (Ps));

         Update_Prompt_End_Mark (Interpreter);

         Get_End_Iter (Buffer, Iter);
         Get_Start_Iter (Buffer, First_Iter);
         Apply_Tag (Buffer, Interpreter.Uneditable, First_Iter, Iter);

         Place_Cursor (Buffer, Iter);
         Scroll_Mark_Onscreen (Interpreter.Console, Get_Insert (Buffer));
      end if;
   end Display_Prompt;

   ----------------------------
   -- Update_Prompt_End_Mark --
   ----------------------------

   procedure Update_Prompt_End_Mark
     (Interpreter : access Python_Interpreter_Record'Class)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Interpreter.Console);
      Iter   : Gtk_Text_Iter;
   begin
      Get_End_Iter (Buffer, Iter);

      if Interpreter.Prompt_End_Mark = null then
         Interpreter.Prompt_End_Mark := Create_Mark (Buffer, "", Iter);
      else
         Move_Mark (Buffer, Interpreter.Prompt_End_Mark, Iter);
      end if;
   end Update_Prompt_End_Mark;

   -----------------------
   -- Key_Press_Handler --
   -----------------------

   function Key_Press_Handler
     (Object      : access Gtk_Widget_Record'Class;
      Event       : Gdk_Event;
      Interpreter : Python_Interpreter) return Boolean
   is
      function Completion
        (Input     : String;
         User_Data : Glib.Object.GObject)
         return String_List_Utils.String_List.List;
      --  Handles completion for the commands

      function Completion
        (Input     : String;
         User_Data : Glib.Object.GObject)
         return String_List_Utils.String_List.List
      is
         use String_List_Utils.String_List;
         pragma Unreferenced (User_Data);
         List        : String_List_Utils.String_List.List;
         Start       : Natural := Input'First - 1;
         Last        : Natural := Input'Last + 1;
         Obj, Item   : PyObject;
      begin
         for N in reverse Input'Range loop
            if Input (N) = ' ' or else Input (N) = ASCII.HT then
               Start := N;
               exit;
            elsif Input (N) = '.' and then Last > Input'Last then
               Last := N;
            end if;
         end loop;

         if Start >= Input'Last then
            return Null_List;
         else
            Obj := Run_Command_Get_Result
              (Interpreter,
               "__builtins__.dir(" & Input (Start + 1 .. Last - 1) & ")");

            if Obj = null then
               return Null_List;
            else
               for Index in 0 .. PyList_Size (Obj) - 1 loop
                  Item := PyList_GetItem (Obj, Index);

                  declare
                     S : constant String := PyString_AsString (Item);
                  begin
                     if Last >= Input'Last
                       or else Input (Last + 1 .. Input'Last)
                         = S (S'First .. S'First + Input'Last - Last - 1)
                     then
                        Prepend
                          (List, Input (Input'First .. Last - 1) & '.' & S);
                     end if;
                  end;
               end loop;

               Py_DECREF (Obj);
               return List;
            end if;
         end if;
      end Completion;


      pragma Unreferenced (Object);
      Buffer    : constant Gtk_Text_Buffer := Get_Buffer (Interpreter.Console);
      Key       : constant Gdk_Key_Type    := Get_Key_Val (Event);
      Iter, Prompt_End : Gtk_Text_Iter;
      Success : Boolean;
   begin
      case Key is
         when GDK_Up | GDK_Down =>
            if not Interpreter.Waiting_For_Input then
               declare
                  Hist : constant String_List_Access := Get_History
                    (Interpreter.History.all, Python_Key);
               begin
                  if Hist /= null then
                     if Key = GDK_Up
                    and then
                       Interpreter.History_Position + Hist'First < Hist'Last
                     then
                        Interpreter.History_Position :=
                          Interpreter.History_Position + 1;

                     elsif Key = GDK_Down
                       and then Interpreter.History_Position /= -1
                     then
                        Interpreter.History_Position :=
                          Interpreter.History_Position - 1;
                     end if;
                  end if;

                  Get_Iter_At_Mark
                    (Buffer, Prompt_End, Interpreter.Prompt_End_Mark);
                  Get_End_Iter (Buffer, Iter);
                  Delete (Buffer, Prompt_End, Iter);
                  if Interpreter.History_Position /= -1 then
                     Insert
                       (Buffer, Prompt_End,
                        Hist (Hist'First + Interpreter.History_Position).all);
                  end if;

                  Get_End_Iter (Buffer, Iter);
                  Place_Cursor (Buffer, Iter);
                  Success := Scroll_To_Iter
                    (Interpreter.Console,
                     Iter          => Iter,
                     Within_Margin => 0.0,
                     Use_Align     => False,
                     Xalign        => 0.0,
                     Yalign        => 0.0);
               end;
            end if;

            return True;

         when GDK_Left =>
            --  Refuse if before prompt
            if not Interpreter.Waiting_For_Input then
               Get_Iter_At_Mark
                 (Buffer, Prompt_End, Interpreter.Prompt_End_Mark);
               Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
               return Compare (Prompt_End, Iter) > -1;
            end if;

         when GDK_Tab | GDK_KP_Tab =>
            Do_Completion
              (View            => Interpreter.Console,
               Completion      => Completion'Unrestricted_Access,
               Prompt_End_Mark => Interpreter.Prompt_End_Mark,
               Uneditable_Tag  => Interpreter.Uneditable,
               User_Data       => null);
            return True;

         when GDK_Return | GDK_KP_Enter =>
            if Interpreter.Waiting_For_Input then
               Gtk.Main.Main_Quit;
            else
               Get_End_Iter (Buffer, Iter);
               Insert (Buffer, Iter, ASCII.LF & "");
               Backward_Char (Iter, Success);

               Get_Iter_At_Mark
                 (Buffer, Prompt_End, Interpreter.Prompt_End_Mark);
               Add_To_History
                 (Interpreter.History.all, Python_Key,
                  Get_Slice (Buffer, Prompt_End, Iter));
               Interpreter.History_Position := -1;

               Run_Command (Interpreter, Get_Slice (Buffer, Prompt_End, Iter));

               --  Preserve the focus on the console after interactive
               --  execution
               Grab_Focus (Interpreter.Console);

            end if;
            return True;

         when GDK_LC_c =>
            if Interpreter.In_Process
              and then Get_State (Event) = Control_Mask
            then
               PyErr_SetInterrupt;
               return True;
            end if;

         when others =>
            null;
      end case;
      return False;
   end Key_Press_Handler;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Interpreter : access Python_Interpreter_Record'Class)
      return Gtk.Text_View.Gtk_Text_View is
   begin
      return Interpreter.Console;
   end Get_Console;

end Python.GUI;
