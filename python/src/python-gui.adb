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
with Gtk.Box;           use Gtk.Box;
with Gtk.Dialog;        use Gtk.Dialog;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Label;         use Gtk.Label;
with Gtk.Stock;         use Gtk.Stock;
with Gtk.Text_Buffer;   use Gtk.Text_Buffer;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Gtk.Text_Mark;     use Gtk.Text_Mark;
with Gtk.Text_Tag;      use Gtk.Text_Tag;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_View;     use Gtk.Text_View;
with Gtk.Main;          use Gtk.Main;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Gdk.Event;         use Gdk.Event;
with Gdk.Main;          use Gdk.Main;
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
with Ada.Unchecked_Conversion;
with Traces;          use Traces;

package body Python.GUI is

   Me : constant Debug_Handle := Create ("Python.GUI");

   Timeout_Threshold : constant Duration := 0.2;   --  in seconds
   --  Timeout between two checks of the gtk+ event queue

   Python_Key : constant History_Key := "python_console";

   function Convert is new Standard.Ada.Unchecked_Conversion
     (System.Address, Python_Interpreter);

   package Interpreter_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Python_Interpreter);

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
   function Flush (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Flush);
   function Read_Line (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Read_Line);
   --  Override the python's methods of the File class.

   procedure Insert_Text
     (Interpreter : access Python_Interpreter_Record'Class;
      Text        : String);
   --  Insert some text in the interpreter console, and scroll as necessary

   procedure Process_Gtk_Events
     (Interpreter : access Python_Interpreter_Record'Class;
      Force       : Boolean := False);
   --  Process all pending gtk+ events

   ------------------------
   -- Process_Gtk_Events --
   ------------------------

   procedure Process_Gtk_Events
     (Interpreter : access Python_Interpreter_Record'Class;
      Force       : Boolean := False)
   is
      Dead   : Boolean;
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
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Interpreter.Console);
      Iter   : Gtk_Text_Iter;
   begin
      Get_End_Iter (Buffer, Iter);
      Insert (Buffer, Iter, Text);

      if Interpreter.Scroll_Mark = null then
         Interpreter.Scroll_Mark := Create_Mark (Buffer, Where => Iter);
      else
         Move_Mark (Buffer, Interpreter.Scroll_Mark, Iter);
      end if;

      Scroll_To_Mark (Interpreter.Console, Interpreter.Scroll_Mark);
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
   end Write;

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
      Dialog : Gtk_Dialog;
      Data   : PyObject;
      Interpreter : Python_Interpreter;
      Button : Gtk_Widget;
      Ent    : Gtk_Entry;
      Label  : Gtk_Label;
      Hbox   : Gtk_Box;
      Iter, Iter2 : Gtk_Text_Iter;
      Buffer : Gtk_Text_Buffer;
   begin
      if not PyArg_ParseTuple (Args, "O|i", File'Address, Size'Address) then
         return null;
      end if;

      Data := PyObject_GetAttrString (File, "gpsdata");
      Interpreter := Convert (PyCObject_AsVoidPtr (Data));

      Process_Gtk_Events (Interpreter, Force => True);

      Gtk_New (Dialog,
               Title  => -"Python input",
               Parent => Gtk_Window (Get_Toplevel (Interpreter.Console)),
               Flags  => Destroy_With_Parent or Modal);

      Gtk_New (Label, -"Some input if needed by the script");
      Pack_Start (Get_Vbox (Dialog), Label);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Hbox);

      --  Grab the prompt for the input line
      Buffer := Get_Buffer (Interpreter.Console);
      Get_End_Iter (Buffer, Iter);
      Copy (Source => Iter, Dest => Iter2);
      Set_Line_Offset (Iter2, 0);
      Gtk_New (Label, Get_Slice (Buffer, Iter2, Iter));
      Pack_Start (Hbox, Label, Expand => False);

      Gtk_New (Ent);
      Set_Activates_Default (Ent, True);
      Pack_Start (Hbox, Ent, Expand => True, Fill => True);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Grab_Default (Button);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      case Run (Dialog) is
         when Gtk_Response_OK =>
            declare
               Text : constant String := Get_Text (Ent);
            begin
               Insert_Text (Interpreter, Text & ASCII.LF);
               Destroy (Dialog);
               return PyString_FromString (Text);
            end;

         when others =>
            Destroy (Dialog);
            PyErr_SetInterrupt;
            return PyString_FromString ("");
      end case;
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
      Stdout      : PyClassObject;
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

      --  Note: we also set __stdout__,..., so that the user can restore them
      --  after temporarily modifying sys.stdout in their own programs
      if not PyRun_SimpleString
        ("sys.stdout=sys.stdin=sys.stderr=GPSStdout ()" & ASCII.LF
         & "sys.__stdout__=sys.__stdin__=sys.__stderr__=sys.stdout" & ASCII.LF)
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
      Console     : access Gtk.Text_View.Gtk_Text_View_Record'Class) is
   begin
      Interpreter.Console := Gtk_Text_View (Console);

      if Interpreter.Uneditable /= null then
         Unref (Interpreter.Uneditable);
      end if;

      if Interpreter.Console /= null then
         --  Disconnect previous key_press_event signal
         Disconnect (Interpreter.Console, Interpreter.Key_Press_Id);
      end if;

      Gtk_New (Interpreter.Uneditable);
      Set_Property
        (Interpreter.Uneditable, Gtk.Text_Tag.Editable_Property, False);
      Add (Get_Tag_Table (Get_Buffer (Console)), Interpreter.Uneditable);
      --  ??? Never unref-ed

      Interpreter.Key_Press_Id := Interpreter_Callback.Connect
        (Console, "key_press_event",
         Interpreter_Callback.To_Marshaller (Key_Press_Handler'Access),
         Python_Interpreter (Interpreter));

      Display_Prompt (Interpreter);
   end Set_Console;

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
      Status : Gdk_Grab_Status;
      pragma Unreferenced (Status);
      use type Gdk_Window;
   begin
      if Cmd = "" & ASCII.LF then
         if not Hide_Output then
            Display_Prompt (Interpreter);
         end if;
         return;
      end if;

      Trace (Me, "Running command: " & Command);

      Code := Py_CompileString (Cmd, "<stdin>", Py_Single_Input);

      --  If code compiled just fine
      if Code /= null and then not Indented_Input then
         --  Grab the mouse, keyboard,... so as to avoid recursive loops in
         --  GPS (user selecting a menu while python is running)
         if Get_Window (Interpreter.Console) /= null then
            Gtk.Main.Grab_Add (Interpreter.Console);
            Status := Pointer_Grab
              (Window     => Get_Window (Interpreter.Console),
               Event_Mask =>
                 Button_Press_Mask
                 or Button_Motion_Mask
                 or Button_Release_Mask,
               Time       => 0);
            Status := Keyboard_Grab
              (Window     => Get_Window (Interpreter.Console),
               Time       => 0);
         end if;

         Obj := PyEval_EvalCode
           (Code, Interpreter.Globals, Interpreter.Globals);
         Py_DECREF (PyObject (Code));

         if Get_Window (Interpreter.Console) /= null then
            Gtk.Main.Grab_Remove (Interpreter.Console);
            Pointer_Ungrab (0);
            Keyboard_Ungrab (0);
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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception "
                & Exception_Information (E));
   end Run_Command;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Interpreter : access Python_Interpreter_Record'Class)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Interpreter.Console);
      Iter, First_Iter : Gtk_Text_Iter;
      Ps     : PyObject;
   begin
      if Interpreter.Use_Secondary_Prompt then
         Ps := PySys_GetObject ("ps2");
      else
         Ps := PySys_GetObject ("ps1");
      end if;

      Insert_Text (Interpreter, PyString_AsString (Ps));

      Get_End_Iter (Buffer, Iter);

      if Interpreter.Prompt_End_Mark = null then
         Interpreter.Prompt_End_Mark := Create_Mark (Buffer, "", Iter);
      else
         Move_Mark (Buffer, Interpreter.Prompt_End_Mark, Iter);
      end if;

      Get_Start_Iter (Buffer, First_Iter);
      Apply_Tag (Buffer, Interpreter.Uneditable, First_Iter, Iter);

      Place_Cursor (Buffer, Iter);
      Scroll_Mark_Onscreen (Interpreter.Console, Get_Insert (Buffer));
   end Display_Prompt;

   -----------------------
   -- Key_Press_Handler --
   -----------------------

   function Key_Press_Handler
     (Object      : access Gtk_Widget_Record'Class;
      Event       : Gdk_Event;
      Interpreter : Python_Interpreter) return Boolean
   is
      pragma Unreferenced (Object);
      Buffer    : constant Gtk_Text_Buffer := Get_Buffer (Interpreter.Console);
      Key       : constant Gdk_Key_Type    := Get_Key_Val (Event);
      Iter, Prompt_End : Gtk_Text_Iter;
      Success : Boolean;
   begin
      case Key is
         when GDK_Up | GDK_Down =>
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
            end;

            return True;

         when GDK_Left =>
            --  Refuse if before prompt
            Get_Iter_At_Mark
              (Buffer, Prompt_End, Interpreter.Prompt_End_Mark);
            Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
            return Compare (Prompt_End, Iter) > -1;

         when GDK_Return | GDK_KP_Enter =>
            Get_End_Iter (Buffer, Iter);
            Insert (Buffer, Iter, ASCII.LF & "");
            Backward_Char (Iter, Success);

            Get_Iter_At_Mark (Buffer, Prompt_End, Interpreter.Prompt_End_Mark);
            Add_To_History
              (Interpreter.History.all, Python_Key,
               Get_Slice (Buffer, Prompt_End, Iter));
            Interpreter.History_Position := -1;

            Run_Command (Interpreter, Get_Slice (Buffer, Prompt_End, Iter));
            return True;

         when GDK_LC_c =>
            if Get_State (Event) = Control_Mask then
               PyErr_SetInterrupt;
               return True;
            end if;

         when others =>
            null;
      end case;
      return False;
   end Key_Press_Handler;

end Python.GUI;
