-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Interfaces.C.Strings;       use Interfaces.C, Interfaces.C.Strings;
with GNAT.IO;                    use GNAT.IO;
with GNAT.Strings;               use GNAT.Strings;
with Scripts.Impl;               use Scripts, Scripts.Impl;
with System;                     use System;

package body Scripts.Python is

   ------------------------
   -- Python_Subprograms --
   ------------------------

   type Python_Subprogram_Record is new Subprogram_Record with record
      Script     : Python_Scripting;
      Subprogram : PyObject;
   end record;

   overriding function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean;
   overriding function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return String;
   overriding function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class)
      return GNAT.Strings.String_List;
   overriding procedure Free (Subprogram : in out Python_Subprogram_Record);
   overriding function Get_Name
     (Subprogram : access Python_Subprogram_Record) return String;
   overriding function Get_Script
     (Subprogram : Python_Subprogram_Record) return Scripting_Language;
   --  See doc from inherited subprograms

   --------------------------
   -- Python_Callback_Data --
   --------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (PyObject_Array, PyObject_Array_Access);

   procedure Prepare_Value_Key
     (Data   : in out Python_Callback_Data'Class;
      Key    : PyObject;
      Append : Boolean);
   --  Internal version of Set_Return_Value_Key

   ---------------------------
   -- Python_Class_Instance --
   ---------------------------

   type Python_Class_Instance_Record is new Class_Instance_Record with record
      Data    : PyObject;
   end record;
   type Python_Class_Instance is access all Python_Class_Instance_Record'Class;

   overriding function Print_Refcount
     (Instance : access Python_Class_Instance_Record) return String;
   overriding procedure Incref (Inst : access Python_Class_Instance_Record);
   overriding procedure Decref (Inst : access Python_Class_Instance_Record);
   overriding function Is_Subclass
     (Instance : access Python_Class_Instance_Record;
      Base     : Class_Type) return Boolean;
   --  See doc from inherited subprogram

   procedure Set_CI (CI : Class_Instance);
   function Get_CI
     (Script : Python_Scripting; Object : PyObject) return Class_Instance;
   --  Set or retrieve the Class_Instance associated with a python object.
   --  In the case of Get, if the object is not already associated with an
   --  class_instance, a new one is created.

   ------------------
   -- Handler_Data --
   ------------------

   type Handler_Data (Length : Natural) is record
      Script                     : Python_Scripting;
      Handler                    : Module_Command_Function;
      Minimum_Args, Maximum_Args : Natural;
      Is_Method                  : Boolean := False;
      Command                    : String (1 .. Length);
   end record;
   type Handler_Data_Access is access Handler_Data;
   --  Information stores with each python function to call the right Ada
   --  subprogram.

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Handler_Data_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Handler_Data, Handler_Data_Access);

   procedure Destroy_Handler_Data (Handler : System.Address);
   pragma Convention (C, Destroy_Handler_Data);
   --  Called when the python object associated with Handler is destroyed.

   procedure On_PyObject_Data_Destroy (Data : System.Address);
   pragma Convention (C, On_PyObject_Data_Destroy);
   --  Called when a PyObject associated with a Class_Instance is destroyed, so
   --  that we also decrement the class_instance's refcounter

   ----------------------
   -- Interpreter_View --
   ----------------------

   function First_Level (Self, Args, Kw : PyObject) return PyObject;
   pragma Convention (C, First_Level);
   --  First level handler for all functions exported to python. This function
   --  is in charge of dispatching to the actual Ada subprogram.

   procedure Setup_Return_Value (Data : in out Python_Callback_Data'Class);
   --  Mark Data as containing a return value, and free the previous value if
   --  there is any

   procedure Trace_Dump (Name : String; Obj : PyObject);
   pragma Unreferenced (Trace_Dump);
   --  Print debug info for Obj

   function Refcount_Msg
     (Obj : PyObject) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Refcount_Msg, "ada_py_refcount_msg");
   --  Print a debug message to trace the refcounting on Obj

   function Run_Command
     (Script          : access Python_Scripting_Record'Class;
      Command         : String;
      Console         : Virtual_Console := null;
      Show_Command    : Boolean := False;
      Hide_Output     : Boolean := False;
      Hide_Exceptions : Boolean := False;
      Errors          : access Boolean) return PyObject;
   --  Execute a command in the interpreter, and send its output to the
   --  console. Return its return value (which doesn't need to be Py_DECREF,
   --  since it is a borrowed reference).
   --  If Hide_Output is True, then nothing is printed on the console. If the
   --  command is incomplete and would require extra input (a secondary prompt
   --  in interactive mode), then it is not executed.
   --  Errors is set to True if there was an error executing the command or
   --  if the input was incomplete.

   function Run_Command
     (Script          : access Python_Scripting_Record'Class;
      Command         : String;
      Console         : Virtual_Console := null;
      Show_Command    : Boolean := False;
      Hide_Output     : Boolean := False;
      Hide_Exceptions : Boolean := False;
      Errors          : access Boolean) return String;
   --  Same as above, but also return the output of the command

   function Trace
     (User_Arg : PyObject;
      Frame    : System.Address;
      Why      : Why_Trace_Func;
      Obj      : PyObject) return Integer;
   pragma Convention (C, Trace);
   --  Suprogram called for each python instruction execution. It periodically
   --  checks the event queue, so that the interpreter can be interrupted.

   function Convert is new Standard.Ada.Unchecked_Conversion
     (System.Address, Python_Scripting);
   function Convert is new Standard.Ada.Unchecked_Conversion
     (Python_Scripting, System.Address);

   --------------------
   -- Block_Commands --
   --------------------

   procedure Block_Commands
     (Script : access Python_Scripting_Record; Block  : Boolean) is
   begin
      Script.Blocked := Block;
   end Block_Commands;

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump (Name : String; Obj : PyObject) is
   begin
      if Obj /= null then
         Put_Line (Name & "=<null>");
      else
         Put_Line (Name & "="""
                & PyString_AsString (PyObject_Str (Obj)) & '"' & ASCII.LF
                & PyString_AsString (PyObject_Str (PyObject_Dir (Obj)))
                & ASCII.LF
                & PyString_AsString (PyObject_Repr (Obj)));
      end if;
   end Trace_Dump;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Script : access Python_Scripting_Record) is
   begin
      Free (Script.Buffer);

      --  Cannot call Py_Finalize, since some class_instance might be finalized
      --  when the program exit, and would try to call Py_DECREF on their
      --  associated PyObject, which no longer exists
      --  Py_Finalize;
   end Destroy;

   -------------------------------
   -- Register_Python_Scripting --
   -------------------------------

   procedure Register_Python_Scripting
     (Repo          : Scripts_Repository;
      Module        : String)
   is
      Script  : Python_Scripting;
      Ignored : Integer;
      Result  : PyObject;
      Tmp     : Boolean;
      pragma Unreferenced (Ignored, Tmp, Result);
      Errors  : aliased Boolean;

      function Signal
        (Num : Integer; Handler : System.Address) return System.Address;
      pragma Import (C, Signal, "signal");

      Setup_Cmd      : constant String := "import sys" & ASCII.LF;
      Init_Output    : constant String :=
        "def __gps_no_write (*args): pass" & ASCII.LF
        & "__gps_saved_stdout=None" & ASCII.LF
        & "__gps_saved_stderr=None" & ASCII.LF
        & "def __gps_hide_output ():" & ASCII.LF
        & "   global __gps_saved_stdout" & ASCII.LF
        & "   global __gps_saved_stderr" & ASCII.LF
        & "   __gps_saved_stdout=sys.stdout.write" & ASCII.LF
        & "   __gps_saved_stderr=sys.stderr.write" & ASCII.LF
        & "   try:" & ASCII.LF
        & "     sys.stdout.write=__gps_no_write" & ASCII.LF
        & "     sys.stderr.write=__gps_no_write" & ASCII.LF
        & "   except: pass" & ASCII.LF
        & ASCII.LF
        & "def __gps_restore_output():" & ASCII.LF
        & "   if sys.stdout.write == __gps_no_write:" & ASCII.LF
        & "      try:" & ASCII.LF
        & "         sys.stdout.write = __gps_saved_stdout" & ASCII.LF
        & "         sys.stderr.write = __gps_saved_stderr" & ASCII.LF
        & "      except: pass" & ASCII.LF
        & ASCII.LF;

      Main_Module    : PyObject;
      Sigint         : constant Integer := 2;
      Old_Handler    : System.Address;
      pragma Warnings (Off, Old_Handler);
      Prompt         : PyObject;

   begin
      Script := new Python_Scripting_Record;
      Script.Repo := Repo;
      Register_Scripting_Language (Repo, Script);

      --  We need to set the program name, or some import commands will raise
      --  errors

      Py_SetProgramName ("Python_Interpreter");

      --  Prevent python's standard Ctrl-C handling, to leave it to the calling
      --  application.

      Old_Handler := Signal (Sigint, System.Null_Address);
      Py_Initialize;
      Old_Handler := Signal (Sigint, Old_Handler);

      if not PyRun_SimpleString (Setup_Cmd) then
         raise Program_Error with "Could not execute " & Setup_Cmd;
      end if;

      Main_Module := PyImport_AddModule ("__main__");
      if Main_Module = null then
         raise Program_Error with "Could not import module __main__";
      end if;
      Script.Globals := PyModule_GetDict (Main_Module);

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

      Script.Buffer := new String'("");

      Script.Builtin := PyImport_ImportModule ("__builtin__");

      --  Create the module, in which all functions and classes are
      --  registered

      Script.Module := Py_InitModule (Module);

      --  Register functions used to provide support for hiding the output
      --  of commands

      Tmp := PyRun_SimpleString (Init_Output);

      Result := Run_Command
        (Script,
         "import " & Module,
         Hide_Output => True,
         Errors      => Errors'Unchecked_Access);
      if Errors then
         raise Program_Error with "Could not import module " & Module;
      end if;

      Script.Exception_Unexpected := PyErr_NewException
        (Module & ".Unexpected_Exception", null, null);
      Script.Exception_Misc := PyErr_NewException
        (Module & ".Exception", null, null);
      Script.Exception_Missing_Args := PyErr_NewException
        (Module & ".Missing_Arguments", null, null);
      Script.Exception_Invalid_Arg := PyErr_NewException
        (Module & ".Invalid_Argument", null, null);

      --  PyGTK prints its error messages using sys.argv, which doesn't
      --  exist in non-interactive mode. We therefore define it here
      Result := Run_Command
        (Script,
         "sys.argv=['GPS']", Hide_Output => True,
         Errors         => Errors'Unchecked_Access);
   end Register_Python_Scripting;

   --------------------------
   -- Destroy_Handler_Data --
   --------------------------

   procedure Destroy_Handler_Data (Handler : System.Address) is
      H : Handler_Data_Access := Convert (Handler);
   begin
      Unchecked_Free (H);
   end Destroy_Handler_Data;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Python_Callback_Data) is
   begin
      if Data.Args /= null then
         Py_DECREF (Data.Args);
      end if;

      if Data.Kw /= null then
         Py_DECREF (Data.Kw);
      end if;

      --  Do not free the return value, this is taken care of later on by all
      --  callers

      Unchecked_Free (Data.Kw_Params);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (Data : Python_Callback_Data) return Callback_Data'Class is
      D    : Python_Callback_Data := Data;
      Item : PyObject;
   begin
      if D.Args /= null then
         D.Args := PyTuple_New (PyTuple_Size (D.Args));
         for T in 0 .. PyTuple_Size (D.Args) - 1 loop
            Item := PyTuple_GetItem (Data.Args, T);
            Py_INCREF (Item);
            PyTuple_SetItem (D.Args, T, Item);
         end loop;
      end if;
      if D.Kw /= null then
         Py_INCREF (D.Kw);
      end if;
      D.Return_Value := null;
      D.Return_Dict  := null;
      D.Kw_Params    := null;
      return D;
   end Clone;

   ------------
   -- Create --
   ------------

   function Create
     (Script          : access Python_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class
   is
      Callback : constant Python_Callback_Data :=
        (Callback_Data with
         Script           => Python_Scripting (Script),
         Args             => PyTuple_New (Arguments_Count),
         Kw               => Py_None,
         Return_Value     => null,
         Return_Dict      => null,
         Has_Return_Value => False,
         Return_As_List   => False,
         Kw_Params        => null,
         Is_Method        => False);
   begin
      Py_INCREF (Callback.Kw);
      return Callback;
   end Create;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Subprogram_Type) is
   begin
      PyTuple_SetItem (Data.Args, N - 1,
                       Python_Subprogram_Record (Value.all).Subprogram);
      Py_INCREF (Python_Subprogram_Record (Value.all).Subprogram);
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : String) is
   begin
      PyTuple_SetItem (Data.Args, N - 1, PyString_FromString (Value));
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Integer) is
   begin
      PyTuple_SetItem
        (Data.Args, N - 1, PyInt_FromLong (Interfaces.C.long (Value)));
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Boolean) is
   begin
      PyTuple_SetItem (Data.Args, N - 1, PyInt_FromLong (Boolean'Pos (Value)));
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Class_Instance)
   is
      Inst : constant PyObject := Python_Class_Instance (Get_CIR (Value)).Data;
   begin
      PyTuple_SetItem (Data.Args, N - 1, Inst);
      Py_INCREF (Inst);
   end Set_Nth_Arg;

   -----------------
   -- First_Level --
   -----------------

   function First_Level (Self, Args, Kw : PyObject) return PyObject is
      Handler  : constant Handler_Data_Access :=
                   Convert (PyCObject_AsVoidPtr (Self));
      Size     : Integer := PyTuple_Size (Args);
      Callback : Python_Callback_Data;
   begin
      if Kw /= null then
         Size := PyDict_Size (Kw) + Size;
      end if;

      if Handler.Is_Method then
         Size := Size - 1;  --  First param is always the instance
      end if;

      --  Check number of arguments
      if Handler.Minimum_Args > Size
        or else Size > Handler.Maximum_Args
      then
         if Handler.Minimum_Args > Size then
            PyErr_SetString (Handler.Script.Exception_Missing_Args,
                             "Wrong number of parameters, expecting at least"
                             & Handler.Minimum_Args'Img);
         else
            PyErr_SetString (Handler.Script.Exception_Missing_Args,
                             "Wrong number of parameters, expecting at most"
                             & Handler.Maximum_Args'Img);
         end if;
         return null;
      end if;

      Callback.Args         := Args;
      Callback.Kw           := Kw;
      Callback.Return_Value := Py_None;
      Callback.Return_Dict  := null;
      Callback.Script       := Handler.Script;
      Callback.Is_Method    := Handler.Is_Method;
      Py_INCREF (Callback.Return_Value);
      Py_INCREF (Callback.Args);

      if Callback.Kw /= null then
         Py_INCREF (Callback.Kw);
      end if;

      Handler.Handler.all (Callback, Handler.Command);

      --  This doesn't free the return value
      Free (Callback);

      if Callback.Return_Dict /= null then
         if Callback.Return_Value /= null then
            Py_DECREF (Callback.Return_Value);
         end if;
         return Callback.Return_Dict;

      else
         return Callback.Return_Value;
      end if;

   exception
      when E : Invalid_Parameter =>
         if not Callback.Has_Return_Value
           or else Callback.Return_Value /= null
         then
            PyErr_SetString
              (Handler.Script.Exception_Invalid_Arg, Exception_Message (E));
         end if;

         Free (Callback);
         return null;

      when others =>
         if not Callback.Has_Return_Value
           or else  Callback.Return_Value /= null
         then
            PyErr_SetString (Handler.Script.Exception_Unexpected,
                             "unexpected internal exception");
         end if;

         Free (Callback);
         return null;
   end First_Level;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Script        : access Python_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False)
   is
      H         : constant Handler_Data_Access := new Handler_Data'
        (Length       => Command'Length,
         Command      => Command,
         Handler      => Handler,
         Script       => Python_Scripting (Script),
         Is_Method    => Class /= No_Class and then not Static_Method,
         Minimum_Args => Minimum_Args,
         Maximum_Args => Maximum_Args);
      User_Data : constant PyObject := PyCObject_FromVoidPtr
        (H.all'Address, Destroy_Handler_Data'Access);
      Klass     : PyObject;
      Def       : PyMethodDef;
   begin
      if Class = No_Class then
         Add_Function
           (Module => Script.Module,
            Func   => Create_Method_Def (Command, First_Level'Access),
            Self   => User_Data);

      else
         if Command = Constructor_Method then
            Def := Create_Method_Def ("__init__", First_Level'Access);
         elsif Command = Addition_Method then
            Def := Create_Method_Def ("__add__", First_Level'Access);
         elsif Command = Substraction_Method then
            Def := Create_Method_Def ("__sub__", First_Level'Access);
         elsif Command = Comparison_Method then
            Def := Create_Method_Def ("__cmp__", First_Level'Access);
         elsif Command = Destructor_Method then
            Def := Create_Method_Def ("__del__", First_Level'Access);
         else
            Def := Create_Method_Def (Command, First_Level'Access);
         end if;

         Klass := Lookup_Class_Object (Script.Module, Get_Name (Class));

         if Static_Method then
            Add_Static_Method
              (Class => Klass, Func => Def, Self => User_Data);
         else
            Add_Method (Class => Klass, Func => Def, Self => User_Data);
         end if;
      end if;
   end Register_Command;

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class
     (Script        : access Python_Scripting_Record;
      Name          : String;
      Base          : Class_Type := No_Class)
   is
      Dict    : constant PyDictObject := PyDict_New;
      Class   : PyClassObject;
      Ignored : Integer;
      Bases   : PyObject := null;
      S       : Interfaces.C.Strings.chars_ptr;
      pragma Unreferenced (Ignored);
   begin
      PyDict_SetItemString
        (Dict, "__module__",
         PyObject_GetAttrString (Script.Module, "__name__"));

      if Base /= No_Class then
         Bases := Create_Tuple
           ((1 => Lookup_Class_Object (Script.Module, Get_Name (Base))));
      else
         --  Would be nice to derive from the object class, so that we can
         --  override __new__. For some reason, this raises a Storage_Error
         --  later on when we call PyClass_Name on such a class...
         --  Since object is a <type> and not a class, it is likely that
         --  PyClass_Name is no longer appropriate...
         --  See http://www.cafepy.com/article/python_types_and_objects/
         --     python_types_and_objects.html#id2514298
         --  We also need to modify the implementation of Is_Subclass, so this
         --  is quite an implementation effort.
         --
         --  The goal in overriding __new__ is so that we can have
         --     GPS.EditorBuffer (file)
         --  return an existing instance if the file is already associated with
         --  a python instance... This isn't doable through __init__

--           declare
--              Module : constant PyObject :=
--                PyImport_ImportModule ("__builtin__");
--              Dict, Obj : PyObject;
--           begin
--              if Module = null
--                or else Module = Py_None
--              then
--                 raise Constraint_Error;
--              end if;
--
--              Dict := PyModule_GetDict (Module);
--              if Dict = null or else Dict = Py_None then
--                 raise Constraint_Error;
--              end if;
--
--              Obj := PyDict_GetItemString (Dict, "object");
--              if Obj = null or else Obj = Py_None then
--                 raise Constraint_Error;
--              end if;
--
--              Py_XINCREF (Obj);
--              Bases := Create_Tuple ((1 => Obj));
--           end;
         null;
      end if;

      Class := PyClass_New
        (Bases => Bases,
         Dict  => Dict,
         Name  => PyString_FromString (Name));

      S := New_String (Name);
      Ignored := PyModule_AddObject (Script.Module, S, Class);
      Free (S);
   end Register_Class;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Script : access Python_Scripting_Record) return Boolean is
   begin
      if Script.In_Process then
         PyErr_SetInterrupt;
         return True;
      else
         return False;
      end if;
   end Interrupt;

   --------------
   -- Complete --
   --------------

   procedure Complete
     (Script      : access Python_Scripting_Record;
      Input       : String;
      Completions : out String_Lists.List)
   is
      Start       : Natural := Input'First - 1;
      Last        : Natural := Input'Last + 1;
      Obj, Item   : PyObject;
      Errors      : aliased Boolean;

   begin
      Completions := String_Lists.Empty_List;

      for N in reverse Input'Range loop
         if Input (N) = ' ' or else Input (N) = ASCII.HT then
            Start := N;
            exit;
         elsif Input (N) = '.' and then Last > Input'Last then
            Last := N;
         end if;
      end loop;

      if Start < Input'Last then
         Obj := Run_Command
           (Script,
            "__builtins__.dir(" & Input (Start + 1 .. Last - 1) & ")",
            Hide_Output => True,
            Hide_Exceptions => True,
            Errors => Errors'Unchecked_Access);

         if Obj /= null then
            for Index in 0 .. PyList_Size (Obj) - 1 loop
               Item := PyList_GetItem (Obj, Index);

               declare
                  S : constant String := PyString_AsString (Item);
               begin
                  if S'First + Input'Last - Last - 1 <= S'Last
                    and then
                      (Last >= Input'Last
                       or else Input (Last + 1 .. Input'Last)
                       = S (S'First .. S'First + Input'Last - Last - 1))
                  then
                     String_Lists.Append
                       (Completions,
                        Input (Input'First .. Last - 1) & '.' & S);
                  end if;
               end;
            end loop;

            Py_DECREF (Obj);
         end if;
      end if;
   end Complete;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Script  : access Python_Scripting_Record;
      Console : Virtual_Console := null)
   is
      Ps : PyObject;
   begin
      if Script.Use_Secondary_Prompt then
         Ps := PySys_GetObject ("ps2");
      else
         Ps := PySys_GetObject ("ps1");
      end if;

      Insert_Prompt (Script, Console, PyString_AsString (Ps));
   end Display_Prompt;

   -----------------
   -- Run_Command --
   -----------------

   function Run_Command
     (Script          : access Python_Scripting_Record'Class;
      Command         : String;
      Console         : Virtual_Console := null;
      Show_Command    : Boolean := False;
      Hide_Output     : Boolean := False;
      Hide_Exceptions : Boolean := False;
      Errors          : access Boolean) return String
   is
      Result : PyObject;
      Str    : PyObject;
   begin
      Result :=
        Run_Command (Script, Command, Console,
                     Show_Command, Hide_Output, Hide_Exceptions, Errors);

      if Result /= null and then not Errors.all then
         Str := PyObject_Str (Result);
         Py_DECREF (Result);
         declare
            S : constant String := PyString_AsString (Str);
         begin
            Py_DECREF (Str);
            Insert_Log (Script, Console, "output is: " & S);
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
     (Script          : access Python_Scripting_Record'Class;
      Command         : String;
      Console         : Virtual_Console := null;
      Show_Command    : Boolean := False;
      Hide_Output     : Boolean := False;
      Hide_Exceptions : Boolean := False;
      Errors          : access Boolean) return PyObject
   is
      Result         : PyObject := null;
      Obj            : PyObject;
      Code           : PyCodeObject;
      Tmp            : GNAT.Strings.String_Access;
      Indented_Input : constant Boolean := Command'Length > 0
        and then
          (Command (Command'First) = ASCII.HT
           or else Command (Command'First) = ' ');
      Cmd          : constant String := Script.Buffer.all & Command & ASCII.LF;

      Ignored : Boolean;
      pragma Unreferenced (Ignored);
      Default_Console : constant Virtual_Console :=
        Get_Default_Console (Script);

   begin
      --  Make sure that the output to sys.stdout is properly hidden. This is
      --  in particular required when doing completion, since the result of
      --  the command to get completions would be output in the middle of the
      --  command the user is typing.

      if Hide_Output then
         Insert_Log (Script, Console, "__gps_hide_output");
         Ignored := PyRun_SimpleString ("__gps_hide_output()");
      end if;

      Insert_Log
        (Script, Console, "executing: " & Script.Buffer.all & Command);

      Insert_Text (Script, Console, Command & ASCII.LF, not Show_Command);

      Errors.all := False;

      if Cmd = "" & ASCII.LF then
         if not Hide_Output then
            Display_Prompt (Script);
         end if;
         return null;
      end if;

      if Console /= null then
         if Default_Console /= null then
            Ref (Default_Console);
         end if;
         Set_Default_Console (Script, Console);
      end if;

      if Get_Default_Console (Script) /= null then
         Set_Hide_Output (Get_Default_Console (Script), Hide_Output);
      end if;

      --  Reset previous output
      PyObject_SetAttrString (Script.Builtin, "_", Py_None);

      Script.In_Process := True;

      Code := Py_CompileString (Cmd, "<stdin>", Py_Single_Input);

      --  If code compiled just fine
      if Code /= null and then not Indented_Input then
         if Get_Default_Console (Script) /= null then
            Grab_Events (Get_Default_Console (Script), True);
         end if;

         --  ??? The trace function is only called when moving to the next line
         --  of the script, so a line that takes a long time to execute blocks
         --  the interface. The only solution here seems to use threads...
         PyEval_SetTrace
           (Trace'Access,
            PyCObject_FromVoidPtr (Convert (Python_Scripting (Script))));
         Obj := PyEval_EvalCode (Code, Script.Globals, Script.Globals);
         Py_DECREF (PyObject (Code));
         PyEval_SetTrace (null, null);
         if Get_Default_Console (Script) /= null then
            Grab_Events (Get_Default_Console (Script), False);
         end if;

         if Obj = null then
            declare
               EType, Occurrence, Traceback : PyObject;
            begin
               if not Hide_Exceptions then
                  if Hide_Output then
                     --  We need to preserve the current exception before
                     --  executing the next command
                     PyErr_Fetch (EType, Occurrence, Traceback);
                     Insert_Log (Script, Console, "__gps_restore_output");
                     Ignored := PyRun_SimpleString ("__gps_restore_output()");
                     if Get_Default_Console (Script) /= null then
                        Set_Hide_Output (Get_Default_Console (Script), False);
                     end if;
                     PyErr_Restore (EType, Occurrence, Traceback);
                  end if;

                  --  Always display exceptions in the python console, since it
                  --  is likely they are unexpected and should be fixed.
                  PyErr_Print;

                  if Hide_Output then
                     Insert_Log (Script, Console, "__gps_hide_output");
                     Ignored := PyRun_SimpleString ("__gps_hide_output()");
                     if Get_Default_Console (Script) /= null then
                        Set_Hide_Output (Get_Default_Console (Script), True);
                     end if;
                  end if;
               else
                  PyErr_Clear;
               end if;

               Errors.all := True;
            end;
         else
            --  No other python command between this one and the previous
            --  call to PyEval_EvalCode
            if PyObject_HasAttrString (Script.Builtin, "_") then
               Result := PyObject_GetAttrString (Script.Builtin, "_");
            else
               Result := null;
            end if;
            Py_DECREF (Obj);
         end if;

         Script.Use_Secondary_Prompt := False;
         Free (Script.Buffer);
         Script.Buffer := new String'("");

      --  Do we have compilation error because input was incomplete ?

      elsif not Hide_Output then
         Script.Use_Secondary_Prompt := Indented_Input;

         if not Script.Use_Secondary_Prompt then
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
                     Script.Use_Secondary_Prompt := False;
                  else
                     declare
                        Msg : constant String := PyString_AsString (S);
                     begin
                        --  Second message appears when typing:
                        --    >>> if 1:
                        --    ...   pass
                        --    ... else:
                        Script.Use_Secondary_Prompt :=
                          Msg = "unexpected EOF while parsing"
                          or else Msg = "expected an indented block";
                     end;
                  end if;

                  if not Script.Use_Secondary_Prompt then
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

         if Script.Use_Secondary_Prompt then
            Tmp := Script.Buffer;
            Script.Buffer := new String'
              (Script.Buffer.all & Command & ASCII.LF);
            Free (Tmp);
         else
            Free (Script.Buffer);
            Script.Buffer := new String'("");
         end if;
      else
         PyErr_Clear;
      end if;

      if not Hide_Output then
         Display_Prompt (Script);
      end if;

      Script.In_Process := False;
      if Get_Default_Console (Script) /= null then
         Set_Hide_Output (Get_Default_Console (Script), False);
      end if;

      if Console /= null then
         Set_Default_Console (Script, Default_Console);
         if Default_Console /= null then
            Unref (Default_Console);
         end if;
      end if;

      if Hide_Output then
         Insert_Log (Script, Console, "__gps_restore_output");
         Ignored := PyRun_SimpleString ("__gps_restore_output()");
      end if;

      return Result;

   exception
      when others =>
         Script.In_Process := False;
         if Get_Default_Console (Script) /= null then
            Set_Hide_Output (Get_Default_Console (Script), False);
         end if;
         Errors.all := True;

         if Hide_Output then
            Ignored := PyRun_SimpleString ("__gps_restore_output()");
         end if;

         return Result;
   end Run_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script       : access Python_Scripting_Record;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean)
   is
      E      : aliased Boolean;
      Result : PyObject;
   begin
      if Script.Blocked then
         Errors := True;
         Insert_Error (Script, Console, "A command is already executing");
      else
         Result := Run_Command
           (Script, Command,
            Console      => Console,
            Hide_Output  => Hide_Output,
            Show_Command => Show_Command,
            Errors       => E'Unchecked_Access);
         Py_XDECREF (Result);
         Errors := E;
      end if;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script       : access Python_Scripting_Record;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String
   is
      pragma Unreferenced (Show_Command);
   begin
      if Script.Blocked then
         Errors.all := True;
         Insert_Error (Script, Console, "A command is already executing");
         return "";
      else
         return Run_Command
           (Script, Command,
            Console     => Console,
            Hide_Output => Hide_Output,
            Errors      => Errors);
      end if;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script      : access Python_Scripting_Record;
      Command     : String;
      Console      : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean
   is
      Obj : PyObject;
      Result : Boolean;
   begin
      if Script.Blocked then
         Errors.all := True;
         Insert_Error (Script, Console, "A command is already executing");
         return False;
      else
         Obj := Run_Command
           (Script, Command, Console,
            False, Hide_Output, False, Errors);
         Result := Obj /= null
           and then ((PyInt_Check (Obj) and then PyInt_AsLong (Obj) = 1)
                     or else
                       (PyString_Check (Obj)
                        and then PyString_AsString (Obj) = "true"));
         Py_XDECREF (Obj);
         return Result;
      end if;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script  : access Python_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean
   is
      Obj : PyObject;
      Errors : aliased Boolean;

   begin
      if Script.Blocked then
         --  Insert (Script.Kernel, "A command is already executing");
         return False;
      else
         Obj := Run_Command
           (Script,
            Command     => Command,
            Console     => null,
            Hide_Output => True,
            Errors      => Errors'Unchecked_Access);

         if Obj /= null and then PyFunction_Check (Obj) then
            return Execute_Command (Script, Obj, Args);
         else
--              Insert
--                (Script.Kernel,
--                 Command & " is not a function, when called from a hook");
            return False;
         end if;
      end if;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return PyObject
   is
      Obj   : PyObject;
      Args2 : PyObject;
      Size  : Integer;
      Item  : PyObject;

   begin
      if Script.Blocked then
         --  Insert (Script.Kernel, "A python command is already executing");
         return null;
      end if;

      --  Bound methods: we need to explicitly pass the instance as the first
      --  argument.
      if PyMethod_Check (Command) then
         if PyMethod_Self (Command) /= null then
            --  See code in classobject.c::instancemethod_call()
            Size  := PyTuple_Size (Python_Callback_Data (Args).Args);
            Args2 := PyTuple_New (Size => Size + 1);
            Py_INCREF (PyMethod_Self (Command));
            PyTuple_SetItem (Args2, 0, PyMethod_Self (Command));
            for T in 0 .. Size - 1 loop
               Item := PyTuple_GetItem (Python_Callback_Data (Args).Args, T);
               Py_INCREF (Item);
               PyTuple_SetItem (Args2, T  + 1, Item);
            end loop;
         else
            --  The "self" argument is the first in Args, nothing special to do
            Args2 := Python_Callback_Data (Args).Args;
            Py_INCREF (Args2);
         end if;

         Obj := PyObject_Call
           (Object => PyMethod_Function (Command),
            Args   => Args2,
            Kw     => Python_Callback_Data (Args).Kw);
         Py_DECREF (Args2);

      else
         Obj := PyEval_EvalCodeEx
           (PyFunction_Get_Code (Command),
            Globals  => PyFunction_Get_Globals (Command),
            Locals   => null,
            Args     => Python_Callback_Data (Args).Args,
            Kwds     => Python_Callback_Data (Args).Kw,
            Defaults => PyFunction_Get_Defaults (Command),
            Closure  => PyFunction_Get_Closure (Command));
      end if;

      if Obj = null then
         PyErr_Print;
      end if;

      return Obj;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return String
   is
      Obj : constant PyObject := Execute_Command (Script, Command, Args);
   begin
      if Obj /= null
        and then PyString_Check (Obj)
      then
         declare
            Str : constant String := PyString_AsString (Obj);
         begin
            Py_DECREF (Obj);
            return Str;
         end;
      else
         if Obj /= null then
            Py_DECREF (Obj);
         end if;
         return "";
      end if;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return Boolean
   is
      Obj    : constant PyObject := Execute_Command (Script, Command, Args);
      Result : Boolean;
   begin
      if Obj = null then
         return False;
      else
         Result := ((PyInt_Check (Obj) and then PyInt_AsLong (Obj) = 1)
                    or else
             (PyString_Check (Obj)
              and then PyString_AsString (Obj) = "true"));
         Py_DECREF (Obj);
         return Result;
      end if;
   end Execute_Command;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File
     (Script      : access Python_Scripting_Record;
      Filename    : String;
      Console      : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Show_Command : Boolean := True;
      Errors      : out Boolean) is
   begin
      Script.Current_File := To_Unbounded_String (Filename);
      Execute_Command
        (Script, "execfile (r'" & Filename & "')",
         Console, Hide_Output, Show_Command, Errors);
      Script.Current_File := Null_Unbounded_String;
   end Execute_File;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Script : access Python_Scripting_Record) return String is
      pragma Unreferenced (Script);
   begin
      return Python_Name;
   end Get_Name;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Data : Python_Callback_Data)
      return Scripting_Language
   is
   begin
      return Scripting_Language (Data.Script);
   end Get_Script;

   --------------------
   -- Get_Repository --
   --------------------

   function Get_Repository (Script : access Python_Scripting_Record)
      return Scripts_Repository is
   begin
      return Script.Repo;
   end Get_Repository;

   --------------------
   -- Current_Script --
   --------------------

   function Current_Script
     (Script : access Python_Scripting_Record) return String
   is
   begin
      if Script.Current_File = Null_Unbounded_String then
         return "<python script>";
      else
         return To_String (Script.Current_File);
      end if;
   end Current_Script;

   -------------------------
   -- Number_Of_Arguments --
   -------------------------

   function Number_Of_Arguments (Data : Python_Callback_Data) return Natural is
   begin
      if Data.Kw /= null then
         return PyDict_Size (Data.Kw) + PyTuple_Size (Data.Args);
      else
         return PyTuple_Size (Data.Args);
      end if;
   end Number_Of_Arguments;

   ---------------------
   -- Name_Parameters --
   ---------------------

   procedure Name_Parameters
     (Data  : in out Python_Callback_Data; Names : Cst_Argument_List)
   is
      S : Integer := 0;
      First : Integer := 0;
   begin
      if Data.Kw = null then
         return;
      end if;

      if Data.Args /= null then
         S := PyTuple_Size (Data.Args);
      end if;

      if Data.Is_Method then
         First := First + 1;
      end if;

      --  Parameters can not be both positional and named

      for Index in First .. S - 1 loop
         if PyDict_GetItemString
           (Data.Kw, Names (Index + Names'First - First).all) /= null
         then
            Set_Error_Msg
              (Data, "Parameter cannot be both positional "
               & " and named: " & Names (Index + Names'First).all);
            raise Invalid_Parameter;
         end if;
      end loop;

      --  Check that there are no unknown keywords

      declare
         Pos : Integer := 0;
         Key, Value : PyObject;
      begin
         loop
            PyDict_Next (Data.Kw, Pos, Key, Value);
            exit when Pos = -1;

            declare
               S : constant String := PyString_AsString (Key);
               Found : Boolean := False;
            begin
               for N in Names'Range loop
                  if Names (N).all = S then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Set_Error_Msg
                    (Data, "Invalid keyword parameter: " & S);
                  raise Invalid_Parameter;
               end if;
            end;
         end loop;
      end;

      --  Assign parameters

      Unchecked_Free (Data.Kw_Params);
      Data.Kw_Params := new PyObject_Array (1 .. Names'Length + First);

      if Data.Is_Method then
         Data.Kw_Params (Data.Kw_Params'First) :=
           PyTuple_GetItem (Data.Args, 0);
      end if;

      for P in Data.Kw_Params'First + First .. Data.Kw_Params'Last loop
         Data.Kw_Params (P) := PyDict_GetItemString
           (Data.Kw,
            Names (P - Data.Kw_Params'First - First + Names'First).all);
      end loop;
   end Name_Parameters;

   ---------------
   -- Get_Param --
   ---------------

   function Get_Param (Data : Python_Callback_Data'Class; N : Positive)
      return PyObject
   is
      Obj : PyObject := null;
   begin
      --  Check keywords parameters. As a special case, we do not check when
      --  getting the first parameter of a method, which is always the
      --  instance, since the callback will generally want to do this in the
      --  common part, before the command-specific parts.
      if (N /= 1 or else not Data.Is_Method)
        and then Data.Kw_Params = null
        and then Data.Kw /= null
      then
         PyErr_SetString
           (Data.Script.Exception_Misc,
            "Keyword parameters not supported");
         raise Invalid_Parameter;
      elsif Data.Args /= null and then N <= PyTuple_Size (Data.Args) then
         Obj := PyTuple_GetItem (Data.Args, N - 1);
      elsif Data.Kw_Params /= null and then N <= Data.Kw_Params'Last then
         Obj := Data.Kw_Params (N);
      end if;

      if Obj = null then
         raise No_Such_Parameter;
      elsif Obj = Py_None then
         raise No_Such_Parameter;
      end if;
      return Obj;
   end Get_Param;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Data : Python_Callback_Data; N : Positive) return String
   is
      Item : constant PyObject := Get_Param (Data, N);
   begin
      if not PyString_Check (Item) then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be a string");
      else
         return PyString_AsString (Item);
      end if;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Data : Python_Callback_Data; N : Positive)
      return Integer
   is
      Item : constant PyObject := Get_Param (Data, N);
   begin
      if not PyInt_Check (Item) then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an integer");
      else
         return Integer (PyInt_AsLong (Item));
      end if;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Boolean
   is
      Item : constant PyObject := Get_Param (Data, N);
   begin
      --  ??? Could add more cases of automatic conversion: strings containing
      --  "true" or "false", or add support for booleans for newer versions of
      --  python (>= 2.3)

      if PyInt_Check (Item) then
         return PyInt_AsLong (Item) = 1;
      elsif PyString_Check (Item) then
         return To_Lower (PyString_AsString (Item)) = "True";
      else
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be a boolean");
      end if;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data       : Python_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance
   is
      Item       : PyObject;
      C          : PyObject;
      Item_Class : PyObject;

   begin
      if Class /= Any_Class then
         C := Lookup_Class_Object (Data.Script.Module, Get_Name (Class));
      end if;

      Item := Get_Param (Data, N);  --  Item is a borrowed reference

      if not PyInstance_Check (Item) then
         if Class /= Any_Class then
            Raise_Exception
              (Invalid_Parameter'Identity,
               "Parameter" & Integer'Image (N) & " should be an instance of "
               & Get_Name (Class));
         else
            Raise_Exception
              (Invalid_Parameter'Identity,
               "Parameter" & Integer'Image (N)
               & " should be a class instance");
         end if;
      end if;

      Item_Class := PyObject_GetAttrString (Item, "__class__");
      --  Item_Class must be DECREF'd

      if Item_Class = null then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class) & " but has no __class__");
      end if;

      if Class /= Any_Class
        and then
          (C = null or else not PyClass_IsSubclass (Item_Class, Base => C))
      then
         Py_DECREF (Item_Class);
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class));
      end if;

      Py_DECREF (Item_Class);
      return Get_CI (Python_Scripting (Get_Script (Data)), Item);

   exception
      when No_Such_Parameter =>
         if Allow_Null then
            return No_Class_Instance;
         else
            raise;
         end if;
   end Nth_Arg;

   ------------
   -- Decref --
   ------------

   procedure Decref (Inst : access Python_Class_Instance_Record) is
   begin
      if Inst.Data /= null then
         Py_DECREF (Inst.Data);
      end if;
      Decref (Class_Instance_Record (Inst.all)'Access);
   end Decref;

   ------------
   -- Incref --
   ------------

   procedure Incref (Inst : access Python_Class_Instance_Record) is
   begin
      Py_INCREF (Inst.Data);
      Incref (Class_Instance_Record (Inst.all)'Access);
   end Incref;

   ------------------------------
   -- On_PyObject_Data_Destroy --
   ------------------------------

   procedure On_PyObject_Data_Destroy (Data : System.Address) is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Python_Class_Instance);
      D : constant Python_Class_Instance := Convert (Data);
   begin
      --  When this function is called, the pyObject D.Data is being destroyed,
      --  so we make sure that we will not try in the future to access it.
      --
      --  We do not need to decrease the refcounting for D, since python owns
      --  a reference to that type through a user data, and that refs is
      --  automatically releases when the user data is freed (through
      --  controlled typed).
      --  ??? Is the above really true: in Set_CI, we did an explicit
      --  Incref, so it seems we should have an explicit Decref here. However,
      --  when we put it we get a Storage_Error in the automatic testsuite.
      --  More likely, since we did an unchecked_conversion, we didn't
      --  increase the refcount once more in this procedure, and since there is
      --  a call to Finalize for D, this takes care of the refcounting.

      D.Data := null;
      --  Decref (D);
   end On_PyObject_Data_Destroy;

   ------------
   -- Set_CI --
   ------------

   procedure Set_CI (CI : Class_Instance) is
      Data : constant PyObject := PyCObject_FromVoidPtr
        (Get_CIR (CI).all'Address, On_PyObject_Data_Destroy'Access);
   begin
      --  Python owns a reference to the CI, so that the latter can never be
      --  freed while the python object exists.
      Incref (Get_CIR (CI));

      PyObject_SetAttrString
        (Python_Class_Instance (Get_CIR (CI)).Data, "__gps_data", Data);
      Py_DECREF (Data);
   end Set_CI;

   ------------
   -- Get_CI --
   ------------

   function Get_CI
     (Script : Python_Scripting; Object : PyObject) return Class_Instance
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Python_Class_Instance);
      Item   : constant PyObject :=
                 PyObject_GetAttrString (Object, "__gps_data");
      CIR    : System.Address;
      CI     : Python_Class_Instance;
      Result : Class_Instance;
   begin
      if Item = null then
         PyErr_Clear;
         --  If there was no instane associated, avoid a python exception later

         CI := new Python_Class_Instance_Record;
         CI.Data := Object;
         Result := From_Instance (Script, CI);
         Set_CI (Result);  --  Associate with Object for the future
         Decref (Get_CIR (Result));  --  Since From_Instance incremented it
         return Result;

      elsif not PyCObject_Check (Item) then
         return No_Class_Instance;

      else
         CIR := PyCObject_AsVoidPtr (Item);
         Py_DECREF (Item);

         Result := From_Instance (Script, Convert (CIR));
         return Result;
      end if;
   end Get_CI;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Instance : access Python_Class_Instance_Record;
      Base     : Class_Type) return Boolean
   is
      C : constant PyObject := PyObject_GetAttrString
        (Instance.Data, "__class__");
      B : constant PyObject := Lookup_Class_Object
        (Python_Scripting (Instance.Script).Module, Get_Name (Base));
   begin
      return PyClass_IsSubclass (C, Base => B);
   end Is_Subclass;

   ------------------------
   -- Setup_Return_Value --
   ------------------------

   procedure Setup_Return_Value (Data : in out Python_Callback_Data'Class) is
   begin
      if Data.Return_Value /= null then
         Py_DECREF (Data.Return_Value);
      end if;

      Data.Has_Return_Value := True;
      Data.Return_Value := null;
   end Setup_Return_Value;

   -------------------
   -- Set_Error_Msg --
   -------------------

   procedure Set_Error_Msg
     (Data : in out Python_Callback_Data; Msg : String) is
   begin
      Setup_Return_Value (Data);
      if Msg /= "" then
         PyErr_SetString (Data.Script.Exception_Misc, Msg);
      end if;
   end Set_Error_Msg;

   -----------------------
   -- Prepare_Value_Key --
   -----------------------

   procedure Prepare_Value_Key
     (Data   : in out Python_Callback_Data'Class;
      Key    : PyObject;
      Append : Boolean)
   is
      Obj, List : PyObject;
      Tmp : Integer;
      pragma Unreferenced (Tmp);

   begin
      if Data.Return_Dict = null then
         Data.Return_Dict := PyDict_New;
      end if;

      if Append then
         Obj := PyDict_GetItem (Data.Return_Dict, Key);

         if Obj /= null then
            if PyList_Check (Obj) then
               List := Obj;
            else
               List := PyList_New;
               Tmp := PyList_Append (List, Obj);
            end if;

            Tmp := PyList_Append (List, Data.Return_Value);

         else
            List := Data.Return_Value;
         end if;

      else
         List := Data.Return_Value;
      end if;

      Tmp := PyDict_SetItem (Data.Return_Dict, Key, List);

      Py_DECREF (Data.Return_Value);

      Data.Return_Value := Py_None;
      Py_INCREF (Data.Return_Value);
      Data.Return_As_List := False;
   end Prepare_Value_Key;

   --------------------------
   -- Set_Return_Value_Key --
   --------------------------

   procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Integer;
      Append : Boolean := False)
   is
      K : constant PyObject := PyInt_FromLong (long (Key));
   begin
      Prepare_Value_Key (Data, K, Append);
      Py_DECREF (K);
   end Set_Return_Value_Key;

   --------------------------
   -- Set_Return_Value_Key --
   --------------------------

   procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : String;
      Append : Boolean := False)
   is
      K : constant PyObject := PyString_FromString (Key);
   begin
      Prepare_Value_Key (Data, K, Append);
      Py_DECREF (K);
   end Set_Return_Value_Key;

   --------------------------
   -- Set_Return_Value_Key --
   --------------------------

   procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False) is
   begin
      Prepare_Value_Key
        (Data, Python_Class_Instance (Get_CIR (Key)).Data, Append);
   end Set_Return_Value_Key;

   ------------------------------
   -- Set_Return_Value_As_List --
   ------------------------------

   procedure Set_Return_Value_As_List
     (Data : in out Python_Callback_Data; Size : Natural := 0)
   is
      pragma Unreferenced (Size);
   begin
      Setup_Return_Value (Data);
      Data.Return_As_List := True;
      Data.Has_Return_Value := True;
      Data.Return_Value := PyList_New;
   end Set_Return_Value_As_List;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : Integer)
   is
      Num : Integer;
      pragma Unreferenced (Num);
   begin
      if Data.Return_As_List then
         Num := PyList_Append
           (Data.Return_Value, PyInt_FromLong (long (Value)));
      else
         Setup_Return_Value (Data);
         Data.Return_Value := PyInt_FromLong (long (Value));
      end if;
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : String)
   is
      Num : Integer;
      pragma Unreferenced (Num);
   begin
      if Data.Return_As_List then
         Num := PyList_Append (Data.Return_Value, PyString_FromString (Value));
      else
         Setup_Return_Value (Data);
         Data.Return_Value := PyString_FromString (Value);
      end if;
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : Boolean) is
   begin
      Set_Return_Value (Data, Boolean'Pos (Value));
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : Class_Instance)
   is
      V   : constant Python_Class_Instance :=
              Python_Class_Instance (Get_CIR (Value));
      Obj : PyObject;
      Num : Integer;
      pragma Unreferenced (Num);
   begin
      if V /= null then
         Obj := V.Data;
      else
         Obj := Py_None;
      end if;

      if Data.Return_As_List then
         Num := PyList_Append (Data.Return_Value, Obj);
      else
         Py_INCREF (Obj);
         Setup_Return_Value (Data);
         Data.Return_Value := Obj;
      end if;
   end Set_Return_Value;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance
     (Script : access Python_Scripting_Record;
      Class  : Class_Type) return Class_Instance
   is
      Klass : constant PyObject := Lookup_Class_Object
        (Script.Module, Get_Name (Class));
      Inst : Class_Instance;
   begin
      if Klass = null then
         return No_Class_Instance;
      end if;

      Inst := Get_CI
        (Python_Scripting (Script), PyInstance_NewRaw (Klass, null));

      --  The PyObject should have a single reference in the end, owned by
      --  the class instance itself.

      Py_DECREF (Python_Class_Instance (Get_CIR (Inst)).Data);
      return Inst;
   end New_Instance;

   --------------------
   -- Print_Refcount --
   --------------------

   function Print_Refcount
     (Instance : access Python_Class_Instance_Record) return String is
   begin
      if Instance.Data /= null then
         return Print_Refcount (Class_Instance_Record (Instance.all)'Access)
           & " Py=" & Value (Refcount_Msg (Instance.Data));
      else
         return Print_Refcount (Class_Instance_Record (Instance.all)'Access)
           & " Py=<None>";
      end if;
   end Print_Refcount;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Subprogram_Type
   is
      Item : constant PyObject := Get_Param (Data, N);
   begin
      if Item /= null
        and then (PyFunction_Check (Item) or else PyMethod_Check (Item))
      then
         Py_INCREF (Item);
         return new Python_Subprogram_Record'
           (Subprogram_Record with
            Script     => Python_Scripting (Get_Script (Data)),
            Subprogram => Item);
      else
         raise Invalid_Parameter;
      end if;
   end Nth_Arg;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean is
   begin
      return Execute_Command
        (Script  => Subprogram.Script,
         Command => Subprogram.Subprogram,
         Args    => Args);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return String is
   begin
      return Execute_Command
        (Script  => Subprogram.Script,
         Command => Subprogram.Subprogram,
         Args    => Args);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return GNAT.Strings.String_List
   is
      Obj : constant PyObject := Execute_Command
        (Script => Subprogram.Script,
         Command => Subprogram.Subprogram,
         Args    => Args);
   begin
      if Obj = null then
         return (1 .. 0 => null);

      elsif Obj = Py_None then
         Py_DECREF (Obj);
         return (1 .. 0 => null);

      elsif PyString_Check (Obj) then
         declare
            Str : constant String := PyString_AsString (Obj);
         begin
            Py_DECREF (Obj);
            return (1 .. 1 => new String'(Str));
         end;

      elsif PyList_Check (Obj) then
         declare
            Result : GNAT.Strings.String_List (1 .. PyList_Size (Obj));
            Item   : PyObject;
         begin
            for J in 0 .. PyList_Size (Obj) - 1 loop
               Item := PyList_GetItem (Obj, J);
               if PyString_Check (Item) then
                  Result (J + 1) := new String'(PyString_AsString (Item));
               end if;
            end loop;
            Py_DECREF (Obj);
            return Result;
         end;
      end if;

      Py_DECREF (Obj);
      return (1 .. 0 => null);
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Subprogram : in out Python_Subprogram_Record) is
   begin
      Py_DECREF (Subprogram.Subprogram);
   end Free;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Subprogram : access Python_Subprogram_Record) return String
   is
      S    : constant PyObject := PyObject_Str (Subprogram.Subprogram);
      Name : constant String := PyString_AsString (S);
   begin
      Py_DECREF (S);
      return Name;
   end Get_Name;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script
     (Subprogram : Python_Subprogram_Record) return Scripting_Language
   is
   begin
      return Scripting_Language (Subprogram.Script);
   end Get_Script;

   -------------------------
   -- Set_Default_Console --
   -------------------------

   procedure Set_Default_Console
     (Script       : access Python_Scripting_Record;
      Console      : Virtual_Console)
   is
      Inst         : Class_Instance;
      Cons         : PyObject := Py_None;
   begin
      Set_Default_Console
        (Scripting_Language_Record (Script.all)'Access, Console);

      if Console /= null
        and then Get_Console_Class (Get_Repository (Script)) /= No_Class
      then
         Inst := Get_Instance (Script, Console);
         if Inst = No_Class_Instance then
            Inst := New_Instance
              (Script, Get_Console_Class (Get_Repository (Script)));
            Set_Data (Inst, Console => Console);
         end if;
         Cons := Python_Class_Instance (Get_CIR (Inst)).Data;

         PyDict_SetItemString
           (PyModule_GetDict (PyImport_ImportModule ("sys")), "stdout", Cons);
         PyDict_SetItemString
           (PyModule_GetDict (PyImport_ImportModule ("sys")), "stderr", Cons);
         PyDict_SetItemString
           (PyModule_GetDict (PyImport_ImportModule ("sys")), "stdin", Cons);
      end if;
   end Set_Default_Console;

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
      Script : constant Python_Scripting := Convert
        (PyCObject_AsVoidPtr (User_Arg));
      C : constant Virtual_Console := Get_Default_Console (Script);
   begin
      if C /= null then
         Process_Pending_Events (C);
      end if;
      return 0;
   end Trace;

end Scripts.Python;
