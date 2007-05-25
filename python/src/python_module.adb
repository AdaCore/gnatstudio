-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
--                              AdaCore                              --
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
with Interfaces.C.Strings;       use Interfaces.C, Interfaces.C.Strings;
with System;                     use System;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;               use GNAT.Strings;

with Basic_Types;

with Glib.Object;                use Glib.Object;
with Glib.Xml_Int;               use Glib.Xml_Int;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;

with Commands.Custom;            use Commands.Custom;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Custom;          use GPS.Kernel.Custom;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with Histories;                  use Histories;
with Python.GUI;                 use Python, Python.GUI;
with Python.Ada;                 use Python.Ada;
with Interactive_Consoles;       use Interactive_Consoles;
with Traces;                     use Traces;
with String_Utils;               use String_Utils;
with String_List_Utils;          use String_List_Utils;
with Projects;                   use Projects;
with Entities;                   use Entities;
with VFS;                        use VFS;
with File_Utils;                 use File_Utils;
with HTables;

package body Python_Module is

   Me  : constant Debug_Handle := Create ("Python_Module");
   Ref : constant Debug_Handle := Create ("Scripts.Ref", Off);

   GPS_Module_Name : constant String := "GPS";
   --  Name of the GPS module in the python interpreter.

   Python_Language_Name : constant String := "Python";

   type Hash_Index is range 0 .. 100000;
   function Hash is new HTables.Hash (Hash_Index);

   ----------------------
   -- Python_scripting --
   ----------------------

   type Python_Scripting_Record is new Scripting_Language_Record with record
      Kernel                   : GPS.Kernel.Kernel_Handle;
      Blocked                  : Boolean := False;
      Interpreter              : Python_Interpreter;
      GPS_Exception            : PyObject;
      GPS_Module               : PyObject;
      GPS_Missing_Args         : PyObject;
      GPS_Invalid_Arg          : PyObject;
      GPS_Unexpected_Exception : PyObject;

      Current_File             : VFS.Virtual_File;
      --  The script we are currently executing
   end record;
   type Python_Scripting is access all Python_Scripting_Record'Class;

   type Python_Module_Record is new Module_ID_Record with record
      Script : Python_Scripting;
   end record;
   type Python_Module_Record_Access is access all Python_Module_Record'Class;

   Python_Module_Id : Python_Module_Record_Access;

   procedure Destroy (Module : in out Python_Module_Record);
   --  Called when the module is destroyed

   procedure Destroy (Script : access Python_Scripting_Record);
   procedure Block_Commands
     (Script : access Python_Scripting_Record; Block  : Boolean);
   procedure Register_Command
     (Script        : access Python_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   procedure Register_Class
     (Script : access Python_Scripting_Record;
      Name   : String;
      Base   : Class_Type := No_Class);
   procedure Execute_Command
     (Script       : access Python_Scripting_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   function Execute_Command
     (Script       : access Python_Scripting_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   function Execute_Command
     (Script      : access Python_Scripting_Record;
      Command     : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean;
   function Execute_Command
     (Script  : access Python_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return String;

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return PyObject;
   --  Need to unref the returned value

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class) return Boolean;
   procedure Execute_File
     (Script      : access Python_Scripting_Record;
      Filename    : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : out Boolean);
   function Get_Name (Script : access Python_Scripting_Record) return String;
   function Get_Kernel
     (Script : access Python_Scripting_Record) return Kernel_Handle;
   function Current_Script
     (Script : access Python_Scripting_Record) return String;
   --  See doc from inherited subprograms

   procedure Load_Dir
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Dir              : String;
      Default_Autoload : Boolean);
   --  Load all .py files from Dir, if any.
   --  Default_Autoload indicates whether scripts in this directory should
   --  be autoloaded by default, unless otherwise mentioned in
   --  ~/.gps/startup.xml

   ------------------------
   -- Python_Subprograms --
   ------------------------

   type Python_Subprogram_Record is new Subprogram_Record with record
      Subprogram : PyObject;
   end record;

   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean;
   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return String;
   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class)
      return GNAT.Strings.String_List;
   procedure Free (Subprogram : in out Python_Subprogram_Record);
   function Get_Name
     (Subprogram : access Python_Subprogram_Record) return String;
   function Get_Script
     (Subprogram : Python_Subprogram_Record) return Scripting_Language;
   --  See doc from inherited subprograms

   --------------------------
   -- Python_Callback_Data --
   --------------------------

   type PyObject_Array is array (Natural range <>) of PyObject;
   type PyObject_Array_Access is access PyObject_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (PyObject_Array, PyObject_Array_Access);

   type Python_Callback_Data is new Callback_Data with record
      Script           : Python_Scripting;
      Args, Kw         : PyObject;
      Return_Value     : PyObject;
      Return_Dict      : PyObject;
      Has_Return_Value : Boolean := False;
      Return_As_List   : Boolean := False;
      Kw_Params        : PyObject_Array_Access;
      Is_Method        : Boolean;
   end record;
   --  Kw_Params is used to handle keyword parameters. They map from positional
   --  index to the actual object.

   function Clone (Data : Python_Callback_Data) return Callback_Data'Class;
   function Get_Script (Data : Python_Callback_Data) return Scripting_Language;
   function Number_Of_Arguments (Data : Python_Callback_Data) return Natural;
   procedure Name_Parameters
     (Data  : in out Python_Callback_Data; Names : Cst_Argument_List);
   function Nth_Arg (Data : Python_Callback_Data; N : Positive) return String;
   function Nth_Arg (Data : Python_Callback_Data; N : Positive) return Integer;
   function Nth_Arg (Data : Python_Callback_Data; N : Positive) return Boolean;
   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Subprogram_Type;
   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance;
   procedure Set_Error_Msg (Data : in out Python_Callback_Data; Msg : String);
   procedure Set_Return_Value_As_List
     (Data : in out Python_Callback_Data; Size : Natural := 0);
   procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : Integer);
   procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : String);
   procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : Boolean);
   procedure Set_Return_Value
     (Data   : in out Python_Callback_Data; Value : Class_Instance);
   procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : String;
      Append : Boolean := False);
   procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Integer;
      Append : Boolean := False);
   procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False);
   --  See doc from inherited subprogram

   procedure Prepare_Value_Key
     (Data   : in out Python_Callback_Data'Class;
      Key    : PyObject;
      Append : Boolean);
   --  Internal version of Set_Return_Value_Key

   function Get_Param (Data : Python_Callback_Data'Class; N : Positive)
      return PyObject;
   --  Return the N-th command line parameter, taking into account the keywords
   --  if any.
   --  The returned value is a borrowed reference and must not be DECREF'd

   procedure Free (Data : in out Python_Callback_Data);
   function Create
     (Script          : access Python_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : String);
   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Integer);
   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Boolean);
   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Class_Instance);
   procedure Set_Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Value : Subprogram_Type);
   --  See inherited documentation

   ---------------------------
   -- Python_Class_Instance --
   ---------------------------

   type Python_Class_Instance_Record is new Class_Instance_Record with record
      Data    : PyObject;
   end record;
   type Python_Class_Instance is access all Python_Class_Instance_Record'Class;

   function New_Instance
     (Script : access Python_Scripting_Record;
      Class : Class_Type) return Class_Instance;
   function Print_Refcount
     (Instance : access Python_Class_Instance_Record) return String;
   procedure Incref (Inst : access Python_Class_Instance_Record);
   procedure Decref (Inst : access Python_Class_Instance_Record);
   function Is_Subclass
     (Instance : access Python_Class_Instance_Record;
      Base     : Class_Type) return Boolean;
   --  See doc from inherited subprogram

   procedure Set_CI (CI : Class_Instance);
   function Get_CI (Object : PyObject) return Class_Instance;
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

   function Create_Python_Console
     (Script : access Python_Scripting_Record'Class;
      Kernel : Kernel_Handle) return MDI_Child;
   --  Create the python console if it doesn't exist yet.

   function Python_Console_Command_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String;
   --  Called when a command was typed by the user in the python console

   function Python_Console_Completion_Handler
     (Input     : String;
      User_Data : System.Address) return String_List_Utils.String_List.List;
   --  Provides completion in the Python console

   function Python_Console_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address) return Boolean;
   --  Called when ctrl-c is pressed in the console

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

   procedure Open_Python_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a new python console if none exists

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Support functions for saving the desktop

   procedure Python_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Global_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands related to the various classes

   function Refcount_Msg
     (Obj : PyObject) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Refcount_Msg, "ada_py_refcount_msg");
   --  Print a debug message to trace the refcounting on Obj

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
      if Obj = null then
         Trace (Me, Name & "=<null>");
      else
         Trace (Me, Name & "="""
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
      Destroy (Script.Interpreter);
   end Destroy;

   ------------------------------------
   -- Python_Console_Command_Handler --
   ------------------------------------

   function Python_Console_Command_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String
   is
      Errors  : aliased Boolean;
      Result : PyObject;
      pragma Unreferenced (User_Data);
   begin
      Result := Run_Command
        (Interpreter  => Python_Module_Id.Script.Interpreter,
         Command      => Input,
         Console      => null,
         Show_Command => False,
         Hide_Output  => False,
         Errors       => Errors'Unchecked_Access);
      Py_XDECREF (Result);

      --  Preserve the focus on the console after interactive execution
      Grab_Focus (Get_View (Console));

      --  Do not return anything, since the output of the command is handled
      --  directly by python already.
      return "";
   end Python_Console_Command_Handler;

   ---------------------------------------
   -- Python_Console_Completion_Handler --
   ---------------------------------------

   function Python_Console_Completion_Handler
     (Input     : String;
      User_Data : System.Address) return String_List_Utils.String_List.List
   is
      use String_List_Utils.String_List;
      pragma Unreferenced (User_Data);
      List        : String_List_Utils.String_List.List;
      Start       : Natural := Input'First - 1;
      Last        : Natural := Input'Last + 1;
      Obj, Item   : PyObject;
      Errors      : aliased Boolean;

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
         Obj := Run_Command
           (Python_Module_Id.Script.Interpreter,
            "__builtins__.dir(" & Input (Start + 1 .. Last - 1) & ")",
            Hide_Output => True, Errors => Errors'Unchecked_Access);

         if Obj = null then
            return Null_List;
         else
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
                     Prepend
                       (List, Input (Input'First .. Last - 1) & '.' & S);
                  end if;
               end;
            end loop;

            Py_DECREF (Obj);
            return List;
         end if;
      end if;
   end Python_Console_Completion_Handler;

   --------------------------------------
   -- Python_Console_Interrupt_Handler --
   --------------------------------------

   function Python_Console_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address) return Boolean
   is
      pragma Unreferenced (Console, User_Data);
   begin
      if In_Process (Python_Module_Id.Script.Interpreter) then
         PyErr_SetInterrupt;
         return True;
      else
         return False;
      end if;
   end Python_Console_Interrupt_Handler;

   ---------------------------
   -- Create_Python_Console --
   ---------------------------

   function Create_Python_Console
     (Script : access Python_Scripting_Record'Class;
      Kernel : Kernel_Handle) return MDI_Child
   is
      Console : Interactive_Console;
   begin
      Console := Create_Interactive_Console
        (Kernel              => Kernel,
         Title               => -"Python",
         Module              => Abstract_Module_ID (Python_Module_Id),
         History             => History_Key'("python_console"),
         Create_If_Not_Exist => True);
      Set_Default_Console
        (Script.Interpreter, Console, Display_Prompt => True);

      Set_Command_Handler
        (Console, Python_Console_Command_Handler'Access, System.Null_Address);
      Set_Completion_Handler
        (Console, Python_Console_Completion_Handler'Access);
      Set_Interrupt_Handler
        (Console, Python_Console_Interrupt_Handler'Access);

      return Find_MDI_Child (Get_MDI (Kernel), Console);
   end Create_Python_Console;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Script : Scripting_Language;
   begin
      if Node.Tag.all = "Python_Console" then
         Script := Lookup_Scripting_Language (User, Python_Language_Name);
         return Create_Python_Console (Python_Scripting (Script), User);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      N : Node_Ptr;
   begin
      --  We must test whether this is indeed a python-specific console, since
      --  it might happen that the default console is redirected elsewhere (for
      --  instance to the Messages window at the beginning)
      if Gtk_Widget (Widget) =
        Gtk_Widget (Get_Console (Python_Module_Id.Script.Interpreter))
        and then Get_Title (Find_MDI_Child (Get_MDI (User), Widget)) = "Python"
      then
         N := new Node;
         N.Tag := new String'("Python_Console");
         return N;
      end if;
      return null;
   end Save_Desktop;

   -------------------------
   -- Override_Default_IO --
   -------------------------

   procedure Override_Default_IO
     (Console : Interactive_Consoles.Interactive_Console)
   is
      Real_Console : Interactive_Console := Console;
      Inst         : Class_Instance;
      Cons         : PyObject := Py_None;
   begin
      if Python_Module_Id = null then
         return;
      end if;

      if Real_Console = null then
         Real_Console := Get_Console (Python_Module_Id.Script.Kernel);
      end if;

      if Real_Console /= null then
         Inst := Get_Instance (Python_Module_Id.Script, Real_Console);
         if Inst = No_Class_Instance then
            Inst := New_Instance
              (Python_Module_Id.Script,
               New_Class (Python_Module_Id.Script.Kernel, "Console"));
            Set_Data (Inst, Widget => GObject (Real_Console));
         end if;
         Cons := Python_Class_Instance (Get_CIR (Inst)).Data;
      end if;

      PyDict_SetItemString
        (PyModule_GetDict (PyImport_ImportModule ("sys")), "stdout", Cons);
      PyDict_SetItemString
        (PyModule_GetDict (PyImport_ImportModule ("sys")), "stderr", Cons);
      PyDict_SetItemString
        (PyModule_GetDict (PyImport_ImportModule ("sys")), "stdin", Cons);
   end Override_Default_IO;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Ignored : Integer;
      Result  : PyObject;
      Tmp     : Boolean;
      pragma Unreferenced (Ignored, Tmp);
      Errors  : aliased Boolean;

      procedure Init_PyGtk;
      pragma Import (C, Init_PyGtk, "ada_init_pygtk");

      function Build_With_PyGtk return Integer;
      pragma Import (C, Build_With_PyGtk, "ada_build_with_pygtk");

   begin
      Python_Module_Id := new Python_Module_Record;
      Register_Module
        (Module      => Module_ID (Python_Module_Id),
         Kernel      => Kernel,
         Module_Name => "Python");
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Python_Module_Id.Script := new Python_Scripting_Record;
      Python_Module_Id.Script.Kernel := Kernel_Handle (Kernel);
      Register_Scripting_Language (Kernel, Python_Module_Id.Script);

      Python_Module_Id.Script.Interpreter := new Python_Interpreter_Record;
      Initialize (Python_Module_Id.Script.Interpreter);

      --  Create the GPS module, in which all functions and classes are
      --  registered

      Python_Module_Id.Script.GPS_Module := Py_InitModule
        (GPS_Module_Name);

      --  Register functions used to provide support for hiding the output
      --  of commands

      Tmp := PyRun_SimpleString
        ("def __gps_no_write (*args): pass" & ASCII.LF
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
         & ASCII.LF);

      Result := Run_Command
        (Python_Module_Id.Script.Interpreter,
         "import GPS", Hide_Output => True,
         Errors => Errors'Unchecked_Access);
      Assert (Me, Result /= null, "Couldn't import GPS module");

      Set_Default_Console
        (Python_Module_Id.Script.Interpreter,
         Get_Console (Kernel),
         Display_Prompt => False);

      Python_Module_Id.Script.GPS_Unexpected_Exception := PyErr_NewException
        (GPS_Module_Name & ".Unexpected_Exception", null, null);
      Python_Module_Id.Script.GPS_Exception := PyErr_NewException
        (GPS_Module_Name & ".Exception", null, null);
      Python_Module_Id.Script.GPS_Missing_Args := PyErr_NewException
        (GPS_Module_Name & ".Missing_Arguments", null, null);
      Python_Module_Id.Script.GPS_Invalid_Arg := PyErr_NewException
        (GPS_Module_Name & ".Invalid_Argument", null, null);

      Register_Menu
        (Kernel,
         Parent_Path => "/" & (-"_Tools") & '/' & (-"Consoles"),
         Text        => -"_Python",
         Callback    => Open_Python_Console'Access);

      --  PyGTK prints its error messages using sys.argv, which doesn't
      --  exist in non-interactive mode. We therefore define it here
      Result := Run_Command
        (Python_Module_Id.Script.Interpreter,
         "sys.argv=['GPS']", Hide_Output => True,
         Errors         => Errors'Unchecked_Access);

      --  If PyGtk is available, register some special functions, so that
      --  users can interact directly with widgets

      if Build_With_PyGtk = 1 then
         Result := Run_Command
           (Python_Module_Id.Script.Interpreter,
            "import pygtk", Hide_Output => True,
            Errors => Errors'Unchecked_Access);
      else
         --  Since we were not build with pygtk, don't even try to activate the
         --  special support for it
         Errors := True;
      end if;

      if not Errors then
         Trace (Me, "Loading support for pygtk");
         Result := Run_Command
           (Python_Module_Id.Script.Interpreter,
            "pygtk.require('2.0'); import gtk", Hide_Output => True,
            Errors         => Errors'Unchecked_Access);
         if Result = null then
            Trace (Me, "Couldn't initialize gtk");
         else
            Init_PyGtk;

            Register_Command
              (Python_Module_Id.Script,
               Command      => "pywidget",
               Handler      => Python_GUI_Command_Handler'Access,
               Class        => Get_GUI_Class (Kernel));
            Register_Command
              (Python_Module_Id.Script,
               Command       => "add",
               Handler       => Python_GUI_Command_Handler'Access,
               Class         => New_Class (Kernel, "MDI"),
               Minimum_Args  => 1,
               Maximum_Args  => 3,
               Static_Method => True);
         end if;

      else
         Trace (Me, "Not loading support for pygtk");
      end if;

      --  This function is required for support of the Python menu (F120-025),
      --  so that we can execute python commands in the context of the global
      --  interpreter instead of the current context (for the menu, that would
      --  be python_support.py, and thus would have no impact on the
      --  interpreter itself)
      Register_Command
        (Python_Module_Id.Script,
         Command       => "exec_in_console",
         Handler       => Python_Global_Command_Handler'Access,
         Minimum_Args  => 1,
         Maximum_Args  => 1);

      --  Change the screen representation of the various classes. This way,
      --  commands can return classes, but still displayed user-readable
      --  strings.
      --  Also make sure these can be used as keys in dictionaries.

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
   end Register_Module;

   --------------
   -- Load_Dir --
   --------------

   procedure Load_Dir
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Dir              : String;
      Default_Autoload : Boolean)
   is
      D       : Dir_Type;
      File    : String (1 .. 1024);
      Last    : Natural;
      Result  : PyObject;
      pragma Unreferenced (Result);
      VF      : VFS.Virtual_File;
      Command : Custom_Command_Access;
      Errors  : aliased Boolean;
   begin
      if not GNAT.OS_Lib.Is_Directory (Dir) then
         return;
      end if;

      Trace (Me, "Load python files from " & Dir);

      Result := Run_Command
        (Python_Module_Id.Script.Interpreter,
         "sys.path=[r'" & Dir & "']+sys.path",
         Hide_Output => True,
         Errors      => Errors'Unchecked_Access);

      Open (D, Dir);

      loop
         Read (D, File, Last);
         exit when Last = 0;

         if Last > 3 and then File (Last - 2 .. Last) = ".py" then
            VF := Create
              (Full_Filename => Name_As_Directory (Dir)
               & File (1 .. Last));
            if Load_File_At_Startup
              (Kernel, VF, Default => Default_Autoload)
            then
               Trace (Me, "Loading " & Full_Name (VF).all);
               Python_Module_Id.Script.Current_File := VF;
               Execute_Command
                 (Python_Module_Id.Script,
                  "import " & Base_Name (File (1 .. Last), ".py"),
                  Hide_Output => True,
                  Errors      => Errors);
               Python_Module_Id.Script.Current_File := VFS.No_File;

               Command := Initialization_Command (Kernel, VF);
               if Command /= null then
                  Launch_Background_Command
                    (Kernel,
                     Command    => Command,
                     Active     => True,
                     Show_Bar   => False,
                     Block_Exit => False);
               end if;
            end if;
         end if;
      end loop;

      Close (D);
   end Load_Dir;

   --------------------------------------
   -- Load_System_Python_Startup_Files --
   --------------------------------------

   procedure Load_System_Python_Startup_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Env_Path : constant String := Get_Custom_Path;
      Path     : Path_Iterator;
   begin
      if Python_Module_Id = null then
         return;
      end if;

      Load_Dir
        (Kernel, Autoload_System_Dir (Kernel), Default_Autoload => True);
      Load_Dir
        (Kernel, No_Autoload_System_Dir (Kernel), Default_Autoload => False);

      Path := Start (Env_Path);
      while not At_End (Env_Path, Path) loop
         if Current (Env_Path, Path) /= "" then
            Load_Dir
              (Kernel, Current (Env_Path, Path), Default_Autoload => True);
         end if;
         Path := Next (Env_Path, Path);
      end loop;
   end Load_System_Python_Startup_Files;

   ------------------------------------
   -- Load_User_Python_Startup_Files --
   ------------------------------------

   procedure Load_User_Python_Startup_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      if Python_Module_Id = null then
         return;
      end if;
      Load_Dir (Kernel, Autoload_User_Dir (Kernel), Default_Autoload => True);
   end Load_User_Python_Startup_Files;

   ---------------------------------
   -- Python_File_Command_Handler --
   ---------------------------------

   procedure Python_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle  := Get_Kernel (Data);
      Instance : constant Class_Instance :=
                   Nth_Arg (Data, 1, Get_File_Class (Kernel));
      Info     : constant Virtual_File := Get_Data (Instance);
   begin
      if Command = "__str__" or else Command = "__repr__" then
         Set_Return_Value (Data, Full_Name (Info).all);

      elsif Command = "__cmp__" then
         declare
            Inst2 : constant Class_Instance := Nth_Arg
              (Data, 2, Get_File_Class (Kernel));
            Name  : constant String := Full_Name (Info).all;
            Name2 : constant String := Full_Name (Get_Data (Inst2)).all;
         begin
            if Name < Name2 then
               Set_Return_Value (Data, -1);
            elsif Name = Name2 then
               Set_Return_Value (Data, 0);
            else
               Set_Return_Value (Data, 1);
            end if;
         end;

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer (Hash (Full_Name (Info).all)));
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing a File with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_File_Command_Handler;

   --------------------------------
   -- Python_GUI_Command_Handler --
   --------------------------------

   procedure Python_GUI_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      function PyObject_From_Widget (W : System.Address) return PyObject;
      pragma Import (C, PyObject_From_Widget, "ada_pyobject_from_widget");

      function Widget_From_PyObject (Object : PyObject) return System.Address;
      pragma Import (C, Widget_From_PyObject, "ada_widget_from_pyobject");

      Stub     : Gtk.Widget.Gtk_Widget_Record;
      Widget   : Glib.Object.GObject;
      Instance : Class_Instance;
      Child    : MDI_Child;
      Object   : GObject;
   begin
      --  This is only called when pygtk has been loaded properly. If pygtk
      --  is not available, they are not even visible to the user, so we do
      --  not need any special test here.

      if Command = "pywidget" then
         Instance := Nth_Arg (Data, 1, Get_GUI_Class (Get_Kernel (Data)));
         Object := Get_Data (Instance, GUI_Class_Name);
         if Object = null then
            Python_Callback_Data (Data).Return_Value := Py_None;
            Py_INCREF (Python_Callback_Data (Data).Return_Value);
         else
            Python_Callback_Data (Data).Return_Value := PyObject_From_Widget
              (Get_Object (Object));
         end if;

      elsif Command = "add" then
         Widget := Get_User_Data
           (Widget_From_PyObject (Get_Param (Python_Callback_Data (Data), 1)),
            Stub);
         Gtk_New (Child, Gtk_Widget (Widget), Group => Group_Default);
         Set_Title (Child, Nth_Arg (Data, 2, ""), Nth_Arg (Data, 3, ""));
         Put (Get_MDI (Get_Kernel (Data)), Child,
              Initial_Position => Position_Automatic);
         Set_Focus_Child (Child);
      end if;
   end Python_GUI_Command_Handler;

   -----------------------------------
   -- Python_Global_Command_Handler --
   -----------------------------------

   procedure Python_Global_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Result   : PyObject;
      pragma Unreferenced (Result);
      Errors   : aliased Boolean;
   begin
      if Command = "exec_in_console" then
         Result := Run_Command
           (Python_Module_Id.Script.Interpreter, Nth_Arg (Data, 1),
            Show_Command => True, Errors => Errors'Unchecked_Access);
      end if;
   end Python_Global_Command_Handler;

   ------------------------------------
   -- Python_Project_Command_Handler --
   ------------------------------------

   procedure Python_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Project : constant Project_Type := Get_Data (Data, 1);
   begin
      if Command = "__str__" then
         Set_Return_Value (Data, Project_Name (Project));

      elsif Command = "__repr__" then
         Set_Return_Value (Data, Full_Name (Project_Path (Project)).all);

      elsif Command = "__cmp__" then
         declare
            Project2 : constant Project_Type := Get_Data (Data, 2);
            Name  : constant Virtual_File := Project_Path (Project);
            Name2 : constant Virtual_File := Project_Path (Project2);
         begin
            if Name < Name2 then
               Set_Return_Value (Data, -1);
            elsif Name = Name2 then
               Set_Return_Value (Data, 0);
            else
               Set_Return_Value (Data, 1);
            end if;
         end;

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer (Hash (Full_Name (Project_Path (Project)).all)));
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing a Project with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_Project_Command_Handler;

   -----------------------------------
   -- Python_Entity_Command_Handler --
   -----------------------------------

   procedure Python_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Entity  : constant Entity_Information := Get_Data (Data, 1);
      Entity2 : Entity_Information;
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         if Is_Predefined_Entity (Entity) then
            Set_Return_Value (Data, Get_Name (Entity).all);
         else
            Set_Return_Value
              (Data,
               Get_Name (Entity).all & ':'
               & Base_Name (Get_Filename
                              (Get_File (Get_Declaration_Of (Entity))))
               & ':'
               & Image (Get_Line (Get_Declaration_Of (Entity))) & ':'
               & Image (Integer (Get_Column (Get_Declaration_Of (Entity)))));
         end if;

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (Get_Name (Entity).all
                   & Full_Name (Get_Filename
                       (Get_File (Get_Declaration_Of (Entity)))).all
                   & Image (Get_Line (Get_Declaration_Of (Entity)))
               & Image
                 (Integer (Get_Column (Get_Declaration_Of (Entity)))))));

      elsif Command = "__cmp__" then
         Entity2 := Get_Data (Data, 2);
         if Entity = null then
            if Entity2 = null then
               Set_Return_Value (Data, 0);
            else
               Set_Return_Value (Data, -1);
            end if;
         elsif Entity2 = null then
            Set_Return_Value (Data, 1);
         else
            declare
               Name1 : constant String := Get_Name (Entity).all;
               Name2 : constant String := Get_Name (Entity2).all;
            begin
               if Name1 < Name2 then
                  Set_Return_Value (Data, -1);
               elsif Name1 = Name2 then
                  declare
                     File1 : constant Virtual_File := Get_Filename
                       (Get_File (Get_Declaration_Of (Entity)));
                     File2 : constant Virtual_File := Get_Filename
                       (Get_File (Get_Declaration_Of (Entity)));
                  begin
                     if File1 < File2 then
                        Set_Return_Value (Data, -1);
                     elsif File1 = File2 then
                        Set_Return_Value (Data, 0);
                     else
                        Set_Return_Value (Data, 1);
                     end if;
                  end;
               else
                  Set_Return_Value (Data, 1);
               end if;
            end;
         end if;
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing an Entity with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_Entity_Command_Handler;

   -------------------------------------
   -- Python_Location_Command_Handler --
   -------------------------------------

   procedure Python_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Info     : constant File_Location_Info := Get_Data (Data, 1);
      Fileinfo : constant Virtual_File := Get_Data (Get_File (Info));
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         Set_Return_Value
           (Data,
            Base_Name (Fileinfo) & ':'
            & Image (Get_Line (Info)) & ':'
            & Image (Integer (Get_Column (Info))));

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (Full_Name (Fileinfo).all
                   & Image (Get_Line (Info))
                   & Image (Integer (Get_Column (Info))))));

      elsif Command = "__cmp__" then
         declare
            use Basic_Types;

            Info2 : constant File_Location_Info := Get_Data (Data, 2);
            Fileinfo2 : constant Virtual_File := Get_Data (Get_File (Info2));
            Line1, Line2 : Integer;
            Col1,  Col2  : Visible_Column_Type;
            Name1 : constant String := Full_Name (Fileinfo).all;
            Name2 : constant String := Full_Name (Fileinfo2).all;
         begin
            if Name1 < Name2 then
               Set_Return_Value (Data, -1);
            elsif Name1 = Name2 then
               Line1 := Get_Line (Info);
               Line2 := Get_Line (Info2);

               if Line1 < Line2 then
                  Set_Return_Value (Data, -1);

               elsif Line1 = Line2 then
                  Col1 := Get_Column (Info);
                  Col2 := Get_Column (Info2);

                  if Col1 < Col2 then
                     Set_Return_Value (Data, -1);
                  elsif Col1 = Col2 then
                     Set_Return_Value (Data, 0);
                  else
                     Set_Return_Value (Data, 1);
                  end if;

               else
                  Set_Return_Value (Data, 1);
               end if;
            else
               Set_Return_Value (Data, 1);
            end if;
         end;
      end if;

   exception
      when Invalid_Parameter =>
         if Command = "__cmp__" then
            --  We are comparing a File_Location with something else
            Set_Return_Value (Data, -1);
         else
            raise;
         end if;
   end Python_Location_Command_Handler;

   -------------------------
   -- Open_Python_Console --
   -------------------------

   procedure Open_Python_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child : MDI_Child;
      pragma Unreferenced (Widget, Child);
   begin
      Child := Create_Python_Console (Python_Module_Id.Script, Kernel);
   end Open_Python_Console;

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
            PyErr_SetString (Handler.Script.GPS_Missing_Args,
                             "Wrong number of parameters, expecting at least"
                             & Handler.Minimum_Args'Img);
         else
            PyErr_SetString (Handler.Script.GPS_Missing_Args,
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
         Trace (Me, "Raised invalid_parameter");

         if not Callback.Has_Return_Value
           or else Callback.Return_Value /= null
         then
            PyErr_SetString
              (Handler.Script.GPS_Invalid_Arg, Exception_Message (E));
         end if;

         Free (Callback);
         return null;

      when E : others =>
         Trace (Exception_Handle, E);

         if not Callback.Has_Return_Value
           or else  Callback.Return_Value /= null
         then
            PyErr_SetString (Handler.Script.GPS_Unexpected_Exception,
                             -"unexpected exception in GPS");
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
           (Module => Script.GPS_Module,
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

         Klass := Lookup_Class_Object (Script.GPS_Module, Get_Name (Class));

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
        (Dict, "__module__", PyString_FromString (GPS_Module_Name));

      if Base /= No_Class then
         Bases := Create_Tuple
           ((1 => Lookup_Class_Object (Script.GPS_Module, Get_Name (Base))));
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
      Ignored := PyModule_AddObject (Script.GPS_Module, S, Class);
      Free (S);
   end Register_Class;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script       : access Python_Scripting_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean)
   is
      E      : aliased Boolean;
      Result : PyObject;
   begin
      if Script.Blocked then
         Errors := True;
         Insert (Script.Kernel, "A command is already executing");
      else
         Result := Run_Command
           (Script.Interpreter, Command,
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
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String
   is
      pragma Unreferenced (Show_Command);
   begin
      if Script.Blocked then
         Errors.all := True;
         Insert (Script.Kernel, "A command is already executing");
         return "";
      else
         return Run_Command
           (Script.Interpreter, Command,
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
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean
   is
      Obj : PyObject;
      Result : Boolean;
   begin
      if Script.Blocked then
         Errors.all := True;
         Insert (Script.Kernel, "A command is already executing");
         return False;
      else
         Obj := Run_Command
           (Script.Interpreter, Command, Console, False, Hide_Output, Errors);
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
         Insert (Script.Kernel, "A command is already executing");
         return False;
      else
         Obj := Run_Command
           (Script.Interpreter,
            Command     => Command,
            Hide_Output => True,
            Errors      => Errors'Unchecked_Access);

         if Obj /= null and then PyFunction_Check (Obj) then
            return Execute_Command (Script, Obj, Args);
         else
            Trace (Me, Command & " is not a function");
            Insert
              (Script.Kernel,
               Command & (-" is not a function, when called from a hook"));
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
         Insert (Script.Kernel, "A python command is already executing");
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
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : out Boolean) is
   begin
      Script.Current_File := Create (Full_Filename => Filename);
      Execute_Command
        (Script, "execfile (r'" & Filename & "')",
         Console, Hide_Output, True, Errors);
      Script.Current_File := VFS.No_File;
   end Execute_File;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Script : access Python_Scripting_Record) return String is
      pragma Unreferenced (Script);
   begin
      return Python_Language_Name;
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

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Script : access Python_Scripting_Record)
      return Kernel_Handle is
   begin
      return Script.Kernel;
   end Get_Kernel;

   --------------------
   -- Current_Script --
   --------------------

   function Current_Script
     (Script : access Python_Scripting_Record) return String
   is
   begin
      if Script.Current_File = VFS.No_File then
         return "<python script>";
      else
         return Full_Name (Script.Current_File).all;
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
              (Data, -("Parameter cannot be both positional "
                       & " and named: ") & Names (Index + Names'First).all);
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
                    (Data, -"Invalid keyword parameter: " & S);
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
         Trace (Me, "Keyword parameters not supported");
         PyErr_SetString
           (Data.Script.GPS_Exception,
            -"Keyword parameters not supported");
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
      C          : constant PyObject := Lookup_Class_Object
        (Data.Script.GPS_Module, Get_Name (Class));
      Item_Class : PyObject;

   begin
      Item := Get_Param (Data, N);  --  Item is a borrowed reference

      if not PyInstance_Check (Item) then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class));
      end if;

      Item_Class := PyObject_GetAttrString (Item, "__class__");
      --  Item_Class must be DECREF'd

      if Item_Class = null then
         Trace (Me, "Nth_Arg: Couldn't find class of instance");
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class) & " but has no __class__");
      end if;

      if C = null or else not PyClass_IsSubclass (Item_Class, Base => C) then
         Py_DECREF (Item_Class);
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class));
      end if;

      Py_DECREF (Item_Class);
      Trace (Ref, "Nth_Arg: getting the class instance");
      return Get_CI (Item);

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
      if Active (Ref) then
         Trace (Ref, "Before Py.Decref " & Print_Refcount (Inst));
      end if;
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
      if Active (Ref) then
         Trace (Ref, "On_PyObject_Data_Destroy " & Print_Refcount (D));
      end if;

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

   function Get_CI (Object : PyObject) return Class_Instance is
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

         if Active (Ref) then
            Trace (Ref, "Get_CI: creating new instance for "
                   & Value (Refcount_Msg (Object)));
         end if;
         CI := new Python_Class_Instance_Record;
         CI.Data := Object;
         Result := From_Instance (Python_Module_Id.Script, CI);
         Set_CI (Result);  --  Associate with Object for the future
         Decref (Get_CIR (Result));  --  Since From_Instance incremented it
         if Active (Ref) then
            Trace (Ref, "After Get_CI "
                   & Print_Refcount (Get_CIR (Result)));
         end if;
         return Result;

      elsif not PyCObject_Check (Item) then
         return No_Class_Instance;

      else
         if Active (Ref) then
            Trace (Ref, "Get_CI: reusing instance for "
                   & Value (Refcount_Msg (Object)));
         end if;
         CIR := PyCObject_AsVoidPtr (Item);
         Py_DECREF (Item);

         if Python_Module_Id /= null then
            Result := From_Instance
              (Python_Module_Id.Script, Convert (CIR));
            if Active (Ref) then
               Trace
                 (Ref, "After Get_CI " & Print_Refcount (Get_CIR (Result)));
            end if;
         end if;
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
        (Python_Module_Id.Script.GPS_Module, Get_Name (Base));
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
         Trace (Me, "Set_Error_Msg: " & Msg);
         PyErr_SetString (Data.Script.GPS_Exception, Msg);
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
        (Script.GPS_Module, Get_Name (Class));
      Inst : Class_Instance;
   begin
      if Active (Ref) then
         Trace (Ref, "New_Instance of " & Get_Name (Class));
      end if;
      if Klass = null then
         return No_Class_Instance;
      end if;

      Inst := Get_CI (PyInstance_NewRaw (Klass, null));

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
           (Subprogram_Record with Subprogram => Item);
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
        (Script  => Python_Module_Id.Script,
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
        (Script => Python_Module_Id.Script,
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
        (Script => Python_Module_Id.Script,
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
      pragma Unreferenced (Subprogram);
   begin
      if Python_Module_Id = null then
         return null;
      else
         return Scripting_Language (Python_Module_Id.Script);
      end if;
   end Get_Script;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Python_Module_Record) is
   begin
      Python_Module_Id := null;
      Module.Script := null;
   end Destroy;

end Python_Module;
