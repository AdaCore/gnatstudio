-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2004                      --
--                            ACT-Europe                             --
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

with Glib.Object;              use Glib.Object;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.MDI;               use Gtkada.MDI;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Histories;                use Histories;
with Python.GUI;               use Python, Python.GUI;
with Python.Ada;               use Python.Ada;
with Glide_Intl;               use Glide_Intl;
with Interfaces.C.Strings;     use Interfaces.C, Interfaces.C.Strings;
with Interactive_Consoles;     use Interactive_Consoles;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;           use Ada.Exceptions;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with System;                   use System;
with Traces;                   use Traces;
with String_Utils;             use String_Utils;
with String_List_Utils;        use String_List_Utils;
with Projects;                 use Projects;
with Entities;                 use Entities;
with VFS;                      use VFS;
with Interactive_Consoles;
with HTables;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Python_Module is

   Me : constant Debug_Handle := Create ("Python_Module");

   GPS_Module_Name : constant String := "GPS";
   --  Name of the GPS module in the python interpreter.

   GPS_Data_Attr : constant String := "__gps_data__";
   --  Internal name of the attributes reserved for GPS

   Python_Language_Name : constant String := "Python";

   type Hash_Index is range 0 .. 100000;
   function Hash is new HTables.Hash (Hash_Index);

   ----------------------
   -- Python_scripting --
   ----------------------

   type Python_Scripting_Record is new Scripting_Language_Record with record
      Kernel                   : Glide_Kernel.Kernel_Handle;
      Interpreter              : Python_Interpreter;
      GPS_Exception            : PyObject;
      GPS_Module               : PyObject;
      GPS_Missing_Args         : PyObject;
      GPS_Invalid_Arg          : PyObject;
      GPS_Unexpected_Exception : PyObject;
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
   procedure Register_Command
     (Script        : access Python_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   procedure Register_Class
     (Script        : access Python_Scripting_Record;
      Name          : String;
      Base          : Class_Type := No_Class);
   procedure Execute_Command
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Show_Command       : Boolean := True;
      Errors             : out Boolean);
   function Execute_Command
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Show_Command       : Boolean := True;
      Errors             : access Boolean) return String;
   function Execute_Command
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Errors             : access Boolean) return Boolean;
   function Execute_Command
     (Script  : access Python_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   function Execute_Command
     (Command : PyObject;
      Args    : Callback_Data'Class) return Boolean;
   procedure Execute_File
     (Script             : access Python_Scripting_Record;
      Filename           : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Errors             : out Boolean);
   function Get_Name (Script : access Python_Scripting_Record) return String;
   function Is_Subclass
     (Script : access Python_Scripting_Record;
      Instance : access Class_Instance_Record'Class;
      Base   : Class_Type) return Boolean;
   function Get_Kernel
     (Script : access Python_Scripting_Record) return Kernel_Handle;
   --  See doc from inherited subprograms

   ------------------------
   -- Python_Subprograms --
   ------------------------

   type Python_Subprogram_Record is new Subprogram_Record with record
      Subprogram : PyObject;
   end record;

   function Execute
     (Subprogram : access Python_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean;
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
     (Data : Python_Callback_Data; N : Positive) return System.Address;
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
     (Data   : in out Python_Callback_Data; Value : System.Address);
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
      Script  : Python_Scripting;
      Counter : Natural;
      Data    : PyObject;
   end record;
   type Python_Class_Instance is access all Python_Class_Instance_Record'Class;
   --  Counter is the reference counter for the Python_Class_Instance, not for
   --  the python object Data itself.
   --  Both do not have the same value: Python itself can do its own ref/decref
   --  internally, and we are not aware of these. On the other hand, Nth_Arg
   --  will return a newly allocated pointer, which needs to be freed when
   --  Free is called, even though the pyhon object itself might still exists.

   function New_Instance
     (Script : access Python_Scripting_Record;
      Class : Class_Type) return Class_Instance;
   function Get_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type) return String;
   function Get_Data
     (Instance : access Python_Class_Instance_Record; Class : Class_Type)
      return System.Address;
   function Get_Data
     (Instance : access Python_Class_Instance_Record; Class : Class_Type)
      return Integer;
   procedure Set_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type;
      Value    : String);
   procedure Set_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type;
      Value    : Integer);
   procedure Set_Data
     (Instance   : access Python_Class_Instance_Record;
      Class      : Class_Type;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null);
   function Get_Script (Instance : access Python_Class_Instance_Record)
      return Scripting_Language;
   procedure Primitive_Free
     (Instance     : in out Python_Class_Instance_Record;
      Free_Pointer : out Boolean);
   procedure Ref (Instance : access Python_Class_Instance_Record);
   --  See doc from inherited subprogram

   ------------------
   -- Handler_Data --
   ------------------

   type Handler_Data (Length : Natural) is record
      Script  : Python_Scripting;
      Handler : Module_Command_Function;
      Command : String (1 .. Length);
      Minimum_Args, Maximum_Args : Natural;
      Is_Method : Boolean := False;
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

   procedure Python_Console_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address);
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
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Node_Ptr;

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Support functions for saving the desktop

   procedure Python_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   procedure Python_Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands related to the various classes

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
      pragma Unreferenced (Result, User_Data);
   begin
      Result := Run_Command
        (Interpreter  => Python_Module_Id.Script.Interpreter,
         Command      => Input,
         Console      => null,
         Show_Command => False,
         Hide_Output  => False,
         Errors       => Errors'Unrestricted_Access);

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
            Hide_Output => True, Errors => Errors'Unrestricted_Access);

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

   procedure Python_Console_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address)
   is
      pragma Unreferenced (Console, User_Data);
   begin
      --      if Python_Module_Id.Script.Interpreter.In_Process then
      PyErr_SetInterrupt;
--      end if;
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
         Module              => Module_ID (Python_Module_Id),
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
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Gtk_Widget (Widget) =
        Gtk_Widget (Get_Console (Python_Module_Id.Script.Interpreter))
      then
         N := new Node;
         N.Tag := new String'("Python_Console");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Ignored : Integer;
      Result  : PyObject;
      pragma Unreferenced (Ignored, Result);
      N       : Node_Ptr;
      Errors  : aliased Boolean;

   begin
      Python_Module_Id := new Python_Module_Record;
      Register_Module
        (Module      => Module_ID (Python_Module_Id),
         Kernel      => Kernel,
         Module_Name => "Python");
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Python_Module_Id.Script := new Python_Scripting_Record;
      Python_Module_Id.Script.Kernel := Kernel_Handle (Kernel);
      Register_Scripting_Language (Kernel, Python_Module_Id.Script);

      Python_Module_Id.Script.Interpreter := new Python_Interpreter_Record;
      Initialize (Python_Module_Id.Script.Interpreter);

      Set_Default_Console
        (Python_Module_Id.Script.Interpreter,
         Get_Console (Kernel),
         Display_Prompt => False);

      N     := new Node;
      N.Tag := new String'("Python_Console");
      Add_Default_Desktop_Item
        (Kernel, N,
         10, 10,
         400, 100,
         "Python", "Python Console",
         Docked, Bottom,
         Focus => False, Raised => False);

      --  Create the GPS module, in which all functions and classes are
      --  registered

      Python_Module_Id.Script.GPS_Module := Py_InitModule
        (GPS_Module_Name, Doc => "Interface with the GPS environment");
      Result := Run_Command
        (Python_Module_Id.Script.Interpreter,
         "import GPS", Hide_Output => True,
         Errors => Errors'Unrestricted_Access);
      Initialize_IO (Python_Module_Id.Script.Interpreter,
                     GPS_Module_Name,
                     Python_Module_Id.Script.GPS_Module);

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
         Parent_Path => "/" & (-"Tools"),
         Text        => -"Python Console",
         Callback    => Open_Python_Console'Access);

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

   -------------------------------
   -- Load_Python_Startup_Files --
   -------------------------------

   procedure Load_Python_Startup_Files
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Sys    : constant String := Get_System_Dir (Kernel) & "share/gps/python";
      Dir    : constant String := Get_Home_Dir (Kernel) & "python_startup";
      D      : Dir_Type;
      File   : String (1 .. 1024);
      Last   : Natural;
      Errors : aliased Boolean;
      Result : PyObject;
      pragma Unreferenced (Result);

   begin
      if Python_Module_Id = null then
         return;
      end if;

      if Is_Regular_File (Sys & "/autoexec.py") then
         Trace (Me, "Load python files from " & Sys & "/autoexec.py");

         Execute_Command
           (Python_Module_Id.Script,
            "execfile (""" & Sys & "/autoexec.py"")",
            Hide_Output => True,
            Errors => Errors);
      end if;

      if Is_Directory (Dir) then
         Trace (Me, "Load python files from " & Dir);

         Result := Run_Command
           (Python_Module_Id.Script.Interpreter,
            "sys.path=[r'" & Dir & "']+sys.path",
            Hide_Output => True,
            Errors      => Errors'Unrestricted_Access);

         Open (D, Dir);

         loop
            Read (D, File, Last);
            exit when Last = 0;

            if Last > 3 and then File (Last - 2 .. Last) = ".py" then
               Execute_Command
                 (Python_Module_Id.Script,
                  "import " & Base_Name (File (1 .. Last), ".py"),
                  Hide_Output => True,
                  Errors => Errors);

               --  The python console is not created yet, so we only want to
               --  redirect error message to the Messages window.
               if Errors then
                  Execute_Command
                    (Python_Module_Id.Script,
                     "import " & Base_Name (File (1 .. Last), ".py")
                     & "; reload (" & Base_Name (File (1 .. Last), ".py")
                     & ")",
                     Hide_Output => False,
                     Errors => Errors);
               end if;
            end if;
         end loop;

         Close (D);
      else
         Make_Dir (Dir);
      end if;
   end Load_Python_Startup_Files;

   ---------------------------------
   -- Python_File_Command_Handler --
   ---------------------------------

   procedure Python_File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle  := Get_Kernel (Data);
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_File_Class (Kernel));
      Info     : constant File_Info := Get_Data (Instance);
   begin
      if Command = "__str__" or else Command = "__repr__" then
         Set_Return_Value (Data, Full_Name (Get_File (Info)).all);

      elsif Command = "__cmp__" then
         declare
            Inst2 : constant Class_Instance := Nth_Arg
              (Data, 2, Get_File_Class (Kernel));
            Info2 : constant File_Info := Get_Data (Inst2);
            Name  : constant String := Full_Name (Get_File (Info)).all;
            Name2 : constant String := Full_Name (Get_File (Info2)).all;
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
           (Data, Integer (Hash (Full_Name (Get_File (Info)).all)));
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

   ------------------------------------
   -- Python_Project_Command_Handler --
   ------------------------------------

   procedure Python_Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Project  : constant Project_Type := Get_Data (Data, 1);
   begin
      if Command = "__str__" then
         Set_Return_Value (Data, Project_Name (Project));

      elsif Command = "__repr__" then
         Set_Return_Value (Data, Project_Path (Project));

      elsif Command = "__cmp__" then
         declare
            Project2 : constant Project_Type := Get_Data (Data, 2);
            Name  : constant String := Project_Path (Project);
            Name2 : constant String := Project_Path (Project2);
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
         Set_Return_Value (Data, Integer (Hash (Project_Path (Project))));
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
      Entity : constant Entity_Information := Get_Data (Data, 1);
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
               & Image (Get_Column (Get_Declaration_Of (Entity))));
         end if;

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (Get_Name (Entity).all
                   & Full_Name (Get_Filename
                       (Get_File (Get_Declaration_Of (Entity)))).all
                   & Image (Get_Line (Get_Declaration_Of (Entity)))
                   & Image (Get_Column (Get_Declaration_Of (Entity))))));

      elsif Command = "__cmp__" then
         declare
            Entity2 : constant Entity_Information := Get_Data (Data, 2);
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
      Fileinfo : constant File_Info := Get_Data (Get_File (Info));
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         Set_Return_Value (Data,
                           Base_Name (Get_File (Fileinfo)) & ':'
                           & Image (Get_Line (Info)) & ':'
                           & Image (Get_Column (Info)));

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (Full_Name (Get_File (Fileinfo)).all
                   & Image (Get_Line (Info))
                   & Image (Get_Column (Info)))));

      elsif Command = "__cmp__" then
         declare
            Info2 : constant File_Location_Info := Get_Data (Data, 2);
            Fileinfo2 : constant File_Info := Get_Data (Get_File (Info2));
            Line1, Line2 : Integer;
            Name1 : constant String := Full_Name (Get_File (Fileinfo)).all;
            Name2 : constant String := Full_Name (Get_File (Fileinfo2)).all;
         begin
            if Name1 < Name2 then
               Set_Return_Value (Data, -1);
            elsif Name1 = Name2 then
               Line1 := Get_Line (Info);
               Line2 := Get_Line (Info2);

               if Line1 < Line2 then
                  Set_Return_Value (Data, -1);

               elsif Line1 = Line2 then
                  Line1 := Get_Column (Info);
                  Line2 := Get_Column (Info2);

                  if Line1 < Line2 then
                     Set_Return_Value (Data, -1);
                  elsif Line1 = Line2 then
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
      Py_DECREF (Data.Args);

      if Data.Kw /= null then
         Py_DECREF (Data.Kw);
      end if;

      --  Do not free the return value, this is taken care of later on by all
      --  callers

      Unchecked_Free (Data.Kw_Params);
   end Free;

   ------------
   -- Create --
   ------------

   function Create
     (Script          : access Python_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class
   is
      Callback : Python_Callback_Data :=
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
     (Data : Python_Callback_Data; N : Positive; Value : Class_Instance) is
   begin
      PyTuple_SetItem (Data.Args, N - 1, Python_Class_Instance (Value).Data);
      Py_INCREF (Python_Class_Instance (Value).Data);
   end Set_Nth_Arg;

   -----------------
   -- First_Level --
   -----------------

   function First_Level (Self, Args, Kw : PyObject) return PyObject is
      Handler : constant Handler_Data_Access :=
        Convert (PyCObject_AsVoidPtr (Self));
      Size : Integer := PyTuple_Size (Args);
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
           or else  Callback.Return_Value /= null
         then
            PyErr_SetString
              (Handler.Script.GPS_Invalid_Arg, Exception_Message (E));
         end if;

         Free (Callback);
         return null;

      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));

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
      H   : constant Handler_Data_Access := new Handler_Data'
        (Length       => Command'Length,
         Command      => Command,
         Handler      => Handler,
         Script       => Python_Scripting (Script),
         Is_Method    => Class /= No_Class and then not Static_Method,
         Minimum_Args => Minimum_Args,
         Maximum_Args => Maximum_Args);
      User_Data : constant PyObject := PyCObject_FromVoidPtr
        (H.all'Address, Destroy_Handler_Data'Access);
      Klass : PyObject;
      Def   : PyMethodDef;
   begin
      if Class = No_Class then
         Add_Function
           (Module => Script.GPS_Module,
            Func   => Create_Method_Def (Command, First_Level'Access),
            Self   => User_Data);

      else
         if Command = Constructor_Method then
            Def := Create_Method_Def ("__init__", First_Level'Access);
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
      Dict  : constant PyDictObject := PyDict_New;
      Class : PyClassObject;
      Ignored : Integer;
      Bases   : PyObject := null;
      S       : chars_ptr;
      pragma Unreferenced (Ignored);
   begin
      PyDict_SetItemString
        (Dict, "__module__", PyString_FromString (GPS_Module_Name));

      if Base /= No_Class then
         Bases := Create_Tuple
           ((1 => Lookup_Class_Object (Script.GPS_Module, Get_Name (Base))));
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
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Show_Command       : Boolean := True;
      Errors             : out Boolean)
   is
      E : aliased Boolean;
      Result : PyObject;
      pragma Unreferenced (Result);
   begin
      Result := Run_Command
        (Script.Interpreter, Command,
         Console      => Console,
         Hide_Output  => Hide_Output,
         Show_Command => Show_Command,
         Errors       => E'Unrestricted_Access);
      Errors := E;
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
      return Run_Command
        (Script.Interpreter, Command,
         Console     => Console,
         Hide_Output => Hide_Output,
         Errors      => Errors);
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Errors             : access Boolean) return Boolean
   is
      Obj : PyObject;
   begin
      Obj := Run_Command
        (Script.Interpreter, Command, Console, False, Hide_Output, Errors);
      return Obj /= null
        and then ((PyInt_Check (Obj) and then PyInt_AsLong (Obj) = 1)
                  or else
                    (PyString_Check (Obj)
                     and then PyString_AsString (Obj) = "true"));
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
      Obj := Run_Command
        (Script.Interpreter,
         Command     => Command,
         Hide_Output => True,
         Errors      => Errors'Unrestricted_Access);

      if Obj /= null and then PyFunction_Check (Obj) then
         return Execute_Command (Obj, Args);
      else
         Trace (Me, Command & " is not a function");
         Insert (Script.Kernel,
                 Command & (-" is not a function, when called from a hook"));
         return False;
      end if;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Command : PyObject;
      Args    : Callback_Data'Class) return Boolean
   is
      Obj : PyObject;
   begin
      Obj := PyEval_EvalCodeEx
        (PyFunction_Get_Code (Command),
         Globals  => PyFunction_Get_Globals (Command),
         Locals   => null,
            Args     => Python_Callback_Data (Args).Args,
            Kwds     => Python_Callback_Data (Args).Kw,
            Defaults => PyFunction_Get_Defaults (Command),
            Closure  => PyFunction_Get_Closure (Command));
      if Obj = null then
         PyErr_Print;
      end if;

      return Obj /= null
        and then ((PyInt_Check (Obj) and then PyInt_AsLong (Obj) = 1)
                  or else
                    (PyString_Check (Obj)
                     and then PyString_AsString (Obj) = "true"));
   end Execute_Command;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File
     (Script             : access Python_Scripting_Record;
      Filename           : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Errors             : out Boolean) is
   begin
      Execute_Command
        (Script, "execfile (r'" & Filename & "')",
         Console, Hide_Output, True, Errors);
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

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Instance : access Python_Class_Instance_Record)
      return Scripting_Language is
   begin
      return Scripting_Language (Instance.Script);
   end Get_Script;

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
         Py_DECREF (Obj);
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
     (Data : Python_Callback_Data; N : Positive) return System.Address
   is
      Item : constant PyObject := Get_Param (Data, N);
   begin
      if not PyCObject_Check (Item) then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an address");
      else
         return PyCObject_AsVoidPtr (Item);
      end if;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance
   is
      Item : PyObject;
      C    : constant PyObject := Lookup_Class_Object
        (Data.Script.GPS_Module, Get_Name (Class));
      Item_Class : PyObject;
      Inst : Class_Instance;
   begin
      Item := Get_Param (Data, N);
      if not PyInstance_Check (Item) then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class));
      end if;

      Item_Class := PyObject_GetAttrString (Item, "__class__");
      if Item_Class = null then
         Trace (Me, "Nth_Arg: Couldn't find class of instance");
      end if;

      if not PyClass_IsSubclass (Item_Class, Base => C) then
         Raise_Exception
           (Invalid_Parameter'Identity,
            "Parameter" & Integer'Image (N) & " should be an instance of "
            & Get_Name (Class));
      end if;

      --  The following call doesn't modify the refcounf of Item.
      Inst := new Python_Class_Instance_Record'
        (Class_Instance_Record with
         Script  => Data.Script,
         Counter => 1,
         Data    => Item);

      Py_INCREF (Item);

      return Inst;

   exception
      when No_Such_Parameter =>
         if Allow_Null then
            return null;
         else
            raise;
         end if;
   end Nth_Arg;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Script   : access Python_Scripting_Record;
      Instance : access Class_Instance_Record'Class;
      Base     : Class_Type) return Boolean
   is
      C : constant PyObject := PyObject_GetAttrString
        (Python_Class_Instance (Instance).Data, "__class__");
      B : constant PyObject := Lookup_Class_Object
        (Script.GPS_Module, Get_Name (Base));
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
      Prepare_Value_Key (Data, Python_Class_Instance (Key).Data, Append);
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
     (Data : in out Python_Callback_Data; Value : System.Address)
   is
      Num : Integer;
      pragma Unreferenced (Num);
   begin
      if Data.Return_As_List then
         Num := PyList_Append
           (Data.Return_Value, PyCObject_FromVoidPtr (Value));
      else
         Setup_Return_Value (Data);
         Data.Return_Value := PyCObject_FromVoidPtr (Value);
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
      V   : constant Python_Class_Instance := Python_Class_Instance (Value);
      Num : Integer;
      pragma Unreferenced (Num);
   begin
      if Data.Return_As_List then
         Num := PyList_Append (Data.Return_Value, V.Data);
      else
         Setup_Return_Value (Data);
         Py_INCREF (V.Data);
         Data.Return_Value := V.Data;
      end if;
   end Set_Return_Value;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance
     (Script : access Python_Scripting_Record;
      Class : Class_Type) return Class_Instance
   is
      Klass : constant PyObject := Lookup_Class_Object
        (Script.GPS_Module, Get_Name (Class));
   begin
      if Klass = null then
         return null;
      end if;

      return new Python_Class_Instance_Record'
        (Class_Instance_Record with
         Script  => Python_Scripting (Script),
         Counter => 1,
         Data    => PyInstance_NewRaw (Klass, null));
   end New_Instance;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type) return Integer
   is
      Item : constant PyObject := PyObject_GetAttrString
        (Instance.Data, GPS_Data_Attr & Get_Name (Class));
   begin
      if Item = null or else not PyInt_Check (Item) then
         raise Invalid_Data;
      end if;
      return Integer (PyInt_AsLong (Item));
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type) return String
   is
      Item : constant PyObject := PyObject_GetAttrString
        (Instance.Data, GPS_Data_Attr & Get_Name (Class));
   begin
      if Item = null or else not PyString_Check (Item) then
         raise Invalid_Data;
      end if;
      return PyString_AsString (Item);
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type) return System.Address
   is
      Item : constant PyObject := PyObject_GetAttrString
        (Instance.Data, GPS_Data_Attr & Get_Name (Class));
      Result : System.Address;
   begin
      if Item = null or else not PyCObject_Check (Item) then
         raise Invalid_Data;
      end if;
      Result := PyCObject_AsVoidPtr (Item);
      Py_DECREF (Item);
      return Result;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type;
      Value    : Integer) is
   begin
      PyObject_SetAttrString
        (Instance.Data, GPS_Data_Attr & Get_Name (Class),
         PyInt_FromLong (long (Value)));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Python_Class_Instance_Record;
      Class    : Class_Type;
      Value    : String) is
   begin
      PyObject_SetAttrString
        (Instance.Data, GPS_Data_Attr & Get_Name (Class),
         PyString_FromString (Value));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance   : access Python_Class_Instance_Record;
      Class      : Class_Type;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null)
   is
      Data : constant PyObject := PyCObject_FromVoidPtr
        (Value, PyCObject_Destructor (On_Destroy));
   begin
      PyObject_SetAttrString
        (Instance.Data, GPS_Data_Attr & Get_Name (Class), Data);
      Py_DECREF (Data);
   end Set_Data;

   --------------------
   -- Primitive_Free --
   --------------------

   procedure Primitive_Free
     (Instance     : in out Python_Class_Instance_Record;
      Free_Pointer : out Boolean) is
   begin
      Py_DECREF (Instance.Data);
      Instance.Counter := Instance.Counter - 1;
      Free_Pointer := Instance.Counter = 0;
   end Primitive_Free;

   ---------
   -- Ref --
   ---------

   procedure Ref (Instance : access Python_Class_Instance_Record) is
   begin
      Instance.Counter := Instance.Counter + 1;
      Py_INCREF (Instance.Data);
   end Ref;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Subprogram_Type
   is
      Item : PyObject := Get_Param (Data, N);
   begin
      if Item /= null and then PyFunction_Check (Item) then
         Py_INCREF (Item);
         return new Python_Subprogram_Record'
           (Subprogram_Record with Subprogram => Item);

      elsif Item /= null and then PyMethod_Check (Item) then
         Item := PyMethod_Function (Item);
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
      Args       : Callback_Data'Class) return Boolean
   is
      Tmp : Boolean;
   begin
      Tmp := Execute_Command
        (Command => Subprogram.Subprogram,
         Args    => Args);
      return Tmp;
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
