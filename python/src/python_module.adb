-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.MDI;               use Gtkada.MDI;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Python.GUI;               use Python, Python.GUI;
with Python.Ada;               use Python.Ada;
with Glide_Intl;               use Glide_Intl;
with Gtk.Enums;                use Gtk.Enums;
with Interfaces.C.Strings;     use Interfaces.C, Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;           use Ada.Exceptions;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with System;                   use System;
with Traces;                   use Traces;
with String_Utils;             use String_Utils;
with Projects;                 use Projects;
with Src_Info.Queries;         use Src_Info.Queries;
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

   procedure Register_Command
     (Script        : access Python_Scripting_Record;
      Command       : String;
      Params        : String := "";
      Return_Value  : String := "";
      Description   : String := "";
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   procedure Register_Class
     (Script        : access Python_Scripting_Record;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class);
   procedure Execute_Command
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Display_In_Console : Boolean := True);
   procedure Execute_File
     (Script             : access Python_Scripting_Record;
      Filename           : String;
      Display_In_Console : Boolean := True);
   function Get_Name (Script : access Python_Scripting_Record) return String;
   function Is_Subclass
     (Script : access Python_Scripting_Record;
      Class  : Class_Type;
      Base   : Class_Type) return Boolean;
   function Get_Kernel (Script : access Python_Scripting_Record)
      return Kernel_Handle;
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
     (Data : Python_Callback_Data; N : Positive) return System.Address;
   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Class : Class_Type)
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

   procedure Free (Data : in out Python_Callback_Data'Class);
   --  Free the memory used by Data

   ---------------------------
   -- Python_Class_Instance --
   ---------------------------

   type Python_Class_Instance_Record is new Class_Instance_Record with record
      Script : Python_Scripting;
      Data   : PyObject;
   end record;
   type Python_Class_Instance is access all Python_Class_Instance_Record'Class;

   function New_Instance
     (Script : access Python_Scripting_Record;
      Class : Class_Type) return Class_Instance;
   function Get_Class (Instance : access Python_Class_Instance_Record)
      return Class_Type;
   function Get_Data (Instance : access Python_Class_Instance_Record)
      return Glib.Object.GObject;
   function Get_Data
     (Instance : access Python_Class_Instance_Record) return String;
   function Get_Data (Instance : access Python_Class_Instance_Record)
      return System.Address;
   function Get_Data (Instance : access Python_Class_Instance_Record)
      return Integer;
   procedure Set_Data
     (Instance : access Python_Class_Instance_Record;
      Value    : access Glib.Object.GObject_Record'Class);
   procedure Set_Data
     (Instance : access Python_Class_Instance_Record; Value : String);
   procedure Set_Data
     (Instance : access Python_Class_Instance_Record; Value : Integer);
   procedure Set_Data
     (Instance   : access Python_Class_Instance_Record;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null);
   function Get_Script (Instance : access Python_Class_Instance_Record)
      return Scripting_Language;
   procedure Primitive_Free (Instance : in out Python_Class_Instance_Record);
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

   type Interpreter_View_Record is new Gtk_Scrolled_Window_Record
     with null record;
   type Interpreter_View is access all Interpreter_View_Record'Class;

   function Create_Python_Console
     (Script : access Python_Scripting_Record'Class;
      Kernel : Kernel_Handle) return MDI_Child;
   --  Create the python console if it doesn't exist yet.

   function First_Level (Self, Args, Kw : PyObject) return PyObject;
   pragma Convention (C, First_Level);
   --  First level handler for all functions exported to python. This function
   --  is in charge of dispatching to the actual Ada subprogram.

   procedure Setup_Return_Value (Data : in out Python_Callback_Data'Class);
   --  Mark Data as containing a return value, and free the previous value if
   --  there is any

   function Convert is new Ada.Unchecked_Conversion (System.Address, GObject);
   function Convert is new Ada.Unchecked_Conversion (GObject, System.Address);

   procedure Unref_Gobject (Data : System.Address);
   pragma Convention (C, Unref_Gobject);
   --  Called when the GObject Data is no longer necessary in the python object
   --  it is associated with.

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

   ---------------------------
   -- Create_Python_Console --
   ---------------------------

   function Create_Python_Console
     (Script : access Python_Scripting_Record'Class;
      Kernel : Kernel_Handle) return MDI_Child
   is
      Child   : MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Interpreter_View_Record'Tag);
      Buffer : Gtk_Text_Buffer;
      View   : Gtk_Text_View;
      Console : Interpreter_View;
   begin
      if Child = null then
         Console := new Interpreter_View_Record;
         Gtk.Scrolled_Window.Initialize (Console);
         Set_Policy (Console, Policy_Automatic, Policy_Automatic);

         Gtk_New (Buffer);
         Gtk_New (View, Buffer);
         Add (Console, View);
         Modify_Font (View, Get_Pref (Kernel, Source_Editor_Font));
         Set_Wrap_Mode (View, Wrap_Char);

         Set_Console
           (Script.Interpreter, View, Grab_Widget => Gtk_Widget (Console));

         Child := Put
           (Kernel, Console,
            Focus_Widget        => Gtk_Widget (View),
            Module              => Python_Module_Id,
            Desktop_Independent => True);
         Set_Focus_Child (Child);
         Set_Title (Child, -"Python");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
      else
         Console := Interpreter_View (Get_Widget (Child));
         Set_Console (Script.Interpreter, Gtk_Text_View (Get_Child (Console)));
         Raise_Child (Child);
      end if;

      return Child;
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
      if Widget.all in Interpreter_View_Record'Class then
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
      Ignored      : Integer;
      pragma Unreferenced (Ignored);
      N      : Node_Ptr;
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
      Initialize (Python_Module_Id.Script.Interpreter, Get_History (Kernel));

      N     := new Node;
      N.Tag := new String'("Python_Console");
      Add_Default_Desktop_Item
        (Kernel, N,
         10, 10,
         400, 100,
         "Python", "Python Console",
         Docked, Bottom,
         True, True);

      --  Create the GPS module, in which all functions and classes are
      --  registered

      Python_Module_Id.Script.GPS_Module := Py_InitModule
        (GPS_Module_Name, Doc => "Interface with the GPS environment");
      Run_Command (Python_Module_Id.Script.Interpreter,
                   "import GPS", Hide_Output => True);

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
         Return_Value => "string",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Return_Value => "string",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Return_Value => "integer",
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Return_Value => "integer",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_File_Command_Handler'Access,
         Class        => Get_File_Class (Kernel));

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Return_Value => "string",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Return_Value => "string",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Return_Value => "integer",
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Return_Value => "integer",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Project_Command_Handler'Access,
         Class        => Get_Project_Class (Kernel));

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Return_Value => "string",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Return_Value => "string",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Return_Value => "integer",
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Return_Value => "integer",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Entity_Command_Handler'Access,
         Class        => Get_Entity_Class (Kernel));

      Register_Command
        (Python_Module_Id.Script,
         Command      => "__str__",
         Return_Value => "string",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__repr__",
         Return_Value => "string",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__hash__",
         Return_Value => "integer",
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
      Register_Command
        (Python_Module_Id.Script,
         Command      => "__cmp__",
         Return_Value => "integer",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Python_Location_Command_Handler'Access,
         Class        => Get_File_Location_Class (Kernel));
   end Register_Module;

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
         Set_Return_Value (Data, Get_Name (Info));

      elsif Command = "__cmp__" then
         declare
            Inst2 : constant Class_Instance := Nth_Arg
              (Data, 2, Get_File_Class (Kernel));
            Info2 : constant File_Info := Get_Data (Inst2);
            Name  : constant String := Get_Name (Info);
            Name2 : constant String := Get_Name (Info2);
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
         Set_Return_Value (Data, Integer (Hash (Get_Name (Info))));
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
      Kernel   : constant Kernel_Handle  := Get_Kernel (Data);
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_Project_Class (Kernel));
      Project  : constant Project_Type := Get_Data (Instance);
   begin
      if Command = "__str__" then
         Set_Return_Value (Data, Project_Name (Project));

      elsif Command = "__repr__" then
         Set_Return_Value (Data, Project_Path (Project));

      elsif Command = "__cmp__" then
         declare
            Inst2 : constant Class_Instance := Nth_Arg
              (Data, 2, Get_Project_Class (Kernel));
            Project2 : constant Project_Type := Get_Data (Inst2);
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
      Kernel   : constant Kernel_Handle  := Get_Kernel (Data);
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_Entity_Class (Kernel));
      Entity   : constant Entity_Information := Get_Data (Instance);
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         Set_Return_Value (Data,
                           Get_Name (Entity) & ':'
                           & Get_Declaration_File_Of (Entity) & ':'
                           & Image (Get_Declaration_Line_Of (Entity)) & ':'
                           & Image (Get_Declaration_Column_Of (Entity)));

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (Get_Name (Entity)
                   & Get_Declaration_File_Of (Entity)
                   & Image (Get_Declaration_Line_Of (Entity))
                   & Image (Get_Declaration_Column_Of (Entity)))));

      elsif Command = "__cmp__" then
         declare
            Inst2 : constant Class_Instance :=
              Nth_Arg (Data, 2, Get_Entity_Class (Kernel));
            Entity2 : constant Entity_Information := Get_Data (Inst2);
            Line1, Line2 : Integer;
            Name1 : constant String := Get_Name (Entity);
            Name2 : constant String := Get_Name (Entity2);
         begin
            if Name1 < Name2 then
               Set_Return_Value (Data, -1);
            elsif Name1 = Name2 then
               declare
                  File1 : constant String :=
                    Get_Declaration_File_Of (Entity);
                  File2 : constant String :=
                    Get_Declaration_File_Of (Entity2);
               begin
                  if File1 < File2 then
                     Set_Return_Value (Data, -1);
                  elsif File1 = File2 then
                     Line1 := Get_Declaration_Line_Of (Entity);
                     Line2 := Get_Declaration_Line_Of (Entity2);
                     if Line1 < Line2 then
                        Set_Return_Value (Data, -1);
                     elsif Line1 = Line2 then
                        Line1 := Get_Declaration_Column_Of (Entity);
                        Line2 := Get_Declaration_Column_Of (Entity2);
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
      Kernel   : constant Kernel_Handle  := Get_Kernel (Data);
      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_File_Location_Class (Kernel));
      Info     : constant File_Location_Info := Get_Data (Instance);
      Fileinfo : constant File_Info := Get_Data (Get_File (Info));
   begin
      if Command = "__str__"
        or else Command = "__repr__"
      then
         Set_Return_Value (Data,
                           Base_Name (Get_Name (Fileinfo)) & ':'
                           & Image (Get_Line (Info)) & ':'
                           & Image (Get_Column (Info)));

      elsif Command = "__hash__" then
         Set_Return_Value
           (Data, Integer
            (Hash (Get_Name (Fileinfo)
                   & Image (Get_Line (Info))
                   & Image (Get_Column (Info)))));

      elsif Command = "__cmp__" then
         declare
            Inst2 : constant Class_Instance :=
              Nth_Arg (Data, 2, Get_File_Location_Class (Kernel));
            Info2 : constant File_Location_Info := Get_Data (Inst2);
            Fileinfo2 : constant File_Info := Get_Data (Get_File (Info2));
            Line1, Line2 : Integer;
            Name1 : constant String := Get_Name (Fileinfo);
            Name2 : constant String := Get_Name (Fileinfo2);
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

   procedure Free (Data : in out Python_Callback_Data'Class) is
   begin
      Unchecked_Free (Data.Kw_Params);
   end Free;

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
      Callback.Script       := Handler.Script;
      Callback.Is_Method    := Handler.Is_Method;
      Py_INCREF (Callback.Return_Value);

      Handler.Handler.all (Callback, Handler.Command);

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
      when Invalid_Parameter =>
         Trace (Me, "Raised invalid_parameter");

         if not Callback.Has_Return_Value
           or else  Callback.Return_Value /= null
         then
            PyErr_SetString (Handler.Script.GPS_Invalid_Arg,
                             -"Invalid argument to GPS function");
         end if;

         Free (Callback);
         return null;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

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
      Params        : String := "";
      Return_Value  : String := "";
      Description   : String := "";
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False)
   is
      function Profile return String;
      --  Return a printable version of the profile

      function Profile return String is
      begin
         if Params = "" then
            if Return_Value = "" then
               return "() -> None";
            else
               return "() -> " & Return_Value;
            end if;
         elsif Return_Value = "" then
            return Params & " -> None";
         else
            return Params & " -> " & Return_Value;
         end if;
      end Profile;


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
            Func   => Create_Method_Def
              (Command, First_Level'Access,
               Command & ' ' & Profile & ASCII.LF & ASCII.LF & Description),
            Self   => User_Data);

      else
         if Command = Constructor_Method then
            Def := Create_Method_Def
              ("__init__", First_Level'Access,
               Get_Name (Class) & ' ' & Profile
               & ASCII.LF & ASCII.LF & Description);
         else
            Def := Create_Method_Def
              (Command, First_Level'Access,
               Command & ' ' & Profile
               & ASCII.LF & ASCII.LF & Description);
         end if;

         Klass := Lookup_Class_Object (Script.GPS_Module, Get_Name (Class));

         if Static_Method then
            Add_Static_Method
              (Class => Klass, Func  => Def, Self   => User_Data);
         else
            Add_Method (Class => Klass, Func  => Def, Self   => User_Data);
         end if;
      end if;
   end Register_Command;

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class
     (Script        : access Python_Scripting_Record;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class)
   is
      Dict  : constant PyDictObject := PyDict_New;
      Class : PyClassObject;
      Ignored : Integer;
      Bases   : PyObject := null;
      pragma Unreferenced (Ignored);
   begin
      PyDict_SetItemString
        (Dict, "__doc__", PyString_FromString (Description));
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
      Ignored := PyModule_AddObject
        (Script.GPS_Module, New_String (Name), Class);
   end Register_Class;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script             : access Python_Scripting_Record;
      Command            : String;
      Display_In_Console : Boolean := True) is
   begin
      Run_Command
        (Script.Interpreter, Command, Hide_Output => not Display_In_Console);
   end Execute_Command;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File
     (Script             : access Python_Scripting_Record;
      Filename           : String;
      Display_In_Console : Boolean := True)
   is
      Cmd : constant String := "execfile (""" & Filename & """)";
   begin
      if Get_Console (Script.Interpreter) /= null then
         Insert_Text (Script.Interpreter, Cmd & ASCII.LF);
      end if;
      Execute_Command (Script, Cmd, Display_In_Console);
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
         raise Invalid_Parameter;
      end if;
      return PyString_AsString (Item);
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
         raise Invalid_Parameter;
      end if;

      return Integer (PyInt_AsLong (Item));
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
         Trace (Me, "Nth_Arg: Invalid parameter type, expected boolean "
                & N'Img);
         raise Invalid_Parameter;
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
         raise Invalid_Parameter;
      end if;
      return PyCObject_AsVoidPtr (Item);
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Class : Class_Type)
      return Class_Instance
   is
      Item : constant PyObject := Get_Param (Data, N);
      C    : constant PyObject := Lookup_Class_Object
        (Data.Script.GPS_Module, Get_Name (Class));
      Item_Class : PyObject;
   begin
      if not PyInstance_Check (Item) then
         Trace (Me, "Nth_Arg: Item is not an instance");
         raise Invalid_Parameter;
      end if;

      Item_Class := PyObject_GetAttrString (Item, "__class__");
      if Item_Class = null then
         Trace (Me, "Nth_Arg: Couldn't find class of instance");
      end if;

      if not PyClass_IsSubclass (Item_Class, Base => C) then
         Trace (Me, "Nth_Arg: invalid class");
         raise Invalid_Parameter;
      end if;

      return new Python_Class_Instance_Record'
        (Class_Instance_Record with Script => Data.Script, Data => Item);
   end Nth_Arg;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Script : access Python_Scripting_Record;
      Class  : Class_Type;
      Base   : Class_Type) return Boolean
   is
      C : constant PyObject := Lookup_Class_Object
        (Script.GPS_Module, Get_Name (Class));
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
      Trace (Me, "Set_Error_Msg: " & Msg);
      Setup_Return_Value (Data);
      PyErr_SetString (Data.Script.GPS_Exception, Msg);
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
         Script => Python_Scripting (Script),
         Data   => PyInstance_NewRaw (Klass, null));
   end New_Instance;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Instance : access Python_Class_Instance_Record)
      return Class_Type
   is
      Class : constant PyObject :=
        PyObject_GetAttrString (Instance.Data, "__class__");
   begin
      if Class = null then
         return No_Class;
      else
         return New_Class
           (Instance.Script.Kernel,
            PyString_AsString (PyObject_GetAttrString (Class, "__name__")));
      end if;
   end Get_Class;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Python_Class_Instance_Record)
      return Glib.Object.GObject
   is
      Addr : constant System.Address := Get_Data (Instance);
   begin
      return Convert (Addr);
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Python_Class_Instance_Record)
      return Integer
   is
      Item : constant PyObject := PyObject_GetAttrString
        (Instance.Data, GPS_Data_Attr);
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
     (Instance : access Python_Class_Instance_Record) return String
   is
      Item : constant PyObject := PyObject_GetAttrString
        (Instance.Data, GPS_Data_Attr);
   begin
      if Item = null or else not PyString_Check (Item) then
         raise Invalid_Data;
      end if;
      return PyString_AsString (Item);
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Python_Class_Instance_Record)
      return System.Address
   is
      Item : constant PyObject := PyObject_GetAttrString
        (Instance.Data, GPS_Data_Attr);
   begin
      if Item = null or else not PyCObject_Check (Item) then
         raise Invalid_Data;
      end if;
      return PyCObject_AsVoidPtr (Item);
   end Get_Data;

   -------------------
   -- Unref_Gobject --
   -------------------

   procedure Unref_Gobject (Data : System.Address) is
   begin
      Unref (GObject'(Convert (Data)));
   end Unref_Gobject;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Python_Class_Instance_Record;
      Value    : access Glib.Object.GObject_Record'Class) is
   begin
      Set_Data
        (Instance, Convert (GObject (Value)),
         On_Destroy => Unref_Gobject'Access);
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Python_Class_Instance_Record; Value : Integer) is
   begin
      PyObject_SetAttrString
        (Instance.Data, GPS_Data_Attr, PyInt_FromLong (long (Value)));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Python_Class_Instance_Record; Value : String) is
   begin
      PyObject_SetAttrString
        (Instance.Data, GPS_Data_Attr, PyString_FromString (Value));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance   : access Python_Class_Instance_Record;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null)
   is
      Data : constant PyObject := PyCObject_FromVoidPtr
        (Value, PyCObject_Destructor (On_Destroy));
   begin
      PyObject_SetAttrString (Instance.Data, GPS_Data_Attr, Data);
   end Set_Data;

   --------------------
   -- Primitive_Free --
   --------------------

   procedure Primitive_Free (Instance : in out Python_Class_Instance_Record) is
   begin
      Py_DECREF (Instance.Data);
   end Primitive_Free;

end Python_Module;
