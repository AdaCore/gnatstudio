-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2006                      --
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

--  This module provides various types and subprograms to integrate various
--  external scripting languages.
--  This API was designed so that multiple scripting languages can be used with
--  GPS, and so that the core of GPS and all the various modules remain as
--  independant as possible from the specific language.

with Ada.Finalization;

with GNAT.OS_Lib;
with GNAT.Strings;

with Gtk.Handlers;
with Gtk.Widget;

with Basic_Types;
with Entities;
with Projects;
with Interactive_Consoles;

package GPS.Kernel.Scripts is

   GUI_Class_Name : constant String := "GUI";
   --  Name for the GPS.GUI class

   type Scripting_Language_Record is abstract tagged private;
   type Scripting_Language is access all Scripting_Language_Record'Class;

   type Cst_String_Access is access constant String;
   type Cst_Argument_List is array (Natural range <>) of Cst_String_Access;

   type Callback_Data is abstract tagged private;
   type Callback_Data_Access is access all Callback_Data'Class;
   --  Data used to communicate with the scripting language engine, to marshall
   --  the parameters and return values.

   ----------------------
   -- Subprogram types --
   ----------------------

   type Subprogram_Record is abstract tagged private;
   type Subprogram_Type is access all Subprogram_Record'Class;
   --  This type represents a subprogram for the language. In the GPS shell,
   --  this is in fact a GPS action, represented as a string. In Python, this
   --  is a python object which is a function or method.
   --  Do not confuse this with a shell command, it has a more general meaning.
   --  In particular, the user cannot define new shell commands in the GPS
   --  shell, and thus Subprogram_Record has a broader meaning.

   procedure Free (Subprogram : in out Subprogram_Type);
   --  Free the subprogram

   function Get_Script
     (Subprogram : Subprogram_Record) return Scripting_Language is abstract;
   --  Return the language in which the subprogram was written

   procedure Free (Subprogram : in out Subprogram_Record) is abstract;
   --  Free the memory occupied by the subprogram instance

   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean is abstract;
   --  Execute the subprogram with the given arguments, and evaluate its output
   --  as a boolean

   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class) return String is abstract;
   --  Execute the subprogram with the given arguments, and evaluate its output
   --  as a string

   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class)
      return GNAT.Strings.String_List is abstract;
   --  Returned value must be freed by the caller.
   --  Some items in the result value might be left to null if the
   --  corresponding element from the shell is not a string.

   function Get_Name
     (Subprogram : access Subprogram_Record) return String is abstract;
   --  Return the name of the subprogram, as a string that can be displayed for
   --  the user. This is used when analysing the contents of a hook for
   --  instance

   package Subprogram_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Subprogram_Type);

   -----------------
   -- Class types --
   -----------------

   type Class_Type is private;
   No_Class : constant Class_Type;
   --  A class type, which can be used to create new instances. Primitive
   --  operations (aka methods) can be associated with the class. This is the
   --  primary way to make new subprograms available to the user, while
   --  organizing them into namespaces.

   type Class_Instance is private;
   No_Class_Instance : constant Class_Instance;
   --  The instance of a class, which embeds some Ada data. This type is
   --  reference counted, and will automatically take care of memory management
   --  issues.

   function New_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Base   : Class_Type := No_Class) return Class_Type;
   --  For some languages, this notion is not supported, and the class will not
   --  be visible by the user in the shell. Methods created for the class will
   --  then simply be made available directly in the shell.
   --  If a class with the same name was created, it is returned, and no class
   --  is created anew.
   --  Base is the base class, or parent class. It only needs to be specified
   --  the first time the class is created (typically just before the matching
   --  calls to Register_Command), and can be left to its default value
   --  afterward.
   --  Description of the new class must be put in the file shell_commands.xml,
   --  which is read dynamically when generating the documentation.

   function Get_Name (Class : Class_Type) return String;
   --  Return the name of the class

   -------------------
   -- Callback_Data --
   -------------------

   Invalid_Parameter : exception;
   No_Such_Parameter : exception;

   function Create
     (Script          : access Scripting_Language_Record;
      Arguments_Count : Natural) return Callback_Data'Class is abstract;
   --  Create a new empty list of arguments. You must call Set_Nth_Arg for
   --  each of these arguments before using the return value

   procedure Free (Data : in out Callback_Data) is abstract;
   procedure Free (Data : in out Callback_Data_Access);
   --  Free the memory occupied by Data. This needs to be called only if Data
   --  was created through Create

   function Clone (Data : Callback_Data) return Callback_Data'Class
      is abstract;
   --  Clone Data. The result value must be freed by the caller

   procedure Set_Nth_Arg
     (Data : Callback_Data; N : Positive; Value : String) is abstract;
   procedure Set_Nth_Arg
     (Data : Callback_Data; N : Positive; Value : Integer) is abstract;
   procedure Set_Nth_Arg
     (Data : Callback_Data; N : Positive; Value : Boolean) is abstract;
   procedure Set_Nth_Arg
     (Data : Callback_Data; N : Positive; Value : Class_Instance) is abstract;
   procedure Set_Nth_Arg
     (Data : Callback_Data; N : Positive; Value : Subprogram_Type) is abstract;
   --  Set the nth argument of Data

   function Number_Of_Arguments
     (Data : Callback_Data) return Natural is abstract;
   --  Return the number of arguments passed to that callback. The number of
   --  arguments has already been check before the transfer to your own
   --  subprogram.

   procedure Name_Parameters
     (Data  : in out Callback_Data; Names : Cst_Argument_List)
      is abstract;
   --  Name the parameters, for languages which support it.
   --  For instance, the following call:
   --     Name_Parameters (Data, (1 => new String'("a"),
   --                             2 => new String'("b"),
   --                             3 => new String'("c")));
   --  will provide support for the following python calls:
   --     func (1, 2, 3)
   --     func (1, c=3, b=2)
   --  This call has no effect for languages which do not support name
   --  parameters.
   --  After calling this procedure, the parameters are reordered so that no
   --  matter what order the user specified them in, calling Nth_Arg (2) will
   --  always return the value for b.
   --  You should pass a default value to Nth_Arg, since otherwise if a
   --  parameter was not given on the command line, even if later parameters
   --  were given, Nth_Arg will raise Invalid_Parameter.
   --
   --  It is recommended that Names be a global constant, which you can also
   --  use when registering the command, through Parameter_Names_To_Usage, so
   --  that the documentation remains up-to-date.

   function Get_Script (Data : Callback_Data) return Scripting_Language
      is abstract;
   --  Return the scripting language that created Data.

   function Get_Kernel (Data : Callback_Data)
      return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated with Data

   function Nth_Arg
     (Data : Callback_Data; N : Positive) return String is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Integer is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Boolean is abstract;
   --  Get the nth argument to the function, starting from 1.
   --  If there is not enough parameters, No_Such_Parameter is raised
   --  If the parameters doesn't have the right type, Invalid_Parameter is
   --  raised.

   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Subprogram_Type is abstract;
   --  Same as above, for a subprogram. The returned value must be freed.

   function Nth_Arg
     (Data       : Callback_Data;
      N          : Positive;
      Class      : Class_Type;
      Allow_Null : Boolean := False) return Class_Instance is abstract;
   --  The class_instance must belong to Class or its children, or
   --  Invalid_Parameter is also raised.
   --  The return value must be freed by the caller.
   --  If Allow_Null is true, then a null instance might be passed as a
   --  parameter. If it is false, passing a null instance will raise
   --  Invalid_Parameter.

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : String)
      return String;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Integer)
      return Integer;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Boolean)
      return Boolean;
   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Class   : Class_Type;
      Default : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance;
   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type;
   --  Same as above, except that if there are not enough parameters, Default
   --  is returned. Returned value must be freed

   procedure Set_Error_Msg
     (Data : in out Callback_Data; Msg : String) is abstract;
   --  Set an error message.
   --  The return value for this callback will be ignored. On most languages
   --  (python,...) this is equivalent to raising an exception.
   --  If Msg is set to the empty string, an exception will still be raised

   procedure Set_Return_Value_As_List
     (Data : in out Callback_Data; Size : Natural := 0) is abstract;
   --  Setup the return value as an empty list. New values can be appended to
   --  the list with Set_Return_Value.
   --  If Size is not 0, then the list has a fixed size. Depending on the
   --  language, this could be a different type, such as a tuple in python.

   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Integer) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : String) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Boolean) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Class_Instance) is abstract;
   --  Set the return value of Data.
   --  If the return value was set as a list, Value is appended to the
   --  list. For languages that do not support lists, the append is only
   --  performed for strings (newline-separated). Other data types simply
   --  replace the current return value.

   procedure Set_Return_Value_Key
     (Data : in out Callback_Data; Key : String; Append : Boolean := False)
      is abstract;
   procedure Set_Return_Value_Key
     (Data : in out Callback_Data; Key : Integer; Append : Boolean := False)
      is abstract;
   procedure Set_Return_Value_Key
     (Data   : in out Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False) is abstract;
   --  Move the current value of Data, as set by Set_Return_Value into a
   --  htable.
   --  Typical usage would be:
   --     Set_Return_Value (Data, 12);
   --     Set_Return_Value_Key (Data, "key1");
   --
   --     Set_Return_Value_As_List (Data);
   --     Set_Return_Value (Data, 1);
   --     Set_Return_Value (Data, 2);
   --     Set_Return_Value_Key (Data, "key2");
   --  will create a htable containing (key1 => 12, key2 => (1, 2))
   --
   --  If Append is true and there is already a value set for Key, then the new
   --  value is append to it (a list is created if necessary). This might not
   --  be supported for languages that do not explicitly support htables like
   --  the GPS shell.
   --
   --  No provision is made for creating htables of htables, although htables
   --  of lists are supported, or for getting the currently set value for Key.

   ---------------------
   -- Class instances --
   ---------------------

   Invalid_Data : exception;

   function New_Instance
     (Script : access Scripting_Language_Record; Class : Class_Type)
      return Class_Instance is abstract;
   --  Create a new instance of the class.
   --  No data is stored in the object.
   --  This call should generally be the result of the user calling a
   --  function, which acts as a constructor for the class.

   function Is_Subclass
     (Instance : Class_Instance; Base : Class_Type) return Boolean;
   --  Whether Instance is a Base or from a subclass of Base

   function Get_Script (Instance : Class_Instance) return Scripting_Language;
   --  Return the scripting language that created this instance

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Integer;
   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return String;
   function Get_Data
     (Instance : Class_Instance;
      Name     : String := GUI_Class_Name) return Glib.Object.GObject;
   --  Get the data embedded in the class.
   --  These are specialized cases of Get_Property.
   --  Invalid_Data is raised if no such data was stored in the instance.
   --  Constraint_Error is raised if the data is not of the appropriate type.
   --  Class is used to differentiate the data for instances that inherit from
   --  several GPS classes, as in:
   --     class Foo (GPS.Console, GPS.Process):
   --        def __init__ (self):
   --           GPS.Console.__init__ (self,..)
   --           GPS.Process.__init__ (self,...)
   --  since both internal classes expect different data stored internally

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : String);
   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Integer);
   --  Associate some data with the instance.
   --  These are specialized cases of Set_Property.
   --  The class name is required to handle multiple inheritance: if we were
   --  always using the same internal identifier to associated data with the
   --  instance, then we couldn't have a class with multiple ancestors, each
   --  expecting its own user data set in the constructor.

   procedure Set_Data
     (Instance : Class_Instance;
      Widget   : Glib.Object.GObject;
      Name     : String := GUI_Class_Name);
   --  When associating a GObject, the Class_Instance will exists as long as
   --  the object exists. However, if the object is destroyed, the
   --  Class_Instance is not necessarily destroyed, although calling any of its
   --  functions at the script level will result in exceptions.
   --  Therefore, the following code works:
   --      e = GPS.MDI.get ("Scenario View")
   --      e.destroy()  ## widget destroyed, but e is still valid
   --      e.hide()     ## error, since e is no longer associated with a widget

   function Get_Instance
     (Script : access Scripting_Language_Record'Class;
      Widget : access Glib.Object.GObject_Record'Class)
      return Class_Instance;
   --  Associate an instance and a widget. The two are then closely associated:
   --  The instance will exists as long as the widget itself exists so that we
   --  always get the user data stored in the instance every time we work with
   --  the same widget.

   ---------------------------
   -- Class_Instance_Record --
   ---------------------------

   --  This type encapsulate some language specific data. It is overriden by
   --  each of the scripting languages. Do not use directly unless you are
   --  implementing a new scripting language

   type Class_Instance_Record is abstract tagged limited private;
   type Class_Instance_Record_Access is access all Class_Instance_Record'Class;
   --  A type overriden by each of the scripting languages

   function From_Instance
     (Script : access Scripting_Language_Record'Class;
      Inst   : access Class_Instance_Record'Class) return Class_Instance;
   --  Return a class instance wrapping Inst.
   --  For internal use by scripting languages only.

   function Is_Subclass
     (Instance : access Class_Instance_Record;
      Base     : Class_Type) return Boolean is abstract;
   --  Whether Instance is a Base or from a subclass of Base. Do not use
   --  directly, use the version that takes a Class_Instance instead

   procedure Incref (Inst : access Class_Instance_Record);
   procedure Decref (Inst : access Class_Instance_Record);
   --  Change the reference counting of Inst.
   --  These subprograms should only be called by Class_Instance itself, not
   --  from your own code.
   --  These subprogram should be overriden by each scripting language that
   --  needs to manage low-level objects, like a PyObject in python for
   --  instance.
   --  They should always call the inherited operation as the last part of
   --  their code (and not as the first call, since otherwise Decref cannot
   --  properly free the allocated memory).

   function Get_CIR
     (Inst : Class_Instance) return Class_Instance_Record_Access;
   --  For internal use only.

   function Print_Refcount
     (Instance : access Class_Instance_Record) return String;
   --  Debug only: print the reference counting for this instance.
   --  Implementations are encourage to concatenate with the inherited
   --  method's result

   --------------------
   -- Instance lists --
   --------------------

   --  Most internal objects, when exported to a shell, should reuse the same
   --  class instance whenever the same physical object is referenced. This is
   --  so that the user can store user data within the instance, and get it
   --  back easily the next time the same object is referenced.
   --  For types derived from GObject_Record, we provide appropriate Set_Data
   --  and Get_Data subprograms. For other types, the instance_list type can
   --  be used to store the instances (of which there is one per scripting
   --  language).

   type Instance_List is new Instance_List_Base with private;
   Null_Instance_List : constant Instance_List;
   --  Stores the instance created for some GPS internal data, so that the same
   --  script instance is reused every time we reference the same Ada object.

   type Instance_List_Access is access all Instance_List;
   --  This type should be convertible to a System.Address for storage in
   --  a selection_context

   type Instance_Array is array (Natural range <>) of Class_Instance;
   type Instance_Array_Access is access Instance_Array;

   procedure Free (List : in out Instance_List);
   procedure Free (List : in out Instance_List_Access);
   --  Free the instances stored in the list

   function Get
     (List   : Instance_List;
      Script : access Scripting_Language_Record'Class) return Class_Instance;
   --  Return the instance for a given script.

   procedure Set
     (List   : in out Instance_List;
      Script : access Scripting_Language_Record'Class;
      Inst   : Class_Instance);
   --  Set the instance for a specific language

   function Get_Instances (List : Instance_List) return Instance_Array;
   --  Return the instance array contained in the given list

   -------------------------
   -- Instance properties --
   -------------------------

   type Instance_Property_Record is abstract tagged null record;
   type Instance_Property is access all Instance_Property_Record'Class;

   procedure Destroy (Prop : in out Instance_Property_Record);
   --  Type of data that can be associated with a class_instance. This is a
   --  general type, but simpler types are provided already

   function Get_Instances
     (Prop : Instance_Property_Record) return Instance_List_Access;
   --  When a component of Prop stores some data that has a list of instances
   --  associated with it, returns that list of instances.
   --  (for instance, a selection_context has such a list to ensure that every
   --  time the user calls the python GPS.Context* constructor we return the
   --  same existing instance).
   --  Some special handling of these types is needed, to properly ensure
   --  refcounting in these mutually dependent types (the context owns a
   --  reference to the context, which itself owns a reference to the instance
   --  through its Instances field).
   --  The default is to return null.

   procedure Set_Property
     (Instance : Class_Instance;
      Name     : String;
      Property : Instance_Property_Record'Class);
   --  Associate user data with Instance. Multiple data can be stored in a
   --  given instance, each associated with a different Name. Typically, GPS
   --  classes use the class name as the property name to avoid conflicts.
   --  When the property is no longer needed (either because it is replaced by
   --  another one with the same name, or because Instance is destroyed), the
   --  Destroy operation is called on Property.
   --  Note that a copy of Property is stored, not Property itself.

   function Get_Property
     (Instance : Class_Instance;
      Name     : String) return Instance_Property_Record'Class;
   --  Return a general property associated with the widget

   -------------------------
   -- Callback_Data lists --
   -------------------------

   --  This type's goal is similar to the one for the instance lists, since the
   --  callback_data are also language-specific

   type Callback_Data_List is private;
   --  Stores a list of callback_data, each associated with a different
   --  scripting language

   procedure Free (List : in out Callback_Data_List);
   --  Free the instances stored in the list

   function Get
     (Kernel : access Kernel_Handle_Record'Class;
      List   : Callback_Data_List;
      Script : access Scripting_Language_Record'Class)
      return Callback_Data_Access;
   --  Return the data for a given script.
   --  The returned value should not be freed by the caller, it is the
   --  responsability of the callback_data_list to do so.

   procedure Set
     (Kernel : access Kernel_Handle_Record'Class;
      List   : in out Callback_Data_List;
      Script : access Scripting_Language_Record'Class;
      Data   : Callback_Data_Access);
   --  Set the data for a specific language. Data should not be freed by the
   --  caller.

   -------------------------
   -- Scripting languages --
   -------------------------

   type Module_Command_Function is access procedure
     (Data : in out Callback_Data'Class; Command : String);
   --  The callback handler for a command.
   --  The first argument is always the instance to which the method applies,
   --  if Command is a method.
   --  Should raise Invalid_Parameters if one of the parameters is incorrect.
   --  The number of parameters has been checked before this procedure is
   --  called.

   procedure Destroy (Script : access Scripting_Language_Record);
   --  Destroy the scripting language and the memory it occupies.
   --  Does nothing by default

   procedure Register_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function;
      Class        : Class_Type := No_Class;
      Class_Method : Boolean := False) is abstract;
   --  See comment for Register_Command in the kernel.

   procedure Register_Class
     (Script : access Scripting_Language_Record;
      Name   : String;
      Base   : Class_Type := No_Class) is abstract;
   --  Create a new class in the interpreter

   procedure Block_Commands
     (Script : access Scripting_Language_Record;
      Block  : Boolean) is abstract;
   --  If Block is true, no command can be executed for this scripting language

   procedure Execute_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean) is abstract;
   --  Execute a command in the script language.
   --  It isn't possible to retrieve the result of that command, this command
   --  is only used for its side effect.
   --  Depending on the language, Command might be a list of commands to
   --  execute, often semicolon or newline separated.
   --  Errors is set to True if there was any error executing the script.
   --  The output of the command, as well as the text of the command itself,
   --  are not visible to the user if Hide_Output is True. Otherwise, the text
   --  is sent to the default Python console if Console is null, or to the
   --  explicit Console specified by the caller.
   --  If Show_Command is True and Hide_Output is False, then the command
   --  itself is also printed in the console

   function Execute_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   --  Execute a command, and return its output as a displayable string.
   --  Note: some languages might simply return an empty string if they cannot
   --  capture the output of their interpreter. This command is mostly useful
   --  for the GPS shell, but also supported by python.
   --  Command can never be a list of commands (no semicolon or newline
   --  separated).

   function Execute_Command
     (Script      : access Scripting_Language_Record;
      Command     : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean is abstract;
   --  Execute a command and evaluate its return value (*not* its output) as a
   --  boolean. This is different from the version returning a string, in that
   --  only the return value is considered, not the full output.

   function Execute_Command
     (Script  : access Scripting_Language_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean is abstract;
   --  Execute a command, the argument of which are specified separately in
   --  Args.
   --  Return the value returned by the command itself.

   function Execute_Command_With_Args
     (Script  : access Scripting_Language_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Execute a command, the arguments of which are already splitted and
   --  unquoted.
   --  This procedure needs only be implemented for the GPS shell, in all other
   --  language you should keep the default which raises Program_Error, since
   --  this function is not used anywhere but for shell commands.
   --  All output is hidden

   procedure Execute_File
     (Script      : access Scripting_Language_Record;
      Filename    : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : out Boolean) is abstract;
   --  Execute a script contained in an external file.

   function Get_Name (Script : access Scripting_Language_Record)
      return String is abstract;
   --  The name of the scripting language

   function Get_Kernel (Script : access Scripting_Language_Record)
      return Kernel_Handle is abstract;
   --  Return the kernel in which Script is registered

   function Current_Script
     (Script : access Scripting_Language_Record) return String is abstract;
   --  Return the name of the current script (file or inline script) that we
   --  are executing. When unknown, the empty string should be returned.

   --------------------------
   -- Commands and methods --
   --------------------------

   GPS_Shell_Name      : constant String := "Shell";
   Constructor_Method  : constant String;
   Addition_Method     : constant String;
   Substraction_Method : constant String;
   Comparison_Method   : constant String;
   Destructor_Method   : constant String;

   procedure Initialize
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Initialize this module

   procedure Register_Default_Script_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Add the standard script commands.
   --  This subprogram should be called only after all scripting languages
   --  have been registered.

   procedure Register_Command
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command       : String;
      Minimum_Args  : Natural    := 0;
      Maximum_Args  : Natural    := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   --  Add a new function to all currently registered script languages.
   --
   --  If Class is not No_Class, then this procedure creates a method for this
   --  class, for the languages for which this is appropriate. An extra
   --  parameter is automatically added to the command, in first position,
   --  which is the instance to which this applies. In some shells, the user
   --  must provide this himself (GPS shell for instance), since the language
   --  is not object oriented. This first parameter must not be counted in
   --  Minimum_args and Maximum_Args
   --  Otherwise, it creates a global function in the script language.
   --
   --  If Static_Method is True, then Class must be different from No_Class.
   --  The resulting method doesn't take an instance as its first
   --  parameter. Instead, it behaves like a global function, except it is in a
   --  specific namespace corresponding to the class name.
   --  This is similar to C++'s static methods.
   --
   --  If Command is Constructor_Method, then the function is setup as the
   --  constructor for Class, which must not be No_Class. For compatibility
   --  with the greater number of languages, only one such constructor can be
   --  defined per class.
   --  A constructor receives an already built instance of the object, and
   --  should initialize the fields. Its first parameter is the instance, the
   --  second, third,... are the parameters passed to the constructor.
   --  The constructor shouldn't return any value through Set_Return_Value.
   --
   --  If Command is Addition_Method, this is a function that should take one
   --  argument in addition to the instance, and return a new instance. This
   --  handles statements like "inst + 1", although the second argument can be
   --  of any type (you can even handle multiple types in your implementation)
   --
   --  Subscription_Method is similar to Addition_Method.
   --
   --  Comparison_Method is a function that takes a second parameter, and
   --  returns -1 if the first is less than the second, 0 if they are equal,
   --  and 1 if the first is greater than the second.
   --
   --  Destructor_Method is called just before the instance is destroyed
   --
   --  Description of the new command must be put in the file
   --  shell_commands.xml, which is read dynamically when generating the
   --  documentation.
   --
   --  If the command has some graphical output (dialog,...), it must run in
   --  a separate main loop (Gtk.Main.Gtk_Main or modal dialogs).

   procedure Block_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Block  : Boolean);
   --  Block all execution of shell commands if Block is true

   procedure Register_Scripting_Language
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Script : access Scripting_Language_Record'Class);
   --  Register a new scripting language in the kernel.
   --  Scripting languages are freed when the kernel is destroyed

   function Lookup_Scripting_Language
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String) return Scripting_Language;
   --  Lookup one of the registered languages by name.

   type Scripting_Language_Array is
     array (Natural range <>) of Scripting_Language;
   function Get_Scripting_Languages
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Scripting_Language_Array;
   --  Return the list of all registered languages.

   No_Args : constant GNAT.OS_Lib.Argument_List := (1 .. 0 => null);

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String) return String;
   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String);
   procedure Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List);
   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Execute the command in the GPS shell.
   --  This is only intended as a simpler form of
   --     Execute_Command
   --       (Lookup_Scripting_Language (Kernel, GPS_Shell_Name), Command, Args)

   ------------------
   -- Entity_Class --
   ------------------

   --  The following services are provided for use in the context of GPS. They
   --  provide access to various predefined classes shared between multiple
   --  modules.

   function Get_Entity_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for entities. This encapsulates an
   --  Entity_Information.

   procedure Set_Data
     (Instance : Class_Instance; Entity : Entities.Entity_Information);
   function Get_Data
     (Data : Callback_Data; N : Positive) return Entities.Entity_Information;
   --  The Entity class stores some Entity_Information data in Instance
   --  You should destroy the entity passed to Set_Data, but not the value
   --  returned by Get_Data

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Entities.Entity_Information) return Class_Instance;
   --  Return a new entity. Entity parameter should be freed by the caller

   ----------------
   -- File_Class --
   ----------------

   function Get_File_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Class_Type;
   --  Return the class to use for file types. This encapsulates a File_Info.
   --  This is more efficient than calling directly
   --  GPS.Kernel.Scripts.New_Class particularly when a File class has already
   --  been created.

   function Get_Data
     (Data : Callback_Data; N : Positive) return VFS.Virtual_File;
   function Get_Data
     (Instance : Class_Instance) return VFS.Virtual_File;
   --  Retrieve the file information from an instance

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : VFS.Virtual_File) return Class_Instance;
   --  Return a new file

   ---------------
   -- GUI_Class --
   ---------------

   function Get_GUI_Class
     (Kernel : access Kernel_Handle_Record'Class) return Class_Type;
   --  Return the class to use for GUI elements. This encapsulate a Gtk_Widget

   -------------------------
   -- File_Location_Class --
   -------------------------

   function Get_File_Location_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class used to represent locations in files. This encapsulates
   --  a File_Location_Info

   type File_Location_Info is private;
   No_File_Location : constant File_Location_Info;

   function Get_File (Location : File_Location_Info) return Class_Instance;
   function Get_Line (Location : File_Location_Info) return Integer;
   function Get_Column
     (Location : File_Location_Info) return Basic_Types.Visible_Column_Type;
   --  Return the information stored in the file location

   function Get_Data (Data : Callback_Data; N : Positive)
      return File_Location_Info;
   --  Retrieve the file location information from an instance

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type) return Class_Instance;
   --  Return a new file.
   --  File mustn't be destroyed after this call.

   ----------------
   -- Hook_Class --
   ----------------

   function Get_Hook_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class used to provide an interface to hooks

   -------------------
   -- Project_Class --
   -------------------

   function Get_Project_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for projects. This encapsulates a Project_Type

   function Get_Data (Data : Callback_Data; N : Positive)
      return Projects.Project_Type;
   --  Retrieve some project information in Instance

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : Projects.Project_Type) return Class_Instance;
   --  Return a new project

   -------------------
   -- Context_Class --
   -------------------

   function Get_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the base class for all context-related classes

   function Create_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : GPS.Kernel.Selection_Context) return Class_Instance;
   --  Return an instance of one of the classes derived from
   --  Context_Class, depending on the type of Context.
   --  If Context is already associated with a Class_Instance, the same
   --  instance is returned.

   function Get_Data (Data : Callback_Data; N : Positive)
      return GPS.Kernel.Selection_Context;
   function Get_Data (Instance : Class_Instance)
      return GPS.Kernel.Selection_Context;
   --  Retrieve some context information from instance

   function Get_Area_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the base class for contexts containing file areas

   function Get_File_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return a class for a File_Selection_Context

   function Get_Entity_Context_Class
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return a class for an Entity_Selection_Context

private
   Constructor_Method  : constant String := "<@constructor@>";
   Addition_Method     : constant String := "+";
   Substraction_Method : constant String := "-";
   Comparison_Method   : constant String := "<=>";
   Destructor_Method   : constant String := "<@destructor@>";

   type Class_Type is record
      Name : GNAT.Strings.String_Access;
   end record;

   type File_Location_Info is record
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
   end record;

   type User_Data;
   type User_Data_List is access User_Data;
   type User_Data (Length : Natural) is record
      Next : User_Data_List;
      Name : String (1 .. Length);
      Prop : Instance_Property;
   end record;

   type Class_Instance_Record is abstract tagged limited record
      Refcount  : Natural := 1;
      Script    : Scripting_Language;
      User_Data : User_Data_List;
   end record;

   type Class_Instance_Data is new Ada.Finalization.Controlled with record
      Data : Class_Instance_Record_Access;
   end record;

   type Class_Instance (Initialized : Boolean := False) is record
      case Initialized is
         when True  => Data : Class_Instance_Data;
         when False => null;
      end case;
   end record;
   --  A Class_Instance cannot be a visibly tagged type if declared in this
   --  package, since otherwise we have operations dispatching on multiple
   --  types.
   --  We use a discriminated type so that we can declare No_Class_Instance.
   --  Otherwise, Adjust is called before its body is seen.

   overriding procedure Adjust   (CI : in out Class_Instance_Data);
   overriding procedure Finalize (CI : in out Class_Instance_Data);
   function "="       (CI1, CI2 : Class_Instance_Data) return Boolean;
   --  Takes care of the reference counting for a Class_Instance

   No_Class_Instance_Data : constant Class_Instance_Data :=
     (Ada.Finalization.Controlled with Data => null);
   No_Class_Instance : constant Class_Instance :=
     (Initialized => False);

   No_Class         : constant Class_Type := (Name => null);
   No_File_Location : constant File_Location_Info := (No_Class_Instance, 0, 0);

   type Subprogram_Record is abstract tagged null record;
   type Callback_Data is abstract tagged null record;
   type Scripting_Language_Record is abstract tagged null record;

   type Instance_List is new Instance_List_Base with record
      List : Instance_Array_Access;
   end record;

   Null_Instance_List : constant Instance_List :=
     (Instance_List_Base with List => null);

   type Callback_Data_Array is
     array (Natural range <>) of Callback_Data_Access;
   type Callback_Data_List  is access Callback_Data_Array;

end GPS.Kernel.Scripts;
