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

--  This module provides various types and subprograms to integrate various
--  external scripting languages.
--  This API was designed so that multiple scripting languages can be used with
--  GPS, and so that the core of GPS and all the various modules remain as
--  independant as possible from the specific language.

with System;
with GNAT.OS_Lib;
with Glib.Object;
with Src_Info.Queries;
with Projects;
with Glide_Kernel.Modules;
with Interactive_Consoles;

package Glide_Kernel.Scripts is

   type Scripting_Language_Record is abstract tagged private;
   type Scripting_Language is access all Scripting_Language_Record'Class;

   type Cst_String_Access is access constant String;
   type Cst_Argument_List is array (Natural range <>) of Cst_String_Access;

   -----------------
   -- Class types --
   -----------------

   type Class_Type is private;
   No_Class : constant Class_Type;
   --  A class type, which can be used to create new instances. Primite
   --  operations (aka methods) can be associated with the class. This is the
   --  primary way to make new subprograms available to the user, while
   --  organizing them into namespaces.

   type Class_Instance_Record is abstract tagged private;
   type Class_Instance is access all Class_Instance_Record'Class;
   --  The instance of a class, which embeds some Ada data.

   function New_Class
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class) return Class_Type;
   --  For some languages, this notion is not supported, and the class will not
   --  be visible by the user in the shell. Methods create for the class will
   --  then simply be made available directly in the shell.
   --  If a class with the same name was created, it is returned, and no class
   --  is created anew.
   --  Base is the base class, or parent class

   function Get_Name (Class : Class_Type) return String;
   --  Return the name of the class

   -------------------
   -- Callback_Data --
   -------------------

   Invalid_Parameter : exception;
   No_Such_Parameter : exception;

   type Callback_Data is abstract tagged private;
   --  Data used to communicate with the scripting language engine, to marshall
   --  the parameters and return values.

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
      return Glide_Kernel.Kernel_Handle;
   --  Return the kernel associated with Data

   function Nth_Arg
     (Data : Callback_Data; N : Positive) return String is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Integer is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Boolean is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return System.Address is abstract;
   function Nth_Arg
     (Data       : Callback_Data;
      N          : Positive;
      Class      : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance is abstract;
   --  Get the nth argument to the function, starting from 1.
   --  If there is not enough parameters, No_Such_Parameter is raised
   --  If the parameters doesn't have the right type, Invalid_Parameter is
   --  raised.
   --  In the last case, the class_instance must belong to Class or its
   --  children, or Invalid_Parameter is also raised.
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
     (Data : Callback_Data; N : Positive; Default : System.Address)
      return System.Address;
   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Class   : Class_Type;
      Default : Class_Instance;
      Allow_Null : Boolean := False)
      return Class_Instance;
   --  Same as above, except that if there are not enough parameters, Default
   --  is returned.

   procedure Set_Error_Msg
     (Data : in out Callback_Data; Msg : String) is abstract;
   --  Set an error message.
   --  The return value for this callback will be ignored. On most languages
   --  (python,...) this is equivalent to raising an exception.

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
     (Data : in out Callback_Data; Value : System.Address) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Class_Instance) is abstract;
   --  Set the return value of Data.
   --  If the return value was set as a list, Value is appended to the
   --  list. For languages that do not support lists, the append is only
   --  performed for strings (newline-separated). Other data types simply
   --  replace the current return value.
   --
   --  When storing a Class_Instance, the Callback_Data takes control of the
   --  instance, and will be in charge of freeing it.

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
   --  Data is used as a dispatcher, since each instance is specific to a given
   --  programming language.
   --  This call should generally be the result of the user calling a
   --  function, which acts as a constructor for the class.
   --  The result must be freed, unless you assign it to a Callback_Data with a
   --  call to Set_Return_Value. In this case, it is adopted by the
   --  Callback_Data.

   function Get_Script (Instance : access Class_Instance_Record)
      return Scripting_Language is abstract;
   --  Return the scripting language that creates this instance

   function Get_Class (Instance : access Class_Instance_Record)
      return Class_Type is abstract;
   --  Return the class to which this instance belongs

   function Get_Data (Instance : access Class_Instance_Record)
      return Glib.Object.GObject is abstract;
   function Get_Data (Instance : access Class_Instance_Record)
      return System.Address is abstract;
   function Get_Data (Instance : access Class_Instance_Record)
      return Integer is abstract;
   function Get_Data
     (Instance : access Class_Instance_Record) return String is abstract;
   --  Get the data embedded in the class.
   --  Invalid_Data is raised if no such data was stored in the instance

   type Destroy_Handler is access procedure (Value : System.Address);
   pragma Convention (C, Destroy_Handler);
   --  Called when the data stored in an instance is no longer needed for this
   --  instance.

   procedure Set_Data
     (Instance : access Class_Instance_Record;
      Value    : access Glib.Object.GObject_Record'Class) is abstract;
   procedure Set_Data
     (Instance : access Class_Instance_Record;
      Value    : String) is abstract;
   procedure Set_Data
     (Instance : access Class_Instance_Record;
      Value    : Integer) is abstract;
   procedure Set_Data
     (Instance   : access Class_Instance_Record;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null) is abstract;
   --  Associate some data with the instance.
   --  In the case of a GObject, the reference counting for the object is
   --  increased or decreased as appropriate.

   procedure Primitive_Free (Instance : in out Class_Instance_Record)
      is abstract;
   --  Primitive operation for Free. Do not call directly, only through Free
   --  below.

   procedure Free (Instance : access Class_Instance_Record'Class);
   --  Free the class instance.

   procedure Ref (Instance : access Class_Instance_Record);
   --  This increments the reference counting on Instance. Use Free to
   --  decrement. This should be called to make sure that the underlying shell
   --  doesn't destroy the object while the instance is stored in some data
   --  structure in the program.
   --  By default, this does nothing

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
      Params       : String;
      Return_Value : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function;
      Class        : Class_Type := No_Class;
      Class_Method : Boolean := False) is abstract;
   --  See comment for Register_Command in the kernel.

   procedure Register_Class
     (Script        : access Scripting_Language_Record;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class) is abstract;
   --  Create a new class in the interpreter

   procedure Execute_Command
     (Script             : access Scripting_Language_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Show_Command       : Boolean := True;
      Errors             : out Boolean) is abstract;
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
     (Script             : access Scripting_Language_Record;
      Command            : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Show_Command       : Boolean := True;
      Errors             : access Boolean) return String;
   --  Execute a command, and return its result as a displayable string.
   --  Note: some languages might simply return an empty string if they cannot
   --  capture the output of their interpreter. This command is mostly useful
   --  for the GPS shell.
   --  Command can never be a list of commands (no semicolon or newline
   --  separated).

   function Execute_Command_With_Args
     (Script             : access Scripting_Language_Record;
      Command            : String;
      Args               : GNAT.OS_Lib.Argument_List) return String;
   --  Execute a command, the arguments of which are already splitted and
   --  unquoted.
   --  This procedure needs only be implemented for the GPS shell, in all other
   --  language you should keep the default which raises Program_Error, since
   --  this function is not used anywhere but for shell commands.
   --  All output is hidden

   procedure Execute_File
     (Script             : access Scripting_Language_Record;
      Filename           : String;
      Console            : Interactive_Consoles.Interactive_Console := null;
      Hide_Output        : Boolean := False;
      Errors             : out Boolean) is abstract;
   --  Execute a script contained in an external file.

   function Get_Name (Script : access Scripting_Language_Record)
      return String is abstract;
   --  The name of the scripting language

   function Get_Kernel (Script : access Scripting_Language_Record)
      return Kernel_Handle is abstract;
   --  Return the kernel in which Script is registered

   function Is_Subclass
     (Script : access Scripting_Language_Record;
      Class  : Class_Type;
      Base   : Class_Type) return Boolean is abstract;
   --  Whether Class is a Base or a subclass of Base.

   --------------------------
   -- Commands and methods --
   --------------------------

   GPS_Shell_Name : constant String := "Shell";
   Constructor_Method : constant String;

   procedure Initialize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize this module

   procedure Finalize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Finalize this module, and free associated memory

   procedure Register_Default_Script_Commands
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Add the standard script commands.
   --  This subprogram should be called only after all scripting languages
   --  have been registered.

   function Parameter_Names_To_Usage
     (Parameters            : Cst_Argument_List;
      Optional_Params_Count : Natural := 0) return String;
   --  From the list of parameters used for Name_Parameters, create a suitable
   --  Usage string for Register_Command.
   --  Optional_Params_Count is the number of parameters in Parameters that are
   --  optional. These have to be the last entries in Parameters.

   procedure Register_Command
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command       : String;
      Params        : String     := "";
      Return_Value  : String     := "";
      Description   : String;
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
   --  Params describes the parameters to the command. Its recommended format
   --  is the following:
   --      "(file, [line=1])"
   --  to indicate a function whose first parameter is a file, the second
   --  parameter is optional and has a default value of 1.
   --
   --  The return value of the command is indicated by Return_Value. Its format
   --  is the following:
   --     "list"
   --  The empty string means that the command doesn't return anything.
   --  In the example above, "list" indicates that the function returns a list
   --  (when supported by the language), or a string (when lists are not
   --  supported).
   --
   --  If the command has some graphical output (dialog,...), it must run in
   --  a separate main loop (Gtk.Main.Gtk_Main or modal dialogs).

   procedure Register_Scripting_Language
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Script  : access Scripting_Language_Record'Class);
   --  Register a new scripting language in the kernel.
   --  Scripting languages are freed when the kernel is destroyed

   function Lookup_Scripting_Language
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name    : String) return Scripting_Language;
   --  Lookup one of the registered languages by name.

   No_Args : constant GNAT.OS_Lib.Argument_List := (1 .. 0 => null);

   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String) return String;
   procedure Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String);
   procedure Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List);
   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for entities. This encapsulates an
   --  Entity_Information.

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Entity   : Src_Info.Queries.Entity_Information);
   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Src_Info.Queries.Entity_Information;
   --  The Entity class stores some Entity_Information data in Instance
   --  You should destroy the entity passed to Set_Data, but not the value
   --  returned by Get_Data

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Src_Info.Queries.Entity_Information) return Class_Instance;
   --  Return a new entity. Entity parameter should be freed by the caller.

   ----------------
   -- File_Class --
   ----------------

   function Get_File_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for file types. This encapsulates a File_Info

   type File_Info is private;
   No_File : constant File_Info;

   function Get_File (File : File_Info) return VFS.Virtual_File;
   --  Return the name of File

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return File_Info;
   --  Retrieve the file information from an instance

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : VFS.Virtual_File) return Class_Instance;
   --  Return a new file.

   -------------------------
   -- File_Location_Class --
   -------------------------

   function Get_File_Location_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class used to represent locations in files. This encapsulates
   --  a File_Location_Info

   type File_Location_Info is private;
   No_File_Location : constant File_Location_Info;

   function Get_File (Location : File_Location_Info) return Class_Instance;
   function Get_Line (Location : File_Location_Info) return Integer;
   function Get_Column (Location : File_Location_Info) return Integer;
   --  Return the information stored in the file location

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return File_Location_Info;
   --  Retrieve the file location information from an instance

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Natural) return Class_Instance;
   --  Return a new file.
   --  File mustn't be destroyed after this call.

   -------------------
   -- Project_Class --
   -------------------

   function Get_Project_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for projects. This encapsulates a Project_Type

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Projects.Project_Type;
   --  Retrieve get some project information in Instance

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : Projects.Project_Type) return Class_Instance;
   --  Return a new project

   -------------------
   -- Context_Class --
   -------------------

   function Get_File_Context_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return a class for a File_Selection_Context

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Glide_Kernel.Modules.File_Selection_Context_Access;
   --  Retrieve some context information from instance

   function Create_File_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : Glide_Kernel.Modules.File_Selection_Context_Access)
      return Class_Instance;
   --  Create a new context

   function Get_Entity_Context_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return a class for an Entity_Selection_Context

   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Glide_Kernel.Modules.Entity_Selection_Context_Access;
   --  Retrieve some context information from instance

   function Create_Entity_Context
     (Script  : access Scripting_Language_Record'Class;
      Context : Glide_Kernel.Modules.Entity_Selection_Context_Access)
      return Class_Instance;
   --  Create a new context

private
   Constructor_Method : constant String := "<@constructor@>";

   type Class_Type is record
      Name : GNAT.OS_Lib.String_Access;
   end record;

   type File_Info is record
      File : VFS.Virtual_File;
   end record;

   type File_Location_Info is record
      File : Class_Instance;
      Line, Column : Natural;
   end record;

   No_File  : constant File_Info := (File => VFS.No_File);
   No_Class : constant Class_Type := (Name => null);
   No_File_Location : constant File_Location_Info := (null, 0, 0);

   type Class_Instance_Record is abstract tagged null record;
   type Callback_Data is abstract tagged null record;
   type Scripting_Language_Record is abstract tagged null record;

end Glide_Kernel.Scripts;
