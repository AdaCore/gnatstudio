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

package Glide_Kernel.Scripts is

   type Scripting_Language_Record is abstract tagged private;
   type Scripting_Language is access all Scripting_Language_Record'Class;

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
      As_Dictionary : Boolean := False) return Class_Type;
   --  For some languages, this notion is not supported, and the class will not
   --  be visible by the user in the shell. Methods create for the class will
   --  then simply be made available directly in the shell.
   --  If As_Dictionary is true, then the class is setup so that items can be
   --  extracted from it using.
   --  If a class with the same name was created, it is returned, and no class
   --  is created anew.

   function Get_Name (Class : Class_Type) return String;
   --  Return the name of the class

   -------------------
   -- Callback_Data --
   -------------------

   Invalid_Parameter : exception;

   type Callback_Data is abstract tagged private;
   --  Data used to communicate with the scripting language engine, to marshall
   --  the parameters and return values.

   function Number_Of_Arguments
     (Data : Callback_Data) return Natural is abstract;
   --  Return the number of arguments passed to that callback. The number of
   --  arguments has already been check before the transfer to your own
   --  subprogram.

   function Get_Kernel (Data : Callback_Data) return Kernel_Handle is abstract;
   --  Return the GPS kernel

   function Nth_Arg
     (Data : Callback_Data; N : Positive) return String is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Integer is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return System.Address is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Class : Class_Type)
      return Class_Instance is abstract;
   --  Get the nth argument to the function, starting from 1.
   --  Raised Invalid parameter if there is no such parameter, or it doesn't
   --  have the right type.
   --  In the last case, the class_instance must belong to Class or its
   --  children, or Invalid_Parameter is also raised.

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
     (Data   : in out Callback_Data;
      Value  : Integer;
      Append : Boolean := False) is abstract;
   procedure Set_Return_Value
     (Data   : in out Callback_Data;
      Value  : String;
      Append : Boolean := False) is abstract;
   procedure Set_Return_Value
     (Data   : in out Callback_Data;
      Value  : System.Address;
      Append : Boolean := False) is abstract;
   procedure Set_Return_Value
     (Data   : in out Callback_Data;
      Value  : Class_Instance;
      Append : Boolean := False) is abstract;
   --  Set the return value of Data.
   --  If Append is True, and the return value was set up as a list, the value
   --  is appended to the list. For languages that do not support lists, the
   --  append is only performed for strings (newline-separated).
   --  If Append is True, and the return value is not a list, the new value
   --  replaces the previous one.
   --
   --  When storing a Class_Instance, the Callback_Data takes control of the
   --  instance, and will be in charge of freeing it.

   ---------------------
   -- Class instances --
   ---------------------

   Invalid_Data : exception;

   function New_Instance
     (Data : Callback_Data; Class : Class_Type)
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

   procedure Register_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Usage        : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function;
      Class        : Class_Type := No_Class) is abstract;
   --  See comment for Register_Command in the kernel.

   procedure Register_Class
     (Script        : access Scripting_Language_Record;
      Name          : String;
      Description   : String := "";
      As_Dictionary : Boolean := False) is abstract;
   --  Create a new class in the interpreter

   procedure Execute_Command
     (Script             : access Scripting_Language_Record;
      Command            : String;
      Display_In_Console : Boolean := True) is abstract;
   --  Execute a command in the script language.
   --  It isn't possible to retrieve the result of that command, this command
   --  is only used for its side effect.

   function Execute_Command
     (Script  : access Scripting_Language_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Execute a command, and return its result as a displayable string.
   --  Note: some languages might simply return an empty string if they cannot
   --  capture the output of their interpreter. This command is mostly useful
   --  for the GPS shell.

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

   GPS_Shell_Name : constant String := "GPS Shell";

   procedure Initialize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize this module

   procedure Register_Default_Script_Commands
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Add the standard script commands.
   --  This subprogram should be called only after all scripting languages
   --  have been registered.

   procedure Register_Command
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command      : String;
      Usage        : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function;
      Class        : Class_Type := No_Class);
   --  Add a new function to all currently registered script languages.
   --  If Class is not No_Class, then this procedure creates a method for this
   --  class, for the languages for which this is appropriate. An extra
   --  parameter is automatically added to the command, in first position,
   --  which is the instance to which this applies. In some shells, the user
   --  must provide this himself (GPS shell for instance), since the language
   --  is not object oriented. This first parameter must not be counted in
   --  Minimum_args and Maximum_Args
   --  Otherwise, it creates a global function in the script language.

   procedure Register_Scripting_Language
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Script  : access Scripting_Language_Record'Class);
   --  Register a new scripting language in the kernel

   function Lookup_Scripting_Language
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name    : String) return Scripting_Language;
   --  Lookup one of the registered languages by name.

   No_Args : constant GNAT.OS_Lib.Argument_List := (1 .. 0 => null);

   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List := No_Args) return String;
   procedure Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List := No_Args);
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
   --  returned by Get_Data.

   ----------------
   -- File_Class --
   ----------------

   function Get_File_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for file types. This encapsulates a File_Info

   type File_Info is private;
   No_File : constant File_Info;
   procedure Free (File : in out File_Info);

   function Get_Name (File : File_Info) return String;
   --  Return the name of File

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      File     : File_Info);
   function Get_Data (Instance : access Class_Instance_Record'Class)
      return File_Info;
   --  Store some file information with a file entity
   --  You should free the file passed to Set_Data, but not the value returned
   --  by Get_Data.

   -------------------
   -- Project_Class --
   -------------------

   function Get_Project_Class
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the class to use for projects. This encapsulates a Project_Type

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Project  : Projects.Project_Type);
   function Get_Data (Instance : access Class_Instance_Record'Class)
      return Projects.Project_Type;
   --  Store or get some project information in Instance


private
   type Class_Type is record
      Name : GNAT.OS_Lib.String_Access;
   end record;

   type File_Info is record
      Name : GNAT.OS_Lib.String_Access;
   end record;

   No_File  : constant File_Info := (Name => null);
   No_Class : constant Class_Type := (Name => null);

   type Class_Instance_Record is abstract tagged null record;
   type Callback_Data is abstract tagged null record;
   type Scripting_Language_Record is abstract tagged null record;


end Glide_Kernel.Scripts;
