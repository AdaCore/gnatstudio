------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the implementation for a simple scripting language

pragma Ada_2012;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;
private with GNAT.Strings;

package GNATCOLL.Scripts.Shell is

   Shell_Name : constant String := "shell";

   type Shell_Scripting_Record is new Scripting_Language_Record with private;
   type Shell_Scripting is access all Shell_Scripting_Record'Class;
   type Shell_Callback_Data is new Callback_Data with private;
   type Shell_Class_Instance_Record is new Class_Instance_Record with private;
   type Shell_Subprogram_Record is new Subprogram_Record with private;
   type Shell_Subprogram is access all Shell_Subprogram_Record'Class;
   --  This types are declared in the spec rather than in the body, so that
   --  their subprograms can be overridden again. For instance, GPS uses that
   --  to make a subprogram_type be a GPS action rather than a simple shell
   --  command.

   procedure Register_Shell_Scripting
     (Repo   : Scripts_Repository;
      Script : Shell_Scripting := null);
   --  Register the scripting language.
   --  Script can be specified if you want to specialize some aspects of the
   --  scripting language

   procedure Initialize
     (Data            : in out Shell_Callback_Data'Class;
      Script          : access Shell_Scripting_Record'Class);
   --  Initialize Data to pass Arguments_Count to a callback

   procedure List_Commands
     (Script  : access Shell_Scripting_Record'Class;
      Console : Virtual_Console := null);
   --  Print the list of all commands on Console. By default, print on the
   --  default console for Script

   procedure Initialize
     (Subprogram : in out Shell_Subprogram_Record'Class;
      Script     : access Scripting_Language_Record'Class;
      Command    : String);
   --  Initialize Subprogram so that it will execute Command

   function Get_Command
     (Subprogram : access Shell_Subprogram_Record) return String;
   --  Return the command that will be executed by Subprogram

   function Get_Args
     (Data : Shell_Callback_Data) return GNAT.OS_Lib.Argument_List;
   --  Return the list of arguments specified by Data. The returned value must
   --  never be freed by the caller

   procedure Set_Prompt
     (Script : access Shell_Scripting_Record'Class;
      Prompt : String);
   --  The prompt to use for consoles associated with this language

private
   Num_Previous_Returns : constant := 9;
   --  Number of parameters %1, %2,... which are used to memorize the result of
   --  previous commands.

   type Shell_Class_Instance_Record is new Class_Instance_Record with record
      Class : Class_Type;
      Props : aliased User_Data_List;
   end record;
   type Shell_Class_Instance is access all Shell_Class_Instance_Record'Class;

   overriding function Print_Refcount
     (Instance : access Shell_Class_Instance_Record) return String;
   overriding function Is_Subclass
     (Instance : access Shell_Class_Instance_Record;
      Base     : String) return Boolean;
   overriding procedure Set_Property
     (Instance : access Shell_Class_Instance_Record;
      Name : String; Value : Integer);
   overriding procedure Set_Property
     (Instance : access Shell_Class_Instance_Record;
      Name : String; Value : Float);
   overriding procedure Set_Property
     (Instance : access Shell_Class_Instance_Record;
      Name : String; Value : Boolean);
   overriding procedure Set_Property
     (Instance : access Shell_Class_Instance_Record;
      Name : String; Value : String);
   overriding function Get_Method
     (Instance : access Shell_Class_Instance_Record;
      Name : String) return Subprogram_Type;
   overriding function Get_User_Data
     (Self : not null access Shell_Class_Instance_Record)
      return access User_Data_List;
   --  See doc from inherited subprogram

   package Instances_List is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Class_Instance);
   --  ??? Would be faster to use a hash-table...

   -------------------------
   -- Command_Information --
   -------------------------

   type Command_Information is record
      Command         : GNAT.Strings.String_Access;
      Cmd             : Command_Descr_Access;
   end record;
   type Command_Information_Access is access Command_Information;
   --  Description for each of the registered commands.
   --  Command is the name that must be typed by the user in the console.
   --  Short_Command is the name under which the command was registered. It is
   --  the same as Command, except when the command is a method of a class. In
   --  this case, Command is equal to "Class.Short_Command"
   --  The command was set as a constructor if Short_Command is
   --  Constructor_Method.

   procedure Free (Com : in out Command_Information_Access);
   --  Free memory associated with Com

   package Command_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Command_Information_Access, Ada.Strings.Hash, "=");

   type Shell_Scripting_Record is new Scripting_Language_Record with record
      Repo      : Scripts_Repository;
      Finalized : Boolean := False;
      Blocked   : Boolean := False;
      Instances : Instances_List.List;
      --  All the instances that were created

      Commands_List : Command_Hash.Map;
      --  The list of all registered commands

      Returns   : GNAT.Strings.String_List (1 .. Num_Previous_Returns);
      --  The result of the Num_Previous_Returns previous commands

      Prompt : GNAT.Strings.String_Access := new String'("[Shell]>");
      --  Prompt to use in consoles for this language
   end record;

   overriding function Command_Line_Treatment
     (Script : access Shell_Scripting_Record) return Command_Line_Mode;
   overriding procedure Destroy (Script : access Shell_Scripting_Record);
   overriding procedure Register_Command
     (Script  : access Shell_Scripting_Record;
      Command : Command_Descr_Access);
   overriding procedure Register_Property
     (Script : access Shell_Scripting_Record;
      Prop   : Property_Descr_Access);
   overriding procedure Register_Class
     (Script : access Shell_Scripting_Record;
      Name   : String;
      Base   : Class_Type := No_Class;
      Module : Module_Type := Default_Module);
   overriding procedure Block_Commands
     (Script : access Shell_Scripting_Record; Block : Boolean);
   overriding procedure Execute_Command
     (Script       : access Shell_Scripting_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   overriding function Execute_Command
     (Script       : access Shell_Scripting_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   overriding function Execute_Command
     (Script      : access Shell_Scripting_Record;
      CL          : Arg_List;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean;
   overriding function Execute_Command
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   overriding function Execute_Command_With_Args
     (Script  : access Shell_Scripting_Record;
      CL      : Arg_List) return String;
   overriding procedure Execute_File
     (Script       : access Shell_Scripting_Record;
      Filename     : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   overriding function Get_Name
     (Script : access Shell_Scripting_Record) return String;
   overriding function Get_Repository
     (Script : access Shell_Scripting_Record)
      return Scripts_Repository;
   overriding function Current_Script
     (Script : access Shell_Scripting_Record) return String;
   overriding procedure Display_Prompt
     (Script  : access Shell_Scripting_Record;
      Console : Virtual_Console := null);
   overriding function Get_Prompt
     (Script : access Shell_Scripting_Record) return String;
   overriding procedure Complete
     (Script      : access Shell_Scripting_Record;
      Input       : String;
      Completions : out String_Lists.List);
   overriding function New_Instance
     (Script : access Shell_Scripting_Record; Class : Class_Type)
      return Class_Instance;
   overriding function New_List
     (Script : access Shell_Scripting_Record;
      Class  : Class_Type := No_Class)
      return List_Instance'Class;
   --  See doc from inherited subprograms

   type Shell_Callback_Data is new Callback_Data with record
      Script          : Shell_Scripting;
      CL              : Arg_List;
      Return_Value    : GNAT.Strings.String_Access;
      Return_Dict     : GNAT.Strings.String_Access;
      Return_As_List  : Boolean := False;
      Return_As_Error : Boolean := False;
   end record;

   overriding function Clone
     (Data : Shell_Callback_Data) return Callback_Data'Class;
   overriding function Get_Script
     (Data : Shell_Callback_Data) return Scripting_Language;
   overriding function Number_Of_Arguments
     (Data : Shell_Callback_Data) return Natural;
   overriding procedure Name_Parameters
     (Data  : in out Shell_Callback_Data; Names : Cst_Argument_List);
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return String;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Unbounded_String;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Integer;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Float;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Boolean;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Subprogram_Type;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False) return Class_Instance;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive)
      return List_Instance'Class;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive)
      return Dictionary_Instance'Class;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Default : String)
      return String;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Default : Integer)
      return Integer;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Default : Float)
      return Float;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Default : Boolean)
      return Boolean;
   overriding function Nth_Arg
     (Data       : Shell_Callback_Data;
      N          : Positive;
      Class      : Class_Type := Any_Class;
      Default    : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance;
   overriding function Nth_Arg
     (Data    : Shell_Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type;
   overriding procedure Set_Error_Msg
     (Data : in out Shell_Callback_Data; Msg : String);
   overriding procedure Set_Return_Value_As_List
     (Data  : in out Shell_Callback_Data;
      Size  : Natural := 0;
      Class : Class_Type := No_Class);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Integer);
   overriding procedure Set_Address_Return_Value
     (Data   : in out Shell_Callback_Data; Value : System.Address);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Float);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Boolean);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : String);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Class_Instance);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : List_Instance);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : String;
      Append : Boolean := False);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : Integer;
      Append : Boolean := False);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False);
   overriding procedure Free (Data : in out Shell_Callback_Data);
   overriding function Create
     (Script          : access Shell_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   overriding procedure Set_Nth_Arg
     (Data : in out Shell_Callback_Data; N : Positive; Value : String);
   overriding procedure Set_Nth_Arg
     (Data : in out Shell_Callback_Data; N : Positive; Value : Integer);
   overriding procedure Set_Nth_Arg
     (Data : in out Shell_Callback_Data; N : Positive; Value : Float);
   overriding procedure Set_Nth_Arg
     (Data : in out Shell_Callback_Data; N : Positive; Value : Boolean);
   overriding procedure Set_Nth_Arg
     (Data  : in out Shell_Callback_Data;
      N     : Positive;
      Value : Class_Instance);
   overriding procedure Set_Nth_Arg
     (Data  : in out Shell_Callback_Data;
      N     : Positive;
      Value : List_Instance);
   overriding procedure Set_Nth_Arg
     (Data  : in out Shell_Callback_Data;
      N     : Positive;
      Value : Subprogram_Type);
   overriding procedure Execute_Command
     (Args    : in out Shell_Callback_Data;
      Command : String;
      Hide_Output : Boolean := True);
   overriding function Return_Value
     (Data : Shell_Callback_Data) return String;
   overriding function Return_Value
     (Data : Shell_Callback_Data) return Integer;
   overriding function Return_Value
     (Data : Shell_Callback_Data) return Float;
   overriding function Return_Value
     (Data : Shell_Callback_Data) return Boolean;
   overriding function Return_Value
     (Data : Shell_Callback_Data) return Class_Instance;
   overriding function Return_Value
     (Data : Shell_Callback_Data) return List_Instance'Class;
   overriding procedure Execute_Expression
     (Result      : in out Shell_Callback_Data;
      Expression  : String;
      Hide_Output : Boolean := True);
   --  See doc from inherited subprogram

   ----------------------
   -- Shell_Subprogram --
   ----------------------

   type Shell_Subprogram_Record is new Subprogram_Record with record
      Script  : Scripting_Language;
      Command : GNAT.Strings.String_Access;
   end record;
   --  subprograms in GPS shell are just GPS actions

   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return Boolean;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return String;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return Class_Instance;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return List_Instance'Class;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return GNAT.Strings.String_List;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return Any_Type;
   overriding procedure Free (Subprogram : in out Shell_Subprogram_Record);
   overriding function Get_Name
     (Subprogram : access Shell_Subprogram_Record) return String;
   overriding function Get_Script
     (Subprogram : Shell_Subprogram_Record) return Scripting_Language;
   --  See doc from inherited subprograms

end GNATCOLL.Scripts.Shell;
