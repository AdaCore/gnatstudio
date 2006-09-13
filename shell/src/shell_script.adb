------------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2006                       --
--                            AdaCore                                --
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

--  This package contains the implementation for a specific scripting language,
--  the simple GPS shell.

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_Image;
with System;                    use System;

with GNAT.Debug_Utilities;      use GNAT.Debug_Utilities;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Object;               use Glib.Object;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;

with Generic_List;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel;                use GPS.Kernel;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Histories;                 use Histories;
with Interactive_Consoles;      use Interactive_Consoles;
with String_List_Utils;         use String_List_Utils;
with String_Hash;
with Traces;                    use Traces;
with Basic_Types;               use Basic_Types;
with String_Utils;              use String_Utils;
with OS_Utils;                  use OS_Utils;

package body Shell_Script is

   Me : constant Debug_Handle := Create ("Shell_Script", Off);

   Num_Previous_Returns : constant := 9;
   --  Number of parameters %1, %2,... which are used to memorize the result of
   --  previous commands.

   type Shell_Console_Record
      is new Interactive_Console_Record with null record;
   type Shell_Console is access all Shell_Console_Record'Class;
   --  The shell console. This is mostly use to have a unique tag when saving
   --  the console.

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);

   --------------------------
   -- Shell_Class_Instance --
   --------------------------

   type Shell_Class_Instance_Record is new Class_Instance_Record with record
      Class : Class_Type;
   end record;
   type Shell_Class_Instance is access all Shell_Class_Instance_Record'Class;

   function Print_Refcount
     (Instance : access Shell_Class_Instance_Record) return String;
   function Is_Subclass
     (Instance : access Shell_Class_Instance_Record;
      Base     : Class_Type) return Boolean;
   --  See doc from inherited subprogram

   procedure Free_Instance (Inst : in out Shell_Class_Instance);
   package Instances_List is new Generic_List
     (Shell_Class_Instance, Free_Instance);
   use Instances_List;
   --  ??? Would be faster to use a hash-table...

   ---------------------
   -- Shell_scripting --
   ---------------------

   type Shell_Scripting_Record is new Scripting_Language_Record with record
      Kernel    : GPS.Kernel.Kernel_Handle;
      Blocked   : Boolean := False;
      Console   : Shell_Console;
      Instances : Instances_List.List;
      --  All the instances that were created

      Returns   : Argument_List (1 .. Num_Previous_Returns);
      --  The result of the Num_Previous_Returns previous commands
   end record;
   type Shell_Scripting is access all Shell_Scripting_Record'Class;

   procedure Destroy (Script : access Shell_Scripting_Record);
   procedure Register_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   procedure Register_Class
     (Script        : access Shell_Scripting_Record;
      Name          : String;
      Base          : Class_Type := No_Class);
   procedure Block_Commands
     (Script : access Shell_Scripting_Record; Block : Boolean);
   procedure Execute_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Hide_Output   : Boolean := False;
      Show_Command  : Boolean := True;
      Errors        : out Boolean);
   function Execute_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Hide_Output   : Boolean := False;
      Show_Command  : Boolean := True;
      Errors        : access Boolean) return String;
   function Execute_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Hide_Output   : Boolean := False;
      Errors        : access Boolean) return Boolean;
   function Execute_Command
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   function Execute_Command_With_Args
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Args          : GNAT.OS_Lib.Argument_List) return String;
   procedure Execute_File
     (Script        : access Shell_Scripting_Record;
      Filename      : String;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Hide_Output   : Boolean := False;
      Errors        : out Boolean);
   function Get_Name (Script : access Shell_Scripting_Record) return String;
   function Get_Kernel
     (Script : access Shell_Scripting_Record)
      return Kernel_Handle;
   function Current_Script
     (Script : access Shell_Scripting_Record) return String;
   --  See doc from inherited subprograms

   function New_Instance
     (Script : access Shell_Scripting_Record; Class : Class_Type)
      return Class_Instance;

   ----------------------
   -- Shell_Subprogram --
   ----------------------

   type Shell_Subprogram_Record is new Subprogram_Record with record
      Action_Name : GNAT.OS_Lib.String_Access;
   end record;
   --  subprograms in GPS shell are just GPS actions

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean;
   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return String;
   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return GNAT.OS_Lib.String_List;
   procedure Free (Subprogram : in out Shell_Subprogram_Record);
   function Get_Name
     (Subprogram : access Shell_Subprogram_Record) return String;
   function Get_Script
     (Subprogram : Shell_Subprogram_Record) return Scripting_Language;
   --  See doc from inherited subprograms

   -------------------------
   -- Shell_Callback_Data --
   -------------------------

   type Shell_Callback_Data is new Callback_Data with record
      Script          : Shell_Scripting;
      Args            : GNAT.OS_Lib.Argument_List_Access;
      Return_Value    : GNAT.OS_Lib.String_Access;
      Return_Dict     : GNAT.OS_Lib.String_Access;
      Return_As_List  : Boolean := False;
      Return_As_Error : Boolean := False;
   end record;

   function Clone (Data : Shell_Callback_Data) return Callback_Data'Class;
   function Get_Script (Data : Shell_Callback_Data) return Scripting_Language;
   function Number_Of_Arguments (Data : Shell_Callback_Data) return Natural;
   procedure Name_Parameters
     (Data  : in out Shell_Callback_Data; Names : Cst_Argument_List);
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return String;
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return Integer;
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return Boolean;
   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Subprogram_Type;
   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance;
   procedure Set_Error_Msg (Data : in out Shell_Callback_Data; Msg : String);
   procedure Set_Return_Value_As_List
     (Data : in out Shell_Callback_Data; Size : Natural := 0);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Integer);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Boolean);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : String);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Class_Instance);
   procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : String;
      Append : Boolean := False);
   procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : Integer;
      Append : Boolean := False);
   procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False);
   procedure Free (Data : in out Shell_Callback_Data);
   function Create
     (Script          : access Shell_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : String);
   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Integer);
   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Boolean);
   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Class_Instance);
   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Subprogram_Type);
   --  See doc from inherited subprogram

   -------------------------
   -- Command_Information --
   -------------------------

   type Command_Information is record
      Command         : GNAT.OS_Lib.String_Access;
      Short_Command   : GNAT.OS_Lib.String_Access;
      Minimum_Args    : Natural;
      Maximum_Args    : Natural;
      Command_Handler : Module_Command_Function;
      Class           : Class_Type;
   end record;
   type Command_Information_Access is access Command_Information;
   --  Description for each of the registered commands.
   --  Command is the name that must be typed by the user in the console.
   --  Short_Command is the name under which the command was registered. It is
   --  the same as Command, except when the command is a method of a class. In
   --  this case, Command is equal to "Class.Short_Command"
   --  The command was set as a constructor if Short_Command is
   --  Constructor_Method.

   procedure Free (X : in out Command_Information_Access);
   --  Free memory associated with X.

   package Command_Hash is new String_Hash
     (Command_Information_Access, Free, null);
   use Command_Hash.String_Hash_Table;

   type Shell_Module_Id_Record is new Module_ID_Record with record
      Script        : Shell_Scripting;
      Commands_List : Command_Hash.String_Hash_Table.HTable;
      --  The list of all registered commands
   end record;
   type Shell_Module_Id_Access is access all Shell_Module_Id_Record;

   procedure Destroy (Module : in out Shell_Module_Id_Record);
   --  Free the memory associated with the module

   Shell_Module_Id : Shell_Module_Id_Access;

   procedure Module_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Internal handler for the shell functions defined in this module

   function Commands_As_List
     (Prefix : String;
      Kernel : System.Address)
      return String_List_Utils.String_List.List;
   --  Return the list of commands. The list must be freed by the caller.

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      Kernel  : System.Address) return String;
   --  Launch the command interpreter for Input and return the output.

   function Name_From_Instance (Instance : Class_Instance) return String;
   --  Return the string to display to report the instance in the shell

   function Instance_From_Name
     (Script : access Shell_Scripting_Record'Class;
      Name : String) return Shell_Class_Instance;
   --  Opposite of Name_From_Instance

   function Instance_From_Address
     (Script : access Shell_Scripting_Record'Class;
      Add : System.Address) return Shell_Class_Instance;
   --  Return an instance from its address

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Errors  : access Boolean) return String;
   --  n a command in the GPS shell and returns its result.
   --  Command might be a series of commands, separated by semicolons or
   --  newlines. The return value is the result of the last command.

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List;
      Errors  : access Boolean) return String;
   --  Execute a command in the GPS shell and returns its result.
   --  Command must be a single command (no semicolon-separated list).

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Support functions for saving the desktop

   function Get_Or_Create_Console (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child;
   --  Return a handle to the shell console, or create a new one if necessary

   procedure Open_Shell_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a new shell console (if none exists). This is a callback for the
   --  menu bar items.

   procedure Console_Destroyed
     (Console : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the console is destroyed

   -------------------
   -- Free_Instance --
   -------------------

   procedure Free_Instance (Inst : in out Shell_Class_Instance) is
      pragma Unreferenced (Inst);
   begin
      null;
   end Free_Instance;

   --------------------
   -- Block_Commands --
   --------------------

   procedure Block_Commands
     (Script : access Shell_Scripting_Record; Block : Boolean) is
   begin
      Script.Blocked := Block;
   end Block_Commands;

   ------------------------
   -- Name_From_Instance --
   ------------------------

   function Name_From_Instance (Instance : Class_Instance) return String is
   begin
      return '<' & Get_Name (Shell_Class_Instance (Get_CIR (Instance)).Class)
        & "_0x" & System.Address_Image (Get_CIR (Instance).all'Address)
        & '>';
   end Name_From_Instance;

   ------------------------
   -- Instance_From_Name --
   ------------------------

   function Instance_From_Name
     (Script : access Shell_Scripting_Record'Class;
      Name   : String) return Shell_Class_Instance
   is
      Index : Natural := Name'First;
   begin
      if Name = "null" then
         return null;
      end if;

      while Index <= Name'Last - 3
        and then Name (Index .. Index + 2) /= "_0x"
      loop
         Index := Index + 1;
      end loop;

      return Instance_From_Address
        (Script, Value ("16#" & Name (Index + 3 .. Name'Last - 1) & "#"));

   exception
      when others =>
         --  Invalid instance
         return null;
   end Instance_From_Name;

   ---------------------------
   -- Instance_From_Address --
   ---------------------------

   function Instance_From_Address
     (Script : access Shell_Scripting_Record'Class;
      Add    : System.Address) return Shell_Class_Instance
   is
      L : List_Node := First (Script.Instances);
   begin
      while L /= Null_Node loop
         if Instances_List.Data (L).all'Address = Add then
            return Instances_List.Data (L);
         end if;

         L := Next (L);
      end loop;
      return null;
   end Instance_From_Address;

   ----------------------
   -- Commands_As_List --
   ----------------------

   function Commands_As_List
     (Prefix : String;
      Kernel : System.Address)
      return String_List_Utils.String_List.List
   is
      pragma Unreferenced (Kernel);
      use String_List_Utils.String_List;
      L       : String_List_Utils.String_List.List :=
                  String_List_Utils.String_List.Null_List;
      Current : Command_Hash.String_Hash_Table.Iterator;
      Info    : Command_Information_Access;
   begin
      Get_First (Shell_Module_Id.Commands_List, Current);
      loop
         Info := Get_Element (Current);
         exit when Info = null;

         declare
            S : constant String := Info.Command.all;
         begin
            if S'Length >= Prefix'Length
              and then S (S'First .. S'First + Prefix'Length - 1) = Prefix
            then
               Prepend (L, S);
            end if;
         end;

         Get_Next (Shell_Module_Id.Commands_List, Current);
      end loop;

      Sort (L);

      return L;
   end Commands_As_List;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Shell_Module_Id_Record) is
   begin
      Command_Hash.String_Hash_Table.Reset (Module.Commands_List);
      Shell_Module_Id := null;
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Information_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Command_Information, Command_Information_Access);
   begin
      Free (X.Command);
      Free (X.Short_Command);
      Unchecked_Free (X);
   end Free;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Instance : access Shell_Class_Instance_Record;
      Base     : Class_Type) return Boolean
   is
      pragma Unreferenced (Instance, Base);
   begin
      --  ??? Not checked
      return True;
   end Is_Subclass;

   -----------------------
   -- Console_Destroyed --
   -----------------------

   procedure Console_Destroyed
     (Console : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Console);
      Script : constant Shell_Scripting := Shell_Scripting
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name));
   begin
      Script.Console := null;
   end Console_Destroyed;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      Kernel  : System.Address) return String
   is
      pragma Unreferenced (Console);
      K : constant Kernel_Handle := Convert (Kernel);
   begin
      declare
         Errors : aliased Boolean;
         S : constant String := Execute_GPS_Shell_Command
           (K, Input, Errors'Unchecked_Access);
      begin
         --  Preserver the focus on the console after an interactive execution
         Set_Focus_Child (Get_Or_Create_Console (K));

         if S = ""
           or else S (S'Last) = ASCII.LF
           or else S (S'Last) = ASCII.CR
         then
            return S;
         else
            return S & ASCII.LF;
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return "";
   end Interpret_Command_Handler;

   ---------------------
   -- Name_Parameters --
   ---------------------

   procedure Name_Parameters
     (Data  : in out Shell_Callback_Data; Names : Cst_Argument_List)
   is
      pragma Unreferenced (Data, Names);
   begin
      null;
   end Name_Parameters;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Shell_Console" then
         return Get_Or_Create_Console (User);
      end if;
      return null;
   end Load_Desktop;

   ---------------------------
   -- Get_Or_Create_Console --
   ---------------------------

   function Get_Or_Create_Console (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child
   is
      Child  : GPS_MDI_Child;
      Script : constant Shell_Scripting := Shell_Scripting
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name));
   begin
      if Script.Console = null then
         Script.Console := new Shell_Console_Record;
         Initialize
           (Script.Console,
            "GPS> ",
            Interpret_Command_Handler'Access,
            Kernel.all'Address,
            Get_Pref_Font (Default_Style),
            History_List => Get_History (Kernel),
            Key          => "shell",
            Wrap_Mode    => Wrap_Char);
         Set_Completion_Handler (Script.Console, Commands_As_List'Access);
         Gtk_New (Child, Script.Console,
                  Default_Width      => 400,
                  Default_Height     => 120,
                  Focus_Widget       => Gtk_Widget (Get_View (Script.Console)),
                  Group              => Group_Consoles,
                  Module             => Shell_Module_Id,
                  Desktop_Independent => True);
         Set_Title (Child, -"Shell");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);

         Kernel_Callback.Connect
           (Script.Console, "destroy", Console_Destroyed'Access,
            Kernel_Handle (Kernel));
      else
         Child := GPS_MDI_Child
           (Find_MDI_Child (Get_MDI (Kernel), Script.Console));
      end if;

      Raise_Child (Child);
      return MDI_Child (Child);
   end Get_Or_Create_Console;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Shell_Console_Record'Class then
         N := new Node;
         N.Tag := new String'("Shell_Console");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ------------------------
   -- Open_Shell_Console --
   ------------------------

   procedure Open_Shell_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child : MDI_Child;
      pragma Unreferenced (Widget, Child);
   begin
      Child := Get_Or_Create_Console (Kernel);
   end Open_Shell_Console;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Script : Shell_Scripting;
   begin
      Shell_Module_Id := new Shell_Module_Id_Record;
      Register_Module
        (Module      => Module_ID (Shell_Module_Id),
         Kernel      => Kernel,
         Module_Name => "Shell script",
         Priority    => GPS.Kernel.Modules.Default_Priority);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Script := new Shell_Scripting_Record;
      Script.Kernel := Kernel_Handle (Kernel);
      Register_Scripting_Language (Kernel, Script);
      Shell_Module_Id.Script := Script;

      Register_Menu
        (Kernel,
         Parent_Path => "/" & (-"_Tools") & '/' & (-"_Consoles"),
         Text        => -"_GPS Shell",
         Callback    => Open_Shell_Console'Access);

      --  Only remember the last 100 commands.
      Set_Max_Length (Get_History (Kernel).all, 100, "shell");
      Allow_Duplicates (Get_History (Kernel).all, "shell", True, True);

      --  The following commands are specific to the GPS shell script.
      Register_Command
        (Script, "help",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);
      Register_Command
        (Script, "echo",
         Minimum_Args => 0,
         Maximum_Args => Natural'Last,
         Handler      => Module_Command_Handler'Access);
      Register_Command
        (Script, "load",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);
      Register_Command
        (Script, "clear_cache",
         Handler => Module_Command_Handler'Access);
   end Register_Module;

   ----------------------------
   -- Module_Command_Handler --
   ----------------------------

   procedure Module_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use String_List_Utils.String_List;

      L      : String_List_Utils.String_List.List;
      L2     : String_List_Utils.String_List.List_Node;
      Result : GNAT.OS_Lib.String_Access := new String'("");
      Kernel : constant Kernel_Handle := Get_Kernel (Data);

      procedure Insert (S : String; Separator : Character := ASCII.LF);
      --  Appends S & Separator to Result.
      --  Result must not be set to Null when calling this subprogram.
      --  If the Separator is ASCII.NUL, nothing is added

      ------------
      -- Insert --
      ------------

      procedure Insert (S : String; Separator : Character := ASCII.LF) is
      begin
         if Separator = ASCII.NUL then
            declare
               R : constant String := Result.all & S;
            begin
               Free (Result);
               Result := new String'(R);
            end;
         else
            declare
               R : constant String := Result.all & S & Separator;
            begin
               Free (Result);
               Result := new String'(R);
            end;
         end if;
      end Insert;

   begin
      if Command = "help" then
         if Number_Of_Arguments (Data) = 0 then
            Insert (-"The following commands are defined:");

            L := Commands_As_List ("", System.Null_Address);
            String_List_Utils.Sort (L);

            L2 := First (L);
            while L2 /= String_List_Utils.String_List.Null_Node loop
               declare
                  Name : constant String :=
                    String_List_Utils.String_List.Data (L2);
               begin
                  if Name'Length > 2
                    and then Name (Name'First .. Name'First + 1) /= "__"
                  then
                     Insert (" " & Name);
                  end if;
               end;

               L2 := String_List_Utils.String_List.Next (L2);
            end loop;

            Free (L);

            Insert
              (-"Type ""help <cmd>"" to get help about a specific command.");

         else
            declare
               Errors : aliased Boolean;
               Usage  : constant String := Execute_GPS_Shell_Command
                 (Kernel  => Kernel,
                  Command =>
                    "Help; Help.getdoc %1 GPS." & Nth_Arg (Data, 1),
                  Errors  => Errors'Unchecked_Access);

               --  Needs to be executed separately, or we wouldn't get output
               --  in Usage
               Ignored : constant String := Execute_GPS_Shell_Command
                 (Kernel  => Kernel,
                  Command => "Help.reset %2",
                  Errors  => Errors'Unchecked_Access);
               pragma Unreferenced (Ignored);
            begin
               Insert (-("Usage: ") & Nth_Arg (Data, 1) & ASCII.LF & Usage);
            end;
         end if;

         Set_Return_Value (Data, Result.all);
         Free (Result);

      elsif Command = "load" then
         declare
            Filename : constant String := Nth_Arg (Data, 1);
            Buffer   : GNAT.OS_Lib.String_Access := Read_File (Filename);
            Errors   : Boolean;
         begin
            if Buffer /= null then
               Execute_Command
                 (Get_Script (Data), Buffer.all,
                  Errors => Errors);
               Free (Buffer);
            else
               Set_Error_Msg (Data, -"File not found: """ & Filename & '"');
            end if;
         end;

      elsif Command = "echo" then
         for A in 1 .. Number_Of_Arguments (Data) loop
            if A = Number_Of_Arguments (Data) then
               Insert (Nth_Arg (Data, A), ASCII.NUL);
            else
               Insert (Nth_Arg (Data, A), ' ');
            end if;
         end loop;

         Set_Return_Value (Data, Result.all);
         Free (Result);

      elsif Command = "clear_cache" then
         Free (Shell_Scripting (Get_Script (Data)).Instances,
               Free_Data => True);
      end if;
   end Module_Command_Handler;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False)
   is
      pragma Unreferenced (Script);
      Cmd  : GNAT.OS_Lib.String_Access;
      Min  : Natural := Minimum_Args;
      Max  : Natural := Maximum_Args;
      Info : Command_Information_Access;
   begin
      if Command = "" or else Shell_Module_Id = null then
         return;
      end if;

      if Class /= No_Class then
         if Command = Constructor_Method then
            Cmd := new String'(Get_Name (Class));

         elsif Command = Destructor_Method then
            Cmd := new String'(Get_Name (Class) & ".__delete");

         else
            Cmd := new String'(Get_Name (Class) & "." & Command);
            --  First parameter is always the instance

            if not Static_Method then
               Min := Min + 1;
               if Max /= Natural'Last then
                  Max := Max + 1;
               end if;
            end if;
         end if;
      else
         Cmd := new String'(Command);
      end if;

      Info := Get (Shell_Module_Id.Commands_List, Cmd.all);

      --  Check that the command is not already registered.

      if Info /= null then
         Trace
           (Me, "Interactive command " & Cmd.all & " is already registered");
         return;
      end if;

      Info := new Command_Information'
        (Command         => Cmd,
         Short_Command   => new String'(Command),
         Minimum_Args    => Min,
         Maximum_Args    => Max,
         Class           => Class,
         Command_Handler => Handler);

      Set (Shell_Module_Id.Commands_List, Cmd.all, Info);
   end Register_Command;

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class
     (Script : access Shell_Scripting_Record;
      Name   : String;
      Base   : Class_Type := No_Class)
   is
      pragma Unreferenced (Script, Name, Base);
   begin
      --   Classes not supported in the shell module
      null;
   end Register_Class;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script       : access Shell_Scripting_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean)
   is
      pragma Unreferenced (Show_Command);
      Err : aliased Boolean;
      S   : constant String := Execute_GPS_Shell_Command
        (Script.Kernel, Command, Err'Unchecked_Access);
   begin
      Errors := Err;
      if not Hide_Output then
         if Console = null then
            if Script.Console /= null then
               Insert (Script.Console, S);
            end if;
         else
            Insert (Console, S);
         end if;
      end if;
   end Execute_Command;

   -------------------------------
   -- Execute_Command_With_Args --
   -------------------------------

   function Execute_Command_With_Args
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      Errors : aliased Boolean;
   begin
      return Execute_GPS_Shell_Command
        (Script.Kernel, Command, Args, Errors'Unchecked_Access);
   end Execute_Command_With_Args;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File
     (Script      : access Shell_Scripting_Record;
      Filename    : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : out Boolean)
   is
      Err  : aliased Boolean;
      Args : Argument_List := (1 => new String'(Filename));
   begin
      Trace (Me, "Execute_File: load " & Filename);

      declare
         S : constant String := Execute_GPS_Shell_Command
           (Script.Kernel, "load", Args, Err'Unchecked_Access);
      begin
         Errors := Err;
         if not Hide_Output then
            if Console = null then
               if Script.Console /= null then
                  Insert (Script.Console, S);
               end if;
            else
               Insert (Console, S);
            end if;
         end if;

         Free (Args);
      end;
   end Execute_File;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Script : access Shell_Scripting_Record) return String is
      pragma Unreferenced (Script);
   begin
      return GPS_Shell_Name;
   end Get_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Script : access Shell_Scripting_Record) is
   begin
      Free (Script.Returns);
   end Destroy;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script       : access Shell_Scripting_Record;
      Command      : String;
      Console      : Interactive_Consoles.Interactive_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String
   is
      pragma Unreferenced (Show_Command);
      Err    : aliased Boolean;
      Result : constant String := Execute_GPS_Shell_Command
        (Script.Kernel, Command, Err'Unchecked_Access);

   begin
      Errors.all := Err;

      if not Hide_Output then
         if Console = null then
            if Script.Console /= null then
               Insert (Script.Console, Result);
            end if;
         else
            Insert (Console, Result);
         end if;
      end if;

      return Result;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script      : access Shell_Scripting_Record;
      Command     : String;
      Console     : Interactive_Consoles.Interactive_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean
   is
      Err    : aliased Boolean;
      Result : constant String := Trim
        (Reduce (Execute_GPS_Shell_Command
                   (Script.Kernel, Command, Err'Unchecked_Access)),
         Ada.Strings.Both);

   begin
      Errors.all := Err;

      if not Hide_Output then
         if Console = null then
            if Script.Console /= null then
               Insert (Script.Console, Result);
            end if;
         else
            Insert (Console, Result);
         end if;
      end if;

      return Result = "1" or else Equal (Result, "true", False);
   end Execute_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List;
      Errors  : access Boolean) return String
   is
      Data     : Command_Information_Access;
      Callback : Shell_Callback_Data;
      Instance : Class_Instance;
      Start    : Natural;
      Shell    : Shell_Scripting;

   begin
      Trace (Me, "Executing command " & Command);

      Shell := Shell_Scripting
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name));
      if Shell.Blocked then
         Errors.all := True;
         Insert (Kernel, -"A command is already executing");
         return "";
      end if;

      Errors.all := False;

      if Shell_Module_Id = null then
         Errors.all := True;
         return -"Shell module not initialized";
      end if;

      Data := Get (Shell_Module_Id.Commands_List, Command);

      if Data /= null then
         if Data.Minimum_Args <= Args'Length
           and then Args'Length <= Data.Maximum_Args
         then
            Callback.Script := Shell;

            if Data.Short_Command.all = Constructor_Method then
               Instance := New_Instance (Callback.Script, Data.Class);
               Callback.Args := new Argument_List (1 .. Args'Length + 1);
               Callback.Args (1) :=
                 new String'(Name_From_Instance (Instance));
               Start := 2;
            else
               Callback.Args := new Argument_List (1 .. Args'Length);
               Start := 1;
            end if;

            for A in Args'Range loop
               if Args (A)'Length > 0
                 and then Args (A) (Args (A)'First) = '%'
               then
                  declare
                     Num : Integer;
                  begin
                     Num := Integer'Value
                       (Args (A) (Args (A)'First + 1 .. Args (A)'Last));
                     Callback.Args (A - Args'First + Start) :=
                       new String'(Shell.Returns
                                     (Num + Shell.Returns'First - 1).all);

                  exception
                     when Constraint_Error =>
                        Callback.Args (A - Args'First + Start) :=
                          new String'(Args (A).all);
                  end;

               else
                  Callback.Args (A - Args'First + Start) :=
                    new String'(Args (A).all);
               end if;
            end loop;

            Data.Command_Handler (Callback, Data.Short_Command.all);
            Free (Callback.Args);

            if Callback.Return_As_Error then
               Errors.all := True;
               Free (Callback.Return_Dict);
               declare
                  R : constant String := Callback.Return_Value.all;
               begin
                  Free (Callback.Return_Value);
                  return R;
               end;
            end if;

            if Data.Short_Command.all = Constructor_Method then
               Set_Return_Value (Callback, Instance);
            end if;

            if Callback.Return_Dict /= null then
               Free (Callback.Return_Value);
               Callback.Return_Value := Callback.Return_Dict;
               Callback.Return_Dict  := null;
            end if;

            --  Save the return value for the future
            Free (Shell.Returns (Shell.Returns'Last));
            Shell.Returns
              (Shell.Returns'First + 1 .. Shell.Returns'Last) :=
              Shell.Returns
                (Shell.Returns'First .. Shell.Returns'Last - 1);

            if Callback.Return_Value = null then
               Shell.Returns (Shell.Returns'First) := new String'("");
            else
               Shell.Returns (Shell.Returns'First) :=
                 Callback.Return_Value;
            end if;

            if Callback.Return_Value = null then
               return "";
            else
               --  Do not free Callback.Return_Value, it is stored in the
               --  list of previous commands
               return Callback.Return_Value.all;
            end if;

         else
            Trace (Me, "Incorrect number of arguments for " & Command
                   & " Got" & Args'Length'Img
                   & " Expecting >=" & Data.Minimum_Args'Img
                   & " and <=" & Data.Maximum_Args'Img);
            return -"Incorrect number of arguments." & ASCII.LF
            & Data.Command.all & ' ';
         end if;
      end if;

      return -"Command not recognized";

   exception
      when Invalid_Parameter =>
         return -"Invalid parameter for " & Command;

      when E : others =>
         Trace (Exception_Handle, "Unexpected exception in Execute_Command: "
                & Exception_Information (E));
         return "";
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Errors  : access Boolean) return String
   is
      function Unprotect (Str : String) return String;
      --  Remove the \ protections in Str

      ---------------
      -- Unprotect --
      ---------------

      function Unprotect (Str : String) return String is
         Result : String (Str'Range);
         Index  : Natural := Result'First;
         S      : Natural := Str'First;
      begin
         while S <= Str'Last loop
            if Str (S) = '\' then
               if S < Str'Last then
                  Result (Index) := Str (S + 1);
               end if;

               S := S + 2;
            else
               Result (Index) := Str (S);
               S := S + 1;
            end if;

            Index := Index + 1;
         end loop;

         return Result (Result'First .. Index - 1);
      end Unprotect;

      Args          : Argument_List_Access;
      First, Last   : Integer;
      Tmp           : GNAT.OS_Lib.String_Access;
      Quoted        : Boolean;
      Triple_Quoted : Boolean;
   begin
      Trace (Me, "Execute_GPS_Shell_Command: " & Command);

      Errors.all := False;

      if Command /= "" then
         First := Command'First;
         while First <= Command'Last loop
            while First <= Command'Last
              and then (Command (First) = ' '
                        or else Command (First) = ASCII.HT)
            loop
               First := First + 1;
            end loop;

            if First > Command'Last then
               exit;
            end if;

            Last := First;
            Quoted := False;
            Triple_Quoted := False;

            --  Search until the beginning of the next command (separated by
            --  semicolon or newline).
            while Last <= Command'Last loop
               exit when not Quoted
                 and then not Triple_Quoted
                 and then (Command (Last) = ';'
                           or else Command (Last) = ASCII.LF);

               if Command (Last) = '"' then
                  if Last <= Command'Last - 2
                    and then Command (Last + 1) = '"'
                    and then Command (Last + 2) = '"'
                  then
                     Triple_Quoted := not Triple_Quoted;
                     Last := Last + 2;
                  elsif not Triple_Quoted then
                     Quoted := not Quoted;
                  end if;

               elsif Command (Last) = '\'
                 and then Last < Command'Last
               then
                  Last := Last + 1;
               end if;

               Last := Last + 1;
            end loop;

            Args := Argument_String_To_List_With_Triple_Quotes
              (Command (First .. Last - 1));

            if Args = null or else Args'Length = 0 then
               Trace (Me, "Couldn't parse argument string for "
                      & Command (First .. Last - 1));
            else
               --  Cleanup the arguments to remove unnecessary quoting
               for J in Args'Range loop
                  if Args (J) (Args (J)'First) = '"'
                    and then Args (J) (Args (J)'Last) = '"'
                  then
                     Tmp := Args (J);
                     Args (J) := new String'
                       (Unprotect (Tmp (Tmp'First + 1 .. Tmp'Last - 1)));
                     Free (Tmp);
                  end if;
               end loop;

               Trace (Me, "Executing command "
                      & Command (First .. Last - 1));

               declare
                  R : constant String := Execute_GPS_Shell_Command
                    (Kernel,
                     Command => Args (Args'First).all,
                     Args    => Args (Args'First + 1 .. Args'Last),
                     Errors  => Errors);
               begin
                  Free (Args);

                  if Last > Command'Last then
                     return R;
                  end if;
               end;
            end if;

            First := Last + 1;
         end loop;

         return "";
      else
         return "";
      end if;
   end Execute_GPS_Shell_Command;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Data : Shell_Callback_Data)
      return Scripting_Language is
   begin
      return Scripting_Language (Data.Script);
   end Get_Script;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Script : access Shell_Scripting_Record)
      return Kernel_Handle is
   begin
      return Script.Kernel;
   end Get_Kernel;

   --------------------
   -- Current_Script --
   --------------------

   function Current_Script
     (Script : access Shell_Scripting_Record) return String
   is
      pragma Unreferenced (Script);
   begin
      return "<shell script>";
   end Current_Script;

   -------------------------
   -- Number_Of_Arguments --
   -------------------------

   function Number_Of_Arguments (Data : Shell_Callback_Data) return Natural is
   begin
      return Data.Args'Length;
   end Number_Of_Arguments;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Shell_Callback_Data) is
   begin
      if Data.Args /= null then
         Trace (Me, "MANU Freeing shell_callback_Data "
                & System.Address_Image (Data.Args.all'Address));
      end if;
      Free (Data.Args);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (Data : Shell_Callback_Data) return Callback_Data'Class is
   begin
      return Shell_Callback_Data'
        (Callback_Data with
         Args        => new Argument_List'(String_Utils.Clone (Data.Args.all)),
         Script      => Data.Script,
         Return_Value    => null,
         Return_Dict     => null,
         Return_As_List  => False,
         Return_As_Error => False);
   end Clone;

   ------------
   -- Create --
   ------------

   function Create
     (Script          : access Shell_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class
   is
      Data : constant Shell_Callback_Data :=
        (Callback_Data with
         Script          => Shell_Scripting (Script),
         Args            => new Argument_List (1 .. Arguments_Count),
         Return_Value    => null,
         Return_Dict     => null,
         Return_As_List  => False,
         Return_As_Error => False);
   begin
      return Data;
   end Create;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Subprogram_Type) is
   begin
      Free (Data.Args (N - 1 + Data.Args'First));
      Data.Args (N - 1 + Data.Args'First) :=
        new String'(Shell_Subprogram_Record (Value.all).Action_Name.all);
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : String) is
   begin
      Free (Data.Args (N - 1 + Data.Args'First));
      Data.Args (N - 1 + Data.Args'First) := new String'(Value);
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Integer) is
   begin
      Set_Nth_Arg (Data, N, Integer'Image (Value));
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Boolean) is
   begin
      Set_Nth_Arg (Data, N, Boolean'Image (Value));
   end Set_Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Class_Instance) is
   begin
      Set_Nth_Arg (Data, N, Name_From_Instance (Value));
   end Set_Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return String is
   begin
      if N > Data.Args'Last then
         raise No_Such_Parameter;
      else
         return Data.Args (N).all;
      end if;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Data : Shell_Callback_Data; N : Positive)
      return Boolean
   is
      S : constant String := Nth_Arg (Data, N);
   begin
      return Boolean'Value (S);
   exception
      when Constraint_Error =>
         raise Invalid_Parameter;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Integer
   is
      S : constant String := Nth_Arg (Data, N);
   begin
      return Integer'Value (S);
   exception
      when Constraint_Error =>
         raise Invalid_Parameter;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data       : Shell_Callback_Data;
      N          : Positive;
      Class      : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance
   is
      Ins : constant Shell_Class_Instance := Instance_From_Name
        (Data.Script, Nth_Arg (Data, N));
   begin
      if Ins = null and then Allow_Null then
         return No_Class_Instance;
      end if;

      if Ins = null or else not Is_Subclass (Ins, Class) then
         Trace (Me, "Instance not found: " & Nth_Arg (Data, N));
         raise Invalid_Parameter;
      else
         return From_Instance (Data.Script, Ins);
      end if;
   end Nth_Arg;

   -------------------
   -- Set_Error_Msg --
   -------------------

   procedure Set_Error_Msg (Data : in out Shell_Callback_Data; Msg : String) is
   begin
      Free (Data.Return_Value);
      Data.Return_As_Error := True;
      Data.Return_Value := new String'(Msg);
   end Set_Error_Msg;

   ------------------------------
   -- Set_Return_Value_As_List --
   ------------------------------

   procedure Set_Return_Value_As_List
     (Data : in out Shell_Callback_Data; Size : Natural := 0)
   is
      pragma Unreferenced (Size);
   begin
      Data.Return_As_List := True;
   end Set_Return_Value_As_List;

   --------------------------
   -- Set_Return_Value_Key --
   --------------------------

   procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : String;
      Append : Boolean := False)
   is
      pragma Unreferenced (Append);
      Tmp : GNAT.OS_Lib.String_Access;
   begin
      if Data.Return_Value = null then
         if Data.Return_Dict = null then
            Data.Return_Dict := new String'(Key & " => ()");
         else
            Tmp := Data.Return_Dict;
            Data.Return_Dict := new String'(Tmp.all & ", " & Key & " => ()");
            Free (Tmp);
         end if;

      else
         if Data.Return_Dict = null then
            Data.Return_Dict := new String'
              (Key & " => (" & Data.Return_Value.all & ')');
         else
            Tmp := Data.Return_Dict;
            Data.Return_Dict := new String'
              (Tmp.all & ", " & Key & " => (" & Data.Return_Value.all & ')');
            Free (Tmp);
         end if;
      end if;

      Data.Return_As_List := False;
      Free (Data.Return_Value);
   end Set_Return_Value_Key;

   --------------------------
   -- Set_Return_Value_Key --
   --------------------------

   procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : Integer;
      Append : Boolean := False) is
   begin
      Set_Return_Value_Key (Data, Integer'Image (Key), Append);
   end Set_Return_Value_Key;

   --------------------------
   -- Set_Return_Value_Key --
   --------------------------

   procedure Set_Return_Value_Key
     (Data   : in out Shell_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False) is
   begin
      Set_Return_Value_Key (Data, Name_From_Instance (Key), Append);
   end Set_Return_Value_Key;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Shell_Callback_Data; Value : Integer) is
   begin
      if not Data.Return_As_List then
         Free (Data.Return_Value);
      end if;

      Set_Return_Value (Data, String_Utils.Image (Value));
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Shell_Callback_Data; Value : Boolean) is
   begin
      if not Data.Return_As_List then
         Free (Data.Return_Value);
      end if;

      Set_Return_Value (Data, Boolean'Image (Value));
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Shell_Callback_Data; Value : String)
   is
      Tmp : GNAT.OS_Lib.String_Access;
   begin
      if Data.Return_As_List and then Data.Return_Value /= null then
         Tmp := Data.Return_Value;

         Data.Return_Value := new String (1 .. Tmp'Length + 1 + Value'Length);
         Data.Return_Value (1 .. Tmp'Length) := Tmp.all;
         Data.Return_Value (Tmp'Length + 1) := ASCII.LF;
         Data.Return_Value (Tmp'Length + 2 .. Data.Return_Value'Last) := Value;
         Free (Tmp);

      else
         Free (Data.Return_Value);
         Data.Return_Value := new String'(Value);
      end if;
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Shell_Callback_Data; Value : Class_Instance) is
   begin
      if Value = No_Class_Instance then
         Set_Return_Value (Data, "null");
      else
         Set_Return_Value (Data, Name_From_Instance (Value));
      end if;
   end Set_Return_Value;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance
     (Script : access Shell_Scripting_Record;
      Class  : Class_Type) return Class_Instance
   is
      Instance : Shell_Class_Instance;
   begin
      Instance := new Shell_Class_Instance_Record;
      Instance.Class := Class;
      Instances_List.Prepend (Script.Instances, Instance);
      return From_Instance (Script, Instance);
   end New_Instance;

   --------------------
   -- Print_Refcount --
   --------------------

   function Print_Refcount
     (Instance : access Shell_Class_Instance_Record) return String
   is
      pragma Unreferenced (Instance);
   begin
      return "";
   end Print_Refcount;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean
   is
      Errors : aliased Boolean;
      Result : constant String := Trim
        (Reduce
           (Execute_GPS_Shell_Command
            (Script.Kernel, Command & ' ' & Argument_List_To_Quoted_String
                 (Shell_Callback_Data (Args).Args.all),
               Errors'Unchecked_Access)),
         Ada.Strings.Both);
   begin
      if Script.Console /= null then
         Insert (Script.Console, Result);
      end if;
      return Result = "1" or else Equal (Result, "true", False);
   end Execute_Command;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean
   is
   begin
      return Equal (Execute (Subprogram, Args), "true",
                    Case_Sensitive => False);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return String
   is
      D    : constant Shell_Callback_Data := Shell_Callback_Data (Args);
      Custom : Command_Access;
      A    : constant Action_Record_Access := Lookup_Action
        (Get_Kernel (Args), Subprogram.Action_Name.all);
   begin
      Custom := Create_Proxy
        (A.Command,
         Context => (Event       => null,
                     Context     => No_Context,
                     Synchronous => True,
                     Dir         => null,
                     Args        => new Argument_List'(Clone (D.Args.all)),
                     Label       => null));

      Launch_Background_Command
        (Kernel          => Get_Kernel (Args),
         Command         => Custom,
         Active          => True,
         Show_Bar        => True,
         Queue_Id        => "",
         Destroy_On_Exit => True);

      --  ??? Should evaluate output properly, but we are in asynchronous mode
      --  ??? In fact, this is no longer true if we use Active set to False
      --  above, since we now know how to execute external actions
      --  synchronously
      return "";
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return GNAT.OS_Lib.String_List
   is
      pragma Unreferenced (Subprogram, Args);
   begin
      --  ??? We are in asynchronous mode, see Execute for String above.
      return (1 .. 0 => null);
   end Execute;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Subprogram : access Shell_Subprogram_Record) return String is
   begin
      return "action: " & Subprogram.Action_Name.all;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Subprogram : in out Shell_Subprogram_Record) is
   begin
      Free (Subprogram.Action_Name);
   end Free;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Subprogram_Type
   is
      A    : Action_Record_Access;
      Name : constant String := Nth_Arg (Data, N);
   begin
      A := Lookup_Action (Get_Kernel (Data), Name);
      if A = null then
         raise Invalid_Parameter;
      else
         return new Shell_Subprogram_Record'
           (Subprogram_Record with
            Action_Name => new String'(Name));
      end if;
   end Nth_Arg;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script
     (Subprogram : Shell_Subprogram_Record) return Scripting_Language
   is
      pragma Unreferenced (Subprogram);
   begin
      return Scripting_Language (Shell_Module_Id.Script);
   end Get_Script;

end Shell_Script;
