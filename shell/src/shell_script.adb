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

--  This package contains the implementation for a specific scripting language,
--  the simple GPS shell.

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Generic_List;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glib.Object;              use Glib.Object;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Glide_Kernel;             use Glide_Kernel;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.MDI;               use Gtkada.MDI;
with Histories;                use Histories;
with Interactive_Consoles;     use Interactive_Consoles;
with String_List_Utils;        use String_List_Utils;
with System.Address_Image;
with System;                   use System;
with Traces;                   use Traces;
with Basic_Types;              use Basic_Types;
with OS_Utils;                 use OS_Utils;
with String_Utils;             use String_Utils;

package body Shell_Script is

   Me : constant Debug_Handle := Create ("Shell_Script");

   Num_Previous_Returns : constant := 9;
   --  Number of parameters %1, %2,... which are used to memorize the result of
   --  previous commands.

   type Shell_Console_Record
      is new Interactive_Console_Record with null record;
   type Shell_Console is access all Shell_Console_Record'Class;
   --  The shell console. This is mostly use to have a unique tag when saving
   --  the console.

   -------------------
   -- Instance_Data --
   -------------------

   type Instance_Data_Type is (Strings, Object, Addresses, Integers);
   type Instance_Data (Data : Instance_Data_Type := Object) is record
      case Data is
         when Strings =>
            Str : GNAT.OS_Lib.String_Access;
         when Object =>
            Obj : Glib.Object.GObject;
         when Addresses =>
            Addr       : System.Address;
            On_Destroy : Destroy_Handler;
         when Integers =>
            Int : Integer;
      end case;
   end record;

   --------------------------
   -- Shell_Class_Instance --
   --------------------------

   type Shell_Class_Instance_Record is new Class_Instance_Record with record
      Class  : Class_Type;
      Script : Scripting_Language;
      Data   : Instance_Data;
   end record;
   type Shell_Class_Instance is access all Shell_Class_Instance_Record'Class;

   function Get_Class (Instance : access Shell_Class_Instance_Record)
      return Class_Type;
   function Get_Data (Instance : access Shell_Class_Instance_Record)
      return Glib.Object.GObject;
   function Get_Data
     (Instance : access Shell_Class_Instance_Record) return String;
   function Get_Data (Instance : access Shell_Class_Instance_Record)
      return System.Address;
   function Get_Data
     (Instance : access Shell_Class_Instance_Record) return Integer;
   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record;
      Value    : access Glib.Object.GObject_Record'Class);
   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record; Value : String);
   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record; Value : Integer);
   procedure Set_Data
     (Instance   : access Shell_Class_Instance_Record;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null);
   function Get_Script (Instance : access Shell_Class_Instance_Record)
      return Scripting_Language;
   procedure Primitive_Free (Instance : in out Shell_Class_Instance_Record);
   --  See doc from inherited subprogram

   procedure Free_Instance (Instance : in out Shell_Class_Instance);
   package Instances_List is new Generic_List
     (Shell_Class_Instance, Free_Instance);
   use Instances_List;
   --  ??? Would be faster to use a hash-table...

   ---------------------
   -- Shell_scripting --
   ---------------------

   type Shell_Scripting_Record is new Scripting_Language_Record with record
      Kernel  : Glide_Kernel.Kernel_Handle;
      Console : Shell_Console;
      Instances : Instances_List.List;
      --  All the instances that were created

      Returns : Argument_List (1 .. Num_Previous_Returns);
      --  The result of the Num_Previous_Returns previous commands
   end record;
   type Shell_Scripting is access all Shell_Scripting_Record'Class;

   procedure Destroy (Script : access Shell_Scripting_Record);
   procedure Register_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Params        : String := "";
      Return_Value  : String := "";
      Description   : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   procedure Register_Class
     (Script        : access Shell_Scripting_Record;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class);
   procedure Execute_Command
     (Script             : access Shell_Scripting_Record;
      Command            : String;
      Display_In_Console : Boolean := True);
   function Execute_Command
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   procedure Execute_File
     (Script             : access Shell_Scripting_Record;
      Filename           : String;
      Display_In_Console : Boolean := True);
   function Get_Name (Script : access Shell_Scripting_Record) return String;
   function Is_Subclass
     (Script : access Shell_Scripting_Record;
      Class  : Class_Type;
      Base   : Class_Type) return Boolean;
   function Get_Kernel (Script : access Shell_Scripting_Record)
      return Kernel_Handle;
   --  See doc from inherited subprograms

   function New_Instance
     (Script : access Shell_Scripting_Record; Class : Class_Type)
      return Class_Instance;

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

   function Get_Script (Data : Shell_Callback_Data) return Scripting_Language;
   function Number_Of_Arguments (Data : Shell_Callback_Data) return Natural;
   procedure Name_Parameters
     (Data  : in out Shell_Callback_Data; Names : Cst_Argument_List);
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return String;
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return Integer;
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return Boolean;
   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return System.Address;
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
     (Data   : in out Shell_Callback_Data; Value : System.Address);
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
   --  See doc from inherited subprogram

   -------------------------
   -- Command_Information --
   -------------------------

   type Command_Information is record
      Command         : GNAT.OS_Lib.String_Access;
      Short_Command   : GNAT.OS_Lib.String_Access;
      Usage           : GNAT.OS_Lib.String_Access;
      Description     : GNAT.OS_Lib.String_Access;
      Minimum_Args    : Natural;
      Maximum_Args    : Natural;
      Command_Handler : Module_Command_Function;
      Class           : Class_Type;
   end record;
   --  Description for each of the registered commands.
   --  Command is the name that must be typed by the user in the console.
   --  Short_Command is the name under which the command was registered. It is
   --  the same as Command, except when the command is a method of a class. In
   --  this case, Command is equal to "Class.Short_Command"
   --  The command was set as a constructor if Short_Command is
   --  Constructor_Method.

   procedure Free (X : in out Command_Information);
   --  Free memory associated with X.

   package Command_List is new Generic_List (Command_Information);
   --  ??? Would be faster to use a hash-table...

   type Shell_Module_Id_Record is new Module_ID_Record with record
      Commands_List : Command_List.List;
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
      Kernel : Glib.Object.GObject)
      return String_List_Utils.String_List.List;
   --  Return the list of commands. The list must be freed by the caller.

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String;
   --  Launch the command interpreter for Input and return the output.

   function Name_From_Instance
     (Instance : access Class_Instance_Record'Class) return String;
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
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String) return String;
   --  Execute a command in the GPS shell and returns its result.
   --  Command might be a series of commands, separated by semicolons or
   --  newlines. The return value is the result of the last command.

   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Execute a command in the GPS shell and returns its result.
   --  Command must be a single command (no semicolon-separated list).

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr;
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

   ------------------------
   -- Name_From_Instance --
   ------------------------

   function Name_From_Instance
     (Instance : access Class_Instance_Record'Class) return String is
   begin
      return '<' & Get_Name (Get_Class (Instance))
        & "_0x" & System.Address_Image (Instance.all'Address)
        & '>';
   end Name_From_Instance;

   ------------------------
   -- Instance_From_Name --
   ------------------------

   function Instance_From_Name
     (Script : access Shell_Scripting_Record'Class;
      Name : String) return Shell_Class_Instance
   is
      Index : Natural := Name'First;
   begin
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
      Add : System.Address) return Shell_Class_Instance
   is
      L   : List_Node := First (Script.Instances);
   begin
      while L /= Null_Node loop
         if Instances_List.Data (L).all'Address = Add then
            return Instances_List.Data (L);
         end if;

         L := Next (L);
      end loop;
      return null;
   end Instance_From_Address;

   -------------------
   -- Free_Instance --
   -------------------

   procedure Free_Instance (Instance : in out Shell_Class_Instance) is
   begin
      Free (Instance);
   end Free_Instance;

   ----------------------
   -- Commands_As_List --
   ----------------------

   function Commands_As_List
     (Prefix : String;
      Kernel : Glib.Object.GObject)
      return String_List_Utils.String_List.List
   is
      pragma Unreferenced (Kernel);
      use String_List_Utils.String_List;
      use type Command_List.List_Node;
      L       : String_List_Utils.String_List.List :=
        String_List_Utils.String_List.Null_List;
      Current : Command_List.List_Node :=
        Command_List.First (Shell_Module_Id.Commands_List);
   begin
      while Current /= Command_List.Null_Node loop
         declare
            S : constant String := Command_List.Data (Current).Command.all;
         begin
            if S'Length >= Prefix'Length
              and then S (S'First .. S'First + Prefix'Length - 1) = Prefix
            then
               Prepend (L, S);
            end if;
         end;
         Current := Command_List.Next (Current);
      end loop;

      return L;
   end Commands_As_List;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Shell_Module_Id_Record) is
   begin
      Command_List.Free (Module.Commands_List);
      Shell_Module_Id := null;
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Information) is
   begin
      Free (X.Command);
      Free (X.Short_Command);
      Free (X.Usage);
      Free (X.Description);
   end Free;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Script : access Shell_Scripting_Record;
      Class  : Class_Type;
      Base   : Class_Type) return Boolean
   is
      pragma Unreferenced (Script, Class, Base);
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
      Script : Shell_Scripting := Shell_Scripting
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name));
   begin
      Script.Console := null;
   end Console_Destroyed;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String
   is
      K : constant Kernel_Handle := Kernel_Handle (Kernel);
   begin
      declare
         S : constant String := Execute_GPS_Shell_Command (K, Input);
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      Child  : MDI_Child;
      Script : Shell_Scripting := Shell_Scripting
        (Lookup_Scripting_Language (Kernel, GPS_Shell_Name));
   begin
      if Script.Console = null then
         Script.Console := new Shell_Console_Record;
         Initialize
           (Script.Console,
            "GPS> ",
            Interpret_Command_Handler'Access,
            GObject (Kernel),
            Get_Pref (Kernel, Source_Editor_Font),
            History_List => Get_History (Kernel),
            Key          => "shell",
            Wrap_Mode    => Wrap_Char);
         Set_Completion_Handler (Script.Console, Commands_As_List'Access);
         Child := Put
           (Kernel, Script.Console,
            Focus_Widget        => Gtk_Widget (Get_View (Script.Console)),
            Default_Width       => 400,
            Default_Height      => 120,
            Module              => Shell_Module_Id,
            Desktop_Independent => True);
         Set_Title (Child, -"Shell");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);

         Kernel_Callback.Connect
           (Script.Console, "destroy",
            Kernel_Callback.To_Marshaller (Console_Destroyed'Access),
            Kernel_Handle (Kernel));
      else
         Child := Find_MDI_Child (Get_MDI (Kernel), Script.Console);
      end if;

      Raise_Child (Child);
      return Child;
   end Get_Or_Create_Console;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Script : Shell_Scripting;
      N      : Node_Ptr;
   begin
      Shell_Module_Id := new Shell_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Shell_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => "Shell script",
         Priority                => Glide_Kernel.Default_Priority);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Script := new Shell_Scripting_Record;
      Script.Kernel := Kernel_Handle (Kernel);
      Register_Scripting_Language (Kernel, Script);

      N     := new Node;
      N.Tag := new String'("Shell_Console");
      Add_Default_Desktop_Item
        (Kernel, N,
         10, 10,
         400, 120,
         "Shell", "Shell Console",
         Docked, Bottom,
         False, False);

      Register_Menu
        (Kernel,
         Parent_Path => "/" & (-"Tools"),
         Text        => -"GPS Shell Console",
         Callback    => Open_Shell_Console'Access);

      --  Only remember the last 100 commands.
      Set_Max_Length (Get_History (Kernel).all, 100, "shell");
      Allow_Duplicates (Get_History (Kernel).all, "shell", True, True);

      --  The following commands are specific to the GPS shell script.
      Register_Command
        (Script,
         Command      => "help",
         Description  => -"List recognized commands.",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Script,
         Command      => "echo",
         Description  => -"Display a line of text.",
         Minimum_Args => 0,
         Maximum_Args => Natural'Last,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Script,
         Command      => "load",
         Params       => "(filename)",
         Description  => -"Load and execute a script file.",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Script,
         Command      => "clear_cache",
         Description  => -"Free the internal cache used for return values.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => Module_Command_Handler'Access);
   end Register_Module;

   ----------------------------
   -- Module_Command_Handler --
   ----------------------------

   procedure Module_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use String_List_Utils.String_List;
      use type Command_List.List_Node;

      Command_Node : Command_List.List_Node;
      L            : String_List_Utils.String_List.List;
      L2           : String_List_Utils.String_List.List_Node;
      Result       : GNAT.OS_Lib.String_Access := new String'("");
      Kernel       : constant Kernel_Handle := Get_Kernel (Data);

      procedure Insert (S : String; Separator : Character := ASCII.LF);
      --  Appends S & Separator to Result.
      --  Result must not be set to Null when calling this subprogram.

      procedure Insert (S : String; Separator : Character := ASCII.LF) is
         R : constant String := Result.all & S & Separator;
      begin
         Free (Result);
         Result := new String'(R);
      end Insert;

   begin
      if Command = "help" then
         if Number_Of_Arguments (Data) = 0 then
            Insert (-"The following commands are defined:");

            L := Commands_As_List ("", GObject (Kernel));
            String_List_Utils.Sort (L);

            L2 := First (L);
            while L2 /= String_List_Utils.String_List.Null_Node loop
               Insert (" " & String_List_Utils.String_List.Data (L2));
               L2 := String_List_Utils.String_List.Next (L2);
            end loop;

            Free (L);

            Insert
              (-"Type ""help <cmd>"" to get help about a specific command.");

         else
            declare
               Cmd  : constant String := Nth_Arg (Data, 1);
               Info : Command_Information;
            begin
               Command_Node := Command_List.First
                 (Shell_Module_Id.Commands_List);

               while Command_Node /= Command_List.Null_Node loop
                  Info := Command_List.Data (Command_Node);
                  if Info.Command.all = Cmd then
                     Insert (-("Usage: ") & Info.Command.all
                             & ' ' & Info.Usage.all);
                     Insert (Info.Description.all);
                  end if;
                  Command_Node := Command_List.Next (Command_Node);
               end loop;
            end;
         end if;

         Set_Return_Value (Data, Result.all);
         Free (Result);

      elsif Command = "load" then
         declare
            Filename : constant String := Nth_Arg (Data, 1);
            Buffer : GNAT.OS_Lib.String_Access := Read_File (Filename);
         begin
            if Buffer /= null then
               Execute_Command
                 (Get_Script (Data), Buffer.all, Display_In_Console => True);
               Free (Buffer);
            else
               Set_Error_Msg (Data, -"File not found: """ & Filename & '"');
            end if;
         end;

      elsif Command = "echo" then
         for A in 1 .. Number_Of_Arguments (Data) loop
            Insert (Nth_Arg (Data, A), ' ');
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
     (Script         : access Shell_Scripting_Record;
      Command        : String;
      Params         : String := "";
      Return_Value   : String := "";
      Description    : String;
      Minimum_Args   : Natural := 0;
      Maximum_Args   : Natural := 0;
      Handler        : Module_Command_Function;
      Class          : Class_Type := No_Class;
      Static_Method  : Boolean := False)
   is
      function Ret_Val return String;
      --  Return a printable version of Return_Value

      function Ret_Val return String is
      begin
         if Return_Value = "" then
            return "";
         else
            return " return " & Return_Value;
         end if;
      end Ret_Val;

      pragma Unreferenced (Script, Static_Method);
      use Command_List;
      Node : Command_List.List_Node;
      Cmd, U  : GNAT.OS_Lib.String_Access;
      Min  : Natural := Minimum_Args;
      Max  : Natural := Maximum_Args;
      Info : Command_Information;
   begin
      if Command = "" or else Shell_Module_Id = null then
         return;
      end if;

      if Class /= No_Class then
         if Command = Constructor_Method then
            Cmd := new String'(Get_Name (Class));
         else
            Cmd := new String'(Get_Name (Class) & "." & Command);
            --  First parameter is always the instance

            Min := Min + 1;
            if Max /= Natural'Last then
               Max := Max + 1;
            end if;
         end if;
      else
         Cmd := new String'(Command);
      end if;

      Node := First (Shell_Module_Id.Commands_List);

      --  Check that the command is not already registered.

      while Node /= Command_List.Null_Node loop
         if Data (Node).Command.all = Cmd.all then
            Trace
              (Me,
               "Interactive command " & Cmd.all & " is already registered");
            return;
         end if;

         Node := Next (Node);
      end loop;

      if Class = No_Class then
         U := new String'(Params & Ret_Val);
      else
         if Params = "" then
            U := new String'
              ('(' & To_Lower (Get_Name (Class))  & ')' & Ret_Val);

         else
            U := new String'
              ('(' & To_Lower (Get_Name (Class))
               & ", " & Params (Params'First + 1 .. Params'Last) & Ret_Val);
         end if;
      end if;

      Info := (Command         => Cmd,
               Short_Command   => new String'(Command),
               Usage           => U,
               Description     => new String'(Description),
               Minimum_Args    => Min,
               Maximum_Args    => Max,
               Class           => Class,
               Command_Handler => Handler);

      Append (Shell_Module_Id.Commands_List, Info);
   end Register_Command;

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class
     (Script        : access Shell_Scripting_Record;
      Name          : String;
      Description   : String := "";
      Base          : Class_Type := No_Class)
   is
      pragma Unreferenced (Script, Name, Description, Base);
   begin
      --   Classes not supported in the shell module
      null;
   end Register_Class;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script             : access Shell_Scripting_Record;
      Command            : String;
      Display_In_Console : Boolean := True)
   is
      S : constant String := Execute_GPS_Shell_Command
        (Script.Kernel, Command);
   begin
      if Display_In_Console and then Script.Console /= null then
         Insert (Script.Console, S);
      end if;
   end Execute_Command;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File
     (Script             : access Shell_Scripting_Record;
      Filename           : String;
      Display_In_Console : Boolean := True)
   is
      Args : Argument_List := (1 => new String'(Filename));
      S    : constant String := Execute_Command (Script, "load", Args);
   begin
      if Display_In_Console and then Script.Console /= null then
         Insert (Script.Console, S);
      end if;

      Free (Args);
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
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String is
   begin
      return Execute_GPS_Shell_Command
        (Script.Kernel, Command, Args);
   end Execute_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      use type Command_List.List_Node;

      Command_Node : Command_List.List_Node;
      Data         : Command_Information;
      Callback     : Shell_Callback_Data;
      Instance     : Class_Instance;
      Start        : Natural;
      Shell        : Shell_Scripting;

   begin
      if Shell_Module_Id = null then
         return -"Shell module not initialized";
      end if;

      Trace (Me, "Launching interactive command: "
             & Command & ' '
             & Argument_List_To_String (Args));

      Command_Node := Command_List.First (Shell_Module_Id.Commands_List);
      while Command_Node /= Command_List.Null_Node loop
         Data := Command_List.Data (Command_Node);
         if Data.Command.all = Command then
            if Data.Minimum_Args <= Args'Length
              and then Args'Length <= Data.Maximum_Args
            then
               Shell := Shell_Scripting
                 (Lookup_Scripting_Language (Kernel, GPS_Shell_Name));
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
                 & Data.Command.all & ' ' & Data.Usage.all;
            end if;
         end if;
         Command_Node := Command_List.Next (Command_Node);
      end loop;

      return -"Command not recognized";

   exception
      when Invalid_Parameter =>
         return -"Invalid parameter for " & Command;

      when E : others =>
         Trace (Me, "Unexpected exception in Execute_Command: "
                & Exception_Information (E));
         return "";
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String) return String
   is
      Args         : Argument_List_Access;
      First, Last  : Integer;
   begin
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

            --  ??? This doesn't allow any embedded semicolon or newline in the
            --  commands. Probably not major at this point, better use python
            --  for real scripts.
            while Last <= Command'Last
              and then Command (Last) /= ';'
              and then Command (Last) /= ASCII.LF
            loop
               Last := Last + 1;
            end loop;

            Args := Argument_String_To_List (Command (First .. Last - 1));

            declare
               R : constant String := Execute_GPS_Shell_Command
                 (Kernel,
                  Command => Args (Args'First).all,
                  Args    => Args (Args'First + 1 .. Args'Last));
            begin
               Free (Args);

               if Last > Command'Last then
                  return R;
               end if;
            end;

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

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Instance : access Shell_Class_Instance_Record)
      return Scripting_Language is
   begin
      return Instance.Script;
   end Get_Script;

   -------------------------
   -- Number_Of_Arguments --
   -------------------------

   function Number_Of_Arguments (Data : Shell_Callback_Data) return Natural is
   begin
      return Data.Args'Length;
   end Number_Of_Arguments;

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
     (Data : Shell_Callback_Data; N : Positive) return System.Address
   is
      S : constant String := Nth_Arg (Data, N);
      Add : constant System.Address := GNAT.Debug_Utilities.Value
        ("16#" & S (S'First + 2 .. S'Last) & '#');
   begin
      return Add;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance
   is
      Ins : constant Shell_Class_Instance := Instance_From_Name
        (Data.Script, Nth_Arg (Data, N));
   begin
      if Ins = null and then Allow_Null then
         return null;
      end if;

      if Ins = null
        or else not Is_Subclass (Data.Script, Get_Class (Ins), Class)
      then
         Trace (Me, "Instance not found: " & Nth_Arg (Data, N));
         raise Invalid_Parameter;
      else
         return Class_Instance (Ins);
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

      Set_Return_Value (Data, Integer'Image (Value));
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Boolean) is
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
         Data.Return_Value := new String'(Tmp.all & ASCII.LF & Value);
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
     (Data : in out Shell_Callback_Data; Value : System.Address) is
   begin
      Set_Return_Value (Data, "0x" & System.Address_Image (Value));
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Shell_Callback_Data; Value : Class_Instance) is
   begin
      Set_Return_Value (Data, Name_From_Instance (Value));
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
      Instance := new Shell_Class_Instance_Record'
        (Class_Instance_Record
         with Script => Scripting_Language (Script),
              Class  => Class,
              Data   => (Data => Strings, Str => null));
      Instances_List.Prepend (Script.Instances, Instance);
      return Class_Instance (Instance);
   end New_Instance;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Instance : access Shell_Class_Instance_Record)
      return Class_Type is
   begin
      return Instance.Class;
   end Get_Class;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Shell_Class_Instance_Record)
      return Glib.Object.GObject is
   begin
      if Instance.Data.Data /= Object
        or else Instance.Data.Obj = null
      then
         raise Invalid_Data;
      else
         return Instance.Data.Obj;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Shell_Class_Instance_Record) return Integer is
   begin
      if Instance.Data.Data /= Integers then
         raise Invalid_Data;
      else
         return Instance.Data.Int;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : access Shell_Class_Instance_Record)
      return System.Address is
   begin
      if Instance.Data.Data = Addresses then
         return Instance.Data.Addr;
      end if;
      raise Invalid_Data;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Shell_Class_Instance_Record) return String is
   begin
      if Instance.Data.Data /= Strings
        or else Instance.Data.Str = null
      then
         raise Invalid_Data;
      else
         return Instance.Data.Str.all;
      end if;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record;
      Value    : access Glib.Object.GObject_Record'Class) is
   begin
      Primitive_Free (Instance.all);
      Instance.Data := (Data => Object, Obj => GObject (Value));
      Ref (Value);
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record;
      Value    : Integer) is
   begin
      Primitive_Free (Instance.all);
      Instance.Data := (Data => Integers, Int => Value);
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance   : access Shell_Class_Instance_Record;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null) is
   begin
      Primitive_Free (Instance.all);
      Instance.Data := (Data       => Addresses,
                        Addr       => Value,
                        On_Destroy => On_Destroy);
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record; Value : String) is
   begin
      Primitive_Free (Instance.all);
      Instance.Data := (Data => Strings, Str => new String'(Value));
   end Set_Data;

   --------------------
   -- Primitive_Free --
   --------------------

   procedure Primitive_Free (Instance : in out Shell_Class_Instance_Record) is
   begin
      case Instance.Data.Data is
         when Object =>
            if Instance.Data.Obj /= null then
               Unref (Instance.Data.Obj);
            end if;

         when Strings =>
            Free (Instance.Data.Str);

         when Integers =>
            null;

         when Addresses =>
            if Instance.Data.On_Destroy /= null
              and then Instance.Data.Addr /= System.Null_Address
            then
               Instance.Data.On_Destroy (Instance.Data.Addr);
            end if;
            Instance.Data.Addr := System.Null_Address;
      end case;
   end Primitive_Free;

end Shell_Script;
