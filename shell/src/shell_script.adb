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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Generic_List;
with Glib.Object;              use Glib.Object;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Glide_Kernel;             use Glide_Kernel;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;
with Histories;                use Histories;
with Interactive_Consoles;     use Interactive_Consoles;
with String_List_Utils;        use String_List_Utils;
with System.Address_Image;
with System;                   use System;
with Traces;                   use Traces;

package body Shell_Script is

   Me : constant Debug_Handle := Create ("Shell_Script");

   Shell_Name : constant String := "GPS Shell";

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   ---------------------
   -- Shell_scripting --
   ---------------------

   type Shell_Scripting_Record is new Scripting_Language_Record with record
      Kernel  : Glide_Kernel.Kernel_Handle;
      Console : Interactive_Consoles.Interactive_Console;
   end record;
   type Shell_Scripting is access all Shell_Scripting_Record'Class;

   procedure Register_Command
     (Script       : access Shell_Scripting_Record;
      Command      : String;
      Usage        : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function;
      Class        : Class_Type := No_Class);
   procedure Register_Class
     (Script        : access Shell_Scripting_Record;
      Name          : String;
      Description   : String := "";
      As_Dictionary : Boolean := False);
   procedure Execute_Command
     (Script             : access Shell_Scripting_Record;
      Command            : String;
      Display_In_Console : Boolean := True);
   function Get_Name (Script : access Shell_Scripting_Record) return String;
   --  See doc from inherited subprograms

   -------------------------
   -- Shell_Callback_Data --
   -------------------------

   type Shell_Callback_Data is new Callback_Data with record
      Kernel       : Glide_Kernel.Kernel_Handle;
      Args         : GNAT.OS_Lib.Argument_List_Access;
      Return_Value : GNAT.OS_Lib.String_Access;
   end record;

   function Get_Kernel (Data : Shell_Callback_Data)
      return Glide_Kernel.Kernel_Handle;
   function Number_Of_Arguments (Data : Shell_Callback_Data) return Natural;
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return String;
   function Nth_Arg (Data : Shell_Callback_Data; N : Positive) return Integer;
   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return System.Address;
   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Class : Class_Type)
      return Class_Instance;
   procedure Set_Error_Msg (Data : in out Shell_Callback_Data; Msg : String);
   procedure Set_Return_Value_As_List
     (Data : in out Shell_Callback_Data; Size : Natural := 0);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : Integer; Append : Boolean := False);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : String; Append : Boolean := False);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : System.Address; Append : Boolean := False);
   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : Class_Instance; Append : Boolean := False);
   --  See doc from inherited subprogram

   -------------------
   -- Instance_Data --
   -------------------

   type Instance_Data_Type is (Strings, Object, Addresses);
   type Instance_Data (Data : Instance_Data_Type := Object) is record
      case Data is
         when Strings =>
            Str : GNAT.OS_Lib.String_Access;
         when Object =>
            Obj : Glib.Object.GObject;
         when Addresses =>
            Addr       : System.Address;
            On_Destroy : Destroy_Handler;
      end case;
   end record;

   --------------------------
   -- Shell_Class_Instance --
   --------------------------

   type Shell_Class_Instance_Record is new Class_Instance_Record with record
      Class : Class_Type;
      Data  : Instance_Data;
   end record;
   type Shell_Class_Instance is access all Shell_Class_Instance_Record'Class;

   function New_Instance
     (Data : Shell_Callback_Data; Class : Class_Type) return Class_Instance;
   function Get_Class (Instance : access Shell_Class_Instance_Record)
      return Class_Type;
   function Get_Data (Instance : access Shell_Class_Instance_Record)
      return Glib.Object.GObject;
   function Get_Data
     (Instance : access Shell_Class_Instance_Record) return String;
   function Get_Data (Instance : access Shell_Class_Instance_Record)
      return System.Address;
   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record;
      Value    : access Glib.Object.GObject_Record'Class);
   procedure Set_Data
     (Instance : access Shell_Class_Instance_Record; Value : String);
   procedure Set_Data
     (Instance   : access Shell_Class_Instance_Record;
      Value      : System.Address;
      On_Destroy : Destroy_Handler := null);
   procedure Primitive_Free (Instance : in out Shell_Class_Instance_Record);
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
   end record;
   --  Description for each of the registered commands.
   --  Command is the name that must be typed by the user in the console.
   --  Short_Command is the name under which the command was registered. It is
   --  the same as Command, except when the command is a method of a class. In
   --  this case, Command is equal to "Class.Short_Command"

   procedure Free (X : in out Command_Information);
   --  Free memory associated with X.

   package Command_List is new Generic_List (Command_Information);
   --  ??? Would be faster to use a hash-table...

   procedure Free_Instance (Instance : in out Shell_Class_Instance);
   package Instances_List is new Generic_List
     (Shell_Class_Instance, Free_Instance);
   use Instances_List;
   --  ??? Would be faster to use a hash-table...

   type Shell_Module_Id_Record is new Module_ID_Record with record
      Commands_List : Command_List.List;
      --  The list of all registered commands

      Instances : Instances_List.List;
      --  All the instances that were created
   end record;
   type Shell_Module_Id_Access is access all Shell_Module_Id_Record;

   procedure Destroy (Module : in out Shell_Module_Id_Record);
   --  Free the memory associated with the module

   Shell_Module_Id : Shell_Module_Id_Access;

   procedure Module_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Internal handler for the shell functions defined in this module

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Callback for the desctruction of the shell window

   function Commands_As_List
     (Prefix : String;
      Kernel : access Glib.Object.GObject_Record'Class)
      return String_List_Utils.String_List.List;
   --  Return the list of commands. The list must be freed by the caller.

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String;
   --  Launch the command interpreter for Input and return the output.

   function Name_From_Instance
     (Instance : access Class_Instance_Record'Class) return String;
   --  Return the string to display to report the instance in the shell

   function Instance_From_Name (Name : String) return Shell_Class_Instance;
   --  Opposite of Name_From_Instance

   function Instance_From_Address
     (Add : System.Address) return Shell_Class_Instance;
   --  Return an instance from its address

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
     (Name : String) return Shell_Class_Instance
   is
      Index : Natural := Name'First;
   begin
      while Index <= Name'Last - 3
        and then Name (Index .. Index + 2) /= "_0x"
      loop
         Index := Index + 1;
      end loop;

      Trace (Me, "Instance_From_Name: addr="
             & Name (Index + 3 .. Name'Last - 1));

      return Instance_From_Address
        (Value ("16#" & Name (Index + 3 .. Name'Last - 1) & "#"));

   exception
      when others =>
         --  Invalid instance
         return null;
   end Instance_From_Name;

   ---------------------------
   -- Instance_From_Address --
   ---------------------------

   function Instance_From_Address
     (Add : System.Address) return Shell_Class_Instance
   is
      L   : List_Node := First (Shell_Module_Id.Instances);
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
      Kernel : access Glib.Object.GObject_Record'Class)
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
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Command_Information) is
   begin
      Free (X.Command);
      Free (X.Usage);
      Free (X.Description);
   end Free;

   --------------------------
   -- Console_Delete_Event --
   --------------------------

   function Console_Delete_Event
     (Console : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Console);
   begin
      return True;
   end Console_Delete_Event;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Input  : String;
      Kernel : access GObject_Record'Class) return String
   is
      S : constant String := Execute_GPS_Shell_Command
        (Kernel_Handle (Kernel), Input);
   begin
      if S = ""
        or else S (S'Last) = ASCII.LF
        or else S (S'Last) = ASCII.CR
      then
         return S;
      else
         return S & ASCII.LF;
      end if;
   end Interpret_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Child : MDI_Child;
      Script : Shell_Scripting;
   begin
      Shell_Module_Id := new Shell_Module_Id_Record;
      Register_Module
        (Module                  => Module_ID (Shell_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => "Shell script",
         Priority                => Glide_Kernel.Default_Priority);

      Script := new Shell_Scripting_Record;
      Script.Kernel := Kernel_Handle (Kernel);
      Register_Scripting_Language (Kernel, Script);

      Gtk_New (Script.Console,
               "GPS> ",
               Interpret_Command_Handler'Access,
               GObject (Kernel),
               Get_Pref (Kernel, Source_Editor_Font),
               History_List => Get_History (Kernel),
               Key          => "shell",
               Wrap_Mode    => Wrap_Char);
      Set_Completion_Handler (Script.Console, Commands_As_List'Access);
      Child := Put
        (Get_MDI (Kernel), Script.Console,
         Iconify_Button or Maximize_Button,
         Focus_Widget => Gtk_Widget (Get_View (Script.Console)),
         Default_Width => 400,
         Default_Height => 100);
      Set_Title (Child, -"Shell Script");
      Set_Dock_Side (Child, Bottom);
      Dock_Child (Child);

      --  Only remember the last 100 commands.
      Set_Max_Length (Get_History (Kernel).all, 100, "shell");
      Allow_Duplicates (Get_History (Kernel).all, "shell", True, True);

      Return_Callback.Connect
        (Script.Console, "delete_event",
         Return_Callback.To_Marshaller (Console_Delete_Event'Access));

      --  The following commands are specific to the GPS shell script.
      Register_Command
        (Script,
         Command      => "help",
         Usage        => "help () -> None",
         Description  => -"List recognized commands.",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Script,
         Command      => "echo",
         Usage        => "echo () -> None",
         Description  => -"Display a line of text.",
         Minimum_Args => 0,
         Maximum_Args => Natural'Last,
         Handler      => Module_Command_Handler'Access);

      Register_Command
        (Script,
         Command      => "clear_cache",
         Usage        => "clear_cache () -> None",
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

      procedure Insert (S : String);
      --  Appends S & ASCII.LF to Result.
      --  Result must not be set to Null when calling this subprogram.

      procedure Insert (S : String) is
         R : constant String := Result.all & S & ASCII.LF;
      begin
         Free (Result);
         Result := new String'(R);
      end Insert;

   begin
      if Command = "help" then
         if Number_Of_Arguments (Data) = 0 then
            Insert (-"The following commands are defined:");

            L := Commands_As_List ("", Kernel);
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
               Cmd : constant String := Nth_Arg (Data, 1);
               Info : Command_Information;
            begin
               Command_Node := Command_List.First
                 (Shell_Module_Id.Commands_List);

               while Command_Node /= Command_List.Null_Node loop
                  Info := Command_List.Data (Command_Node);
                  if Info.Command.all = Cmd then
                     Insert (-("Usage: ") & Info.Usage.all);
                     Insert (Info.Description.all);
                  end if;
                  Command_Node := Command_List.Next (Command_Node);
               end loop;
            end;
         end if;

      elsif Command = "echo" then
         for A in 2 .. Number_Of_Arguments (Data) loop
            Insert (Nth_Arg (Data, A));
         end loop;

      elsif Command = "clear_cache" then
         Free (Shell_Module_Id.Instances, Free_Data => True);
      end if;

      Set_Return_Value (Data, Result.all);
      Free (Result);
   end Module_Command_Handler;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Script       : access Shell_Scripting_Record;
      Command      : String;
      Usage        : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function;
      Class        : Class_Type := No_Class)
   is
      pragma Unreferenced (Script);
      use Command_List;
      Node : Command_List.List_Node;
      Cmd  : String_Access;
      Extra_Arg : Natural;
   begin
      if Command = "" or else Shell_Module_Id = null then
         return;
      end if;

      Node := First (Shell_Module_Id.Commands_List);

      --  Check that the command is not already registered.

      while Node /= Command_List.Null_Node loop
         if Data (Node).Command.all = Command then
            Trace
              (Me,
               "Interactive command " & Command & " is already registered");

            return;
         end if;

         Node := Next (Node);
      end loop;

      if Class /= No_Class then
         Cmd := new String'(Get_Name (Class) & "." & Command);
         Extra_Arg := 1;  --  First parameter is always the instance
      else
         Cmd := new String'(Command);
         Extra_Arg := 0;
      end if;

      Append (Shell_Module_Id.Commands_List,
              (Command         => Cmd,
               Short_Command   => new String'(Command),
               Usage           => new String'(Usage),
               Description     => new String'(Description),
               Minimum_Args    => Minimum_Args + Extra_Arg,
               Maximum_Args    => Maximum_Args + Extra_Arg,
               Command_Handler => Handler));
   end Register_Command;

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class
     (Script        : access Shell_Scripting_Record;
      Name          : String;
      Description   : String := "";
      As_Dictionary : Boolean := False)
   is
      pragma Unreferenced (Script, Name, As_Dictionary, Description);
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
      if Display_In_Console then
         Insert (Script.Console, S);
      end if;
   end Execute_Command;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Script : access Shell_Scripting_Record) return String is
      pragma Unreferenced (Script);
   begin
      return Shell_Name;
   end Get_Name;

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

   begin
      if Shell_Module_Id = null then
         return -"Shell module not initialized";
      end if;

      Command_Node := Command_List.First (Shell_Module_Id.Commands_List);
      while Command_Node /= Command_List.Null_Node loop
         Data := Command_List.Data (Command_Node);
         if Data.Command.all = Command then
            if Data.Minimum_Args <= Args'Length
              and then Args'Length <= Data.Maximum_Args
            then
               Callback.Kernel := Kernel_Handle (Kernel);
               Callback.Args := new Argument_List (1 .. Args'Length);
               for A in Args'Range loop
                  Callback.Args (A - Args'First + 1) := Args (A);
               end loop;

               Data.Command_Handler (Callback, Data.Short_Command.all);

               Unchecked_Free (Callback.Args);
               if Callback.Return_Value = null then
                  return "";
               else
                  declare
                     S : constant String := Callback.Return_Value.all;
                  begin
                     Free (Callback.Return_Value);
                     return S;
                  end;
               end if;
            else
               Trace (Me, "Incorrect number of arguments for " & Command);
               return -"Incorrect number of arguments." & ASCII.LF
                 & Data.Usage.all;
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
   begin
      Trace (Me, "Launching interactive command: " & Command);

      Args := Argument_String_To_List (Command);

      declare
         R : constant String := Execute_GPS_Shell_Command
           (Kernel,
            Command => Args (Args'First).all,
            Args    => Args (Args'First + 1 .. Args'Last));
      begin
         Free (Args);
         return R;
      end;
   end Execute_GPS_Shell_Command;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (Data : Shell_Callback_Data) return Kernel_Handle is
   begin
      return Data.Kernel;
   end Get_Kernel;

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
         Trace (Me, "Missing parameters: " & N'Img
                & " > " & Data.Args'Last'Img);
         raise Invalid_Parameter;
      else
         return Data.Args (N).all;
      end if;
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
     (Data : Shell_Callback_Data; N : Positive; Class : Class_Type)
      return Class_Instance
   is
      Ins : constant Shell_Class_Instance := Instance_From_Name
        (Nth_Arg (Data, N));
   begin
      if Ins = null

         --  ??? Should check the children of Class as well
        or else Get_Class (Ins) /= Class
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
      Data.Return_Value := new String'(Msg);
   end Set_Error_Msg;

   ------------------------------
   -- Set_Return_Value_As_List --
   ------------------------------

   procedure Set_Return_Value_As_List
     (Data : in out Shell_Callback_Data; Size : Natural := 0)
   is
      pragma Unreferenced (Data, Size);
   begin
      null;
   end Set_Return_Value_As_List;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : Integer; Append : Boolean := False)
   is
      pragma Unreferenced (Append);
   begin
      Free (Data.Return_Value);
      Data.Return_Value := new String'(Integer'Image (Value));
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : String; Append : Boolean := False)
   is
      Tmp : String_Access;
   begin
      if Append then
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
     (Data   : in out Shell_Callback_Data;
      Value  : System.Address; Append : Boolean := False) is
   begin
      Set_Return_Value (Data, "0x" & System.Address_Image (Value), Append);
   end Set_Return_Value;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data;
      Value  : Class_Instance; Append : Boolean := False) is
   begin
      Set_Return_Value (Data, Name_From_Instance (Value), Append);
   end Set_Return_Value;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance
     (Data : Shell_Callback_Data; Class : Class_Type) return Class_Instance
   is
      pragma Unreferenced (Data);
      Instance : Shell_Class_Instance;
   begin
      Instance := new Shell_Class_Instance_Record'
        (Class_Instance_Record
         with Class => Class,
              Data  => (Data => Strings, Str => null));
      Instances_List.Prepend (Shell_Module_Id.Instances, Instance);
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
