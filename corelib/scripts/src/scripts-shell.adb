------------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007, AdaCore              --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
with GNAT.Debug_Utilities;      use GNAT.Debug_Utilities;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with Scripts;                   use Scripts;
with Scripts.Impl;              use Scripts.Impl;
with Scripts.Utils;             use Scripts.Utils;
with System.Address_Image;
with System;                    use System;

package body Scripts.Shell is

   Num_Previous_Returns : constant := 9;
   --  Number of parameters %1, %2,... which are used to memorize the result of
   --  previous commands.

   --------------------------
   -- Shell_Class_Instance --
   --------------------------

   type Shell_Class_Instance_Record is new Class_Instance_Record with record
      Class : Class_Type;
   end record;
   type Shell_Class_Instance is access all Shell_Class_Instance_Record'Class;

   overriding function Print_Refcount
     (Instance : access Shell_Class_Instance_Record) return String;
   overriding function Is_Subclass
     (Instance : access Shell_Class_Instance_Record;
      Base     : Class_Type) return Boolean;
   --  See doc from inherited subprogram

   package Instances_List is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Shell_Class_Instance);
   use Instances_List;
   --  ??? Would be faster to use a hash-table...

   -------------------------
   -- Command_Information --
   -------------------------

   type Command_Information is record
      Command         : GNAT.Strings.String_Access;
      Short_Command   : GNAT.Strings.String_Access;
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

   procedure Free (Com : in out Command_Information_Access);
   --  Free memory associated with Com

   package Command_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Command_Information_Access, Ada.Strings.Hash, "=");
   use Command_Hash;

   ---------------------
   -- Shell_scripting --
   ---------------------

   type Shell_Scripting_Record is new Scripting_Language_Record with record
      Repo      : Scripts_Repository;
      Blocked   : Boolean := False;
      Instances : Instances_List.List;
      --  All the instances that were created

      Commands_List : Command_Hash.Map;
      --  The list of all registered commands

      Returns   : Argument_List (1 .. Num_Previous_Returns);
      --  The result of the Num_Previous_Returns previous commands
   end record;
   type Shell_Scripting is access all Shell_Scripting_Record'Class;

   overriding procedure Destroy (Script : access Shell_Scripting_Record);
   overriding procedure Register_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False);
   overriding procedure Register_Class
     (Script        : access Shell_Scripting_Record;
      Name          : String;
      Base          : Class_Type := No_Class);
   overriding procedure Block_Commands
     (Script : access Shell_Scripting_Record; Block : Boolean);
   overriding procedure Execute_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Console       : Virtual_Console := null;
      Hide_Output   : Boolean := False;
      Show_Command  : Boolean := True;
      Errors        : out Boolean);
   overriding function Execute_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Console       : Virtual_Console := null;
      Hide_Output   : Boolean := False;
      Show_Command  : Boolean := True;
      Errors        : access Boolean) return String;
   overriding function Execute_Command
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Console       : Virtual_Console := null;
      Hide_Output   : Boolean := False;
      Errors        : access Boolean) return Boolean;
   overriding function Execute_Command
     (Script  : access Shell_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   overriding function Execute_Command_With_Args
     (Script        : access Shell_Scripting_Record;
      Command       : String;
      Args          : GNAT.OS_Lib.Argument_List) return String;
   overriding procedure Execute_File
     (Script        : access Shell_Scripting_Record;
      Filename      : String;
      Console       : Virtual_Console := null;
      Hide_Output   : Boolean := False;
      Show_Command  : Boolean := True;
      Errors        : out Boolean);
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
   overriding procedure Complete
     (Script      : access Shell_Scripting_Record;
      Input       : String;
      Completions : out String_Lists.List);
   --  See doc from inherited subprograms

   overriding function New_Instance
     (Script : access Shell_Scripting_Record; Class : Class_Type)
      return Class_Instance;

   procedure Free_Internal_Data (Script : access Shell_Scripting_Record'Class);
   --  Free the internal memory used to store the results of previous commands
   --  and class instances

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
      Args       : Callback_Data'Class) return Boolean;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return String;
   overriding function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return GNAT.Strings.String_List;
   overriding procedure Free (Subprogram : in out Shell_Subprogram_Record);
   overriding function Get_Name
     (Subprogram : access Shell_Subprogram_Record) return String;
   overriding function Get_Script
     (Subprogram : Shell_Subprogram_Record) return Scripting_Language;
   --  See doc from inherited subprograms

   -------------------------
   -- Shell_Callback_Data --
   -------------------------

   type Shell_Callback_Data is new Callback_Data with record
      Script          : Shell_Scripting;
      Args            : GNAT.OS_Lib.Argument_List_Access;
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
     (Data : Shell_Callback_Data; N : Positive) return Integer;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Boolean;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Subprogram_Type;
   overriding function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance;
   overriding procedure Set_Error_Msg
     (Data : in out Shell_Callback_Data; Msg : String);
   overriding procedure Set_Return_Value_As_List
     (Data : in out Shell_Callback_Data; Size : Natural := 0);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Integer);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Boolean);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : String);
   overriding procedure Set_Return_Value
     (Data   : in out Shell_Callback_Data; Value : Class_Instance);
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
     (Data : Shell_Callback_Data; N : Positive; Value : String);
   overriding procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Integer);
   overriding procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Boolean);
   overriding procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Class_Instance);
   overriding procedure Set_Nth_Arg
     (Data : Shell_Callback_Data; N : Positive; Value : Subprogram_Type);
   --  See doc from inherited subprogram

   ----------
   -- Misc --
   ----------

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
     (Script  : access Shell_Scripting_Record'Class;
      Command : String;
      Errors  : access Boolean) return String;
   --  Execute a command in the GPS shell and returns its result.
   --  Command might be a series of commands, separated by semicolons or
   --  newlines. The return value is the result of the last command.
   --  If Errors is set to True on exit, then the return value is an error msg

   function Execute_GPS_Shell_Command
     (Script  : access Shell_Scripting_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List;
      Errors  : access Boolean) return String;
   --  Execute a command in the GPS shell and returns its result.
   --  Command must be a single command (no semicolon-separated list).

   procedure Module_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handles functions specific to the shell language

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
      L : Instances_List.Cursor := First (Script.Instances);
   begin
      while Has_Element (L) loop
         if Element (L).all'Address = Add then
            return Element (L);
         end if;

         Next (L);
      end loop;
      return null;
   end Instance_From_Address;

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

   ----------------------------
   -- Module_Command_Handler --
   ----------------------------

   procedure Module_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
   begin
      if Command = "load" then
         declare
            Filename : constant String := Nth_Arg (Data, 1);
            Buffer   : GNAT.Strings.String_Access := Read_File (Filename);
            Errors   : Boolean;
         begin
            if Buffer /= null then
               Execute_Command
                 (Get_Script (Data), Buffer.all,
                  Errors => Errors);
               Free (Buffer);
            else
               Set_Error_Msg (Data, "File not found: """ & Filename & '"');
            end if;
         end;

      elsif Command = "echo" then
         for A in 1 .. Number_Of_Arguments (Data) loop
            if A = Number_Of_Arguments (Data) then
               Insert_Text (Get_Script (Data), null, Nth_Arg (Data, A));
            else
               Insert_Text (Get_Script (Data), null, Nth_Arg (Data, A) & ' ');
            end if;
         end loop;
         Insert_Text (Get_Script (Data), null, "" & ASCII.LF);

      elsif Command = "clear_cache" then
         Free_Internal_Data (Shell_Scripting (Get_Script (Data)));
      end if;
   end Module_Command_Handler;

   ------------------------------
   -- Register_Shell_Scripting --
   ------------------------------

   procedure Register_Shell_Scripting
     (Repo : Scripts_Repository)
   is
      Script : Shell_Scripting;
   begin
      Script := new Shell_Scripting_Record;
      Script.Repo := Repo;
      Register_Scripting_Language (Repo, Script);

      Register_Command
        (Script, "load",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);
      Register_Command
        (Script, "echo",
         Minimum_Args => 0,
         Maximum_Args => Natural'Last,
         Handler      => Module_Command_Handler'Access);
      Register_Command
        (Script, "clear_cache",
         Handler => Module_Command_Handler'Access);
   end Register_Shell_Scripting;

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
      Cmd  : GNAT.Strings.String_Access;
      Min  : Natural := Minimum_Args;
      Max  : Natural := Maximum_Args;
      Info_C : Command_Hash.Cursor;
      Info : Command_Information_Access;
   begin
      if Command = "" then
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

      Info_C := Find (Script.Commands_List, Cmd.all);

      --  Check that the command is not already registered.

      if Has_Element (Info_C) then
         raise Program_Error with
         "Command already registered " & Cmd.all;

      else
         Info := new Command_Information'
           (Command         => Cmd,
            Short_Command   => new String'(Command),
            Minimum_Args    => Min,
            Maximum_Args    => Max,
            Class           => Class,
            Command_Handler => Handler);

         Include (Script.Commands_List, Cmd.all, Info);
      end if;
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

   --------------------
   -- Display_Prompt --
   --------------------

   overriding procedure Display_Prompt
     (Script  : access Shell_Scripting_Record;
      Console : Virtual_Console := null) is
   begin
      Insert_Prompt (Script, Console, "[Shell]>");
   end Display_Prompt;

   --------------
   -- Complete --
   --------------

   procedure Complete
     (Script      : access Shell_Scripting_Record;
      Input       : String;
      Completions : out String_Lists.List)
   is
      Current : Command_Hash.Cursor;
      Info    : Command_Information_Access;
   begin
      Completions := String_Lists.Empty_List;

      Current := First (Script.Commands_List);
      while Has_Element (Current) loop
         Info := Element (Current);
         declare
            S : constant String := Info.Command.all;
         begin
            if S'Length >= Input'Length
              and then S (S'First .. S'First + Input'Length - 1) = Input
            then
               String_Lists.Append (Completions, S);
            end if;
         end;

         Next (Current);
      end loop;

      String_Lists_Sort.Sort (Completions);
   end Complete;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script       : access Shell_Scripting_Record;
      Command      : String;
      Console     : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean)
   is
      pragma Unreferenced (Show_Command);
      Err : aliased Boolean;
      S   : constant String := Execute_GPS_Shell_Command
        (Script, Command, Err'Unchecked_Access);
   begin
      Errors := Err;
      if S /= "" then
         Insert_Text (Script, Console, S & ASCII.LF, Hide_Output);
      end if;
      if not Hide_Output then
         Display_Prompt (Script, Console);
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
        (Script, Command, Args, Errors'Unchecked_Access);
   end Execute_Command_With_Args;

   ------------------
   -- Execute_File --
   ------------------

   procedure Execute_File
     (Script       : access Shell_Scripting_Record;
      Filename     : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean)
   is
      Err  : aliased Boolean;
      Args : Argument_List := (1 => new String'(Filename));
   begin
      Insert_Text (Script, Console, "load " & Filename, not Show_Command);
      declare
         S : constant String := Execute_GPS_Shell_Command
           (Script, "load", Args, Err'Unchecked_Access);
      begin
         Errors := Err;
         if S /= "" then
            Insert_Text (Script, Console, S & ASCII.LF, Hide_Output);
         end if;
         if not Hide_Output then
            Display_Prompt (Script, Console);
         end if;

         for F in Args'Range loop
            Free (Args (F));
         end loop;
      end;
   end Execute_File;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Script : access Shell_Scripting_Record) return String is
      pragma Unreferenced (Script);
   begin
      return Shell_Name;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Com : in out Command_Information_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Command_Information, Command_Information_Access);
   begin
      Free (Com.Command);
      Free (Com.Short_Command);
      Unchecked_Free (Com);
   end Free;

   ------------------------
   -- Free_Internal_Data --
   ------------------------

   procedure Free_Internal_Data
     (Script : access Shell_Scripting_Record'Class)
   is
      C2   : Instances_List.Cursor;
      Inst : Shell_Class_Instance;
   begin
      for R in Script.Returns'Range loop
         Free (Script.Returns (R));
      end loop;

      C2 := First (Script.Instances);
      while Has_Element (C2) loop
         Inst := Element (C2);
         Decref (Inst);
         Next (C2);
      end loop;
   end Free_Internal_Data;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Script : access Shell_Scripting_Record) is
      C    : Command_Hash.Cursor;
      Com  : Command_Information_Access;
   begin
      Free_Internal_Data (Script);

      C := First (Script.Commands_List);
      while Has_Element (C) loop
         Com := Element (C);
         Free (Com);
         Next (C);
      end loop;
   end Destroy;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script       : access Shell_Scripting_Record;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String
   is
      pragma Unreferenced (Show_Command);
      Err    : aliased Boolean;
      Result : constant String := Execute_GPS_Shell_Command
        (Script, Command, Err'Unchecked_Access);

   begin
      Errors.all := Err;
      if Result /= "" then
         Insert_Text (Script, Console, Result & ASCII.LF, Hide_Output);
      end if;
      if not Hide_Output then
         Display_Prompt (Script, Console);
      end if;
      return Result;
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script      : access Shell_Scripting_Record;
      Command     : String;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean
   is
      Err    : aliased Boolean;
      Result : constant String := Trim
        (Execute_GPS_Shell_Command (Script, Command, Err'Unchecked_Access),
         Ada.Strings.Both);

   begin
      Errors.all := Err;
      Insert_Text (Script, Console, Result & ASCII.LF, Hide_Output);
      if not Hide_Output then
         Display_Prompt (Script, Console);
      end if;
      return Result = "1" or else To_Lower (Result) = "true";
   end Execute_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Script    : access Shell_Scripting_Record'Class;
      Command   : String;
      Args      : GNAT.OS_Lib.Argument_List;
      Errors    : access Boolean) return String
   is
      Data_C   : Command_Hash.Cursor;
      Data     : Command_Information_Access;
      Callback : Shell_Callback_Data;
      Instance : Class_Instance;
      Start    : Natural;

   begin
      if Script.Blocked then
         Errors.all := True;
         return "A command is already executing";
      end if;

      Insert_Log (Script, null, "Executing " & Command);

      Errors.all := False;

      Data_C := Find (Script.Commands_List, Command);
      if Has_Element (Data_C) then
         Data := Element (Data_C);

         if Data.Minimum_Args <= Args'Length
           and then Args'Length <= Data.Maximum_Args
         then
            Callback.Script := Shell_Scripting (Script);

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
                       new String'(Script.Returns
                                     (Num + Script.Returns'First - 1).all);

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
            Free (Script.Returns (Script.Returns'Last));
            Script.Returns
              (Script.Returns'First + 1 .. Script.Returns'Last) :=
              Script.Returns
                (Script.Returns'First .. Script.Returns'Last - 1);

            if Callback.Return_Value = null then
               Script.Returns (Script.Returns'First) := new String'("");
            else
               Script.Returns (Script.Returns'First) :=
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
            Errors.all := True;
            return "Incorrect number of arguments for " & Command;
         end if;
      end if;

      Errors.all := True;
      return "Command not recognized: " & Command;

   exception
      when Invalid_Parameter =>
         Errors.all := True;
         return "Invalid parameter for " & Command;

      when E : others =>
         Errors.all := True;
         return Exception_Information (E);
   end Execute_GPS_Shell_Command;

   -------------------------------
   -- Execute_GPS_Shell_Command --
   -------------------------------

   function Execute_GPS_Shell_Command
     (Script  : access Shell_Scripting_Record'Class;
      Command : String;
      Errors  : access Boolean) return String
   is
      Args          : Argument_List_Access;
      First, Last   : Integer;
      Tmp           : GNAT.Strings.String_Access;
      Quoted        : Boolean;
      Triple_Quoted : Boolean;
   begin
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

            if Last - 1 >= First then
               Args := Argument_String_To_List_With_Triple_Quotes
                 (Command (First .. Last - 1));

               if Args = null or else Args'Length = 0 then
                  Errors.all := True;
                  return "Couldn't parse argument string for "
                    & Command (First .. Last - 1);
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

                  declare
                     R : constant String := Execute_GPS_Shell_Command
                       (Script,
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
            end if;

            First := Last + 1;
         end loop;
      end if;

      return "";
   end Execute_GPS_Shell_Command;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Data : Shell_Callback_Data)
      return Scripting_Language is
   begin
      return Scripting_Language (Data.Script);
   end Get_Script;

   --------------------
   -- Get_Repository --
   --------------------

   function Get_Repository (Script : access Shell_Scripting_Record)
      return Scripts_Repository is
   begin
      return Script.Repo;
   end Get_Repository;

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
      Free (Data.Args);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (Data : Shell_Callback_Data) return Callback_Data'Class is
      C : constant Argument_List_Access := new Argument_List (Data.Args'Range);
   begin
      for A in Data.Args'Range loop
         C (A) := new String'(Data.Args (A).all);
      end loop;

      return Shell_Callback_Data'
        (Callback_Data with
         Args            => C,
         Script          => Data.Script,
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
        new String'(Shell_Subprogram_Record (Value.all).Command.all);
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

      if Ins = null
        or else (Class /= Any_Class and then not Is_Subclass (Ins, Class))
      then
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
      Tmp : GNAT.Strings.String_Access;
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
      Tmp : GNAT.Strings.String_Access;
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
        (Execute_GPS_Shell_Command
           (Script, Command & ' ' & Argument_List_To_Quoted_String
              (Shell_Callback_Data (Args).Args.all),
            Errors'Unchecked_Access),
         Ada.Strings.Both);
   begin
      return Result = "1" or else To_Lower (Result) = "true";
   end Execute_Command;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return Boolean
   is
   begin
      return To_Lower (Execute (Subprogram, Args)) = "true";
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return String
   is
      D : constant Shell_Callback_Data := Shell_Callback_Data (Args);
      C : Argument_List (D.Args'Range);
      Errors : aliased Boolean;
   begin
      for A in D.Args'Range loop
         C (A) := new String'(D.Args (A).all);
      end loop;

      return Execute_GPS_Shell_Command
        (Script  => Shell_Scripting (Subprogram.Script),
         Command => Subprogram.Command.all,
         Args    => C,
         Errors  => Errors'Unchecked_Access);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Shell_Subprogram_Record;
      Args       : Callback_Data'Class) return GNAT.Strings.String_List
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
      return "command: " & Subprogram.Command.all;
   end Get_Name;

   ----------
   -- Free --
   ----------

   procedure Free (Subprogram : in out Shell_Subprogram_Record) is
   begin
      Free (Subprogram.Command);
   end Free;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Shell_Callback_Data; N : Positive) return Subprogram_Type
   is
      Name : constant String := Nth_Arg (Data, N);
   begin
      return new Shell_Subprogram_Record'
        (Subprogram_Record with
         Script  => Get_Script (Data),
         Command => new String'(Name));
   end Nth_Arg;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script
     (Subprogram : Shell_Subprogram_Record) return Scripting_Language
   is
   begin
      return Scripting_Language (Subprogram.Script);
   end Get_Script;

end Scripts.Shell;
