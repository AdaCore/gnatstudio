------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Debugger;                use Debugger;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Intl;                use GPS.Intl;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Types;
with GVD_Module;              use GVD_Module;

package body GVD.Scripts is

   Debugger_Hook_Data_Type        : constant Hook_Type := "Debugger";
   Debugger_String_Hook_Data_Type : constant Hook_Type := "Debugger_String";
   Debugger_Hook_States_Data_Type : constant Hook_Type := "Debugger_States";

   Show_In_Console_Cst : aliased constant String := "show_in_console";

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module

   function Get_Or_Create_Instance
     (Script  : access Scripting_Language_Record'Class;
      Process : Visual_Debugger) return Class_Instance;
   --  Get or create an existing instance associated with Process

   function From_Callback_Data_Debugger
     (Data : Callback_Data'Class)
      return Hooks_Data'Class;
   function From_Callback_Data_Debugger_String
     (Data : Callback_Data'Class)
      return Hooks_Data'Class;
   function From_Callback_Data_Debugger_States
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  Convert Data into one of the supported Hooks_Data type for the debugger

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_Hooks_Data)
      return Callback_Data_Access
   is
      D   : constant Callback_Data_Access :=
              new Callback_Data'Class'(Create (Script, 2));
      Inst : constant Class_Instance :=
               Get_Or_Create_Instance (Script, Data.Debugger);
   begin
      Set_Nth_Arg (D.all, 1, To_String (Hook));
      Set_Nth_Arg (D.all, 2, Inst);
      return D;
   end Create_Callback_Data;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_Hooks_States_Data)
      return GNATCOLL.Scripts.Callback_Data_Access
   is
      D   : constant Callback_Data_Access :=
              new Callback_Data'Class'(Create (Script, 3));
      Inst : constant Class_Instance :=
              Get_Or_Create_Instance (Script, Data.Debugger);
   begin
      Set_Nth_Arg (D.all, 1, To_String (Hook));
      Set_Nth_Arg (D.all, 2, Inst);

      case Data.New_State is
         when Debug_None      => Set_Nth_Arg (D.all, 3, String'("none"));
         when Debug_Busy      => Set_Nth_Arg (D.all, 3, String'("busy"));
         when Debug_Available => Set_Nth_Arg (D.all, 3, String'("idle"));
      end case;

      return D;
   end Create_Callback_Data;

   ---------------------------------
   -- From_Callback_Data_Debugger --
   ---------------------------------

   function From_Callback_Data_Debugger
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Process : Visual_Debugger;
      Inst    : Class_Instance;
   begin
      Inst := Nth_Arg (Data, 2, New_Class (Get_Kernel (Data), "Debugger"));
      Process := Visual_Debugger (GObject'(Get_Data (Inst)));
      return Debugger_Hooks_Data'(Hooks_Data with Debugger => Process);
   end From_Callback_Data_Debugger;

   ----------------------------------------
   -- From_Callback_Data_Debugger_States --
   ----------------------------------------

   function From_Callback_Data_Debugger_States
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Process   : Visual_Debugger;
      Inst      : Class_Instance;
      New_S     : constant String := Nth_Arg (Data, 3);
      New_State : Debugger_State;
   begin
      Inst := Nth_Arg (Data, 2, New_Class (Get_Kernel (Data), "Debugger"));
      Process := Visual_Debugger (GObject'(Get_Data (Inst)));

      if New_S = "none" then
         New_State := Debug_None;
      elsif New_S = "busy" then
         New_State := Debug_Busy;
      else
         New_State := Debug_Available;
      end if;

      return Debugger_Hooks_States_Data'
        (Hooks_Data with
         Debugger  => Process,
         New_State => New_State);
   end From_Callback_Data_Debugger_States;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_String_Hooks_Data)
      return Callback_Data_Access
   is
      D   : constant Callback_Data_Access :=
              new Callback_Data'Class'(Create (Script, 3));
      Inst : constant Class_Instance :=
               Get_Or_Create_Instance (Script, Data.Debugger);
   begin
      Set_Nth_Arg (D.all, 1, To_String (Hook));
      Set_Nth_Arg (D.all, 2, Inst);
      Set_Nth_Arg (D.all, 3, Data.Command);
      return D;
   end Create_Callback_Data;

   ----------------------------------------
   -- From_Callback_Data_Debugger_String --
   ----------------------------------------

   function From_Callback_Data_Debugger_String
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Debugger_Cmd : constant String := Nth_Arg (Data, 3);
      Inst         : constant Class_Instance :=
        Nth_Arg (Data, 2, New_Class (Get_Kernel (Data), "Debugger"));
      Args         : constant Debugger_String_Hooks_Data :=
        (Hooks_Data with
         Length   => Debugger_Cmd'Length,
         Debugger => Visual_Debugger (GObject'(Get_Data (Inst))),
         Command  => Debugger_Cmd);
   begin
      return Args;
   end From_Callback_Data_Debugger_String;

   -----------------------
   -- Run_Debugger_Hook --
   -----------------------

   procedure Run_Debugger_Hook
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Hook     : Hook_Name)
   is
      Args   : aliased Debugger_Hooks_Data;
   begin
      --  In the case of the gdb testsuite, the debugger is null
      if Debugger /= null then
         Args := (Hooks_Data with Debugger => Visual_Debugger (Debugger));
         Run_Hook (Debugger.Window.Kernel, Hook, Args'Unchecked_Access);
      end if;
   end Run_Debugger_Hook;

   ------------------------------
   -- Run_Debugger_States_Hook --
   ------------------------------

   procedure Run_Debugger_States_Hook
     (Debugger  : access GVD.Process.Visual_Debugger_Record'Class;
      Hook      : Hook_Name;
      New_State : Debugger_State)
   is
      Kernel : constant Kernel_Handle := Debugger.Window.Kernel;
      Args   : aliased Debugger_Hooks_States_Data;
   begin
      Args := (Hooks_Data with
               Debugger  => Visual_Debugger (Debugger),
               New_State => New_State);
      Run_Hook (Kernel, Hook, Args'Unchecked_Access);
   end Run_Debugger_States_Hook;

   ---------------------------------------
   -- Run_Debugger_Hook_Until_Not_Empty --
   ---------------------------------------

   function Run_Debugger_Hook_Until_Not_Empty
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Hook     : Hook_Name;
      Command  : String) return String
   is
      Kernel : constant Kernel_Handle := Debugger.Window.Kernel;
      Args   : aliased Debugger_String_Hooks_Data :=
                 (Hooks_Data with
                  Length   => Command'Length,
                  Command  => Command,
                  Debugger => Visual_Debugger (Debugger));
   begin
      Set_Busy (Debugger);
      declare
         Tmp : constant String := Run_Hook_Until_Not_Empty
           (Kernel, Hook, Args'Unchecked_Access, Set_Busy => True);
      begin
         Set_Busy (Debugger, False);
         return Tmp;
      end;
   end Run_Debugger_Hook_Until_Not_Empty;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Data : access Hooks_Data'Class)
      return GVD.Process.Visual_Debugger is
   begin
      return Debugger_Hooks_Data_Access (Data).Debugger;
   end Get_Process;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Data : access GPS.Kernel.Hooks.Hooks_Data'Class)
      return Debugger_State is
   begin
      return Debugger_Hooks_States_Data_Access (Data).New_State;
   end Get_State;

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Script  : access Scripting_Language_Record'Class;
      Process : Visual_Debugger) return Class_Instance
   is
      Inst : Class_Instance := Get_Instance (Script, Process);
   begin
      if Inst = No_Class_Instance then
         Inst := New_Instance
           (Script, New_Class (Get_Kernel (Script), "Debugger"));
         Set_Data (Inst, GObject (Process));
      end if;
      return Inst;
   end Get_Or_Create_Instance;

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
      Arg_Cmd    : aliased constant String := "cmd";
      Arg_ID     : aliased constant String := "id";
      Arg_Exec   : aliased constant String := "executable";
      Arg_Args   : aliased constant String := "args";
      Arg_Output : aliased constant String := "output";
      Process    : Visual_Debugger;
      Inst       : Class_Instance;
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, -("Cannot create instances of Debugger directly"
            & ASCII.LF
            & "Use GPS.Debugger.get() or GPS.Debugger.spawn() instead"));

      elsif Command = "get" then
         Name_Parameters (Data, (1 => Arg_ID'Unchecked_Access));
         declare
            Id   : Natural;
            File_Inst : Class_Instance;
            File : Virtual_File;
            List : Debugger_List_Link := Get_Debugger_List (Kernel);
         begin
            if Number_Of_Arguments (Data) = 0 then
               Process := Visual_Debugger (Get_Current_Debugger (Kernel));

            else
               Id := Nth_Arg (Data, 1);
               while List /= null loop
                  Process := Visual_Debugger (List.Debugger);
                  exit when Get_Num (Process) = Gint (Id);
                  Process := null;
                  List := List.Next;
               end loop;
            end if;

         exception
            when Invalid_Data =>
               --  We got pass a file as Id
               File_Inst := Nth_Arg
                 (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
               File := Get_Data (File_Inst);

               while List /= null loop
                  Process := Visual_Debugger (List.Debugger);
                  exit when Get_Executable (Process.Debugger) = File;
                  Process := null;
                  List := List.Next;
               end loop;
         end;

         if Process = null then
            Set_Error_Msg (Data, "No such debugger");
         else
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
         end if;

      elsif Command = "list" then
         declare
            List : Debugger_List_Link := Get_Debugger_List (Kernel);
         begin
            Set_Return_Value_As_List (Data);
            while List /= null loop
               Process := Visual_Debugger (List.Debugger);
               Set_Return_Value
                 (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
               List := List.Next;
            end loop;
         end;

      elsif Command = "send" then
         Name_Parameters
           (Data,
            (1 => Arg_Cmd'Unchecked_Access,
             2 => Arg_Output'Unchecked_Access,
             3 => Show_In_Console_Cst'Access));
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));

         if Nth_Arg (Data, 4, False) then
            Process_User_Command
              (Debugger       => Process,
               Command        => Nth_Arg (Data, 2),
               Output_Command => False,  --  Done by Visible parameter
               Mode           => GVD.Types.Visible);
         else
            Set_Return_Value
              (Data, Process_User_Command
                 (Debugger       => Process,
                  Command        => Nth_Arg (Data, 2),
                  Output_Command => Nth_Arg (Data, 3, True),
                  Mode           => GVD.Types.Hidden));
         end if;

      elsif Command = "non_blocking_send" then
         Name_Parameters
           (Data,
            (1 => Arg_Cmd'Unchecked_Access,
             2 => Arg_Output'Unchecked_Access));
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Process_User_Command
           (Process, Nth_Arg (Data, 2),
            Output_Command => Nth_Arg (Data, 3, True),
            Mode           => GVD.Types.User);

      elsif Command = "command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Get_Command (Process));

      elsif Command = "is_exec_command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Is_Execution_Command (Process));

      elsif Command = "is_context_command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         if Process.Current_Command /= null then
            Set_Return_Value
              (Data,
               Is_Context_Command
                 (Process.Debugger, Process.Current_Command.all));
         else
            Set_Return_Value (Data, False);
         end if;

      elsif Command = "is_break_command" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         if Process.Current_Command /= null then
            Set_Return_Value
              (Data,
               Is_Break_Command
                 (Process.Debugger, Process.Current_Command.all));
         else
            Set_Return_Value (Data, False);
         end if;

      elsif Command = "get_executable" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value
           (Data, Create_File
              (Get_Script (Data), Get_Executable (Process.Debugger)));

      elsif Command = "get_num" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Integer (Get_Num (Process)));

      elsif Command = "is_busy" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value (Data, Command_In_Process (Process));

      elsif Command = "close" then
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Close_Debugger (Process);

      elsif Command = "spawn" then
         Name_Parameters (Data, (1 => Arg_Exec'Unchecked_Access,
                                 2 => Arg_Args'Unchecked_Access));
         declare
            File_Inst : constant Class_Instance := Nth_Arg
              (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
            File : constant Virtual_File := Get_Data (File_Inst);
         begin
            Process := Spawn
              (Kernel => Kernel,
               Kind   => Debugger_Kind.Get_Pref,
               File   => File,
               Project => Get_Project (Kernel),
               Args    => Nth_Arg (Data, 2, ""));
            Set_Return_Value
              (Data, Get_Or_Create_Instance (Get_Script (Data), Process));
         end;
      end if;
   end Shell_Handler;

   ------------------
   -- Create_Hooks --
   ------------------

   procedure Create_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Class : constant Class_Type := New_Class (Kernel, "Debugger");
   begin
      Register_Hook_Data_Type
        (Kernel,
         Data_Type_Name => Debugger_Hook_Data_Type,
         Args_Creator   => From_Callback_Data_Debugger'Access);
      Register_Hook_Data_Type
        (Kernel,
         Data_Type_Name => Debugger_Hook_States_Data_Type,
         Args_Creator   => From_Callback_Data_Debugger_States'Access);
      Register_Hook_Data_Type
        (Kernel,
         Data_Type_Name => Debugger_String_Hook_Data_Type,
         Args_Creator   => From_Callback_Data_Debugger_String'Access);

      Register_Hook_No_Return
        (Kernel, Debugger_Process_Stopped_Hook, Debugger_Hook_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_Process_Terminated_Hook, Debugger_Hook_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_Context_Changed_Hook, Debugger_Hook_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_Executable_Changed_Hook, Debugger_Hook_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_Breakpoints_Changed_Hook, Debugger_Hook_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_State_Changed_Hook, Debugger_Hook_States_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_Started_Hook, Debugger_Hook_Data_Type);
      Register_Hook_No_Return
        (Kernel, Debugger_Terminated_Hook, Debugger_Hook_Data_Type);
      Register_Hook_Return_String
        (Kernel, Debugger_Command_Action_Hook, Debugger_String_Hook_Data_Type);
      Register_Hook_Return_String
        (Kernel, Debugger_Question_Action_Hook,
         Debugger_String_Hook_Data_Type);

      --  Commands

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "get", 0, 1, Shell_Handler'Access, Class,
         Static_Method => True);
      Register_Command
        (Kernel, "list", 0, 0, Shell_Handler'Access, Class,
         Static_Method => True);
      Register_Command (Kernel, "send", 1, 3, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "non_blocking_send", 1, 1, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "get_executable", 0, 0, Shell_Handler'Access, Class);
      Register_Command (Kernel, "get_num", 0, 0, Shell_Handler'Access, Class);
      Register_Command (Kernel, "is_busy", 0, 0, Shell_Handler'Access, Class);
      Register_Command (Kernel, "close", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "spawn", 1, 2, Shell_Handler'Access, Class,
         Static_Method => True);
      Register_Command
        (Kernel, "command", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "is_exec_command", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "is_context_command", 0, 0, Shell_Handler'Access, Class);
      Register_Command
        (Kernel, "is_break_command", 0, 0, Shell_Handler'Access, Class);
   end Create_Hooks;

end GVD.Scripts;
