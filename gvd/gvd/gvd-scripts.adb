-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005                           --
--                               AdaCore                             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Debugger;           use Debugger;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GPS.Intl;           use GPS.Intl;
with GVD.Process;        use GVD.Process;
with GVD_Module;         use GVD_Module;
with VFS;                use VFS;

package body GVD.Scripts is

   Debugger_Hook_Data_Name : constant String := "Debugger";

   procedure Debugger_Hooks_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Process the hook types associated with the debugger

   procedure Shell_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module.

   function Get_Or_Create_Instane
     (Script  : access Scripting_Language_Record'Class;
      Process : Visual_Debugger) return Class_Instance;
   --  Get or create an existing instance associated with Process.

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Data : Debugger_Hooks_Data) return String is
      pragma Unreferenced (Data);
   begin
      return Debugger_Hook_Data_Name;
   end Get_Name;

   -------------------
   -- Execute_Shell --
   -------------------

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Debugger_Hooks_Data) return Boolean
   is
      pragma Unreferenced (Data);
      D   : Callback_Data'Class := Create (Script, 2);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (D, 1, Hook_Name);
      Set_Nth_Arg (D, 2, "debugger");  --  ??? Should contain the instance
      Tmp := Execute (Command, D);
      --  Free instance if needed
      return Tmp;
   end Execute_Shell;

   ----------------------------
   -- Debugger_Hooks_Handler --
   ----------------------------

   procedure Debugger_Hooks_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Args : aliased Debugger_Hooks_Data;
   begin
      Args := (Kernel   => Get_Kernel (Data),
               Debugger => null);  --  ??? Get from instance
      Run_Hook (Get_Kernel (Data),
                Get_Hook_Name (Data, 1),
                Args'Unchecked_Access);
   end Debugger_Hooks_Handler;

   -----------------------
   -- Run_Debugger_Hook --
   -----------------------

   procedure Run_Debugger_Hook
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Name     : String)
   is
      Kernel : constant Kernel_Handle := Debugger.Window.Kernel;
      Args : aliased Debugger_Hooks_Data;
   begin
      Args := (Kernel   => Kernel,
               Debugger => Visual_Debugger (Debugger));
      Run_Hook (Kernel, Name, Args'Unchecked_Access);
   end Run_Debugger_Hook;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Data : access Hooks_Data'Class)
      return GVD.Process.Visual_Debugger is
   begin
      return Debugger_Hooks_Data_Access (Data).Debugger;
   end Get_Process;

   ---------------------------
   -- Get_Or_Create_Instane --
   ---------------------------

   function Get_Or_Create_Instane
     (Script  : access Scripting_Language_Record'Class;
      Process : Visual_Debugger) return Class_Instance
   is
      Inst : Class_Instance := Get_Instance (Script, Process);
   begin
      if Inst = null then
         Inst := New_Instance
           (Script, New_Class (Get_Kernel (Script), "Debugger"));
         Set_Data (Inst, GObject (Process));
      end if;
      return Inst;
   end Get_Or_Create_Instane;

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
      Arg_Cmd  : aliased constant String := "cmd";
      Arg_ID   : aliased constant String := "id";
      Arg_File : aliased constant String := "file";
      Arg_Args : aliased constant String := "args";
      Process : Visual_Debugger;
      Inst    : Class_Instance;
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
            Id := Nth_Arg (Data, 1);
            while List /= null loop
               Process := Visual_Debugger (List.Debugger);
               exit when Get_Num (Process) = Gint (Id);
               Process := null;
               List := List.Next;
            end loop;

         exception
            when Invalid_Data =>
               --  We got pass a file as Id
               File_Inst := Nth_Arg
                 (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
               File := Get_File (Get_Data (File_Inst));
               Free (File_Inst);

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
              (Data, Get_Or_Create_Instane (Get_Script (Data), Process));
         end if;

      elsif Command = "list" then
         declare
            List : Debugger_List_Link := Get_Debugger_List (Kernel);
         begin
            Set_Return_Value_As_List (Data);
            while List /= null loop
               Process := Visual_Debugger (List.Debugger);
               Set_Return_Value
                 (Data, Get_Or_Create_Instane (Get_Script (Data), Process));
               List := List.Next;
            end loop;
         end;

      elsif Command = "send" then
         Name_Parameters (Data, (1 => Arg_Cmd'Unchecked_Access));
         Inst := Nth_Arg (Data, 1, New_Class (Kernel, "Debugger"));
         Process := Visual_Debugger (GObject'(Get_Data (Inst)));
         Set_Return_Value
           (Data, Process_User_Command
              (Process, GPS.Kernel.Scripts.Nth_Arg (Data, 2),
               Output_Command => True));

      elsif Command = "get_file" then
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
         Name_Parameters (Data, (1 => Arg_File'Unchecked_Access,
                                 2 => Arg_Args'Unchecked_Access));
         declare
            File_Inst : constant Class_Instance := Nth_Arg
              (Data, 1, Get_File_Class (Kernel), Allow_Null => False);
            File : constant Virtual_File := Get_File (Get_Data (File_Inst));
         begin
            Free (File_Inst);
            Process := Spawn
              (Kernel => Kernel,
               File   => File,
               Project => Get_Project (Kernel),
               Args    => Nth_Arg (Data, 2, ""));
            Set_Return_Value
              (Data, Get_Or_Create_Instane (Get_Script (Data), Process));
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
      Create_Hook_Type
        (Kernel           => Kernel,
         Type_Name        => Debugger_Hook_Data_Name,
         Profile          => Hook_With_Args,
         Run_Hook_Handler => Debugger_Hooks_Handler'Access);

      Register_Hook
        (Kernel, Debugger_Process_Stopped_Hook,
         Type_Name => Debugger_Hook_Data_Name);
      Register_Hook (Kernel, Debugger_Started_Hook);
      Register_Hook (Kernel, Debugger_Terminated_Hook);

      --  Commands

      GPS.Kernel.Scripts.Register_Command
        (Kernel, Constructor_Method, 0, 0, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get", 1, 1, Shell_Handler'Access, Class,
         Static_Method => True);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "list", 0, 0, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "send", 1, 1, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get_file", 0, 0, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get_num", 0, 0, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "is_busy", 0, 0, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "close", 0, 0, Shell_Handler'Access, Class);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "spawn", 1, 2, Shell_Handler'Access, Class,
         Static_Method => True);
   end Create_Hooks;

end GVD.Scripts;
