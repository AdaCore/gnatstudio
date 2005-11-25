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

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GVD.Process;        use GVD.Process;

package body GVD.Scripts is

   Debugger_Hook_Data_Name : constant String := "Debugger";

   procedure Debugger_Hooks_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Process the hook types associated with the debugger

   procedure Debugger_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Interactive script handler for the debugger module.

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

   ------------------------------
   -- Debugger_Command_Handler --
   ------------------------------

   procedure Debugger_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := GPS.Kernel.Scripts.Get_Kernel (Data);
   begin
      if Command = "send" then
         declare
            Process : constant Visual_Debugger :=
              Get_Current_Process (Get_Main_Window (Kernel));
         begin
            Process_User_Command
              (Process,
               GPS.Kernel.Scripts.Nth_Arg (Data, 2),
               Output_Command => True);
         end;

      elsif Command = Constructor_Method then
         --  Nothing to do for now. The plan, ultimately, is to be able to
         --  control which debugger the commands will apply to by selecting
         --  a debugger from the constructor, for instance by passing a name
         --  to the constructor
         null;
      end if;
   end Debugger_Command_Handler;

   ------------------
   -- Create_Hooks --
   ------------------

   procedure Create_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debugger_Class : constant Class_Type := New_Class (Kernel, "Debugger");
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
        (Kernel, Constructor_Method,
         Class         => Debugger_Class,
         Handler       => Debugger_Command_Handler'Access);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "send",
         Class         => Debugger_Class,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Handler       => Debugger_Command_Handler'Access);
   end Create_Hooks;

end GVD.Scripts;
