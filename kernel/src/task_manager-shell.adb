-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009-2010, AdaCore                 --
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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;

with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with Task_Manager.GUI;        use Task_Manager.GUI;

package body Task_Manager.Shell is

   Class      : constant String := "Task";
   Task_Class : Class_Type;

   procedure Task_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Task commands

   --------------------------
   -- Task_Command_Handler --
   --------------------------

   procedure Task_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Data);
      Task_Inst : Class_Instance;
      Manager   : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      C         : Command_Access;
   begin
      if Manager = null then
         return;
      end if;

      if Command = "list" then
         Set_Return_Value_As_List (Data);

         if Manager.Queues = null then
            return;
         end if;

         for J in Manager.Queues.all'Range loop
            Task_Inst := New_Instance (Get_Script (Data), Task_Class);
            Set_Data (Task_Inst, Task_Class, J);
            Set_Return_Value (Data, Task_Inst);
         end loop;

      elsif Command = "interrupt" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Interrupt_Command (Manager, Get_Data (Task_Inst, Task_Class));

      elsif Command = "pause" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Pause_Command (Manager, Get_Data (Task_Inst, Task_Class));

      elsif Command = "resume" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Resume_Command (Manager, Get_Data (Task_Inst, Task_Class));

      elsif Command = "status" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Set_Return_Value
           (Data,
            Manager.Queues (Get_Data (Task_Inst, Task_Class)).Status'Img);

      elsif Command = "name" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         C := Manager.Queues
           (Get_Data (Task_Inst, Task_Class)).Queue.First_Element;
         Set_Return_Value (Data, Commands.Name (C));
      end if;
   end Task_Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Task_Class := New_Class (Kernel, Class);

      Register_Command
        (Kernel, "list", 0, 0, Task_Command_Handler'Access, Task_Class, True);
      Register_Command
        (Kernel, "interrupt", 0, 0, Task_Command_Handler'Access, Task_Class);
      Register_Command
        (Kernel, "pause", 0, 0, Task_Command_Handler'Access, Task_Class);
      Register_Command
        (Kernel, "resume", 0, 0, Task_Command_Handler'Access, Task_Class);
      Register_Command
        (Kernel, "name", 0, 0, Task_Command_Handler'Access, Task_Class);
      Register_Command
        (Kernel, "status", 0, 0, Task_Command_Handler'Access, Task_Class);
   end Register_Commands;

end Task_Manager.Shell;
