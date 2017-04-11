------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;

with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;

package body Task_Manager.Shell is

   Class      : constant String := "Task";
   Task_Class : Class_Type;

   procedure Task_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Task commands

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Data  : Callback_Data'Class;
      Index : Integer) return Class_Instance
   is
      Manager : constant Task_Manager_Access :=
        Get_Task_Manager (Get_Kernel (Data));
      Queue   : constant Task_Queue_Access := Manager.Queues (Index);
   begin
      if Queue.Inst = No_Class_Instance then
         Queue.Inst := New_Instance (Get_Script (Data), Task_Class);
      end if;

      Set_Data (Queue.Inst, Task_Class, Index);
      return Queue.Inst;
   end Get_Or_Create_Instance;

   --------------------------
   -- Task_Command_Handler --
   --------------------------

   procedure Task_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Data);
      Task_Inst : Class_Instance;
      Manager   : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      Q         : Task_Queue_Access;
      C         : Command_Access;
      Progress  : Progress_Record;
      Id        : Integer;
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
            --  The "Completed" queues are not meant to be shown to the
            --  external world: these are queues that have just been
            --  interrupted and will be destroyed at the next iteration
            --  of the task manager. So, do not include in the list of
            --  tasks.
            if Manager.Queues (J).Status /= Completed then
               Task_Inst := Get_Or_Create_Instance (Data, J);
               Set_Return_Value (Data, Task_Inst);
            end if;
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
         Id := Get_Data (Task_Inst, Task_Class);
         Set_Return_Value (Data, Manager.Queues (Id).Status'Img);

      elsif Command = "block_exit" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Id := Get_Data (Task_Inst, Task_Class);
         Set_Return_Value
           (Data, Manager.Queues (Id).Block_Exit
            and then Manager.Queues (Id).Show_Bar);

      elsif Command = "visible" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Id := Get_Data (Task_Inst, Task_Class);
         Data.Set_Return_Value (Manager.Queues (Id).Show_Bar);

      elsif Command = "progress" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Id := Get_Data (Task_Inst, Task_Class);
         if Manager.Queues (Id).Queue.Is_Empty then
            --  the task might have been terminated already, or not be started
            --  yet (since a GPS.Task represents a queue, it might not contain
            --  a command yet).

            Set_Return_Value_As_List (Data);
            Set_Return_Value (Data, Manager.Queues (Id).Done);
            Set_Return_Value (Data, Manager.Queues (Id).Total);

         else
            C := Manager.Queues (Id).Queue.First_Element;
            Progress := C.Progress;

            Set_Return_Value_As_List (Data);
            Set_Return_Value (Data, Progress.Current);
            Set_Return_Value (Data, Progress.Total);
         end if;

      elsif Command = "name" then
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         Q := Manager.Queues (Get_Data (Task_Inst, Task_Class));
         if Q.Queue.Is_Empty then
            Data.Set_Return_Value (String'(""));
         else
            declare
               N : constant String := Q.Queue.First_Element.Name;
            begin
               if N = "" then
                  Data.Set_Return_Value (To_String (Q.Id));
               else
                  Data.Set_Return_Value (N);
               end if;
            end;
         end if;
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
        (Kernel, "block_exit", 0, 0, Task_Command_Handler'Access, Task_Class);
      Register_Command
        (Kernel, "status", 0, 0, Task_Command_Handler'Access, Task_Class);
      Kernel.Scripts.Register_Property
        ("visible", Task_Class, Getter => Task_Command_Handler'Access);
      Register_Command
        (Kernel, "progress", 0, 0, Task_Command_Handler'Access, Task_Class);
   end Register_Commands;

end Task_Manager.Shell;
