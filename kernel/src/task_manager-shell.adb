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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;

with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;

package body Task_Manager.Shell is

   Class      : constant String := "Task";
   Task_Class : Class_Type;

   -------------------
   -- Shell_Command --
   -------------------

   --  This is a command type that can be created from Python

   type Shell_Command is new Root_Command with record
      Kernel  : Kernel_Handle;
      Name    : Unbounded_String;
      Execute : Subprogram_Type;
   end record;
   type Shell_Command_Access is access all Shell_Command'Class;

   overriding function Name (Command : access Shell_Command) return String
     is (To_String (Command.Name));

   overriding function Execute
     (Command : access Shell_Command) return Command_Return_Type;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Task_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Task commands

   function Get_Or_Create_Instance
     (Kernel : Kernel_Handle;
      Script : Scripting_Language;
      Id     : String) return Class_Instance;
   --  Get or create a task instance

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Shell_Command) return Command_Return_Type
   is
      S : constant Scripting_Language := Command.Execute.Get_Script;
      The_Task : constant Class_Instance := Get_Or_Create_Instance
        (Command.Kernel, S, To_String (Command.Name));
      C : Callback_Data'Class := Create
        (S, Arguments_Count => 1);
   begin
      Set_Nth_Arg (C, 1, The_Task);
      declare
         Result : constant String := To_Upper (Execute (Command.Execute, C));
      begin
         --  Do this to avoid raising Constraint_Errors
         for J in Command_Return_Type loop
            if Result = J'Img then
               return J;
            end if;
         end loop;

         --  If we reach this, this means the command didn't return a valid
         --  string: print an error and abort the command.

         Command.Kernel.Insert
           (Text => "Task 'execute' method didn't return an expected value",
            Add_LF => True,
            Mode   => Error);
         return Failure;
      end;
   end Execute;

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Data  : GNATCOLL.Scripts.Callback_Data'Class;
      Id    : String) return GNATCOLL.Scripts.Class_Instance is
   begin
      return Get_Or_Create_Instance
        (Get_Kernel (Data), Get_Script (Data), Id);
   end Get_Or_Create_Instance;

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Kernel : Kernel_Handle;
      Script : Scripting_Language;
      Id     : String) return Class_Instance
   is
      Manager : constant Task_Manager_Access := Get_Task_Manager (Kernel);
      Queue   : constant Task_Queue_Access := Queue_From_Id (Manager, Id);
   begin
      if Queue = null then
         return No_Class_Instance;
      end if;

      if Queue.Inst = No_Class_Instance then
         Queue.Inst := New_Instance (Script, Task_Class);
      end if;

      Set_Data (Queue.Inst, Task_Class, Id);
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
      S         : Scheduled_Command_Access;
      Progress  : Progress_Record;

      function Get_Id_Arg_1 return String;
      --  Convenience function to get the Task Id contained in Arg 1

      function Get_Task_Arg_1 return Task_Queue_Access;
      --  Convenience function to get the task contained in Arg 1

      function Get_Id_Arg_1 return String is
         Task_Inst : Class_Instance;
      begin
         Task_Inst := Nth_Arg (Data, 1, Task_Class);
         return Get_Data (Task_Inst, Task_Class);
      end Get_Id_Arg_1;

      function Get_Task_Arg_1 return Task_Queue_Access is
      begin
         return Queue_From_Id (Manager, Get_Id_Arg_1);
      end Get_Task_Arg_1;

   begin
      if Manager = null then
         return;
      end if;

      if Command = Constructor_Method then
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Task_Class);
            Name : constant String := Nth_Arg (Data, 2);
            Active     : constant Boolean := Nth_Arg (Data, 3, False);
            Block_Exit : constant Boolean := Nth_Arg (Data, 4, False);
            TC   : Shell_Command_Access;
         begin
            TC := new Shell_Command;
            TC.Kernel := Kernel;
            --  ??? name_arguments here or something
            TC.Name := To_Unbounded_String (Name);
            TC.Execute := Nth_Arg (Data, 3);

            Launch_Background_Command (Kernel            => Kernel,
                                       Command           => TC,
                                       Active            => Active,
                                       Show_Bar          => True,
                                       Queue_Id          => Name,
                                       Block_Exit        => Block_Exit,
                                       Start_Immediately => True);

            Set_Data (Inst, Task_Class, Name);
         end;

      elsif Command = "list" then
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
               Task_Inst := Get_Or_Create_Instance
                 (Data,
                  To_String (Manager.Queues (J).Id));
               if Task_Inst /= No_Class_Instance then
                  Set_Return_Value (Data, Task_Inst);
               end if;
            end if;
         end loop;

      elsif Command = "interrupt" then
         Interrupt_Command (Manager, Get_Id_Arg_1);

      elsif Command = "pause" then
         Pause_Command (Manager, Get_Id_Arg_1);

      elsif Command = "resume" then
         Resume_Command (Manager, Get_Id_Arg_1);

      elsif Command = "status" then
         Set_Return_Value (Data, Get_Task_Arg_1.Status'Img);

      elsif Command = "block_exit" then
         Q := Get_Task_Arg_1;
         Set_Return_Value (Data, Q.Block_Exit and then Q.Show_Bar);

      elsif Command = "visible" then
         Data.Set_Return_Value (Get_Task_Arg_1.Show_Bar);

      elsif Command = "progress" then
         Q := Get_Task_Arg_1;
         if Q.Queue.Is_Empty then
            --  the task might have been terminated already, or not be started
            --  yet (since a GPS.Task represents a queue, it might not contain
            --  a command yet).

            Set_Return_Value_As_List (Data);
            Set_Return_Value (Data, Q.Done);
            Set_Return_Value (Data, Q.Total);

         else
            C := Q.Queue.First_Element;
            Progress := C.Progress;

            Set_Return_Value_As_List (Data);
            Set_Return_Value (Data, Progress.Current);
            Set_Return_Value (Data, Progress.Total);
         end if;

      elsif Command = "name" then
         Q := Get_Task_Arg_1;
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

      elsif Command = "set_progress" then
         S := Head (Get_Task_Manager (Kernel), Get_Id_Arg_1);
         if S /= null then
            Set_Progress (Command  => S,
                          Progress => (Unknown,
                                       Nth_Arg (Data, 2),
                                       Nth_Arg (Data, 3)));
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

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params  => (Param ("name"),
                     Param ("execute"),
                     Param ("active",     Optional => True),
                     Param ("block_exit", Optional => True)),
         Handler => Task_Command_Handler'Access,
         Class   => Task_Class);
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
      Kernel.Scripts.Register_Command
        ("set_progress",
         Params  => (Param ("current"),
                     Param ("total")),
         Handler => Task_Command_Handler'Access,
         Class   => Task_Class);
   end Register_Commands;

end Task_Manager.Shell;
