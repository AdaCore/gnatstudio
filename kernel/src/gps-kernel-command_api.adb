-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2005                            --
--                             AdaCore                               --
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

with Ada.Unchecked_Conversion;

with Commands;                use Commands;
with Task_Manager;            use Task_Manager;

with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;

package body GPS.Kernel.Command_API is

   --  Local subprograms

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Command_Access);

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands.

   ------------------
   -- Command_Cmds --
   ------------------

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Command_Class    : constant Class_Type :=
        New_Class (Get_Kernel (Data), "Command");
      Data_Command     : Command_Access;
      Command_Instance : Class_Instance;
   begin
      if Command = "list" then
         declare
            Commands : constant Command_Array := Get_Scheduled_Commands
                (GPS.Kernel.Task_Manager.Get_Task_Manager
                     (Get_Kernel (Data)));
         begin
            Set_Return_Value_As_List (Data);

            for J in Commands'Range loop
               Set_Return_Value
                 (Data, Get_Command_Instance
                    (Get_Script (Data), Commands (J)));
            end loop;
         end;
      elsif Command = "get" then
         declare
            Commands : constant Command_Array := Get_Scheduled_Commands
              (GPS.Kernel.Task_Manager.Get_Task_Manager
                 (Get_Kernel (Data)));
            Expected_Name : constant String := Nth_Arg (Data, 1, "");
         begin
            Set_Return_Value_As_List (Data);

            for J in Commands'Range loop
               if Name (Commands (J)) = Expected_Name then
                  Set_Return_Value
                    (Data, Get_Command_Instance
                       (Get_Script (Data), Commands (J)));
               end if;
            end loop;
         end;
      elsif Command = "progress" then
         Command_Instance := Nth_Arg (Data, 1, Command_Class);
         Data_Command := Convert
           (Get_Data (Command_Instance, Command_Class));

         Set_Return_Value (Data, Progress (Data_Command).Current);
         Set_Return_Value_Key (Data, "current");
         Set_Return_Value (Data, Progress (Data_Command).Total);
         Set_Return_Value_Key (Data, "total");
      elsif Command = "interrupt" then
         Command_Instance := Nth_Arg (Data, 1, Command_Class);
         Data_Command := Convert
           (Get_Data (Command_Instance, Command_Class));

         Interrupt_Queue (Get_Kernel (Data), Data_Command);
      elsif Command = "name" then
         Command_Instance := Nth_Arg (Data, 1, Command_Class);
         Data_Command := Convert
           (Get_Data (Command_Instance, Command_Class));

         Set_Return_Value (Data, Name (Data_Command));
      elsif Command = "get_result" then
         Set_Return_Value
           (Data,
            "Error: this primitive should be implemeted by subclasses");
      end if;

      if Command_Instance /= null then
         Free (Command_Instance);
      end if;
   end Command_Cmds;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Command_Class  : constant Class_Type := New_Class (Kernel, "Command");
   begin
      Register_Command
        (Kernel, "list", 0, 0, Command_Cmds'Access, Command_Class, True);
      Register_Command
        (Kernel, "get", 0, 0, Command_Cmds'Access, Command_Class, True);
      Register_Command
        (Kernel, "progress", 0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel, "interrupt", 0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel, "name", 0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel, "get_result", 0, 0, Command_Cmds'Access, Command_Class);
   end Register_Commands;

end GPS.Kernel.Command_API;