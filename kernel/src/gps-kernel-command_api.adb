------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Task_Manager;            use Task_Manager;

package body GPS.Kernel.Command_API is
   --  Local subprograms

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands.

   ------------------
   -- Command_Cmds --
   ------------------

   procedure Command_Cmds
     (Data : in out Callback_Data'Class; Command : String)
   is
      Cmd : Scheduled_Command_Access;
   begin
      if Command = "list" then
         declare
            Commands : constant Command_Array := Get_Scheduled_Commands
                (GPS.Kernel.Task_Manager.Get_Task_Manager
                     (Get_Kernel (Data)));
         begin
            Set_Return_Value_As_List (Data);

            for J in Commands'Range loop
               if Commands (J).all in Scheduled_Command then
                  Set_Return_Value
                    (Data, Get_Instance (Commands (J), Get_Script (Data)));
               end if;
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
               if Name (Commands (J)) = Expected_Name
                 and then Commands (J).all in Scheduled_Command
               then
                  Set_Return_Value
                    (Data, Get_Instance (Commands (J), Get_Script (Data)));
               end if;
            end loop;
         end;

      elsif Command = "interrupt" then
         Cmd := Get_Command (Data, 1);
         if Cmd /= null then
            Interrupt_Queue (Get_Kernel (Data), Cmd);
         end if;
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
        (Kernel, "interrupt", 0, 0, Command_Cmds'Access, Command_Class);
   end Register_Commands;

end GPS.Kernel.Command_API;
