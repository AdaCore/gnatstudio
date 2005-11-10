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
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;

package body GPS.Kernel.Command_API is

   Command_Class  : Class_Type;

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
      Data_Command  : Command_Access;
   begin
      Data_Command := Convert
        (Get_Data (Nth_Arg (Data, 1, Command_Class), Command_Class));

      if Command = "progress" then
         Set_Return_Value (Data, Progress (Data_Command).Current);
         Set_Return_Value_Key (Data, "current");
         Set_Return_Value (Data, Progress (Data_Command).Total);
         Set_Return_Value_Key (Data, "total");
      elsif Command = "interrupt" then
         Interrupt_Queue (Get_Kernel (Data), Data_Command);
      end if;
   end Command_Cmds;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Command_Class := New_Class (Kernel, "Command");

      Register_Command
        (Kernel, "progress", 0, 0, Command_Cmds'Access, Command_Class);
      Register_Command
        (Kernel, "interrupt", 0, 0, Command_Cmds'Access, Command_Class);
   end Register_Commands;

end GPS.Kernel.Command_API;