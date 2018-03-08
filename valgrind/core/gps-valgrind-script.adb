------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2018, AdaCore                   --
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

with GNATCOLL.Scripts;

with GPS.Valgrind.Binding;             use GPS.Valgrind.Binding;

package body GPS.Valgrind.Script is

   function Get_Valgrind_Class
     (Repo : access GNATCOLL.Scripts.Scripts_Repository_Record'Class)
      return GNATCOLL.Scripts.Class_Type;

   procedure Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String);

   Valgrind_Class_Name : constant String := "Valgrind";

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Data);
   begin
      if Command = "callgrind_dump_stats" then
         Do_Client_Request (Callgrind_Dump_Stats);
      elsif Command = "callgrind_zero_stats" then
         Do_Client_Request (Callgrind_Zero_Stats);
      elsif Command = "callgrind_toggle_collect" then
         Do_Client_Request (Callgrind_Toggle_Collect);
      elsif Command = "callgrind_start_instrumentation" then
         Do_Client_Request (Callgrind_Start_Instrumentation);
      elsif Command = "callgrind_stop_instrumentation" then
         Do_Client_Request (Callgrind_Stop_Instrumentation);
      end if;
   end Command_Handler;

   ------------------------
   -- Get_Valgrind_Class --
   ------------------------

   function Get_Valgrind_Class
     (Repo : access GNATCOLL.Scripts.Scripts_Repository_Record'Class)
      return GNATCOLL.Scripts.Class_Type is
   begin
      return Repo.New_Class (Valgrind_Class_Name);
   end Get_Valgrind_Class;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class) is
   begin
      Kernel.Scripts.Register_Command
        ("callgrind_dump_stats",
         Class         => Get_Valgrind_Class (Kernel.Scripts),
         Handler       => Command_Handler'Access,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("callgrind_zero_stats",
         Class        => Get_Valgrind_Class (Kernel.Scripts),
         Handler      => Command_Handler'Access,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("callgrind_toggle_collect",
         Class        => Get_Valgrind_Class (Kernel.Scripts),
         Handler      => Command_Handler'Access,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("callgrind_start_instrumentation",
         Class        => Get_Valgrind_Class (Kernel.Scripts),
         Handler      => Command_Handler'Access,
         Static_Method => True);
      Kernel.Scripts.Register_Command
        ("callgrind_stop_instrumentation",
         Class        => Get_Valgrind_Class (Kernel.Scripts),
         Handler      => Command_Handler'Access,
         Static_Method => True);
   end Register_Commands;

end GPS.Valgrind.Script;
