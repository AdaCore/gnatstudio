------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GPS.Kernel.Scripts;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Aliases_Module.Scripts is

   Alias_Class_Name : constant String := "Alias";

   Name_Cst         : aliased constant String := "name";

   procedure Aliases_Static_Get
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Static alias getter for the Alias class

   procedure Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String);

   ------------------------
   -- Aliases_Static_Get --
   ------------------------

   procedure Aliases_Static_Get
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Alias_Name     : constant String := Nth_Arg (Data, 1);
      Alias          : constant Alias_Type := Get_Alias (Alias_Name);
      Target_Class   : constant Class_Type :=
        Data.Get_Repository.New_Class (Alias_Class_Name);
      Alias_Instance : constant Class_Instance :=
        Data.Get_Script.New_Instance (Target_Class);
   begin
      if Alias /= No_Alias then
         Set_Data
           (Alias_Instance, Target_Class, Alias_Name);
         Set_Property
           (Alias_Instance, "name", Alias.Get_Name);
         Set_Property
           (Alias_Instance, "expansion", Alias.Get_Expansion);
         Set_Return_Value (Data, Alias_Instance);
      end if;
   end Aliases_Static_Get;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String)
   is
      Target_Class   : constant Class_Type :=
        Data.Get_Repository.New_Class (Alias_Class_Name);
      Inst           : constant Class_Instance := Nth_Arg (Data, 1);
      Alias_Name     : constant String := Get_Data (Inst, Target_Class);
      Alias          : constant Alias_Type := Get_Alias (Alias_Name);
   begin
      if Command = "get_expanded" then
         Set_Return_Value (Data, Alias.Expand_Macro);
      elsif Command = "get_default_value" then
         Name_Parameters (Data, (1 => Name_Cst'Access));
         Set_Return_Value (Data, Alias.Get_Default_Value (Nth_Arg (Data, 2)));
      end if;
   end Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : GPS.Kernel.Kernel_Handle) is
      Target_Class : constant Class_Type :=
        New_Class (Kernel.Scripts, Alias_Class_Name);
   begin
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Target_Class,
         Handler      => Aliases_Static_Get'Access,
         Static_Method => True);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get_expanded",
         Class        => Target_Class,
         Handler      => Command_Handler'Access);
      GPS.Kernel.Scripts.Register_Command
        (Kernel, "get_default_value",
         Class        => Target_Class,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Command_Handler'Access);
   end Register_Commands;

end Aliases_Module.Scripts;
