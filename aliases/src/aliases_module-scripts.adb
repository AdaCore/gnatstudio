------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

   procedure Aliases_Static_Get
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Static alias getter for the Alias class

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
         Set_Property
           (Alias_Instance, "name", Alias.Get_Name);
         Set_Property
           (Alias_Instance, "expansion", Alias.Get_Expansion);
         Set_Return_Value (Data, Alias_Instance);
      end if;
   end Aliases_Static_Get;

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
   end Register_Commands;

end Aliases_Module.Scripts;
