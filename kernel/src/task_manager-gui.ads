------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

--  This package handles the GUI part of the task manager

with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GPS.Kernel;       use GPS.Kernel;
with GPS.Kernel.Hooks; use GPS.Kernel.Hooks;

package Task_Manager.GUI is

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Initialize support for the task manager view, and reg

   -----------
   -- Hooks --
   -----------

   Task_Hook_Type : constant Hook_Type := "task_hooks";
   type Task_Hooks_Args is new Hooks_Data with record
      Queue_ID : Integer;
   end record;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Task_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Hooks that take a preference in parameter

   Task_Started_Hook : constant Hook_Name := To_Hook_Name ("task_started");
   Task_Terminated_Hook : constant Hook_Name :=
     To_Hook_Name ("task_terminated");
   Task_Changed_Hook : constant Hook_Name := To_Hook_Name ("task_changed");

end Task_Manager.GUI;
