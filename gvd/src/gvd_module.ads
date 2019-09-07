------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This package defines the debugger module (called GVD)

with GPS.Debuggers;           use GPS.Debuggers;
with GPS.Kernel.Modules;

package GVD_Module is

   Debugger_Module_ID : GPS.Kernel.Modules.Module_ID;

   procedure Create_GVD_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create the module, and register it in the module if Kernel is not null

   function Get_Module return GPS.Kernel.Modules.Module_ID;
   --  Return the debugger module

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String);
   --  Initialize the debugger if needed

   procedure Debug_Terminate (Kernel : GPS.Kernel.Kernel_Handle);
   --  Terminate the debugging session, and closes all remaining debuggers

   function Get_Current_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return access Base_Visual_Debugger'Class;
   --  Return the current visual debugger

   procedure Set_Current_Debugger
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Current : access Base_Visual_Debugger'Class);
   --  Set the current active visual debugger

   procedure Add_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object : not null access Base_Visual_Debugger'Class);
   --  Add Object to debugger list

   procedure Remove_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Object : not null access Base_Visual_Debugger'Class);
   --  Delete Object from debugger list

   procedure For_Each_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Action : access procedure
        (Object : not null access Base_Visual_Debugger'Class));
   --  Execute callback Action for each debugger

   function Count_Running_Debuggers
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
     return Natural;
   --  Return the number of debuggers that are currently running

end GVD_Module;
