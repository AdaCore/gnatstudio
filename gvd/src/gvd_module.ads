------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with Glib.Object;
with GPS.Debuggers;           use GPS.Debuggers;
with GPS.Kernel.Modules;
with GVD;                     use GVD;

package GVD_Module is

   Debugger_Module_ID : GPS.Kernel.Modules.Module_ID;

   procedure Create_GVD_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create the module, and register it in the module if Kernel is not null

   function Get_Module return GPS.Kernel.Modules.Module_ID;
   --  Return the debugger module

   type Debugger_List_Node;
   type Debugger_List_Link is access Debugger_List_Node;

   type Debugger_List_Node is record
      Debugger : access Base_Visual_Debugger'Class;
      --  The real type is a Visual_Debugger

      Next     : Debugger_List_Link;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Debugger_List_Node, Debugger_List_Link);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String);
   --  Initialize the debugger if needed

   procedure Debug_Terminate (Kernel : GPS.Kernel.Kernel_Handle);
   --  Terminate the debugging session, and closes all remaining debuggers

   procedure Setup_Side_Columns
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create the hooks that are responsible for refreshing the side column
   --  while at least one debugger is running. Does nothing if the hooks have
   --  already been created

   function Get_Debugger_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Debugger_List_Link;
   --  Return to the current list of active debuggers

   function Get_Current_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return access Base_Visual_Debugger'Class;
   --  Return the current visual debugger

   procedure Set_First_Debugger
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : Debugger_List_Link);
   --  Set the first debugger returned by Get_Debugger_List

   procedure Set_Current_Debugger
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Current : access Base_Visual_Debugger'Class);
   --  Set the current active visual debugger

end GVD_Module;
