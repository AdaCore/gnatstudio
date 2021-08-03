------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with GPS.Kernel; use GPS.Kernel;
with GPS.Debuggers;
with GPS.Kernel.Hooks;

package LSP.DAP_Module is

   type On_Breakpoint_Added is new GPS.Kernel.Hooks
     .Debugger_Breakpoint_Hook_Function with
   null record;
   overriding procedure Execute
     (Self     : On_Breakpoint_Added;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Id       : Integer);

   type On_Breakpoint_Deleted is new GPS.Kernel.Hooks
     .Debugger_Breakpoint_Hook_Function with
   null record;
   overriding procedure Execute
     (Self     : On_Breakpoint_Deleted;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Id       : Integer);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   procedure DAP_Terminate (Kernel : GPS.Kernel.Kernel_Handle);
   --  Terminate the debugging session, and closes all remaining debuggers

private

   procedure Send_Breakpoint_Request;

   procedure Update_Breakpoints;

end LSP.DAP_Module;
