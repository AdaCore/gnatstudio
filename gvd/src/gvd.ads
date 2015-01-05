------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with GPS.Kernel;

package GVD is

   type Debugger_State is (Debug_None, Debug_Busy, Debug_Available);
   --  Possible states of a debugger:
   --  - Debug_None: debugger is not running
   --  - Debug_Busy: debugger is busy processing a command
   --  - Debug_Available: debugger is available

   -----------
   -- Hooks --
   -----------
   --  All these hooks take a Debugger_Hooks_Data in parameter

   Debugger_Process_Stopped_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_process_stopped");
   --  Called when the debugged process ran and then stopped, for instance on
   --  a breakpoint, after a "next" command, ...

   Debugger_Process_Terminated_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_process_terminated");
   --  Called when the debugged process has finished

   Debugger_Context_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_context_changed");
   --  Called when the context of the debuggee has changed, for instance after
   --  thread switching, frame selection,...

   Debugger_Executable_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_executable_changed");
   --  Called when the executable associated with the debugger has changed, for
   --  instance via Debug->Debug->Open File. This is also called initially when
   --  the executable is given on the command line

   Debugger_Breakpoints_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_breakpoints_changed");
   --  The list of breakpoints set in the debugger was reloaded. It might not
   --  have changed since the last time

   Debugger_Started_Hook    : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_started");
   --  Debugger_Started_Hook is called after the debugger has been spawn, and
   --  it is possible to send commands to it.
   --  Superceded by Debugger_State_Changed_Hook (when old="none")

   Debugger_Terminated_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_terminated");
   --  Debugger_Terminated_Hook is called just before the connection to the
   --  debugger is closed. It is still possible to issue commands to the
   --  debugger at this stage.
   --  Superceded by Debugger_State_Changed_Hook (when new="none")

   Debugger_State_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     GPS.Kernel.To_Hook_Name ("debugger_state_changed");
   --  Called when the state of the debugger changes: the old and new states
   --  are passed as argument:
   --     "none": there is no more debugger running
   --     "busy": the debugger is now processing a command, and for instance
   --        the debuggee is running
   --     "idle": the debugger is waiting for user input
   --  Takes a Debugger_Hooks_States_Data as argument

end GVD;
