-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
     "debugger_process_stopped";
   --  Called when the debugged process ran and then stopped, for instance on
   --  a breakpoint, after a "next" command, ...

   Debugger_Context_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     "debugger_context_changed";
   --  Called when the context of the debuggee has changed, for instance after
   --  thread switching, frame selection,...

   Debugger_Executable_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     "debugger_executable_changed";
   --  Called when the executable associated with the debugger has changed, for
   --  instance via Debug->Debug->Open File. This is also called initially when
   --  the executable is given on the command line

   Debugger_Started_Hook    : constant GPS.Kernel.Hook_Name :=
     "debugger_started";
   --  Debugger_Started_Hook is called after the debugger has been spawn, and
   --  it is possible to send commands to it.
   --  Superceded by Debugger_State_Changed_Hook (when old="none")

   Debugger_Terminated_Hook : constant GPS.Kernel.Hook_Name :=
     "debugger_terminated";
   --  Debugger_Terminated_Hook is called just before the connection to the
   --  debugger is closed. It is still possible to issue commands to the
   --  debugger at this stage.
   --  Superceded by Debugger_State_Changed_Hook (when new="none")

   Debugger_State_Changed_Hook : constant GPS.Kernel.Hook_Name :=
     "debugger_state_changed";
   --  Called when the state of the debugger changes: the old and new states
   --  are passed as argument:
   --     "none": there is no more debugger running
   --     "busy": the debugger is now processing a command, and for instance
   --        the debuggee is running
   --     "idle": the debugger is waiting for user input
   --  Takes a Debugger_Hooks_States_Data as argument

end GVD;
