-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

--  This package defines the hooks used in the GVD module

with GNATCOLL.Scripts;

with GPS.Kernel;       use GPS.Kernel;
with GPS.Kernel.Hooks;
with GVD.Process;

package GVD.Scripts is

   type Debugger_Hooks_Data is new GPS.Kernel.Hooks.Hooks_Data with private;
   type Debugger_Hooks_Data_Access is access all Debugger_Hooks_Data'Class;
   --  Data associated with all the hooks declared in the GVD module.
   --  It contains a pointer to the debugger that generated the event

   type Debugger_String_Hooks_Data (Length : Natural) is new
     GPS.Kernel.Hooks.Hooks_Data with
      record
         Process : GVD.Process.Visual_Debugger;
         Command : String (1 .. Length);
      end record;
   type Debugger_String_Hooks_Data_Access is access all
     Debugger_String_Hooks_Data'Class;

   procedure Run_Debugger_Hook
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Hook     : Hook_Name);
   --  Run a hook and passes Debugger as a parameter to it

   function Run_Debugger_Hook_Until_Not_Empty
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Hook     : Hook_Name;
      Command  : String) return String;
   --  Execute the hook until one of the callbacks return a non-empty string

   function Get_Process
     (Data : access GPS.Kernel.Hooks.Hooks_Data'Class)
      return GVD.Process.Visual_Debugger;
   --  Return the debugger stored in Data

   procedure Create_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create the hooks and shell API

   -----------
   -- Hooks --
   -----------
   --  All these hooks take a Debugger_Hooks_Data in parameter

   Debugger_Process_Stopped_Hook : constant Hook_Name :=
     "debugger_process_stopped";
   --  Called when the debugged process ran and then stopped, for instance on
   --  a breakpoint, after a "next" command, ...

   Debugger_Context_Changed_Hook : constant Hook_Name :=
     "debugger_context_changed";
   --  Called when the context of the debuggee has changed, for instance after
   --  thread switching, frame selection,...

   Debugger_Executable_Changed_Hook : constant Hook_Name :=
     "debugger_executable_changed";
   --  Called when the executable associated with the debugger has changed, for
   --  instance via Debug->Debug->Open File. This is also called initially when
   --  the executable is given on the command line

   Debugger_Started_Hook    : constant Hook_Name := "debugger_started";
   --  Debugger_Started_Hook is called after the debugger has been spawn, and
   --  it is possible to send commands to it

   Debugger_Terminated_Hook : constant Hook_Name := "debugger_terminated";
   --  Debugger_Terminated_Hook is called just before the connection to the
   --  debugger is closed. It is still possible to issue commands to the
   --  debugger at this stage.

   ------------------
   -- Action Hooks --
   ------------------
   --  All these hooks take a Debugger_Action_Hooks_Data in parameter

   Debugger_Command_Action_Hook : constant Hook_Name :=
     "debugger_command_action_hook";
   --  Action hook called when the user has typed a command in the debugger
   --  console. This hooks gives a chance to scripts to implement their own
   --  debugger commands.

   Debugger_Question_Action_Hook : constant Hook_Name :=
     "debugger_question_action_hook";
   --  Action hook called just before displaying an interactive dialog, when
   --  the underlying debugger is asking a question to the user. This hook
   --  can be used to disable the dialog (and send the reply directly to the
   --  debugger instead). It should return a non-empty string to pass to the
   --  debugger if the dialog should not be displayed.
   --  You cannot send any command to the debugger in this hook.
   --  The string parameter contains the debugger question

private
   type Debugger_Hooks_Data is new GPS.Kernel.Hooks.Hooks_Data with record
      Debugger : GVD.Process.Visual_Debugger;
   end record;

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_Hooks_Data)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited documentation

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_String_Hooks_Data)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited documentation

end GVD.Scripts;
