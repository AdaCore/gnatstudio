------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package defines the hooks used in the GVD module

with GNATCOLL.Scripts;

with GPS.Kernel;       use GPS.Kernel;
with GPS.Kernel.Hooks;
with GVD.Process;

package GVD.Scripts is

   type Debugger_Hooks_Data is new GPS.Kernel.Hooks.Hooks_Data with record
      Debugger : GVD.Process.Visual_Debugger;
   end record;
   type Debugger_Hooks_Data_Access is access all Debugger_Hooks_Data'Class;
   --  Data associated with all the hooks declared in the GVD module.
   --  It contains a pointer to the debugger that generated the event

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_Hooks_Data)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited documentation

   type Debugger_Hooks_States_Data is new Debugger_Hooks_Data with
      record
         New_State : Debugger_State;
      end record;
   type Debugger_Hooks_States_Data_Access
     is access all Debugger_Hooks_States_Data'Class;

   type Debugger_String_Hooks_Data (Length : Natural) is new
     Debugger_Hooks_Data with
      record
         Command : String (1 .. Length);
      end record;
   type Debugger_String_Hooks_Data_Access is access all
     Debugger_String_Hooks_Data'Class;

   procedure Run_Debugger_Hook
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Hook     : Hook_Name);
   --  Run a hook and passes Debugger as a parameter to it

   procedure Run_Debugger_States_Hook
     (Debugger  : access GVD.Process.Visual_Debugger_Record'Class;
      Hook      : Hook_Name;
      New_State : Debugger_State);
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

   function Get_State
     (Data : access GPS.Kernel.Hooks.Hooks_Data'Class)
      return Debugger_State;
   --  Return the state stored in Data. This only works if Data is of type
   --  Debugger_Hooks_States_Data, but this subprogram is needed for
   --  instantiation of GVD.Generic_Views

   procedure Create_Hooks
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create the hooks and shell API

   ------------------
   -- Action Hooks --
   ------------------
   --  All these hooks take a Debugger_Action_Hooks_Data in parameter

   Debugger_Command_Action_Hook : constant Hook_Name :=
     To_Hook_Name ("debugger_command_action_hook");
   --  Action hook called when the user has typed a command in the debugger
   --  console. This hooks gives a chance to scripts to implement their own
   --  debugger commands.

   Debugger_Question_Action_Hook : constant Hook_Name :=
     To_Hook_Name ("debugger_question_action_hook");
   --  Action hook called just before displaying an interactive dialog, when
   --  the underlying debugger is asking a question to the user. This hook
   --  can be used to disable the dialog (and send the reply directly to the
   --  debugger instead). It should return a non-empty string to pass to the
   --  debugger if the dialog should not be displayed.
   --  You cannot send any command to the debugger in this hook.
   --  The string parameter contains the debugger question

private
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_String_Hooks_Data)
      return GNATCOLL.Scripts.Callback_Data_Access;
   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Debugger_Hooks_States_Data)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited documentation

end GVD.Scripts;
