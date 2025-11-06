------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPS.Kernel;        use GPS.Kernel;
with GPS.Kernel.Messages;

with DAP.Clients;

package DAP.Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   procedure Terminate_Debuggers;
   --  Terminate all debuggers

   procedure Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "";
      Remote_Target   : String := "");
   function Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "";
      Remote_Target   : String := "")
      return DAP.Clients.DAP_Client_Access;
   --  Initialize a DAP debugging session.
   --  File and Project are used to refer to the executable we want to debug.
   --  When not specified, the debugger will start without any executable to
   --  debug: this is the case when the user wants to attach to an already
   --  running executable for instance.
   --  Executable_Args contain the extra arguments that will be passed
   --  to the debugged executable.

   procedure Start_Executable
     (Kernel               : not null Kernel_Handle;
      Client               : not null DAP.Clients.DAP_Client_Access;
      Display_Start_Dialog : Boolean := False);
   --  Start the executable attached with the given DAP debugger.
   --  If Display_Start_Dialog is True, a modal dialog asking for arguments
   --  to be passed to the executable will be displayed.

   procedure Finished (Id : Positive);
   --  Called when some debugger is finished

   function Get_Current_Debugger return DAP.Clients.DAP_Client_Access;
   --  Returns the debugger that is "selected" now if several are started

   procedure Set_Current_Debugger (Current : DAP.Clients.DAP_Client_Access);
   --  Set the current debugger.

   function Get_Debugger (Id : Integer) return DAP.Clients.DAP_Client_Access;
   --  Return the debugger associated with the given Id.

   function Count_Running_Debuggers return Natural;
   --  Returns the count for the running debuggers

   procedure For_Each_Debugger
     (Callback : access procedure (Debugger : DAP.Clients.DAP_Client_Access));
   --  Calls Callback for each debugger

   function Get_Started_Per_Session_Debuggers return Integer;
   --  Returns count of debuggers that have been started in parallel
   --  in current session.

   procedure Remove_Continue_To_Line_Messages
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Remove the message associated with the "Continue to line" action

   function Continue_To_Line_Flags
     return GPS.Kernel.Messages.Message_Flags;
   --  Message flags used for "Continue to line" annotations

end DAP.Module;
