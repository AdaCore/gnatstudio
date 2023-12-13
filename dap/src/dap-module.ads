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

with DAP.Modules.Breakpoints;
with DAP.Clients;
with DAP.Types;

with Generic_Views;

package DAP.Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   procedure Terminate_Debuggers;
   --  Terminate all debuggers

   procedure Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "");
   function Initialize_Debugger
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      File            : GNATCOLL.VFS.Virtual_File := No_File;
      Project         : Project_Type := No_Project;
      Executable_Args : String := "")
      return DAP.Clients.DAP_Client_Access;
   --  Initialize a DAP debugging session.
   --  File and Project are used to refer to the executable we want to debug.
   --  When not specified, the debugger will start without any executable to
   --  debug: this is the case when the user wants to attach to an already
   --  running executable for instance.
   --  Executable_Args contain the extra arguments that will be passed
   --  to the debugged executable.

   procedure Finished (Id : Positive);
   --  Called when some debugger is finished

   function Get_Current_Debugger return DAP.Clients.DAP_Client_Access;
   --  Returns the debugger that is "selected" now if several are started

   procedure Set_Current_Debugger (Current : DAP.Clients.DAP_Client_Access);

   function Get_Debugger (Id : Integer) return DAP.Clients.DAP_Client_Access;

   function Count_Running_Debuggers return Natural;
   --  Returns the count for the running debuggers

   procedure For_Each_Debugger
     (Callback : access procedure (Debugger : DAP.Clients.DAP_Client_Access));
   --  Calls Callback for each debugger

   procedure Start_Program (Client : DAP.Clients.DAP_Client_Access);

   function Get_Started_Per_Session_Debuggers return Integer;
   --  Returns count of debuggers that have been started in parallel
   --  in current session.

   function Get_Breakpoints_View return Generic_Views.Abstract_View_Access;
   procedure Set_Breakpoints_View (View : Generic_Views.Abstract_View_Access);

   function Get_Breakpoint_From_Id
     (Id : DAP.Types.Breakpoint_Identifier)
      return DAP.Modules.Breakpoints.Breakpoint_Data;

end DAP.Module;
