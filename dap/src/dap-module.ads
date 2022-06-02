------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.VFS;   use GNATCOLL.VFS;
with GPS.Kernel;     use GPS.Kernel;

with DAP.Clients;

package DAP.Module is

   procedure Register_Module
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Prefix_Dir : Virtual_File);

   procedure Terminate_Debuggers;
   --  Terminate all debuggers

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String);
   --  Initialize the debugger. Uses when debugging is demanded
   --  by the command line switch.

   procedure Finished (Id : Positive);
   --  Called when some debugger is finished

   function Get_Current_Debugger return DAP.Clients.DAP_Client_Access;
   --  Returns the debugger that is "selected" now if several are started

   function Get_Debugger (Id : Integer) return DAP.Clients.DAP_Client_Access;

   function Count_Running_Debuggers return Natural;
   --  Returns the count for the running debuggers

   procedure For_Each_Debugger
     (Callback : access procedure (Debugger : DAP.Clients.DAP_Client_Access));
   --  Calls Callback for each debugger

end DAP.Module;
