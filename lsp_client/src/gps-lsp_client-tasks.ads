------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2023, AdaCore                   --
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

--  Integration with task manager

private with Ada.Strings.Unbounded;

with GPS.Kernel;
with GPS.LSP_Client.Language_Servers.Interceptors;
private with GPS.LSP_Client.Requests;

package GPS.LSP_Client.Tasks is

   type Task_Manager_Integration (<>) is limited
     new GPS.LSP_Client.Language_Servers.Interceptors.Request_Listener
       with private;

   type Task_Manager_Integration_Access is
     access all Task_Manager_Integration'Class;

   function New_Task_Manager_Integration
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String) return not null Task_Manager_Integration_Access;

private

   type Task_Manager_Integration
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   limited new GPS.LSP_Client.Language_Servers.Interceptors.Request_Listener
   with record
      Language : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure On_Send_Request
     (Self    : in out Task_Manager_Integration;
      Request : GPS.LSP_Client.Requests.Reference);

end GPS.LSP_Client.Tasks;
