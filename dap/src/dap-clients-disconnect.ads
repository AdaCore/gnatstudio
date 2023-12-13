------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  Concrete implementation of the DAP 'disconnect' request

with DAP.Requests;        use DAP.Requests;
with DAP.Requests.Disconnect;
with GPS.Kernel;          use GPS.Kernel;

package DAP.Clients.Disconnect is

   type Disconnect_Request (<>) is
     new DAP.Requests.Disconnect.Disconnect_DAP_Request with private;
   type Disconnect_Request_Access is access all Disconnect_Request'Class;

   function Create
     (Kernel             : not null Kernel_Handle;
      Terminate_Debuggee : Boolean)
      return Disconnect_Request_Access;
   --  Create a new DAP 'disconnect' request.
   --  Terminate_Debuggee indicates whether the debuggee should be terminated
   --   when the debugger is disconnected if supported. If unspecified, the
   --   debug adapter is free to do whatever it thinks is best.

   overriding procedure On_Result_Message
     (Self        : in out Disconnect_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.DisconnectResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Rejected
     (Self   : in out Disconnect_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);

   overriding procedure On_Error_Message
     (Self    : in out Disconnect_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

private

   type Disconnect_Request is
     new DAP.Requests.Disconnect.Disconnect_DAP_Request with null record;

end DAP.Clients.Disconnect;
