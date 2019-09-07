------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Unbounded;

with GPS.Kernel;
with GPS.LSP_Client.Configurations;
limited with GPS.LSP_Client.Language_Servers.Interceptors;
with GPS.LSP_Clients;

with Language; use Language;

package GPS.LSP_Client.Language_Servers.Real is

   type Real_Language_Server
     (Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Configuration : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Interceptor   : not null access Interceptors.Interceptor_Listener'Class;
      Language      : not null access Language_Root'Class)
   is
     new Abstract_Language_Server
     and GPS.LSP_Clients.LSP_Client_Listener with
   record
      Client      : aliased GPS.LSP_Clients.LSP_Client
        (Kernel, Real_Language_Server'Unchecked_Access, Language);
      In_Shutdown : Boolean := False;
      --  Shutdown sequence has been initiated.
   end record;

   overriding function Get_Client
     (Self : Real_Language_Server)
      return GPS.LSP_Clients.LSP_Client_Access is
     (Self.Client'Unrestricted_Access);

   function Create
     (Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Configuration : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Interceptor   : not null access Interceptors.Interceptor_Listener'Class;
      Language      : not null access Language_Root'Class)
      return not null Language_Server_Access;
   --  Create and initialize language server object. Language server
   --  must be configured and server process should be started before it
   --  is possible to interact with server.

   procedure Start (Self : in out Real_Language_Server'Class);
   --  Initiate startup sequence for language server. It includes start of
   --  language server process and send of initialization/configuration
   --  requests/notifications to the server.

   procedure Shutdown
     (Self               : in out Real_Language_Server'Class;
      Reject_Immediately : Boolean);
   --  Initiate shutdown sequence for the language server. This procedure
   --  executes the shutdown request, send the exit notification and wait
   --  until the process ends.
   --  If Reject_Immediately is True then all ongoing and queued requests are
   --  rejected immediately.

private

   overriding procedure On_Server_Started (Self : in out Real_Language_Server);
   --  Handles startup of the language server.

   overriding procedure On_Server_Stopped (Self : in out Real_Language_Server);
   --  Handles shutdown of the language server.

   overriding procedure On_Response_Processed
     (Self : in out Real_Language_Server;
      Data : Ada.Strings.Unbounded.Unbounded_String);
   --  Handles receive of the response message.

   overriding procedure Execute
     (Self    : in out Real_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Executes request.

   overriding procedure Configuration_Changed
     (Self : in out Real_Language_Server);
   --  Send didConfigurationChange notification if necessary.

end GPS.LSP_Client.Language_Servers.Real;
