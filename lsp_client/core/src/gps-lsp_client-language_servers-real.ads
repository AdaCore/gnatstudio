------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2023, AdaCore                   --
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

with LSP.Types;

with GPS.LSP_Client.Callbacks;
with GPS.LSP_Client.Configurations;
limited with GPS.LSP_Client.Language_Servers.Interceptors;
with GPS.LSP_Clients;

with Language; use Language;

package GPS.LSP_Client.Language_Servers.Real is

   type Real_Language_Server
     (Callbacks           : not null access
        GPS.LSP_Client.Callbacks.LSP_Callback_Interface'Class;
      Configuration       : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Server_Interceptor  : not null access
        Interceptors.Server_Listener'Class;
      Request_Interceptor : not null access
        Interceptors.Request_Listener'Class;
      Language            : not null access Language_Root'Class)
   is
     new Abstract_Language_Server
     and GPS.LSP_Clients.LSP_Client_Listener with
   record
      Client    : aliased GPS.LSP_Clients.LSP_Client
        (Callbacks, Real_Language_Server'Unchecked_Access, Language);
      Destroyed : Boolean := False;
      --  Set when module was destroyed.
   end record;

   overriding function Get_Client
     (Self : Real_Language_Server)
      return GPS.LSP_Clients.LSP_Client_Access is
     (Self.Client'Unrestricted_Access);

   overriding function Is_Configuration_Supported
     (Self    : in out Real_Language_Server;
      Setting : GPS.LSP_Client.Configurations.Setting_Kind)
      return Boolean;

   function Create
     (Kernel              : not null access
        GPS.Kernel.Kernel_Handle_Record'Class;
      Configuration       : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Server_Interceptor  : not null access
        Interceptors.Server_Listener'Class;
      Request_Interceptor : not null access
        Interceptors.Request_Listener'Class;
      Language            : not null access Language_Root'Class)
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

   procedure Restart (Self : in out Real_Language_Server'Class);
   --  Restart the language server executable

   function Get_Running_Request
     (Self : Real_Language_Server'Class;
      Id   : LSP.Types.LSP_Number_Or_String)
      return GPS.LSP_Client.Requests.Request_Access;
   --  If a request with the given Id is currently running, return it.
   --  Return null otherwise.
   --  Clients must NOT free the result: the request remains owned by
   --  the server.

private

   overriding procedure On_Server_Started (Self : in out Real_Language_Server);
   --  Handles startup of the language server.

   overriding procedure On_Server_Stopped (Self : in out Real_Language_Server);
   --  Handles shutdown of the language server.

   overriding procedure On_Response_Processed
     (Self   : in out Real_Language_Server;
      Data   : Ada.Strings.Unbounded.Unbounded_String;
      Method : Ada.Strings.Unbounded.Unbounded_String);
   --  Handles receive of the response message from server.

   overriding procedure On_Response_Sent
     (Self : in out Real_Language_Server;
      Data : Ada.Strings.Unbounded.Unbounded_String);
   --  Handles receive of the response message to server.

   overriding procedure On_Send_Request
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access);

   overriding procedure On_Send_Cancel
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access);

   overriding procedure On_Receive_Reply
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access);

   overriding procedure On_Reject_Request
     (Self    : in out Real_Language_Server;
      Request : GPS.LSP_Client.Requests.Request_Access);

   overriding procedure Execute
     (Self    : in out Real_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Executes request.

   overriding procedure Cancel
     (Self    : in out Real_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access);
   --  Cancel request. Memory will be deallocated.

   overriding procedure Configuration_Changed
     (Self : in out Real_Language_Server);
   --  Send didConfigurationChange notification if necessary.

end GPS.LSP_Client.Language_Servers.Real;
