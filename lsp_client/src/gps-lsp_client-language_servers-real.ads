------------------------------------------------------------------------------
--                                  G P S                                   --
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

package GPS.LSP_Client.Language_Servers.Real is

   type Real_Language_Server
     (Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager       : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class;
      Configuration : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Interceptor   : not null access Interceptors.Interceptor_Listener'Class)
   is
     new Abstract_Language_Server (Manager)
     and GPS.LSP_Clients.LSP_Client_Listener with
   record
      Client      : aliased GPS.LSP_Clients.LSP_Client
                      (Kernel, Real_Language_Server'Unchecked_Access);
      In_Shutdown : Boolean := False;
      --  Shutdown sequence has been initiated.
   end record;

   function Create
     (Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager       : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class;
      Configuration : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class;
      Interceptor   : not null access Interceptors.Interceptor_Listener'Class)
      return not null Language_Server_Access;
   --  Create and initialize language server object. Language server
   --  must be configured and server process should be started before it
   --  is possible to interact with server.

   procedure Start (Self : in out Real_Language_Server'Class);
   --  Initiate startup sequence for language server. It includes start of
   --  language server process and send of initialization/configuration
   --  requests/notifications to the server.

   procedure Shutdown (Self : in out Real_Language_Server'Class);
   --  Initiate shutdown sequence for the language server. This procedure
   --  executes the shutdown request, send the exit notification and wait
   --  until the process ends.

private

   overriding procedure Associate
     (Self     : in out Real_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Associate text document with language server. Set_Server supbrogram of
   --  the text document will be called immediately if the server is up.
   --  Otherwise, it will be called later when server will be up.

   overriding procedure Dissociate
     (Self     : in out Real_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access);
   --  Dissociate association of text document and the language server.
   --  Set_Server with null value will be called on text document before
   --  dissociation if server was set for text document.

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
