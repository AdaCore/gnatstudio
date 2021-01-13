------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2021, AdaCore                   --
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
--  Interceptor for the language server interactions
--
--  Server_Listener allows to listen changes of the server state and sending/
--  receiving of the data.
--
--  Request_Listener allows to listen for request life cycle event: send of the
--  request, send cancelation of the request, rejection of the request, and
--  reception of the reply.

with Ada.Strings.Unbounded;

package GPS.LSP_Client.Language_Servers.Interceptors is

   type Server_Listener is limited interface;

   procedure On_Server_Started
     (Self   : in out Server_Listener;
      Server : not null Language_Server_Access) is null;
   --  Called when language server process has been started, language server
   --  has been initialized and configured (if necessary) and ready to
   --  process requests.

   procedure On_Server_Stopped
     (Self   : in out Server_Listener;
      Server : not null Language_Server_Access) is null;
   --  Called when language server has been stopped for any reason and
   --  unable to process requests anymore.

   procedure On_Response_Processed
     (Self   : in out Server_Listener;
      Server : not null Language_Server_Access;
      Data   : Ada.Strings.Unbounded.Unbounded_String;
      Method : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Called when response message from the server has been processed.

   procedure On_Response_Sent
     (Self   : in out Server_Listener;
      Server : not null Language_Server_Access;
      Data   : Ada.Strings.Unbounded.Unbounded_String) is null;
   --  Called when response message from GNAT Studio has been sent.

   type Request_Listener is limited interface;
   --  Intercepts lifecycle of the requests

   procedure On_Send_Request
     (Self    : in out Request_Listener;
      Request : GPS.LSP_Client.Requests.Reference) is null;
   --  Called when request send to the server.

   procedure On_Send_Cancel
     (Self    : in out Request_Listener;
      Request : GPS.LSP_Client.Requests.Reference) is null;
   --  Called when request cancel notification send to the server.

   procedure On_Receive_Reply
     (Self    : in out Request_Listener;
      Request : GPS.LSP_Client.Requests.Reference) is null;
   --  Called when reply is received.

   procedure On_Reject_Request
     (Self    : in out Request_Listener;
      Request : GPS.LSP_Client.Requests.Reference) is null;
   --  Called when request is rejected.

end GPS.LSP_Client.Language_Servers.Interceptors;
