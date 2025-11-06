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

--  Abstract DAP request that is a basis for all requests

with VSS.Strings;
with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;

with GPS.Kernel;
with GPS.DAP_Client.Callbacks;
with GPS.DAP_Client.Requests;

limited with DAP.Clients;

package DAP.Requests is

   --  No additional private components
   Default_Callback_Access   : constant GPS.DAP_Client.Callbacks.DAP_Callback_Access :=
     Default_Callback_Instance'Access;

   type DAP_Request
     (Kernel    : GPS.Kernel.Kernel_Handle;
      Callbacks : not null access
        GPS.DAP_Client.Callbacks.DAP_Callback_Interface'Class :=
          Default_Callback_Access)
     is abstract limited private;

   type DAP_Request_Access is access all DAP_Request'Class;

   procedure Finalize (Self : in out DAP_Request) is null;
   --  Called before deallocation of the request object.

   procedure Write
     (Self   : DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class)
   is abstract;
   --  Fill the stream with the request parameters.

   procedure On_Result_Message
     (Self        : in out DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access) is abstract;
   --  Called when a "result" response is received from the server.
   --  Fill New_Request is new request should be sent after this one.
   --  Sets Success to False when the response can't be parsed.

   procedure On_Rejected
     (Self   : in out DAP_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);

   --  Called when the processing of the request rejected by any reason, for
   --  example server is not ready, or dies before request is sent, it dies
   --  after request was send but before response or error is received.

   procedure On_Error_Message
     (Self    : in out DAP_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   procedure Destroy (Item : in out DAP_Request_Access);
   --  Call Finalize and deallocate memory. All references are reset to null.

   procedure Set_Seq
     (Self : in out DAP_Request;
      Seq  : Integer) is abstract;
   --  Set unique ID for the request

   function Method (Self : in out DAP_Request) return String is abstract;

private

   type DAP_Request
     (Kernel    : GPS.Kernel.Kernel_Handle;
      Callbacks : not null access
        GPS.DAP_Client.Callbacks.DAP_Callback_Interface'Class)
     is abstract new
       GPS.DAP_Client.Requests.Request (Callbacks) with null record;

   Default_Callback_Instance : aliased GPS.DAP_Client.Callbacks.Null_Callback;

end DAP.Requests;
