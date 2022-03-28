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

--  Abstract DAP request that is a basis for all requests

with VSS.Strings;

with LSP.Types;
with LSP.JSON_Streams;

with GPS.Kernel;

limited with DAP.Clients;

package DAP.Requests is

   type DAP_Request
     (Kernel : GPS.Kernel.Kernel_Handle) is
     abstract tagged limited private;

   type DAP_Request_Access is access all DAP_Request'Class;

   procedure Finalize (Self : in out DAP_Request) is null;
   --  Called before deallocation of the request object.

   procedure Write
     (Self   : DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is abstract;
   --  Fill the stream with the request parameters.

   procedure On_Result_Message
     (Self        : in out DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access) is abstract;
   --  Called when a "result" response is received from the server.
   --  Fill New_Request is new request should be sent after this one.

   procedure On_Rejected (Self : in out DAP_Request) is null;
   --  Called when the processing of the request rejected by any reason, for
   --  example server is not ready, or dies before request is sent, it dies
   --  after request was send but before response or error is received.

   procedure On_Error_Message
     (Self    : in out DAP_Request;
      Message : VSS.Strings.Virtual_String) is null;

   procedure Destroy (Item : in out DAP_Request_Access);
   --  Call Finalize and deallocate memory. All references are reset to null.

   procedure Set_Seq
     (Self : in out DAP_Request;
      Id   : LSP.Types.LSP_Number) is abstract;
   --  Set unique ID for the request

   procedure Set_Client
     (Self   : in out DAP_Request;
      Client : access DAP.Clients.DAP_Client'Class);
   --  Set the client that process the request

   function Method (Self : in out DAP_Request) return String is abstract;

private

   type DAP_Request
     (Kernel : GPS.Kernel.Kernel_Handle) is
     abstract tagged limited record
      Client : access DAP.Clients.DAP_Client'Class := null;
   end record;

end DAP.Requests;
