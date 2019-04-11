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
--  Abstract_Request is base type for all kinds of requests to language
--  server. Its API is oriented for communitation with the language server
--  and too low level to be used by applications.
--
--  Child packages provides more concrete, but still abstract types for each
--  request, these types are responsible for marshalling/unmarshalling of the
--  data mostly. They also provides abstract On_Result_Message subprogram
--  with request specific set of parameters.
--
--  Please note, that all objects of Abstract_Request'Class type must be
--  allocated on heap with Request_Access type. Call of Execute subprogram
--  moves responsibility for memory management to the language server. Memory
--  may be deallocated at any time, even during execution of Execute
--  subprogram.
--
--  +------ LSP_Module ----
--  |
--  |   +-------------+            --  Low-level request, root class for all
--  |   | LSP_Request |            --  request objects. This is internal and
--  |   +-------------+            --  handles low-level communication with
--  |         ^                    --  the language servers.
--  |         |
--  |      +--------------------+  --  Abstract layer for request X - where X
--  |      | Abstract_X_Request |  --  maps to a language server method, for
--  |      +--------------------+  --  instance "/references".
--  |           ^
--  +-----------|----------
--              |
--           +----------------+    --  Implemented by clients anywhere in GPS
--           | Real_X_Request |    --  to make an actual request to the
--           +----------------+    --  language server, and react to the
--                                 --  response.

with GNATCOLL.JSON;

with LSP.JSON_Streams;
with LSP.Messages;
private with LSP.Types;

with Language;

package GPS.LSP_Client.Requests is

   type LSP_Request is abstract tagged limited private;

   type Request_Access is access all LSP_Request'Class;

   function Method (Self : LSP_Request) return String is abstract;
   --  Name of the RPC method to be called.

   procedure Params
     (Self   : LSP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is abstract;
   --  Fill the stream with the request parameters.

   procedure On_Result_Message
     (Self   : in out LSP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is abstract;
   --  Called when a "result" response is received from the server.

   procedure On_Error_Message
     (Self    : in out LSP_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is null;
   --  Called when an "error" response is received from the server.

   procedure On_Rejected (Self : in out LSP_Request) is null;
   --  Called when the processing of the request rejected by any reason, for
   --  example server is not ready, or dies before request is sent, or
   --  it dies after request was send but before response or error is
   --  received.

   procedure Finalize (Self : in out LSP_Request) is null;
   --  Called before deallocation of the request object.

   procedure Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access);
   --  Execute request using language server for the given language. Request
   --  parameter is set to null.

   procedure Destroy (Item : in out Request_Access);
   --  Call Finalize and deallocate memory.

private

   type LSP_Request is abstract tagged limited record
      Id : LSP.Types.LSP_Number_Or_String;
      --  Identifier of the processing request. This field is used by
      --  implementation only and not visible to clients.
   end record;

end GPS.LSP_Client.Requests;
