------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2020, AdaCore                   --
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
--  Please note, that all objects of LSP_Request'Class type must be allocated
--  on heap with Request_Access type. Use of other access types (usually
--  access to derived type of the LSP_Request) is prohibited. Call of Execute
--  subprogram moves responsibility for memory management to the language
--  server. Memory may be deallocated at any time, even during execution of
--  Execute subprogram.
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
--
--  Reference type is weak-reference to request. It can be obtained by call
--  to corresponding Execute function. Note, Execute can return "null"
--  reference in case when request was executed/rejected during execution
--  of the Execute function.

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Finalization;

with GNATCOLL.JSON;
with GNATCOLL.VFS;         use GNATCOLL.VFS;

with GPS.Kernel;           use GPS.Kernel;

with LSP.JSON_Streams;
with LSP.Messages;
private with LSP.Types;

with Language;             use Language;

package GPS.LSP_Client.Requests is

   type LSP_Request
     (Kernel : GPS.Kernel.Kernel_Handle) is
     abstract tagged limited private;
   --  Do not fill Kernel if an answer on request should not be skipped
   --  when Kernel is destroying

   type Request_Access is access all LSP_Request'Class;

   type Reference is tagged private;

   -----------------
   -- LSP_Request --
   -----------------

   function Text_Document
     (Self : LSP_Request) return GNATCOLL.VFS.Virtual_File;
   --  Return Text_Document associated with the request, if any. Default
   --  implementation always returns No_File.

   function Method (Self : LSP_Request) return String is abstract;
   --  Name of the RPC method to be called.

   function Get_Task_Label (Self : LSP_Request) return String is ("");
   --  Label displayed in the mini-tasks bar and/or the Tasks view.
   --  Return an empty string if the task should not be shown (default).

   procedure Params
     (Self   : LSP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is abstract;
   --  Fill the stream with the request parameters.

   function Is_Request_Supported
     (Self    : LSP_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is abstract;
   --  Returns False when server does not support the request

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

   ---------------
   -- Reference --
   ---------------

   function Request (Self : Reference) return Request_Access;
   --  Return associated request object or null.

   function Has_Request (Self : Reference) return Boolean;
   --  Return True when associated request object is alive.

   ---------------------------------------------------------
   -- Utility subprograms to execute and destroy requests --
   ---------------------------------------------------------

   function Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access) return Boolean;
   --  Return False if the request was not sent due to pre-send checks failing
   --  Note: It is not recommended to use this subprogram. It's return value
   --  is almost meaningless, On_Rejected subprogam of the LSP_Request should
   --  be redefined to process case when request is rejected immediately.

   procedure Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access);
   --  The same as above but without returning the result

   function Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access) return Reference;
   --  Execute request using language server for the given language. Request
   --  parameter is set to null.

   procedure Destroy (Item : in out Request_Access);
   --  Call Finalize and deallocate memory. All references are reset to null.

private

   type Abstract_Reference is tagged;
   type Reference_Access is access all Abstract_Reference'Class;

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference_Access);

   type Abstract_Reference is
     abstract new Ada.Finalization.Controlled with record
      Request  : Request_Access;
      Position : Reference_Lists.Cursor;
   end record;

   procedure Initialize
     (Self    : in out Abstract_Reference'Class;
      Request : Request_Access);
   --  Initialize reference and add it to the list of the request's references.

   overriding procedure Adjust (Self : in out Abstract_Reference);
   overriding procedure Finalize (Self : in out Abstract_Reference);

   type LSP_Request (Kernel : GPS.Kernel.Kernel_Handle) is
     abstract tagged limited record
      Id         : LSP.Types.LSP_Number_Or_String;
      --  Identifier of the processing request. This field is used by
      --  implementation only and not visible to clients.

      References : Reference_Lists.List;
   end record;

   type Reference is new Abstract_Reference with null record;

end GPS.LSP_Client.Requests;
