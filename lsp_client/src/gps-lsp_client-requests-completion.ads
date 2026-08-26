------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2026, AdaCore                  --
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

with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;

with GPS.LSP_Client.Requests.Base;

package GPS.LSP_Client.Requests.Completion is

   type Abstract_Completion_Request is abstract
     new GPS.LSP_Client.Requests.Base.Text_Document_Request
   with record
      Position : LSP.Structures.Position;
      Context  : LSP.Structures.CompletionContext;
   end record;

   function Params
     (Self : Abstract_Completion_Request)
      return LSP.Structures.CompletionParams;
   --  Return parameters of the request to be sent to the server.

   procedure On_Result_Message
     (Self   : in out Abstract_Completion_Request;
      Result : LSP.Structures.CompletionList)
   is abstract;
   --  Called when a result response is received from the server. The
   --  CompletionItem_Vector/null variants of the wire-level Completion_Result
   --  are normalized into an equivalent CompletionList here.

   overriding
   function Method
     (Self : Abstract_Completion_Request) return VSS.Strings.Virtual_String;

   overriding
   procedure Params
     (Self    : Abstract_Completion_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Completion_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean;

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Completion_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class);

   type Abstract_CompletionItem_Resolve_Request is abstract
     new GPS.LSP_Client.Requests.LSP_Request
   with record
      Item : LSP.Structures.CompletionItem;
   end record;

   function Params
     (Self : Abstract_CompletionItem_Resolve_Request)
      return LSP.Structures.CompletionItem;
   --  Return parameters of the request to be sent to the server.

   procedure On_Result_Message
     (Self   : in out Abstract_CompletionItem_Resolve_Request;
      Result : LSP.Structures.CompletionItem)
   is abstract;
   --  Called when a result response is received from the server.

   overriding
   function Method
     (Self : Abstract_CompletionItem_Resolve_Request)
      return VSS.Strings.Virtual_String;

   overriding
   procedure Params
     (Self    : Abstract_CompletionItem_Resolve_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding
   function Is_Request_Supported
     (Self    : Abstract_CompletionItem_Resolve_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean;

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_CompletionItem_Resolve_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class);

end GPS.LSP_Client.Requests.Completion;
