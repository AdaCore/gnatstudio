------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2019-2026, AdaCore                     --
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

with GPS.LSP_Client.Partial_Results;

package GPS.LSP_Client.Requests.Symbols is

   type Abstract_Symbol_Request is abstract
     new LSP_Request
     and GPS.LSP_Client.Partial_Results.LSP_Request_Partial_Result
   with record
      Query              : VSS.Strings.Virtual_String;
      Case_Sensitive     : LSP.Structures.Boolean_Optional;
      Whole_Word         : LSP.Structures.Boolean_Optional;
      Negate             : LSP.Structures.Boolean_Optional;
      Kind               : LSP.Structures.AlsSearchKind_Optional;
      partialResultToken : LSP.Structures.ProgressToken_Optional;
   end record;

   procedure On_Partial_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Result : LSP.Structures.SymbolInformation_Vector)
   is abstract;
   --  Called when a partial result response is received from the server.
   --  Only the SymbolInformation shape of the Symbol_Progress_Report variant
   --  is routed here (see GPS.LSP_Clients.Progress_Handler); the newer
   --  WorkspaceSymbol shape is not currently handled.

   procedure On_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Result : LSP.Structures.SymbolInformation_Vector)
   is abstract;
   --  Called when a result response is received from the server. Only the
   --  SymbolInformation variant of Symbol_Result is handled; the
   --  WorkspaceSymbol and null variants are treated as an empty vector.

   overriding
   function Method
     (Self : Abstract_Symbol_Request) return VSS.Strings.Virtual_String;

   overriding
   procedure Params
     (Self    : Abstract_Symbol_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Symbol_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean;

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Symbol_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class);

   overriding
   function Get_Task_Label (Self : Abstract_Symbol_Request) return String
   is ("querying symbols");

   overriding
   procedure Set_Partial_Result_Token
     (Self : in out Abstract_Symbol_Request;
      To   : LSP.Structures.ProgressToken);

   overriding
   function Partial_Result_Token
     (Self : Abstract_Symbol_Request) return LSP.Structures.ProgressToken
   is (Self.partialResultToken.Value);

end GPS.LSP_Client.Requests.Symbols;
