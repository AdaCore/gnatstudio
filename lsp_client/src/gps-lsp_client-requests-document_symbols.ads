------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2026, AdaCore                   --
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

package GPS.LSP_Client.Requests.Document_Symbols is

   type Document_Symbols_Request is abstract
     new GPS.LSP_Client.Requests.Base.Text_Document_Request
   with record
      Query          : VSS.Strings.Virtual_String;
      Case_Sensitive : LSP.Structures.Boolean_Optional;
      Whole_Word     : LSP.Structures.Boolean_Optional;
      Negate         : LSP.Structures.Boolean_Optional;
      Kind           : LSP.Structures.AlsSearchKind_Optional;
   end record;

   procedure On_Result_Message
     (Self   : in out Document_Symbols_Request;
      Result : LSP.Structures.DocumentSymbol_Result)
   is abstract;
   --  Called when a result response is received from the server. Result may
   --  be a flat SymbolInformation list or a hierarchical DocumentSymbol
   --  tree, passed through unnormalized.

   overriding
   function Method
     (Self : Document_Symbols_Request) return VSS.Strings.Virtual_String;

   overriding
   procedure Params
     (Self    : Document_Symbols_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding
   function Is_Request_Supported
     (Self    : Document_Symbols_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean;

   overriding
   procedure On_Result_Message
     (Self    : in out Document_Symbols_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class);

end GPS.LSP_Client.Requests.Document_Symbols;
