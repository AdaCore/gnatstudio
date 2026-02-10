------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2024-2026, AdaCore                   --
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

with LSP.JSON_Streams;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.SemanticTokens_Range is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_SemanticTokens_Range_Request)
      return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/semanticTokens/range";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_SemanticTokens_Range_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Tokens : LSP.Messages.SemanticTokens;
   begin
      LSP.Messages.SemanticTokens'Read (Stream, Tokens);
      Abstract_SemanticTokens_Range_Request'Class
        (Self).On_Result_Message (Tokens);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_SemanticTokens_Range_Request)
      return LSP.Messages.SemanticTokensRangeParams is
   begin
      return
        (textDocument =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
         span         =>
           (first => (LSP.Types.Line_Number (Self.From - 1), 0),
            last  => (LSP.Types.Line_Number (Self.To - 1), 0)),
         others       => <>);
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_SemanticTokens_Range_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.semanticTokensProvider.Is_Set
        and then Options.semanticTokensProvider.Value.span.Is_Set
        and then Options.semanticTokensProvider.Value.span.Value;
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_SemanticTokens_Range_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.SemanticTokensRangeParams'Write (Stream, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.SemanticTokens_Range;
