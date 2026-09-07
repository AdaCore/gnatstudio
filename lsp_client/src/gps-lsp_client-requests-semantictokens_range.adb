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

with LSP.Inputs;
with LSP.Outputs;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.SemanticTokens_Range is

   ------------
   -- Method --
   ------------

   overriding
   function Method
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

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_SemanticTokens_Range_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Tokens : LSP.Structures.SemanticTokens_Or_Null;
   begin
      LSP.Inputs.Read_SemanticTokens_Or_Null (Handler, Tokens);
      Abstract_SemanticTokens_Range_Request'Class (Self).On_Result_Message
        (Tokens);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_SemanticTokens_Range_Request)
      return LSP.Structures.SemanticTokensRangeParams is
   begin
      return
        (textDocument =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
         a_range      =>
           (start  => (line => Self.From - 1, character => 0),
            an_end => (line => Self.To - 1, character => 0)),
         others       => <>);
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_SemanticTokens_Range_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      return
        Options.semanticTokensProvider.Is_Set
        and then
          (if Options.semanticTokensProvider.Value.Is_SemanticTokensOptions
           then
             Options
               .semanticTokensProvider
               .Value
               .SemanticTokensOptions
               .a_range
               .Is_Set
           else
             Options
               .semanticTokensProvider
               .Value
               .SemanticTokensRegistrationOptions
               .a_range
               .Is_Set);
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_SemanticTokens_Range_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_SemanticTokensRangeParams (Handler, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.SemanticTokens_Range;
