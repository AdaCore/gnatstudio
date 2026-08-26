------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2026, AdaCore                   --
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

package body GPS.LSP_Client.Requests.Document_Highlight is

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Document_Highlight_Request)
      return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/documentHighlight";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Document_Highlight_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Locations : LSP.Structures.DocumentHighlight_Vector;
   begin
      LSP.Inputs.Read_DocumentHighlight_Vector (Handler, Locations);
      Abstract_Document_Highlight_Request'Class (Self).On_Result_Message
        (Locations);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Document_Highlight_Request)
      return LSP.Structures.DocumentHighlightParams is
   begin
      return
        (LSP.Structures.TextDocumentPositionParams'
           (textDocument =>
              (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
            position     => Self.Position)
         with
           workDoneToken      => (Is_Set => False),
           partialResultToken => (Is_Set => False));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Document_Highlight_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      return Options.documentHighlightProvider.Is_Set;
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Document_Highlight_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_DocumentHighlightParams (Handler, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Document_Highlight;
