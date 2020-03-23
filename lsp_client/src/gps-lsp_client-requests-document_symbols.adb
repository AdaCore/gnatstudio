------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

package body GPS.LSP_Client.Requests.Document_Symbols is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Document_Symbols_Request) return String
   is
      pragma Unreferenced (Self);
   begin
      return "textDocument/documentSymbol";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Document_Symbols_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Symbols : LSP.Messages.Symbol_Vector;
   begin
      LSP.Messages.Symbol_Vector'Read (Stream, Symbols);
      Document_Symbols_Request'Class (Self).On_Result_Message (Symbols);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Document_Symbols_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.DocumentSymbolParams'Write
        (Stream,
         (workDoneToken      => <>,
          partialResultToken => <>,
          textDocument       =>
              (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document))));
   end Params;

end GPS.LSP_Client.Requests.Document_Symbols;
