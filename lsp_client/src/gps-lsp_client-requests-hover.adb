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

with LSP.JSON_Streams;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Hover is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Hover_Request) return String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/hover";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Hover_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Hover : LSP.Messages.Hover;

   begin
      LSP.Messages.Hover'Read (Stream, Hover);
      Abstract_Hover_Request'Class (Self).On_Result_Message (Hover);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Hover_Request)
      return LSP.Messages.TextDocumentPositionParams is
   begin
      return
        (textDocument =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
         position     =>
           (line      => LSP.Types.Line_Number (Self.Line - 1),
            character =>
              GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                (Self.Column)));
   end Params;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Hover_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.TextDocumentPositionParams'Write (Stream, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Hover;
