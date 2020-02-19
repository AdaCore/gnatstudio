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

with LSP.JSON_Streams;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Simple_Editor_Requests is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Simple_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Locations : LSP.Messages.Location_Vector;
   begin
      LSP.Messages.Location_Vector'Read (Stream, Locations);
      Abstract_Simple_Request'Class
        (Self).On_Result_Message (Locations);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Simple_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.TextDocumentPositionParams'Write
        (Stream,
         (textDocument =>
              (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
          position     =>
            (line      => LSP.Types.Line_Number (Self.Line - 1),
             character =>
               GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                 (Self.Column))));
   end Params;

end GPS.LSP_Client.Requests.Simple_Editor_Requests;
