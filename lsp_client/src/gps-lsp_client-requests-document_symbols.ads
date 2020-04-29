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

package GPS.LSP_Client.Requests.Document_Symbols is

   type Document_Symbols_Request is
     abstract new LSP_Request with null record;

   procedure On_Result_Message
     (Self   : in out Document_Symbols_Request;
      Result : LSP.Messages.Symbol_Vector) is abstract;
   --  Called when a result response is received from the server.

   overriding function Method
     (Self : Document_Symbols_Request) return String;

   overriding procedure Params
     (Self   : Document_Symbols_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Is_Request_Supported
     (Self    : Document_Symbols_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean;

   overriding procedure On_Result_Message
     (Self   : in out Document_Symbols_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

end GPS.LSP_Client.Requests.Document_Symbols;
