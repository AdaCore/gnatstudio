------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2020, AdaCore                  --
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

--  Handlers for the "alsCalledBy" and "alsCalls" requests

with Basic_Types;

package GPS.LSP_Client.Requests.Called_By is

   -------------------
   -- Common parent --
   -------------------

   type Abstract_Calls_Or_Called_By_Request is
     abstract new LSP_Request with record
      File   : Virtual_File;
      Line   : Positive;
      Column : Basic_Types.Visible_Column_Type;
   end record;

   function Params
     (Self : Abstract_Calls_Or_Called_By_Request)
      return LSP.Messages.TextDocumentPositionParams;
   --  Return parameters of the request to be sent to the server.

   procedure On_Result_Message
     (Self   : in out Abstract_Calls_Or_Called_By_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector) is abstract;
   --  Called when a result response is received from the server.

   overriding function Method
     (Self : Abstract_Calls_Or_Called_By_Request) return String is abstract;

   overriding procedure Params
     (Self   : Abstract_Calls_Or_Called_By_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Is_Request_Supported
     (Self    : Abstract_Calls_Or_Called_By_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean;

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Calls_Or_Called_By_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   ---------------
   -- Called By --
   ---------------

   type Abstract_Called_By_Request is
     abstract new Abstract_Calls_Or_Called_By_Request with null record;

   overriding function Method
     (Self : Abstract_Called_By_Request) return String;

   -----------
   -- Calls --
   -----------

   type Abstract_Calls_Request is
     abstract new Abstract_Calls_Or_Called_By_Request with null record;

   overriding function Method
     (Self : Abstract_Calls_Request) return String;

end GPS.LSP_Client.Requests.Called_By;
