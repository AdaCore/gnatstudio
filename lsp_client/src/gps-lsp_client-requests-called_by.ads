------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2021, AdaCore                  --
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

--  Handlers for the "textDocument/prepareCallHierarchy",
--  "callHierarchy/incomingCalls", "callHierarchy/outgoingCalls" requests.

package GPS.LSP_Client.Requests.Called_By is

   ------------------------------------
   -- Prepare Call Hierarchy Request --
   ------------------------------------

   type Abstract_Prepare_Call_Hierarchy_Request is
     abstract new LSP_Request with record
      File     : Virtual_File;
      Position : LSP.Messages.Position;
   end record;

   overriding procedure Params
     (Self   : Abstract_Prepare_Call_Hierarchy_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Prepare_Call_Hierarchy_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   procedure On_Result_Message
     (Self   : in out Abstract_Prepare_Call_Hierarchy_Request;
      Result : LSP.Messages.CallHierarchyItem_Vector) is abstract;
   --  Called when a result response is received from the server.

   overriding function Method
     (Self : Abstract_Prepare_Call_Hierarchy_Request) return String;

   overriding function Is_Request_Supported
     (Self    : Abstract_Prepare_Call_Hierarchy_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean;

   -------------------
   -- Common parent --
   -------------------

   type Abstract_Calls_Or_Called_By_Request is
     abstract new LSP_Request with record
      Item : LSP.Messages.CallHierarchyItem;
   end record;

   overriding procedure Params
     (Self   : Abstract_Calls_Or_Called_By_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Method
     (Self : Abstract_Calls_Or_Called_By_Request) return String is abstract;

   overriding function Is_Request_Supported
     (Self    : Abstract_Calls_Or_Called_By_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean;

   ---------------
   -- Called By --
   ---------------

   type Abstract_Called_By_Request is
     abstract new Abstract_Calls_Or_Called_By_Request with null record;

   overriding function Method
     (Self : Abstract_Called_By_Request) return String;

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Called_By_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   procedure On_Result_Message
     (Self   : in out Abstract_Called_By_Request;
      Result : LSP.Messages.CallHierarchyIncomingCall_Vector) is abstract;

   -----------
   -- Calls --
   -----------

   type Abstract_Calls_Request is
     abstract new Abstract_Calls_Or_Called_By_Request with null record;

   overriding function Method
     (Self : Abstract_Calls_Request) return String;

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Calls_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   procedure On_Result_Message
     (Self   : in out Abstract_Calls_Request;
      Result : LSP.Messages.CallHierarchyOutgoingCall_Vector) is abstract;

end GPS.LSP_Client.Requests.Called_By;
