------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2022, AdaCore                  --
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

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Called_By is

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Prepare_Call_Hierarchy_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.CallHierarchyPrepareParams'Write
        (Stream,
         (textDocument =>
              (uri => GPS.LSP_Client.Utilities.To_URI (Self.File)),
          position     => Self.Position,
          others => <>));
   end Params;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Prepare_Call_Hierarchy_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Result : LSP.Messages.CallHierarchyItem_Vector;
   begin
      LSP.Messages.CallHierarchyItem_Vector'Read
        (Stream, Result);
      Abstract_Prepare_Call_Hierarchy_Request'Class (Self).On_Result_Message
        (Result);
   end On_Result_Message;

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Prepare_Call_Hierarchy_Request)
      return VSS.Strings.Virtual_String
   is
      ("textDocument/prepareCallHierarchy");

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Prepare_Call_Hierarchy_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.callHierarchyProvider.Is_Set;
   end Is_Request_Supported;

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Called_By_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "callHierarchy/incomingCalls";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Called_By_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Result : LSP.Messages.CallHierarchyIncomingCall_Vector;
   begin
      LSP.Messages.CallHierarchyIncomingCall_Vector'Read (Stream, Result);
      Abstract_Called_By_Request'Class (Self).On_Result_Message (Result);
   end On_Result_Message;

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Calls_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "callHierarchy/outgoingCalls";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Calls_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Result : LSP.Messages.CallHierarchyOutgoingCall_Vector;
   begin
      LSP.Messages.CallHierarchyOutgoingCall_Vector'Read (Stream, Result);
      Abstract_Calls_Request'Class (Self).On_Result_Message (Result);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Calls_Or_Called_By_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.CallHierarchyIncomingCallsParams'Write
        (Stream, (item => Self.Item, others => <>));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Calls_Or_Called_By_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.callHierarchyProvider.Is_Set;
   end Is_Request_Supported;

end GPS.LSP_Client.Requests.Called_By;
