------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2026, AdaCore                  --
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

package body GPS.LSP_Client.Requests.Called_By is

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Prepare_Call_Hierarchy_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_CallHierarchyPrepareParams
        (Handler,
         (textDocument => (uri => GPS.LSP_Client.Utilities.To_URI (Self.File)),
          position     => Self.Position,
          others       => <>));
   end Params;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Prepare_Call_Hierarchy_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Result : LSP.Structures.CallHierarchyItem_Vector;
   begin
      LSP.Inputs.Read_CallHierarchyItem_Vector_Or_Null (Handler, Result);
      Abstract_Prepare_Call_Hierarchy_Request'Class (Self).On_Result_Message
        (Result);
   end On_Result_Message;

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Prepare_Call_Hierarchy_Request)
      return VSS.Strings.Virtual_String
   is ("textDocument/prepareCallHierarchy");

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Prepare_Call_Hierarchy_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      return Options.callHierarchyProvider.Is_Set;
   end Is_Request_Supported;

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Called_By_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "callHierarchy/incomingCalls";
   end Method;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Called_By_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_CallHierarchyIncomingCallsParams
        (Handler, (item => Self.Item, others => <>));
   end Params;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Called_By_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Result : LSP.Structures.CallHierarchyIncomingCall_Vector;
   begin
      LSP.Inputs.Read_CallHierarchyIncomingCall_Vector (Handler, Result);
      Abstract_Called_By_Request'Class (Self).On_Result_Message (Result);
   end On_Result_Message;

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Calls_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "callHierarchy/outgoingCalls";
   end Method;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Calls_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_CallHierarchyOutgoingCallsParams
        (Handler, (item => Self.Item, others => <>));
   end Params;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Calls_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Result : LSP.Structures.CallHierarchyOutgoingCall_Vector;
   begin
      LSP.Inputs.Read_CallHierarchyOutgoingCall_Vector (Handler, Result);
      Abstract_Calls_Request'Class (Self).On_Result_Message (Result);
   end On_Result_Message;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Calls_Or_Called_By_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      return Options.callHierarchyProvider.Is_Set;
   end Is_Request_Supported;

end GPS.LSP_Client.Requests.Called_By;
