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

package body GPS.LSP_Client.Requests.Shutdown is

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Shutdown_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "shutdown";
   end Method;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Shutdown_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class)
   is
      pragma Unreferenced (Self);

   begin
      --  The "shutdown" request takes no parameters, but the "params"
      --  member is unconditionally written by the caller, so this must
      --  still produce a valid JSON value (an empty object).

      Handler.Start_Object;
      Handler.End_Object;
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Shutdown_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      return True;
   end Is_Request_Supported;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Shutdown_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      pragma Unreferenced (Handler);

   begin
      Abstract_Shutdown_Request'Class (Self).On_Result_Message;
   end On_Result_Message;

end GPS.LSP_Client.Requests.Shutdown;
