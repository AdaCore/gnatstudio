------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2026, AdaCore                   --
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
with VSS.Strings.Conversions;

package body GPS.LSP_Client.Requests.Execute_Command is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Execute_Command_Request)
      return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "workspace/executeCommand";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Execute_Command_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      pragma Unreferenced (Stream);

   begin
      Abstract_Execute_Command_Request'Class (Self).On_Result_Message;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Abstract_Execute_Command_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
   begin
      Self.Kernel.Get_Messages_Window.Insert_Error
        ("Failed to execute command: "
         & VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Execute_Command_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.ExecuteCommandParams'Write
        (Stream, Abstract_Execute_Command_Request'Class (Self).Params);
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Execute_Command_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean
   is
      Command_Name : constant VSS.Strings.Virtual_String :=
        Abstract_Execute_Command_Request'Class (Self).Command_Name;
   begin
      return Options.executeCommandProvider.Is_Set
        and then Options.executeCommandProvider.Value.commands.Contains
          (Command_Name);
   end Is_Request_Supported;

end GPS.LSP_Client.Requests.Execute_Command;
