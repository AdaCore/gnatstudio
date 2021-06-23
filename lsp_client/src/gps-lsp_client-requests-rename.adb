------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2021, AdaCore                   --
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

package body GPS.LSP_Client.Requests.Rename is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Rename_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/rename";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Rename_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Edit : LSP.Messages.WorkspaceEdit;

   begin
      LSP.Messages.WorkspaceEdit'Read (Stream, Edit);
      Abstract_Rename_Request'Class
        (Self).On_Result_Message (Edit);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Rename_Request)
      return LSP.Messages.RenameParams is
   begin
      return
        (textDocument  =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
         position      => Self.Position,
         newName       => Self.New_Name,
         workDoneToken => (Is_Set => False));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Rename_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.renameProvider.Is_Set;
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Rename_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.RenameParams'Write (Stream, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Rename;
