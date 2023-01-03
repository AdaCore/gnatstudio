------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2023, AdaCore                   --
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

package body GPS.LSP_Client.Requests.Symbols is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Symbol_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "workspace/symbol";
   end Method;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Symbol_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.WorkspaceSymbolParams'Write
        (Stream,
         (query              => Self.Query,
          case_sensitive     => Self.Case_Sensitive,
          whole_word         => Self.Whole_Word,
          negate             => Self.Negate,
          kind               => Self.Kind,
          partialResultToken => Self.partialResultToken,
          others             => <>));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Symbol_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.workspaceSymbolProvider.Is_Set;
   end Is_Request_Supported;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Info : LSP.Messages.SymbolInformation_Vector;
   begin
      LSP.Messages.SymbolInformation_Vector'Read (Stream, Info);
      Abstract_Symbol_Request'Class
        (Self).On_Result_Message (Info);
   end On_Result_Message;

end GPS.LSP_Client.Requests.Symbols;
