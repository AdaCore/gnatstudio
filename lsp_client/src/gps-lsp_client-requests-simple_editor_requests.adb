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

package body GPS.LSP_Client.Requests.Simple_Editor_Requests is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Simple_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Locations : LSP.Messages.Location_Or_Link_Vector;
   begin
      LSP.Messages.Location_Or_Link_Vector'Read (Stream, Locations);
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
      case Self.Command is
         when Goto_Type_Decl =>
            LSP.Messages.TextDocumentPositionParams'Write
              (Stream,
               (textDocument =>
                    (uri => GPS.LSP_Client.Utilities.To_URI
                       (Self.Text_Document)),
                position     =>
                  (line      => LSP.Types.Line_Number (Self.Line - 1),
                   character =>
                      GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                     (Self.Column))));
         when others =>
            LSP.Messages.NavigationRequestParams'Write
              (Stream,
               (textDocument =>
                    (uri => GPS.LSP_Client.Utilities.To_URI
                       (Self.Text_Document)),
                position     =>
                  (line      => LSP.Types.Line_Number (Self.Line - 1),
                   character =>
                      GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                     (Self.Column)),
                alsDisplayMethodAncestryOnNavigation =>
                 (Is_Set => True,
                  Value  => Self.Display_Ancestry_On_Navigation),
                others => <>));
      end case;
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Simple_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      case Self.Command is
         when Goto_Body         =>
            return Options.implementationProvider.Is_Set;

         when Goto_Spec         =>
            return Options.declarationProvider.Is_Set;

         when Goto_Spec_Or_Body =>
            return Options.definitionProvider.Is_Set;

         when Goto_Type_Decl    =>
            return Options.typeDefinitionProvider.Is_Set;
      end case;
   end Is_Request_Supported;

end GPS.LSP_Client.Requests.Simple_Editor_Requests;
