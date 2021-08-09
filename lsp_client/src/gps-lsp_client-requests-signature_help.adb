------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2021, AdaCore                  --
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

package body GPS.LSP_Client.Requests.Signature_Help is

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Signature_Help_Request)
      return LSP.Messages.SignatureHelpParams is
   begin
      return
        (textDocument =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.File)),
         position     => Self.Position,
         context      => Self.Context,
         others       => <>);
   end Params;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Signature_Help_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
   begin
      LSP.Messages.SignatureHelpParams'Write (Stream, Self.Params);
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Signature_Help_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.signatureHelpProvider.Is_Set;
   end Is_Request_Supported;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Signature_Help_Request;
      Stream :        not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Response : LSP.Messages.SignatureHelp;
   begin
      LSP.Messages.SignatureHelp'Read (Stream, Response);
      Abstract_Signature_Help_Request'Class
        (Self).On_Result_Message (Response);
   end On_Result_Message;

end GPS.LSP_Client.Requests.Signature_Help;
