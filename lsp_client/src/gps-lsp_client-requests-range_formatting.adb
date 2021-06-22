------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2021, AdaCore                   --
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

with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Range_Formatting is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Range_Formatting_Request)
      return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/rangeFormatting";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Range_Formatting_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Response : LSP.Messages.TextEdit_Vector;

   begin
      LSP.Messages.TextEdit_Vector'Read (Stream, Response);
      Abstract_Range_Formatting_Request'Class
        (Self).On_Result_Message (Response);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Range_Formatting_Request)
      return LSP.Messages.DocumentRangeFormattingParams is
   begin
      return
        (workDoneToken => <>,
         textDocument  =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
         span          => Self.Span,
         options       =>
           (tabSize                => LSP.Types.LSP_Number
              (Self.Indentation_Level),
            insertSpaces           => not Self.Use_Tabs,
            trimTrailingWhitespace =>
              (Is_Set => True, Value => Strip_Blanks.Get_Pref /= Never),
            insertFinalNewline     => (Is_Set => True, Value => False),
            trimFinalNewlines      =>
              (Is_Set => True, Value => Strip_Lines.Get_Pref /= Never)));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Range_Formatting_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return Options.documentRangeFormattingProvider.Is_Set;
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Range_Formatting_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.DocumentRangeFormattingParams'Write (Stream, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Range_Formatting;
