------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2022, AdaCore                   --
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

package body GPS.LSP_Client.Requests.Code_Action is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Code_Action_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/codeAction";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Code_Action_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Response : LSP.Messages.CodeAction_Vector;

   begin
      LSP.Messages.CodeAction_Vector'Read (Stream, Response);
      Abstract_Code_Action_Request'Class
        (Self).On_Result_Message (Response);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Code_Action_Request)
      return LSP.Messages.CodeActionParams
   is
      Diagnostics : LSP.Messages.Diagnostic_Vector;
   begin
      return
        (workDoneToken      => (Is_Set => False),
         partialResultToken => (Is_Set => False),
         textDocument =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
         span         =>
           (first => Self.Start_Position,
            last  => Self.End_Position),
         context =>
           (diagnostics => Diagnostics,
            only        => (Is_Set => False)));
   end Params;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Code_Action_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.CodeActionParams'Write (Stream, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Code_Action;
