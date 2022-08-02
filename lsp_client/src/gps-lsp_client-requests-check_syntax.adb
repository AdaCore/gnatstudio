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
with GPS.LSP_Client.Editors.Code_Actions.Dialog;

package body GPS.LSP_Client.Requests.Check_Syntax is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Check_Syntax_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "$/alsCheckSyntax";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Check_Syntax_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      pragma Unreferenced (Self);
      Response : LSP.Messages.ALS_Check_Syntax_Result;

   begin
      LSP.Messages.ALS_Check_Syntax_Result'Read (Stream, Response);
      GPS.LSP_Client.Editors.Code_Actions.Dialog.Set_Result_Message
        (Response);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Check_Syntax_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.ALS_Check_Syntax_Params'Write
        (Stream, (Self.Input, Self.Rules));
   end Params;

end GPS.LSP_Client.Requests.Check_Syntax;
