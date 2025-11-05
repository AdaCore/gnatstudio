------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with LSP.Types;

with GPS.LSP_Client.Partial_Results;

package GPS.LSP_Client.Requests.Symbols is

   type Abstract_Symbol_Request is
     abstract new LSP_Request
       and GPS.LSP_Client.Partial_Results.LSP_Request_Partial_Result with
      record
         Query              : VSS.Strings.Virtual_String;
         Case_Sensitive     : LSP.Types.Optional_Boolean;
         Whole_Word         : LSP.Types.Optional_Boolean;
         Negate             : LSP.Types.Optional_Boolean;
         Kind               : LSP.Messages.Optional_Search_Kind;
         partialResultToken : LSP.Messages.Optional_ProgressToken;
      end record;

   procedure On_Partial_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector) is abstract;
   --  Called when a partial result response is received from the server.

   procedure On_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Result : LSP.Messages.SymbolInformation_Vector) is abstract;
   --  Called when a result response is received from the server.

   overriding function Method
     (Self : Abstract_Symbol_Request) return VSS.Strings.Virtual_String;

   overriding procedure Params
     (Self   : Abstract_Symbol_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Is_Request_Supported
     (Self    : Abstract_Symbol_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean;

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Partial_Result_Message
     (Self   : in out Abstract_Symbol_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Get_Task_Label
     (Self : Abstract_Symbol_Request) return String
   is
     ("querying symbols");

   overriding procedure Set_Partial_Result_Token
     (Self : in out Abstract_Symbol_Request;
      To   : LSP.Types.ProgressToken);

   overriding function Partial_Result_Token
     (Self : Abstract_Symbol_Request)
      return LSP.Types.ProgressToken is (Self.partialResultToken.Value);

end GPS.LSP_Client.Requests.Symbols;
