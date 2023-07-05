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
--  LSP request to be used by scripting languages integration.

with GNATCOLL.Scripts;

package GPS.LSP_Client.Requests.Shell is

   -------------------
   -- Shell_Request --
   -------------------

   type Shell_Request is new LSP_Request with record
      Method            : VSS.Strings.Virtual_String;
      Params            : GNATCOLL.JSON.JSON_Value;
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type;
      Auto_Canceled     : Boolean := False;
   end record;

   overriding procedure Finalize (Self : in out Shell_Request);

   overriding function Method
     (Self : Shell_Request) return VSS.Strings.Virtual_String;

   overriding procedure Params
     (Self   : Shell_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Is_Request_Supported
     (Self    : Shell_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean;

   overriding procedure On_Result_Message
     (Self   : in out Shell_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Error_Message
     (Self    : in out Shell_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out Shell_Request; Reason : Reject_Reason);

   overriding function Auto_Cancel
     (Self : in out Shell_Request) return Boolean is (Self.Auto_Canceled);

end GPS.LSP_Client.Requests.Shell;
