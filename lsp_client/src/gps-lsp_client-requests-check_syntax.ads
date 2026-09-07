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

--  This package implements the custom request "$/alsCheckSyntax"

with VSS.JSON.Content_Handlers;
with VSS.JSON.Pull_Readers;
with VSS.Strings;
with VSS.String_Vectors;

with LSP.Enumerations;
with LSP.Structures;

package GPS.LSP_Client.Requests.Check_Syntax is

   type Check_Syntax_Request is new LSP_Request with record
      Input : VSS.Strings.Virtual_String;
      Rules : VSS.String_Vectors.Virtual_String_Vector;
   end record;
   type Check_Syntax_Request_Access is access all Check_Syntax_Request'Class;

   overriding
   function Method
     (Self : Check_Syntax_Request) return VSS.Strings.Virtual_String;

   overriding
   procedure Params
     (Self    : Check_Syntax_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class);

   overriding
   procedure On_Result_Message
     (Self    : in out Check_Syntax_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class);

   overriding
   procedure On_Error_Message
     (Self    : in out Check_Syntax_Request;
      Code    : LSP.Enumerations.ErrorCodes;
      Message : VSS.Strings.Virtual_String)
   is null;

   overriding
   function Is_Request_Supported
     (Self : Check_Syntax_Request; Options : LSP.Structures.ServerCapabilities)
      return Boolean
   is (True);

end GPS.LSP_Client.Requests.Check_Syntax;
